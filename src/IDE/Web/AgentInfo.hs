{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | What leksah remembers ABOUT its agents, and the tree the Agents pane
-- ("IDE.Web.Widget.Agents") draws from it.
--
-- Two things a Claude Code session cannot tell you about itself:
--
--   * __who started it__.  @claude@ records a session's directory and its
--     @\/rename@ name, but nothing about lineage — yet 'IDE.Web.Agent.forkAgent'
--     knows the parent at the instant it forks, and never again afterwards.  So
--     that edge is written down here as it happens, and it is what makes the
--     pane a tree rather than a list.
--   * __what it is for__.  A first prompt is a poor label for an hour of work,
--     and no amount of transcript scraping produces "opened PR #412, waiting on
--     Hydra".  So the agent is /asked/ ('agentRefreshPrompt'), and answers with
--     a title and a small HTML fragment ('describeAgent') — links included.
--
-- Everything here is FS\/JSON only (no reflex), because the writer is usually a
-- control-socket connection: @leksah-cmd agent describe@ / the @describe_agent@
-- MCP tool land in "IDE.Web.CmdServer", not in a reflex network.  The reader is
-- the pane's poll, which must stay cheap — see 'agentForest'.
module IDE.Web.AgentInfo
  ( AgentInfo(..)
  , AgentNode(..)
  , recordAgentFork
  , describeAgent
  , dismissAgent
  , agentForest
  , agentRefreshPrompt
  , sanitizeAgentHtml
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (catch, SomeException)
import Control.Monad (forM)

import Data.Aeson
       (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject,
        encode, decode')
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isSpace)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
       (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import System.Directory (doesFileExist, getModificationTime)
import System.FilePath (dropTrailingPathSeparator)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Utils.FileUtils (getConfigFilePathForSave)
import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.Claude
       (ClaudeLive(..), claudeLiveBySession, claudeSessionLabel,
        claudeTranscriptPath)
import IDE.Web.ClaudeStatus
       (ClaudeStatus(..), ClaudeStatusRow(..), claudeStatusNow)
import IDE.Web.GitInfo (gitCurrentBranch, prForBranch)

-- | What leksah knows about one agent beyond what the CLI writes down.  Keyed
-- by session id — the only durable handle (pids and panes come and go, and a
-- @--resume@ keeps the id but loses the name).
data AgentInfo = AgentInfo
  { aiSession :: Text          -- ^ the agent's session id
  , aiParent  :: Maybe Text    -- ^ the session that forked it: the tree's edges
  , aiDir     :: FilePath      -- ^ its working directory (transcript / resume)
  , aiTitle   :: Maybe Text    -- ^ what it last called itself
  , aiDesc    :: Maybe Text    -- ^ its description, as sanitized HTML
  , aiWhen    :: Maybe Text    -- ^ when it last described itself (row tooltip)
  , aiBorn    :: Maybe Double  -- ^ when leksah started it (POSIX seconds) — so a
                               --   fork that has not registered a session yet
                               --   still shows as @starting@ rather than as
                               --   nothing at all
  } deriving (Eq, Show)

instance ToJSON AgentInfo where
  toJSON i = object
    [ "session" .= aiSession i, "parent" .= aiParent i, "dir" .= aiDir i
    , "title" .= aiTitle i, "desc" .= aiDesc i, "when" .= aiWhen i
    , "born" .= aiBorn i ]

instance FromJSON AgentInfo where
  parseJSON = withObject "AgentInfo" $ \o -> AgentInfo
    <$> o .: "session" <*> o .:? "parent" <*> (fromMaybe "" <$> o .:? "dir")
    <*> o .:? "title" <*> o .:? "desc" <*> o .:? "when" <*> o .:? "born"

-- | One row of the Agents pane: an agent, and the agents it forked.
data AgentNode = AgentNode
  { anSession  :: Text        -- ^ session id (the handle for show\/resume\/send)
  , anTitle    :: Text        -- ^ its own title, else its @\/rename@ name, else
                              --   its first prompt, else the short id
  , anState    :: Text        -- ^ @waiting@ | @busy@ | @idle@ | @gone@
  , anDetail   :: Text        -- ^ what it is doing, spelled out (row tooltip)
  , anDir      :: FilePath    -- ^ its working directory
  , anDesc     :: Maybe Text  -- ^ sanitized HTML, shown while expanded
  , anAge      :: Text        -- ^ when an exited agent was last active ("2h ago")
  , anLive     :: Bool        -- ^ False = exited (dimmed; click resumes it)
  , anBranch   :: Maybe Text  -- ^ the branch its checkout is on — the useful
                              --   label when agents work in worktrees
  , anPr       :: Maybe (Int, Text)
                              -- ^ its branch's open PR as @(number, url)@
  , anChildren :: [AgentNode] -- ^ the agents it forked
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- the store

-- The sidecar, lazily loaded, behind one MVar (the ClaudeQueue/Browser sidecar
-- pattern).  Writers are socket connections, readers the panes' polls, and both
-- can be several at once.
{-# NOINLINE infoVar #-}
infoVar :: MVar (Maybe (Map Text AgentInfo))
infoVar = unsafePerformIO (newMVar Nothing)

infoPath :: IO FilePath
infoPath = getConfigFilePathForSave "agents.json"

loadInfos :: IO (Map Text AgentInfo)
loadInfos = (`catch` \(_ :: SomeException) -> return M.empty) $ do
  p  <- infoPath
  ex <- doesFileExist p
  if not ex then return M.empty else do
    is <- fromMaybe [] . decode' <$> LBS.readFile p
    return $ M.fromList [ (aiSession i, i) | i <- is ]

saveInfos :: Map Text AgentInfo -> IO ()
saveInfos m = (`catch` \(_ :: SomeException) -> return ()) $ do
  p <- infoPath
  LBS.writeFile p (encode (M.elems m))

-- | Update the (lazily loaded) store and persist it.
withInfos :: (Map Text AgentInfo -> (Map Text AgentInfo, a)) -> IO a
withInfos f = modifyMVar infoVar $ \m0 -> do
  m <- maybe loadInfos return m0
  let (m', a) = f m
  saveInfos m'
  return (Just m', a)

readInfos :: IO (Map Text AgentInfo)
readInfos = withInfos (\m -> (m, m))

-- | Remember that @parent@ forked @child@ (in @dir@) with this first prompt —
-- called by 'IDE.Web.Agent.forkAgent' the moment the child's pane exists, since
-- this is the only point at which any of it is knowable.
--
-- The prompt becomes the child's provisional title (until it describes itself),
-- and it is the only honest label available: a @--fork-session@ child inherits
-- the parent's @\/rename@ name AND a copy of its transcript, so both the name
-- and the "first prompt" fallback would show the PARENT's — two rows reading
-- identically, which is exactly what you don't want in a tree of forks.
recordAgentFork :: Maybe Text -> Text -> FilePath -> Maybe Text -> IO ()
recordAgentFork parent child dir mprompt = do
  now <- getCurrentTime
  withInfos $ \m ->
    let old = M.lookup child m
    in ( M.insert child (blank child dir)
           { aiParent = parent
           , aiTitle  = maybe (fmap firstLine mprompt) Just (old >>= aiTitle)
           , aiDesc   = old >>= aiDesc
           , aiBorn   = Just (realToFrac (utcTimeToPOSIXSeconds now)) } m
       , () )
  where firstLine = T.take 70 . T.strip . T.takeWhile (/= '\n')

-- | Record what an agent says it is doing: a short title and\/or an HTML
-- description (sanitized here, once, so every reader can trust the store).
-- Either may be omitted to leave that half alone.  Creates the entry if leksah
-- has never seen this session before — a top-level session nobody forked can
-- describe itself too.
describeAgent :: Text -> Maybe Text -> Maybe Text -> IO Text
describeAgent sid mtitle mhtml = do
  live <- claudeLiveBySession
  now  <- getCurrentTime
  let dir = maybe "" clDir (M.lookup sid live)
      stamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" now)
  withInfos $ \m ->
    let cur  = fromMaybe (blank sid dir) (M.lookup sid m)
        cur' = cur { aiTitle = maybe (aiTitle cur) (Just . T.strip) mtitle
                   , aiDesc  = maybe (aiDesc cur) (Just . sanitizeAgentHtml) mhtml
                   , aiWhen  = Just stamp
                   , aiDir   = if null (aiDir cur) then dir else aiDir cur }
    in ( M.insert sid cur' m
       , T.unlines $
           [ "Recorded. The Agents pane now shows " <> sid <> " as:" ]
           <> [ "  title: " <> t | Just t <- [aiTitle cur'] ]
           <> [ "  description: " <> T.take 200 d | Just d <- [aiDesc cur'] ] )

-- | Forget an agent (the pane's ✕ on an exited row).  Its children re-attach to
-- ITS parent rather than disappearing — see 'agentForest'.
dismissAgent :: Text -> IO ()
dismissAgent sid = withInfos $ \m -> (M.delete sid m, ())

blank :: Text -> FilePath -> AgentInfo
blank sid dir = AgentInfo
  { aiSession = sid, aiParent = Nothing
  , aiDir = dropTrailingPathSeparator dir
  , aiTitle = Nothing, aiDesc = Nothing, aiWhen = Nothing, aiBorn = Nothing }

--------------------------------------------------------------------------------
-- the tree

-- | How long an exited agent stays in the pane, and how many at most.  Kept so
-- a finished agent's description is still readable, bounded so the pane (and the
-- sidecar) can't silently accumulate history.
exitedMaxAge :: Double
exitedMaxAge = 24 * 3600

exitedMaxRows :: Int
exitedMaxRows = 20

-- | How long a just-started agent may go without registering a session before we
-- stop showing it.  It covers the gap between the pane appearing and the CLI
-- writing its session file — and the case where it never will, because it is
-- sitting on a first-run question only a human can answer (see
-- 'IDE.Web.Agent.agentStatus'\'s @starting@).
startingMaxAge :: Double
startingMaxAge = 600

-- | Which checkout an agent is working in, as the pane labels it: the branch,
-- and its open PR if there is one.  Per-directory and time-limited
-- ('gitTtl') because the underlying calls are @git@ subprocesses and — for the
-- PR — a network round trip, while the pane asks every couple of seconds.
-- ('prForBranch' has its own 30s cache, but only *after* three git calls.)
{-# NOINLINE gitRef #-}
gitRef :: IORef (Map FilePath (Double, (Maybe Text, Maybe (Int, Text))))
gitRef = unsafePerformIO (newIORef M.empty)

gitTtl :: Double
gitTtl = 20

agentGit :: Double -> FilePath -> IO (Maybe Text, Maybe (Int, Text))
agentGit now dir
  | null dir || isRemotePath dir = return (Nothing, Nothing)
  | otherwise = do
      cached <- M.lookup dir <$> readIORef gitRef
      case cached of
        Just (t, v) | now - t < gitTtl -> return v
        _ -> (`catch` \(_ :: SomeException) -> return (Nothing, Nothing)) $ do
          v <- (,) <$> gitCurrentBranch dir <*> prForBranch dir
          atomicModifyIORef' gitRef (\m -> (M.insert dir (now, v) m, ()))
          return v

-- | The agent tree, roots first: every live session, plus recently-exited
-- agents leksah knows something about, arranged by who forked whom.
--
-- Cheap on purpose (the pane polls it every couple of seconds): the live half
-- comes from 'claudeStatusNow' — the 3s status poll's 'IORef', already read for
-- the traffic light and the menu-bar item, so this adds no directory listing and
-- no @ps@.  The rest is bounded by caches: 'agentGit' per directory (agents
-- share a checkout, so a fork costs nothing extra), one
-- 'getModificationTime' per exited candidate, and a 'claudeSessionLabel' for any
-- that never titled itself.
agentForest :: IO [AgentNode]
agentForest = (`catch` \(_ :: SomeException) -> return []) $ do
  st    <- claudeStatusNow
  infos <- readInfos
  now   <- getCurrentTime
  let posix = realToFrac (utcTimeToPOSIXSeconds now)
      liveRows = M.fromList [ (csrSession r, r) | r <- csRows st ]
  exited <- exitedNodes now infos liveRows
  liveNodes <- forM (csRows st) $ \r -> do
    (br, pr) <- agentGit posix (T.unpack (csrDir r))
    let i = M.lookup (csrSession r) infos
    return (csrSession r, (node r i) { anBranch = br, anPr = pr })
  let nodes = M.fromList (liveNodes <> exited)
      -- The parent of x, skipping over agents that aren't shown (dismissed, or
      -- exited long ago) so their children stay visible under the nearest
      -- ancestor that IS.  Bounded, like claudeLiveOwners' walk, so a cycle in
      -- a hand-edited file can't spin.
      parentOf = go (12 :: Int)
        where go 0 _ = Nothing
              go n x = M.lookup x infos >>= aiParent >>= \p ->
                if p `M.member` nodes && p /= x then Just p else go (n - 1) p
      kids p = ordered [ n | (s, n) <- M.toList nodes, parentOf s == Just p ]
      build seen s n
        | s `elem` seen = n { anChildren = [] }
        | otherwise     = n { anChildren =
            [ build (s : seen) (anSession k) k | k <- kids s ] }
  return [ build [] (anSession n) n
         | n <- ordered [ n | (s, n) <- M.toList nodes, isNothing (parentOf s) ] ]
  where
    -- The order every status surface uses: needs-you first, then working, then
    -- idle, then exited; alphabetical within a group.
    ordered = sortOn (\n -> (rank (anState n), T.toLower (anTitle n)))
    rank = \case
      "waiting" -> 0 :: Int
      "busy"    -> 1
      "shell"   -> 1
      "idle"    -> 2
      _         -> 3

    node r i = AgentNode
      { anSession = csrSession r
      , anTitle   = firstNonEmpty [ maybe "" T.strip (i >>= aiTitle)
                                  , csrTitle r
                                  , T.take 8 (csrSession r) ]
      , anState   = csrState r
      , anDetail  = csrDetail r
      , anDir     = T.unpack (csrDir r)
      , anDesc    = i >>= aiDesc
      , anAge     = ""
      , anLive    = True
      -- Filled in by the caller, which does the (cached) git lookup.
      , anBranch  = Nothing
      , anPr      = Nothing
      , anChildren = [] }

    -- Stored agents that are NOT running: either exited (their transcript's mtime
    -- is when they last did anything) or not yet registered — a pane leksah just
    -- made, which may be stuck on a first-run question.  Newest first, capped.
    -- No transcript and not recently born means nothing is left to look at.
    exitedNodes now infos liveRows = do
      let posix = realToFrac (utcTimeToPOSIXSeconds now) :: Double
      cands <- fmap (mapMaybe id) . forM (M.elems infos) $ \i ->
        if aiSession i `M.member` liveRows || null (aiDir i) then return Nothing
        else (`catch` \(_ :: SomeException) -> return Nothing) $ do
          p  <- claudeTranscriptPath (aiDir i) (aiSession i)
          ex <- doesFileExist p
          if not ex then return (starting now i) else do
            t <- getModificationTime p
            let age = realToFrac (diffUTCTime now t) :: Double
            return $ if age > exitedMaxAge then Nothing else Just (Right (t, i))
      startedNodes <- forM [ i | Left i <- cands ] $ \i -> do
        (br, pr) <- agentGit posix (aiDir i)
        return . (,) (aiSession i) $ AgentNode
          { anSession = aiSession i
          , anTitle   = firstNonEmpty [ maybe "" T.strip (aiTitle i)
                                      , T.take 8 (aiSession i) ]
          , anState   = "starting"
          , anDetail  = "Starting — if it stays like this, look at its pane: it \
                        \is probably waiting at a first-run prompt"
          , anDir     = aiDir i
          , anDesc    = aiDesc i
          , anAge     = "starting…"
          , anLive    = False
          , anBranch  = br
          , anPr      = pr
          , anChildren = [] }
      goneNodes <- forM (take exitedMaxRows (sortOn (Down . fst) [ x | Right x <- cands ])) $ \(t, i) -> do
        title <- case aiTitle i of
          Just x | not (T.null (T.strip x)) -> return (T.strip x)
          _ -> fromMaybe "" <$> claudeSessionLabel (aiDir i) (aiSession i)
        (br, pr) <- agentGit posix (aiDir i)
        return . (,) (aiSession i) $ AgentNode
          { anSession = aiSession i
          , anTitle   = firstNonEmpty [ T.takeWhile (/= '\n') title
                                      , T.take 8 (aiSession i) ]
          , anState   = "gone"
          , anDetail  = "Exited — click to resume it here"
          , anDir     = aiDir i
          , anDesc    = aiDesc i
          , anAge     = humanAge now t
          , anLive    = False
          , anBranch  = br
          , anPr      = pr
          , anChildren = [] }
      return (startedNodes <> goneNodes)

    -- A fork whose session file has not appeared yet, while that is still
    -- plausible: 'Left' it into the same candidate list.
    starting now i = case aiBorn i of
      Just b | realToFrac (utcTimeToPOSIXSeconds now) - b < startingMaxAge ->
                 Just (Left i)
      _ -> Nothing

    firstNonEmpty xs = fromMaybe "" (listToMaybe (filter (not . T.null) xs))

humanAge :: UTCTime -> UTCTime -> Text
humanAge now t
  | mins  < 1    = "just now"
  | mins  < 60   = T.pack (show (round mins :: Int)) <> "m ago"
  | hours < 24   = T.pack (show (round hours :: Int)) <> "h ago"
  | otherwise    = T.pack (show (round (hours / 24) :: Int)) <> "d ago"
  where
    mins  = realToFrac (diffUTCTime now t) / 60 :: Double
    hours = mins / 60

--------------------------------------------------------------------------------
-- asking an agent to describe itself

-- | What the pane's ⟳ button sends to an agent (submitted, so it lands as a
-- real turn and wakes an idle one).  Spells out the shape wanted, because the
-- reader is a model and the pane is four lines wide.
agentRefreshPrompt :: Text
agentRefreshPrompt = T.intercalate "\n"
  [ "Leksah's Agents pane is asking you to refresh how you appear in it. Do \
    \just this, then stop: call the describe_agent tool (or run `leksah-cmd \
    \agent describe --title '…' --html '…'`)."
  , ""
  , "The title is a few words naming what you are working on. The description \
    \is an HTML fragment that renders in about FOUR LINES in a narrow side \
    \pane, so keep it terse and avoid long unbreakable strings: say what state \
    \the work is in, and include <a href=\"…\">links</a> to any pull request, \
    \CI/Hydra build or issue involved. Allowed tags: p, br, a, code, b, \
    \strong, i, em, ul, li, span."
  ]

--------------------------------------------------------------------------------
-- sanitizing what it answers with

-- | The tags an agent's description may use.  Everything else is dropped
-- (keeping its text): a description is a sentence with links, not a document.
allowedTags :: [Text]
allowedTags =
  [ "p", "br", "a", "code", "b", "strong", "i", "em", "ul", "ol", "li"
  , "span", "small" ]

-- | Tags whose CONTENT goes too, rather than surviving as visible text.  The
-- markup can't run once the tag is gone, but @\<script\>alert(1)\<\/script\>@
-- reading as the words "alert(1)" in the pane is nobody's idea of a description.
strippedTags :: [Text]
strippedTags = [ "script", "style", "template", "svg", "math" ]

-- | Make an agent-authored HTML fragment safe to hand to @innerHTML@.
--
-- Whitelist tags, drop every attribute except @href@ on @\<a\>@, and require an
-- @http@ \/ @https@ \/ @mailto@ scheme.  The one rewrite that matters:
-- @href@ becomes @data-href@ (plus a marker class), so the anchor is inert —
-- clicking it can never navigate the page, and the pane's own click handler
-- decides where the URL opens.  That also sidesteps jsaddle-wkwebview's
-- asynchronous event dispatch, where a @preventDefault@ from Haskell is too
-- late to stop a real link.
sanitizeAgentHtml :: Text -> Text
sanitizeAgentHtml = T.take 4096 . go
  where
    go t = case T.break (== '<') t of
      (before, rest) | T.null rest -> before
                     | otherwise   -> before <> tag (T.drop 1 rest)

    -- At the character after '<'.  Everything up to the matching '>' is one
    -- tag; if there is no '>' the fragment is truncated, so stop.
    tag t = case T.break (== '>') t of
      (_, rest) | T.null rest -> ""
      (inner, rest)
        | Just nm <- tagName inner, not (isClosing inner), nm `elem` strippedTags ->
            go (skipContent nm (T.drop 1 rest))
        | otherwise -> emit inner <> go (T.drop 1 rest)

    -- Everything up to (and including) @\</nm …\>@.  Case-insensitive by
    -- searching a lowercased copy and cutting the original at the same offset.
    skipContent nm s =
      let (pre, post) = T.breakOn ("</" <> nm) (T.toLower s)
      in if T.null post then ""
         else T.drop 1 (T.dropWhile (/= '>') (T.drop (T.length pre) s))

    isClosing inner = "/" `T.isPrefixOf` T.stripStart inner
    tagBody   inner = T.dropWhile (== '/') (T.stripStart inner)
    tagName   inner = case T.takeWhile isAlphaNum (tagBody inner) of
      n | T.null n  -> Nothing
        | otherwise -> Just (T.toLower n)

    emit inner
      | Just nm <- tagName inner, nm `elem` allowedTags =
          if isClosing inner then "</" <> nm <> ">"
          else "<" <> nm <> attrs nm <> (if selfClose then "/>" else ">")
      | otherwise = ""
      where
        selfClose = "/" `T.isSuffixOf` T.stripEnd inner && not (isClosing inner)
        attrs "a" = case href (T.dropWhile isAlphaNum (tagBody inner)) of
          Just u  -> " class=\"agent-link\" data-href=\"" <> u <> "\""
          Nothing -> ""
        attrs _ = ""

    -- The href of an attribute list, if it has a scheme we are willing to open.
    href s = case attrValue "href" s of
      Just v | ok (T.toLower (T.strip v)) -> Just (escapeAttr (T.strip v))
      _ -> Nothing
      where ok v = any (`T.isPrefixOf` v) ["http://", "https://", "mailto:"]

    -- A quoted (or bare) attribute value, scanning attributes left to right so
    -- an earlier attribute's text can't be mistaken for the name.
    attrValue want = scan
      where
        scan s0 =
          let s = T.dropWhile (\c -> isSpace c || c == '/') s0
              (nm, r0) = T.span (\c -> isAlphaNum c || c `elem` ("-_:" :: String)) s
          in if T.null nm then Nothing else
             case T.uncons (T.dropWhile isSpace r0) of
               Just ('=', r1) ->
                 let (v, r2) = value (T.dropWhile isSpace r1)
                 in if T.toLower nm == want then Just v else scan r2
               _ -> scan r0
        value s = case T.uncons s of
          Just (q, r) | q == '"' || q == '\'' ->
            let (v, r') = T.break (== q) r in (v, T.drop 1 r')
          _ -> T.break isSpace s

    escapeAttr = T.replace "\"" "%22" . T.replace "<" "%3C" . T.replace ">" "%3E"
