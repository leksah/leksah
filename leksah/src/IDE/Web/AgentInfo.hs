{-# LANGUAGE CPP #-}
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
  , recordRemoteFork
  , remoteAgentRecord
  , applyLoggedCommand
  , setRemoteSuspended
  , describeAgent
  , dismissAgent
  , markAgentDone
  , setAgentRole
  , titleForClaude
  , sessionCli
  , cliTranscriptOf
  , agentsHeld
  , setAgentSuspended
  , Interruption(..)
  , syncAgentLiveness
  , interruptedOffer
  , latestInterruption
  , acknowledgeInterrupted
  , setAgentManager
  , agentManager
  , agentInfoOf
  , moveAgentRecord
  , agentParentOf
  , agentForest
  , agentForestShared
  , agentTitles
  , agentTitleSources
  , stripTags
  , agentDirOf
  , agentRefreshPrompt
  , sanitizeAgentHtml
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_, withMVar)
import Control.Exception (catch, SomeException)
import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, mfilter, unless, void, when)

import Data.Aeson
       (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject,
        encode, decode')
#if defined(ghcjs_HOST_OS)
import Data.Aeson (eitherDecodeStrict)
import Data.IORef (atomicWriteIORef)
import Data.Text.Encoding (encodeUtf8)
import Language.Javascript.JSaddle (eval, valToText)
#endif
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isSpace)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (nub, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
       (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import System.Directory (doesFileExist, getModificationTime)
import System.FilePath (dropTrailingPathSeparator, takeFileName)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Paths (sidecarPath, writeSidecar)
import IDE.Web.SharedStore (SharedStore, atomicWriteFile, newSharedStore, peekStore, modifyStore, remoteModifyJSON)
import IDE.Utils.RemotePath (isRemotePath, renderRemotePath)
import IDE.Web.AgentEvents
       (SessionEvents(..), seCompactingAt, seSubagentsAt, seTaskSummary, seTool, sessionEvents)
import IDE.Web.AgentUsage (SessionUsage, sessionUsage)
import IDE.Web.AgentHost (Host(..), RanOn(..), currentHost, diedWithHost, ranOnFor)
import IDE.Web.AgentNames
       (agentContinuations, agentLastSeen, agentNames, holdAgentNames)
import IDE.Web.Claude
       (ClaudeLive(..), ClaudeBackground(..), backgroundNeedsYou, claudeBackgroundSessions,
        claudeFindTranscript, claudeLatestActivity, paneForSession,
        claudeLiveBySession, claudeSessionLabel, claudeTranscriptPath,
        TranscriptGist(..), cachedTranscriptGist, liveTranscriptPath)
import IDE.Web.Codex (codexTranscriptOf, isCodexSession)
import IDE.Web.Agy (agyTranscriptOf, isAgySession)
import IDE.Web.AgentCli (AgentCli(..), cliTag)
import IDE.Web.RemoteAgents (remoteActivity, remoteInfoBytes, remoteStartingSessions)
import IDE.Web.ClaudeStatus
       (ClaudeStatus(..), ClaudeStatusRow(..), TitleSource(..), chooseTitle, claudeStatusNow)
import IDE.Web.GitInfo (gitCurrentBranch, prForBranch)
import IDE.Web.WorktreeRegistry
       (SessionClaim(..), WorktreeInfo(..), claimsBySession, roleText)

-- | What leksah knows about one agent beyond what the CLI writes down.  Keyed
-- by session id — the only durable handle (pids and panes come and go, and a
-- @--resume@ keeps the id but loses the name).
data AgentInfo = AgentInfo
  { aiSession :: Text          -- ^ the agent's session id
  , aiParent  :: Maybe Text    -- ^ the session that forked it: the tree's edges
  , aiDir     :: FilePath      -- ^ its working directory (transcript / resume)
  , aiTitle   :: Maybe Text    -- ^ what it last called itself
  , aiTitleProvisional :: Maybe Bool
                               -- ^ @Just True@: 'aiTitle' is only the first line
                               --   of the prompt leksah started it with
                               --   ('recordAgentFork'), not a title it chose, so
                               --   the conversation's own title outranks it;
                               --   @Just False@: it chose it; 'Nothing': a record
                               --   from before this was kept (see
                               --   'IDE.Web.ClaudeStatus.chooseTitle')
  , aiCommandAt :: Maybe Double -- ^ when it last described itself or said it
                               --   was done (POSIX seconds), by whichever route:
                               --   a logged command older than this is already
                               --   applied, or superseded ('applyLoggedCommand')
  , aiInherited :: [Text]      -- ^ what a fork took over from the session it
                               --   copied — its name and titles at that moment
                               --   — so none of them is shown as the fork's own
  , aiDesc    :: Maybe Text    -- ^ its description, as sanitized HTML
  , aiWhen    :: Maybe Text    -- ^ when it last described itself (row tooltip)
  , aiBorn    :: Maybe Double  -- ^ when leksah started it (POSIX seconds) — so a
                               --   fork that has not registered a session yet
                               --   still shows as @starting@ rather than as
                               --   nothing at all
  , aiDone    :: Maybe Text    -- ^ its summary, when it said it was done
                               --   (@agent done@) — cleared once it gets busy
                               --   again
  , aiDoneAt  :: Maybe Double  -- ^ when it said so (POSIX seconds)
  , aiRole    :: Maybe Text    -- ^ the role it was started in (@--as ROLE@)
  , aiBio     :: Maybe Text    -- ^ its bio, as sanitized HTML: a few sentences on
                               --   what it has worked on and is good for — the
                               --   Team pane's card (@agent describe --bio@)
  , aiBioAt   :: Maybe Double  -- ^ when it last wrote the bio (POSIX seconds) —
                               --   how the manager spots a stale one
  , aiSuspended :: Maybe Double -- ^ when it was suspended (@agent suspend@): its
                               --   process ended on purpose, and it stays on the
                               --   team until it is resumed
  , aiManager :: Bool          -- ^ it is the workspace's manager agent
  , aiRanOn   :: Maybe RanOn   -- ^ it was RUNNING, there, when leksah last
                               --   looked ('syncAgentLiveness'); 'Nothing' once
                               --   it is known to have stopped
  , aiInterrupted :: Maybe Interruption
                               -- ^ it did not stop: its host went down under it
                               --   (a restart, leksah's tmux server dying) —
                               --   offered back until resumed or forgotten
  , aiTitleSent :: Maybe Text  -- ^ the title leksah last gave Claude Code for this
                               --   session (a hook's @sessionTitle@): when the
                               --   session's title is something else, the user
                               --   named it (@/rename@), and leksah leaves it be
  } deriving (Eq, Show)

-- | An agent taken down with its host: when leksah found out, and the host it
-- was running on (the agents that went down together share it — which is
-- what "Resume all" resumes).
data Interruption = Interruption
  { inAt   :: Double  -- ^ when leksah noticed (POSIX seconds)
  , inHost :: RanOn
  } deriving (Eq, Show)

instance ToJSON Interruption where
  toJSON i = object [ "at" .= inAt i, "host" .= inHost i ]

instance FromJSON Interruption where
  parseJSON = withObject "Interruption" $ \o -> Interruption <$> o .: "at" <*> o .: "host"

instance ToJSON AgentInfo where
  toJSON i = object
    [ "session" .= aiSession i, "parent" .= aiParent i, "dir" .= aiDir i
    , "title" .= aiTitle i, "desc" .= aiDesc i, "when" .= aiWhen i
    , "born" .= aiBorn i, "done" .= aiDone i, "doneAt" .= aiDoneAt i
    , "role" .= aiRole i, "bio" .= aiBio i, "bioAt" .= aiBioAt i
    , "suspended" .= aiSuspended i, "manager" .= aiManager i
    , "ranOn" .= aiRanOn i, "interrupted" .= aiInterrupted i
    , "titleSent" .= aiTitleSent i, "titleProvisional" .= aiTitleProvisional i
    , "inherited" .= aiInherited i, "commandAt" .= aiCommandAt i ]

instance FromJSON AgentInfo where
  parseJSON = withObject "AgentInfo" $ \o -> AgentInfo
    <$> o .: "session" <*> o .:? "parent" <*> (fromMaybe "" <$> o .:? "dir")
    <*> o .:? "title" <*> o .:? "titleProvisional" <*> o .:? "commandAt"
    <*> (fromMaybe [] <$> o .:? "inherited")
    <*> o .:? "desc" <*> o .:? "when" <*> o .:? "born"
    <*> o .:? "done" <*> o .:? "doneAt" <*> o .:? "role"
    <*> o .:? "bio" <*> o .:? "bioAt" <*> o .:? "suspended"
    <*> (fromMaybe False <$> o .:? "manager")
    <*> o .:? "ranOn" <*> o .:? "interrupted" <*> o .:? "titleSent"

-- | One row of the Agents pane: an agent, and the agents it forked.
data AgentNode = AgentNode
  { anSession  :: Text        -- ^ session id (the handle for show\/resume\/send)
  , anName     :: Text        -- ^ the name leksah gave it ("Wren" — see
                              --   "IDE.Web.AgentNames"), @""@ if it has none
  , anTitle    :: Text        -- ^ its own title, else its @\/rename@ name, else
                              --   its first prompt, else the short id
  , anState    :: Text        -- ^ @waiting@ | @busy@ | @idle@ | @gone@
  , anDetail   :: Text        -- ^ what it is doing, spelled out (row tooltip)
  , anDone     :: Maybe Text  -- ^ the summary it gave when it said it was done
  , anRole     :: Maybe Text  -- ^ the role it was started in, if any
  , anActivity :: Maybe Text  -- ^ what it is doing RIGHT NOW, in a few words —
                              --   its last tool call while busy, the prompt it
                              --   is stuck on while waiting; 'Nothing' at rest
  , anDir      :: FilePath    -- ^ its working directory
  , anDesc     :: Maybe Text  -- ^ sanitized HTML, shown while expanded
  , anAge      :: Text        -- ^ when an exited agent was last active ("2h ago")
  , anLive     :: Bool        -- ^ False = exited (dimmed; click resumes it)
  , anBranch   :: Maybe Text  -- ^ the branch its checkout is on — the useful
                              --   label when agents work in worktrees
  , anPr       :: Maybe (Int, Text)
                              -- ^ its branch's open PR as @(number, url)@
  , anWorktrees :: [Text]     -- ^ the worktree relationships it REGISTERED
                              --   (`agent register` \/ register_worktree),
                              --   pre-rendered one per line — shown under the
                              --   checkout line
  , anBio      :: Maybe Text  -- ^ its bio (sanitized HTML), if it wrote one
  , anManager  :: Bool        -- ^ it is the workspace's manager
  , anBorn     :: Maybe Double -- ^ when leksah started it (POSIX seconds)
  , anParentName :: Maybe Text -- ^ the name of the agent that started it
  , anChildNames :: [Text]    -- ^ the names of the agents it started
  , anEdits    :: [FilePath]  -- ^ files it edited (the hooks saw), newest first
  , anError    :: Maybe Text  -- ^ the API error its last turn ended on
  , anCwd      :: Maybe FilePath -- ^ where its shell is, when that is not 'anDir'
  , anApproval :: Maybe Text    -- ^ the approval it is waiting for (live, and
                                --   blocked on a permission prompt), in a few
                                --   words: the card offers Approve / Deny
  , anBackground :: Maybe Text  -- ^ a Claude Code BACKGROUND session's id (the
                                --   short one @claude rm@\/@attach@ take), for a
                                --   row that came from @claude agents@
  , anLastSaid :: Maybe Text  -- ^ the first line of the last thing it said
                              --   (from its transcript; live agents only)
  , anUsage    :: Maybe SessionUsage -- ^ its context use, cost and rate limits,
                                --   from its status line ("IDE.Web.AgentUsage")
  , anAgent    :: Text          -- ^ which CLI runs it ('cliTag': @claude@, @codex@, @agy@) (the
                                --   badge on the left of its face)
  , anChildren :: [AgentNode] -- ^ the agents it forked
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- the store

-- The sidecar, lazily loaded, behind one MVar (the ClaudeQueue/Browser sidecar
-- pattern).  Writers are socket connections, readers the panes' polls, and both
-- can be several at once.
-- Shared with every other leksah that shows this machine's agents
-- ("IDE.Web.SharedStore": locked, re-read whenever it changed).
{-# NOINLINE infoStore #-}
infoStore :: SharedStore (Map Text AgentInfo)
infoStore = unsafePerformIO $ newSharedStore "agents.json"
  (fmap (\is -> M.fromList [ (aiSession i, i) | i <- is ]) . decode')
  (encode . M.elems) M.empty

-- | Update the store from what is in the file now; written back only when it
-- changed.  (An unwritable store answers from what can be read, unchanged:
-- the record keeping must not take the Agents pane down with it.)
withInfos :: (Map Text AgentInfo -> (Map Text AgentInfo, a)) -> IO a
withInfos f = modifyStore infoStore (return . f)
  `catch` \(_ :: SomeException) -> snd . f <$> peekStore infoStore

readInfos :: IO (Map Text AgentInfo)
readInfos = peekStore infoStore

-- | The store WITHOUT writing it back ('withInfos' saves on every call, which is
-- the wrong price to pay on a poll that only looks).  Every writer goes through
-- the same 'MVar' in this process, so the cached map is current.
peekInfos :: IO (Map Text AgentInfo)
peekInfos = peekStore infoStore

-- | What each agent calls ITSELF (session → title), for the surfaces that list
-- sessions.  The name a session gives itself through @agent describe@ says what
-- it is FOR, so the traffic light's hover text, the macOS menu-bar item and the
-- Agents pane can all say the same thing — see 'IDE.Web.ClaudeStatus.summarize',
-- which is handed this and prefers it over a @\/rename@ name or a first prompt.
agentTitles :: IO (Map Text Text)
agentTitles = M.map tsTitle <$> agentTitleSources

-- | The text of a little sanitized HTML (a description, a bio), for a line on
-- a card, a filter or a search.  An inline tag (<code>, <b>, <a> …) is part of
-- the text around it, so it goes without a trace ("in <code>main</code>."
-- stays "in main."); any other (<p>, <br>, <li> …) separates words.  Runs of
-- spaces collapse, but each line stays a line: a card shows the first.
stripTags :: Text -> Text
stripTags = T.intercalate "\n" . filter (not . T.null) . map (T.unwords . T.words)
          . T.lines . go
  where
    go t = case T.breakOn "<" t of
      (a, rest)
        | T.null rest -> a
        | otherwise ->
            let (tg, after) = T.break (== '>') (T.drop 1 rest)
                name = T.toLower . T.takeWhile (\c -> c /= ' ' && c /= '/')
                     . T.dropWhile (== '/') $ tg
                gap  = if name `elem` inlineTags then "" else " "
            in a <> gap <> go (T.drop 1 after)
    inlineTags = ["code", "b", "i", "em", "strong", "a", "span", "kbd", "tt", "u", "s"]

-- | 'agentTitles' with where each came from, for 'IDE.Web.ClaudeStatus.chooseTitle':
-- whether it is provisional (only the prompt leksah launched the agent with)
-- and what a fork inherited.
agentTitleSources :: IO (Map Text TitleSource)
agentTitleSources = do
  local  <- peekInfos
  remote <- remoteInfos
  return (M.mapMaybe titleSourceOf (local `M.union` remote))

-- | What the agent stores of the hosts whose leksah tmux server this leksah
-- shares say about their agents ("IDE.Web.RemoteAgents"), decoded once per
-- change of each.  Read-only here: a remote agent's record is its own
-- machine's to change.
remoteInfos :: IO (Map Text AgentInfo)
remoteInfos = (`catch` \(_ :: SomeException) -> return M.empty) $ do
  hosts <- remoteInfoBytes
  ms <- forM hosts $ \(h, bytes) -> do
    cached <- M.lookup h <$> readIORef remoteInfosRef
    case cached of
      Just (b, m) | b == bytes -> return m
      _ -> do
        let m = M.fromList [ (aiSession i, i) | i <- fromMaybe [] (decode' bytes) ]
        atomicModifyIORef' remoteInfosRef (\c -> (M.insert h (bytes, m) c, ()))
        return m
  return (M.unions ms)

-- | Which shared host's store holds a record of @sid@, and the record — an
-- agent on that machine that is not running now (suspended, or ended).
remoteAgentRecord :: Text -> IO (Maybe (Text, AgentInfo))
remoteAgentRecord sid = do
  _ <- remoteInfos   -- (decode any store that changed)
  m <- readIORef remoteInfosRef
  return $ listToMaybe [ (h, i) | (h, (_, im)) <- M.toList m, Just i <- [M.lookup sid im] ]

-- | A row with nothing known about it but its session: the base the rows
-- built from a record (not a live session) fill in.
blankAgentNode :: Text -> AgentNode
blankAgentNode sid = AgentNode
  { anSession = sid, anName = "", anTitle = T.take 8 sid, anState = "gone"
  , anDetail = "", anActivity = Nothing, anDone = Nothing, anRole = Nothing
  , anDir = "", anDesc = Nothing, anAge = "", anLive = False
  , anBranch = Nothing, anPr = Nothing, anWorktrees = []
  , anBio = Nothing, anManager = False, anBorn = Nothing
  , anParentName = Nothing, anChildNames = []
  , anEdits = [], anError = Nothing, anCwd = Nothing, anUsage = Nothing
  , anBackground = Nothing, anApproval = Nothing, anLastSaid = Nothing
  , anAgent = "claude", anChildren = [] }

-- | The first of these that is not empty (else empty).
firstNonEmpty :: [Text] -> Text
firstNonEmpty xs = fromMaybe "" (listToMaybe (filter (not . T.null) xs))

-- | The agents on the shared hosts that are not running now but can be
-- brought back — suspended, or interrupted by their host going down — as
-- Agents-pane rows (and the Workspace tree's server nodes'), from each
-- host's own store; their directory is the @ssh://@ one.  (Not the ones that
-- simply ended: their transcripts are on the host, so how long ago is not
-- known here.)
remoteRestingNodes :: Double -> Map Text a -> S.Set Text -> IO [(Text, AgentNode)]
remoteRestingNodes posix live hidden = do
  _ <- remoteInfos   -- (decode any store that changed)
  m <- readIORef remoteInfosRef
  seen <- remoteTitles
  return
    [ (aiSession i, n)
    | (h, (_, im)) <- M.toList m, i <- M.elems im
    , not (aiSession i `M.member` live), not (aiSession i `S.member` hidden)
    , let suspended = isJust (aiSuspended i)
    , suspended || isInterrupted posix i
    , let n = (blankAgentNode (aiSession i))
            -- The title it showed while it ran (this leksah saw it), else
            -- its record's — one it chose, or the prompt it was started with.
            { anTitle   = firstNonEmpty [ M.findWithDefault "" (aiSession i) seen
                                        , maybe "" T.strip (aiTitle i)
                                        , T.take 8 (aiSession i) ]
            , anState   = if suspended then "suspended" else "interrupted"
            , anDetail  = (if suspended then "Suspended" else "Interrupted: " <> h <> " went down under it")
                          <> " — resuming brings it back on " <> h
            , anDone    = aiDone i
            , anRole    = aiRole i
            , anDir     = renderRemotePath h (aiDir i)
            , anDesc    = aiDesc i } ]

-- | The last title each remote agent showed while it ran — what its row
-- shows once it stops, when only the host's record of it is left.  Kept in
-- @remote-agent-titles.json@ too, so a restart does not lose them.
{-# NOINLINE remoteTitlesRef #-}
remoteTitlesRef :: IORef (Maybe (Map Text Text))
remoteTitlesRef = unsafePerformIO (newIORef Nothing)

remoteTitlesPath :: IO FilePath
remoteTitlesPath = sidecarPath "remote-agent-titles.json"

remoteTitles :: IO (Map Text Text)
remoteTitles = readIORef remoteTitlesRef >>= \case
  Just m  -> return m
  Nothing -> do
    m <- (`catch` \(_ :: SomeException) -> return M.empty) $ do
      bytes <- LBS.readFile =<< remoteTitlesPath
      return (fromMaybe M.empty (decode' bytes))
    atomicModifyIORef' remoteTitlesRef (\cur -> (Just (fromMaybe m cur), ()))
    return m

-- | Change the remembered titles — read, change and written under one lock,
-- so two forest computations at once can't drop each other's — and save
-- them when they changed.
modifyRemoteTitles :: (Map Text Text -> Map Text Text) -> IO ()
modifyRemoteTitles f = withMVar remoteTitlesLock $ \_ -> do
  m <- remoteTitles
  let m' = f m
  unless (m' == m) $ do
    atomicModifyIORef' remoteTitlesRef (const (Just m', ()))
    (`catch` \(_ :: SomeException) -> return ()) $
      remoteTitlesPath >>= \p -> atomicWriteFile p (encode m')

{-# NOINLINE remoteTitlesLock #-}
remoteTitlesLock :: MVar ()
remoteTitlesLock = unsafePerformIO (newMVar ())

noteRemoteTitle :: Text -> Text -> IO ()
noteRemoteTitle sid t = unless (T.null t) $ modifyRemoteTitles (M.insert sid t)

-- | Forget the titles of remote agents no host's store knows any more —
-- once there are enough to matter: until every shared host has been read,
-- @known@ is short, and pruning then would forget titles still wanted.
pruneRemoteTitles :: S.Set Text -> IO ()
pruneRemoteTitles known = do
  m <- remoteTitles
  when (M.size m > 500 && not (M.keysSet m `S.isSubsetOf` known)) $
    modifyRemoteTitles (`M.restrictKeys` known)

-- | The sessions started on a shared host that have not registered yet —
-- waiting at a first-run question in their pane there (most often: trust
-- this folder?) — as \"starting\" rows, so they can be found and answered
-- (Show brings the pane up).  Their title is the host's record of what they
-- were started for, if any.
remoteStartingNodes :: Map Text AgentInfo -> S.Set Text -> S.Set Text -> IO [(Text, AgentNode)]
remoteStartingNodes rinfos resting hidden = do
  ss <- remoteStartingSessions
  return
    [ (sid, (blankAgentNode sid)
        { anName    = name
        , anTitle   = firstNonEmpty [ maybe "" T.strip (M.lookup sid rinfos >>= aiTitle), "Starting\8230" ]
        , anState   = "starting"
        , anDetail  = "Starting — waiting at a first-run question in its pane on " <> h
                      <> " (most often: trust this folder?)"
        , anRole    = M.lookup sid rinfos >>= aiRole
        , anDir     = renderRemotePath h dir
        , anAge     = "starting\8230" })
    | (h, sid, dir, name) <- ss
    , not (sid `S.member` resting), not (sid `S.member` hidden) ]

-- | Mark an agent on @host@ suspended, or not, in THAT machine's store.
setRemoteSuspended :: Text -> Text -> Bool -> IO ()
setRemoteSuspended host sid on = do
  now <- realToFrac . utcTimeToPOSIXSeconds <$> getCurrentTime
  void . (`catch` \(_ :: SomeException) -> return (Left "")) $
    remoteModifyJSON host "agents.json" ([] :: [AgentInfo]) $ \is -> do
      let m  = M.fromList [ (aiSession i, i) | i <- is ]
          m' = M.adjust (\i -> i { aiSuspended = if on then Just now else Nothing }) sid m
      return (if m' == m then is else M.elems m', ())

{-# NOINLINE remoteInfosRef #-}
remoteInfosRef :: IORef (Map Text (LBS.ByteString, Map Text AgentInfo))
remoteInfosRef = unsafePerformIO (newIORef M.empty)

titleSourceOf :: AgentInfo -> Maybe TitleSource
titleSourceOf i = case T.strip <$> aiTitle i of
  Just t | not (T.null t) -> Just (TitleSource t (aiTitleProvisional i) (aiInherited i))
  _                       -> Nothing

-- | Where a session lives, as far as leksah remembers (its 'aiDir') — how a
-- worktree row resumes a session whose transcripts are in ANOTHER checkout.
agentDirOf :: Text -> IO (Maybe FilePath)
agentDirOf sid = do
  m <- peekInfos
  return $ case aiDir <$> M.lookup sid m of
    Just d | not (null d) -> Just d
    _                     -> Nothing

-- | Remember that @parent@ forked @child@ (in @dir@) with this first prompt —
-- called by 'IDE.Web.Agent.forkAgent' the moment the child's pane exists, since
-- this is the only point at which any of it is knowable.
--
-- The prompt becomes the child's provisional title (until it describes itself),
-- and it is the only honest label available: a @--fork-session@ child inherits
-- the parent's @\/rename@ name AND a copy of its transcript, so both the name
-- and the "first prompt" fallback would show the PARENT's — two rows reading
-- identically, which is exactly what you don't want in a tree of forks.
--
-- @mfrom@ is the session it was forked FROM (nothing for a @--fresh@ one): what
-- that session is called right now — its name, its own title, its
-- conversation's title — is what the fork starts out with, and is recorded as
-- inherited ('aiInherited'), never to be shown as the fork's own.
recordAgentFork :: Maybe Text -> Text -> FilePath -> Maybe Text -> Maybe Text -> IO ()
recordAgentFork parent child dir mprompt mfrom = do
  now <- getCurrentTime
  inherited <- maybe (return []) inheritedFrom mfrom
  withInfos $ \m ->
    let old = M.lookup child m
    in ( M.insert child (blank child dir)
           { aiParent = parent
           , aiTitle  = maybe (fmap firstLine mprompt) Just (old >>= aiTitle)
           , aiTitleProvisional = maybe (Just (isJust mprompt)) aiTitleProvisional
                                    (mfilter (isJust . aiTitle) old)
           , aiInherited = inherited
           , aiDesc   = old >>= aiDesc
           , aiBorn   = Just (realToFrac (utcTimeToPOSIXSeconds now)) } m
       , () )
  where firstLine = T.take 70 . T.strip . T.takeWhile (/= '\n')

-- | 'recordAgentFork' for an agent launched on another machine: the record
-- goes into THAT machine's store (its @agents.json@, written by compare-and-
-- swap — "IDE.Web.SharedStore"), where every leksah showing the machine reads
-- it.  @rdir@ is host-local.  Best effort: the agent runs either way.
recordRemoteFork :: Text -> Maybe Text -> Text -> FilePath -> Maybe Text -> Maybe Text
                 -> Bool -> IO ()
recordRemoteFork host parent child rdir mprompt mrole manager = do
  now <- realToFrac . utcTimeToPOSIXSeconds <$> getCurrentTime
  void . (`catch` \(_ :: SomeException) -> return (Left "")) $
    remoteModifyJSON host "agents.json" ([] :: [AgentInfo]) $ \is -> do
      let m  = M.fromList [ (aiSession i, i) | i <- is ]
          i' = (blank child rdir)
                 { aiParent = parent
                 , aiTitle  = T.take 70 . T.strip . T.takeWhile (/= '\n') <$> mprompt
                 , aiTitleProvisional = Just (isJust mprompt)
                 , aiRole   = mrole
                 , aiManager = manager
                 , aiBorn   = Just now }
          m' = M.insertWith (\_ o -> o) child i' m
      return (if m' == m then is else M.elems m', ())

-- | What a fork of session @src@ takes over from it: its @\/rename@ (or derived)
-- name, the titles leksah holds for it, and its conversation's title.
inheritedFrom :: Text -> IO [Text]
inheritedFrom src = (`catch` \(_ :: SomeException) -> return []) $ do
  live  <- claudeLiveBySession
  infos <- peekInfos
  let ml = M.lookup src live
      mi = M.lookup src infos
  conv <- case ml of
    Just l  -> liveTranscriptPath l >>= maybe (return Nothing) (fmap tgTitle . cachedTranscriptGist)
    Nothing -> case mi of
      Just i | not (null (aiDir i)) ->
        tgTitle <$> (claudeTranscriptPath (aiDir i) src >>= cachedTranscriptGist)
      _ -> return Nothing
  return . nub . filter (not . T.null) . map T.strip . catMaybes $
    [ ml >>= clName, ml >>= clDerivedName, mi >>= aiTitle, mi >>= aiTitleSent, conv ]

-- | Record what an agent says it is doing: a short title and\/or an HTML
-- description (sanitized here, once, so every reader can trust the store).
-- Either may be omitted to leave that half alone.  Creates the entry if leksah
-- has never seen this session before — a top-level session nobody forked can
-- describe itself too.
describeAgent :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO Text
describeAgent sid mtitle mhtml mbio = do
  live <- claudeLiveBySession
  now  <- getCurrentTime
  let dir = maybe "" clDir (M.lookup sid live)
      stamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" now)
      posix = realToFrac (utcTimeToPOSIXSeconds now) :: Double
  withInfos $ \m ->
    let cur  = fromMaybe (blank sid dir) (M.lookup sid m)
        cur' = describedAs stamp posix mtitle mhtml mbio cur
                 { aiDir = if null (aiDir cur) then dir else aiDir cur }
    in ( M.insert sid cur' m
       , T.unlines $
           [ "Recorded. The Agents pane now shows " <> sid <> " as:" ]
           <> [ "  title: " <> t | Just t <- [aiTitle cur'] ]
           <> [ "  description: " <> T.take 200 d | Just d <- [aiDesc cur'] ]
           <> [ "  bio: " <> T.take 200 b | Just b <- [aiBio cur'] ] )

-- | What @agent describe@ makes of a record (at @stamp@ \/ @posix@): its title,
-- description and bio, each one only when given.
describedAs :: Text -> Double -> Maybe Text -> Maybe Text -> Maybe Text -> AgentInfo -> AgentInfo
describedAs stamp posix mtitle mhtml mbio cur = cur
  { aiTitle = maybe (aiTitle cur) (Just . T.strip) mtitle
  , aiTitleProvisional = maybe (aiTitleProvisional cur) (const (Just False)) mtitle
  , aiDesc  = maybe (aiDesc cur) (Just . sanitizeAgentHtml) mhtml
  , aiBio   = maybe (aiBio cur) (Just . sanitizeAgentHtml) mbio
  , aiBioAt = maybe (aiBioAt cur) (const (Just posix)) mbio
  , aiWhen  = Just stamp
  , aiCommandAt = Just posix }

-- | An agent's @agent describe@ \/ @agent done@ that reached no leksah on its
-- own machine (@leksah-cmd@ put it on that machine's event log), applied to
-- that machine's store ('Nothing': this one's) by the leksah that read the log
-- ("IDE.Web.RemoteAgents").  @t@ is when the agent ran it; @sid@ the session it
-- is about (already resolved); @args@ its flags.  Idempotent: a log is read
-- again after a restart, and every leksah showing the machine reads it.
applyLoggedCommand :: Maybe Text -> Double -> Text -> Text -> [Text] -> IO ()
applyLoggedCommand mhost t verb sid args = do
  let posix = t
      stamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
                       (posixSecondsToUTCTime (realToFrac t)))
      flag f = listToMaybe [ v | (k, v) <- zip args (drop 1 args), k == f ]
      change = case verb of
        "describe" -> Just (describedAs stamp posix (flag "--title") (flag "--html") (flag "--bio"))
        "done"     -> Just (\i -> i { aiDone = flag "--summary" <|> Just ""
                                    , aiDoneAt = Just posix, aiCommandAt = Just posix })
        _          -> Nothing
      -- Applied once: not when the record has heard from the agent since
      -- (a command already applied, or a newer describe / done).
      apply upd m = case M.lookup sid m of
        Just i | maybe False (>= t) (aiCommandAt i) -> m
        cur -> M.insert sid (upd (fromMaybe (blank sid "") cur)) m
  forM_ change $ \upd -> void . (`catch` \(_ :: SomeException) -> return (Left "")) $
    case mhost of
      Nothing   -> Right <$> withInfos (\m -> (apply upd m, ()))
      Just host -> remoteModifyJSON host "agents.json" ([] :: [AgentInfo]) $ \is -> do
        let m  = M.fromList [ (aiSession i, i) | i <- is ]
            m' = apply upd m
        return (if m' == m then is else M.elems m', ())

-- | A session that carried on under a new id ('agentContinuations' — Claude
-- Code backgrounding it, say) is the same agent: what leksah knows about the
-- old id — its title, description, bio, role, manager flag, where it came
-- from — moves to the new id (filling in, never overwriting, what the new id
-- has already said), and the agents it started are re-pointed at the new id.
-- A no-op, without a write, when there is nothing to move.
migrateContinued :: IO ()
migrateContinued = do
  conts <- agentContinuations
  infos <- peekInfos
  let pending = any (`M.member` infos) (M.keys conts)
             || any (maybe False (`M.member` conts) . aiParent) (M.elems infos)
  when pending . withInfos $ \m -> (M.map (reparent conts) (moveAll conts m), ())
  where
    moveAll conts m = M.foldrWithKey move m conts
      where
        move old new acc = case M.lookup old acc of
          Nothing -> acc
          Just o  -> M.insert new (maybe o { aiSession = new } (`merge` o) (M.lookup new acc))
                              (M.delete old acc)
    reparent conts i =
      i { aiParent = (\p -> M.findWithDefault p p conts) <$> aiParent i }
    merge n o = n
      { aiParent  = aiParent n <|> aiParent o
      , aiDir     = if null (aiDir n) then aiDir o else aiDir n
      , aiTitle   = aiTitle n <|> aiTitle o
      , aiTitleProvisional = if isJust (aiTitle n) then aiTitleProvisional n
                                                   else aiTitleProvisional o
      , aiInherited = aiInherited n <> aiInherited o
      , aiDesc    = aiDesc n <|> aiDesc o
      , aiWhen    = aiWhen n <|> aiWhen o
      , aiBorn    = aiBorn o <|> aiBorn n
      , aiDone    = aiDone n <|> aiDone o
      , aiDoneAt  = aiDoneAt n <|> aiDoneAt o
      , aiRole    = aiRole n <|> aiRole o
      , aiBio     = aiBio n <|> aiBio o
      , aiBioAt   = aiBioAt n <|> aiBioAt o
      , aiManager = aiManager n || aiManager o }

-- | Move what leksah knows about session @old@ to @new@ at once (the hook's
-- path, see 'IDE.Web.Agent.agentHook'), re-pointing its children.  With
-- @keepStory@ False — a @/clear@, which starts the conversation over — only
-- WHO it is moves (parent, role, manager, birth, directory); its title,
-- description, bio and done summary described the conversation it cleared.
moveAgentRecord :: Bool -> Text -> Text -> IO ()
moveAgentRecord keepStory old new = withInfos $ \m ->
  let reparent i = i { aiParent = (\p -> if p == old then new else p) <$> aiParent i }
      forget i = i { aiTitle = Nothing, aiTitleProvisional = Nothing
                   , aiDesc = Nothing, aiWhen = Nothing
                   , aiBio = Nothing, aiBioAt = Nothing, aiDone = Nothing
                   , aiDoneAt = Nothing }
      moved = case M.lookup old m of
        Nothing -> m
        Just o  -> let o' = (if keepStory then id else forget) o { aiSession = new }
                   in M.insert new (maybe o' (\n -> n { aiParent = aiParent n <|> aiParent o'
                                                     , aiRole = aiRole n <|> aiRole o'
                                                     , aiManager = aiManager n || aiManager o'
                                                     , aiBorn = aiBorn o' <|> aiBorn n })
                                           (M.lookup new m))
                                (M.delete old m)
  in (M.map reparent moved, ())

-- | An agent saying it has finished (@agent done@), with what it would like
-- the user — and the agent that started it — to know.
markAgentDone :: Text -> Text -> IO ()
markAgentDone sid summary = do
  live <- claudeLiveBySession
  now  <- realToFrac . utcTimeToPOSIXSeconds <$> getCurrentTime
  let dir = maybe "" clDir (M.lookup sid live)
  withInfos $ \m ->
    let cur = fromMaybe (blank sid dir) (M.lookup sid m)
    in (M.insert sid cur { aiDone = Just summary, aiDoneAt = Just now, aiCommandAt = Just now } m, ())

clearAgentDone :: Text -> IO ()
clearAgentDone sid = withInfos $ \m ->
  (M.adjust (\i -> i { aiDone = Nothing, aiDoneAt = Nothing }) sid m, ())

-- | Mark an agent suspended (its process is about to be ended on purpose), or
-- clear the mark when it is resumed.
setAgentSuspended :: Text -> Bool -> IO ()
setAgentSuspended sid on = do
  now <- realToFrac . utcTimeToPOSIXSeconds <$> getCurrentTime
  live <- claudeLiveBySession
  let dir = maybe "" clDir (M.lookup sid live)
  withInfos $ \m ->
    let cur = fromMaybe (blank sid dir) (M.lookup sid m)
    in (M.insert sid cur { aiSuspended = if on then Just now else Nothing
                         -- (resumed — or suspended on purpose — it is no
                         -- longer waiting to be brought back)
                         , aiInterrupted = Nothing } m, ())

-- | Make @sid@ the workspace's manager (and nobody else).
setAgentManager :: Text -> FilePath -> IO ()
setAgentManager sid dir = withInfos $ \m ->
  let m' = M.map (\i -> i { aiManager = False }) m
      cur = fromMaybe (blank sid dir) (M.lookup sid m')
  in (M.insert sid cur { aiManager = True } m', ())

-- | The manager's session, if there is one.
agentManager :: IO (Maybe AgentInfo)
agentManager = listToMaybe . filter aiManager . M.elems <$> peekInfos

-- | Everything leksah remembers about one agent.
agentInfoOf :: Text -> IO (Maybe AgentInfo)
agentInfoOf sid = M.lookup sid <$> peekInfos

-- | Remember which role an agent was started in.
setAgentRole :: Text -> Text -> IO ()
setAgentRole sid role = withInfos $ \m ->
  (M.adjust (\i -> i { aiRole = Just role }) sid m, ())

-- | The title to give Claude Code for session @sid@ (a hook's @sessionTitle@),
-- if any: what the agent calls itself (@agent describe --title@), when that is
-- not already the session's title (@current@, the hook's @session_title@).
-- Never over a name the user chose: a session title leksah did not set
-- ('aiTitleSent') came from @/rename@ or @--name@, and stays.  Records what
-- it hands out; reads before writing, so the common no-change case (every
-- prompt) does not rewrite the store.
--
-- Only a title the agent gave ITSELF (@agent describe@, which sets 'aiWhen'):
-- a fork's record starts out titled with its prompt, which is no title.  And
-- a fork inherits its parent's session title: one that is the parent's (as
-- leksah set it, or its own title) is not the user's either.
titleForClaude :: Text -> Maybe Text -> IO (Maybe Text)
titleForClaude sid current = do
  m <- peekInfos
  case M.lookup sid m of
    Just i | isJust (aiWhen i)
           , Just t <- clean <$> aiTitle i, not (T.null t), Just t /= current'
           , isNothing current' || current' `elem` ours m i -> do
      withInfos $ \m' -> (M.adjust (\x -> x { aiTitleSent = Just t }) sid m', ())
      return (Just t)
    _ -> return Nothing
  where
    current' = mfilter (not . T.null) (T.strip <$> current)
    clean = T.take 80 . T.strip . T.takeWhile (/= '\n')
    -- The titles leksah is responsible for: what it set for this session,
    -- and — for a fork — what its parent was titled.
    ours m i = map Just $ catMaybes
      ( aiTitleSent i
      : [ t | Just p <- [aiParent i >>= (`M.lookup` m)]
            , t <- [aiTitleSent p, clean <$> aiTitle p] ] )

-- | The sessions leksah holds as suspended or interrupted — what its Agents
-- pane shows them as, whatever else may say they are doing (see
-- 'IDE.Web.ClaudeStatus.startClaudeStatusPoll').
--
-- Not one this leksah's user dismissed ('dismissAgent'): put out of sight, it
-- no longer keeps a name from being handed on.
agentsHeld :: IO [Text]
agentsHeld = do
  hidden <- peekStore hiddenStore `catch` \(_ :: SomeException) -> return S.empty
  (\m -> [ aiSession i | i <- M.elems m
                       , isJust (aiSuspended i) || isJust (aiInterrupted i)
                       , aiSession i `S.notMember` hidden ])
    <$> peekInfos

-- | The agent that started @sid@, if leksah saw it happen.
agentParentOf :: Text -> IO (Maybe Text)
agentParentOf sid = (aiParent =<<) . M.lookup sid <$> peekInfos

-- | Forget an agent (the pane's ✕ on an exited row).  Its children re-attach to
-- ITS parent rather than disappearing — see 'agentForest'.
--
-- Only for THIS leksah: the record is shared with every leksah that shows the
-- agent's machine, and one person putting an old agent out of sight must not
-- take it from everyone else's pane.  So it joins this leksah's own hidden
-- set ('hiddenStore'), which hides it only while it is not running.
dismissAgent :: Text -> IO ()
dismissAgent sid = modifyStore hiddenStore (\h -> return (S.insert sid h, ()))
  `catch` \(_ :: SomeException) -> return ()

-- | The agents this leksah's user put out of sight ('dismissAgent'): a local
-- sidecar, never shared.
{-# NOINLINE hiddenStore #-}
hiddenStore :: SharedStore (S.Set Text)
hiddenStore = unsafePerformIO $
  newSharedStore "agents-hidden.json" decode' encode S.empty

blank :: Text -> FilePath -> AgentInfo
blank sid dir = AgentInfo
  { aiSession = sid, aiParent = Nothing
  , aiDir = dropTrailingPathSeparator dir
  , aiTitle = Nothing, aiTitleProvisional = Nothing, aiCommandAt = Nothing, aiInherited = []
  , aiDesc = Nothing, aiWhen = Nothing, aiBorn = Nothing
  , aiDone = Nothing, aiDoneAt = Nothing, aiRole = Nothing
  , aiBio = Nothing, aiBioAt = Nothing, aiSuspended = Nothing, aiManager = False
  , aiRanOn = Nothing, aiInterrupted = Nothing, aiTitleSent = Nothing }

--------------------------------------------------------------------------------
-- liveness: which agents were running, and which of them were interrupted

-- | Can leksah reach session @sid@'s pane (to answer its prompt)?  A @ps@
-- walk, so remembered for 30s per session.
paneReachable :: Text -> IO Bool
paneReachable sid = do
  now <- posixNow
  cached <- M.lookup sid <$> readIORef reachRef
  case cached of
    Just (t, ok) | now - t < 30 -> return ok
    _ -> do
      ok <- isJust <$> (paneForSession sid `catch` \(_ :: SomeException) -> return Nothing)
      atomicModifyIORef' reachRef (\m -> (M.insert sid (now, ok) m, ()))
      return ok

{-# NOINLINE reachRef #-}
reachRef :: IORef (Map Text (Double, Bool))
reachRef = unsafePerformIO (newIORef M.empty)

-- | How long an interrupted agent is offered back (by its card, and by "Resume
-- all") before it becomes an ordinary exited one — the same week its name is
-- reserved for ("IDE.Web.AgentNames").
interruptedMaxAge :: Double
interruptedMaxAge = 7 * 24 * 3600

-- | An agent that vanished from leksah's tmux while that server still LOOKS
-- alive waits this long before it is called exited: when the server dies, its
-- panes' processes go a moment before it does, and the poll can land in
-- between.  (No grace is needed for anything else — see 'diedWithHost'.)
vanishGrace :: Double
vanishGrace = 4

isInterrupted :: Double -> AgentInfo -> Bool
isInterrupted now i = maybe False (\x -> now - inAt x < interruptedMaxAge) (aiInterrupted i)

posixNow :: IO Double
posixNow = realToFrac . utcTimeToPOSIXSeconds <$> getCurrentTime

-- | Like 'withInfos', but writes the sidecar only when something changed —
-- the liveness sync runs on every change of the live set.
withInfosChanged :: (Map Text AgentInfo -> (Map Text AgentInfo, a)) -> IO a
withInfosChanged = withInfos

-- | One pass of liveness tracking (serialised; the state is the vanished
-- agents still in their 'vanishGrace').  Run on every change of the live set
-- and once at startup:
--
--   * every running session gets a record — whoever started it, ＋, a
--     terminal, the resume picker — and 'aiRanOn' says where it runs;
--   * a recorded agent that is no longer running was either SUSPENDED, taken
--     down with its host ('diedWithHost': the machine rebooted, or leksah's
--     tmux server is gone — whichever of leksah and tmux went first), or it
--     EXITED on its own.  Only the second becomes 'aiInterrupted'.
--
-- Leksah being down when the agents went changes nothing: their records
-- still say they were running, and the first pass after startup decides.
syncAgentLiveness :: IO ()
syncAgentLiveness = (`catch` \(_ :: SomeException) -> return ()) $
  modifyMVar_ syncVar $ \pending -> do
    seedInterrupted
    live <- claudeLiveBySession
    host <- currentHost
    now  <- posixNow
    (pending', held) <- withInfosChanged $ \m0 ->
      let running l mi =
            let i = fromMaybe (blank (clSession l) (clDir l)) mi
            in i { aiRanOn = Just (ranOnFor host (clPid l))
                 , aiInterrupted = Nothing
                 , aiDir = if null (aiDir i) then dropTrailingPathSeparator (clDir l)
                                             else aiDir i }
          m1 = M.foldr (\l -> M.alter (Just . running l) (clSession l)) m0 live
          gone = [ (sid, i, r) | (sid, i) <- M.toList m1, Just r <- [aiRanOn i]
                               , sid `M.notMember` live ]
          stopped i = i { aiRanOn = Nothing }
          decide (m, pend) (sid, i, r)
            | isJust (aiSuspended i) = (M.insert sid (stopped i) m, pend)
            | diedWithHost host r    =
                (M.insert sid (stopped i) { aiInterrupted = Just (Interruption now r) } m, pend)
            | isJust (roTmux r)
            , let since = M.findWithDefault now sid pending
            , now - since < vanishGrace = (m, M.insert sid since pend)
            | otherwise              = (M.insert sid (stopped i) m, pend)
          (m2, pend') = foldl decide (m1, M.empty) gone
          heldSids = [ sid | (sid, i) <- M.toList m2
                           , isJust (aiSuspended i) || isInterrupted now i ]
      in (m2, (pend', heldSids))
    holdAgentNames held
    -- Look again once the grace is over, whether or not anything else changes.
    unless (M.null pending') . void . forkIO $ do
      threadDelay (round (vanishGrace * 1000000) + 500000)
      syncAgentLiveness
    return pending'

{-# NOINLINE syncVar #-}
syncVar :: MVar (Map Text Double)
syncVar = unsafePerformIO (newMVar M.empty)

-- | The other half of the record: which "Resume all" offers have been answered,
-- and whether the one-off recovery ('seedInterrupted') has run.
data HostsState = HostsState
  { hsSeeded       :: Bool
  , hsAcknowledged :: [RanOn]  -- ^ newest first, bounded
  } deriving (Eq, Show)

instance ToJSON HostsState where
  toJSON h = object [ "seeded" .= hsSeeded h, "acknowledged" .= hsAcknowledged h ]

instance FromJSON HostsState where
  parseJSON = withObject "HostsState" $ \o -> HostsState
    <$> (fromMaybe False <$> o .:? "seeded") <*> (fromMaybe [] <$> o .:? "acknowledged")

{-# NOINLINE hostsVar #-}
hostsVar :: MVar (Maybe HostsState)
hostsVar = unsafePerformIO (newMVar Nothing)

withHosts :: (HostsState -> (HostsState, a)) -> IO a
withHosts f = modifyMVar hostsVar $ \h0 -> do
  (h, known) <- maybe loadHosts (\h -> return (h, True)) h0
  let (h', a) = f h
  -- Only a state that came from the file (or from its absence) is cached and
  -- written back: one built on an UNREADABLE file (half-written at boot, a
  -- transient IO error) would overwrite the user's answered offers with an
  -- empty list.  That call works on the empty state and the next one
  -- retries the read.
  when (known && h' /= h) . (`catch` \(_ :: SomeException) -> return ()) $ do
    p <- hostsPath
    writeSidecar p (encode h')
  return (if known then Just h' else Nothing, a)

hostsPath :: IO FilePath
hostsPath = sidecarPath "agent-hosts.json"

-- | The stored hosts state, and whether it can be trusted: 'False' when the
-- file exists but couldn't be read or decoded.
loadHosts :: IO (HostsState, Bool)
loadHosts = (`catch` \(_ :: SomeException) -> return (emptyHosts, False)) $ do
  p  <- hostsPath
  ex <- doesFileExist p
  if not ex then return (emptyHosts, True) else
    maybe (emptyHosts, False) (\h -> (h, True)) . decode' <$> LBS.readFile p

emptyHosts :: HostsState
emptyHosts = HostsState False []

-- | The one-off recovery, for the restart that happened before leksah tracked
-- liveness: the agents whose names were last touched together, just before
-- this boot, were the ones running when it went down (the name store marks
-- every live session seen, within 10 minutes, on the status poll).  Runs once;
-- a no-op on a machine with nothing of the kind.
seedInterrupted :: IO ()
seedInterrupted = do
  done <- withHosts (\h -> (h { hsSeeded = True }, hsSeeded h))
  unless done . (`catch` \(_ :: SomeException) -> return ()) $ do
    host <- currentHost
    seen <- agentLastSeen
    live <- claudeLiveBySession
    now  <- posixNow
    infos <- peekInfos
    let tracked = M.filter (isJust . aiRanOn) infos
    case hBoot host of
      Just boot | M.null tracked -> do
        let before = [ (sid, t) | (sid, t) <- M.toList seen, t < boot
                                , sid `M.notMember` live ]
        unless (null before) $ do
          let newest  = maximum (map snd before)
              cluster = [ sid | (sid, t) <- before, t >= newest - 600
                              , now - t < interruptedMaxAge ]
              from = RanOn { roBoot = Nothing
                           , roTmux = Just ("before boot " <> T.pack (show (round boot :: Integer))) }
          dirs <- forM cluster $ \sid -> case M.lookup sid infos of
            Just i | not (null (aiDir i)) -> return (sid, Just (aiDir i))
            _ -> (,) sid <$> claudeFindTranscript sid
          withInfosChanged $ \m ->
            let add m' (sid, Just d) = flip (M.alter (Just . mark sid d)) m' sid
                add m' (_, Nothing)  = m'
                mark sid d mi = let i = fromMaybe (blank sid d) mi
                                in if isJust (aiSuspended i) then i
                                   else i { aiInterrupted = Just (Interruption now from) }
            in (foldl add m dirs, ())
      _ -> return ()

-- | The agents to offer back with "Resume all": those taken down together in
-- the most recent interruption, while that offer is unanswered.  Older
-- interruptions are never offered wholesale — their agents keep their own
-- cards until resumed, forgotten or a week old.
interruptedOffer :: IO (Maybe (RanOn, [Text]))
interruptedOffer = do
  acked <- withHosts (\h -> (h, hsAcknowledged h))
  latest <- latestInterruption
  return $ case latest of
    Just (host, _) | host `elem` acked -> Nothing
    other -> other

-- | The agents taken down together in the most recent interruption that still
-- has any left to resume, whether or not its offer was answered — what the ↻
-- filter's "Resume all" resumes (agents from older interruptions are not in
-- it: they keep only their own cards).
latestInterruption :: IO (Maybe (RanOn, [Text]))
latestInterruption = do
  now   <- posixNow
  infos <- peekInfos
  let ints = [ (x, aiSession i) | i <- M.elems infos, isInterrupted now i
                                , isNothing (aiSuspended i)
                                , Just x <- [aiInterrupted i] ]
  return $ case sortOn (Down . inAt . fst) ints of
    [] -> Nothing
    ((x0, _) : _) -> Just (inHost x0, [ sid | (x, sid) <- ints, inHost x == inHost x0 ])

-- | The offer for @host@ has been answered (resumed, or dismissed).
acknowledgeInterrupted :: RanOn -> IO ()
acknowledgeInterrupted host = withHosts $ \h ->
  (h { hsAcknowledged = take 20 (host : filter (/= host) (hsAcknowledged h)) }, ())

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

-- | 'agentForest', or the one it last computed if that is younger than
-- 'sharedForestAge' — for the panes that only show the forest (the Workspace
-- tree's server nodes, in every window), so they ride on the Agents pane's
-- poll, and on each other's, instead of each computing it again (it runs git
-- and writes to the stores).  Callers arriving together share one
-- computation: the second waits for the first, then finds its result fresh.
-- The Agents pane itself calls 'agentForest': it re-reads right after an
-- action, and must see what the action did.
agentForestShared :: IO [AgentNode]
#if defined(ghcjs_HOST_OS)
agentForestShared = agentForest
#else
agentForestShared = do
  let fresh = do
        now <- getCurrentTime
        readIORef lastForestRef >>= \case
          Just (at, ns) | diffUTCTime now at < sharedForestAge -> return (Just ns)
          _ -> return Nothing
  fresh >>= \case
    Just ns -> return ns
    Nothing -> withMVar sharedForestLock $ \_ -> fresh >>= maybe agentForest return

-- | Longer than the Workspace tree's poll period (5s), so one window's poll
-- serves the others'.
sharedForestAge :: NominalDiffTime
sharedForestAge = 6

{-# NOINLINE sharedForestLock #-}
sharedForestLock :: MVar ()
sharedForestLock = unsafePerformIO (newMVar ())

{-# NOINLINE lastForestRef #-}
lastForestRef :: IORef (Maybe (UTCTime, [AgentNode]))
lastForestRef = unsafePerformIO (newIORef Nothing)
#endif

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
#if defined(ghcjs_HOST_OS)
-- Browser demo: no Claude CLI and no sidecar, so the hosting page seeds a
-- canned forest via @window.leksahDemoAgents@ (same page-seeded pattern as
-- "IDE.Web.DemoTerminals").  Rows should be @live: true@ with states
-- @busy@\/@waiting@\/@idle@ — a @gone@ row's click path tries to spawn a
-- resume, while a live row's click\/⟳ find no pane and return silently.
-- Descriptions pass through 'sanitizeAgentHtml' at decode, so the pane's
-- innerHTML trust invariant holds for page-seeded data too.
agentForest = readIORef demoForestRef >>= \case
  Just ns -> return ns
  Nothing -> do
    txt <- valToText
        =<< eval ("JSON.stringify(window.leksahDemoAgents || [])" :: Text)
    ns <- case eitherDecodeStrict (encodeUtf8 txt) of
      Left err -> [] <$ putStrLn ("leksahDemoAgents did not decode: " <> err)
      Right ds -> return (map demoNode ds)
    atomicWriteIORef demoForestRef (Just ns)
    return ns

{-# NOINLINE demoForestRef #-}
demoForestRef :: IORef (Maybe [AgentNode])
demoForestRef = unsafePerformIO (newIORef Nothing)

-- | One page-seeded row (children nest the same shape).
data DemoAgent = DemoAgent Text Text Text Text FilePath (Maybe Text) Text
                           (Maybe Text) (Maybe (Int, Text)) [DemoAgent] (Maybe Text)
                           (Maybe Text) (Maybe Text)

instance FromJSON DemoAgent where
  parseJSON = withObject "DemoAgent" $ \o -> DemoAgent
    <$> o .:  "session"
    <*> o .:  "title"
    <*> o .:  "state"
    <*> (fromMaybe "" <$> o .:? "detail")
    <*> (fromMaybe "" <$> o .:? "dir")
    <*> o .:? "desc"
    <*> (fromMaybe "" <$> o .:? "age")
    <*> o .:? "branch"
    <*> o .:? "pr"
    <*> (fromMaybe [] <$> o .:? "children")
    <*> o .:? "name"
    <*> o .:? "activity"
    <*> o .:? "bio"

demoNode :: DemoAgent -> AgentNode
demoNode (DemoAgent s title state detail dir desc age br pr kids name activity bio) = AgentNode
  { anSession  = s
  , anName     = fromMaybe "" name
  , anTitle    = title
  , anState    = state
  , anDetail   = detail
  , anActivity = activity
  , anDone     = Nothing
  , anRole     = Nothing
  , anDir      = dir
  , anDesc     = sanitizeAgentHtml <$> desc
  , anAge      = age
  , anLive     = state /= "gone"
  , anBranch   = br
  , anPr       = pr
  , anWorktrees = []
  , anBio      = sanitizeAgentHtml <$> bio
  , anManager  = False
  , anBorn     = Nothing
  , anParentName = Nothing
  , anChildNames = map (\(DemoAgent _ _ _ _ _ _ _ _ _ _ nm _ _) -> fromMaybe "" nm) kids
  , anEdits = [], anError = Nothing, anCwd = Nothing, anUsage = Nothing, anBackground = Nothing, anApproval = Nothing, anLastSaid = Nothing, anAgent = "claude"
  , anChildren = map demoNode kids
  }
#else
agentForest = do
  ns <- computeAgentForest
  now <- getCurrentTime
  atomicModifyIORef' lastForestRef (const (Just (now, ns), ()))
  return ns

computeAgentForest :: IO [AgentNode]
computeAgentForest = (`catch` \(_ :: SomeException) -> return []) $ do
  migrateContinued
  st    <- claudeStatusNow
  infos <- readInfos
  now   <- getCurrentTime
  let posix = realToFrac (utcTimeToPOSIXSeconds now)
      -- (The status poll also carries a recently blocked BACKGROUND session
      -- as a row, so the dot and menu-bar item count it; it is not live here
      -- — its row comes from 'claudeBackgroundSessions' below.)
      rows = filter (not . csrBackground) (csRows st)
      liveRows = M.fromList [ (csrSession r, r) | r <- rows ]
  lastSeen <- agentLastSeen
  -- (what this leksah's user dismissed stays out of sight while it is not
  -- running — 'dismissAgent')
  hidden <- peekStore hiddenStore `catch` \(_ :: SomeException) -> return S.empty
  exited <- exitedNodes now (M.withoutKeys infos hidden) liveRows lastSeen
  -- Background sessions with no process (blocked on a question, finished,
  -- failed, stopped): no session file, so they are not in 'liveRows', but
  -- `claude agents` still lists them — one blocked is waiting for YOU.  Each
  -- replaces the exited row leksah would otherwise show for the same session.
  --
  -- Not every one, though:
  --   * one with a process is running — it shows up through its session file
  --     (if 'liveRows' has not seen it yet, the next poll will; as a non-live
  --     row a click would start a SECOND process on it);
  --   * leksah's own row wins when it says more: an agent interrupted by a
  --     restart, or suspended, keeps that state and its resume path;
  --   * finished, failed and stopped ones age out like exited agents
  --     ('exitedMaxAge').  A blocked one stays — it is still waiting on a
  --     decision — but sorts with the exited rows once stale, and Forget
  --     (`claude rm`) is how it goes.
  bgs <- claudeBackgroundSessions
  let ownWins = [ s | (s, n) <- exited, anState n `elem` ["interrupted", "suspended"] ]
  bgCands <- fmap catMaybes . forM
      [ b | b <- bgs, isNothing (cbPid b), not (cbSession b `M.member` liveRows)
          , cbSession b `notElem` ownWins ] $ \b -> do
    let active = cbLastActive b
        idleFor = maybe 0 (\t -> realToFrac (diffUTCTime now t)) active :: Double
        n = backgroundNode now active b (M.lookup (cbSession b) infos)
    if cbState b /= "blocked" && idleFor > exitedMaxAge then return Nothing else do
      (br, pr) <- agentGit posix (cbDir b)
      return (Just (cbSession b, n { anBranch = br, anPr = pr }))
  let bgNodes = bgCands
      bgIds = map fst bgNodes
  -- (an agent on another machine: what its own machine's store says of it)
  rinfos <- remoteInfos
  liveNodes <- forM rows $ \r -> do
    (br, pr) <- agentGit posix (T.unpack (csrDir r))
    act <- agentActivity r
    let i = M.lookup (csrSession r) (if isJust (csrHost r) then rinfos else infos)
        n0 = (node r i) { anBranch = br, anPr = pr, anActivity = act }
    -- (what a remote agent was called while it ran: its row keeps it when it
    -- stops — 'remoteRestingNodes' — where only the host's record is left)
    -- (not the short-id stand-in a row with no title at all shows)
    when (isJust (csrHost r) && anTitle n0 /= T.take 8 (csrSession r)) $
      noteRemoteTitle (csrSession r) (anTitle n0)
    -- Done ✓ while it rests after saying so; busy again a minute after
    -- `agent done` (the turn that said it is over by then) means it has been
    -- given more work, and the summary is stale.
    n <- case (aiDone =<< i, aiDoneAt =<< i) of
      (Just _, Just at)
        | csrState r == "idle" -> return n0 { anState = "done"
                                            , anDetail = "Done — it said it has finished" }
        | posix - at > 60      -> clearAgentDone (csrSession r) >> return n0 { anDone = Nothing }
      _ -> return n0
    return (csrSession r, n)
  -- Decorate every row with the worktree relationships its session registered
  -- (cheap: 'claimsBySession' is an MVar peek per node over a small map).
  -- (And with its name: an exited agent keeps the one it had.)
  -- And with what agents.json knows that the CLI does not: its bio, whether it
  -- is the manager, when it was born, and who started it.
  -- And with what the hooks have said about it ("IDE.Web.AgentEvents"): the
  -- approval it waits for, the API error its turn ended on, or the tool it is
  -- running — each a better activity line than the transcript can give —
  -- and the files it edited and where its shell went.
  names  <- agentNames
  events <- sessionEvents
  usage  <- sessionUsage
  -- (and the agents on shared hosts that are not running but can come back:
  -- suspended or interrupted, by their own machine's store)
  resting <- remoteRestingNodes posix liveRows hidden
  pruneRemoteTitles (M.keysSet rinfos <> S.fromList [ csrSession r | r <- rows, isJust (csrHost r) ])
  rstarting <- remoteStartingNodes rinfos (S.fromList (map fst resting)) hidden
  decorated <- forM (liveNodes <> filter ((`notElem` bgIds) . fst) exited <> bgNodes <> resting <> rstarting)
                 $ \(s, n) -> do
    wls <- worktreeClaimLines now s
    let i = M.lookup s infos
        e = M.lookup s events
        recentErr = e >>= seError
        activity0 = case anState n of
          "waiting" | Just (p, _) <- e >>= sePermission -> Just ("needs approval: " <> p)
          state | state `elem` ["busy", "shell"], Just (t, _) <- e >>= seTool -> Just t
          _ -> anActivity n
        -- Compacting its conversation says more than anything else it is
        -- doing; its subagents ride along with whatever that is.
        running = maybe [] (seSubagentsAt posix) e
        subs = nub (map (fst . snd) running)
        nSubs = length running
        subsText = T.pack (show nSubs) <> (if nSubs == 1 then " subagent" else " subagents")
                   <> " (" <> T.intercalate ", " subs <> ")"
        activity1
          | maybe False (seCompactingAt posix) e = Just "Compacting the conversation…"
          | nSubs == 0 = activity0
          | otherwise = Just $ maybe (subsText <> " running") (<> " · " <> subsText) activity0
        -- Its task list's progress leads: "[3/7] Running the tests · Bash: …".
        -- (Only while it works: an abandoned, half-done list must not stand
        -- in for an idle agent's own description for good.)
        working = anState n `elem` ["busy", "shell", "waiting"]
        activity = case if working then e >>= seTaskSummary else Nothing of
          Nothing -> activity1
          Just ts -> Just (maybe ts ((ts <> " · ") <>) activity1)
        pending = if anLive n && anState n == "waiting"
                    then fst <$> (e >>= sePermission) else Nothing
    -- Approve / Deny only where they can work: a pane leksah can reach (not
    -- a `claude` in iTerm, say).  Looked up only for a session that waits on
    -- an approval, and remembered a while.
    reachable <- maybe (return False) (const (paneReachable s)) pending
    -- A row that is not running says which CLI it was from leksah's own
    -- record of the Codex and Antigravity sessions it has seen.
    agent <- if anLive n || anBackground n /= Nothing then return (anAgent n)
             else cliTag <$> sessionCli s
    return (s, n { anWorktrees = wls
                 , anAgent = agent
                 , anName = M.findWithDefault (anName n) s names
                 , anBio = i >>= aiBio
                 , anManager = maybe False aiManager i
                 , anBorn = i >>= aiBorn
                 , anParentName = (i >>= aiParent) >>= (`M.lookup` names)
                 , anActivity = if anLive n then activity else anActivity n
                 , anError = if anLive n then fst <$> recentErr else Nothing
                 , anEdits = maybe [] (map fst . seEdits) e
                 , anUsage = M.lookup s usage
                 , anApproval = if reachable then pending else Nothing
                 , anCwd = e >>= seCwd >>= \c ->
                     if dropTrailingPathSeparator c == dropTrailingPathSeparator (anDir n)
                       then Nothing else Just c })
  let nodes = M.fromList decorated
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
        | otherwise     =
            let ks = [ build (s : seen) (anSession k) k | k <- kids s ]
            in n { anChildren = ks
                 , anChildNames = filter (not . T.null) (map anName ks) }
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
      "done"    -> 2
      "interrupted" -> 3
      _         -> 4

    node r i = AgentNode
      { anSession = csrSession r
      , anName    = csrName r
      -- A title the agent chose wins; a provisional one (its launch prompt)
      -- only through the poll's chain, behind the conversation's own title.
      , anTitle   = firstNonEmpty [ maybe "" T.strip
                                      (mfilter ((== Just False) . aiTitleProvisional) i >>= aiTitle)
                                  , csrTitle r
                                  , T.take 8 (csrSession r) ]
      , anState   = csrState r
      , anDetail  = csrDetail r
      , anActivity = Nothing
      , anDone    = i >>= aiDone
      , anRole    = i >>= aiRole
      , anDir     = T.unpack (csrDir r)
      , anDesc    = i >>= aiDesc
      , anAge     = ""
      , anLive    = True
      -- Filled in by the caller, which does the (cached) git lookup.
      , anBranch  = Nothing
      , anPr      = Nothing
      , anWorktrees = []
      , anBio = Nothing, anManager = False, anBorn = Nothing
      , anParentName = Nothing, anChildNames = []
      , anEdits = [], anError = Nothing, anCwd = Nothing, anUsage = Nothing, anBackground = Nothing, anApproval = Nothing, anAgent = csrAgent r, anLastSaid = csrLastSaid r
      , anChildren = [] }

    -- A background session with no process, as a row: not live (a click
    -- resumes it — `claude --resume` on a background session takes it up in
    -- the pane), its state the CLI's, mapped onto the pane's states.
    -- A blocked one is "needs you" only while recent: one left blocked for
    -- longer than 'backgroundStaleAge' was most likely abandoned, so it sorts
    -- with the exited rows (still clickable) instead of heading the pane.
    -- (Only ever an exited-style row: a background session with a live
    -- process is shown through its session file instead.  "stopped" is NOT
    -- leksah's "suspended", which means leksah suspended it and will offer it
    -- back.)
    backgroundNode now active b i =
      let (state, what) = case cbState b of
            "blocked" | backgroundNeedsYou now b -> ("waiting", "blocked — it is waiting for you")
                      | otherwise -> ("gone", "blocked (left waiting)")
            "done"    -> ("done",      "finished")
            "stopped" -> ("gone",      "stopped")
            "failed"  -> ("gone",      "failed")
            other     -> ("gone",      other)
      in AgentNode
        { anSession = cbSession b
        , anName    = ""
        , anTitle   = firstNonEmpty [ maybe "" T.strip (i >>= aiTitle)
                                    , fromMaybe "" (cbName b)
                                    , T.take 8 (cbSession b) ]
        , anState   = state
        , anDetail  = "Background session " <> cbId b <> ", " <> what
                      <> " — click to open it here"
        , anActivity = Just ("background session: " <> what)
        , anBackground = Just (cbId b)
        , anDone    = i >>= aiDone
        , anRole    = i >>= aiRole
        , anDir     = cbDir b
        , anDesc    = i >>= aiDesc
        , anAge     = maybe "" (humanAge now) active
        , anLive    = False
        , anBranch  = Nothing
        , anPr      = Nothing
        , anWorktrees = []
        , anBio = Nothing, anManager = False, anBorn = Nothing
        , anParentName = Nothing, anChildNames = []
        , anEdits = [], anError = Nothing, anCwd = Nothing, anUsage = Nothing, anApproval = Nothing, anLastSaid = Nothing
        , anAgent = "claude"
        , anChildren = [] }

    -- Stored agents that are NOT running: either exited (their transcript's mtime
    -- is when they last did anything) or not yet registered — a pane leksah just
    -- made, which may be stuck on a first-run question.  Newest first, capped.
    -- No transcript and not recently born means nothing is left to look at.
    exitedNodes now infos liveRows lastSeen = do
      let posix = realToFrac (utcTimeToPOSIXSeconds now) :: Double
      cands <- fmap (mapMaybe id) . forM (M.elems infos) $ \i ->
        if aiSession i `M.member` liveRows || null (aiDir i) then return Nothing
        else (`catch` \(_ :: SomeException) -> return Nothing) $ do
          -- (A Codex agent's conversation is its rollout; an agy one's, agy's.)
          p  <- cliTranscriptOf (aiSession i)
                  >>= maybe (claudeTranscriptPath (aiDir i) (aiSession i)) return
          ex <- doesFileExist p
          if not ex then return (starting now i) else do
            t <- getModificationTime p
            let age = realToFrac (diffUTCTime now t) :: Double
            -- A suspended agent is still on the team, and an interrupted one
            -- is waiting to be brought back: no age limit for either.
            return $ if age > exitedMaxAge && isNothing (aiSuspended i)
                          && not (isInterrupted posix i)
                       then Nothing else Just (Right (t, i))
      startedNodes <- forM [ i | Left i <- cands ] $ \i -> do
        (br, pr) <- agentGit posix (aiDir i)
        return . (,) (aiSession i) $ AgentNode
          { anSession = aiSession i
          , anName    = ""
          , anTitle   = firstNonEmpty [ maybe "" T.strip (aiTitle i)
                                      , T.take 8 (aiSession i) ]
          , anState   = "starting"
          , anActivity = Nothing
          , anDone    = Nothing
          , anRole    = aiRole i
          , anDetail  = "Starting — if it stays like this, look at its pane: it "
                        <> "is probably waiting at a first-run prompt"
          , anDir     = aiDir i
          , anDesc    = aiDesc i
          , anAge     = "starting…"
          , anLive    = False
          , anBranch  = br
          , anPr      = pr
          , anWorktrees = []
          , anBio = Nothing, anManager = False, anBorn = Nothing
          , anParentName = Nothing, anChildNames = []
          , anEdits = [], anError = Nothing, anCwd = Nothing, anUsage = Nothing, anBackground = Nothing, anApproval = Nothing, anLastSaid = Nothing, anAgent = "claude"
          , anChildren = [] }
      let exitedCands = sortOn (Down . fst) [ x | Right x <- cands ]
          interrupted i = isNothing (aiSuspended i) && isInterrupted posix i
          (suspendedCands, interruptedCands, goneCands) =
            ( [ x | x@(_, i) <- exitedCands, isJust (aiSuspended i) ]
            , [ x | x@(_, i) <- exitedCands, interrupted i ]
            , [ x | x@(_, i) <- exitedCands, isNothing (aiSuspended i)
                                            , not (interrupted i) ] )
      goneNodes <- forM (suspendedCands <> interruptedCands <> take exitedMaxRows goneCands)
                     $ \(t, i) -> do
        -- The same title chain as a live agent's ('chooseTitle'), so a card
        -- keeps its title when its agent stops.  (Its first prompt is read
        -- only when the chain may need it.)
        gist <- (`catch` \(_ :: SomeException) -> return Nothing) $
          cliTranscriptOf (aiSession i)
            >>= maybe (claudeTranscriptPath (aiDir i) (aiSession i)) return
            >>= fmap Just . cachedTranscriptGist
        let src = titleSourceOf i
            needLabel = isNothing (chooseTitle src Nothing gist Nothing)
                          || maybe False (isNothing . tsProvisional) src
        lbl <- if needLabel then claudeSessionLabel (aiDir i) (aiSession i) else return Nothing
        let title = fromMaybe "" (chooseTitle src Nothing gist lbl)
        (br, pr) <- agentGit posix (aiDir i)
        return . (,) (aiSession i) $ AgentNode
          { anSession = aiSession i
          , anName    = ""
          , anTitle   = firstNonEmpty [ T.takeWhile (/= '\n') title
                                      , T.take 8 (aiSession i) ]
          , anState   = if isJust (aiSuspended i) then "suspended"
                        else if interrupted i then "interrupted" else "gone"
          , anActivity = Nothing
          , anDone    = aiDone i
          , anRole    = aiRole i
          , anDetail  = if isJust (aiSuspended i)
                          then "Suspended — resume it to carry on where it left off"
                          else if interrupted i
                          then "Stopped by a restart (its terminal went away under it)"
                               <> " — click to resume it"
                          else "Exited — click to resume it here"
          , anDir     = aiDir i
          , anDesc    = aiDesc i
          -- An interrupted agent's transcript time says nothing (every
          -- transcript is touched as its process is killed): when it was last
          -- seen running is the time that means something.
          , anAge     = case M.lookup (aiSession i) lastSeen of
              Just seenAt | interrupted i ->
                "last seen " <> humanAge now (posixSecondsToUTCTime (realToFrac seenAt))
              _ -> humanAge now t
          , anLive    = False
          , anBranch  = br
          , anPr      = pr
          , anWorktrees = []
          , anBio = Nothing, anManager = False, anBorn = Nothing
          , anParentName = Nothing, anChildNames = []
          , anEdits = [], anError = Nothing, anCwd = Nothing, anUsage = Nothing, anBackground = Nothing, anApproval = Nothing, anLastSaid = Nothing, anAgent = "claude"
          , anChildren = [] }
      return (startedNodes <> goneNodes)

    -- A fork whose session file has not appeared yet, while that is still
    -- plausible: 'Left' it into the same candidate list.
    starting now i = case aiBorn i of
      Just b | realToFrac (utcTimeToPOSIXSeconds now) - b < startingMaxAge ->
                 Just (Left i)
      _ -> Nothing

#endif

#if !defined(ghcjs_HOST_OS)
-- | The activity line for a live row: the approval it is waiting on, or —
-- while busy — its last tool call, read from the transcript's tail and cached
-- per transcript by mtime (the pane polls every 2s; a busy agent's transcript
-- changes far less often than that).
agentActivity :: ClaudeStatusRow -> IO (Maybe Text)
agentActivity r = case csrState r of
  "waiting" -> return (Just (csrDetail r))
  -- (on another machine: what its transcript tail in the host's snapshot says)
  "busy" | isJust (csrHost r) -> shellAware <$> remoteActivity (csrSession r)
  "busy"    -> fmap shellAware . (`catch` \(_ :: SomeException) -> return Nothing) $ do
    p <- claudeTranscriptPath (T.unpack (csrDir r)) (csrSession r)
    t <- getModificationTime p
    cached <- M.lookup p <$> readIORef activityRef
    case cached of
      Just (t', a) | t' == t -> return a
      _ -> do
        a <- claudeLatestActivity p
        atomicModifyIORef' activityRef (\m -> (M.insert p (t, a) m, ()))
        return a
  _ -> return Nothing

  where
    -- A session busy only because a shell command of its own is still running
    -- (a background job, a long build) has finished its turn: its transcript's
    -- last message is prose, which would read as "Writing".  Say what it is.
    shellAware a
      | csrDetail r == "Running a shell command"
      , maybe True (`elem` ["Writing", "Thinking"]) a = Just (csrDetail r)
      | otherwise = a

{-# NOINLINE activityRef #-}
activityRef :: IORef (Map FilePath (UTCTime, Maybe Text))
activityRef = unsafePerformIO (newIORef M.empty)
#endif

-- | One line per worktree relationship the session has registered — via
-- @leksah-cmd agent register@ \/ the @register_worktree@ MCP tool \/ the
-- PostToolUse hook — e.g. @"created · fix-tests [claude\/fix-tests] · 2h ago —
-- fixing the parser"@, newest first, capped so a busy session's row stays
-- bounded.
worktreeClaimLines :: UTCTime -> Text -> IO [Text]
worktreeClaimLines now sid = do
  cs <- claimsBySession sid
  return $ take 4
    [ roleText (scRole c) <> " · "
        <> T.pack (takeFileName (dropTrailingPathSeparator (wiPath w)))
        <> maybe "" (\b -> " [" <> b <> "]") (scBranch c)
        <> " · " <> humanAge now (posixSecondsToUTCTime (realToFrac (scWhen c)))
        <> (if T.null (scNote c) then "" else " — " <> scNote c)
    | (w, c) <- cs ]

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
-- (Concatenation, not string gaps: CPP strips the backslash-newline gaps.)
agentRefreshPrompt :: Text
agentRefreshPrompt = T.intercalate "\n"
  [ "Leksah's Agents pane is asking you to refresh how you appear in it. Do "
    <> "just this, then stop: call the describe_agent tool (or run `leksah-cmd "
    <> "agent describe --title '…' --html '…' --bio '…'`)."
  , ""
  , "The title is a few words naming what you are working on. The description "
    <> "is an HTML fragment that renders in about FOUR LINES in a narrow side "
    <> "pane, so keep it terse and avoid long unbreakable strings: say what state "
    <> "the work is in, and include <a href=\"…\">links</a> to any pull request, "
    <> "CI/Hydra build or issue involved. Allowed tags: p, br, a, code, b, "
    <> "strong, i, em, ul, li, span."
  , ""
  , "Also give a bio (--bio '…', or the bio argument): two to four sentences, "
    <> "in the third person, for the Team pane's card about you — what you have "
    <> "worked on in this session, what you now know well about this codebase, "
    <> "and what kind of task you would be good to hand next. Same HTML tags; "
    <> "links welcome."
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

-- | Which CLI session @sid@ runs (or ran) in: a Codex or Antigravity one when
-- leksah's hooks for that CLI have seen it, else Claude Code.
sessionCli :: Text -> IO AgentCli
sessionCli sid = isCodexSession sid >>= \case
  True  -> return CliCodex
  False -> isAgySession sid >>= \case
    True  -> return CliAgy
    False -> return CliClaude

-- | Session @sid@'s transcript when it is a Codex (its rollout) or an
-- Antigravity session, as leksah's hooks recorded it; 'Nothing' for a Claude
-- Code session (whose transcript is found by its directory).
cliTranscriptOf :: Text -> IO (Maybe FilePath)
cliTranscriptOf sid = codexTranscriptOf sid >>= \case
  Just p  -> return (Just p)
  Nothing -> agyTranscriptOf sid
