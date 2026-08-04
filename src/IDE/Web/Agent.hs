{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Agents forking agents: one Claude Code session starting another in a
-- terminal pane of its own, and the small vocabulary the two need to work
-- together.
--
-- The motivating shape is a session that wants three things done in parallel,
-- or a second opinion, or a long grind kept out of its own context.  It can
-- already do that with subagents inside its own process — but those are
-- invisible, share its context window and die with its turn.  A forked agent is
-- a real @claude@ in a real pane: the user can watch it, talk to it, and
-- approve its tools; it survives the parent's turn; and (because
-- @--fork-session@ copies the conversation) it starts out already knowing
-- everything the parent knew, with no briefing.
--
-- Everything here is FS/tmux/process only — no reflex — so it can run straight
-- off a control-socket connection ("IDE.Web.CmdServer"'s @agent@ verbs, which
-- @leksah-cmd agent@ and its MCP tools drive).  The pieces it composes all
-- predate it:
--
--   * 'sessionOwningPid' answers "which session is asking?" from the client's
--     pid, so a fork needs no arguments at all in the common case;
--   * 'paneForSession' answers "where is that session?" exactly (pid ancestry,
--     not directory guesswork), which is what makes "beside me" meaningful;
--   * 'splitPane' + 'requestLocalTerm' put the child in that window;
--   * @--session-id@ pins the child's id before it starts, so the reply is a
--     handle the parent can use immediately.
--
-- Deliberately NOT here: git worktrees.  A fork lands in the parent's checkout
-- (see 'forkAgent' on why a forked conversation can't change directory), so
-- children editing the same files will collide — for isolated parallel work the
-- Tasks pane's queue ("IDE.Web.ClaudeQueue") is the right door, since it makes a
-- worktree per task and reviews the diffs afterwards.
module IDE.Web.Agent
  ( ForkPlace(..)
  , ForkRequest(..)
  , emptyForkRequest
  , forkAgent
  , agentList
  , agentStatus
  , agentSend
  , agentRead
  , newAgentSessionId
  ) where

import Control.Exception (catch, SomeException)
import Control.Monad (filterM, forM)

import Data.Bits ((.&.), (.|.))
import Data.List (intercalate, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Numeric (showHex)

import Data.Aeson (Value(..), decodeStrict', withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS

import System.Directory
       (doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory)
import System.FilePath ((</>), dropTrailingPathSeparator)
import System.IO
       (withFile, IOMode(ReadMode), hSeek, SeekMode(SeekFromEnd), hFileSize)

import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.Claude
       (AgentSpec(..), ClaudeCmd(..), ClaudeLive(..), claudeAvailable,
        claudeCommandLine, claudeLiveBySession, claudeLiveOwners,
        claudeSessionLabel, claudeTranscriptPath, paneForSession)
import IDE.Web.NewLwRequest (requestNewLw)
import IDE.Web.RemoteTermRequest (requestLocalTerm)
import IDE.Web.ReplTmux
       (freshSessionName, liveRunPanes, livePanePids, newSessionWindow, pasteTo,
        sendKeysTo, splitPane, tmuxCmd)

-- | Where the child's pane goes.
data ForkPlace
  = PlaceBeside    -- ^ split the parent's pane side by side (the default)
  | PlaceBelow     -- ^ split it top/bottom
  | PlaceTab       -- ^ its own leksah window (tab), like the New Claude Session
                   --   menu item — and the fallback whenever the parent's pane
                   --   can't be found (a session running outside leksah's tmux)
  deriving (Eq, Show)

-- | What to fork.  'emptyForkRequest' plus the caller's cwd/pid is enough: the
-- common call is @leksah-cmd agent fork 'do the thing'@ with no options.
data ForkRequest = ForkRequest
  { frPlace  :: ForkPlace
  , frDir    :: Maybe FilePath -- ^ explicit working directory (implies 'frFresh'
                               --   semantics — see 'forkAgent')
  , frFrom   :: Maybe Text     -- ^ fork THIS session rather than the caller's
  , frFresh  :: Bool           -- ^ start a context-free agent: it knows only its
                               --   prompt (and its CLAUDE.md)
  , frPrompt :: Maybe Text     -- ^ the first prompt, submitted at launch
  , frParent :: Maybe Text     -- ^ the calling session, from the client pid
  , frCwd    :: FilePath       -- ^ the caller's cwd (the dir of last resort)
  } deriving (Eq, Show)

emptyForkRequest :: FilePath -> ForkRequest
emptyForkRequest cwd = ForkRequest
  { frPlace = PlaceBeside, frDir = Nothing, frFrom = Nothing, frFresh = False
  , frPrompt = Nothing, frParent = Nothing, frCwd = cwd }

-- | Start a child agent, and answer with its session id and a line the parent
-- can act on.
--
-- The child's conversation:  by default it forks the caller's own session, so it
-- starts with the parent's full context.  @claude --resume@ only finds a
-- conversation in the project folder of the CURRENT directory, so a forked
-- conversation cannot change directory — asking for both a fork and a different
-- @--dir@ is an error rather than a silently context-free agent.  'frFresh' (or
-- no identifiable caller) starts a context-free agent instead, which may go
-- anywhere.
--
-- The child's pane: a tmux split of the parent's pane, which leksah renders as
-- a split inside the leksah pane already showing that window.  We deliberately
-- do NOT @select-pane@ it: the fork is the parent's doing, not the user's, so it
-- must not move the user's focus (or the parent's, if it is mid-turn) — the new
-- pane just appears.
forkAgent :: ForkRequest -> IO (Either Text (Text, Text))
forkAgent fr = (`catch` \(e :: SomeException) ->
                  return (Left ("agent fork failed: " <> T.pack (show e)))) $
    claudeAvailable >>= \case
      False -> return (Left "the claude CLI is not on leksah's PATH")
      True  -> do
        live <- claudeLiveBySession
        let parent  = frFrom fr `orElse` frParent fr
            mParent = parent >>= (`M.lookup` live)
        case (parent, mParent) of
          (Just p, Nothing) | not (frFresh fr) -> return . Left $
            "session " <> p <> " is not running, so there is no conversation to \
            \fork — resume it first, or use --fresh"
          _ -> go live parent (clDir <$> mParent)
  where
    orElse a b = maybe b Just a

    go live parent parentDir = do
      -- Fork the parent's conversation unless asked not to (or there is none).
      let from | frFresh fr = Nothing
               | otherwise  = parent
          dir  = dropTrailingPathSeparator . fromMaybe (frCwd fr) $
                   frDir fr `orElse` (if isNothing from then Nothing else parentDir)
      if | isRemotePath dir -> return (Left ("cannot start an agent in a remote \
                                             \directory: " <> T.pack dir))
         | Just f <- from, Just pd <- parentDir, dropTrailingPathSeparator pd /= dir ->
             return . Left $
               "a forked conversation has to stay in its own directory (session "
               <> f <> " is in " <> T.pack pd <> "): drop --dir, or use --fresh \
               \to start a context-free agent in " <> T.pack dir
         | otherwise -> do
             child <- newAgentSessionId
             let spec = AgentSpec { asDir     = dir
                                  , asSession = child
                                  , asFrom    = from
                                  , asPrompt  = frPrompt fr
                                  , asParent  = parent }
             (_, key, line) <- claudeCommandLine (ClaudeAgent spec)
             place live spec key line

    -- Beside the parent when we can find its pane; its own tab otherwise.
    place live spec key line = do
      mPane <- case (frPlace fr, asParent spec `orElse` asFrom spec) of
        (PlaceTab, _)     -> return Nothing
        (_, Just p)       -> paneForSession p
        (_, Nothing)      -> return Nothing
      case mPane of
        Just (tsess, _, pane) ->
          splitPane (frPlace fr /= PlaceBelow) pane (asDir spec) (Just line) >>= \case
            Nothing      -> return (Left "tmux would not split the parent's pane")
            Just newPane -> do
              tmuxCmd ["set-option", "-p", "-t", T.unpack newPane
                      , "@leksah_run", T.unpack key]
              requestLocalTerm tsess
              return (Right (asSession spec, described spec live ("pane " <> newPane)))
        Nothing -> do
          name <- freshSessionName "claude"
          newSessionWindow name (asDir spec) (Just key) line False >>= \case
            Nothing -> return (Left "could not open a terminal for the agent")
            Just (tsess, wid, _) -> do
              requestNewLw (tsess, wid)
              return (Right (asSession spec, described spec live "a new tab"))

    -- The reply: what was started, and the three things the parent can do next.
    -- Spelled out because the caller is usually a model reading tool output.
    described spec live whereAt = T.unlines $
      [ "Started agent " <> asSession spec <> " in " <> whereAt <> " ("
          <> T.pack (asDir spec) <> ")."
      , case asFrom spec of
          Just f -> "It is a fork of session " <> f <> nameOf live f
                      <> ", so it already has that conversation's context."
          Nothing -> "It is a fresh agent: it knows only the prompt you gave it."
      , maybe "No prompt was sent, so it is waiting at its prompt."
              (const "Its first prompt has been sent.") (asPrompt spec)
      , ""
      , "It reports back by messaging you when it finishes.  You can also:"
      , "  leksah-cmd agent wait " <> asSession spec <> "        # block until it is idle"
      , "  leksah-cmd agent read " <> asSession spec <> "        # its last answers"
      , "  leksah-cmd agent send " <> asSession spec <> " --submit 'more'"
      ]

    nameOf live sid = case M.lookup sid live >>= clName of
      Just n  -> " (" <> n <> ")"
      Nothing -> ""

-- | The live sessions, one tab-separated row each: session id, state, tmux pane
-- (@-@ when it isn't in one of leksah's), directory, title.  The caller's own
-- session is marked, so an agent reading this can tell itself apart from its
-- peers.  Ordered like the status surfaces: needs-you first, then working, then
-- idle.
agentList :: Maybe Text -> IO Text
agentList me = do
  live  <- claudeLiveBySession
  panes <- panesBySession
  if M.null live then return "No Claude Code sessions are running.\n" else do
    rows <- forM (sortOn (\l -> (rank l, clDir l)) (M.elems live)) $ \l -> do
      title <- case clName l of
        Just n  -> return n
        Nothing -> fromMaybe "" <$> claudeSessionLabel (clDir l) (clSession l)
      return $ T.intercalate "\t"
        [ clSession l
        , fromMaybe "idle" (clStatus l)
        , fromMaybe "-" (M.lookup (clSession l) panes)
        , T.pack (clDir l)
        , T.takeWhile (/= '\n') title
        , if me == Just (clSession l) then "(you)" else "" ]
    return . T.unlines $
      "# session\tstate\tpane\tdir\ttitle" : rows
  where
    rank l = case clStatus l of
      Just "waiting" -> 0 :: Int
      Just "busy"    -> 1
      Just "shell"   -> 1
      _              -> 2

-- | One line for one session: its state, then what it is doing.  The state is
-- the first word so a poller can match on it — @waiting@ (blocked on an
-- approval prompt, and it needs a human), @busy@ / @shell@ (working), @idle@
-- (ready for input, i.e. finished), @gone@ (not running any more).  This is
-- what @leksah-cmd agent wait@ polls.
agentStatus :: Text -> IO Text
agentStatus sid = claudeLiveBySession >>= \live -> case M.lookup sid live of
  Nothing -> startingPane sid >>= \case
    -- Its pane is there but the CLI hasn't registered the session.  Nearly
    -- always the first-run question a directory gets asked once ("is this a
    -- project you trust?"), which no amount of waiting will answer.
    Just p  -> return $ T.intercalate "\t"
        [ "starting"
        , "its pane exists but the session has not registered — look at the \
          \pane: it is probably waiting at a first-run prompt (the folder-trust \
          \question, which only a human can answer)"
        , p ] <> "\n"
    Nothing -> return "gone\tthat session is not running\n"
  Just l  -> do
    pane <- M.lookup sid <$> panesBySession
    return $ T.intercalate "\t"
        [ fromMaybe "idle" (clStatus l)
        , fromMaybe (detail l) (clWaitingFor l)
        , fromMaybe "-" pane
        , T.pack (clDir l) ] <> "\n"
  where
    detail l = case clStatus l of
      Just "busy"  -> "working"
      Just "shell" -> "running a shell command"
      _            -> "ready for input"

-- | Type @txt@ into a live session's composer, optionally pressing Enter to
-- submit it as a turn.
--
-- Multi-line text goes through a tmux paste buffer in bracketed-paste mode
-- rather than @send-keys -l@: the CLI reads a paste as one block, where literal
-- newlines would each submit what had been typed so far (turning one report into
-- several truncated turns).
agentSend :: Text -> Bool -> Text -> IO Text
agentSend sid submit txt = paneForSession sid >>= \case
  Nothing -> return $ "session " <> sid <> " is not in a pane leksah can reach \
                      \(not running, remote, or in a terminal outside leksah)\n"
  Just (_, _, pane) -> do
    ok <- if T.any (== '\n') txt then pasteTo pane txt
                                 else sendKeysTo pane ["-l", T.unpack txt]
    if not ok then return "tmux would not deliver the text\n" else do
      submitted <- if submit then sendKeysTo pane ["Enter"] else return True
      return $ if submit && submitted
        then "Sent and submitted to " <> sid <> ".\n"
        else "Left in " <> sid <> "'s composer unsubmitted (add --submit to send it).\n"

-- | The last @n@ things a session SAID (assistant text, oldest first), prefixed
-- by its current state — how a parent collects a child's answer.
--
-- Reads the tail of the transcript only (the last 512KB, like
-- 'IDE.Web.Claude.claudeLatestPlan'): these files reach megabytes, and the
-- interesting end is always the end.
agentRead :: Text -> Int -> IO Text
agentRead sid n = do
  st    <- T.strip <$> agentStatus sid
  mpath <- transcriptFor sid
  case mpath of
    Nothing   -> return . T.unlines $
        [ "state\t" <> st, "No transcript found for " <> sid ]
    Just path -> do
      msgs <- assistantTail path
      return . T.unlines $ ("state\t" <> st) :
        (if null msgs then [ "(it has not answered yet)" ]
                      else map ("\n--- said ---\n" <>) (lastN n msgs))
  where
    lastN k xs = drop (max 0 (length xs - max 1 k)) xs

-- | A fresh RFC 4122 version-4 UUID, the shape @claude --session-id@ wants.
-- 16 bytes of @\/dev\/urandom@ (present on both platforms leksah's tmux
-- integration runs on) rather than a new dependency.
newAgentSessionId :: IO Text
newAgentSessionId = do
  bs <- withFile "/dev/urandom" ReadMode (`BS.hGet` 16)
  let ws = zipWith stamp [0 :: Int ..]
             (map (fromIntegral . fromEnum) (BS.unpack bs)) :: [Word8]
  return . T.pack . intercalate "-" . groups [8, 4, 4, 4, 12] $ concatMap hex ws
  where
    stamp 6 w = (w .&. 0x0f) .|. 0x40   -- version 4
    stamp 8 w = (w .&. 0x3f) .|. 0x80   -- variant 1
    stamp _ w = w
    hex w = let s = showHex w "" in if length s == 1 then '0' : s else s
    groups []       _ = []
    groups (k : ks) s = let (a, b) = splitAt k s in a : groups ks b

--------------------------------------------------------------------------------
-- helpers

-- | Every live session's tmux pane id, in one pass (where 'paneForSession' pays
-- for a @ps@ and a tmux listing per session).
panesBySession :: IO (Map Text Text)
panesBySession = do
  owners <- claudeLiveOwners
  panes  <- livePanePids
  return $ M.fromList
    [ (clSession l, p) | (_, _, p, ppid) <- panes, Just l <- [M.lookup ppid owners] ]

-- | The pane a not-yet-registered agent is starting in, found by its run key
-- (@\<dir\>#claude#\<session id\>@, set when the pane was made — which is why
-- pinning the id up front pays off twice).
startingPane :: Text -> IO (Maybe Text)
startingPane sid = do
  panes <- liveRunPanes
  return $ listToMaybe
    [ p | (k, _, _, p) <- panes, ("#claude#" <> sid) `T.isSuffixOf` k ]

-- | A session's transcript: straight from its directory when it is live, else by
-- looking for @\<id\>.jsonl@ among the project folders (a child that has already
-- exited is exactly when a parent wants to read it).
transcriptFor :: Text -> IO (Maybe FilePath)
transcriptFor sid = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  live <- claudeLiveBySession
  fromLive <- case M.lookup sid live of
    Just l | not (null (clDir l)) -> do
      p <- claudeTranscriptPath (clDir l) sid
      ex <- doesFileExist p
      return (if ex then Just p else Nothing)
    _ -> return Nothing
  case fromLive of
    Just p  -> return (Just p)
    Nothing -> do
      home <- getHomeDirectory
      let root = home </> ".claude" </> "projects"
          leaf = T.unpack sid <> ".jsonl"
      ex <- doesDirectoryExist root
      if not ex then return Nothing else do
        ds <- listDirectory root
        listToMaybe <$> filterM doesFileExist [ root </> d </> leaf | d <- ds ]

-- | The assistant's text messages from the tail of a transcript, oldest first.
assistantTail :: FilePath -> IO [Text]
assistantTail path = (`catch` \(_ :: SomeException) -> return []) $ do
  ls <- withFile path ReadMode $ \h -> do
    size <- hFileSize h
    let back = min size (512 * 1024)
    hSeek h SeekFromEnd (negate back)
    BS.lines <$> BS.hGet h (fromIntegral back)
  return [ t | l <- ls, "\"assistant\"" `BS.isInfixOf` l
             , Just t <- [decodeStrict' l >>= parseMaybe textP]
             , not (T.null (T.strip t)) ]
  where
    -- The text blocks of one assistant line, joined (tool_use blocks skipped).
    textP = withObject "line" $ \o -> do
      ty <- o .: "type"
      if ty /= ("assistant" :: Text) then fail "not assistant" else do
        m  <- o .: "message"
        cs <- m .: "content"
        let oneP = withObject "content" $ \c -> do
              t <- c .: "type"
              if t == ("text" :: Text) then c .: "text" else fail "not text"
        return . T.intercalate "\n" $ mapMaybe (parseMaybe oneP) (cs :: [Value])
