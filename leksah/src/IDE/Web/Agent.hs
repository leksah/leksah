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
--   * 'splitPane' + 'raiseTabOfPane' put the child in that window;
--   * @--session-id@ pins the child's id before it starts, so the reply is a
--     handle the parent can use immediately.
--
-- The lineage a fork establishes is recorded by "IDE.Web.AgentInfo"
-- ('recordAgentFork'), which is what lets the Agents pane show the sessions as a
-- tree; @agent describe@ (there too) is the other half — how an agent says what
-- it is doing.
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
  , remoteAgentEvent
  , forkAgent
  , agentList
  , agentStatus
  , agentSend
  , agentRead
  , agentLastSaid
  , renameAgentEverywhere
  , agentConversation
  , agentHook
  , agentEvents
  , resolveAgent
  , agentFind
  , agentDone
  , agentRoleList
  , agentSuspend
  , agentResume
  , agentResumeAll
  , agentManagerTriage
  , agentRefresh
  , agentManagerStatus
  , agentManagerStart
  , newAgentSessionId
  , showAgentPane
  , agentAnswerApproval
  , showAgentPaneHere
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad (filterM, forM, mfilter, unless, when, void)

import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson
       (Object, Value(..), decodeStrict', encode, object, parseJSON, withObject,
        (.:), (.:?), (.=))
import Data.Foldable (toList)
import Data.Aeson.Types (Pair, parseMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Aeson.KeyMap as KM (lookup)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Set as S
import qualified Data.Aeson.Key as Key
import qualified Data.Text.Encoding as TE (decodeUtf8, decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, getCurrentTimeZone)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO.Unsafe (unsafePerformIO)

import System.Directory
       (doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory)
import System.FilePath ((</>), dropTrailingPathSeparator, normalise)
import System.IO
       (withFile, IOMode(ReadMode), hSeek, SeekMode(SeekFromEnd), hFileSize)

import IDE.Utils.RemotePath (isRemotePath, parseRemotePath, renderRemotePath)
import IDE.Web.AgentInfo
       (AgentInfo(..), AgentNode(..), agentForest, agentInfoOf, agentManager,
        agentParentOf, agentRefreshPrompt, agentTitles, markAgentDone,
        moveAgentRecord, recordAgentFork, recordRemoteFork, remoteAgentRecord,
        setRemoteSuspended, applyRemoteCommand, setAgentManager, setAgentRole,
        setAgentSuspended, acknowledgeInterrupted, stripTags, titleForClaude)
import IDE.Web.AgentHost (RanOn)
import IDE.Web.WorktreeRegistry (ensureSessionWorktree)
import IDE.Web.AgentNames
       (adoptContinuation, agentContinuations, agentNameFor, agentNames, renameAgent,
        resolveAgentName, touchAgentNames)
import IDE.Web.AgentRoles (AgentRole(..), agentRoles, findAgentRole, roleNote)
import IDE.Web.Claude
       (AgentSpec(..), ClaudeBackground(..), ClaudeCmd(..), ClaudeLive(..), claudeAvailable,
        claudeBackgroundSessions,
        claudeCommandLine, claudeLiveBySession,
        claudeSessionLabel, claudeTranscriptPath, newAgentSessionId,
        paneForSession, runClaudeCmd,
        showLiveSession, showLiveSessionHere, showTmuxPaneWith, claudeProcessOf, nameNote, agentNote,
        managerNote, describeToolUse, claudePaneSessions, planHtmlNote, worktreeNote,
        raiseTabOfPane)
import System.Process (readProcessWithExitCode)
import IDE.Web.AgentEvents
       (SessionEvents(..), seTool, perToolHookEvents, editedRecently, eventsFor, sessionEvents,
        recordCwd, recordEdit, recordFailure, recordPermission,
        recordToolEnd, recordToolStart, recordTurnEnd, recordPromptSubmit,
        recordSubagentStart, recordSubagentStop, recordCompacting, recordSessionEnd,
        recordTaskCreated, recordTaskActive, recordTaskStatus, TaskInfo(..))
import IDE.Web.AgentUsage (recordStatusLine, usageFor, usageLines, usageSummary)
import IDE.Web.ClaudeStatus
       (ClaudeStatus(..), ClaudeStatusRow(..), claudeStatusNow, pokeClaudeStatus)
import IDE.Web.Codex
       (codexAvailable, codexInteractive, codexProcessOf, codexSessionStart, codexSetStatus, codexSessionEnd,
        codexPatchFiles, codexSessionDir, codexStarted, codexTranscriptOf, isCodexSession,
        isRollout, rolloutTurns)
import IDE.Web.NewLwRequest (requestNewLw)
import IDE.Web.ReplTmux
       (freshSessionName, liveRunPanes, newAgentWindow, newRemoteAgentWindow, pasteTo,
        sendKeysTo, splitPane, tmuxCmd, tmuxOut)
import IDE.Web.TmuxServer
       (TmuxServer(..), rawTmuxId, selectWindowAsViewer, tmuxCmdOn, tmuxOnIds, tmuxStdoutOn)
import IDE.Web.RemoteTermRequest (requestRemoteTerm)
import IDE.Web.RemoteAgents
       (remoteAgentName, remoteHostOf, remoteLiveSessions, remotePaneOf, remoteStartingPane,
        remoteTranscriptTail)

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
  , frRole   :: Maybe Text     -- ^ start it in this role ("IDE.Web.AgentRoles");
                               --   a role agent is always fresh
  , frManager :: Bool          -- ^ start it as the workspace's manager
  , frCodex  :: Bool           -- ^ run Codex rather than Claude Code (always a
                               --   fresh agent: a Claude conversation cannot be
                               --   carried into Codex)
  , frClaude :: Bool           -- ^ run Claude Code even when forking from a Codex
                               --   agent (whose children are Codex ones otherwise)
  } deriving (Eq, Show)

emptyForkRequest :: FilePath -> ForkRequest
emptyForkRequest cwd = ForkRequest
  { frPlace = PlaceBeside, frDir = Nothing, frFrom = Nothing, frFresh = False
  , frPrompt = Nothing, frParent = Nothing, frCwd = cwd, frRole = Nothing
  , frManager = False, frCodex = False, frClaude = False }

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
--
-- With 'frRole', the role is looked up first (from the directory the agent
-- will work in) and the agent starts fresh, with the role's instructions.
--
-- A Codex conversation cannot be forked at all (into Claude Code, or into
-- Codex: @codex fork@ mints its own id), so a fork FROM a Codex agent — the
-- caller, or @--from@ — is a fresh agent, and a Codex one like its parent
-- unless @--claude@ asks otherwise.
forkAgent :: ForkRequest -> IO (Either Text (Text, Text))
forkAgent fr1 = do
  live <- claudeLiveBySession
  srcCodex <- case frFrom fr1 <|> frParent fr1 of
    Nothing  -> return False
    Just src -> case M.lookup src live of
      Just l  -> return (clAgent l == "codex")
      Nothing -> isCodexSession src
  let fr0 | frCodex fr1                  = fr1 { frFresh = True }
          | srcCodex, not (frClaude fr1) = fr1 { frCodex = True, frFresh = True }
          | srcCodex                     = fr1 { frFresh = True }
          | otherwise                    = fr1
  case frRole fr0 of
    Nothing -> forkAgentAs Nothing fr0
    Just n  -> findAgentRole (fromMaybe (frCwd fr0) (frDir fr0)) n >>= \case
      Left err   -> return (Left err)
      Right role -> forkAgentAs (Just role) fr0 { frFresh = True }

forkAgentAs :: Maybe AgentRole -> ForkRequest -> IO (Either Text (Text, Text))
forkAgentAs mrole fr = (`catch` \(e :: SomeException) ->
                  return (Left ("agent fork failed: " <> T.pack (show e)))) $
    (if frCodex fr then codexAvailable else claudeAvailable) >>= \case
      False -> return (Left (if frCodex fr then "the codex CLI is not on leksah's PATH"
                                           else "the claude CLI is not on leksah's PATH"))
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
      if | Just (host, rdir) <- parseRemotePath dir -> goRemote host rdir from parent
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
                                  , asParent  = parent
                                  , asRoleNote = roleNote <$> mrole
                                  , asResume  = False
                                  , asManager = frManager fr
                                  , asCodex   = frCodex fr }
             (_, key, line) <- claudeCommandLine (ClaudeAgent spec)
             place live spec key line

    -- On another machine (a project there): the agent runs THERE, in the
    -- Agents session of its leksah tmux server, named and recorded in that
    -- machine's own stores — so the leksah running there, if any, and every
    -- other one showing the machine know it as the same agent.
    goRemote host rdir from parent = do
      sameHost <- maybe (return True) (fmap (== Just host) . remoteHostOf) from
      if not sameHost
        then return . Left $
          "a conversation can only be forked on the machine it runs on: use \
          \--fresh to start an agent on " <> host
        else do
          child <- newAgentSessionId
          remoteAgentName host child >>= \case
            Left err -> return (Left ("could not name the agent on " <> host <> ": " <> err))
            Right name -> do
              recordRemoteFork host parent child rdir (frPrompt fr)
                               (roleName <$> mrole) (frManager fr)
              let spec = AgentSpec { asDir     = renderRemotePath host rdir
                                   , asSession = child
                                   , asFrom    = from
                                   , asPrompt  = frPrompt fr
                                   , asParent  = parent
                                   , asRoleNote = roleNote <$> mrole
                                   , asResume  = False
                                   , asManager = frManager fr
                                   , asCodex   = False }
              fmap (const (child, name)) <$> startRemoteAgent host name rdir spec

    -- Beside the parent when we can find its pane; its own tab otherwise.
    place live spec key line = do
      -- Write the lineage down NOW: the Agents pane's tree is built from it, and
      -- after this moment nothing on disk relates a child to its parent (nor
      -- says what the child was asked to do — its forked transcript starts with
      -- the PARENT's first prompt).
      recordAgentFork (asParent spec) (asSession spec) (asDir spec) (asPrompt spec) (asFrom spec)
      -- (already chosen by 'claudeCommandLine', which told the agent its name)
      ownName <- agentNameFor (asSession spec)
      mapM_ (setAgentRole (asSession spec) . roleName) mrole
      when (frManager fr) $ setAgentManager (asSession spec) (asDir spec)
      mPane <- case (frPlace fr, asParent spec `orElse` asFrom spec) of
        (PlaceTab, _)     -> return Nothing
        (_, Just p)       -> paneForSession p
        (_, Nothing)      -> return Nothing
      case mPane of
        Just (_, _, pane) ->
          splitPane (frPlace fr /= PlaceBelow) pane (asDir spec) (Just line) >>= \case
            Nothing      -> return (Left "tmux would not split the parent's pane")
            Just newPane -> do
              tmuxCmd ["set-option", "-p", "-t", T.unpack newPane
                      , "@leksah_run", T.unpack key]
              raiseTabOfPane newPane
              return (Right (asSession spec, described ownName spec live ("pane " <> newPane)))
        Nothing -> do
          newAgentWindow (Just ownName) (asDir spec) (Just key) line False >>= \case
            Nothing -> return (Left "could not open a terminal for the agent")
            Just (tsess, wid, _) -> do
              requestNewLw (tsess, wid)
              return (Right (asSession spec, described ownName spec live "a new tab"))

    -- The reply: what was started, and the three things the parent can do next.
    -- Spelled out because the caller is usually a model reading tool output.
    described ownName spec live whereAt = T.unlines $
      [ "Started agent " <> ownName <> " (session " <> asSession spec <> ") in "
          <> whereAt <> " (" <> T.pack (asDir spec) <> ")."
      , case asFrom spec of
          Just f -> "It is a fork of session " <> f <> nameOf live f
                      <> ", so it already has that conversation's context."
          Nothing | Just ro <- mrole ->
                     "It is a fresh agent in the role " <> roleName ro
                       <> ": it knows only that role and the prompt you gave it."
                  | asCodex spec ->
                     "It is a fresh Codex agent: it knows only the prompt you gave it."
                  | otherwise ->
                     "It is a fresh agent: it knows only the prompt you gave it."
      , maybe "No prompt was sent, so it is waiting at its prompt."
              (const "Its first prompt has been sent.") (asPrompt spec)
      , ""
      , "It reports back by messaging you when it finishes.  You can also:"
      , "  leksah-cmd agent wait " <> handle <> "        # block until it is idle"
      , "  leksah-cmd agent read " <> handle <> "        # its last answers"
      , "  leksah-cmd agent send " <> handle <> " --submit 'more'"
      ]
      where handle = T.toLower ownName

    nameOf live sid = case M.lookup sid live >>= clName of
      Just n  -> " (" <> n <> ")"
      Nothing -> ""

-- | The live sessions, one tab-separated row each: session id, name, state
-- ('liveState'), what it is doing ('liveDetail'), tmux pane (@-@ when it isn't
-- in one of leksah's), directory, title.  The caller's own session is marked,
-- so an agent reading this can tell itself apart from its peers.  Ordered like
-- the status surfaces: needs-you first, then working, then idle.
agentList :: Bool -> Maybe Text -> IO Text
agentList withStopped me = do
  live  <- allLiveSessions
  panes <- panesBySession
  -- (--all) The agents that are not running but are still waiting to be
  -- brought back: interrupted by a restart, or suspended.  Same columns, the
  -- pane always "-".
  stopped <- if not withStopped then return [] else do
    forest <- agentForest
    let flat ns = concatMap (\n -> n : flat (anChildren n)) ns
    return [ T.intercalate "\t"
               [ anSession n, if T.null (anName n) then "-" else anName n
               , anState n, anDetail n, "-", T.pack (anDir n)
               , T.takeWhile (/= '\n') (anTitle n), "" ]
           | n <- flat forest, anState n `elem` ["interrupted", "suspended"] ]
  if M.null live && null stopped then return "No Claude Code sessions are running.\n" else do
    -- (names this machine's sessions; a remote one is named in its own
    -- machine's store — "IDE.Web.RemoteAgents" — and 'agentNames' has it)
    names  <- M.union <$> touchAgentNames [ k | (k, l) <- M.toList live, isNothing (clHost l) ]
                      <*> agentNames
    titles <- agentTitles
    polled <- M.fromList . map (\r -> (csrSession r, csrTitle r)) . csRows <$> claudeStatusNow
    events <- sessionEvents
    rows <- forM (sortOn (\l -> (rank l, clDir l)) (M.elems live)) $ \l -> do
      -- The same title every other surface shows: the status poll's (what the
      -- agent called itself, a /rename name, Claude Code's own title for the
      -- conversation, the first prompt), else — a session the poll has not
      -- seen yet — the agent's own title or its /rename name, else its first
      -- prompt.
      title <- case M.lookup (clSession l) polled <|> M.lookup (clSession l) titles
                      <|> clName l of
        Just n  -> return n
        Nothing -> fromMaybe "" <$> claudeSessionLabel (clDir l) (clSession l)
      return $ T.intercalate "\t"
        [ clSession l
        , M.findWithDefault "-" (clSession l) names
        , liveState l
        , detailWith (M.lookup (clSession l) events) l
        , fromMaybe "-" (M.lookup (clSession l) panes)
        , T.pack (clDir l)
        , T.takeWhile (/= '\n') title
        , T.unwords ([ "(you)" | me == Just (clSession l) ]
                     <> [ "(codex)" | clAgent l == "codex" ]) ]
    return . T.unlines $
      "# session\tname\tstate\tdetail\tpane\tdir\ttitle" : rows <> stopped
  where
    rank l = case clStatus l of
      Just "waiting" -> 0 :: Int
      Just "busy"    -> 1
      Just "shell"   -> 1
      _              -> 2

-- | A live session's state, in the few words every reader can act on:
-- @waiting@ (blocked on an approval prompt — it needs a human), @busy@
-- (working — including while it runs a shell command, which the CLI reports
-- as @shell@: printed raw, that read as "the session ended, a shell is left",
-- which it is not), or @idle@ (ready for input).
liveState :: ClaudeLive -> Text
liveState l = case clStatus l of
  Just "waiting" -> "waiting"
  Just "busy"    -> "busy"
  Just "shell"   -> "busy"
  _              -> "idle"

-- | What a live session is doing, spelled out — where the shell-command case
-- is still told apart from the rest of @busy@ (a shell running for hours may
-- be a forgotten watcher).
liveDetail :: ClaudeLive -> Text
liveDetail l = case clStatus l of
  Just "waiting" -> "waiting for approval" <> maybe "" (": " <>) (clWaitingFor l)
  Just "busy"    -> "working"
  Just "shell"   -> "running a shell command"
  _              -> "ready for input"

-- | @agent events SID@: what the hooks have told leksah about a session — the
-- tool running now, a pending approval, the last API error, where its shell
-- is, whether it is compacting, its subagents, and the files it edited — and,
-- as @usage@ rows, what its status line last said ("IDE.Web.AgentUsage").
agentEvents :: Text -> IO Text
agentEvents sid = do
  e <- eventsFor sid
  -- And what its status line last said: context use, cost, rate limits.
  mu <- usageFor sid
  tz <- getCurrentTimeZone
  now <- realToFrac <$> getPOSIXTime
  let row k = maybe (k <> "\t-") (\(t, _) -> k <> "\t" <> t)
  return . T.unlines $
    [ row "tool" (seTool e), row "approval" (sePermission e), row "error" (seError e)
    , "cwd\t" <> maybe "-" T.pack (seCwd e)
    , "compacting\t" <> maybe "-" (const "yes") (seCompacting e) ]
    <> [ "subagent\t" <> kind <> " " <> aid | (aid, (kind, _)) <- seSubagents e ]
    <> [ "task\t" <> tid <> " [" <> tiStatus t <> "] " <> tiSubject t | (tid, t) <- seTasks e ]
    <> [ "edited\t" <> T.pack f | (f, _) <- seEdits e ]
    <> [ "usage\t" <> l | u <- maybe [] pure mu, l <- usageLines tz now u ]

-- | 'liveDetail', sharpened by what the hooks have said
-- ("IDE.Web.AgentEvents"): the exact approval it waits for, the tool it is
-- running, or the API error its last turn ended on (a turn that failed so
-- looks merely idle to the CLI).
detailWith :: Maybe SessionEvents -> ClaudeLive -> Text
detailWith me l = case (liveState l, me) of
  ("waiting", Just e) | Just (p, _) <- sePermission e -> "waiting for approval: " <> p
  ("busy", Just e) | Just (t, _) <- seTool e -> t
  ("idle", Just e) | Just (err, _) <- seError e -> "API error: " <> err
  _ -> liveDetail l

-- | One line for one session: its state, then what it is doing.  The state is
-- the first word so a poller can match on it — @waiting@ (blocked on an
-- approval prompt, and it needs a human), @busy@ (working), @idle@
-- (ready for input, i.e. finished), @gone@ (not running any more).  This is
-- what @leksah-cmd agent wait@ polls.
agentStatus :: Text -> IO Text
agentStatus sid = allLiveSessions >>= \live -> case M.lookup sid live of
  -- (here, else a pane on a shared host started for it — "IDE.Web.RemoteAgents")
  Nothing -> ((<|>) <$> startingPane sid <*> remoteStartingPane sid) >>= \case
    -- Its pane is there but the CLI hasn't registered the session.  Nearly
    -- always the first-run question a directory gets asked once ("is this a
    -- project you trust?"), which no amount of waiting will answer.
    Just p  -> return $ T.intercalate "\t"
        [ "starting"
        , "its pane exists but the session has not registered — look at the \
          \pane: it is probably waiting at a first-run prompt (the folder-trust \
          \question, which only a human can answer)"
        , p ] <> "\n"
    Nothing -> agentInfoOf sid >>= \case
      Just i | isJust (aiSuspended i) ->
        return "suspended\tits process was ended on purpose; `agent resume` brings it back\n"
      _ -> return "gone\tthat session is not running\n"
  Just l  -> do
    pane <- (<|>) <$> (M.lookup sid <$> panesBySession)
                  <*> (fmap (\(_, _, p) -> p) <$> remotePaneOf sid)
    ev   <- M.lookup sid <$> sessionEvents
    -- (Its context use, cost and rate limits last, when its status line
    -- has reported: a field a poller reading the first few never sees.)
    mu   <- usageFor sid
    now  <- realToFrac <$> getPOSIXTime
    return $ T.intercalate "\t"
        ([ liveState l
         , detailWith ev l
         , fromMaybe "-" pane
         , T.pack (clDir l) ]
         <> [ summary | Just u <- [mu], let summary = usageSummary now u
                      , not (T.null summary) ]) <> "\n"

-- | Bring an agent's pane to the front — whether or not its session has
-- registered yet.  A fork parked on a first-run question ('agentStatus'\'s
-- @starting@) has a pane but no session for 'showLiveSession' to find, and that
-- pane is exactly where someone needs to look.
-- | Start agent @spec@ on @host@ (its directory @rdir@, host-local) under the
-- name it has in that machine's store: a window in the Agents session of the
-- host's leksah tmux server, with the run key a leksah there would give it,
-- then this leksah's tab on that session brought up with the agent's window
-- current.  Used by a fork onto the host and by resuming an agent there.
startRemoteAgent :: Text -> Text -> FilePath -> AgentSpec -> IO (Either Text ())
startRemoteAgent host name rdir spec = do
  (_, _, line) <- claudeCommandLine (ClaudeAgent spec)
  let key = T.pack rdir <> "#claude#" <> asSession spec
  newRemoteAgentWindow host name rdir key line >>= \case
    Nothing -> return (Left ("could not start a window on " <> host))
    Just (s, _, pane) -> do
      requestRemoteTerm (host <> "#" <> rawTmuxId s)
      _ <- forkIO $ do
        threadDelay 1500000
        selectWindowAsViewer (SshServer host) (T.unpack (rawTmuxId pane))
      return (Right ())

-- | Every live session: this machine's, and those on the hosts whose leksah
-- tmux server this leksah shares ("IDE.Web.RemoteAgents" — their 'clHost' set).
allLiveSessions :: IO (Map Text ClaudeLive)
allLiveSessions = do
  local  <- claudeLiveBySession
  remote <- remoteLiveSessions
  return (local `M.union` M.fromList [ (clSession l, l) | l <- remote ])

-- | Where a session's pane is: here ('paneForSession'), else on its host, as
-- server-qualified ids the tmux helpers route there ('remotePaneOf').
agentPane :: Text -> IO (Maybe (Text, Text, Text))
agentPane sid = paneForSession sid >>= maybe (remotePaneOf sid) (return . Just)

showAgentPane :: Text -> IO Bool
showAgentPane = showAgentPaneWith False

-- | 'showAgentPane', bringing the pane's tab to the active OS window — the one
-- clicked in (an agent card) — instead of raising the window that holds it.
showAgentPaneHere :: Text -> IO Bool
showAgentPaneHere = showAgentPaneWith True

showAgentPaneWith :: Bool -> Text -> IO Bool
showAgentPaneWith here sid = showLiveSessionWith' sid >>= \case
  True  -> return True
  False -> do
    panes <- liveRunPanes
    case [ (s, w, p) | (k, s, w, p) <- panes, ("#claude#" <> sid) `T.isSuffixOf` k ] of
      ((s, w, p) : _) -> showTmuxPaneWith here s w p >> return True
      -- On a shared host: bring up (or open) the tab on its session there, then
      -- — once that tab's viewer session exists — make its window this
      -- viewer's current one and its pane the window's active one.
      [] -> remotePaneOf sid >>= \case
        Just (s, _, p) | Just (host, _) <- T.breakOn "#" <$> T.stripPrefix "ssh://" s -> do
          requestRemoteTerm (host <> "#" <> rawTmuxId s)
          _ <- forkIO $ do
            threadDelay 1500000
            selectWindowAsViewer (SshServer host) (T.unpack (rawTmuxId p))
            tmuxCmdOn (SshServer host) ["select-pane", "-t", T.unpack (rawTmuxId p)]
          return True
        _ -> return False
  where showLiveSessionWith' = if here then showLiveSessionHere else showLiveSession

-- | Type @txt@ into a live session's composer, optionally pressing Enter to
-- submit it as a turn.
--
-- Multi-line text goes through a tmux paste buffer in bracketed-paste mode
-- rather than @send-keys -l@: the CLI reads a paste as one block, where literal
-- newlines would each submit what had been typed so far (turning one report into
-- several truncated turns).
agentSend :: Text -> Bool -> Text -> IO Text
agentSend sid submit txt = agentPane sid >>= \case
  Nothing -> return $ "session " <> sid <> " is not in a pane leksah can reach \
                      \(not running, remote, or in a terminal outside leksah)\n"
  Just (_, _, pane) -> do
    -- Codex takes keys that arrive in a burst for a paste, and an Enter at the
    -- end of one for a newline in it: so hand it a real (bracketed) paste, and
    -- press Enter a moment later.
    codex <- maybe False ((== "codex") . clAgent) . M.lookup sid <$> claudeLiveBySession
    ok <- if codex || T.any (== '\n') txt then pasteTo pane txt
          else sendKeysTo pane ["-l", T.unpack txt]
    if not ok then return "tmux would not deliver the text\n" else do
      when (codex && submit) $ threadDelay 300000
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
  -- (here, else fetched from the shared host it runs on)
  mpath <- transcriptFor sid >>= maybe (remoteTranscriptTail sid) (return . Just)
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

-- | Give an agent a name of the user's choosing ('renameAgent'), and carry it
-- to the tmux session that is named after the agent — so the tab, which shows
-- the session's name, follows.  A session the user named something else is
-- left alone.
renameAgentEverywhere :: Text -> Text -> IO (Either Text Text)
renameAgentEverywhere sid new = do
  old <- M.lookup sid <$> agentNames
  res <- renameAgent sid new
  case (res, old) of
    (Right nm, Just o) -> do
      paneForSession sid >>= mapM_ (\(ts, _, pane) -> do
        let namedAfter c = c == o || (o <> "-") `T.isPrefixOf` c
        cur <- fmap T.strip <$> tmuxOut
                 ["display-message", "-p", "-t", T.unpack ts, "#{session_name}"]
        when (maybe False namedAfter cur) $ do
          fresh <- freshSessionName nm
          tmuxCmd ["rename-session", "-t", T.unpack ts, T.unpack fresh]
        -- …and its window (an agent in the shared Agents session is named
        -- by its window, which the Terminals poll won't rename from a name
        -- that is no longer an agent's)
        win <- fmap T.strip <$> tmuxOut
                 ["display-message", "-p", "-t", T.unpack pane, "#{window_name}"]
        when (win == Just o) $
          tmuxCmd ["rename-window", "-t", T.unpack pane, T.unpack nm])
      return res
    _ -> return res

-- | What a person or agent typed to name an agent — "wren", a session-id
-- prefix, a whole id — as a session id ('resolveAgentName' over the live
-- sessions).  'Left' only when it is ambiguous.
resolveAgent :: Text -> IO (Either Text Text)
resolveAgent arg = do
  live <- claudeLiveBySession
  resolveAgentName (M.keys live) arg

-- | @agent find QUERY@: which agent to hand this to?  Ranks the agents in the
-- Agents pane (running ones, and those that exited recently) by how well what
-- they are — name, title, self-description, branch, PR, directory, role, what
-- they are doing — matches the query's words, with a nudge towards agents that
-- are free (idle, or done) and away from ones that have exited.  Also lists the
-- roles ("IDE.Web.AgentRoles") whose descriptions match, as agents you could
-- start instead.
agentFind :: FilePath -> Text -> IO Text
agentFind cwd q = do
  forest <- agentForest
  roles  <- agentRoles cwd
  let terms = filter ((> 1) . T.length) . T.words . T.toLower
            $ T.map (\c -> if isAlphaNum c || c `elem` ("-_#./" :: String) then c else ' ') q
      flat ns = concatMap (\n -> n : flat (anChildren n)) ns
      fields n =
        [ (T.toLower (anName n), 6 :: Int), (T.toLower (anTitle n), 4)
        , (T.toLower (maybe "" stripTags (anDesc n)), 2)
        , (T.toLower (fromMaybe "" (anBranch n)), 2)
        , (maybe "" (\(k, _) -> "#" <> T.pack (show k)) (anPr n), 3)
        , (T.toLower (T.pack (anDir n)), 1)
        , (T.toLower (fromMaybe "" (anRole n)), 3)
        , (T.toLower (fromMaybe "" (anDone n)), 2)
        , (T.toLower (fromMaybe "" (anActivity n)), 1)
        , (T.toLower (T.unwords (anWorktrees n)), 2) ]
      hits n = [ t | t <- terms, any ((t `T.isInfixOf`) . fst) (fields n) ]
      score n = sum [ w | t <- terms, (f, w) <- fields n, t `T.isInfixOf` f ]
              + (case anState n of
                   "idle" -> 2; "done" -> 2; "gone" -> -3; _ -> 0)
      ranked = take 5 . sortOn (\(sc, _) -> negate sc)
               $ [ (score n, n) | n <- flat forest, not (null (hits n)) ]
      roleHits = [ ro | ro <- roles
                      , any (`T.isInfixOf` T.toLower (roleName ro <> " " <> roleDescription ro)) terms ]
      row (_, n) = T.intercalate "\t"
        [ if T.null (anName n) then "-" else anName n, anSession n, anState n
        , T.pack (anDir n), T.takeWhile (/= '\n') (anTitle n)
        , "matched: " <> T.unwords (hits n) ]
  return . T.unlines $
    (if null ranked
       then [ "No agent matches \"" <> q <> "\"." ]
       else "# name\tsession\tstate\tdir\ttitle\twhy" : map row ranked)
    <> (if null roleHits then [] else
          "" : "Roles you could start for it (leksah-cmd agent fork --as ROLE 'task'):"
          : [ "  " <> roleName ro <> " — " <> roleDescription ro | ro <- roleHits ])
    <> (if null ranked then []
        else [ "", "Hand it over with: leksah-cmd agent send NAME --submit '...'" ])

-- | @agent done@: an agent saying it has finished.  Marks it ✓ in the Agents
-- pane (with the summary), and — if another agent started it — submits the
-- summary to that agent, so the parent wakes up to the result.
agentDone :: Text -> Text -> IO Text
agentDone sid summary = do
  markAgentDone sid summary
  name <- agentNameFor sid
  agentParentOf sid >>= \case
    Nothing -> return ("Marked " <> name <> " done.\n")
    Just p  -> do
      _ <- agentSend p True (name <> " is done: " <> summary)
      pname <- agentNameFor p
      return ("Marked " <> name <> " done, and sent the summary to " <> pname <> ".\n")

-- | @agent roles@: the roles an agent can be started in from @cwd@.
agentRoleList :: FilePath -> IO Text
agentRoleList cwd = agentRoles cwd >>= \case
  [] -> return "No roles are defined. A role is a Claude Code subagent file: \
               \.claude/agents/NAME.md (in the project, or in ~/.claude/agents) \
               \with name: and description: in its front matter.\n"
  rs -> return . T.unlines $
          [ roleName ro <> "\t" <> roleDescription ro <> "\t" <> T.pack (roleFile ro) | ro <- rs ]

-- | @agent suspend@: end an agent's process on purpose, keeping the agent.  It
-- is marked suspended (the Agents and Team panes keep it, as "suspended", with
-- no age limit), then asked to quit the way a user would — the composer
-- cleared, @/exit@ typed and submitted — so it exits 0 and its pane closes
-- (the launch wrapper only keeps a shell after a FAILED exit).  A pane still
-- alive a few seconds later is killed, which ends claude with a hangup.
--
-- A BUSY agent is refused unless @force@: suspending it would throw away the
-- turn it is in the middle of.
agentSuspend :: Text -> Bool -> IO Text
agentSuspend sid force = do
  live <- allLiveSessions
  -- (a remote agent keeps the name its own machine's store gives it)
  name <- if maybe False (isJust . clHost) (M.lookup sid live)
            then M.findWithDefault (T.take 8 sid) sid <$> agentNames
            else agentNameFor sid
  case M.lookup sid live of
    Nothing -> agentInfoOf sid >>= \case
      Just i | isJust (aiSuspended i) -> return (name <> " is already suspended.\n")
      _ -> return (name <> " is not running, so there is nothing to suspend.\n")
    Just l
      | not force, liveState l /= "idle" ->
          return $ name <> " is " <> liveDetail l
                <> "; suspending it now would lose what it is doing. Wait for it \
                   \to go idle, or pass --force.\n"
      | otherwise -> agentPane sid >>= \case
          Nothing -> return $ name <> " is not in a pane leksah can reach, so it \
                                     \cannot be suspended from here.\n"
          Just (_, _, pane) -> do
            -- (the record is its own machine's: a remote one is marked there)
            maybe (setAgentSuspended sid True) (\h -> setRemoteSuspended h sid True) (clHost l)
            _ <- sendKeysTo pane ["C-u"]
            _ <- sendKeysTo pane ["-l", "/exit"]
            _ <- sendKeysTo pane ["Enter"]
            -- (Codex's first Enter only picks /exit from its command popup.)
            when (clAgent l == "codex") $ do
              threadDelay 300000
              void (sendKeysTo pane ["Enter"])
            _ <- forkIO $ do
              threadDelay 6000000
              still <- M.member sid <$> allLiveSessions
              when still . tmuxOnIds [pane] () $ \srv raw ->
                tmuxCmdOn srv ["kill-pane", "-t", raw pane]
            return $ "Suspended " <> name <> ". Resume it with: leksah-cmd agent \
                     \resume " <> T.toLower name <> "\n"

-- | @agent resume@: bring a suspended (or exited) agent back — its own
-- conversation (@claude --resume@), in a tab named after it, and with what it
-- was launched with: the note about the agent that started it (and permission
-- to report back to it), its role's instructions, and the manager's brief if it
-- is the manager.  A plain @claude --resume@ would lose all three.
agentResume :: Text -> IO (Either Text Text)
agentResume sid = remoteAgentRecord sid >>= \case
  -- An agent on a shared host: back on that host ('startRemoteAgent'), named
  -- and marked running again in that machine's stores.
  Just (host, i) | not (null (aiDir i)) -> do
    running <- M.member sid <$> allLiveSessions
    remoteAgentName host sid >>= \case
      Left err -> return (Left ("could not name the agent on " <> host <> ": " <> err))
      Right name
        | running -> showAgentPane sid >> return (Right (name <> " is already running; showing it.\n"))
        | otherwise -> do
            setRemoteSuspended host sid False
            r <- startRemoteAgent host name (aiDir i) AgentSpec
                   { asDir = renderRemotePath host (aiDir i), asSession = sid
                   , asFrom = Nothing, asPrompt = Nothing, asParent = aiParent i
                   , asRoleNote = Nothing, asResume = True, asManager = aiManager i
                   , asCodex = False }
            return (("Resuming " <> name <> " on " <> host <> ".\n") <$ r)
  _ -> agentResumeHere sid

agentResumeHere :: Text -> IO (Either Text Text)
agentResumeHere sid = do
  live <- claudeLiveBySession
  name <- agentNameFor sid
  if sid `M.member` live
    then do
      _ <- showAgentPane sid
      return (Right (name <> " is already running; showing it.\n"))
    else agentInfoOf sid >>= \case
      -- Not one leksah started — but leksah's Codex hooks, or a Claude Code
      -- background session, say where it ran: resume it there.
      Nothing -> codexSessionDir sid >>= \case
       Just d -> do
        runClaudeCmd (CodexResume d sid)
        return (Right ("Resuming " <> name <> " in its own tab.\n"))
       Nothing -> claudeBackgroundSessions >>= \bgs ->
        case [ b | b <- bgs, cbSession b == sid, not (null (cbDir b)) ] of
          (b : _) -> do
            runClaudeCmd (ClaudeResume (cbDir b) sid)
            return (Right ("Resuming " <> name <> " in its own tab.\n"))
          [] -> return . Left $
            "leksah does not know where " <> name <> " ran, so it cannot resume it \
            \(resume it from the workspace tree's Agents node instead)"
      Just i | null (aiDir i) -> return . Left $
        "leksah does not know where " <> name <> " ran, so it cannot resume it"
      Just i -> do
        mrole <- case aiRole i of
          Nothing -> return Nothing
          Just ro -> either (const Nothing) Just <$> findAgentRole (aiDir i) ro
        setAgentSuspended sid False
        codex <- isCodexSession sid
        runClaudeCmd . ClaudeAgent $ AgentSpec
          { asDir = aiDir i, asSession = sid, asFrom = Nothing, asPrompt = Nothing
          , asParent = aiParent i, asRoleNote = roleNote <$> mrole
          , asResume = True, asManager = aiManager i, asCodex = codex }
        return (Right ("Resuming " <> name <> " in its own tab.\n"))

-- | Resume every one of these agents (the Agents pane's "Resume all", for the
-- agents a restart took down together), one at a time with a pause between —
-- a burst of a couple of dozen @claude@ start-ups at once is a burst of
-- logins, MCP servers and API calls.  The offer is answered first, so it is
-- not made again while they come back.  Answers one line per agent.
agentResumeAll :: RanOn -> [Text] -> IO [Text]
agentResumeAll host sids = do
  acknowledgeInterrupted host
  forM (zip [0 :: Int ..] sids) $ \(k, sid) -> do
    when (k > 0) (threadDelay 1500000)
    either id id <$> agentResume sid

-- | "Let the manager choose": hand the agents a restart took down together to
-- the team's manager, which resumes only the ones the user probably still
-- wants — not the finished, the superseded or the throwaway ones — and says
-- which it left and why.  Starts the manager with this as its first task when
-- there is none (in @mdir@, else where most of the agents ran), resumes it
-- first when it is not running, and waits for it to be ready rather than
-- typing into an approval prompt.  The offer (@mhost@, when it came from
-- one) is answered either way: the agents left behind keep their ↻ cards.
agentManagerTriage :: Maybe FilePath -> Maybe RanOn -> [Text] -> IO (Either Text Text)
agentManagerTriage mdir mhost sids0 = do
  mgr <- agentManager
  let mgrSid = aiSession <$> mgr
  forest <- agentForest
  let flat ns = concatMap (\n -> n : flat (anChildren n)) ns
      nodes = [ n | n <- flat forest, anSession n `elem` sids0
                  , Just (anSession n) /= mgrSid, anState n == "interrupted" ]
  if null nodes then return (Left "There are no interrupted agents left to choose from.") else do
    let brief = triageBrief nodes
    case mgr of
      Nothing -> do
        let dirs = map anDir nodes
            common = fst . last $ sortOn snd (M.toList (M.fromListWith (+) [ (d, 1 :: Int) | d <- dirs ]))
            dir = fromMaybe common mdir
        r <- forkAgent (emptyForkRequest dir)
               { frFresh = True, frDir = Just dir, frPlace = PlaceTab
               , frManager = True, frPrompt = Just brief }
        case r of
          Left err -> return (Left err)
          Right _  -> do
            mapM_ acknowledgeInterrupted mhost
            return (Right "Started a manager to choose which agents to resume.")
      Just i -> do
        let sid = aiSession i
        name <- agentNameFor sid
        live <- claudeLiveBySession
        started <- if sid `M.member` live then return (Right "")
                   else agentResume sid
        case started of
          Left err -> return (Left err)
          Right _  -> waitReady sid (120 :: Int) >>= \case
            False -> return . Left $ name <> " (the manager) is not ready for input — \
                                             \it may be waiting on an approval prompt."
            True  -> do
              _ <- agentSend sid True brief
              mapM_ acknowledgeInterrupted mhost
              return (Right ("Asked " <> name <> " to choose which agents to resume."))
  where
    -- Running and not blocked on a prompt (busy is fine: a submitted message
    -- queues until its turn ends).
    waitReady _ 0 = return False
    waitReady sid k = do
      live <- claudeLiveBySession
      case M.lookup sid live of
        Just l | clStatus l /= Just "waiting" -> paneForSession sid >>= \case
          Just _  -> return True
          Nothing -> threadDelay 1000000 >> waitReady sid (k - 1)
        _ -> threadDelay 1000000 >> waitReady sid (k - 1)

-- | The manager's brief for 'agentManagerTriage': what happened, how to judge
-- each agent, and one line of what leksah knows about each.
triageBrief :: [AgentNode] -> Text
triageBrief ns = T.unlines $
  [ "The machine restarted (or leksah's terminals went away) and these "
      <> T.pack (show (length ns)) <> " agents were interrupted mid-session. \
    \Resume only the ones the user is likely to want back, with \
    \`leksah-cmd agent resume NAME` (one at a time)."
  , ""
  , "Do NOT resume an agent that is probably not needed any more:"
  , "- it said it was done, or its work is evidently finished (its PR is merged, \
    \its last message reports the task complete with nothing left to do);"
  , "- it has been superseded: another agent — running now, or in this list — \
    \has taken over the same task, branch or PR, or a later fork carries its \
    \work on (resume only the most recent);"
  , "- it was a throwaway: a test of leksah's agent tools, a one-question \
    \helper that has answered, a forked helper whose task is complete or whose \
    \parent is not being resumed."
  , "When in doubt, resume agents with unfinished work, especially on an open \
    \branch or PR. Look before deciding: `leksah-cmd agent read NAME --last 3` \
    \(what it last said), `leksah-cmd agent list` (who is running now), and \
    \git / gh for the state of its branch and PR. Do not resume yourself."
  , ""
  , "Then reply with a short table — name, resumed or left, a one-line reason. \
    \The ones you leave keep their ↻ card in the Agents pane, so the user can \
    \still resume them."
  , ""
  , "The interrupted agents:" ]
  <> concatMap one ns
  where
    one n =
      [ "- " <> (if T.null (anName n) then T.take 8 (anSession n) else anName n)
          <> " (" <> T.take 8 (anSession n) <> ") · " <> T.pack (anDir n)
          <> maybe "" (" · branch " <>) (anBranch n)
          <> maybe "" (\(k, st) -> " · PR #" <> T.pack (show k) <> " " <> st) (anPr n)
          <> maybe "" (" · role " <>) (anRole n)
          <> maybe "" (" · started by " <>) (anParentName n)
          <> (if T.null (anAge n) then "" else " · " <> anAge n)
      , "  title: " <> T.take 160 (T.takeWhile (/= '\n') (anTitle n)) ]
      <> [ "  said it was done: " <> T.take 200 d | Just d <- [anDone n] ]

-- | @agent refresh@: ask an agent to rewrite its title, description and bio
-- (what the Agents pane's ⟳ does).
agentRefresh :: Text -> IO Text
agentRefresh sid = agentSend sid True agentRefreshPrompt

-- | @agent manager@: who the manager is, and what state it is in.
agentManagerStatus :: IO Text
agentManagerStatus = agentManager >>= \case
  Nothing -> return "There is no manager. Start one with: leksah-cmd agent manager start\n"
  Just i  -> do
    live <- claudeLiveBySession
    name <- agentNameFor (aiSession i)
    let st = case M.lookup (aiSession i) live of
          Just l  -> fromMaybe "idle" (clStatus l)
          Nothing | isJust (aiSuspended i) -> "suspended"
                  | otherwise              -> "not running"
    return (name <> "\t" <> aiSession i <> "\t" <> st <> "\t" <> T.pack (aiDir i) <> "\n")

-- | @agent manager start@: start the workspace's manager in @dir@ — or bring
-- the existing one back (resume it if it is not running, show it if it is):
-- there is one manager.
agentManagerStart :: FilePath -> IO (Either Text Text)
agentManagerStart dir = agentManager >>= \case
  Just i -> agentResume (aiSession i)
  Nothing -> forkAgent (emptyForkRequest dir)
      { frFresh = True, frDir = Just dir, frPlace = PlaceTab, frManager = True
      , frPrompt = Just "Introduce yourself to the user in two or three lines \
                        \as the team's manager, then run `leksah-cmd agent list` \
                        \and say briefly who is on the team and what needs \
                        \attention." }
      >>= \case
        Left err       -> return (Left err)
        Right (_, msg) -> return (Right msg)

-- | The last thing a session said (its latest assistant text), for the Agents
-- pane's "last said" peek — 'Nothing' if it has said nothing yet.
agentLastSaid :: Text -> IO (Maybe Text)
agentLastSaid sid = transcriptFor sid >>= \case
  Nothing   -> return Nothing
  Just path -> listToMaybe . reverse <$> assistantTail path

--------------------------------------------------------------------------------
-- helpers

-- | Every live session's tmux pane id, in one pass (where 'paneForSession' pays
-- for a @ps@ and a tmux listing per session).
panesBySession :: IO (Map Text Text)
panesBySession = do
  ps <- claudePaneSessions
  return $ M.fromList [ (clSession l, p) | ((_, _, p), l) <- ps ]

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
transcriptFor sid = (`catch` \(_ :: SomeException) -> return Nothing) $
 codexTranscriptOf sid >>= \case
 -- (A Codex session's rollout, as its hooks reported it.)
 Just p -> return (Just p)
 Nothing -> do
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
-- | A Claude Code hook reporting on a leksah-launched session (installed by
-- 'IDE.Web.Claude.claudeHooksFlag'; arrives through @leksah-cmd agent hook
-- EVENT@, whose pid — a child of the session's @claude@ — is @mpid@).
-- Answers what the hook should print: SessionStart's extra context as the
-- JSON Claude Code expects, or nothing.  Never throws — a hook must not fail.
--
--   * SessionStart: a @/clear@ or @/branch@ switches the SAME claude process
--     to a new session id — the same agent, so the new id takes the old one's
--     name and record ('adoptContinuation', 'moveAgentRecord'); the old id is
--     the one this process ran last (remembered from its previous
--     SessionStart, else its session file).  Then anything but a fresh
--     startup gets a re-briefing — its name, who started it, its role, the
--     manager's brief — which a resumed session started outside leksah's
--     launcher (claude -c, the resume picker) would not otherwise have.
--   * Stop, Notification, SessionEnd: the status surfaces look again now.
--
-- Every event but the per-tool ones ('perToolHookEvents') is logged, one line,
-- to @~\/.leksah\/agent-hooks.log@.
agentHook :: Maybe Int -> Text -> Text -> [Text] -> IO Text
agentHook mpid ev raw ("codex" : rest) =
  codexHook mpid ev raw (mfilter (not . T.null) (listToMaybe rest))
agentHook mpid ev raw _ = (`catch` \(_ :: SomeException) -> return "") $ do
  let h = parseHookIn raw
      field = hiField h
      sid = field "session_id"
  -- Only SessionStart needs to know which claude process is calling (a `ps`
  -- walk, ~100ms): every other event skips the lookup, and Stop — a
  -- synchronous hook at the end of every turn — answers at once.  The
  -- per-tool events, several a turn, are not logged either.
  proc <- if ev == "SessionStart" then maybe (return Nothing) claudeProcessOf mpid
                                  else return Nothing
  if ev `elem` perToolHookEvents then return ()
    else logHook ev proc [sid, field "source", field "reason", field "notification_type"
                         , field "error", field "tool_name"]
  case (ev, sid) of
    ("SessionStart", Just new) -> do
      let source = fromMaybe "" (field "source")
          ppid = fst <$> proc
      prev <- atomicModifyIORef' hookSessions $ \m ->
        ( maybe m (\p -> M.insert p new m) ppid
        , ppid >>= (`M.lookup` m) )
      -- The process's previous session: what its last SessionStart said, else
      -- what its session file still says (it may not have been rewritten yet).
      let old = case prev of
            Just o | o /= new -> Just o
            _ -> case snd <$> proc of
              Just o | o /= new -> Just o
              _                 -> Nothing
      case (source, old) of
        (src, Just o) | src `elem` ["clear", "fork"] -> do
          _ <- adoptContinuation o new
          moveAgentRecord (src /= "clear") o new
        _ -> return ()
      -- A session that starts in a linked worktree (`claude --worktree`, a
      -- background session's isolation) is working there; off the hook's
      -- critical path — it is synchronous, and this runs git.
      when (source `elem` ["startup", "resume", "fork"]) $
        mapM_ (\d -> void . forkIO $ ensureSessionWorktree new (T.unpack d)) (field "cwd")
      -- After a compaction the session starts again with source "compact".
      when (source == "compact") $ recordCompacting new False
      pokeClaudeStatus
      -- A session leksah did not launch (`claude` in a terminal, with the
      -- leksah plugin) never got leksah's --append-system-prompt notes: give
      -- them now, so it knows its name, to register worktrees, and the rest.
      -- (After /clear, a compaction or a resume too: their SessionStart is
      -- where a session's context is rebuilt.)
      plain <- maybe (return False) (fmap not . launchedByLeksah . fst) proc
      base <- if source == "startup" then return [] else briefing new
      extra <- if plain then plainSessionNotes new (source == "startup") else return []
      let notes = base <> extra
      -- The title it gave itself (agent describe), as its Claude Code
      -- session title too — sessionTitle applies on startup/resume/fork.
      title <- if source `elem` ["startup", "resume", "fork"]
                 then titleForClaude new (field "session_title") else return Nothing
      return (hookOutput "SessionStart"
                ([ "additionalContext" .= T.intercalate "\n\n" notes | not (null notes) ]
                 <> [ "sessionTitle" .= t | Just t <- [title] ]))
    (_, Just s) -> do
      recordHookEvent ev s h
      -- The status surfaces update at once on the events that change what
      -- they show (a turn starting or ending, an approval, an error …) —
      -- not on the per-tool ones, several a turn.
      unless (ev `elem` quietHookEvents) pokeClaudeStatus
      case ev of
        -- Its own title follows a new prompt into Claude Code.
        "UserPromptSubmit" -> do
          title <- titleForClaude s (field "session_title")
          return (hookOutput "UserPromptSubmit" [ "sessionTitle" .= t | Just t <- [title] ])
        "PostToolUse" -> noteEdits s (editedFiles (hiTool h) (hiInput h) (hiResponse h))
        _ -> return ""
    _ -> pokeClaudeStatus >> return ""

-- | One line of a shared host's event log ("IDE.Web.RemoteAgents"): what an
-- agent THERE told its hooks (or its status line), recorded here as this
-- leksah records its own agents' — so a remote agent's card shows what it is
-- doing, its edits, its context use and what it waits for, whether or not a
-- leksah runs on its machine.  (Recording only: no answer goes back, and the
-- SessionStart briefing is the host's own leksah's business.)
remoteAgentEvent :: Text -> BS.ByteString -> IO ()
remoteAgentEvent host line = case decodeStrict' line of
  Just (Object o)
    | Just (String kind) <- KM.lookup "kind" o
    , Just payload <- KM.lookup "payload" o -> do
        let raw = TE.decodeUtf8 (BL.toStrict (encode payload))
            ev  = case KM.lookup "event" o of Just (String e) -> e; _ -> ""
        case kind of
          "hook" | Just (String s) <- sessionOf payload, ev /= "SessionStart" -> do
            let h = parseHookIn raw
            recordHookEvent ev s h
            when (ev == "PostToolUse") $
              recordEdit s (editedFiles (hiTool h) (hiInput h) (hiResponse h))
            unless (ev `elem` quietHookEvents) pokeClaudeStatus
          "statusline" -> recordStatusLine raw
          -- (an agent's describe / done that found no leksah on its machine)
          "command" | Just (String s) <- sessionOf payload -> do
            let args = case payload of
                  Object p | Just (Array a) <- KM.lookup "args" p -> [ t | String t <- toList a ]
                  _ -> []
            applyRemoteCommand host ev s args
            pokeClaudeStatus
          _ -> return ()
  _ -> return ()
  where sessionOf (Object p) = KM.lookup "session_id" p
        sessionOf _          = Nothing

-- | One hook's JSON, parsed once.
data HookIn = HookIn
  { hiField    :: Text -> Maybe Text   -- ^ a top-level string field
  , hiTool     :: Text                 -- ^ @tool_name@ (or @""@)
  , hiInput    :: Object               -- ^ @tool_input@ (or empty)
  , hiResponse :: Maybe Value          -- ^ @tool_response@
  }

parseHookIn :: Text -> HookIn
parseHookIn raw = HookIn
  { hiField    = field
  , hiTool     = fromMaybe "" (field "tool_name")
  , hiInput    = fromMaybe mempty (v >>= parseMaybe (withObject "hook" (.: "tool_input")))
  , hiResponse = v >>= parseMaybe (withObject "hook" (.: "tool_response")) }
  where
    v = decodeStrict' (TE.encodeUtf8 raw) :: Maybe Value
    field k = v >>= parseMaybe (withObject "hook" (.: Key.fromText k))

-- | The events that do not change what the status surfaces show by
-- themselves (they feed the activity line, read on the next poll).
quietHookEvents :: [Text]
quietHookEvents =
  [ "SubagentStart", "SubagentStop", "PreCompact", "PostCompact"
  , "TaskCreated", "TaskCompleted", "PreToolUse", "PostToolUse"
  , "PostToolUseFailure", "CwdChanged" ]

-- | What a hook event says about session @s@ ("IDE.Web.AgentEvents"),
-- recorded the same way whichever CLI sent it — Claude Code and Codex send
-- the same events, with the same fields.  Everything but SessionStart (each
-- CLI's own) and the files a tool changed ('noteEdits').
recordHookEvent :: Text -> Text -> HookIn -> IO ()
recordHookEvent ev s h = case ev of
  -- A prompt was submitted (by the user, or `agent send --submit`): a new
  -- turn starts — the poll counts a just-submitted prompt as working,
  -- whatever the session's state says yet.
  "UserPromptSubmit" -> recordPromptSubmit s
  -- Its subagents, for the activity line.  An empty type is one of Claude
  -- Code's own internal agents (prompt suggestions, /btw).
  "SubagentStart" -> case (field "agent_id", field "agent_type") of
    (Just aid, Just kind) | not (T.null kind) -> recordSubagentStart s aid kind
    _ -> return ()
  "SubagentStop" -> mapM_ (recordSubagentStop s) (field "agent_id")
  -- Compacting its conversation, which can take a while and otherwise looks
  -- like nothing at all.  (Claude's PreCompact hook runs async: its stdout
  -- would become instructions for the summary.)
  "PreCompact"  -> recordCompacting s True
  "PostCompact" -> recordCompacting s False
  "SessionEnd"  -> recordSessionEnd s
  -- Its task list (Claude Code's Task tools): created, and done.
  "TaskCreated" -> case (field "task_id", field "task_subject") of
    (Just tid, Just sub) -> recordTaskCreated s tid sub
    _ -> return ()
  "TaskCompleted" -> mapM_ (\tid -> recordTaskStatus s tid "completed") (field "task_id")
  -- What it is doing this moment (the activity line).
  "PreToolUse" -> recordToolStart s (field "tool_use_id") (describeToolUse tool inp)
  "PostToolUseFailure" -> recordToolEnd s (field "tool_use_id") (approvalText tool inp)
  "PostToolUse" -> do
    recordToolEnd s (field "tool_use_id") (approvalText tool inp)
    -- Task status changes come only as TaskUpdate calls, and a task's
    -- "doing" form only with TaskCreate.
    let str k = parseMaybe (.: k) inp :: Maybe Text
    case tool of
      "TaskUpdate" | Just tid <- str "taskId", Just st <- str "status" ->
        recordTaskStatus s tid st
      "TaskCreate" | Just sub <- str "subject", Just act <- str "activeForm" ->
        recordTaskActive s sub act
      _ -> return ()
  -- What it is waiting for you to approve, in full.
  "PermissionRequest" -> recordPermission s (approvalText tool inp)
  -- A turn that ended on an API error, which would otherwise look idle.
  "StopFailure" -> recordFailure s $ fromMaybe "unknown" (field "error")
    <> maybe "" (\m -> " — " <> T.take 160 m) (field "last_assistant_message")
  "Stop" -> recordTurnEnd s
  "CwdChanged" -> mapM_ (recordCwd s . T.unpack) (field "new_cwd")
  _ -> return ()
  where
    field = hiField h
    tool = hiTool h
    inp = hiInput h

-- | Record the files a tool call changed, and answer with the note telling
-- the agent that another live agent edited one of them lately (if one did).
noteEdits :: Text -> [FilePath] -> IO Text
noteEdits _ []    = return ""
noteEdits s files = recordEdit s files >> overlapNote s files

-- | A Codex hook (@leksah-cmd agent hook EVENT codex [TOKEN]@, run by the
-- @~\/.leksah\/codex-hook@ wrapper — see "IDE.Web.Codex").  The same events as
-- Claude Code's, feeding the same per-session records, plus the one thing
-- Claude Code does for itself: the live record that says the session is
-- running and what it is doing ('codexSessionStart', 'codexSetStatus').
--
-- At SessionStart, @token@ — what a leksah launch exported as
-- @LEKSAH_AGENT_TOKEN@ and used as the agent's id until now (Codex cannot be
-- told its session id) — hands everything leksah knows over to the real id:
-- its name, and its record (parent, role, the prompt it was given).  Then
-- the session is briefed, in plain text (Codex adds a SessionStart hook's
-- stdout to the session as developer context): its name, who started it and
-- how to report back, its role, the worktree note.
codexHook :: Maybe Int -> Text -> Text -> Maybe Text -> IO Text
codexHook mpid ev raw token = (`catch` \(_ :: SomeException) -> return "") $ do
  let h = parseHookIn raw
      field = hiField h
      sid = field "session_id"
  ignored <- maybe (return False) (\s -> S.member s <$> readIORef codexIgnored) sid
  if ignored then return "" else do
   if ev `elem` perToolHookEvents then return ()
     else logHook ev Nothing [sid, Just "codex", field "source", token, field "tool_name"]
   case (ev, sid) of
    ("SessionStart", Just new) -> do
      let source = fromMaybe "" (field "source")
      proc <- maybe (return Nothing) codexProcessOf mpid
      case proc of
        -- `codex exec` and the like run the same (global) hooks: they are
        -- not agents — no record, no name, no briefing; their later events
        -- are dropped too.
        Just (_, args) | not (codexInteractive args) -> do
          atomicModifyIORef' codexIgnored (\m -> (S.insert new m, ()))
          return ""
        _ -> do
          prev <- case proc of
            Just (p, _) -> codexSessionStart p new (maybe "" T.unpack (field "cwd"))
                                             (T.unpack <$> field "transcript_path")
            Nothing -> return Nothing
          -- The launch token, the first time it is seen (a later /new in the
          -- same process inherits the environment, and is handled as a /clear
          -- below).
          conts <- agentContinuations
          case token of
            Just t | t /= new, t `M.notMember` conts -> do
              _ <- adoptContinuation t new
              moveAgentRecord True t new
            _ -> case (source, prev) of
              (src, Just o) | src `elem` ["clear", "fork"] -> do
                _ <- adoptContinuation o new
                moveAgentRecord (src /= "clear") o new
              -- What the process was recorded as before its first prompt
              -- (IDE.Web.Claude.codexDiscover's token for a codex typed into
              -- a terminal): the same agent, which keeps its name.
              (_, Just o) -> codexStarted o >>= \started -> unless started $ do
                _ <- adoptContinuation o new
                moveAgentRecord True o new
              _ -> return ()
          when (source `elem` ["startup", "resume", "fork"]) $
            mapM_ (\d -> void . forkIO $ ensureSessionWorktree new (T.unpack d)) (field "cwd")
          when (source == "compact") $ recordCompacting new False
          pokeClaudeStatus
          notes <- briefing new
          return (T.intercalate "\n\n" (notes <> [worktreeNote]))
    (_, Just s) -> do
      recordHookEvent ev s h
      -- And the live state, which Codex does not keep for itself.
      changed <- case ev of
        "UserPromptSubmit"  -> codexSetStatus s "busy" Nothing >> return True
        "PreToolUse"        -> codexSetStatus s "busy" Nothing
        "PostToolUse"       -> codexSetStatus s "busy" Nothing
        "PermissionRequest" -> codexSetStatus s "waiting" (Just (approvalText (hiTool h) (hiInput h)))
        "Stop"              -> codexSetStatus s "idle" Nothing >> return True
        "SessionEnd"        -> codexSessionEnd s >> return True
        _                   -> return False
      when changed pokeClaudeStatus
      if ev /= "PostToolUse" then return "" else
        -- The files it changed: an apply_patch names them in its patch; any
        -- other tool the way Claude Code's do (a file_path …).  Made absolute
        -- against its cwd — a patch's paths are relative — so they compare
        -- with every other agent's.
        let named = case codexPatchFiles (Object (hiInput h)) of
              [] -> editedFiles (hiTool h) (hiInput h) (hiResponse h)
              ps -> ps
            cwd = maybe "" T.unpack (field "cwd")
        in noteEdits s [ if null cwd then f else normalise (cwd </> f) | f <- named ]
    _ -> return ""

-- | Codex sessions that are not agents (@codex exec@ …): their hooks are
-- ignored.
{-# NOINLINE codexIgnored #-}
codexIgnored :: IORef (S.Set Text)
codexIgnored = unsafePerformIO (newIORef S.empty)

-- Another live agent edited one of these files lately: tell this one, once
-- per file and colleague (an async hook's context reaches it next turn).
overlapNote :: Text -> [FilePath] -> IO Text
overlapNote s files = do
  live <- claudeLiveBySession
  hits <- fmap concat . forM files $ \f -> do
    others <- filter (`M.member` live) <$> editedRecently (2 * 3600) s f
    fresh <- atomicModifyIORef' overlapWarned $ \w ->
      let new = [ o | o <- others, (s, f, o) `notElem` w ]
      in (w <> [ (s, f, o) | o <- new ], new)
    return [ (f, o) | o <- fresh ]
  if null hits then return "" else do
    named <- forM hits $ \(f, o) -> (,) f <$> agentNameFor o
    let say (f, nm) = nm <> " also edited " <> T.pack f <> " in the last two hours"
        msg = "Leksah: " <> T.intercalate "; " (map say named)
           <> ". Their changes may not be committed yet — check with them before \
              \going further (`leksah-cmd agent send NAME …`), so your edits \
              \do not collide."
    return (hookOutput "PostToolUse" [ "additionalContext" .= msg ])

briefing :: Text -> IO [Text]
briefing sid = do
  name <- agentNameFor sid
  minfo <- agentInfoOf sid
  roleText <- case (minfo >>= aiRole, minfo) of
    (Just ro, Just i) -> either (const Nothing) (Just . roleNote) <$> findAgentRole (aiDir i) ro
    _                 -> return Nothing
  return $ [ nameNote name | not (T.null name) ]
        <> [ agentNote p sid | Just p <- [minfo >>= aiParent] ]
        <> maybe [] (: []) roleText
        <> [ managerNote | maybe False aiManager minfo ]

-- (The name only at startup: 'briefing' already gives it otherwise.  And
-- the worktree note without "pre-approved": only leksah's own launches
-- pre-approve `leksah-cmd agent register`.)
plainSessionNotes :: Text -> Bool -> IO [Text]
plainSessionNotes sid withName = do
  name <- agentNameFor sid
  return ([ nameNote name | withName, not (T.null name) ]
          <> [ T.replace " — pre-approved, it never prompts" "" worktreeNote, planHtmlNote ])

-- A hook's answer: nothing at all when there is nothing to say, else
-- @{"hookSpecificOutput": {"hookEventName": EV, …}}@.
hookOutput :: Text -> [Pair] -> Text
hookOutput _ [] = ""
hookOutput evName ps = TE.decodeUtf8 . BL.toStrict . encode $ object
  [ "hookSpecificOutput" .= object (("hookEventName" .= evName) : ps) ]

-- | Which @(session, file, other session)@ overlaps have been pointed out
-- already — each once.
{-# NOINLINE overlapWarned #-}
overlapWarned :: IORef [(Text, FilePath, Text)]
overlapWarned = unsafePerformIO (newIORef [])

-- | The files a tool call edited: the path an Edit\/Write\/NotebookEdit names,
-- or the ones a Bash command's @bashEditDiff@ lists (its shape is not
-- documented, so any string, or any object's @filePath@\/@file_path@\/@path@,
-- found in it counts).
editedFiles :: Text -> Object -> Maybe Value -> [FilePath]
editedFiles name inp resp = map T.unpack $ case name of
  "Edit"         -> str "file_path"
  "MultiEdit"    -> str "file_path"
  "Write"        -> str "file_path"
  "NotebookEdit" -> str "notebook_path"
  "Bash"         -> maybe [] paths (resp >>= parseMaybe (withObject "resp" (.: "bashEditDiff")))
  _              -> []
  where
    str k = maybe [] (: []) (parseMaybe (.: k) inp)
    paths :: Value -> [Text]
    paths = \case
      String t -> [t]
      Array xs -> concatMap paths (toList xs)
      Object o -> case [ p | k <- ["filePath", "file_path", "path"]
                           , Just p <- [parseMaybe (.: k) o] ] of
                    (p : _) -> [p]
                    []      -> concatMap paths (toList o)
      _        -> []

-- | An approval request in a few words, showing what would actually run:
-- @Bash: rm -rf node_modules@, @Edit Main.hs@, @WebFetch https://…@.
approvalText :: Text -> Object -> Text
approvalText name inp = case parseMaybe (.: "command") inp of
  Just c | name == "Bash" -> "Bash: " <> T.take 120 (T.strip (T.takeWhile (/= '\n') c))
  _                       -> describeToolUse name inp

-- | Answer the permission prompt session @sid@ is blocked on — the Agents
-- pane's Approve / Deny.  Only when its pane really shows one ("Do you want
-- to proceed?"): a stale click must never type into whatever is on screen
-- now.  Approve picks option 1 ("Yes" — never the "always allow" or
-- "switch to auto mode" ones); Deny is Esc, which tells Claude no.
--
-- Two checks before a key is sent:
--
--   * the pane shows a permission prompt at all — any kind (Bash, an edit, a
--     fetch …): its numbered options start "1. Yes" and it ends "Esc to
--     cancel";
--   * it is the prompt being answered: @expected@ (what the card showed;
--     else what leksah last recorded for this session) must appear in it —
--     so a second click, or a card one refresh behind, never approves the
--     NEXT prompt, which nobody has seen yet.
agentAnswerApproval :: Text -> Bool -> Maybe Text -> IO Text
agentAnswerApproval sid yes mexpected = agentPane sid >>= \case
  Nothing -> return "Its pane can't be found, so there is nothing to answer.\n"
  Just (_, _, pane) -> do
    -- (The whole screen: 'tmuxOut' keeps only the first line.)
    screen <- tmuxOnIds [pane] "" $ \srv raw ->
                tmuxStdoutOn srv ["capture-pane", "-p", "-t", raw pane]
    recorded <- fmap fst . sePermission <$> eventsFor sid
    codex <- maybe False ((== "codex") . clAgent) . M.lookup sid <$> claudeLiveBySession
    let flat = T.unwords (T.words screen)
        -- Claude Code: "1. Yes" … "Esc to cancel".  Codex: "1. Yes, proceed
        -- (y)" … "Press enter to confirm or esc to cancel".
        asking = "1. Yes" `T.isInfixOf` screen
                 && T.toLower "Esc to cancel" `T.isInfixOf` T.toLower screen
        expected = mexpected <|> recorded
        matches = maybe True (\e -> approvalKey e `T.isInfixOf` flat) expected
    if not asking then return "It is not asking for approval right now.\n"
    else if not matches then return
      "The approval on screen is not the one shown — look at its pane before answering.\n"
    else do
        -- (Codex's own key for "Yes, proceed" is y; Esc is "No" in both.)
        ok <- sendKeysTo pane [if yes then (if codex then "y" else "1") else "Escape"]
        name <- agentNameFor sid
        return $ if not ok then "tmux would not deliver the answer.\n"
                 else (if yes then "Approved for " else "Denied for ") <> name <> ".\n"
  where
    -- The part of an approval's text ('approvalText': "Bash: CMD", "Edit
    -- FILE", "Fetch URL" …) that its prompt shows: what follows the tool's
    -- name, whitespace-normalised, its first 40 characters (a long command
    -- wraps on screen).
    approvalKey t =
      let body = case T.breakOn ": " t of
                   (_, rest) | not (T.null rest) -> T.drop 2 rest
                   _ -> T.drop 1 (T.dropWhile (/= ' ') t)
      in T.take 40 (T.unwords (T.words body))

-- | Did leksah launch this claude process?  Its command line carries leksah's
-- appended notes then — every leksah launch adds 'worktreeNote', whose
-- wording is leksah's own (a user's own --append-system-prompt is not it).
launchedByLeksah :: Int -> IO Bool
launchedByLeksah pid = (`catch` \(_ :: SomeException) -> return True) $ do
  (_, out, _) <- readProcessWithExitCode "ps" ["-o", "args=", "-p", show pid] ""
  return ("register_worktree MCP tool" `T.isInfixOf` T.pack out)

-- | The session each claude process (by pid) last started — how a SessionStart
-- for a NEW id in the same process is recognised as the same agent.
{-# NOINLINE hookSessions #-}
hookSessions :: IORef (Map Int Text)
hookSessions = unsafePerformIO (newIORef M.empty)

-- | One line per hook event in @~\/.leksah\/agent-hooks.log@ (kept to the last
-- ~2000 lines): when, which event, which process, and the fields that matter.
logHook :: Text -> Maybe (Int, Text) -> [Maybe Text] -> IO ()
logHook ev proc fs = (`catch` \(_ :: SomeException) -> return ()) $ do
  home <- getHomeDirectory
  now  <- getCurrentTime
  let path = home </> ".leksah" </> "agent-hooks.log"
      ln = T.intercalate "\t" $
             [ T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now), ev
             , maybe "-" (\(p, s) -> T.pack (show p) <> ":" <> T.take 8 s) proc ]
             <> map (fromMaybe "-") fs
  ex <- doesFileExist path
  old <- if ex then T.lines . TE.decodeUtf8With lenientDecode <$> BS.readFile path else return []
  BS.writeFile path . TE.encodeUtf8 . T.unlines $ drop (max 0 (length old - 1999)) old <> [ln]

-- | The tail of a session's conversation as @(fromUser, text)@ turns, oldest
-- first: what the user typed (or another agent sent it) and what it answered —
-- tool calls, tool results, and the CLI's own command and reminder messages
-- left out.  For the Manager pane, which shows its conversation as a chat.
agentConversation :: Text -> Int -> IO [(Bool, Text)]
agentConversation sid n = transcriptFor sid >>= \case
  Nothing   -> return []
  Just path | isRollout path -> (\ts -> drop (max 0 (length ts - n)) ts) <$> rolloutTurns path
  Just path -> (`catch` \(_ :: SomeException) -> return []) $ do
    ls <- withFile path ReadMode $ \h -> do
      size <- hFileSize h
      let back = min size (512 * 1024)
      hSeek h SeekFromEnd (negate back)
      BS.lines <$> BS.hGet h (fromIntegral back)
    let turns = [ t | l <- ls, Just t <- [decodeStrict' l >>= parseMaybe turnP]
                    , not (T.null (T.strip (snd t))) ]
    return (drop (max 0 (length turns - n)) turns)
  where
    turnP = withObject "line" $ \o -> do
      ty <- o .: "type"
      meta <- o .:? "isMeta"
      m  <- o .: "message"
      case ty :: Text of
        "assistant" -> do
          cs <- m .: "content"
          return (False, T.intercalate "\n" (mapMaybe (parseMaybe (textOf "text")) (cs :: [Value])))
        "user" | meta /= Just True -> do
          c <- m .: "content"
          txt <- case c of
            String t -> return t
            Array _  -> T.intercalate "\n" . mapMaybe (parseMaybe (textOf "text"))
                          <$> parseJSON' c
            _        -> fail "no text"
          -- The CLI's own messages (slash-command echoes, reminders) start
          -- with a tag; they are not things anybody said.
          if "<" `T.isPrefixOf` T.stripStart txt then fail "cli message"
            else return (True, txt)
        _ -> fail "not a turn"
    textOf want = withObject "content" $ \c -> do
      t <- c .: "type"
      if t == (want :: Text) then c .: "text" else fail "not text"
    parseJSON' v = maybe (fail "content") return (parseMaybe parseJSON v :: Maybe [Value])

assistantTail :: FilePath -> IO [Text]
assistantTail path | isRollout path = map snd . filter (not . fst) <$> rolloutTurns path
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
