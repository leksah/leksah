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
  , forkAgent
  , agentList
  , agentStatus
  , agentSend
  , agentRead
  , agentLastSaid
  , renameAgentEverywhere
  , agentConversation
  , agentHook
  , resolveAgent
  , agentFind
  , agentDone
  , agentRoleList
  , agentSuspend
  , agentResume
  , agentRefresh
  , agentManagerStatus
  , agentManagerStart
  , newAgentSessionId
  , showAgentPane
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad (filterM, forM, when)

import Data.Char (isAlphaNum)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson
       (Value(..), decodeStrict', encode, object, parseJSON, withObject, (.:),
        (.:?), (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Text.Encoding as TE (decodeUtf8, decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

import System.Directory
       (doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory)
import System.FilePath ((</>), dropTrailingPathSeparator)
import System.IO
       (withFile, IOMode(ReadMode), hSeek, SeekMode(SeekFromEnd), hFileSize)

import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.AgentInfo
       (AgentInfo(..), AgentNode(..), agentForest, agentInfoOf, agentManager,
        agentParentOf, agentRefreshPrompt, agentTitles, markAgentDone,
        moveAgentRecord, recordAgentFork, setAgentManager, setAgentRole,
        setAgentSuspended)
import IDE.Web.AgentNames
       (adoptContinuation, agentNameFor, agentNames, renameAgent,
        resolveAgentName, touchAgentNames)
import IDE.Web.AgentRoles (AgentRole(..), agentRoles, findAgentRole, roleNote)
import IDE.Web.Claude
       (AgentSpec(..), ClaudeCmd(..), ClaudeLive(..), claudeAvailable,
        claudeCommandLine, claudeLiveBySession, claudeLiveOwners,
        claudeSessionLabel, claudeTranscriptPath, newAgentSessionId,
        paneForSession, runClaudeCmd,
        showLiveSession, showTmuxPane, claudeProcessOf, nameNote, agentNote,
        managerNote)
import IDE.Web.ClaudeStatus (pokeClaudeStatus)
import IDE.Web.NewLwRequest (requestNewLw)
import IDE.Web.RemoteTermRequest (requestLocalTerm)
import IDE.Web.ReplTmux
       (freshSessionName, liveRunPanes, livePanePids, newSessionWindow, pasteTo,
        sendKeysTo, splitPane, tmuxCmd, tmuxOut)

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
  } deriving (Eq, Show)

emptyForkRequest :: FilePath -> ForkRequest
emptyForkRequest cwd = ForkRequest
  { frPlace = PlaceBeside, frDir = Nothing, frFrom = Nothing, frFresh = False
  , frPrompt = Nothing, frParent = Nothing, frCwd = cwd, frRole = Nothing
  , frManager = False }

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
forkAgent :: ForkRequest -> IO (Either Text (Text, Text))
forkAgent fr0 = case frRole fr0 of
  Nothing -> forkAgentAs Nothing fr0
  Just n  -> findAgentRole (fromMaybe (frCwd fr0) (frDir fr0)) n >>= \case
    Left err   -> return (Left err)
    Right role -> forkAgentAs (Just role) fr0 { frFresh = True }

forkAgentAs :: Maybe AgentRole -> ForkRequest -> IO (Either Text (Text, Text))
forkAgentAs mrole fr = (`catch` \(e :: SomeException) ->
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
                                  , asParent  = parent
                                  , asRoleNote = roleNote <$> mrole
                                  , asResume  = False
                                  , asManager = frManager fr }
             (_, key, line) <- claudeCommandLine (ClaudeAgent spec)
             place live spec key line

    -- Beside the parent when we can find its pane; its own tab otherwise.
    place live spec key line = do
      -- Write the lineage down NOW: the Agents pane's tree is built from it, and
      -- after this moment nothing on disk relates a child to its parent (nor
      -- says what the child was asked to do — its forked transcript starts with
      -- the PARENT's first prompt).
      recordAgentFork (asParent spec) (asSession spec) (asDir spec) (asPrompt spec)
      -- (already chosen by 'claudeCommandLine', which told the agent its name)
      ownName <- agentNameFor (asSession spec)
      mapM_ (setAgentRole (asSession spec) . roleName) mrole
      when (frManager fr) $ setAgentManager (asSession spec) (asDir spec)
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
              return (Right (asSession spec, described ownName spec live ("pane " <> newPane)))
        Nothing -> do
          name <- freshSessionName ownName
          newSessionWindow name (asDir spec) (Just key) line False >>= \case
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

-- | The live sessions, one tab-separated row each: session id, name, state, tmux
-- pane (@-@ when it isn't in one of leksah's), directory, title.  The caller's own
-- session is marked, so an agent reading this can tell itself apart from its
-- peers.  Ordered like the status surfaces: needs-you first, then working, then
-- idle.
agentList :: Maybe Text -> IO Text
agentList me = do
  live  <- claudeLiveBySession
  panes <- panesBySession
  if M.null live then return "No Claude Code sessions are running.\n" else do
    names  <- touchAgentNames (M.keys live)
    titles <- agentTitles
    rows <- forM (sortOn (\l -> (rank l, clDir l)) (M.elems live)) $ \l -> do
      -- The same title every other surface shows: what the agent called
      -- itself, else its /rename name, else its first prompt.
      title <- case M.lookup (clSession l) titles <|> clName l of
        Just n  -> return n
        Nothing -> fromMaybe "" <$> claudeSessionLabel (clDir l) (clSession l)
      return $ T.intercalate "\t"
        [ clSession l
        , M.findWithDefault "-" (clSession l) names
        , fromMaybe "idle" (clStatus l)
        , fromMaybe "-" (M.lookup (clSession l) panes)
        , T.pack (clDir l)
        , T.takeWhile (/= '\n') title
        , if me == Just (clSession l) then "(you)" else "" ]
    return . T.unlines $
      "# session\tname\tstate\tpane\tdir\ttitle" : rows
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
    Nothing -> agentInfoOf sid >>= \case
      Just i | isJust (aiSuspended i) ->
        return "suspended\tits process was ended on purpose; `agent resume` brings it back\n"
      _ -> return "gone\tthat session is not running\n"
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

-- | Bring an agent's pane to the front — whether or not its session has
-- registered yet.  A fork parked on a first-run question ('agentStatus'\'s
-- @starting@) has a pane but no session for 'showLiveSession' to find, and that
-- pane is exactly where someone needs to look.
showAgentPane :: Text -> IO Bool
showAgentPane sid = showLiveSession sid >>= \case
  True  -> return True
  False -> do
    panes <- liveRunPanes
    case [ (s, w, p) | (k, s, w, p) <- panes, ("#claude#" <> sid) `T.isSuffixOf` k ] of
      ((s, w, p) : _) -> showTmuxPane s w p >> return True
      [] -> return False

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
      paneForSession sid >>= mapM_ (\(ts, _, _) -> do
        cur <- fmap T.strip <$> tmuxOut
                 ["display-message", "-p", "-t", T.unpack ts, "#{session_name}"]
        when (maybe False (\c -> c == o || (o <> "-") `T.isPrefixOf` c) cur) $ do
          fresh <- freshSessionName nm
          tmuxCmd ["rename-session", "-t", T.unpack ts, T.unpack fresh])
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
  where
    stripTags t = case T.breakOn "<" t of
      (a, rest) | T.null rest -> a
                | otherwise   -> a <> " " <> stripTags (T.drop 1 (T.dropWhile (/= '>') rest))

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
  live <- claudeLiveBySession
  name <- agentNameFor sid
  case M.lookup sid live of
    Nothing -> agentInfoOf sid >>= \case
      Just i | isJust (aiSuspended i) -> return (name <> " is already suspended.\n")
      _ -> return (name <> " is not running, so there is nothing to suspend.\n")
    Just l
      | not force, clStatus l `elem` [Just "busy", Just "shell", Just "waiting"] ->
          return $ name <> " is " <> fromMaybe "busy" (clStatus l)
                <> "; suspending it now would lose what it is doing. Wait for it \
                   \to go idle, or pass --force.\n"
      | otherwise -> paneForSession sid >>= \case
          Nothing -> return $ name <> " is not in a pane leksah can reach, so it \
                                     \cannot be suspended from here.\n"
          Just (_, _, pane) -> do
            setAgentSuspended sid True
            _ <- sendKeysTo pane ["C-u"]
            _ <- sendKeysTo pane ["-l", "/exit"]
            _ <- sendKeysTo pane ["Enter"]
            _ <- forkIO $ do
              threadDelay 6000000
              still <- M.member sid <$> claudeLiveBySession
              when still $ tmuxCmd ["kill-pane", "-t", T.unpack pane]
            return $ "Suspended " <> name <> ". Resume it with: leksah-cmd agent \
                     \resume " <> T.toLower name <> "\n"

-- | @agent resume@: bring a suspended (or exited) agent back — its own
-- conversation (@claude --resume@), in a tab named after it, and with what it
-- was launched with: the note about the agent that started it (and permission
-- to report back to it), its role's instructions, and the manager's brief if it
-- is the manager.  A plain @claude --resume@ would lose all three.
agentResume :: Text -> IO (Either Text Text)
agentResume sid = do
  live <- claudeLiveBySession
  name <- agentNameFor sid
  if sid `M.member` live
    then do
      _ <- showAgentPane sid
      return (Right (name <> " is already running; showing it.\n"))
    else agentInfoOf sid >>= \case
      Nothing -> return . Left $
        "leksah does not know where " <> name <> " ran, so it cannot resume it \
        \(resume it from the workspace tree's Agents node instead)"
      Just i | null (aiDir i) -> return . Left $
        "leksah does not know where " <> name <> " ran, so it cannot resume it"
      Just i -> do
        mrole <- case aiRole i of
          Nothing -> return Nothing
          Just ro -> either (const Nothing) Just <$> findAgentRole (aiDir i) ro
        setAgentSuspended sid False
        runClaudeCmd . ClaudeAgent $ AgentSpec
          { asDir = aiDir i, asSession = sid, asFrom = Nothing, asPrompt = Nothing
          , asParent = aiParent i, asRoleNote = roleNote <$> mrole
          , asResume = True, asManager = aiManager i }
        return (Right ("Resuming " <> name <> " in its own tab.\n"))

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
-- Every event is logged, one line, to @~\/.leksah\/agent-hooks.log@.
agentHook :: Maybe Int -> Text -> Text -> IO Text
agentHook mpid ev raw = (`catch` \(_ :: SomeException) -> return "") $ do
  let v = decodeStrict' (TE.encodeUtf8 raw) :: Maybe Value
      field k = v >>= parseMaybe (withObject "hook" (.: k)) :: Maybe Text
      sid = field "session_id"
  proc <- maybe (return Nothing) claudeProcessOf mpid
  logHook ev proc [sid, field "source", field "reason", field "notification_type"]
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
      pokeClaudeStatus
      if source == "startup" then return "" else briefing new
    _ -> pokeClaudeStatus >> return ""
  where
    briefing sid = do
      name <- agentNameFor sid
      minfo <- agentInfoOf sid
      roleText <- case (minfo >>= aiRole, minfo) of
        (Just ro, Just i) -> either (const Nothing) (Just . roleNote) <$> findAgentRole (aiDir i) ro
        _                 -> return Nothing
      let notes = [ nameNote name | not (T.null name) ]
               <> [ agentNote p sid | Just p <- [minfo >>= aiParent] ]
               <> maybe [] (: []) roleText
               <> [ managerNote | maybe False aiManager minfo ]
      return $ if null notes then "" else TE.decodeUtf8 . BL.toStrict . encode $ object
        [ "hookSpecificOutput" .= object
            [ "hookEventName" .= ("SessionStart" :: Text)
            , "additionalContext" .= T.intercalate "\n\n" notes ] ]

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
