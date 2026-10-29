{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Claude Code integration: discover the CLI, enumerate a directory's saved
-- sessions, and launch @claude@ in a terminal.
--
-- Claude Code stores each conversation as a JSONL transcript under
-- @~\/.claude\/projects\/<encoded-cwd>\/<session-id>.jsonl@, where the encoding
-- replaces every non-alphanumeric character of the absolute working directory
-- with @-@ (so @\/Users\/me\/haskell.nix@ → @-Users-me-haskell-nix@).  The
-- encoding is lossy, so after locating the folder we re-read each transcript's
-- @cwd@ (cheap — it's on the first user line) to drop any collisions.  The
-- filename (a UUID) is the session id passed to @claude --resume@.
--
-- This module is FS/process only (no reflex); the tree widget lives in
-- "IDE.Web.Widget.FileTree".
module IDE.Web.Claude
  ( claudeAvailable
  , ClaudeSession(..)
  , csTitle
  , claudeSessionsFor
  , claudeSessionNames
  , ClaudeLive(..)
  , claudeLiveSessions
  , claudeLiveBySession
  , claudeLiveOwners
  , ClaudeCmd(..)
  , AgentSpec(..)
  , newAgentSessionId
  , runClaudeCmd
  , claudeCommandLine
  , claudeLatestActivity
  , claudeProcessOf
  , nameNote
  , agentNote
  , managerNote
  , claudeRunning
  , claudeKeyFor
  , mruClaudePane
  , claudeLatestPlan
  , claudeTranscriptPath
  , activateMruClaude
  , showLiveSession
  , showTmuxPane
  , paneForSession
  , sessionForPaneId
  , sessionOwningPid
  , sendToSession
  , paneCurrentPath
  , claudeSessionLabel
  , copySessionId
  , revealSession
  , deleteSession
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Exception (catch, SomeException)
import Control.Monad (void, filterM, forM, forM_, mfilter, unless, when)

import Data.Bits ((.&.), (.|.))
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Set as S
import Data.List (intercalate, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Data.Ord (Down(..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
       (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)

import Data.Aeson
       (Object, Value(..), decodeStrict', withObject, (.:), (.:?), object, (.=),
        encode)
import Data.Aeson.Types (Pair, parseMaybe, FromJSON)
import qualified Data.Aeson.Key as Key (fromText)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL (writeFile)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)
import Numeric (showHex)

import System.Directory
       (findExecutable, getHomeDirectory, doesDirectoryExist, doesFileExist,
        listDirectory, getModificationTime, removeFile)
import System.FilePath
       ((</>), takeExtension, takeBaseName, takeDirectory, takeFileName,
        dropTrailingPathSeparator, replaceExtension)
import System.Info (os)
import System.IO
       (withFile, IOMode(ReadMode), hIsEOF, hSeek,
        SeekMode(SeekFromEnd), hFileSize)

import IDE.Utils.RemotePath (isRemotePath)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (ExitCode(..))
import System.Process (createProcess, proc, readProcess, readProcessWithExitCode)
import Text.Read (readMaybe)

import IDE.App (appUi, getGlobalApp)
import IDE.Reactive (readCell)
import IDE.Web.Model (WebUi(_leksahWindows))
import IDE.Web.AgentNames
       (adoptContinuation, agentContinuations, agentNameFor, sessionOfRunKey)
import IDE.Web.ReplTmux
       (clipboardCopyCmd, cmdPrefixForDir, liveRunKeys,
        liveRunPanes, livePanePids, tmuxCmd, sendKeysTo, findRunPane,
        newSessionWindow, freshSessionName)
import IDE.Web.RemoteTermRequest (requestLocalTerm)
import IDE.Web.NewLwRequest (requestNewLw)
import IDE.Web.SplitLayout (windowOwner)

-- | A saved Claude Code session for some directory.
data ClaudeSession = ClaudeSession
  { csId       :: Text      -- ^ session id (the transcript's UUID filename)
  , csPath     :: FilePath  -- ^ absolute path to the @.jsonl@ transcript
  , csModified :: UTCTime   -- ^ transcript mtime (used for MRU ordering)
  , csAge      :: Text      -- ^ human-friendly age, e.g. @"3h ago"@
  , csLabel    :: Text      -- ^ first user prompt (best-effort), for the row
  , csName     :: Maybe Text -- ^ the name @/rename@ gave a LIVE session, if any
                             --   (see 'claudeSessionNames')
  }

-- | What to call a session in the UI: the name @/rename@ gave it if it has one,
-- else its first user prompt.
csTitle :: ClaudeSession -> Text
csTitle s = fromMaybe (csLabel s) (csName s)

-- Whether @claude@ is on PATH.  We cache only the *positive* result: once the
-- CLI is seen it can't disappear from under a running process in any way we
-- care about, so that's resolved once.  A negative result is NOT cached — the
-- @claude@ binary is often installed (or self-updates its symlink) after leksah
-- has already started, and re-checking on a miss lets the feature light up
-- without a restart.  @findExecutable@ on a miss is a cheap PATH scan and only
-- runs from tree builds / the 30s agentsNode poll.
{-# NOINLINE claudeAvailableRef #-}
claudeAvailableRef :: IORef Bool
claudeAvailableRef = unsafePerformIO (newIORef False)

claudeAvailable :: IO Bool
claudeAvailable = readIORef claudeAvailableRef >>= \case
  True  -> return True
  False -> do
    found <- maybe False (const True) <$> findExecutable "claude"
    when found $ writeIORef claudeAvailableRef True
    return found

-- | Encode an absolute directory the way Claude Code names its project folder:
-- every non-alphanumeric character becomes @-@.
encodeClaudeDir :: FilePath -> FilePath
encodeClaudeDir = map (\c -> if isAlphaNum c then c else '-') . dropTrailingPathSeparator

-- | A currently-RUNNING Claude Code session.
data ClaudeLive = ClaudeLive
  { clPid        :: Int         -- ^ pid of the @claude@ process
  , clSession    :: Text        -- ^ session id (matches a transcript's 'csId')
  , clDir        :: FilePath    -- ^ its working directory
  , clName       :: Maybe Text  -- ^ the name @/rename@ gave it, if any
  , clStatus     :: Maybe Text  -- ^ semantic state the CLI writes alongside the
                                --   pid: @busy@ (agent working) / @shell@ (a
                                --   shell command running) / @waiting@ (blocked
                                --   on an approval prompt) / @idle@ (ready for
                                --   input).  'Nothing' on an older CLI.
  , clWaitingFor :: Maybe Text  -- ^ what a @waiting@ session is blocked on
                                --   (e.g. the tool name), when the CLI says
  } deriving (Eq, Show)

-- | Every Claude Code session running right now.
--
-- Claude Code keeps one small JSON file per live session at
-- @~\/.claude\/sessions\/\<pid\>.json@ (@pid@, @sessionId@, @cwd@, @status@ and
-- — once renamed — @name@).  A @/rename@ is NOT written into the transcript, so
-- this is the only place the name can be read from; it also means a name is only
-- knowable while the session is running (the file goes when the process does,
-- and a later @--resume@ starts out unnamed again).  @[]@ on any error — no
-- folder, an older CLI that doesn't write these, unreadable JSON — so callers
-- just fall back to the transcript's first prompt.
claudeLiveSessions :: IO [ClaudeLive]
claudeLiveSessions = claudeSessionFiles >>= dropContinued

-- | A session that has carried on under another id (Claude Code backgrounding
-- it writes @{"type":"continued-in","continuedInSessionId":…}@ as the last
-- line of the old transcript) is the SAME agent: the new id takes its name
-- ('adoptContinuation'), and the old id is left out of the live sessions — its
-- process is only the host that started the new one, and still reports itself
-- busy.  Each session is checked once, the first time it is seen.
dropContinued :: [ClaudeLive] -> IO [ClaudeLive]
dropContinued ls = (`catch` \(_ :: SomeException) -> return ls) $ do
  checked <- readIORef continuationChecked
  let fresh = [ l | l <- ls, clSession l `S.notMember` checked ]
  unless (null fresh) $ do
    atomicModifyIORef' continuationChecked
      (\c -> (foldr (S.insert . clSession) c fresh, ()))
    forM_ fresh $ \l -> continuedFrom l >>=
      mapM_ (\old -> adoptContinuation old (clSession l))
  conts <- agentContinuations
  return [ l | l <- ls, clSession l `M.notMember` conts ]

{-# NOINLINE continuationChecked #-}
continuationChecked :: IORef (S.Set Text)
continuationChecked = unsafePerformIO (newIORef S.empty)

-- | The session @l@ carried on from, if one did: a recent transcript in its
-- project folder whose last line says it continued in @l@.
continuedFrom :: ClaudeLive -> IO (Maybe Text)
continuedFrom l = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  home <- getHomeDirectory
  now  <- getCurrentTime
  let pdir = home </> ".claude" </> "projects" </> encodeClaudeDir (clDir l)
      own  = T.unpack (clSession l) <> ".jsonl"
  ok <- doesDirectoryExist pdir
  if null (clDir l) || not ok then return Nothing else do
    fs <- filter (\f -> takeExtension f == ".jsonl" && f /= own) <$> listDirectory pdir
    recent <- filterM (\f -> (\t -> diffUTCTime now t < 2 * 86400)
                                <$> getModificationTime (pdir </> f)) fs
    hits <- forM recent $ \f -> do
      tl <- withFile (pdir </> f) ReadMode $ \h -> do
        size <- hFileSize h
        let back = min size 4096
        hSeek h SeekFromEnd (negate back)
        BS.hGet h (fromIntegral back)
      let says = [ () | ln <- BS.lines tl, "\"continued-in\"" `BS.isInfixOf` ln
                      , Just v <- [decodeStrict' ln]
                      , objField "continuedInSessionId" v == Just (clSession l) ]
      return [ T.pack (takeBaseName f) | not (null says) ]
    return (listToMaybe (concat hits))

-- | Every @~\/.claude\/sessions\/*.json@ as a 'ClaudeLive' (pid liveness and
-- continuations not considered yet).
claudeSessionFiles :: IO [ClaudeLive]
claudeSessionFiles = (`catch` \(_ :: SomeException) -> return []) $ do
  home <- getHomeDirectory
  let sdir = home </> ".claude" </> "sessions"
  ex <- doesDirectoryExist sdir
  if not ex then return [] else do
    names <- listDirectory sdir
    fmap concat . forM [ sdir </> n | n <- names, takeExtension n == ".json" ] $ \f ->
      (`catch` \(_ :: SomeException) -> return []) $ do
        v <- decodeStrict' <$> BS.readFile f
        return $ case (v >>= objField "pid", v >>= objField "sessionId") of
          (Just p, Just i) ->
            [ ClaudeLive
                { clPid        = p
                , clSession    = i
                , clDir        = maybe "" (dropTrailingPathSeparator . T.unpack)
                                       (v >>= objField "cwd")
                , clName       = mfilter (not . T.null) (v >>= objField "name")
                , clStatus     = mfilter (not . T.null) (v >>= objField "status")
                , clWaitingFor = mfilter (not . T.null) (v >>= objField "waitingFor") } ]
          _ -> []

-- | Live sessions keyed by session id, with dead leftovers dropped: a session
-- file can outlive its process (@kill -9@, a crash), so the pids are checked
-- against the process table in one @ps@ call.  This is the poll the tree
-- widgets use for names AND status badges, so a stale \"busy\" file mustn't
-- show as a working session forever.
claudeLiveBySession :: IO (Map Text ClaudeLive)
claudeLiveBySession = do
  ls <- claudeLiveSessions
  if null ls then return M.empty else do
    parents <- readParentPids
    let alive l = M.null parents || clPid l `M.member` parents
    return $ M.fromList [ (clSession l, l) | l <- ls, alive l ]

-- | The names @/rename@ has given to live sessions, keyed by session id — the
-- cheap half of 'claudeLiveSessions', for callers that already know which
-- session a row is about.
claudeSessionNames :: IO (Map Text Text)
claudeSessionNames = do
  ls <- claudeLiveSessions
  return $ M.fromList [ (clSession l, nm) | l <- ls, Just nm <- [clName l] ]

-- | Live sessions keyed by every pid they run *under* — the @claude@ process and
-- each of its ancestors.  Lets a caller holding a tmux @#{pane_pid}@ identify the
-- exact session running in that pane, however deeply it is wrapped (leksah
-- launches claude through the project's command prefix, e.g.
-- @zsh -c \'nix develop -c claude …\'@, so it is rarely a direct child).
--
-- Exact identification matters because two sessions in the SAME directory can be
-- open in two windows: keyed by directory they'd both show whichever session was
-- used most recently, so a @/rename@ would relabel the wrong tab.
--
-- One @ps@ call, and none at all when nothing is running.  'M.empty' if @ps@
-- fails (callers fall back to the per-directory guess).
claudeLiveOwners :: IO (Map Int ClaudeLive)
claudeLiveOwners = (`catch` \(_ :: SomeException) -> return M.empty) $ do
  ls <- claudeLiveSessions
  if null ls then return M.empty else do
    parents <- readParentPids
    return $ M.fromList
      [ (p, l) | l <- ls, p <- clPid l : ancestors parents (clPid l) ]
  where
    -- Up to the session leader; bounded so a cycle in a garbled table can't spin.
    ancestors parents = go (12 :: Int)
      where go 0 _ = []
            go n p = case M.lookup p parents of
              Just q | q > 1 -> q : go (n - 1) q
              _              -> []

-- | @pid -> ppid@ for every process on the machine.
readParentPids :: IO (Map Int Int)
readParentPids = (`catch` \(_ :: SomeException) -> return M.empty) $ do
  out <- readProcess "ps" ["-Ao", "pid=,ppid="] ""
  return $ M.fromList
    [ (p, q) | l <- lines out
             , (pT : qT : _) <- [words l]
             , Just p <- [readMaybe pT], Just q <- [readMaybe qT] ]

-- | The saved sessions whose working directory is @dir@, most-recently-used
-- first.  @[]@ on any error (no folder, unreadable, …) so the node just hides.
claudeSessionsFor :: FilePath -> IO [ClaudeSession]
claudeSessionsFor dir = (`catch` \(_ :: SomeException) -> return []) $ do
  home <- getHomeDirectory
  let target = dropTrailingPathSeparator dir
      pdir   = home </> ".claude" </> "projects" </> encodeClaudeDir target
  ex <- doesDirectoryExist pdir
  if not ex then return [] else do
    names <- listDirectory pdir
    now   <- getCurrentTime
    -- One read of the live-session names for the whole scan (a handful of tiny
    -- files) so each session can show its /rename name instead of its prompt.
    renamed <- claudeSessionNames
    let transcripts = [ pdir </> n | n <- names, takeExtension n == ".jsonl" ]
    fmap (sortOn (Down . csModified) . mapMaybe id) . forM transcripts $ \f ->
      (`catch` \(_ :: SomeException) -> return Nothing) $ do
        mt <- getModificationTime f
        (mcwd, mlabel) <- scanHead f
        -- The encoded folder is lossy; keep only transcripts that actually ran
        -- in this directory (or whose cwd we couldn't read).
        let matches = maybe True ((== target) . dropTrailingPathSeparator . T.unpack) mcwd
        let sid = T.pack (takeBaseName f)
        return $ if not matches then Nothing else Just ClaudeSession
          { csId       = sid
          , csPath     = f
          , csModified = mt
          , csAge      = humanAge (diffUTCTime now mt)
          , csLabel    = maybe "(untitled session)" id mlabel
          , csName     = M.lookup sid renamed
          }

-- | Read a transcript's head for @(cwd, first-user-prompt)@, stopping as soon as
-- both are found (or after a small line budget) so we never read a huge file.
scanHead :: FilePath -> IO (Maybe Text, Maybe Text)
scanHead f = withFile f ReadMode $ \h -> go h (120 :: Int) Nothing Nothing
  where
    go _ 0 cwd lbl = return (cwd, lbl)
    go h n cwd lbl
      | Just _ <- cwd, Just _ <- lbl = return (cwd, lbl)
      | otherwise = hIsEOF h >>= \case
          True  -> return (cwd, lbl)
          False -> do
            l <- BS.hGetLine h
            let v    = decodeStrict' l :: Maybe Value
                cwd' = cwd `orElse` (v >>= objField "cwd")
                lbl' = lbl `orElse` (v >>= userPrompt)
            go h (n - 1) cwd' lbl'
    orElse a b = maybe b Just a

-- | The user prompt text of a @type:"user"@ entry (message content is a bare
-- string or a list of blocks); 'Nothing' for any other entry.
userPrompt :: Value -> Maybe Text
userPrompt v = do
  ty <- objField "type" v :: Maybe Text
  if ty /= "user" then Nothing else do
    msg     <- objField "message" v :: Maybe Value
    content <- objField "content" msg :: Maybe Value
    firstText content >>= cleanLabel

firstText :: Value -> Maybe Text
firstText (String s) = Just s
firstText (Array a)  = listToMaybe (mapMaybe blockText (toList a))
  where blockText b = case objField "type" b :: Maybe Text of
          Just "text" -> objField "text" b
          _           -> Nothing
firstText _ = Nothing

-- | First meaningful line of a prompt: skip blank / tag-ish lines (slash-command
-- wrappers, @<system-reminder>@ …), then truncate for the tree row.
cleanLabel :: Text -> Maybe Text
cleanLabel raw =
  case filter good (map T.strip (T.lines raw)) of
    (l : _) -> Just (ellipsize 72 l)
    []      -> case T.strip <$> listToMaybe (T.lines raw) of
                 Just l | not (T.null l) -> Just (ellipsize 72 l)
                 _                       -> Nothing
  where
    good l = not (T.null l) && not ("<" `T.isPrefixOf` l)
    ellipsize k t | T.length t > k = T.take (k - 1) t <> "…"
                  | otherwise      = t

-- | Look up @key@ in a JSON object and decode it, version-agnostically across
-- aeson 1/2 (the string literal becomes @Text@ or @Key@ as needed).
objField :: FromJSON a => String -> Value -> Maybe a
objField k = parseMaybe (withObject "o" (\o -> o .: fromString k))

humanAge :: NominalDiffTime -> Text
humanAge d
  | s < 60        = "just now"
  | s < 3600      = tshow (s `div` 60)          <> "m ago"
  | s < 86400     = tshow (s `div` 3600)        <> "h ago"
  | s < 7 * 86400 = tshow (s `div` 86400)       <> "d ago"
  | otherwise     = tshow (s `div` (7 * 86400)) <> "w ago"
  where s = max 0 (round d) :: Int
        tshow = T.pack . show

-- | A @claude@ invocation to run in a terminal for some directory.
data ClaudeCmd
  = ClaudeNew          FilePath        -- ^ @claude@ — a fresh session
  | ClaudeContinue     FilePath        -- ^ @claude -c@ — continue the most recent
  | ClaudeResumePicker FilePath        -- ^ @claude --resume@ — interactive picker
  | ClaudeResume       FilePath Text   -- ^ @claude --resume <id>@
  | ClaudeResumeFork   FilePath Text   -- ^ @claude --resume <id> --fork-session@
  | ClaudeAsk          FilePath        -- ^ @claude "<prompt>"@ — new session
                                       --   seeded to explain the given file
  | ClaudePrompt       FilePath Text   -- ^ @claude "<prompt>"@ — new session in
                                       --   the dir seeded with a free-form task
                                       --   prompt (the Stage-4 task queue)
  | ClaudeAgent        AgentSpec       -- ^ one agent forking another — see
                                       --   "IDE.Web.Agent"

-- | A child agent an agent asked for: what to run, and who asked.
--
-- The child's session id is minted by leksah BEFORE launch and pinned with
-- @--session-id@, so @leksah-cmd agent fork@ can answer with a handle the
-- parent can immediately send to, wait on and read — no polling for the id to
-- appear.  (Verified: @--session-id@ composes with @--resume … --fork-session@,
-- which would otherwise mint its own.)
data AgentSpec = AgentSpec
  { asDir     :: FilePath   -- ^ the child's working directory
  , asSession :: Text       -- ^ its pre-minted session id ('newAgentSessionId')
  , asFrom    :: Maybe Text -- ^ session to fork: the child starts with that
                            --   conversation's full context ('Nothing' = a
                            --   fresh agent, which knows only its prompt)
  , asPrompt  :: Maybe Text -- ^ seeded first prompt (submitted at launch)
  , asParent  :: Maybe Text -- ^ the session that asked, if one did — told to
                            --   the child so it can report back
  , asRoleNote :: Maybe Text -- ^ the instructions of the role it was started in
                            --   ("IDE.Web.AgentRoles"), for its system prompt
  , asResume  :: Bool       -- ^ continue session 'asSession' itself
                            --   (@--resume@ it) rather than start a new one —
                            --   how a suspended or exited agent comes back with
                            --   its notes and permissions intact
  , asManager :: Bool       -- ^ it is the workspace's manager: 'managerNote',
                            --   and the wider allow-list that job needs
  } deriving (Eq, Show)

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

-- | Open (or focus) a terminal in the right directory running the command.
-- An existing pane for the same conversation (its @\@leksah_run@ key) is
-- focused instead of duplicated — one claude process per conversation —
-- otherwise a NEW tmux session per open, wrapped in a new leksah window via
-- the requestNewLw seam.  Fire-and-forget.
--
-- The pane closes itself when @claude@ exits cleanly (status 0, e.g. @\/exit@
-- or Ctrl-D) and only lingers as a shell on a non-zero exit — so a crash stays
-- on screen but a normal quit tidies up (which also clears the tree's
-- "running" dot and frees the per-directory key, see 'claudeRunning').
runClaudeCmd :: ClaudeCmd -> IO ()
runClaudeCmd cmd = void . forkIO $ do
  (d, key, line') <- claudeCommandLine cmd
  findRunPane key >>= \case
    Just (sid, wid, pid) -> do
      tmuxCmd ["select-window", "-t", T.unpack wid]
      tmuxCmd ["select-pane", "-t", T.unpack pid]
      requestLocalTerm sid
    Nothing -> do
      -- The tmux session (so the tab, and the spoken bell) is called after the
      -- agent ("Wren") whenever the launch pins its session id; a launch that
      -- can't know its id yet (@-c@, the picker) is renamed by the Terminals
      -- poll once its session shows up ('IDE.Web.Widget.Terminal').
      base <- maybe (return "claude") agentNameFor (sessionOfRunKey key)
      name <- freshSessionName base
      newSessionWindow name d (Just key) line' False
        >>= mapM_ (\(sid, wid, _) -> requestNewLw (sid, wid))

-- | Resolve a 'ClaudeCmd' to its @(working dir, run key, shell command line)@,
-- with the owning project's command prefix (e.g. @nix develop -c@) applied so
-- the tools claude spawns inherit the project environment.  Shared by
-- 'runClaudeCmd' and the ⌥-open-into-split pipeline (which runs the line in a
-- split pane rather than a new window).
claudeCommandLine :: ClaudeCmd -> IO (FilePath, Text, Text)
claudeCommandLine (ClaudeNew d) = do
  -- A NEW session must get a new pane.  With no id of its own it would share
  -- the directory key (@dir#claude@) with Continue/Resume…, and 'runClaudeCmd'
  -- would just focus whichever of those is already open.  So pin a fresh
  -- @--session-id@: the run key becomes @dir#claude#\<id\>@ — unique, and the
  -- plain resume shape, so the pane's session is known from its key.
  sid <- newAgentSessionId
  claudeCommandLine . ClaudeAgent $ AgentSpec
    { asDir = d, asSession = sid, asFrom = Nothing, asPrompt = Nothing
    , asParent = Nothing, asRoleNote = Nothing, asResume = False
    , asManager = False }
claudeCommandLine (ClaudePrompt d p) = do
  -- Pinned for the same reason, and so a queued task's agent has a name (and
  -- an Agents-pane identity) from the moment it starts.  The run key keeps the
  -- @dir#claude@ prefix the worktree flows match on ('claudeKeyFor').
  sid <- newAgentSessionId
  claudeCommandLine . ClaudeAgent $ AgentSpec
    { asDir = d, asSession = sid, asFrom = Nothing, asPrompt = Just p
    , asParent = Nothing, asRoleNote = Nothing, asResume = False
    , asManager = False }
claudeCommandLine cmd = do
  let d = dropTrailingPathSeparator dir
  mbPrefix <- mfilter (not . T.null) <$> cmdPrefixForDir d
  -- Local sessions get leksah's MCP server (IDE tools: diagnostics, open_file,
  -- build, hover, screenshot…) — see `leksah-cmd mcp`.  The config file points
  -- at a local binary, so remote (ssh://) dirs skip it.
  mcpFlag   <- if isRemotePath d then return "" else claudeMcpFlag
  hooksFlag <- if isRemotePath d then return "" else claudeHooksFlag
  -- The agent's own name, when its session id is known at launch — so it can
  -- tell the user and other agents who it is.
  myName <- traverse agentNameFor ownSession
  let flags = mcpFlag <> hooksFlag <> allowFlag
           <> appendSystemPrompt (planHtmlNote : worktreeNote
                                   : maybe [] ((: []) . nameNote) myName <> notes)
      line' = maybe line (\p -> p <> " " <> line) mbPrefix <> flags
  return (d, key, line')
  where
    ownSession = case cmd of
      ClaudeAgent s    -> Just (asSession s)
      ClaudeResume _ i -> Just i
      _                -> Nothing
    -- Extra system-prompt notes this command needs, joined into the ONE
    -- @--append-system-prompt@ 'appendSystemPrompt' emits.
    notes = case cmd of
      ClaudeAgent s -> [ agentNote p (asSession s) | Just p <- [asParent s] ]
                       <> maybe [] (: []) (asRoleNote s)
                       <> [ managerNote | asManager s ]
      _             -> []
    -- Two things a leksah-launched session may do without stopping to ask,
    -- because in both cases the thing asking is leksah itself:
    --
    --   * @agent describe@ — answer the Agents pane's ⟳ button (it writes a
    --     title and a description into a sidecar, and nothing else).  Granted to
    --     every session, since any of them can be asked.
    --   * @agent done@ — saying it has finished (it marks the agent done in
    --     the Agents pane and, for a child, hands the summary to its parent).
    --   * @agent send@ — a child agent reporting back to the parent that forked
    --     it.  Without it that loop stalls on a prompt the parent can't answer.
    --   * @agent register@ — answering 'worktreeNote' (it records a
    --     worktree relationship in a sidecar, and nothing else).
    --
    -- And nothing else: NOT `agent fork`, so children can't quietly fan out.
    -- Note the placement: @--allowedTools@ is variadic, so it must be followed
    -- by another flag — never by the positional prompt, which it would swallow
    -- ('flags' always ends with --append-system-prompt).
    allowFlag = " --allowedTools " <> T.unwords (map shq allowed)
      where
        allowed = "Bash(leksah-cmd agent describe:*)" :
          "Bash(leksah-cmd agent register:*)" :
          "Bash(leksah-cmd agent done:*)" :
          case cmd of
            -- The manager runs the team: everything `agent` does except
            -- forgetting agents (there is no such verb) — still only leksah's
            -- own commands, so nothing outside the IDE is pre-approved.
            ClaudeAgent s | asManager s ->
              [ "Bash(leksah-cmd agent " <> v <> ":*)"
              | v <- [ "fork", "send", "suspend", "resume", "refresh", "find"
                     , "list", "read", "status", "wait", "roles", "show"
                     , "rename", "manager" ] ]
            ClaudeAgent s | Just _ <- asParent s -> ["Bash(leksah-cmd agent send:*)"]
            _                                    -> []
    (dir, keyTag, line) = case cmd of
      -- (not reached: the equation above turns ClaudeNew into ClaudeAgent)
      ClaudeNew d          -> (d, "claude",            "claude")
      ClaudeContinue d     -> (d, "claude",            "claude -c")
      ClaudeResumePicker d -> (d, "claude",            "claude --resume")
      ClaudeResume d i     -> (d, "claude#" <> i,      "claude --resume " <> i)
      ClaudeResumeFork d i -> (d, "claude#fork#" <> i, "claude --resume " <> i <> " --fork-session")
      ClaudeAsk f          -> let b = T.pack (takeFileName f)
                              in ( takeDirectory f
                                 , "claude#ask#" <> b
                                 , "claude " <> shq ("Please explain the file @" <> b
                                     <> " — what it does and how it fits into this project.") )
      -- The queue's worktree dir is unique per task, so the shared "claude"
      -- key keeps it addressable by the worktree flows (Review's send etc.).
      -- (not reached either: ClaudePrompt pins an id, like ClaudeNew)
      ClaudePrompt d p     -> (d, "claude", "claude " <> shq p)
      -- A child agent: its own pinned session id makes the run key unique, so
      -- every fork gets a NEW pane (findRunPane can still bring it back).
      ClaudeAgent s        ->
        ( asDir s
        , "claude#" <> asSession s
        , T.unwords $
            (if asResume s
               then [ "claude", "--resume", asSession s ]
               else [ "claude", "--session-id", asSession s ]
                    <> maybe [] (\f -> ["--resume", f, "--fork-session"]) (asFrom s))
            <> maybe [] ((:[]) . shq) (asPrompt s) )
    key = T.pack (dropTrailingPathSeparator dir) <> "#" <> keyTag

-- | Single-quote for the shell tmux runs the window command through, so a
-- multi-word argument (a seeded prompt, the system-prompt suffix) reaches
-- @claude@ as one argument.
shq :: Text -> Text
shq t = "'" <> T.replace "'" "'\\''" t <> "'"

-- | ONE @--append-system-prompt@ carrying every note a launch wants to add
-- (blank-line separated).  One flag rather than several because the CLI keeps
-- only the last occurrence — two flags silently lose the first note.
appendSystemPrompt :: [Text] -> Text
appendSystemPrompt notes
  | null notes' = ""
  | otherwise   = " --append-system-prompt " <> shq (T.intercalate "\n\n" notes')
  where notes' = filter (not . T.null) notes

-- | Tell a child agent who forked it and how to report back, so the parent can
-- dispatch work and be woken by the answer instead of blocking on it (see
-- "IDE.Web.Agent").  @--submit@ presses Enter, so the report becomes a real
-- turn in the parent's conversation even if it had gone idle.
agentNote :: Text -> Text -> Text
agentNote parent me = T.unlines
  [ "You are a Claude Code agent in a Leksah IDE terminal pane, started by \
    \another agent (Claude Code session " <> parent <> ") that is working \
    \alongside you."
  , ""
  , "When you finish, say so — that marks you done in the IDE's Agents pane \
    \and hands your summary to the agent that started you:"
  , ""
  , "    leksah-cmd agent done --summary '<your report>'"
  , ""
  , "If you get stuck, or need a decision only it can make, ask it directly:"
  , ""
  , "    leksah-cmd agent send " <> parent <> " --submit '<your question>'"
  , ""
  , "Keep reports self-contained (what you did, what you found, which files \
    \changed): that session cannot see your screen. Your own session id is "
    <> me <> ", which it can use to read your transcript or send you more \
    \work. Do not fork further agents unless you are asked to."
  ]

-- | What the manager agent is for, and how it goes about it.
managerNote :: Text
managerNote = T.unlines
  [ "You are the MANAGER of the Claude Code agents running in this Leksah IDE. \
    \Your job is to keep the team effective — not to do the coding yourself. \
    \The user talks to you; you get the work to the right agent."
  , ""
  , "How you work (all through `leksah-cmd agent …`, or the matching MCP tools):"
  , "- When the user gives you a task, run `agent find 'topic'` first. If an \
    \agent already has the context, hand it over with `agent send NAME --submit \
    \'…'` (self-contained: it cannot see your conversation). Start a new one only \
    \when nobody fits — `agent fork --fresh --tab 'task'`, or `agent fork --as \
    \ROLE --tab 'task'` when a role (`agent roles`) matches. Tell the user who \
    \took it."
  , "- Keep an eye on the team with `agent list`: tell the user about agents \
    \blocked on an approval prompt (state waiting) — they need a human."
  , "- Agents report back to you with `NAME is done: …` messages. Summarise \
    \those for the user, and follow up if more work is needed."
  , "- Keep the team's descriptions current: every so often, and whenever the \
    \user asks about the team, `agent refresh NAME` the agents whose title, \
    \description or bio is missing or stale (a couple of hours old). An agent \
    \answers a refresh by describing itself; you do not need to write it."
  , "- Suspend agents that are idle with nothing left to do when the user agrees \
    \or asks (`agent suspend NAME` — it ends the process but keeps the agent and \
    \its conversation), and `agent resume NAME` them when their work comes \
    \back. Never suspend a busy agent without asking."
  , "- Use `agent read NAME` to catch up on what an agent said, and `agent wait \
    \NAME` only when you genuinely need the answer before going on."
  , ""
  , "Keep your own replies short: who is doing what, what needs the user."
  ]

-- | Tell an agent the name leksah gave it ("IDE.Web.AgentNames"), which is
-- what its tab, the status surfaces and the Agents pane call it.
nameNote :: Text -> Text
nameNote name =
  "In the Leksah IDE you are called " <> name <> ". That is the name on your \
  \tab and in Leksah's Agents pane, and it is how the user and other agents \
  \refer to you: `leksah-cmd agent send " <> T.toLower name <> " ...` reaches \
  \you, and the other agents' names work the same way (`leksah-cmd agent \
  \list` shows them; `leksah-cmd agent find 'topic'` finds the one already \
  \working on something). Start any report you send to another agent with \
  \your name."

-- | Ask every leksah-launched session to keep an HTML rendering of its plan
-- next to the markdown one — leksah's plan-review pane shows the HTML version
-- as a full document when it is current (see 'claudeLatestPlan').  Appended
-- to the CLI's system prompt at launch, like the MCP registration.
planHtmlNote :: Text
planHtmlNote =
  "When you write or update a plan file in plan mode, also save an HTML \
  \rendering of the same plan next to it: same directory and base name with \
  \a .html extension. Make it a complete self-contained document (starts \
  \with <!doctype html>, inline CSS only, no external resources, readable \
  \typography, dark and light supported via prefers-color-scheme). The \
  \markdown file stays the authoritative plan; regenerate the .html whenever \
  \the plan changes."

-- | Ask every leksah-launched session to keep the IDE's worktree map accurate:
-- register when it creates, adopts, reviews or finishes with a git worktree,
-- and re-register when it re-points one at another branch.  The command is on
-- the launch allow-list ('allowFlag'), so following this note never stops the
-- session on a permission prompt.
worktreeNote :: Text
worktreeNote =
  "When you create a git worktree, start working in one, review one, finish \
  \with one, or switch the branch a worktree is on, immediately register that \
  \with the IDE by calling the register_worktree MCP tool (or running \
  \`leksah-cmd agent register --worktree PATH --role \
  \created|working|reviewing|abandoned --note '<one line: your relationship \
  \with it>'` — pre-approved, it never prompts; pass --branch after switching \
  \branches). This keeps the IDE's map of worktrees, branches and sessions \
  \accurate for the user."

-- | The @--mcp-config@ flag wiring a leksah-launched session to leksah's MCP
-- server (@leksah-cmd mcp@ — IDE tools: diagnostics, open_file, build, hover,
-- screenshot), (re)writing @~\/.leksah\/mcp.json@ so the recorded binary path
-- stays fresh across rebuilds.  Empty when @leksah-cmd@ isn't on PATH (the
-- session just runs without IDE tools).
claudeMcpFlag :: IO Text
claudeMcpFlag = (`catch` \(_ :: SomeException) -> return "") $
  findExecutable "leksah-cmd" >>= \case
    Nothing  -> return ""
    Just exe -> do
      home <- getHomeDirectory
      let path = home </> ".leksah" </> "mcp.json"
      BL.writeFile path . encode $ object
        [ "mcpServers" .= object
            [ "leksah" .= object
                [ "type"    .= ("stdio" :: Text)
                , "command" .= exe
                , "args"    .= (["mcp"] :: [Text]) ] ] ]
      return (" --mcp-config " <> T.pack path)

-- | The @--settings@ flag wiring a leksah-launched session to the worktree
-- PostToolUse hook: every @git worktree add@ \/ @git switch@ a session runs in
-- Bash lands in the worktree registry, no cooperation needed.  The hook script
-- itself is written by @leksah-cmd hooks write-script@ (the script text lives
-- with the CLI, which also installs it into user\/project settings on demand —
-- @leksah-cmd hooks install@); this just refreshes it and points a small
-- settings JSON at it.  Empty when @leksah-cmd@ isn't on PATH.  @--settings@
-- loads ADDITIONAL settings, so the user's own hooks keep working.
claudeHooksFlag :: IO Text
claudeHooksFlag = (`catch` \(_ :: SomeException) -> return "") $
  findExecutable "leksah-cmd" >>= \case
    Nothing  -> return ""
    Just exe -> do
      (ec, _, _) <- readProcessWithExitCode exe ["hooks", "write-script"] ""
      case ec of
        ExitSuccess -> do
          home <- getHomeDirectory
          let path   = home </> ".leksah" </> "hooks.json"
              script = home </> ".leksah" </> "worktree-hook.sh"
          -- The session-lifecycle hooks report to leksah ('IDE.Web.Agent.agentHook'):
          -- SessionStart keeps an agent's identity across /clear and /branch
          -- and re-briefs it on a resume; Stop, Notification and SessionEnd
          -- make the status surfaces update at once rather than at the
          -- next poll.  Each is `leksah-cmd agent hook EVENT`, which always
          -- exits 0 quickly, so a hook can never hold up or fail a session.
          let lifecycle :: Text -> Pair
              lifecycle ev = Key.fromText ev .=
                [ object [ "hooks" .=
                    [ object [ "type"    .= ("command" :: Text)
                             , "command" .= (shq (T.pack exe) <> " agent hook " <> ev)
                             , "timeout" .= (5 :: Int) ] ] ] ]
          BL.writeFile path . encode $ object
            [ "hooks" .= object
                ([ "PostToolUse" .=
                    [ object
                        [ "matcher" .= ("Bash" :: Text)
                        , "hooks" .=
                            [ object [ "type"    .= ("command" :: Text)
                                     , "command" .= script
                                     , "timeout" .= (10 :: Int) ] ] ] ] ]
                 <> map lifecycle ["SessionStart", "Stop", "Notification", "SessionEnd"]) ]
          return (" --settings " <> T.pack path)
        _ -> return ""

-- | Is a @claude@ terminal window currently live for @dir@ — the shared
-- interactive window (@dir#claude@) or any resumed/ask window (@dir#claude#…@)?
-- Used to show a "running" marker on the tree node.
claudeRunning :: FilePath -> IO Bool
claudeRunning dir = do
  let base = T.pack (dropTrailingPathSeparator dir) <> "#claude"
  any (claudeKeyFor base) <$> liveRunKeys

-- | Does run key @k@ belong to @dir@'s claude terminals (@base@ =
-- @\<dir\>#claude@)?  Matches the shared interactive window and any
-- resumed\/fork\/ask window.
claudeKeyFor :: Text -> Text -> Bool
claudeKeyFor base k = k == base || (base <> "#") `T.isPrefixOf` k

-- | The most-recently-used LIVE claude pane id (@%N@) for @dir@ — where
-- Approve\/Revise keystrokes and review comments go.  MRU is tmux
-- @window_activity@, via 'liveRunPanes'.
mruClaudePane :: FilePath -> IO (Maybe Text)
mruClaudePane dir = do
  let base = T.pack (dropTrailingPathSeparator dir) <> "#claude"
  panes <- filter (\(k, _, _, _) -> claudeKeyFor base k) <$> liveRunPanes
  return $ case panes of
    ((_, _, _, pid) : _) -> Just pid
    []                   -> Nothing

-- | The transcript path for session @sid@ running in @dir@ (the encoded
-- projects folder + @\<session id\>.jsonl@).  Purely path arithmetic — the
-- file may not exist.
claudeTranscriptPath :: FilePath -> Text -> IO FilePath
claudeTranscriptPath dir sid = do
  home <- getHomeDirectory
  return $ home </> ".claude" </> "projects"
                </> encodeClaudeDir (dropTrailingPathSeparator dir)
                </> (T.unpack sid <> ".jsonl")

-- | The plan of the LAST plan-mode round in a transcript: scan the tail (the
-- final 512KB — a plan is always recent when it matters) for either the last
-- assistant line carrying an @ExitPlanMode@ tool use (@input.plan@ holds the
-- markdown — older CLIs), or, failing that, the last mention of a
-- @~\/.claude\/plans\/…\.md@ plan FILE (current CLIs write the plan there
-- before asking for approval, so the tool_use may not be flushed yet while
-- the session waits) — whose contents are the plan.  'Nothing' when the
-- session never planned (or nothing is readable).
claudeLatestPlan :: FilePath -> IO (Maybe Text)
claudeLatestPlan path = (`catch` \(_ :: SomeException) -> return Nothing) $ do
    tailLines <- withFile path ReadMode $ \h -> do
      size <- hFileSize h
      let back = min size (512 * 1024)
      hSeek h SeekFromEnd (negate back)
      BS.lines <$> BS.hGet h (fromIntegral back)
    let toolUse = [ l | l <- tailLines
                  , "\"name\":\"ExitPlanMode\"" `BS.isInfixOf` l ]
    case reverse toolUse of
      (l : _) | Just p <- decodeStrict' l >>= parseMaybe lineP -> return (Just p)
      _ -> case reverse (mapMaybe planFileIn tailLines) of
        (f : _) -> do
          f' <- preferHtml f
          (Just <$> readUtf8 f')
            `catch` \(_ :: SomeException) -> return Nothing
        []      -> return Nothing
  where
    readUtf8 f = decodeUtf8With lenientDecode <$> BS.readFile f
    -- Sessions leksah launches keep an HTML rendering next to the markdown
    -- plan (see 'planHtmlNote'); show that when it is at least as fresh as
    -- the .md (a stale sibling from an earlier plan round loses).
    preferHtml f
      | takeExtension f == ".html" = return f
      | otherwise = do
          let h = replaceExtension f "html"
          ex <- doesFileExist h
          if not ex then return f else do
            th <- getModificationTime h
            tf <- getModificationTime f
            return (if th >= tf then h else f)
    -- The absolute ~/.claude/plans/….md path embedded in a JSON line (a Write
    -- tool result / file-history record): expand back to the enclosing quotes.
    planFileIn l = do
      let (pre, rest) = BS.breakSubstring ".claude/plans/" l
      if BS.null rest then Nothing else do
        let start = BS.takeWhileEnd (/= '"') pre
            end   = BS.takeWhile (/= '"') rest
            p     = start <> end
        if ".md" `BS.isSuffixOf` p || ".html" `BS.isSuffixOf` p
          then Just (BS.unpack p) else Nothing
    lineP = withObject "line" $ \o -> do
      m  <- o .: "message"
      cs <- m .: "content"
      let oneP = withObject "content" $ \c -> do
            n <- c .: "name"
            if n == ("ExitPlanMode" :: Text)
              then (c .: "input") >>= withObject "input" (.: "plan")
              else fail "not a plan"
      case mapMaybe (parseMaybe oneP) (cs :: [Value]) of
        (p : _) -> return p
        []      -> fail "no plan in line"

-- | What the agent is doing right now, in a few words, from the last assistant
-- message in its transcript: the tool it last called (@Bash: cabal build@,
-- @Edit Main.hs@, @Agent: review the diff@…), or @Thinking@ \/ @Writing@ when
-- that message is thought or prose.  For the Agents pane's activity line,
-- which shows it only while the agent is busy.  Reads the last 64KB only.
claudeLatestActivity :: FilePath -> IO (Maybe Text)
claudeLatestActivity path = (`catch` \(_ :: SomeException) -> return Nothing) $ do
    tailLines <- withFile path ReadMode $ \h -> do
      size <- hFileSize h
      let back = min size (64 * 1024)
      hSeek h SeekFromEnd (negate back)
      BS.lines <$> BS.hGet h (fromIntegral back)
    return . listToMaybe . mapMaybe (\l -> decodeStrict' l >>= parseMaybe lineP)
      . reverse $ filter ("\"type\":\"assistant\"" `BS.isInfixOf`) tailLines
  where
    lineP = withObject "line" $ \o -> do
      t <- o .: "type"
      if t /= ("assistant" :: Text) then fail "not assistant" else do
        m  <- o .: "message"
        cs <- m .: "content"
        case reverse (cs :: [Value]) of
          (c : _) -> withObject "content" blockP c
          []      -> fail "empty"
    blockP c = c .: "type" >>= \case
      ("tool_use" :: Text) -> do
        name <- c .: "name"
        inp  <- c .:? "input" >>= maybe (return mempty) return
        return (describeTool name inp)
      "thinking" -> return "Thinking"
      _          -> return "Writing"
    describeTool :: Text -> Object -> Text
    describeTool name inp =
      let str k = case parseMaybe (.: k) inp of
                    Just v | not (T.null (T.strip v)) -> Just (oneLine v)
                    _                                 -> Nothing
          file  = T.pack . takeFileName . T.unpack
          short = T.takeWhileEnd (/= '_') name
      in case name of
        "Bash"         -> maybe "Bash" ("Bash: " <>) (str "description" <|> str "command")
        "Read"         -> maybe "Read" (("Read " <>) . file) (str "file_path")
        "Edit"         -> maybe "Edit" (("Edit " <>) . file) (str "file_path")
        "MultiEdit"    -> maybe "Edit" (("Edit " <>) . file) (str "file_path")
        "Write"        -> maybe "Write" (("Write " <>) . file) (str "file_path")
        "NotebookEdit" -> maybe "Edit" (("Edit " <>) . file) (str "notebook_path")
        "Grep"         -> maybe "Grep" ("Grep " <>) (str "pattern")
        "Glob"         -> maybe "Glob" ("Glob " <>) (str "pattern")
        "WebFetch"     -> maybe "WebFetch" ("Fetch " <>) (str "url")
        "WebSearch"    -> maybe "WebSearch" ("Search: " <>) (str "query")
        "Agent"        -> maybe "Agent" ("Agent: " <>) (str "description")
        "Task"         -> maybe "Task" ("Task: " <>) (str "description")
        _ | "mcp__" `T.isPrefixOf` name -> short
          | otherwise                   -> name
    oneLine = T.take 80 . T.strip . T.takeWhile (/= '\n')

-- | Activate the most-recently-used LIVE claude terminal for @dir@ (select its
-- tmux window\/pane wherever it now lives and open that session's terminal
-- tab); 'False' when no claude terminal is open here.  MRU is tmux
-- @window_activity@ — the pane that last produced output\/was used — via
-- 'liveRunPanes'.
activateMruClaude :: FilePath -> IO Bool
activateMruClaude dir = do
  let base = T.pack (dropTrailingPathSeparator dir) <> "#claude"
  panes <- filter (\(k, _, _, _) -> claudeKeyFor base k) <$> liveRunPanes
  case panes of
    [] -> return False
    ((_, sid, wid, pid) : _) -> showTmuxPane sid wid pid >> return True

-- | Make tmux pane @p@ of window @w@ current, and bring up the leksah TAB that
-- shows it.
--
-- The tab is asked for by the LEKSAH WINDOW that owns tmux window @w@, not by
-- the tmux session @s@.  A session can back several leksah windows — one per
-- tmux window — and a request naming only the session resolves to whichever of
-- them is first, so selecting a pane in any other window raised the wrong tab:
-- "show me this agent" put a plain shell on screen while the agent's own pane
-- stayed hidden one tab over.  Falling back to @s@ covers the window no leksah
-- window owns yet (the reconcile mints one within a poll tick).
showTmuxPane :: Text -> Text -> Text -> IO ()
showTmuxPane s w p = do
  tmuxCmd ["select-window", "-t", T.unpack w]
  tmuxCmd ["select-pane", "-t", T.unpack p]
  mlw <- lwForTmuxWindow w
  requestLocalTerm (fromMaybe s mlw)

-- | The leksah window (wide0 tab) holding tmux window @w@, if one does.
lwForTmuxWindow :: Text -> IO (Maybe Text)
lwForTmuxWindow w = getGlobalApp >>= \case
  Nothing  -> return Nothing
  Just app -> do
    ui <- readCell (appUi app)
    return (fst <$> windowOwner w (_leksahWindows ui))

-- | Bring the terminal running LIVE session @sid@ to the front: find the tmux
-- pane whose process tree owns that session's @claude@ process, select its
-- window and pane, and open (or bring up) that session's terminal tab.
--
-- Identification is by pid ('claudeLiveOwners' keys a session by every pid it
-- runs under, and 'livePanePids' gives each pane's), so it is exact even with
-- two sessions open in the same directory — where the run key alone would be
-- ambiguous.  Falls back to the session's directory MRU claude pane when the
-- pane can't be identified (no @ps@; a session running on a remote host or in a
-- terminal outside leksah), and returns 'False' when there is nothing to show.
showLiveSession :: Text -> IO Bool
showLiveSession sid = paneForSession sid >>= \case
    Just (s, w, p) -> showTmuxPane s w p >> return True
    Nothing -> claudeLiveBySession >>= \live -> case M.lookup sid live of
      Just l | not (null (clDir l)) -> mruClaudeUnclaimed sid (clDir l)
      _                             -> return False

-- | The directory-MRU fallback of 'showLiveSession', but never someone ELSE's
-- pane.  The MRU pane for a directory says nothing about WHICH of that
-- directory's sessions is in it, so a session with no pane of its own — running
-- in another terminal, on another tmux server, or as a background job with no
-- terminal at all — used to be "shown" by raising a different live session's
-- pane.  From the Agents pane that read as one row activating another row's
-- agent.
--
-- A pane running no live session we can name is still fair game: that is the
-- case the fallback exists for (no @ps@, so 'paneForSession' can identify
-- nothing at all, and the run key is the only evidence there is).
mruClaudeUnclaimed :: Text -> FilePath -> IO Bool
mruClaudeUnclaimed sid dir = do
  let base = T.pack (dropTrailingPathSeparator dir) <> "#claude"
  panes <- filter (\(k, _, _, _) -> claudeKeyFor base k) <$> liveRunPanes
  case panes of
    [] -> return False
    ((_, s, w, p) : _) -> sessionForPaneId p >>= \case
      Just other | other /= sid -> return False
      _                         -> showTmuxPane s w p >> return True

-- | The tmux @(session id, window id, pane id)@ whose process tree owns LIVE
-- session @sid@ — the exact identification 'showLiveSession' documents, factored
-- out so senders can aim at a session without also navigating to it.
-- 'Nothing' when the session isn't running in a pane of leksah's tmux server
-- (not running at all, on a remote host, or in a terminal outside leksah).
paneForSession :: Text -> IO (Maybe (Text, Text, Text))
paneForSession sid = do
  owners <- claudeLiveOwners
  panes  <- livePanePids
  return $ listToMaybe
    [ (s, w, p) | (s, w, p, ppid) <- panes
                , Just l <- [M.lookup ppid owners], clSession l == sid ]

-- | The live session running in tmux pane @pid@ (@%7@), if any — the reverse of
-- 'paneForSession', for asking "is THIS pane a Claude session?".  A pane whose
-- own session is the obvious AI target needs no stored binding.
sessionForPaneId :: Text -> IO (Maybe Text)
sessionForPaneId pid = do
  owners <- claudeLiveOwners
  panes  <- livePanePids
  return $ listToMaybe
    [ clSession l | (_, _, p, ppid) <- panes, p == pid
                  , Just l <- [M.lookup ppid owners] ]

-- | The live session that process @pid@ is running UNDER — walk up its parents
-- until one of them is a @claude@ process we know.  Note the direction:
-- 'claudeLiveOwners' indexes a session by its own ANCESTORS, whereas here the
-- caller is a DESCENDANT (leksah-cmd, or the @leksah-cmd mcp@ server, invoked by
-- a session), so this is the other lookup.  Lets a pane opened by a Claude
-- session be bound to it.
-- | The @claude@ process a pid runs under (itself or its nearest ancestor with
-- a session file) and the session id that file names now — for a hook, which
-- runs as a child of the session it reports on.  Unlike 'sessionOwningPid' it
-- looks at every session file, continued or not.
claudeProcessOf :: Int -> IO (Maybe (Int, Text))
claudeProcessOf pid = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  ls <- claudeSessionFiles
  parents <- readParentPids
  let byPid = M.fromList [ (clPid l, clSession l) | l <- ls ]
      go :: Int -> Int -> Maybe (Int, Text)
      go 0 _ = Nothing
      go n p = case M.lookup p byPid of
        Just s  -> Just (p, s)
        Nothing -> case M.lookup p parents of
          Just q | q > 1 -> go (n - 1) q
          _              -> Nothing
  return (go (12 :: Int) pid)

sessionOwningPid :: Int -> IO (Maybe Text)
sessionOwningPid pid = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  ls <- claudeLiveSessions
  if null ls then return Nothing else do
    parents <- readParentPids
    let byPid = M.fromList [ (clPid l, l) | l <- ls ]
        -- Bounded like 'claudeLiveOwners'' walk, so a garbled table can't spin.
        go :: Int -> Int -> Maybe Text
        go 0 _ = Nothing
        go n p = case M.lookup p byPid of
          Just l -> Just (clSession l)
          Nothing -> case M.lookup p parents of
            Just q | q > 1 -> go (n - 1) q
            _              -> Nothing
    return (go (12 :: Int) pid)

-- | A tmux pane's current working directory — which project a plain shell pane
-- belongs to, so it can inherit that project's AI session.  'Nothing' if the
-- pane is gone or tmux failed.
paneCurrentPath :: Text -> IO (Maybe FilePath)
paneCurrentPath pane = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  (ec, out, _) <- readProcessWithExitCode "tmux"
    ["-L", "leksah", "display-message", "-p", "-t", T.unpack pane
    , "-F", "#{pane_current_path}"] ""
  let p = T.strip (T.pack out)
  return $ if ec /= ExitSuccess || T.null p then Nothing else Just (T.unpack p)

-- | Type literal @txt@ into LIVE session @sid@'s pane (no Enter, so it lands
-- unsubmitted in the session's composer).  'False' when the session isn't in a
-- pane we can reach — the caller should offer to resume it.
sendToSession :: Text -> Text -> IO Bool
sendToSession sid txt = paneForSession sid >>= \case
  Nothing           -> return False
  Just (_, _, pane) -> sendKeysTo pane ["-l", T.unpack txt]

-- | A label for session @sid@ in @dir@: the first user prompt of its
-- transcript.  One head-read of that ONE file, where 'claudeSessionsFor' scans
-- every transcript in the project folder — for callers that already know which
-- session they are labelling (a live session has no other cheap label unless
-- @/rename@ gave it a name).  'Nothing' when the transcript isn't readable, or
-- holds no prompt yet.
claudeSessionLabel :: FilePath -> Text -> IO (Maybe Text)
claudeSessionLabel dir sid = (`catch` \(_ :: SomeException) -> return Nothing) $
  claudeTranscriptPath dir sid >>= fmap snd . scanHead

-- | Copy a session id to the system clipboard (best-effort).
copySessionId :: Text -> IO ()
copySessionId sid = clipboardCopyCmd >>= \case
  Nothing  -> return ()
  Just ccmd -> void . (`catch` \(_ :: SomeException) -> return "") $
      readProcess "sh" ["-c", ccmd] (T.unpack sid)

-- | Reveal a transcript in the OS file manager.
revealSession :: FilePath -> IO ()
revealSession f = void . (`catch` \(_ :: SomeException) -> return ()) $
  if os == "darwin"
    then void (createProcess (proc "open" ["-R", f]))
    else void (createProcess (proc "xdg-open" [takeDirectory f]))

-- | Delete a session's transcript (the caller should rescan afterwards).
deleteSession :: FilePath -> IO ()
deleteSession f = removeFile f `catch` \(_ :: SomeException) -> return ()
