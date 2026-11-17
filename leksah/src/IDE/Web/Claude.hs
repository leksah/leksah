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
  , prepareAgentLaunchFiles
  , ClaudeSession(..)
  , csTitle
  , claudeSessionsFor
  , claudeSessionNames
  , ClaudeLive(..)
  , claudeLiveSessions
  , claudeLiveBySession
  , ClaudeBackground(..)
  , claudeBackgroundSessions
  , leksahPluginInstall
  , leksahPluginUninstall
  , leksahPluginStatus
  , leksahPluginWrite
  , backgroundNeedsYou
  , backgroundStaleAge
  , claudeRemoveBackground
  , claudeLiveOwners
  , ClaudeCmd(..)
  , AgentSpec(..)
  , newAgentSessionId
  , runClaudeCmd
  , claudeCommandLine
  , claudeLatestActivity
  , latestActivityOf
  , parseSessionFile
  , gistOfTail
  , pidAncestors
  , TranscriptGist(..)
  , noGist
  , cachedTranscriptGist
  , liveTranscriptPath
  , codexDiscover
  , describeToolUse
  , claudeProcessOf
  , claudePaneSessions
  , nameNote
  , planHtmlNote
  , worktreeNote
  , agentNote
  , managerNote
  , claudeRunning
  , claudeKeyFor
  , mruClaudePane
  , claudeLatestPlan
  , claudeTranscriptPath
  , claudeFindTranscript
  , activateMruClaude
  , showLiveSession
  , showLiveSessionHere
  , showTmuxPane
  , showTmuxPaneWith
  , raiseTabOfPane
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
import Data.Maybe (isJust, isNothing, mapMaybe, listToMaybe, fromMaybe)
import Data.Ord (Down(..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
       (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)

import Data.Aeson
       (Object, Value(..), decodeStrict', withObject, (.:), (.:?), object, (.=),
        encode)
import Data.Aeson.Types (Pair, Parser, parseMaybe, FromJSON)
import qualified Data.Aeson.Key as Key (fromText)
import qualified Data.Aeson.KeyMap as KM (lookup)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL (ByteString, toStrict, writeFile)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)
import Numeric (showHex)

import System.Directory
       (findExecutable, getHomeDirectory, doesDirectoryExist, doesFileExist,
        listDirectory, getModificationTime, removeFile, createDirectoryIfMissing,
        removeDirectory, renameFile)
import System.FilePath
       ((</>), takeExtension, takeBaseName, takeDirectory, takeFileName,
        dropTrailingPathSeparator, replaceExtension)
import System.Info (os)
import System.Environment (lookupEnv)
import System.IO
       (withFile, IOMode(ReadMode), hIsEOF, hSeek,
        SeekMode(SeekFromEnd), hFileSize)

import IDE.Utils.RemotePath (isRemotePath, parseRemotePath)
import IDE.Utils.RemoteExec (runSsh)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (ExitCode(..))
import System.Process
       (CreateProcess(std_in, std_out, std_err), StdStream(..), createProcess, proc, readProcess,
        readProcessWithExitCode, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import IDE.App (appNote, appUi, getGlobalApp)
import IDE.Web.Codex
       (CodexLive(..), codexLiveRecords, codexEnsureInstalled, codexWriteScripts,
        codexClaimLaunch, codexRememberLaunch, codexStarted, isCodexProgram,
        codexAvailable, codexInteractive, codexKnownPid,
        codexTranscriptOf, rolloutFirstPrompt, rolloutTurns, isRollout)
import IDE.Reactive (readCell)
import IDE.Web.Instance (tmuxServerSocket)
import IDE.Web.DemoAgents (demoShowAgent)
import IDE.Web.Model (WebUi(_leksahWindows))
import IDE.Web.AgentEvents (perToolHookEvents)
import IDE.Web.AgentNames
       (adoptContinuation, agentContinuations, agentNameFor, sessionOfRunKey)
import IDE.Web.ReplTmux
       (clipboardCopyCmd, cmdPrefixForDir, liveRunKeys,
        liveRunPanes, livePanePids, livePaneTitles, tmuxCmd, sendKeysTo, findRunPane,
        newAgentWindow, tmuxOut)
import IDE.Web.RemoteTermRequest (requestLocalTerm, requestBringTerm)
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
  , clName       :: Maybe Text  -- ^ the name @/rename@ (or @--name@) gave it, if
                                --   any — never the one Claude Code DERIVES from
                                --   its directory (@haskell-nix-d1@), which says
                                --   nothing about the session: that is
                                --   'clDerivedName'
  , clDerivedName :: Maybe Text -- ^ the name Claude Code derived for it itself
                                --   (@nameSource: derived@); only for matching
                                --   it against what a terminal shows, not to
                                --   label it
  , clStatus     :: Maybe Text  -- ^ semantic state the CLI writes alongside the
                                --   pid: @busy@ (agent working) / @shell@ (a
                                --   shell command running) / @waiting@ (blocked
                                --   on an approval prompt) / @idle@ (ready for
                                --   input).  'Nothing' on an older CLI.
  , clWaitingFor :: Maybe Text  -- ^ what a @waiting@ session is blocked on
                                --   (e.g. the tool name), when the CLI says
  , clJob        :: Maybe Text  -- ^ a background session's job (short) id — what
                                --   @claude attach \<id\>@ names
  , clKind       :: Maybe Text  -- ^ @bg@ for a background session (run by Claude
                                --   Code's daemon, shown in whichever terminal
                                --   is attached to it — see
                                --   'claudePaneSessions'); else interactive
  , clHost       :: Maybe Text  -- ^ the machine it runs on, when that is not this
                                --   one ("IDE.Web.RemoteAgents": a session on a
                                --   host whose leksah tmux server this leksah
                                --   shares); 'Nothing' here
  , clAgent      :: Text        -- ^ which CLI runs it: @claude@, or @codex@ (a
                                --   record leksah's own hooks keep — see
                                --   "IDE.Web.Codex")
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
  let fresh = [ l | l <- ls, clAgent l == "claude", clSession l `S.notMember` checked ]
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

-- | Every @~\/.claude\/sessions\/*.json@ as a 'ClaudeLive', and every Codex
-- session leksah's hooks keep a record of ("IDE.Web.Codex") — pid liveness and
-- continuations not considered yet.
claudeSessionFiles :: IO [ClaudeLive]
claudeSessionFiles = (<>) <$> claudeOwnSessionFiles <*> (map fromCodex <$> codexLiveRecords)
  where
    fromCodex r = ClaudeLive
      { clPid = cxPid r, clSession = cxSession r, clDir = cxDir r, clName = Nothing
      , clDerivedName = Nothing
      , clStatus = Just (cxStatus r), clWaitingFor = cxWaitingFor r
      , clJob = Nothing, clKind = Nothing, clHost = Nothing, clAgent = "codex" }

claudeOwnSessionFiles :: IO [ClaudeLive]
claudeOwnSessionFiles = (`catch` \(_ :: SomeException) -> return []) $ do
  home <- getHomeDirectory
  let sdir = home </> ".claude" </> "sessions"
  ex <- doesDirectoryExist sdir
  if not ex then return [] else do
    names <- listDirectory sdir
    fmap concat . forM [ sdir </> n | n <- names, takeExtension n == ".json" ] $ \f ->
      (`catch` \(_ :: SomeException) -> return []) $
        maybe [] pure . parseSessionFile Nothing <$> BS.readFile f

-- | One @~\/.claude\/sessions\/*.json@, read on @host@ ('Nothing': here).
parseSessionFile :: Maybe Text -> BS.ByteString -> Maybe ClaudeLive
parseSessionFile host bytes = do
  v <- decodeStrict' bytes
  p <- objField "pid" v
  i <- objField "sessionId" v
  let name    = mfilter (not . T.null) (objField "name" v)
      derived = objField "nameSource" v == Just ("derived" :: Text)
  return ClaudeLive
    { clPid        = p
    , clSession    = i
    , clDir        = maybe "" (dropTrailingPathSeparator . T.unpack) (objField "cwd" v)
    , clName       = if derived then Nothing else name
    , clDerivedName = if derived then name else Nothing
    , clStatus     = mfilter (not . T.null) (objField "status" v)
    , clWaitingFor = mfilter (not . T.null) (objField "waitingFor" v)
    , clJob        = mfilter (not . T.null) (objField "jobId" v)
    , clKind       = mfilter (not . T.null) (objField "kind" v)
    , clHost       = host
    , clAgent      = "claude" }

-- | One background session (@claude --bg@, or one dispatched from @claude
-- agents@), as the CLI's own @claude agents --json@ reports it — the one
-- documented, machine-readable list of them.  A background session that is
-- running has a session file like any other and shows up in
-- 'claudeLiveBySession'; one that has stopped (blocked on a question,
-- finished, failed, stopped) has no process and no session file, so this is
-- the only way leksah sees it at all.
data ClaudeBackground = ClaudeBackground
  { cbId      :: Text            -- ^ the short id @claude attach@ takes
  , cbSession :: Text            -- ^ the session id (@claude --resume@ takes it)
  , cbName    :: Maybe Text
  , cbState   :: Text            -- ^ working | blocked | done | failed | stopped
  , cbDir     :: FilePath
  , cbStarted :: Maybe Double    -- ^ POSIX seconds
  , cbPid     :: Maybe Int       -- ^ while its process is alive
  , cbLastActive :: Maybe UTCTime -- ^ when it last did anything: its transcript's
                                 --   mtime, else when it started
  , cbStateAt :: Maybe UTCTime   -- ^ when its job last reported (its state
                                 --   file's mtime — never its contents)
  } deriving (Eq, Show)

-- | A blocked background session that is waiting for YOU: no process, it
-- blocked recently, and its conversation has not moved on since.
--
--   * One left blocked for longer than 'backgroundStaleAge' was most likely
--     abandoned; it still shows in the Agents pane (Forget removes it) but no
--     longer counts as needing you — in the pane's sort order, or on the
--     status dot and menu-bar item.  When there is no telling how long ago it
--     last did anything, it counts as stale too: a session nobody can date
--     must not keep the dot red for good.
--   * One whose transcript was written well after its job last reported
--     ('cbStateAt') was taken up since — resumed in a pane, say, and
--     answered — and the CLI's "blocked" is out of date.
backgroundNeedsYou :: UTCTime -> ClaudeBackground -> Bool
backgroundNeedsYou now b =
  cbState b == "blocked" && isNothing (cbPid b)
    && maybe False (\t -> realToFrac (diffUTCTime now t) <= backgroundStaleAge) (cbLastActive b)
    && not movedOn
  where
    movedOn = case (cbLastActive b, cbStateAt b) of
      (Just act, Just st) -> realToFrac (diffUTCTime act st) > (60 :: Double)
      _                   -> False

-- | How long a blocked background session counts as needing you: a day.
backgroundStaleAge :: Double
backgroundStaleAge = 24 * 3600

-- | The background sessions, from @claude agents --json@ — CACHED: the CLI
-- takes a few hundred ms, and the status poll and the Agents pane call this
-- every few seconds.  Returns what the last run found at once, and starts a
-- fresh run in the background (one at a time) when something may have
-- changed — so the first call after start-up returns @[]@ and the list fills
-- in on the next poll.  Never throws: no @claude@, a failed run or unparsable
-- output keep the previous list (and the change that prompted the run still
-- counts as unseen, so the next poll after 'backgroundTtl' tries again).
--
-- "May have changed": a background session's state lives in
-- @~\/.claude\/jobs\/\<id\>\/state.json@, rewritten on every change.  Those
-- files are only WATCHED here — their mtimes, never their contents (the
-- format is undocumented; @claude agents --json@ is the interface) — and the
-- CLI is run again when any of them (or the set of jobs) changes, at most
-- every 'backgroundTtl'; with nothing changing, every 'backgroundBackstop'.
-- They are only looked at when a run could start at all.
claudeBackgroundSessions :: IO [ClaudeBackground]
claudeBackgroundSessions = do
  now <- realToFrac <$> getPOSIXTime
  st0 <- readIORef backgroundRef
  when (not (bsRunning st0) && now - bsAt st0 > backgroundTtl) $ do
    fp <- jobsFingerprint
    start <- atomicModifyIORef' backgroundRef $ \st ->
      let due = now - bsAt st > backgroundBackstop
                || (fp /= bsSeen st && now - bsAt st > backgroundTtl)
      in if not (bsRunning st) && due then (st { bsRunning = True }, True) else (st, False)
    when start . void . forkIO $ do
      fresh <- (Just <$> (runAgentsJson >>= mapM (withTimes fp)))
                 `catch` \(_ :: SomeException) -> return Nothing
      done <- realToFrac <$> getPOSIXTime
      atomicModifyIORef' backgroundRef $ \st -> (case fresh of
        -- A failed run leaves the change it was for unseen: retried after
        -- 'backgroundTtl', not left until the backstop.
        Nothing -> st { bsAt = done, bsRunning = False }
        Just xs ->
          -- A session removed (Forget) while this run was in flight may
          -- still be in its output: keep it out, until the CLI stops
          -- listing it.
          let listed = map cbId xs
          in st { bsAt = done, bsRunning = False, bsSeen = fp
                , bsList = filter ((`notElem` bsRemoved st) . cbId) xs
                , bsRemoved = filter (`elem` listed) (bsRemoved st) }, ())
  bsList <$> readIORef backgroundRef
 where
  jobsFingerprint = (`catch` \(_ :: SomeException) -> return []) $ do
    home <- getHomeDirectory
    let jobs = home </> ".claude" </> "jobs"
    ex <- doesDirectoryExist jobs
    if not ex then return [] else do
      ids <- listDirectory jobs
      fmap concat . forM ids $ \i -> do
        let f = jobs </> i </> "state.json"
        e <- doesFileExist f
        if e then (\t -> [(i, t)]) <$> getModificationTime f else return [(i, posixSecondsToUTCTime 0)]
  -- When it last did anything (its transcript, else when it started), and
  -- when its job last reported (its state file's mtime, from the fingerprint).
  withTimes fp b = do
    t <- (`catch` \(_ :: SomeException) -> return Nothing) $ do
      p <- claudeTranscriptPath (cbDir b) (cbSession b)
      e <- doesFileExist p
      if e then Just <$> getModificationTime p else return Nothing
    return b { cbLastActive = t <|> (posixSecondsToUTCTime . realToFrac <$> cbStarted b)
             , cbStateAt = lookup (T.unpack (cbId b)) fp }
  -- The output is read as BYTES (it is UTF-8 JSON; a String round trip
  -- through the locale would mangle any non-ASCII name or path), and under a
  -- timeout: a CLI stuck on a prompt or a network stall must not wedge the
  -- refresh, which only ever runs one at a time.
  runAgentsJson = findExecutable "claude" >>= \case
    Nothing  -> return []
    Just exe -> do
      (_, Just hout, _, ph) <- createProcess (proc exe ["agents", "--json"])
        { std_in = NoStream, std_out = CreatePipe, std_err = NoStream }
      r <- timeout 15000000 $ do
        out <- BS.hGetContents hout
        (,) out <$> waitForProcess ph
      case r of
        Just (out, ExitSuccess) | Just vs <- decodeStrict' out -> return (mapMaybe one vs)
        Nothing -> do
          terminateProcess ph
          ioError (userError "claude agents --json timed out")
        _ -> ioError (userError "claude agents --json failed")
  one v = do
    k <- objField "kind" v
    if (k :: Text) /= "background" then Nothing else do
      sid <- objField "sessionId" v
      Just ClaudeBackground
        { cbId      = fromMaybe (T.take 8 sid) (objField "id" v)
        , cbSession = sid
        , cbName    = mfilter (not . T.null) (objField "name" v)
        , cbState   = fromMaybe "working" (objField "state" v)
        , cbDir     = maybe "" (dropTrailingPathSeparator . T.unpack) (objField "cwd" v)
        , cbStarted = (/ 1000) . fromInteger <$> objField "startedAt" v
        , cbPid     = objField "pid" v
        , cbLastActive = Nothing
        , cbStateAt = Nothing }

-- | Remove background session @jobId@ from Claude Code's list (@claude rm@:
-- it keeps the conversation, and deletes the session's worktree only when
-- that is safe).  Answers what the CLI said.  The Agents pane's Forget, for a
-- row that came from @claude agents@ — leksah's own record has nothing to
-- forget, and the row would come straight back.
claudeRemoveBackground :: Text -> IO Text
claudeRemoveBackground jobId = (`catch` \(e :: SomeException) -> return (T.pack (show e))) $
  findExecutable "claude" >>= \case
    Nothing  -> return "claude is not on PATH"
    Just exe -> do
      r <- timeout 30000000 (readProcessWithExitCode exe ["rm", T.unpack jobId] "")
      -- Out of the list at once, and kept out even if a run already in
      -- flight still lists it; a fresh run is due straight away.
      atomicModifyIORef' backgroundRef (\st ->
        (st { bsAt = 0, bsList = filter ((/= jobId) . cbId) (bsList st)
            , bsRemoved = jobId : bsRemoved st }, ()))
      return $ case r of
        Nothing -> "claude rm " <> jobId <> " timed out"
        Just (_, out, err) -> T.strip (T.pack (out <> err))

-- | The cache behind 'claudeBackgroundSessions'.
data BackgroundState = BackgroundState
  { bsAt      :: Double                 -- ^ when the last run finished
  , bsRunning :: Bool                   -- ^ a run is in flight
  , bsSeen    :: [(FilePath, UTCTime)]  -- ^ the jobs' state-file mtimes it ran for
  , bsList    :: [ClaudeBackground]     -- ^ what it found
  , bsRemoved :: [Text]                 -- ^ job ids Forget removed meanwhile
  }

{-# NOINLINE backgroundRef #-}
backgroundRef :: IORef BackgroundState
backgroundRef = unsafePerformIO (newIORef (BackgroundState 0 False [] [] []))

-- | The least time between two runs of @claude agents --json@, however much
-- changes.
backgroundTtl :: Double
backgroundTtl = 10

-- | The most time between two runs while nothing seems to change (a change
-- the jobs' state files don't show — a session removed from elsewhere).
backgroundBackstop :: Double
backgroundBackstop = 300

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
--
-- A BACKGROUND session (@kind: bg@) claims only its own pid: it runs under
-- Claude Code's daemon, whose ancestry leads to whichever terminal happened
-- to start the daemon — not to the pane showing the session.  Which pane that
-- is, 'claudePaneSessions' works out from the pane's title.
claudeLiveOwners :: IO (Map Int ClaudeLive)
claudeLiveOwners = (`catch` \(_ :: SomeException) -> return M.empty) $ do
  ls <- claudeLiveSessions
  if null ls then return M.empty else do
    parents <- readParentPids
    return $ M.fromList
      [ (p, l) | l <- ls
               , p <- clPid l : (if clKind l == Just "bg" then []
                                  else pidAncestors parents (clPid l)) ]

-- | A pid's ancestors, up to the session leader; bounded so a cycle in a
-- garbled table can't spin.
pidAncestors :: Map Int Int -> Int -> [Int]
pidAncestors parents = go (12 :: Int)
  where go 0 _ = []
        go n p = case M.lookup p parents of
          Just q | q > 1 -> q : go (n - 1) q
          _              -> []

-- | Every tmux pane running a live Claude session, as @((tmux session,
-- window, pane), session)@ — the one place that answers "which conversation
-- is in this pane?".
--
--   * A pane whose process tree runs an interactive session's @claude@ shows
--     that session ('claudeLiveOwners').
--   * A pane running @claude attach \<job id\>@ (what @claude --resume@ of a
--     running background session becomes) shows that job's session.
--   * Otherwise, a pane hosting a Claude front end whose own session moved to
--     the background (← on an empty prompt, /bg) may be ATTACHED to any
--     background session — the agent view lets you switch between them —
--     and nothing in the process table says which.  Claude Code titles the
--     pane after the conversation it shows (@#{pane_title}@, a status glyph
--     first), and a background session's file carries the same name, so a
--     unique match on that is the attached session.  The agent view's own
--     title (@N awaiting input · claude agents@) matches nothing.
claudePaneSessions :: IO [((Text, Text, Text), ClaudeLive)]
claudePaneSessions = (`catch` \(_ :: SomeException) -> return []) $ do
  owners  <- claudeLiveOwners
  panes   <- livePanePids
  live    <- claudeLiveBySession
  if M.null live then return [] else do
    titles  <- livePaneTitles
    files   <- claudeSessionFiles
    procs   <- readProcessArgs
    let parents = fst <$> procs
        -- @claude attach \<job\>@ processes, by the pids they run under
        attachJobs = M.fromList
          [ (q, job) | (p, (_, args)) <- M.toList procs
                     , (exe : "attach" : job : _) <- [words args]
                     , takeFileName exe == "claude"
                     , q <- p : pidAncestors parents p ]
        bgByJob = M.fromList [ (j, l) | l <- M.elems live, clKind l == Just "bg"
                                      , Just j <- [clJob l] ]
        attachedProc ppid = M.lookup ppid attachJobs >>= \j -> M.lookup (T.pack j) bgByJob
        -- pids that run (or are the ancestors of) a Claude front end, live or
        -- not: the panes worth reading a title from
        hosts = S.fromList [ p | f <- files, clKind f /= Just "bg"
                               , p <- clPid f : pidAncestors parents (clPid f) ]
        bgByName = M.fromListWith (\_ _ -> Nothing)
          [ (T.toLower n, Just l) | l <- M.elems live, clKind l == Just "bg"
                                  , Just n <- [clName l <|> clDerivedName l] ]
        attached p ppid
          | ppid `S.member` hosts
          , Just t <- M.lookup p titles
          , Just (Just l) <- M.lookup (T.toLower (paneTitleText t)) bgByName = Just l
          | otherwise = Nothing
    return [ ((s, w, p), l)
           | (s, w, p, ppid) <- panes
           , Just l <- [ M.lookup ppid owners <|> attachedProc ppid <|> attached p ppid ] ]

-- | A Claude pane's title without the status glyph Claude Code puts in front
-- (@✳@, a spinner frame, …).
paneTitleText :: Text -> Text
paneTitleText = T.strip . T.dropWhile (\c -> not (isAlphaNum c) && c /= '(')

-- | @pid -> (ppid, command line)@ for every process on the machine.
readProcessArgs :: IO (Map Int (Int, String))
readProcessArgs = (`catch` \(_ :: SomeException) -> return M.empty) $ do
  out <- readProcess "ps" ["-Ao", "pid=,ppid=,args="] ""
  return $ M.fromList
    [ (p, (q, unwords rest)) | l <- lines out
                             , (pT : qT : rest) <- [words l]
                             , Just p <- [readMaybe pT], Just q <- [readMaybe qT] ]

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

-- | Where a session ran, when nothing but its id is known: its transcript is
-- looked for in every project folder, and its head read for the @cwd@.  A full
-- directory scan — for one-off recovery, not for a poll.
claudeFindTranscript :: Text -> IO (Maybe FilePath)
claudeFindTranscript sid = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  home <- getHomeDirectory
  let root = home </> ".claude" </> "projects"
      leaf = T.unpack sid <> ".jsonl"
  ex <- doesDirectoryExist root
  if not ex then return Nothing else do
    ds <- listDirectory root
    found <- filterM doesFileExist [ root </> d </> leaf | d <- ds ]
    case found of
      (f : _) -> fmap (dropTrailingPathSeparator . T.unpack) . fst <$> scanHead f
      []      -> return Nothing

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
                                       --   "IDE.Web.Agent" (a Codex one too,
                                       --   with 'asCodex')
  | CodexNew           FilePath        -- ^ @codex@ — a fresh Codex session
  | CodexResume        FilePath Text   -- ^ @codex resume <id>@

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
  , asCodex   :: Bool       -- ^ run Codex rather than Claude Code ('codexCommandLine')
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
    -- (its own tab: the session's first would be another agent's, every
    -- agent being on the Agents session)
    Just (sid, wid, pid) -> showTmuxPane sid wid pid
    Nothing -> do
      -- A window of the shared Agents session, called after the agent
      -- ("Wren": the tab, and the spoken bell) whenever the launch pins its
      -- session id; a launch that can't know its id yet (@-c@, the picker)
      -- is named by the Terminals poll once its session shows up
      -- ('IDE.Web.Widget.Terminal').
      name <- traverse agentNameFor (sessionOfRunKey key)
      newAgentWindow name d (Just key) line' False
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
    , asManager = False, asCodex = False }
claudeCommandLine (CodexNew d) = do
  tok <- newAgentSessionId
  codexCommandLine AgentSpec
    { asDir = d, asSession = tok, asFrom = Nothing, asPrompt = Nothing
    , asParent = Nothing, asRoleNote = Nothing, asResume = False
    , asManager = False, asCodex = True }
claudeCommandLine (CodexResume d i) = codexCommandLine AgentSpec
    { asDir = d, asSession = i, asFrom = Nothing, asPrompt = Nothing
    , asParent = Nothing, asRoleNote = Nothing, asResume = True
    , asManager = False, asCodex = True }
claudeCommandLine (ClaudeAgent s) | asCodex s = codexCommandLine s
claudeCommandLine (ClaudePrompt d p) = do
  -- Pinned for the same reason, and so a queued task's agent has a name (and
  -- an Agents-pane identity) from the moment it starts.  The run key keeps the
  -- @dir#claude@ prefix the worktree flows match on ('claudeKeyFor').
  sid <- newAgentSessionId
  claudeCommandLine . ClaudeAgent $ AgentSpec
    { asDir = d, asSession = sid, asFrom = Nothing, asPrompt = Just p
    , asParent = Nothing, asRoleNote = Nothing, asResume = False
    , asManager = False, asCodex = False }
claudeCommandLine cmd = do
  let d = dropTrailingPathSeparator dir
  mbPrefix <- mfilter (not . T.null) <$> cmdPrefixForDir d
  -- Local sessions get leksah's MCP server (IDE tools: diagnostics, open_file,
  -- build, hover, screenshot…) — see `leksah-cmd mcp`.  The config file points
  -- at a local binary, so remote (ssh://) dirs skip it.
  -- Whether the leksah plugin brings the hooks and the MCP server instead
  -- (installed, and not turned off for this project): worked out once.
  plugin    <- if isRemotePath d then return False else leksahPluginEnabledFor (Just d)
  mcpFlag   <- if isRemotePath d then return "" else claudeMcpFlag plugin
  -- (on another machine: the hooks and MCP server a leksah THERE set up, when
  -- one has — their files name that machine's leksah-cmd — else none)
  (envPrefix, hooksFlag) <- case parseRemotePath d of
    Just (host, _) -> remoteAgentFlags host
    Nothing        -> (\f -> ("", f)) <$> claudeHooksFlag plugin d
  -- The agent's own name, when its session id is known at launch — so it can
  -- tell the user and other agents who it is.
  myName <- traverse agentNameFor ownSession
  let flags = mcpFlag <> hooksFlag <> allowFlag
           <> appendSystemPrompt (planHtmlNote : worktreeNote
                                   : maybe [] ((: []) . nameNote) myName <> notes)
      line' = envPrefix <> maybe line (\p -> p <> " " <> line) mbPrefix <> flags
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

-- | The command line for a Codex agent.  Codex cannot be told its session id,
-- so 'asSession' is a TOKEN standing in for it: the launch exports it as
-- @LEKSAH_AGENT_TOKEN@, the hooks hand it back with the session's real id at
-- SessionStart, and leksah then moves everything it knows from the token to
-- that id ('IDE.Web.Agent.agentHook').  The run key keeps the
-- @\<dir\>#claude#\<id\>@ shape every AI-pane lookup parses, so a Codex pane
-- is an agent pane everywhere.  (On a resume the token is the real id.)
--
-- What a leksah launch tells a Claude session on its command line (its name,
-- who started it, the worktree note …) reaches a Codex one from its
-- SessionStart hook instead, so the line itself is just @codex@.  The first
-- launch installs leksah's hooks into Codex if they are missing — and Codex
-- runs them only after the user trusts them in @/hooks@.
codexCommandLine :: AgentSpec -> IO (FilePath, Text, Text)
codexCommandLine s = do
  let d = dropTrailingPathSeparator (asDir s)
  mbPrefix <- mfilter (not . T.null) <$> cmdPrefixForDir d
  unless (isRemotePath d) $ do
    _ <- codexWriteScripts
    codexEnsureInstalled >>= mapM_ (\msg ->
      getGlobalApp >>= mapM_ (\app -> appNote app msg))
  -- Resuming an agent that never reached its first prompt has nothing to
  -- resume (its id is still the launch token): start Codex afresh under it.
  started <- if asResume s then codexStarted (asSession s) else return False
  unless started $ codexRememberLaunch (asSession s) d
  let line = T.unwords $
        [ "env", "LEKSAH_AGENT_TOKEN=" <> asSession s, "codex" ]
        <> [ x | started, x <- ["resume", asSession s] ]
        <> maybe [] ((:[]) . shq) (asPrompt s)
      key = T.pack d <> "#claude#" <> asSession s
  return (d, key, maybe line (\p -> p <> " " <> line) mbPrefix)

-- | Codex processes in leksah's terminals that no record covers yet: record
-- each straight away, so it shows up like any other agent.  Codex's TUI
-- reports SessionStart only when the first prompt is sent, so without this a
-- fresh Codex pane — leksah's own launches, and a @codex@ typed into a
-- terminal — would be invisible until then.  Run by the status poll (every
-- few seconds), so it also finds the ones launched by a leksah that has
-- since restarted.
--
-- Each is recorded under the token its launch exported (read from the
-- process's own environment: @LEKSAH_AGENT_TOKEN@, see 'codexCommandLine'),
-- else under a fresh one; its real id takes over at SessionStart, which
-- carries the name and the agent record across ('IDE.Web.Agent.codexHook').
-- Only top-level interactive codex processes count — not its helpers, nor
-- @codex exec@ and the other subcommands.
--
-- Without @codex@ installed it does nothing at all (no @ps@); a codex running
-- outside leksah's terminals is looked at once, not on every poll.
codexDiscover :: IO ()
codexDiscover = (`catch` \(_ :: SomeException) -> return ()) $
 codexAvailable >>= \avail -> when avail $ do
  procs <- readProcessArgs
  recs  <- codexLiveRecords
  rejected <- readIORef discoverRejected
  let parents = fst <$> procs
      covered = S.fromList (map cxPid recs)
      prog p = maybe "" (takeWhile (/= ' ') . snd) (M.lookup p procs)
      cands0 = [ p | (p, (q, args)) <- M.toList procs
                   , isCodexProgram (takeWhile (/= ' ') args), codexInteractive args
                   , not (isCodexProgram (prog q))       -- not one of its helpers
                   , p `S.notMember` covered, p `S.notMember` rejected ]
  cands <- filterM (fmap not . codexKnownPid) cands0
  -- (Forget rejections of processes that are gone: pids are reused.)
  -- STRICTLY: nothing else forces this set unless a codex process is
  -- running, and a lazy write keeps this poll's whole process table (every
  -- command line as a String) alive inside the thunk, chained to the last
  -- poll's -- about 3.5GB an hour.
  writeIORef discoverRejected $! S.filter (`M.member` procs) rejected
  unless (null cands) $ do
    panes <- livePanePids
    forM_ cands $ \cp ->
      case [ pane | (_, _, pane, pp) <- panes, pp `elem` (cp : pidAncestors parents cp) ] of
        [] -> atomicModifyIORef' discoverRejected (\r -> (S.insert cp r, ()))
                                                         -- not in a leksah terminal
        (pane : _) -> do
          tok <- processToken cp >>= maybe newAgentSessionId return
          dir <- fromMaybe "" <$> paneCurrentPath pane
          codexRememberLaunch tok dir
          codexClaimLaunch cp tok dir

-- | Codex processes 'codexDiscover' found outside leksah's terminals.
{-# NOINLINE discoverRejected #-}
discoverRejected :: IORef (S.Set Int)
discoverRejected = unsafePerformIO (newIORef S.empty)

-- | The @LEKSAH_AGENT_TOKEN@ in a process's environment, if it has one.
processToken :: Int -> IO (Maybe Text)
processToken pid = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  envText <- if os == "darwin"
    then T.pack <$> readProcess "ps" ["-E", "-ww", "-o", "command=", "-p", show pid] ""
    else T.map (\c -> if c == '\0' then ' ' else c) . decodeUtf8With lenientDecode
           <$> BS.readFile ("/proc/" <> show pid <> "/environ")
  return $ listToMaybe
    [ t | w <- T.words envText, Just t <- [T.stripPrefix "LEKSAH_AGENT_TOKEN=" w]
        , not (T.null t) ]

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
  [ "You are a coding agent in a Leksah IDE terminal pane, started by \
    \another agent (session " <> parent <> ") that is working \
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
  [ "You are the MANAGER of the coding agents (Claude Code and Codex sessions) running in this Leksah IDE. \
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
    \blocked on an approval prompt (state waiting) — they need a human. The \
    \states are waiting, busy (working; its detail says when that is a shell \
    \command, which may run for a long time — the agent is still there) and \
    \idle (ready for input). An agent that has exited is not in the list at \
    \all: never resume one that is listed."
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
claudeMcpFlag :: Bool -> IO Text
claudeMcpFlag installed = (`catch` \(_ :: SomeException) -> return "") $
  findExecutable "leksah-cmd" >>= \case
    Nothing  -> return ""
    Just exe -> do
      home <- getHomeDirectory
      let path = home </> ".leksah" </> "mcp.json"
      -- (The installed leksah plugin brings this server to every session:
      -- a second copy would double every tool.)
      if installed then return "" else do
        writeIfChanged path . encode $ object [ "mcpServers" .= leksahMcpServers exe ]
        return (" --mcp-config " <> T.pack path)

-- | The @--settings@ flag wiring a leksah-launched session to the worktree
-- PostToolUse hook: every @git worktree add@\/@remove@ \/ @git switch@ a session
-- runs in Bash, and every worktree it enters or removes with Claude Code's own
-- worktree tools, lands in the worktree registry, no cooperation needed.  The hook script
-- itself is written by @leksah-cmd hooks write-script@ (the script text lives
-- with the CLI, which also installs it into user\/project settings on demand —
-- @leksah-cmd hooks install@); this just refreshes it and points a small
-- settings JSON at it.  Empty when @leksah-cmd@ isn't on PATH.  @--settings@
-- loads ADDITIONAL settings, so the user's own hooks keep working.
claudeHooksFlag :: Bool -> FilePath -> IO Text
claudeHooksFlag installed dir = (`catch` \(_ :: SomeException) -> return "") $
  leksahHookTools >>= \case
    Nothing -> return ""
    Just (exe, script) -> do
      home <- getHomeDirectory
      statusLine <- leksahStatusLine exe <$> claudeUserStatusLine exe dir
      -- One file per distinct content: running sessions may re-read their
      -- --settings file, so a launch must never rewrite the one another
      -- session is using into something different — with hooks for a
      -- session where the plugin is off, without for one where it is on
      -- (settings-statusline…), and per carried-over status-line options
      -- ('statusLineSettingsFile').
      let base = statusLineSettingsFile statusLine
          path = home </> ".leksah"
                   </> (if installed then "settings-statusline" <> drop (length ("hooks" :: String)) base
                                     else base)
      -- With the leksah plugin installed (`leksah-cmd plugin install`) every
      -- session — leksah's included — already has the hooks: keep that copy
      -- fresh, and leave them out here, or each event would arrive twice.
      -- The status line is not something a plugin can carry, so it stays.
      when installed $ do
        dir <- leksahPluginInstallDir
        writeLeksahPlugin dir exe script
      writeIfChanged path . encode . object $
        [ "hooks" .= leksahHooks exe script | not installed ]
        <> [ "statusLine" .= statusLine ]
      return (" --settings " <> T.pack path)

-- | The @--settings@ and @--mcp-config@ flags for a session launched on
-- @host@: the files a leksah running there wrote for its own sessions (its
-- hooks call ITS leksah-cmd, which reaches it — or, when it is not running,
-- the machine's event log), or nothing when no leksah has run there.  One ssh
-- round trip; absolute paths, as the host spells them.
remoteAgentFlags :: Text -> IO (Text, Text)
remoteAgentFlags host = (`catch` \(_ :: SomeException) -> return ("", "")) $ do
  (_, out, _) <- runSsh host
    (T.concat [ "for f in \"$HOME/.leksah/settings-statusline.json\" \"$HOME/.leksah/hooks.json\"; "
              , "do [ -f \"$f\" ] && { echo \"S $f\"; break; }; done; "
              , "[ -f \"$HOME/.leksah/mcp.json\" ] && echo \"M $HOME/.leksah/mcp.json\"; "
              , "[ -f \"$HOME/.leksah/bin-dir\" ] && echo \"P $(head -n1 \"$HOME/.leksah/bin-dir\")\"; exit 0" ])
    [] mempty
  let ls = T.lines (T.pack (BS.unpack out))
  return
    -- (the host leksah's leksah-cmd on the agent's PATH: its system prompt
    -- tells it to run @leksah-cmd agent …@, and the host's tmux server need
    -- not have been started from a shell that has it)
    ( T.concat [ "PATH=" <> shq dir <> ":\"$PATH\" "
               | l <- ls, Just dir <- [T.stripPrefix "P " l], not (T.null dir) ]
    , T.concat
        [ case T.stripPrefix "S " l of
            Just f  -> " --settings " <> shq f
            Nothing -> maybe "" ((" --mcp-config " <>) . shq) (T.stripPrefix "M " l)
        | l <- ls ] )

-- | Write the files 'remoteAgentFlags' looks for — this leksah's hooks and
-- status-line settings, its MCP config, and @~\/.leksah\/bin-dir@ (where its
-- @leksah-cmd@ is) — at start-up rather than at its first launch of an agent,
-- so a leksah on ANOTHER machine launching an agent here finds them even
-- when no agent has been launched here yet.
prepareAgentLaunchFiles :: IO ()
prepareAgentLaunchFiles = (`catch` \(_ :: SomeException) -> return ()) $
  findExecutable "leksah-cmd" >>= mapM_ (\exe -> do
    home <- getHomeDirectory
    plugin <- leksahPluginEnabledFor Nothing
    _ <- claudeMcpFlag plugin
    _ <- claudeHooksFlag plugin home
    writeIfChanged (home </> ".leksah" </> "bin-dir")
      (BS.fromStrict (encodeUtf8 (T.pack (takeDirectory exe <> "\n")))))

-- | The two programs the hooks run: @leksah-cmd@ (its path), and the worktree
-- PostToolUse script, which @leksah-cmd hooks write-script@ (re)writes.
-- 'Nothing' without @leksah-cmd@ on PATH, or when the script can't be written.
leksahHookTools :: IO (Maybe (FilePath, FilePath))
leksahHookTools = findExecutable "leksah-cmd" >>= \case
  Nothing  -> return Nothing
  Just exe -> do
    (ec, _, _) <- readProcessWithExitCode exe ["hooks", "write-script"] ""
    home <- getHomeDirectory
    return $ case ec of
      ExitSuccess -> Just (exe, home </> ".leksah" </> "worktree-hook.sh")
      _           -> Nothing

-- | Every Claude Code hook leksah uses — for the @--settings@ file of the
-- sessions it launches, and for the leksah plugin ('writeLeksahPlugin'), which
-- carries the same table to sessions started anywhere.
--
-- The session-lifecycle hooks report to leksah ('IDE.Web.Agent.agentHook'):
-- SessionStart keeps an agent's identity across /clear and /branch, re-briefs
-- it on a resume, registers the worktree it started in, and (with
-- UserPromptSubmit) gives it the title it gave itself; UserPromptSubmit, Stop,
-- Notification and SessionEnd make the status surfaces update at once rather
-- than at the next poll.  Each is `leksah-cmd agent hook EVENT`, which always
-- exits 0 quickly, so a hook can never hold up or fail a session — also when
-- leksah is not running.
--
-- The rest only REPORT, so they run async — they never hold a session up: the
-- tool call running now (PreToolUse / PostToolUse[Failure]) and the files it
-- edited, the approval a session waits for (PermissionRequest), a turn that
-- ended on an API error (StopFailure), and where its shell went (CwdChanged).
-- PreToolUse above all: a synchronous hook there delays EVERY tool call by the
-- hook's run time.  A PostToolUse answer (another agent edited the same file)
-- reaches the session on its next turn.  (Async hooks can arrive out of order;
-- 'recordToolStart' copes.)
--
-- And the worktree PostToolUse hook (@script@): every @git worktree
-- add@\/@remove@ \/ @git switch@ a session runs in Bash, and every worktree it
-- enters or removes with Claude Code's own worktree tools, lands in the
-- worktree registry.
leksahHooks :: FilePath -> FilePath -> Value
leksahHooks exe script = object $
  [ "PostToolUse" .=
      [ object
          [ "matcher" .= ("Bash|EnterWorktree|ExitWorktree" :: Text)
          , "hooks" .=
              [ object [ "type"    .= ("command" :: Text)
                       , "command" .= script
                       , "timeout" .= (10 :: Int) ] ] ]
      , reporter "PostToolUse" ] ]
  -- (UserPromptSubmit is synchronous because its answer can carry the
  -- session's title.)
  <> map lifecycle [ "SessionStart", "UserPromptSubmit", "Stop"
                   , "Notification", "SessionEnd" ]
  <> map (hookOn True)
       -- (PostToolUse is registered above, with the worktree hook.)
       (filter (/= "PostToolUse") perToolHookEvents
        <> [ "PermissionRequest", "StopFailure", "CwdChanged"
           -- Its subagents, and a compaction in progress, for the activity
           -- line.  PreCompact MUST stay async: a synchronous PreCompact
           -- hook's stdout becomes instructions for the compaction summary.
           , "SubagentStart", "SubagentStop", "PreCompact", "PostCompact"
           -- Its task list (Claude Code's Task tools), for the activity line.
           , "TaskCreated", "TaskCompleted" ])
  where
    hookGroup :: Bool -> Text -> Value
    hookGroup async ev = object [ "hooks" .=
      [ object ([ "type"    .= ("command" :: Text)
                , "command" .= (shq (T.pack exe) <> " agent hook " <> ev)
                , "timeout" .= (5 :: Int) ]
                <> [ "async" .= True | async ]) ] ]
    hookOn :: Bool -> Text -> Pair
    hookOn async ev = Key.fromText ev .= [hookGroup async ev]
    lifecycle = hookOn False
    reporter  = hookGroup True

-- | Leksah's MCP server (@leksah-cmd mcp@ — IDE tools: diagnostics, open_file,
-- build, hover, screenshot, the agent tools), as an @mcpServers@ object.
leksahMcpServers :: FilePath -> Value
leksahMcpServers exe = object
  [ "leksah" .= object
      [ "type"    .= ("stdio" :: Text)
      , "command" .= exe
      , "args"    .= (["mcp"] :: [Text]) ] ]

--------------------------------------------------------------------------------
-- The leksah plugin

-- | Write the leksah Claude Code plugin into @dir@: the manifest, every hook
-- ('leksahHooks'), and the MCP server ('leksahMcpServers').  Installed
-- (`leksah-cmd plugin install`) as a skills-directory plugin, it gives EVERY
-- session the same hooks and tools a leksah-launched one gets — `claude` in a
-- plain terminal, the resume picker, a background session — so they all show
-- up, and report, in leksah.  A marker file says leksah wrote it, so leksah
-- only ever rewrites or removes its own.
--
-- Every session on the machine reads these files, and leksah calls this at
-- each launch: each file is replaced atomically, and only when it changed
-- ('writeIfChanged'), so a session starting meanwhile never reads half a file.
writeLeksahPlugin :: FilePath -> FilePath -> FilePath -> IO ()
writeLeksahPlugin dir exe script = do
  createDirectoryIfMissing True (dir </> ".claude-plugin")
  createDirectoryIfMissing True (dir </> "hooks")
  writeIfChanged (dir </> ".claude-plugin" </> "plugin.json") . encode $ object
    [ "name"        .= ("leksah" :: Text)
    , "version"     .= ("1.0.0" :: Text)
    , "description" .= ("Leksah IDE integration: reports every session's state, \
                        \tools, edits, subagents and worktrees to Leksah, and \
                        \gives it Leksah's MCP tools (diagnostics, build, \
                        \open file, agents)." :: Text) ]
  writeIfChanged (dir </> "hooks" </> "hooks.json") . encode $
    object [ "hooks" .= leksahHooks exe script ]
  writeIfChanged (dir </> ".mcp.json") . encode $
    object [ "mcpServers" .= leksahMcpServers exe ]
  writeIfChanged (dir </> leksahPluginMarker)
    "Written by Leksah (leksah-cmd plugin install); refreshed when a leksah \
    \launch of a Claude session finds it out of date.  Remove with: \
    \leksah-cmd plugin uninstall\n"

-- | The files 'writeLeksahPlugin' writes, relative to the plugin directory —
-- all that 'leksahPluginUninstall' removes.
leksahPluginFiles :: [FilePath]
leksahPluginFiles =
  [ ".claude-plugin" </> "plugin.json", "hooks" </> "hooks.json", ".mcp.json"
  , leksahPluginMarker ]

-- | Replace @path@ with @bytes@ — atomically (a temporary file renamed over
-- it), and only when the content differs, so readers never see a partial
-- file and an unchanged file is not touched at all.
writeIfChanged :: FilePath -> BL.ByteString -> IO ()
writeIfChanged path bytes = do
  ex <- doesFileExist path
  same <- if not ex then return False
          else (== BL.toStrict bytes) <$> BS.readFile path
  unless same $ do
    let tmp = path <> ".tmp-leksah"
    BL.writeFile tmp bytes
    renameFile tmp path

-- | Where the installed plugin lives: Claude Code's skills directory, where a
-- plugin loads in every new session as @leksah\@skills-dir@ — no marketplace.
leksahPluginInstallDir :: IO FilePath
leksahPluginInstallDir = do
  conf <- lookupEnv "CLAUDE_CONFIG_DIR" >>= \case
    Just d | not (null d) -> return d
    _ -> (</> ".claude") <$> getHomeDirectory
  return (conf </> "skills" </> "leksah")

leksahPluginMarker :: FilePath
leksahPluginMarker = ".leksah-generated"

-- | Is the leksah plugin installed (leksah's own, by its marker) and not
-- turned off — @enabledPlugins: {"leksah\@skills-dir": false}@ — for a
-- session in @mdir@: the project's @.claude/settings.local.json@, then its
-- @.claude/settings.json@, then the user's settings, the first that says.
-- (Turned off for one project, leksah must give its sessions there the hooks
-- and MCP server itself, or they would have none at all.)
leksahPluginEnabledFor :: Maybe FilePath -> IO Bool
leksahPluginEnabledFor mdir = (`catch` \(_ :: SomeException) -> return False) $ do
  dir <- leksahPluginInstallDir
  ours <- doesFileExist (dir </> leksahPluginMarker)
  if not ours then return False else do
    let conf = takeDirectory (takeDirectory dir)
        files = [ d </> ".claude" </> f | Just d <- [mdir]
                                        , f <- ["settings.local.json", "settings.json"] ]
                <> [conf </> "settings.json"]
    says <- forM files $ \f -> do
      ex <- doesFileExist f
      if not ex then return Nothing else do
        v <- decodeStrict' <$> BS.readFile f
        return (v >>= parseMaybe (withObject "settings" $ \o -> o .: "enabledPlugins"
                    >>= withObject "enabledPlugins" (.: "leksah@skills-dir")))
    return (listToMaybe (catMaybes' says) /= Just False)
  where catMaybes' = foldr (\m acc -> maybe acc (: acc) m) []

-- | @leksah-cmd plugin install@: write the plugin into Claude Code's skills
-- directory.  It loads in sessions started from now on.
leksahPluginInstall :: IO Text
leksahPluginInstall = (`catch` \(e :: SomeException) -> return (T.pack (show e) <> "\n")) $
  leksahHookTools >>= \case
    Nothing -> return "leksah-cmd is not on leksah's PATH, so there is nothing for the plugin's hooks to run.\n"
    Just (exe, script) -> do
      dir <- leksahPluginInstallDir
      ex <- doesDirectoryExist dir
      ours <- doesFileExist (dir </> leksahPluginMarker)
      if ex && not ours
        then return $ T.pack dir <> " exists and is not leksah's: not touching it.\n"
        else do
          writeLeksahPlugin dir exe script
          dup <- duplicateWorktreeHook
          return $ "Installed the leksah plugin in " <> T.pack dir <> ".\n\
            \Claude Code sessions started from now on — in any terminal — report to \
            \leksah and have its MCP tools (sessions already running are unchanged).\n\
            \Turn it off with `/plugin` (leksah@skills-dir), or remove it with \
            \`leksah-cmd plugin uninstall`.\n" <> dup

-- | @leksah-cmd plugin write DIR@: write the plugin to @DIR@ without installing
-- it — for @claude --plugin-dir DIR@ (one session only), or to look at.
leksahPluginWrite :: FilePath -> IO Text
leksahPluginWrite dir = (`catch` \(e :: SomeException) -> return (T.pack (show e) <> "\n")) $
  leksahHookTools >>= \case
    Nothing -> return "leksah-cmd is not on leksah's PATH.\n"
    Just (exe, script) -> do
     -- Never into a directory with other things in it (`plugin write .` in a
     -- project would replace its .mcp.json): a new or empty one, or one
     -- leksah wrote before.
     ex <- doesDirectoryExist dir
     ours <- doesFileExist (dir </> leksahPluginMarker)
     xs <- if ex then listDirectory dir else return []
     if ex && not ours && not (null xs)
      then return $ T.pack dir <> " is not empty and leksah did not write it: \
                   \give a new (or empty) directory.\n"
      else do
        writeLeksahPlugin dir exe script
        return $ "Wrote the leksah plugin to " <> T.pack dir
               <> " (use it for one session with: claude --plugin-dir " <> T.pack dir <> ").\n"

-- | @leksah-cmd plugin uninstall@: remove the plugin leksah installed (and
-- nothing else).
leksahPluginUninstall :: IO Text
leksahPluginUninstall = (`catch` \(e :: SomeException) -> return (T.pack (show e) <> "\n")) $ do
  dir <- leksahPluginInstallDir
  ours <- doesFileExist (dir </> leksahPluginMarker)
  if not ours
    then return "The leksah plugin is not installed.\n"
    else do
      -- Only the files leksah wrote: anything else in that directory is
      -- someone else's, and stays (with the directory, then).
      forM_ leksahPluginFiles $ \f -> do
        let p = dir </> f
        e <- doesFileExist p
        when e (removeFile p)
      forM_ [dir </> "hooks", dir </> ".claude-plugin", dir] $ \d -> do
        e <- doesDirectoryExist d
        when e $ listDirectory d >>= \xs -> when (null xs) (removeDirectory d)
      left <- doesDirectoryExist dir
      return $ "Removed the leksah plugin from " <> T.pack dir
               <> (if left then " (other files there were left alone)" else "")
               <> ".  Sessions started from now on no longer load it; \
                  \leksah-launched ones get leksah's hooks again from leksah itself.\n"

-- | @leksah-cmd plugin status@.
leksahPluginStatus :: IO Text
leksahPluginStatus = do
  dir <- leksahPluginInstallDir
  ours <- doesFileExist (dir </> leksahPluginMarker)
  on <- leksahPluginEnabledFor Nothing
  dup <- if ours then duplicateWorktreeHook else return ""
  return $ (case (ours, on) of
    (False, _)    -> "not installed (leksah-cmd plugin install)\n"
    (True, True)  -> "installed and enabled: " <> T.pack dir <> "\n"
    (True, False) -> "installed but turned off (enabledPlugins: leksah@skills-dir = false): "
                     <> T.pack dir <> "\n") <> dup

-- | A note when the user's settings also carry the worktree hook that
-- @leksah-cmd hooks install@ adds: with the plugin installed it runs twice
-- after every Bash call (harmless, but wasted).  Not removed automatically —
-- it is the user's settings file.
duplicateWorktreeHook :: IO Text
duplicateWorktreeHook = (`catch` \(_ :: SomeException) -> return "") $ do
  conf <- takeDirectory . takeDirectory <$> leksahPluginInstallDir
  let f = conf </> "settings.json"
  ex <- doesFileExist f
  has <- if ex then ("worktree-hook.sh" `BS.isInfixOf`) <$> BS.readFile f else return False
  return $ if not has then "" else
    "Note: " <> T.pack f <> " also runs leksah's worktree hook (from `leksah-cmd \
    \hooks install`); the plugin has it too, so you can remove that entry.\n"

-- | The @statusLine@ setting for the sessions leksah launches: @leksah-cmd
-- statusline@, which hands the status-line JSON (context use, cost, rate
-- limits — "IDE.Web.AgentUsage") to leksah and then runs the user's OWN
-- status-line command and prints what it prints.  A @--settings@ file
-- outranks the user's and project's settings, so without that wrapper this
-- would replace their status line.  Its @padding@ and @refreshInterval@ are
-- carried over from the same setting the wrapper will run the command of
-- ('claudeUserStatusLine').
leksahStatusLine :: FilePath -> Maybe Object -> Value
leksahStatusLine exe mine = object $
  [ "type"    .= ("command" :: Text)
  , "command" .= (shq (T.pack exe) <> " statusline") ]
  <> [ Key.fromText k .= v | Just o <- [mine], k <- statusLineCarried
                           , Just v <- [KM.lookup (Key.fromText k) o] ]

statusLineCarried :: [Text]
statusLineCarried = ["padding", "refreshInterval"]

-- | Where to write the settings file with this @statusLine@: one file per
-- distinct carried-over @padding@\/@refreshInterval@ (nearly always just
-- @hooks.json@), so two sessions launched together in projects with
-- different settings can't overwrite each other's file before their
-- @claude@ has read it.
statusLineSettingsFile :: Value -> FilePath
statusLineSettingsFile v = case tag of
    "" -> "hooks.json"
    t  -> "hooks-" <> t <> ".json"
  where
    tag = concat [ T.unpack k <> filter isAlphaNum (BS.unpack (BL.toStrict (encode x)))
                 | Object o <- [v], k <- statusLineCarried
                 , Just x <- [KM.lookup (Key.fromText k) o] ]

-- | The user's own @statusLine@ setting for a session in @dir@ — exactly the
-- one @leksah-cmd statusline@ will run the command of, because it asks it
-- (@statusline --resolve DIR@: project settings only in a trusted project,
-- nothing under @disableAllHooks@, then local > project > user).
claudeUserStatusLine :: FilePath -> FilePath -> IO (Maybe Object)
claudeUserStatusLine exe dir = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  (ec, out, _) <- readProcessWithExitCode exe ["statusline", "--resolve", dir] ""
  return $ case ec of
    ExitSuccess -> decodeStrict' (encodeUtf8 (T.pack out))
    _           -> Nothing

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
    return (latestActivityOf tailLines)

-- | 'claudeLatestActivity' over a transcript tail's lines (from anywhere).
latestActivityOf :: [BS.ByteString] -> Maybe Text
latestActivityOf tailLines =
    listToMaybe . mapMaybe (\l -> decodeStrict' l >>= parseMaybe lineP)
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
        return (describeToolUse name inp)
      "thinking" -> return "Thinking"
      _          -> return "Writing"

-- | What a session's transcript says about it right now, for the Agents pane
-- and every list of sessions: what it is about and what it last said.
data TranscriptGist = TranscriptGist
  { tgTitle    :: !(Maybe Text) -- ^ Claude Code's own title for the conversation
                              --   (its latest @ai-title@ line — rewritten as the
                              --   conversation moves on); Codex has none
  , tgFirstTitle :: !(Maybe (Text, Maybe UTCTime))
                              -- ^ the transcript's FIRST title, and roughly when
                              --   it was written (the timestamp of the last line
                              --   before it: its own line has none).  A fork's
                              --   transcript OPENS with the title it copied, before
                              --   any message (no timestamp before it), and Claude
                              --   Code goes on re-writing that title
  , tgHeadDone :: !Bool       -- ^ the head has been searched for good: the first
                              --   title was found, or the file is past the bound
                              --   the search reads, so it never will be
  , tgLastSaid :: !(Maybe Text) -- ^ the first line of prose in the last thing the
                              --   agent said (not a tool call, not thinking)
  } deriving (Eq, Show)

noGist :: TranscriptGist
noGist = TranscriptGist Nothing Nothing False Nothing

-- | The 'TranscriptGist' of a transcript (a Claude Code @.jsonl@, or a Codex
-- rollout), from its last 256KB: both lines are appended every turn, so the
-- latest are near the end.  @prev@, an earlier gist of the same file, saves
-- searching the head again once its first title is settled ('tgHeadDone').
-- Its fields are strict, and their texts forced here: a lazy one would keep
-- the whole 256KB tail it was parsed from alive in a cache.
-- Use 'cachedTranscriptGist'.
transcriptGist :: Maybe TranscriptGist -> FilePath -> IO TranscriptGist
transcriptGist prev path
  | isRollout path = (`catch` \(_ :: SomeException) -> return noGist) $
      rolloutTurns path >>= \turns -> return $! forceGist noGist
        { tgLastSaid = listToMaybe (mapMaybe proseLine (reverse [ t | (False, t) <- turns ])) }
  | otherwise = (`catch` \(_ :: SomeException) -> return noGist) $ do
    tailLines <- withFile path ReadMode $ \h -> do
      size <- hFileSize h
      let back = min size (256 * 1024)
      hSeek h SeekFromEnd (negate back)
      BS.lines <$> BS.hGet h (fromIntegral back)
    -- The first title, from the head: Claude Code titles a conversation after
    -- its first exchange (and a fork's copy opens with it), so it is near the
    -- top.  Searched in the first 1MB only, and once found never again.
    (first, done) <- case prev of
      Just g | tgHeadDone g -> return (tgFirstTitle g, True)
      _ -> withFile path ReadMode $ \h -> do
        size <- hFileSize h
        headLines <- BS.lines <$> BS.hGetSome h headBound
        let go _ [] = Nothing
            go ts (l : ls) = case titleOf l of
              Just t  -> Just (t, ts)
              Nothing -> go (stampOf l <|> ts) ls
            found = go Nothing (if size > fromIntegral headBound
                                  then initSafe headLines else headLines)
        return (found, isJust found || size > fromIntegral headBound)
    return $! forceGist (gistOfTail tailLines) { tgFirstTitle = first, tgHeadDone = done }
  where
    headBound = 1024 * 1024
    -- (a line the bound cut in two parses as nothing anyway; drop it)
    initSafe xs = if null xs then xs else init xs
    titleOf l
      | "\"ai-title\"" `BS.isInfixOf` l =
          mfilter (not . T.null) . fmap T.strip $
            decodeStrict' l >>= parseMaybe (withObject "l" (.: "aiTitle"))
      | otherwise = Nothing
    stampOf l = do
      let (_, rest) = BS.breakSubstring "\"timestamp\":\"" l
      if BS.null rest then Nothing else
        iso8601ParseM (BS.unpack (BS.takeWhile (/= '"') (BS.drop 13 rest)))

-- | The title and the last thing said, from a transcript tail's lines (read
-- here, or fetched from another machine): no first title, which needs the
-- head ('transcriptGist').
gistOfTail :: [BS.ByteString] -> TranscriptGist
gistOfTail tailLines = forceGist noGist
    { tgTitle    = latest (withObject "l" (.: "aiTitle")) "\"ai-title\""
                     >>= mfilter (not . T.null) . Just . T.strip
    , tgLastSaid = latest saidP "\"type\":\"assistant\"" }
  where
    latest :: (Value -> Parser Text) -> BS.ByteString -> Maybe Text
    latest p mark = listToMaybe . mapMaybe (\l -> decodeStrict' l >>= parseMaybe p)
                      . reverse $ filter (mark `BS.isInfixOf`) tailLines
    -- An assistant line of the main conversation carrying prose (a subagent's
    -- are sidechain lines; tool calls and thinking are not "said").
    saidP = withObject "line" $ \o -> do
      t  <- o .: "type"
      sc <- o .:? "isSidechain"
      if t /= ("assistant" :: Text) || sc == Just True then fail "not said" else do
        m  <- o .: "message"
        cs <- m .: "content"
        case [ x | Object c <- reverse (cs :: [Value])
                 , Just ("text" :: Text) <- [parseMaybe (.: "type") c]
                 , Just x <- [parseMaybe (.: "text") c] ] >>= maybe [] pure . proseLine of
          (x : _) -> return x
          []      -> fail "no prose"

forceGist :: TranscriptGist -> TranscriptGist
forceGist g = T.length (fromMaybe "" (tgTitle g)) `seq`
              T.length (fromMaybe "" (tgLastSaid g)) `seq`
              maybe () (\(t, ts) -> T.length t `seq` ts `seq` ()) (tgFirstTitle g) `seq` g

-- | The first line of prose in a message, as plain text — the card shows it
-- raw, so markdown's code ticks, emphasis and heading, bullet and quote marks
-- would show as noise, and a code fence, a rule or a table row is not prose
-- at all.  'Nothing' when there is none.
proseLine :: Text -> Maybe Text
proseLine msg = listToMaybe
  [ T.take 200 p
  | l <- notFenced (T.lines msg)
  , let s = T.strip l
  , not (T.all (`elem` ("-=*_|: " :: String)) s)   -- rules, table rules, blanks
  , not ("|" `T.isPrefixOf` s)                       -- table rows
  , let p = T.strip (plain s)
  , not (T.null p) ]
  where
    plain = T.replace "**" "" . T.filter (/= '`') . T.dropWhile (`elem` ("#-*> " :: String))
    -- Drop fenced code blocks: everything from a ``` line to the next.
    notFenced [] = []
    notFenced (l : ls)
      | "```" `T.isPrefixOf` T.strip l = notFenced (drop 1 (dropWhile (not . fence) ls))
      | otherwise = l : notFenced ls
    fence = ("```" `T.isPrefixOf`) . T.strip

-- | 'transcriptGist', cached per file by modification time (one stat per call
-- otherwise).  A re-read that finds no title or nothing said keeps the last
-- ones it had: a large tool result can push them out of the tail it reads,
-- and the card would otherwise flick back to the first prompt while the
-- agent works.
cachedTranscriptGist :: FilePath -> IO TranscriptGist
cachedTranscriptGist p = (`catch` \(_ :: SomeException) -> maybe noGist snd . M.lookup p <$> readIORef gistCache) $ do
  t <- getModificationTime p
  prev <- M.lookup p <$> readIORef gistCache
  case prev of
    Just (t', g) | t' == t -> return g
    _ -> do
      g <- transcriptGist (snd <$> prev) p
      let g' = case snd <$> prev of
            Nothing -> g
            Just o  -> forceGist g { tgTitle    = tgTitle g <|> tgTitle o
                                   , tgLastSaid = tgLastSaid g <|> tgLastSaid o }
      -- (Bounded: every live agent and the exited ones the Agents pane shows
      -- fit many times over; past that, start again.)
      atomicModifyIORef' gistCache $ \m ->
        (M.insert p (t, g') (if M.size m > 500 then M.empty else m), ())
      return g'

{-# NOINLINE gistCache #-}
gistCache :: IORef (Map FilePath (UTCTime, TranscriptGist))
gistCache = unsafePerformIO (newIORef M.empty)

-- | Where a live session's transcript is: its Codex rollout, or the Claude
-- Code transcript for its directory.
liveTranscriptPath :: ClaudeLive -> IO (Maybe FilePath)
liveTranscriptPath l
  | clAgent l == "codex" = codexTranscriptOf (clSession l)
  | otherwise            = Just <$> claudeTranscriptPath (clDir l) (clSession l)

-- | A tool call in a few words — @Bash: cabal build@, @Edit Main.hs@,
-- @Agent: review the diff@ — for the Agents pane's activity line (from a
-- transcript, or as a hook reports it).
describeToolUse :: Text -> Object -> Text
describeToolUse name inp =
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
  where
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
showTmuxPane = showTmuxPaneWith False

-- | 'showTmuxPane', or with @True@: bring the tab to the active OS window
-- ('requestBringTerm') instead of raising the window that holds it.
showTmuxPaneWith :: Bool -> Text -> Text -> Text -> IO ()
showTmuxPaneWith here s w p = do
  tmuxCmd ["select-window", "-t", T.unpack w]
  tmuxCmd ["select-pane", "-t", T.unpack p]
  mlw <- lwForTmuxWindow w
  (if here then requestBringTerm else requestLocalTerm) (fromMaybe s mlw)

-- | Bring up the tab holding tmux pane @p@: the leksah window owning its
-- window, else its session's tab.  Not the session's tab outright — tabs
-- share sessions (every agent's is on the Agents session), so that would be
-- whichever tab the session reaches first.
raiseTabOfPane :: Text -> IO ()
raiseTabOfPane p = do
  ids <- fmap (T.splitOn "\t" . T.strip) <$>
           tmuxOut ["display-message", "-p", "-t", T.unpack p, "#{session_id}\t#{window_id}"]
  case ids of
    Just [s, w] | not (T.null s) -> lwForTmuxWindow w >>= requestLocalTerm . fromMaybe s
    _                            -> return ()

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
-- (The browser demo has no claude processes; 'demoShowAgent' finds a canned
-- agent's pane by name there, and is 'Nothing' natively.)
showLiveSession = showLiveSessionWith False

-- | 'showLiveSession', bringing the pane's tab to the active OS window (the
-- one clicked in) rather than raising the window that holds it.
showLiveSessionHere :: Text -> IO Bool
showLiveSessionHere = showLiveSessionWith True

showLiveSessionWith :: Bool -> Text -> IO Bool
showLiveSessionWith here sid = demoShowAgent sid >>= \case
  Just shown -> return shown
  Nothing -> nativeShowLiveSession here sid

nativeShowLiveSession :: Bool -> Text -> IO Bool
nativeShowLiveSession here sid = paneForSession sid >>= \case
    Just (s, w, p) -> showTmuxPaneWith here s w p >> return True
    Nothing -> claudeLiveBySession >>= \live -> case M.lookup sid live of
      Just l | not (null (clDir l)) -> mruClaudeUnclaimed here sid (clDir l)
      _                             -> background
  where
    -- A Claude Code background session with no process (blocked, say: the
    -- status dot and menu-bar item list it as needing you) has no pane to
    -- show: take it up in one — `claude --resume` resumes a background
    -- session in the foreground.
    background = claudeBackgroundSessions >>= \bgs ->
      -- Only a BLOCKED one — the kind the status surfaces list as needing
      -- you; a finished or failed one is not "shown" by starting it again.
      case [ b | b <- bgs, cbSession b == sid, cbState b == "blocked"
               , isNothing (cbPid b), not (null (cbDir b)) ] of
        (b : _) -> runClaudeCmd (ClaudeResume (cbDir b) sid) >> return True
        []      -> return False

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
mruClaudeUnclaimed :: Bool -> Text -> FilePath -> IO Bool
mruClaudeUnclaimed here sid dir = do
  let base = T.pack (dropTrailingPathSeparator dir) <> "#claude"
  panes <- filter (\(k, _, _, _) -> claudeKeyFor base k) <$> liveRunPanes
  case panes of
    [] -> return False
    ((_, s, w, p) : _) -> sessionForPaneId p >>= \case
      Just other | other /= sid -> return False
      _                         -> showTmuxPaneWith here s w p >> return True

-- | The tmux @(session id, window id, pane id)@ whose process tree owns LIVE
-- session @sid@ — the exact identification 'showLiveSession' documents, factored
-- out so senders can aim at a session without also navigating to it.
-- 'Nothing' when the session isn't running in a pane of leksah's tmux server
-- (not running at all, on a remote host, or in a terminal outside leksah).
paneForSession :: Text -> IO (Maybe (Text, Text, Text))
paneForSession sid = do
  ps <- claudePaneSessions
  return $ listToMaybe [ pane | (pane, l) <- ps, clSession l == sid ]

-- | The live session running in tmux pane @pid@ (@%7@), if any — the reverse of
-- 'paneForSession', for asking "is THIS pane a Claude session?".  A pane whose
-- own session is the obvious AI target needs no stored binding.
sessionForPaneId :: Text -> IO (Maybe Text)
sessionForPaneId pid = do
  ps <- claudePaneSessions
  return $ listToMaybe [ clSession l | ((_, _, p), l) <- ps, p == pid ]

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
    ["-L", tmuxServerSocket, "display-message", "-p", "-t", T.unpack pane
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
  codexTranscriptOf sid >>= \case
    Just r  -> rolloutFirstPrompt r          -- a Codex session
    Nothing -> claudeTranscriptPath dir sid >>= fmap snd . scanHead

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
