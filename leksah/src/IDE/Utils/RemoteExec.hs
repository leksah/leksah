{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Remote filesystem\/exec primitives over ssh — the ONLY module that
-- invokes ssh for @ssh:\/\/HOST\/…@ paths (see "IDE.Utils.RemotePath").
--
-- Every operation is a one-shot @ssh HOST \"sh -c 'SCRIPT' args…\"@.  With
-- the user's @ControlMaster auto@\/@ControlPersist@ pooling, each exec is
-- ~1 round trip (a mux channel open, no key exchange), so the latency
-- discipline is about COUNTING round trips, not avoiding ssh: batch
-- multi-step operations into one script ('remoteCabalSnapshot'), and never
-- poll.  Per-exec timings go to the debug log so a future persistent
-- pipelined channel (same signatures, one long-lived @ssh host sh@) can be
-- justified by data.
--
-- No ControlMaster options are passed — pooling is the user's ssh config's
-- business.  @BatchMode=yes@ means we never trigger an interactive auth
-- prompt from inside the IDE.
module IDE.Utils.RemoteExec
  ( RemoteError(..)
  , runSsh
  , shellQuote
  , remoteReadFile
  , remoteWriteFile
  , remoteFileExists
  , remoteDirExists
  , remoteListDirectoryAnnotated
  , remoteListFilesRecursive
  , remoteCreateDirectoryIfMissing
  , remoteHomeDir
  , resolveProjectInput
  , SnapshotEntry(..)
  , remoteCabalSnapshot
  -- * remote command runs (builds, repls) — see IDE.Utils.ExternalTool
  , newRunNonce
  , remoteRunScript
  , remoteSshArgs
  , interruptRemoteRun
  -- * activity indicator feed
  , remoteInFlight
  , remoteInFlightChanged
  , dupRemoteInFlight
  ) where

import Control.Concurrent.STM (TChan, TVar)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Map as M (Map)
import System.Exit (ExitCode(..))

import IDE.Utils.RemotePath

#if defined(ghcjs_HOST_OS)

import Control.Concurrent.STM
       (atomically, dupTChan, newBroadcastTChanIO, newTVarIO)
import Control.Exception (throwIO)
import qualified Data.Map as M (empty)
import GHC.IO (unsafePerformIO)
import System.IO.Error (mkIOError, doesNotExistErrorType)

-- The browser demo has no ssh; remote paths never arise there (the mock FS
-- seeds only local-looking paths), so these are unreachable stubs that keep
-- the module list identical across the native and JS builds.

data RemoteError = RemoteConnectError Text Text
                 | RemoteOpFailed Text Int Text
  deriving Show

noRemote :: IO a
noRemote = throwIO (mkIOError doesNotExistErrorType
                      "remote paths are not supported in the browser demo"
                      Nothing Nothing)

runSsh :: Text -> Text -> [Text] -> ByteString -> IO (ExitCode, ByteString, ByteString)
runSsh _ _ _ _ = noRemote

shellQuote :: Text -> Text
shellQuote = id

remoteReadFile :: Text -> FilePath -> IO ByteString
remoteReadFile _ _ = noRemote
remoteWriteFile :: Text -> FilePath -> ByteString -> IO ()
remoteWriteFile _ _ _ = noRemote
remoteFileExists :: Text -> FilePath -> IO Bool
remoteFileExists _ _ = noRemote
remoteDirExists :: Text -> FilePath -> IO Bool
remoteDirExists _ _ = noRemote
remoteListDirectoryAnnotated :: Text -> FilePath -> IO [(FilePath, Bool)]
remoteListDirectoryAnnotated _ _ = noRemote
remoteListFilesRecursive :: Text -> FilePath -> IO [FilePath]
remoteListFilesRecursive _ _ = noRemote
remoteCreateDirectoryIfMissing :: Text -> FilePath -> IO ()
remoteCreateDirectoryIfMissing _ _ = noRemote
remoteHomeDir :: Text -> IO FilePath
remoteHomeDir _ = noRemote

resolveProjectInput :: FilePath -> Text -> IO (Either Text FilePath)
resolveProjectInput _ t = return (Left ("Remote paths are not supported here: " <> t))

data SnapshotEntry = SnapshotEntry
  { seLocalPath :: FilePath
  , seBytes     :: ByteString
  } deriving Show

remoteCabalSnapshot :: Text -> FilePath -> [FilePath] -> IO [SnapshotEntry]
remoteCabalSnapshot _ _ _ = noRemote

newRunNonce :: IO Text
newRunNonce = noRemote
remoteRunScript :: Maybe Text -> FilePath -> Text -> FilePath -> [Text] -> Text
remoteRunScript _ _ _ _ _ = ""
remoteSshArgs :: Text -> Text -> (FilePath, [Text])
remoteSshArgs _ _ = ("ssh", [])
interruptRemoteRun :: Text -> Text -> IO ()
interruptRemoteRun _ _ = noRemote

{-# NOINLINE remoteInFlight #-}
remoteInFlight :: TVar (M.Map Text Int)
remoteInFlight = unsafePerformIO (newTVarIO M.empty)

-- | BROADCAST, not a queue: every OS window's statusbar has to see every
-- transition.  A plain 'Chan' would hand each poke to exactly one window's
-- drain and leave the other statusbars stale — see 'dupRemoteInFlight'.
{-# NOINLINE remoteInFlightChanged #-}
remoteInFlightChanged :: TChan ()
remoteInFlightChanged = unsafePerformIO newBroadcastTChanIO

-- | One window's reading copy of the activity feed.
dupRemoteInFlight :: IO (TChan ())
dupRemoteInFlight = atomically (dupTChan remoteInFlightChanged)

#else

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
       (atomically, dupTChan, newBroadcastTChanIO, writeTChan)
import Control.Concurrent.MVar
       (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.QSem
       (QSem, newQSem, waitQSem, signalQSem)
import Control.Concurrent.STM
       (atomically, modifyTVar', newTVarIO)
import Control.Exception
       (Exception, SomeException, bracket_, evaluate, throwIO, try)
import Control.Monad (void, when)
import qualified Data.ByteString as BS
       (drop, hGetContents, hPut, length, null, take)
import qualified Data.ByteString.Char8 as BSC
       (break, dropWhile, lines, readInt, unpack)
import qualified Data.Map as M
       (alter, empty, insert, lookup)
import qualified Data.Text as T
       (concatMap, pack, singleton, unpack, unwords)
import qualified Data.Text.Encoding as TE (decodeUtf8With)
import qualified Data.Text.Encoding.Error as TEE (lenientDecode)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time.Clock
       (diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO (unsafePerformIO)
import Numeric (showHex)
import System.FilePath ((</>))
import System.IO (hClose, hSetBinaryMode)
import System.IO.Error (mkIOError, doesNotExistErrorType)
import System.Log.Logger (debugM)
import System.Process
       (StdStream(..), createProcess, proc, std_err, std_in, std_out,
        waitForProcess)
import Data.Word (Word64)

-- | ssh\/remote failures that are NOT ordinary tool exit codes.
--
-- ssh reserves exit 255 for its own failures (DNS, route, auth, timeout);
-- our scripts reserve exit 44 for \"path does not exist\", which is
-- rethrown as a standard does-not-exist 'IOError' so existing callers'
-- catches keep working.  Everything else surfaces as 'RemoteOpFailed'.
data RemoteError
  = RemoteConnectError Text Text  -- ^ host, ssh stderr
  | RemoteOpFailed Text Int Text  -- ^ host, exit code, stderr
  deriving Show

instance Exception RemoteError

-- | Exit code our remote scripts use for \"does not exist\".
notFoundExit :: Int
notFoundExit = 44

-- | POSIX single-quote escaping — the same algorithm the terminal ssh path
-- uses (Widget\/Terminal.hs @sshTmux@); shared here so every remote quoting
-- site can converge on one definition.
shellQuote :: Text -> Text
shellQuote s = "'" <> T.concatMap esc s <> "'"
  where
    esc '\'' = "'\\''"
    esc c    = T.singleton c

-- Per-host session semaphores: ControlMaster multiplexes every exec over
-- one TCP connection, but each exec is still an ssh session; sshd caps
-- sessions per connection (MaxSessions, default 10).  Cap our concurrency
-- below that so an unrelated interactive session never gets refused.
{-# NOINLINE hostSems #-}
hostSems :: MVar (M.Map Text QSem)
hostSems = unsafePerformIO (newMVar M.empty)

hostSem :: Text -> IO QSem
hostSem host = modifyMVar hostSems $ \m ->
  case M.lookup host m of
    Just s  -> return (m, s)
    Nothing -> do
      s <- newQSem 6
      return (M.insert host s m, s)

-- | Hosts with remote operations currently in flight (host → count) — feeds
-- the statusbar activity indicator; 'remoteInFlightChanged' is poked on
-- every transition.
{-# NOINLINE remoteInFlight #-}
remoteInFlight :: TVar (M.Map Text Int)
remoteInFlight = unsafePerformIO (newTVarIO M.empty)

-- | BROADCAST, not a queue: every OS window's statusbar has to see every
-- transition.  A plain 'Chan' would hand each poke to exactly one window's
-- drain and leave the other statusbars stale — see 'dupRemoteInFlight'.
{-# NOINLINE remoteInFlightChanged #-}
remoteInFlightChanged :: TChan ()
remoteInFlightChanged = unsafePerformIO newBroadcastTChanIO

-- | One window's reading copy of the activity feed.
dupRemoteInFlight :: IO (TChan ())
dupRemoteInFlight = atomically (dupTChan remoteInFlightChanged)

trackInFlight :: Text -> IO a -> IO a
trackInFlight host = bracket_ (bump 1) (bump (-1))
  where
    bump d = do
      atomically . modifyTVar' remoteInFlight $ M.alter (upd d) host
      atomically (writeTChan remoteInFlightChanged ())
    upd d mb = let n = maybe 0 id mb + d in if n <= 0 then Nothing else Just n

-- | Run @sh -c SCRIPT arg0 args…@ on the host with the given stdin, and
-- return (exit code, stdout, stderr).  The args bind to @$0@, @$1@, … in
-- the script, each single-quoted locally so the remote login shell passes
-- them through verbatim.  This is the raw primitive — no error taxonomy
-- applied; use 'checked' \/ the higher-level ops for that.
runSsh :: Text -> Text -> [Text] -> ByteString -> IO (ExitCode, ByteString, ByteString)
runSsh host script args input = do
  sem <- hostSem host
  bracket_ (waitQSem sem) (signalQSem sem) . trackInFlight host $ do
    t0 <- getCurrentTime
    let remoteCmd = T.unpack . T.unwords $
          "sh" : "-c" : shellQuote script : map shellQuote args
        sshArgs = [ "-o", "BatchMode=yes", "-o", "ConnectTimeout=10"
                  , T.unpack host, remoteCmd ]
    (Just hin, Just hout, Just herr, ph) <- createProcess (proc "ssh" sshArgs)
      { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    hSetBinaryMode hin True
    hSetBinaryMode hout True
    hSetBinaryMode herr True
    -- Write stdin from a separate thread: a large payload would deadlock
    -- against an unread stdout pipe otherwise.
    _ <- forkIO $ (BS.hPut hin input >> hClose hin)
                    `catchAny` \_ -> return ()
    outV <- newEmptyMVar
    _ <- forkIO $ (BS.hGetContents hout >>= putMVar outV)
                    `catchAny` \_ -> putMVar outV mempty
    err <- BS.hGetContents herr
    out <- takeMVar outV
    code <- waitForProcess ph
    t1 <- getCurrentTime
    debugM "leksah" $ "RemoteExec: " <> T.unpack host <> " "
      <> show (round (diffUTCTime t1 t0 * 1000) :: Int) <> "ms "
      <> show code <> " sh -c " <> take 500 (T.unpack script)
    return (code, out, err)
  where
    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny a h = either h return =<< try a

-- | Apply the error taxonomy to a finished exec.
checked :: Text -> String -> FilePath -> (ExitCode, ByteString, ByteString)
        -> IO ByteString
checked host op p (code, out, err) = case code of
  ExitSuccess -> return out
  ExitFailure 255 -> throwIO (RemoteConnectError host (decodeErr err))
  ExitFailure c
    | c == notFoundExit ->
        throwIO (mkIOError doesNotExistErrorType op Nothing (Just p))
    | otherwise -> throwIO (RemoteOpFailed host c (decodeErr err))

decodeErr :: ByteString -> Text
decodeErr = TE.decodeUtf8With TEE.lenientDecode

-- | Read a remote file (1 round trip).
remoteReadFile :: Text -> FilePath -> IO ByteString
remoteReadFile host p =
  checked host "readFile" p =<<
    runSsh host "test -f \"$0\" || exit 44; exec cat -- \"$0\"" [T.pack p] mempty

-- | Write a remote file atomically (1 round trip): the bytes go over stdin
-- into a temp file in the target directory, permissions are copied
-- best-effort (GNU @chmod --reference@; silently skipped elsewhere), then
-- @mv -f@ renames into place — a dropped connection mid-write can never
-- truncate the target.
remoteWriteFile :: Text -> FilePath -> ByteString -> IO ()
remoteWriteFile host p bytes =
  void . checked host "writeFile" p =<< runSsh host script [T.pack p] bytes
  where
    script =
      "d=$(dirname -- \"$0\") || exit 45; "
      <> "t=$(mktemp \"$d/.leksah.XXXXXX\") || exit 45; "
      <> "cat > \"$t\" || { rm -f \"$t\"; exit 46; }; "
      <> "[ -e \"$0\" ] && chmod --reference=\"$0\" \"$t\" 2>/dev/null; "
      <> "mv -f -- \"$t\" \"$0\""

existsWith :: Text -> Text -> FilePath -> IO Bool
existsWith flag host p = do
  (code, _, err) <- runSsh host ("test " <> flag <> " \"$0\"") [T.pack p] mempty
  case code of
    ExitSuccess     -> return True
    ExitFailure 1   -> return False
    ExitFailure 255 -> throwIO (RemoteConnectError host (decodeErr err))
    ExitFailure c   -> throwIO (RemoteOpFailed host c (decodeErr err))

remoteFileExists :: Text -> FilePath -> IO Bool
remoteFileExists = existsWith "-f"

remoteDirExists :: Text -> FilePath -> IO Bool
remoteDirExists = existsWith "-d"

-- | Immediate children of a remote directory with an is-directory flag, in
-- ONE round trip (the file tree's per-child @doesDirectoryExist@ would
-- otherwise be N+1 round trips).
remoteListDirectoryAnnotated :: Text -> FilePath -> IO [(FilePath, Bool)]
remoteListDirectoryAnnotated host p = do
  out <- checked host "getDirectoryContents" p =<< runSsh host script [T.pack p] mempty
  return [ (BSC.unpack (BS.drop 2 l), BS.take 1 l == "d")
         | l <- BSC.lines out, BS.length l > 2 ]
  where
    script =
      "test -d \"$0\" || exit 44; cd -- \"$0\" || exit 44; "
      <> "for f in * .[!.]* ..?*; do "
      <> "[ -e \"$f\" ] || [ -L \"$f\" ] || continue; "
      <> "if [ -d \"$f\" ]; then printf 'd %s\\n' \"$f\"; "
      <> "else printf 'f %s\\n' \"$f\"; fi; "
      <> "done"

-- | Every file under a remote directory, any depth (1 round trip).  Paths
-- come back absolute (remote-local); the caller re-prefixes the host.
remoteListFilesRecursive :: Text -> FilePath -> IO [FilePath]
remoteListFilesRecursive host p = do
  out <- checked host "listFilesRecursive" p =<<
    runSsh host "test -d \"$0\" || exit 44; exec find \"$0\" -type f" [T.pack p] mempty
  return (map BSC.unpack (BSC.lines out))

remoteCreateDirectoryIfMissing :: Text -> FilePath -> IO ()
remoteCreateDirectoryIfMissing host p =
  void . checked host "createDirectoryIfMissing" p =<<
    runSsh host "exec mkdir -p -- \"$0\"" [T.pack p] mempty

{-# NOINLINE homeCache #-}
homeCache :: MVar (M.Map Text FilePath)
homeCache = unsafePerformIO (newMVar M.empty)

-- | The remote @$HOME@, cached per host (used once per host to expand a
-- leading @~@ in user input).
remoteHomeDir :: Text -> IO FilePath
remoteHomeDir host = do
  cached <- modifyMVar homeCache $ \m -> return (m, M.lookup host m)
  case cached of
    Just h  -> return h
    Nothing -> do
      out <- checked host "homeDir" "~" =<<
        runSsh host "printf %s \"$HOME\"" [] mempty
      let h = BSC.unpack out
      when (null h) $ throwIO (RemoteOpFailed host 0 "empty $HOME")
      modifyMVar homeCache $ \m -> return (M.insert host h m, ())
      return h

-- | Turn user input (scp-style @host:path@, @ssh:\/\/…@, or a local path)
-- into the canonical stored form: @ssh:\/\/host\/abs\/path@ with @~@
-- expanded, or an absolute local path (joined onto @cwd@ when relative).
resolveProjectInput :: FilePath -> Text -> IO (Either Text FilePath)
resolveProjectInput cwd input = case parseProjectInput input of
  Left e -> return (Left e)
  Right (LocalInput p)
    | fsIsAbsolute p -> return (Right p)
    | otherwise      -> return (Right (cwd </> p))
  Right (RemoteInput host p) -> do
    r <- try $ case p of
      "~"                 -> remoteHomeDir host
      '~':'/':rest        -> (</> rest) <$> remoteHomeDir host
      _                   -> return p
    return $ case r of
      Left (e :: SomeException) ->
        Left ("Cannot resolve " <> input <> ": " <> T.pack (show e))
      Right abs' -> Right (renderRemotePath host abs')

-- | One file captured by 'remoteCabalSnapshot'.
data SnapshotEntry = SnapshotEntry
  { seLocalPath :: FilePath   -- ^ path on the remote host (absolute)
  , seBytes     :: ByteString
  } deriving Show

-- | The project-open hot path: in ONE round trip, expand each
-- entry's @*.cabal@ shell glob relative to the project directory and stream
-- back every matching @.cabal@ file AND its sibling @.lkshf@ flag file,
-- length-prefix framed.  Shell globs match the local 'Glob' semantics for
-- the plain @*@ patterns cabal @packages:@ stanzas use in practice; @**@
-- globs are a documented limitation (a @find@-based variant can follow).
remoteCabalSnapshot :: Text -> FilePath -> [FilePath] -> IO [SnapshotEntry]
remoteCabalSnapshot host dir entries = do
  out <- checked host "cabalSnapshot" dir =<<
    runSsh host script (T.pack dir : map (T.pack . (</> "*.cabal")) entries) mempty
  evaluate (parseFrames out)
  where
    script =
      -- $0 is the project dir; the patterns arrive as $1… ($@ never
      -- includes $0, so no shift).
      "cd -- \"$0\" || exit 44; "
      <> "emit() { [ -f \"$1\" ] || return 0; s=$(wc -c < \"$1\") || return 0; "
      <> "printf 'F %s %s\\n' $((s+0)) \"$1\"; cat -- \"$1\"; }; "
      <> "for pat in \"$@\"; do "
      <> "for m in $pat; do "
      <> "emit \"$m\"; "
      <> "case \"$m\" in *.cabal) emit \"${m%.cabal}.lkshf\";; esac; "
      <> "done; "
      <> "done"
    parseFrames bs
      | BS.null bs = []
      | otherwise = case BSC.break (== '\n') bs of
          (header, rest0)
            | BS.null rest0 -> []   -- truncated tail; ignore
            | otherwise ->
                let rest = BS.drop 1 rest0
                in case parseHeader header of
                     Nothing -> []  -- malformed; stop rather than misparse
                     Just (size, path) ->
                       SnapshotEntry (dir </> cleanRel path) (BS.take size rest)
                         : parseFrames (BS.drop size rest)
    -- Globs like "./*.cabal" match as "./x.cabal"; drop the leading "./"
    -- segments so the joined path has no "/./" inside (it becomes a map key
    -- and an editor tab key).
    cleanRel ('.':'/':r) = cleanRel r
    cleanRel r           = r
    parseHeader h = do
      rest <- if BS.take 2 h == "F " then Just (BS.drop 2 h) else Nothing
      (size, rest') <- BSC.readInt rest
      let path = BSC.unpack (BSC.dropWhile (== ' ') rest')
      if null path then Nothing else Just (size, path)

{-# NOINLINE nonceCounter #-}
nonceCounter :: IORef Word64
nonceCounter = unsafePerformIO (newIORef 0)

-- | A fresh hex nonce naming one remote command run (its pidfile and
-- @$0@ marker).  Uniqueness (time × per-process counter) is what matters,
-- not unpredictability — the pidfile lives in the remote \/tmp under the
-- connecting user's own account.
newRunNonce :: IO Text
newRunNonce = do
  n <- atomicModifyIORef' nonceCounter (\x -> (x + 1, x))
  t <- getPOSIXTime
  return . T.pack $ showHex (floor (t * 1000) :: Word64) ("-" <> showHex n "")

-- | The script a remote build\/tool run executes: cd to the directory, then
-- exec the command under @setsid@ (where available) with the process-group
-- id parked in a nonce pidfile so 'interruptRemoteRun' can kill the whole
-- cabal→ghc tree.  @prefix@ is spliced VERBATIM — it is a user-supplied
-- shell fragment (e.g. @nix develop -c@), not a quoted word.
remoteRunScript :: Maybe Text  -- ^ per-project command prefix
                -> FilePath    -- ^ remote directory to run in
                -> Text        -- ^ nonce from 'newRunNonce'
                -> FilePath    -- ^ executable
                -> [Text]      -- ^ arguments
                -> Text
remoteRunScript prefix rdir nonce cmd args =
  -- setsid -w: without -w setsid detaches (forks) and the ssh exit code
  -- becomes 0 even for a failed build.
  "cd " <> shellQuote (T.pack rdir) <> " || exit 44; "
  <> "if command -v setsid >/dev/null 2>&1; then exec setsid -w sh -c "
  <> shellQuote inner <> " " <> marker
  <> "; else exec sh -c " <> shellQuote inner <> " " <> marker <> "; fi"
  where
    marker = "leksah-run-" <> nonce
    pidfile = "/tmp/leksah-run-" <> nonce <> ".pid"
    cmdLine = maybe "" (<> " ") prefix
              <> T.unwords (shellQuote (T.pack cmd) : map shellQuote args)
    inner = "echo $$ > " <> pidfile <> "; "
            <> cmdLine
            <> "; s=$?; rm -f " <> pidfile <> "; exit $s"

-- | The local argv that runs a 'remoteRunScript' on a host.
remoteSshArgs :: Text -> Text -> (FilePath, [Text])
remoteSshArgs host script =
  ("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=10", host, script])

-- | Interrupt a remote run started via 'remoteRunScript': SIGINT the
-- process group named by the nonce pidfile, escalating to SIGTERM after 3s
-- (escalation is backgrounded remotely so this returns in ~1 round trip).
-- A missing pidfile (run already finished) is a no-op.
interruptRemoteRun :: Text -> Text -> IO ()
interruptRemoteRun host nonce = do
  (code, _, err) <- runSsh host script [] mempty
  case code of
    ExitSuccess     -> return ()
    ExitFailure 255 -> throwIO (RemoteConnectError host (decodeErr err))
    ExitFailure c   -> throwIO (RemoteOpFailed host c (decodeErr err))
  where
    pidfile = "/tmp/leksah-run-" <> nonce <> ".pid"
    script =
      -- The group kill takes the wrapper (and its `rm -f` cleanup) with it,
      -- so remove the pidfile here once the group is gone.
      "pf=" <> pidfile <> "; [ -f \"$pf\" ] || exit 0; pg=$(cat \"$pf\"); "
      <> "( kill -INT -- \"-$pg\" 2>/dev/null || kill -INT \"$pg\" 2>/dev/null; "
      <> "sleep 3; kill -0 \"$pg\" 2>/dev/null && "
      <> "{ kill -TERM -- \"-$pg\" 2>/dev/null || kill -TERM \"$pg\" 2>/dev/null; }; "
      <> "sleep 1; kill -0 \"$pg\" 2>/dev/null || rm -f \"$pf\" "
      <> ") >/dev/null 2>&1 &"

#endif
