{-# LANGUAGE ScopedTypeVariables #-}
-- | Zero-downtime restart ("handoff"): the running instance stays up while its
-- successor starts, and only retires once the successor's UI is ready.
--
-- Opt-in via @LEKSAH_HANDOFF=1@ (compiled web front end only — ghci mode has no
-- supervisor loop, and the classic Gtk UI is unaffected).  When on, the four
-- restart exit sites no longer @exitImmediately@; instead the old instance drops
-- a request file and keeps running.  The @leksah.sh@ supervisor loop drains the
-- request, launches the successor (on an ephemeral asset port so it can coexist
-- on the fixed 'IDE.Web.Instance.leksahPort'), waits for it to signal readiness
-- (the successor touches the ready file from its first window's post-build), then
-- SIGTERMs the old one.  The successor's control server acquires @cmd.sock@ once
-- the old releases it (see 'IDE.Web.CmdServer.startCmdServer').
--
-- Off macOS / when the flag is unset everything here is inert, so the ordinary
-- exit-2/3 relaunch path is completely unchanged.
module IDE.Web.Handoff
  ( handoffEnabled
  , isHandoffSuccessor
  , requestHandoff
  , handingOff
  , signalHandoffReady
  , registerSessionFlush
  , signalSessionFlushDone
  , handoffRequestFile
  , handoffReadyFile
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Exception (catch, SomeException)
import Control.Monad (when, void)
import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef')
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import IDE.Utils.ExitImmediately (exitImmediately)

-- | Is the overlapping-handoff restart enabled (@LEKSAH_HANDOFF=1@)?  Read once.
handoffEnabled :: Bool
handoffEnabled = unsafePerformIO (lookupEnv "LEKSAH_HANDOFF") == Just "1"
{-# NOINLINE handoffEnabled #-}

-- | Was this process launched as a handoff SUCCESSOR (@LEKSAH_SUCCESSOR=1@, set
-- by the supervisor loop)?  A successor touches the ready file when its first
-- window's DOM is built, so the loop knows it may retire the predecessor.
isHandoffSuccessor :: Bool
isHandoffSuccessor = unsafePerformIO (lookupEnv "LEKSAH_SUCCESSOR") == Just "1"
{-# NOINLINE isHandoffSuccessor #-}

-- | @~\/.leksah\/handoff-request@ — the old instance writes @rebuild@\/@norebuild@
-- here to ask the loop to start a successor.
handoffRequestFile :: IO FilePath
handoffRequestFile = leksahFile "handoff-request"

-- | @~\/.leksah\/handoff-ready@ — the successor touches this when its UI is up.
handoffReadyFile :: IO FilePath
handoffReadyFile = leksahFile "handoff-ready"

leksahFile :: FilePath -> IO FilePath
leksahFile name = do
  home <- getHomeDirectory
  let dir = home </> ".leksah"
  createDirectoryIfMissing True dir
  return (dir </> name)

-- | Set once this instance has asked to hand off, so the session writer stops
-- (the successor's restore+save is authoritative — see 'IDE.Web.Main').
{-# NOINLINE handingOffRef #-}
handingOffRef :: IORef Bool
handingOffRef = unsafePerformIO (newIORef False)

-- | Firers that, when run, make each window's reflex network write the CURRENT
-- session to disk and 'signalSessionFlushDone' (only the active window actually
-- writes — see 'IDE.Web.Main').  Registered per window; fired by
-- 'flushSessionAndWait' just before a handoff so the successor loads the old
-- instance's up-to-date state (flipper MRU, tabs, layout, …).
{-# NOINLINE flushFirersRef #-}
flushFirersRef :: IORef [IO ()]
flushFirersRef = unsafePerformIO (newIORef [])

registerSessionFlush :: IO () -> IO ()
registerSessionFlush f = modifyIORef' flushFirersRef (f :)

{-# NOINLINE flushDoneVar #-}
flushDoneVar :: MVar ()
flushDoneVar = unsafePerformIO newEmptyMVar

-- | Called (from the active window's network) once it has written the session
-- during a flush.
signalSessionFlushDone :: IO ()
signalSessionFlushDone = void (tryPutMVar flushDoneVar ())

-- | Force the current session to disk NOW and wait (bounded, ~3s) for the write
-- to confirm — so the successor restores the state as it stands at handoff,
-- not the last debounced snapshot.
flushSessionAndWait :: IO ()
flushSessionAndWait = do
  _ <- tryTakeMVar flushDoneVar          -- clear any stale completion
  readIORef flushFirersRef >>= sequence_
  let waitLoop n
        | n <= (0 :: Int) = return ()
        | otherwise = tryTakeMVar flushDoneVar >>= \m -> case m of
            Just () -> return ()
            Nothing -> threadDelay 100000 >> waitLoop (n - 1)
  waitLoop 30

-- | Ask the supervisor loop to hand off to a fresh successor (compiled mode).
-- @noRebuild@ mirrors the exit-3 semantics — the loop skips the cabal build
-- (e.g. after @rebuild-self@ already produced the binary).  The old instance
-- flushes its state, then stays up until the successor signals ready (the ready
-- file), at which point it retires itself (exit 0) so the successor takes over
-- @cmd.sock@ / the windows.  If the successor never signals (bounded wait), the
-- handoff is aborted and the old instance simply resumes.
requestHandoff :: Bool -> IO ()
requestHandoff noRebuild = do
  -- Stop the debounced writer (so it can't clobber the flush), flush the current
  -- state, THEN publish the request so the loop only launches the successor once
  -- the old instance's state is on disk.
  writeIORef handingOffRef True
  flushSessionAndWait
  reqPath <- handoffRequestFile
  writeFile reqPath (if noRebuild then "norebuild" else "rebuild")
    `catch` \(_ :: SomeException) -> return ()
  readyPath <- handoffReadyFile
  void $ forkIO $ retireWhenReady readyPath 0

-- | Poll for the successor's ready file; retire (exit 0) once it appears, so the
-- successor inherits the socket/windows.  Give up after ~120s (successor never
-- came up): resume normal operation instead of dying.
retireWhenReady :: FilePath -> Int -> IO ()
retireWhenReady readyPath n = do
  ready <- doesFileExist readyPath `catch` \(_ :: SomeException) -> return False
  if ready
    then exitImmediately ExitSuccess
    else if n >= 240
      then writeIORef handingOffRef False   -- abort: successor never signalled
      else threadDelay 500000 >> retireWhenReady readyPath (n + 1)

-- | Has this instance initiated a handoff (so it should stop persisting the
-- web session)?
handingOff :: IO Bool
handingOff = readIORef handingOffRef

-- | Signal that this (successor) instance's UI is up — touch the ready file so
-- the supervisor loop can retire the predecessor.  A no-op unless this process
-- is actually a successor.
signalHandoffReady :: IO ()
signalHandoffReady = when isHandoffSuccessor $ do
  path <- handoffReadyFile
  writeFile path "ready" `catch` \(_ :: SomeException) -> return ()
