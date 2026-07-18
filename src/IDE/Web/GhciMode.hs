-- | Support for running leksah inside a cabal repl (@leksah.sh --ghci@).
--
-- In ghci mode the process must never 'IDE.Utils.ExitImmediately.exitImmediately'
-- — that would kill the ghci session whose whole point is surviving the app so
-- @:reload@ + a fresh @:main@ replace the rebuild-and-relaunch loop.  Instead
-- the exit sites call 'stopForGhci': run the registered cleanups (close the
-- listening sockets and native windows a fresh @:main@ must be able to
-- recreate), then stop the front end's run loop so @main@ returns to the
-- prompt.  Background threads that merely reference dead resources are left
-- to leak — acceptable dev-mode debris; a full process restart stays the
-- deep-clean.
module IDE.Web.GhciMode
  ( ghciMode
  , registerGhciCleanup
  , setGhciStop
  , stopForGhci
  , suspendForGhci
  ) where

import Control.Concurrent (myThreadId, killThread)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, join, void, when)
import Data.IORef (IORef, newIORef, atomicModifyIORef', writeIORef, readIORef)
import Data.List (isPrefixOf)
import GHC.Conc.Sync (listThreads, threadLabel)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | True when running under @leksah.sh --ghci@ (@LEKSAH_GHCI@ set non-empty
-- in the environment).
{-# NOINLINE ghciMode #-}
ghciMode :: Bool
ghciMode = unsafePerformIO $ maybe False (not . null) <$> lookupEnv "LEKSAH_GHCI"

{-# NOINLINE cleanupsRef #-}
cleanupsRef :: IORef [IO ()]
cleanupsRef = unsafePerformIO (newIORef [])

-- | Register the teardown of a resource a fresh @:main@ must be able to
-- recreate (the warp listener, the cmd-socket listener, native windows).
-- Each registration runs at most once, in 'stopForGhci'.
registerGhciCleanup :: IO () -> IO ()
registerGhciCleanup c = atomicModifyIORef' cleanupsRef (\cs -> (c : cs, ()))

{-# NOINLINE stopRef #-}
stopRef :: IORef (IO ())
stopRef = unsafePerformIO (newIORef (return ()))

-- | How the front end's run loop is made to return control to the ghci
-- prompt (macOS: 'IDE.Web.MacGlue.stopApp').  Registered at startup by the
-- platform layer.
setGhciStop :: IO () -> IO ()
setGhciStop = writeIORef stopRef

-- | Kill the previous run's leksah network/bridge threads before a
-- @:reload@ + fresh @:main@.  @:reload@ leaves the old run's threads alive; the
-- jsaddle-wkwebview bridge serialises on the single Cocoa main queue, so a
-- stale per-window frame thread (or a duplicated bridge drain routing to a dead
-- window) blocks the reloaded windows' initial build on that queue and they
-- freeze.  Match by the labels leksah gives these threads; the unlabelled
-- pollers only post to the (now-dead) frame threads' channels, so they go inert
-- and can stay as harmless debris.  Never kill the current thread (the one
-- running teardown).
killAppThreads :: IO ()
killAppThreads = do
  me <- myThreadId
  ts <- listThreads
  forM_ ts $ \t -> when (t /= me) $ do
    mlbl <- threadLabel t
    case mlbl of
      Just lbl | any (`isPrefixOf` lbl) appThreadPrefixes ->
        void (try (killThread t) :: IO (Either SomeException ()))
      _ -> return ()
  where appThreadPrefixes =
          ["reflex-frames-", "resync-notifier-", "bridge-drain-"]

-- | Tear down for a @:reload@: kill the old run's network threads, run the
-- registered cleanups (emptying the list, so a re-registering fresh run starts
-- clean), then stop the run loop so @main@ returns to the ghci prompt.
-- Exceptions in individual cleanups are swallowed — teardown must always reach
-- the stop.
stopForGhci :: IO ()
stopForGhci = do
  when ghciMode $ void (try killAppThreads :: IO (Either SomeException ()))
  cs <- atomicModifyIORef' cleanupsRef (\pending -> ([], pending))
  mapM_ (\c -> void (try c :: IO (Either SomeException ()))) cs
  suspendForGhci

-- | Stop only the run loop, with NO cleanups: @leksah-cmd hs eval@'s suspend.
-- Windows, sockets and all state stay; 'IDE.Web.MacGlue.resumeApp' evaluated
-- at the prompt takes the UI straight back to where it was.
suspendForGhci :: IO ()
suspendForGhci = join (readIORef stopRef)
