-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}

-- | Coalescing runners for the IDE's rescans.
--
-- A file-system watcher fires per changed file, so a build that touches
-- thousands of files in @dist-newstyle@ asks for thousands of rescans — and
-- a @git status --ignored@ over a big ignored tree takes tens of seconds, so
-- naive spawning piles up dozens of concurrent scans, each slower than the
-- last.
--
-- 'newCoalescer' turns that into at most two runs: the one in flight, and one
-- more afterwards if anything asked while it ran.  Requests never block and
-- never queue up.
module IDE.Web.Coalesce
  ( newCoalescer
  , newSharedCoalescer
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
       (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M

-- | What a coalescer is doing.
data Phase
    = Idle
    | Running
    | RunningDirty  -- ^ a request arrived while running; run once more

-- | Wrap an action so that concurrent requests coalesce: the returned
-- action starts the work in a background thread when idle, and otherwise
-- just records that another run is wanted.  It returns immediately either
-- way, so it is safe to call from a watcher callback or the frame thread.
--
-- The work runs to completion even if it throws (the exception is
-- swallowed, as a watcher-driven rescan has nowhere to report it) — so the
-- gate can never be left stuck closed.
newCoalescer :: IO () -> IO (IO ())
newCoalescer work = do
    phase <- newMVar Idle
    let request = modifyMVar_ phase $ \case
            Idle -> do
                void . forkIO $ loop
                return Running
            Running      -> return RunningDirty
            RunningDirty -> return RunningDirty
        loop = do
            _ <- try work :: IO (Either SomeException ())
            again <- modifyMVar phase $ \case
                RunningDirty -> return (Running, True)
                _            -> return (Idle, False)
            if again then loop else return ()
    return request

-- | A family of coalescers keyed by something (a directory, a repository):
-- returns a request function that coalesces per key, so one project's
-- rescans can't be starved by another's.  The action receives the key.
newSharedCoalescer :: Ord k => (k -> IO ()) -> IO (k -> IO ())
newSharedCoalescer work = do
    ref <- newIORef M.empty
    return $ \k -> do
        existing <- M.lookup k <$> readIORef ref
        case existing of
            Just request -> request
            Nothing -> do
                request <- newCoalescer (work k)
                -- Another thread may have won the race; keep whichever
                -- coalescer landed in the map first, so a key always has
                -- exactly one gate.
                winner <- atomicModifyIORef' ref $ \m ->
                    case M.lookup k m of
                        Just r  -> (m, r)
                        Nothing -> (M.insert k request m, request)
                winner
