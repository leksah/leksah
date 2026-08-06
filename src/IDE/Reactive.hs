-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE BangPatterns #-}

-- | The one primitive bridging services and reflex widgets: an observable
-- state cell.  Services own cells and write them from any thread; each
-- window's widgets lift the cells they show into 'Reflex.Dynamic's with
-- @cellDyn@ (in "IDE.Reactive.Dyn" — this module deliberately has no
-- reflex dependency, so every service typechecks with a bare ghc).
-- Nothing else crosses the boundary — there is no shared mutable record
-- and no whole-state resync.
--
-- Concurrency contract:
--
-- * Writes to one cell are serialized; watchers see every committed value,
--   in order, on the writer's thread.
-- * 'readCell' never blocks (it is not gated by the write lock).
-- * A watcher must be cheap and non-blocking — hand the value to your own
--   executor.  'cellDyn' obeys this: its watcher is a trigger-event fire,
--   which queues into the reflex host without waiting for it.  This is why
--   a service can never be wedged by a stuck window, or a window by
--   another window.
-- * A watcher must not synchronously write the cell it watches (the write
--   lock is not re-entrant).  Loop through your own executor instead.
module IDE.Reactive
  ( Cell
  , newCell
  , readCell
  , writeCell
  , modifyCell
  , watchCell
  , watchCellCurrent
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
       (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

-- | An observable state cell.
data Cell a = Cell
    { cellValue :: IORef a
    , cellLock  :: MVar ()               -- ^ serializes write+notify
    , cellSubs  :: IORef (IntMap (a -> IO ()))
    , cellNext  :: IORef Int
    }

newCell :: a -> IO (Cell a)
newCell a = Cell <$> newIORef a <*> newMVar () <*> newIORef IM.empty <*> newIORef 0

-- | The current value.  Never blocks, even mid-write (you may see the
-- value a concurrent write is about to replace).
readCell :: MonadIO m => Cell a -> m a
readCell = liftIO . readIORef . cellValue

writeCell :: MonadIO m => Cell a -> a -> m ()
writeCell c a = modifyCell c (const a)

modifyCell :: MonadIO m => Cell a -> (a -> a) -> m ()
modifyCell c f = liftIO . withMVar (cellLock c) $ \() -> do
    a <- readIORef (cellValue c)
    let !a' = f a
    writeIORef (cellValue c) a'
    subs <- readIORef (cellSubs c)
    mapM_ ($ a') (IM.elems subs)

-- | Watch for committed values.  Returns the unsubscribe action.
watchCell :: Cell a -> (a -> IO ()) -> IO (IO ())
watchCell c k = do
    i <- atomicModifyIORef' (cellNext c) (\i -> (i + 1, i))
    atomicModifyIORef' (cellSubs c) (\m -> (IM.insert i k m, ()))
    return $ atomicModifyIORef' (cellSubs c) (\m -> (IM.delete i m, ()))

-- | 'watchCell', atomically returning the value at subscription time: no
-- write can slip between the read and the subscription, so a consumer
-- seeded with the returned value plus subsequent notifications never goes
-- stale.
watchCellCurrent :: Cell a -> (a -> IO ()) -> IO (a, IO ())
watchCellCurrent c k = withMVar (cellLock c) $ \() -> do
    unsub <- watchCell c k
    a <- readIORef (cellValue c)
    return (a, unsub)
