module IDE.Gtk.State (
    postSyncIDE'
  , postAsyncIDE'
  , postSyncIDE
  , postAsyncIDE
  , postSyncIDEIdle
  , postAsyncIDEIdle
  , delayedBy

  , bringPaneToFront

  , module Reexported
) where

import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Data.Int (Int32)
import GHC.Stack (HasCallStack)
import IDE.Core.State (IDEAction, IDEM, MonadIDE, liftIDE, forkIDE)
import IDE.Gtk.Types as Reexported
import Control.Monad (void)
import Control.Concurrent (threadDelay, forkIO)

postSyncIDE' :: (MonadIDE m, HasCallStack) => Int32 -> IDEM a -> m a
postSyncIDE' _priority = liftIDE

postSyncIDE :: (MonadIDE m, HasCallStack) => IDEM a -> m a
postSyncIDE = liftIDE

postSyncIDEIdle :: (MonadIDE m, HasCallStack) => IDEM a -> m a
postSyncIDEIdle = liftIDE

postAsyncIDE' :: (MonadIDE m, HasCallStack) => Int32 -> IDEM () -> m ()
postAsyncIDE' _priority = forkIDE

postAsyncIDE :: (MonadIDE m, HasCallStack) => IDEM () -> m ()
postAsyncIDE = forkIDE

postAsyncIDEIdle :: (MonadIDE m, HasCallStack) => IDEM () -> m ()
postAsyncIDEIdle = forkIDE

delayedBy :: MonadUnliftIO m => Int -> m () -> m ()
delayedBy n f = withRunInIO $ \run ->
    void . forkIO $ threadDelay n >> run f

bringPaneToFront :: pane -> IDEAction
bringPaneToFront _ = return ()

