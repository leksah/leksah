-- SPDX-License-Identifier: Apache-2.0

-- | The reflex side of "IDE.Reactive": lift a service's 'Cell' into the
-- current reflex host.  Kept apart from the cell itself so services stay
-- reflex-free.
module IDE.Reactive.Dyn
  ( cellDyn
  , cellDynUniq
  ) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Reflex
       (Dynamic, MonadHold, Reflex, TriggerEvent, holdDyn, holdUniqDyn,
        newTriggerEvent)

import IDE.Reactive (Cell, watchCellCurrent)

-- | Lift a cell into the current reflex host.  The watcher is a
-- trigger-event fire — async into this host's queue, so the writing
-- service never waits on this window.  The subscription lives as long as
-- the host does (there is no per-widget teardown hook in 'TriggerEvent');
-- create window-lived dynamics near the window root and pass them down,
-- rather than calling this in short-lived widgets.
cellDyn
    :: (Reflex t, MonadHold t m, TriggerEvent t m, MonadIO m)
    => Cell a -> m (Dynamic t a)
cellDyn c = do
    (ev, fire) <- newTriggerEvent
    (a0, _unsub) <- liftIO $ watchCellCurrent c fire
    holdDyn a0 ev

-- | 'cellDyn' deduplicated with 'holdUniqDyn' — the usual choice when the
-- cell holds a record and a widget selects a piece of it.
cellDynUniq
    :: (Reflex t, MonadHold t m, MonadFix m, TriggerEvent t m, MonadIO m, Eq a)
    => Cell a -> m (Dynamic t a)
cellDynUniq c = cellDyn c >>= holdUniqDyn
