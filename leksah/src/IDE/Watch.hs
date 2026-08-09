-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

-- | File-system watching, as a service: the one fsnotify manager plus the
-- registries of active watches.  Consumers (file tree, workspace
-- refresh) start and stop their own listeners through the manager and
-- record the stop actions here, keyed by what they watch, so a closing
-- project can retire exactly its own watches.
--
-- On the JS backend there is no fsnotify; the types become inert stubs.
module IDE.Watch
  ( WatchManager
  , StopListening
  , WatchService(..)
  , newWatchService
  ) where

import Control.Concurrent.MVar (MVar, newMVar)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import IDE.Ws.Types (ProjectKey)

#if defined(ghcjs_HOST_OS) || defined(javascript_HOST_ARCH)
data WatchManager = WatchManager
type StopListening = IO ()
#else
import System.FSNotify (StopListening, WatchManager, startManager)
#endif

data WatchService = WatchService
    { wManager   :: WatchManager
    , wWatchers  :: MVar (Map ProjectKey StopListening, Map FilePath StopListening)
      -- ^ per-project and per-file stop actions
    , wExternallyModified :: MVar (Set FilePath)
      -- ^ files changed on disk while open in an editor (auto-reload set)
    }

newWatchService :: IO WatchService
newWatchService = do
#if defined(ghcjs_HOST_OS) || defined(javascript_HOST_ARCH)
    let mgr = WatchManager
#else
    mgr <- startManager
#endif
    WatchService mgr <$> newMVar (M.empty, M.empty) <*> newMVar S.empty
