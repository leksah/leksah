{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A process-global "something about a remote project may have changed"
-- notification.  Remote projects have no file watchers and NO periodic
-- polling (a high-latency link must not tick) — instead the panes that
-- would poll refresh when one of these fires: an editor save of a remote
-- file, a remote build finishing, a project opening, or the user pressing
-- a refresh button.
--
-- Fan-out registry rather than a Chan: several widgets listen (Changes
-- pane, file-tree git status, the workspace-files cache), and each
-- registers the fire side of its own reflex trigger — the
-- 'IDE.Web.WindowBridge' registerResync shape.
module IDE.Web.RemoteRefresh
  ( RefreshReason(..)
  , requestRemoteRefresh
  , registerRemoteRefresh
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M (delete, elems, empty, insert)
import System.IO.Unsafe (unsafePerformIO)

data RefreshReason
  = RefreshSaved FilePath   -- ^ a remote file was saved
  | RefreshBuildDone        -- ^ a remote tool run finished
  | RefreshProjectOpened
  | RefreshManual           -- ^ the Changes pane's refresh button
  deriving (Show, Eq)

{-# NOINLINE handlers #-}
handlers :: IORef (Int, Map Int (RefreshReason -> IO ()))
handlers = unsafePerformIO (newIORef (0, M.empty))

-- | Notify every registered listener (usually reflex trigger fires — cheap
-- and non-blocking).  Callable from any thread; listener exceptions are
-- swallowed so one dead widget can't break the rest.
requestRemoteRefresh :: RefreshReason -> IO ()
requestRemoteRefresh r = do
    (_, m) <- readIORef handlers
    mapM_ (\h -> h r `catch` \(_ :: SomeException) -> return ()) (M.elems m)

-- | Register a listener; returns the unregister action.
registerRemoteRefresh :: (RefreshReason -> IO ()) -> IO (IO ())
registerRemoteRefresh h = do
    k <- atomicModifyIORef' handlers $ \(n, m) ->
            ((n + 1, M.insert n h m), n)
    return . void $ atomicModifyIORef' handlers $ \(n, m) ->
            ((n, M.delete k m), ())
