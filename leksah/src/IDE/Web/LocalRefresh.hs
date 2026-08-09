{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A process-global "a watched local file changed" notification — the local
-- twin of 'IDE.Web.RemoteRefresh'.  Local projects used to poll @git status@ /
-- @git diff@ and stat @flake.nix@ every few seconds; instead the fsnotify
-- watchers that 'IDE.Workspaces.Writer' already installs on every project dir,
-- package source tree and @.git@ dir fire one of these, and the panes that
-- would poll (Changes, file-tree git status, the flake node) refresh in
-- response.  No wall-clock timer ticks while the IDE is idle.
--
-- Fan-out registry (not a Chan): several widgets listen and each registers the
-- fire side of its own reflex trigger — the 'IDE.Web.WindowBridge'
-- registerResync shape, same as 'IDE.Web.RemoteRefresh'.  The payload is the
-- changed path, so a listener can scope its refresh to the affected dir.
module IDE.Web.LocalRefresh
  ( requestLocalRefresh
  , registerLocalRefresh
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M (delete, elems, empty, insert)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE handlers #-}
handlers :: IORef (Int, Map Int (FilePath -> IO ()))
handlers = unsafePerformIO (newIORef (0, M.empty))

-- | Notify every registered listener that @path@ changed (usually a reflex
-- trigger fire — cheap and non-blocking).  Callable from any thread; listener
-- exceptions are swallowed so one dead widget can't break the rest.
requestLocalRefresh :: FilePath -> IO ()
requestLocalRefresh path = do
    (_, m) <- readIORef handlers
    mapM_ (\h -> h path `catch` \(_ :: SomeException) -> return ()) (M.elems m)

-- | Register a listener; returns the unregister action.
registerLocalRefresh :: (FilePath -> IO ()) -> IO (IO ())
registerLocalRefresh h = do
    k <- atomicModifyIORef' handlers $ \(n, m) ->
            ((n + 1, M.insert n h m), n)
    return . void $ atomicModifyIORef' handlers $ \(n, m) ->
            ((n, M.delete k m), ())
