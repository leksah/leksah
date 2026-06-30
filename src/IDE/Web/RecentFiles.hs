-- | A process-global hook for publishing the recent-files list to a native UI.
--
-- The recent-files list is maintained in the reflex network ('IDE.Web.Main'),
-- but the place that shows it (the wkwebview "Open Recent" submenu) lives in the
-- wkwebview-only menu glue, which the shared library can't call directly.  So the
-- wkwebview front end registers a handler here and 'IDE.Web.Main' pushes updates
-- through it; with no handler registered (warp/webkitgtk) the updates are no-ops.
module IDE.Web.RecentFiles
  ( setRecentFilesHandler
  , updateRecentFiles
  ) where

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE handlerRef #-}
handlerRef :: IORef ([FilePath] -> IO ())
handlerRef = unsafePerformIO (newIORef (const (return ())))

-- | Register how the recent-files list should be shown (e.g. the native menu).
setRecentFilesHandler :: ([FilePath] -> IO ()) -> IO ()
setRecentFilesHandler = writeIORef handlerRef

-- | Publish the current recent-files list (most recent first) to the handler.
updateRecentFiles :: [FilePath] -> IO ()
updateRecentFiles fps = readIORef handlerRef >>= ($ fps)
