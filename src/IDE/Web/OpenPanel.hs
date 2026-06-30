-- | Process-global hooks for showing the native open dialogs.
--
-- The toolbar/menubar Open and Open-Project commands are reflex events in the
-- shared library, but the dialogs they show are native (wkwebview-only) and
-- can't be called from the library.  The wkwebview front end registers handlers
-- here and 'IDE.Web.Main' runs them; with no handler (warp/webkitgtk) they're
-- no-ops.
module IDE.Web.OpenPanel
  ( setOpenFilePanelHandler
  , runOpenFilePanel
  , setOpenProjectPanelHandler
  , runOpenProjectPanel
  ) where

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE openFileHandler #-}
openFileHandler :: IORef (IO ())
openFileHandler = unsafePerformIO (newIORef (return ()))

setOpenFilePanelHandler :: IO () -> IO ()
setOpenFilePanelHandler = writeIORef openFileHandler

runOpenFilePanel :: IO ()
runOpenFilePanel = readIORef openFileHandler >>= id

{-# NOINLINE openProjectHandler #-}
openProjectHandler :: IORef (IO ())
openProjectHandler = unsafePerformIO (newIORef (return ()))

setOpenProjectPanelHandler :: IO () -> IO ()
setOpenProjectPanelHandler = writeIORef openProjectHandler

runOpenProjectPanel :: IO ()
runOpenProjectPanel = readIORef openProjectHandler >>= id
