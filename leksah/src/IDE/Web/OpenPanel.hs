-- | Process-global hooks for showing the native open dialogs.
--
-- The toolbar/menubar Open command and the Add Project… dialog's browse button
-- are reflex events in the shared library, but the dialogs they show are native
-- (wkwebview / GTK / Win32) and can't be called from the library.  Each front
-- end registers handlers here and the library runs them; with no handler
-- (warp, ghcjs) they're no-ops.
--
-- Two separate seams, on purpose:
--
--   * 'runOpenFilePanel' — File ▸ Open.  Its result goes to
--     'IDE.Web.OpenFileRequest' and opens an editor tab.
--   * 'runPickPathPanel' — the Add Project… browse button.  Its result goes to
--     'IDE.Web.PickPathRequest', tagged with the asking dialog's token, and only
--     ever fills in a text field.
module IDE.Web.OpenPanel
  ( setOpenFilePanelHandler
  , runOpenFilePanel
  , PickCapability(..)
  , PickMode(..)
  , pickModeCode
  , setPickPathHandler
  , pickCapability
  , runPickPathPanel
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

-- | What this front end's native picker can do.  Decides how many browse
-- buttons the Add Project… dialog draws: none, one, or two.
data PickCapability
  = PickNone      -- ^ no native picker at all (warp, ghcjs) — draw no button
  | PickBoth      -- ^ one panel chooses files AND directories (macOS NSOpenPanel)
  | PickSeparate  -- ^ files and directories need separate panels (GTK4, Win32)
  deriving (Eq, Show)

-- | What one invocation of the picker should accept.
data PickMode = PickFilesAndDirs | PickFiles | PickDirs
  deriving (Eq, Show)

-- | The wire form of a 'PickMode', for the C/ObjC glue.
pickModeCode :: PickMode -> Int
pickModeCode PickFilesAndDirs = 0
pickModeCode PickFiles        = 1
pickModeCode PickDirs         = 2

{-# NOINLINE pickPathHandler #-}
pickPathHandler :: IORef (PickCapability, PickMode -> Int -> IO ())
pickPathHandler = unsafePerformIO (newIORef (PickNone, \_ _ -> return ()))

-- | Registered by each native front end during menu installation, i.e. before
-- any window builds — so the dialog can read 'pickCapability' once at build.
setPickPathHandler :: PickCapability -> (PickMode -> Int -> IO ()) -> IO ()
setPickPathHandler cap run = writeIORef pickPathHandler (cap, run)

pickCapability :: IO PickCapability
pickCapability = fst <$> readIORef pickPathHandler

-- | Show the native picker; the chosen path comes back through
-- 'IDE.Web.PickPathRequest.deliverPickedPath' carrying @token@.
runPickPathPanel :: PickMode -> Int -> IO ()
runPickPathPanel mode token = do
    (_, run) <- readIORef pickPathHandler
    run mode token
