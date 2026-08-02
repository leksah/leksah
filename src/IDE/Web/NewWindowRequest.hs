-- | A process-global hook for opening a new OS window.
--
-- File ▸ New Window (⌘N) is a shared 'Command' whose action runs in the library
-- ('IDE.Web.Command'), but actually creating a native NSWindow + WKWebView is
-- wkwebview-only glue the library can't call.  So the wkwebview front end
-- ('IDE.Web.MacMenu') registers a handler here and the command action runs it;
-- with no handler (warp/webkitgtk, where one browser tab is the whole UI) the
-- request is a no-op.  Mirrors 'IDE.Web.OpenPanel'.
module IDE.Web.NewWindowRequest
  ( setNewWindowHandler
  , requestNewWindow
  , setOpenWindowHandler
  , requestOpenWindow
  , setRaiseWindowHandler
  , requestRaiseWindow
  ) where

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Web.GhciMode (phaseLog)

{-# NOINLINE newWindowHandler #-}
newWindowHandler :: IORef (IO ())
newWindowHandler = unsafePerformIO (newIORef (return ()))

-- | Register how a new window should be opened (the wkwebview native glue).
setNewWindowHandler :: IO () -> IO ()
setNewWindowHandler = writeIORef newWindowHandler

-- | Ask for a new window (called from the New Window command action).
requestNewWindow :: IO ()
requestNewWindow = readIORef newWindowHandler >>= id

{-# NOINLINE openWindowHandler #-}
openWindowHandler :: IORef (Int -> IO ())
openWindowHandler = unsafePerformIO (newIORef (const (return ())))

-- | Register how to create a native window for an already-seeded 'WindowId'
-- (the restore path, which seeds the window state first then asks native to
-- create the NSWindow + WKWebView for each saved window past the first).
setOpenWindowHandler :: (Int -> IO ()) -> IO ()
setOpenWindowHandler = writeIORef openWindowHandler

-- | Create a native window for the given (already-seeded) window id.
requestOpenWindow :: Int -> IO ()
requestOpenWindow n = do
  phaseLog ("boot: requestOpenWindow " <> show n)
  readIORef openWindowHandler >>= ($ n)

{-# NOINLINE raiseWindowHandler #-}
raiseWindowHandler :: IORef (Int -> IO ())
raiseWindowHandler = unsafePerformIO (newIORef (const (return ())))

-- | Register how to bring a window to the front (the global flipper's
-- cross-window raise; wkwebview @makeKeyAndOrderFront@).
setRaiseWindowHandler :: (Int -> IO ()) -> IO ()
setRaiseWindowHandler = writeIORef raiseWindowHandler

-- | Bring the given window to the front.
requestRaiseWindow :: Int -> IO ()
requestRaiseWindow n = readIORef raiseWindowHandler >>= ($ n)
