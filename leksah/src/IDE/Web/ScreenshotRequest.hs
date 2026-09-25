{-# LANGUAGE LambdaCase #-}
-- | A process-global hook for capturing a screenshot of the running UI.
--
-- @leksah-cmd screenshot FILE@ reaches 'IDE.Web.CmdServer' in the shared lib,
-- but the actual capture is front-end specific and native (WKWebView's
-- @takeSnapshot@ on macOS), living in @main/@.  So the native front end
-- registers a handler here at start-up and the command server calls it.  A
-- front end without a capture path (warp, webkitgtk) simply never registers one,
-- and the command reports that it's unavailable.  Mirrors the other native
-- bridges (e.g. 'IDE.Web.PreferencesRequest'), but request/response since the
-- caller needs to know whether the file was written.
module IDE.Web.ScreenshotRequest
  ( registerScreenshotHandler
  , requestScreenshot
  , registerScreenshotRegionHandler
  , requestScreenshotRegion
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE handlerRef #-}
handlerRef :: IORef (Maybe (Text -> Int -> IO Bool))
handlerRef = unsafePerformIO (newIORef Nothing)

-- | Register the front end's capture function (path → OS window id → wrote
-- it?).  Called once at start-up by a front end that can screenshot
-- (wkwebview).
registerScreenshotHandler :: (Text -> Int -> IO Bool) -> IO ()
registerScreenshotHandler = writeIORef handlerRef . Just

-- | Capture OS window @wid@ to @path@; 'False' if no front end registered a
-- handler (or the capture failed).
--
-- The window id is explicit because the native capture used to be hard-wired
-- to window 0: with several windows open, @leksah-cmd screenshot@ photographed
-- whichever window happened to be first, so anything in the window the user was
-- actually looking at — a modal dialog, say — never appeared in the PNG.
-- Callers resolve the default from 'IDE.Web.Model.activeWindow'.
requestScreenshot :: Text -> Int -> IO Bool
requestScreenshot path wid = readIORef handlerRef >>= \case
  Just h  -> h path wid
  Nothing -> return False

{-# NOINLINE regionHandlerRef #-}
regionHandlerRef :: IORef (Maybe (Text -> Int -> (Int, Int, Int, Int) -> IO Bool))
regionHandlerRef = unsafePerformIO (newIORef Nothing)

-- | Register the front end's region-capture function: @path -> OS window id ->
-- (x,y,w,h) in CSS px -> wrote it?@.  Used by 'IDE.Web.Main' for the
-- permission-free path (a WKWebView snapshot cropped to the selected rectangle).
registerScreenshotRegionHandler
  :: (Text -> Int -> (Int, Int, Int, Int) -> IO Bool) -> IO ()
registerScreenshotRegionHandler = writeIORef regionHandlerRef . Just

-- | Snapshot just the given rectangle of OS window @wid@ to @path@.  'False' if
-- no handler is registered or the snapshot failed.
--
-- The rectangle is in the coordinate system of the window whose page drew the
-- picker, so it is only meaningful together with that window's id — the picker
-- runs in the window the user selected in, which need not be window 0.
requestScreenshotRegion :: Text -> Int -> (Int, Int, Int, Int) -> IO Bool
requestScreenshotRegion path wid rect = readIORef regionHandlerRef >>= \case
  Just h  -> h path wid rect
  Nothing -> return False
