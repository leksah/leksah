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
handlerRef :: IORef (Maybe (Text -> IO Bool))
handlerRef = unsafePerformIO (newIORef Nothing)

-- | Register the front end's capture function (path → wrote it?).  Called once
-- at start-up by a front end that can screenshot (wkwebview).
registerScreenshotHandler :: (Text -> IO Bool) -> IO ()
registerScreenshotHandler = writeIORef handlerRef . Just

-- | Capture the UI to @path@; 'False' if no front end registered a handler (or
-- the capture failed).
requestScreenshot :: Text -> IO Bool
requestScreenshot path = readIORef handlerRef >>= \case
  Just h  -> h path
  Nothing -> return False

{-# NOINLINE regionHandlerRef #-}
regionHandlerRef :: IORef (Maybe (Text -> (Int, Int, Int, Int) -> IO Bool))
regionHandlerRef = unsafePerformIO (newIORef Nothing)

-- | Register the front end's region-capture function: @path -> (x,y,w,h) in CSS
-- px -> wrote it?@.  Used by 'IDE.Web.Main' for the permission-free path (a
-- WKWebView snapshot cropped to the selected rectangle).
registerScreenshotRegionHandler :: (Text -> (Int, Int, Int, Int) -> IO Bool) -> IO ()
registerScreenshotRegionHandler = writeIORef regionHandlerRef . Just

-- | Snapshot just the given rectangle of the UI to @path@.  'False' if no
-- handler is registered or the snapshot failed.
requestScreenshotRegion :: Text -> (Int, Int, Int, Int) -> IO Bool
requestScreenshotRegion path rect = readIORef regionHandlerRef >>= \case
  Just h  -> h path rect
  Nothing -> return False
