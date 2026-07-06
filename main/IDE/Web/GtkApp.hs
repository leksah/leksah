{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | The native GTK4 window for leksah-webkitgtk: a GtkApplicationWindow
-- hosting a WebKitGTK 6.0 WebView that renders the leksah web UI (served by
-- the warp instance 'IDE.Web.Main.startJSaddle' forks).  The Linux sibling of
-- the AppKit window leksah-wkwebview gets from jsaddle-wkwebview.
module IDE.Web.GtkApp
  ( runGtkApp
  ) where

import Control.Exception (SomeException, try)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (unless, void)

import Data.ByteString (ByteString)
import Data.IORef (newIORef, atomicModifyIORef')
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8)

import qualified GI.Gdk as Gdk (textureSaveToPng)
import qualified GI.Gio as Gio
       (ApplicationFlags(..), Cancellable, applicationRun,
        onApplicationActivate)
import qualified GI.Gtk as Gtk
       (applicationNew, applicationWindowNew, windowSetTitle,
        windowSetDefaultSize, windowSetChild, windowPresent)
import qualified GI.WebKit as WK
       (LoadEvent(..), SnapshotOptions(..), SnapshotRegion(..), WebView,
        onWebViewLoadChanged, setSettingsEnableDeveloperExtras,
        setSettingsEnableJavascript,
        setSettingsEnableWriteConsoleMessagesToStdout, webViewGetSettings,
        webViewGetSnapshot, webViewGetSnapshotFinish, webViewLoadHtml,
        webViewNew, webViewSetSettings)

import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.WebKitGTK (runInWebView)

import IDE.Web.GtkMenu (installGtkMenu, postGUIAsync)
import IDE.Web.ScreenshotRequest (registerScreenshotHandler)

-- | Run the leksah UI in a GTK4 window.  Matches the runner signature
-- 'IDE.Web.Main.startJSaddle' expects: the index HTML is loaded with the warp
-- server's URL as base, so relative fetches (xterm assets, terminal iframes)
-- resolve against it — the GTK counterpart of wkwebview's
-- @runHTMLWithBaseURL@.
runGtkApp :: ByteString -> ByteString -> JSM () -> IO ()
runGtkApp html url jsm = do
  -- NonUnique: the develop-leksah relaunch (exit 2 + respawn) must start a
  -- fresh instance, not forward its activation to the dying one.
  app <- Gtk.applicationNew (Just "org.leksah.Leksah")
                            [Gio.ApplicationFlagsNonUnique]
  _ <- Gio.onApplicationActivate app $ do
    win <- Gtk.applicationWindowNew app
    Gtk.windowSetTitle win (Just "Leksah")
    Gtk.windowSetDefaultSize win 1200 800
    webView <- WK.webViewNew
    settings <- WK.webViewGetSettings webView
    WK.setSettingsEnableJavascript settings True
    WK.setSettingsEnableDeveloperExtras settings True
    WK.setSettingsEnableWriteConsoleMessagesToStdout settings True
    WK.webViewSetSettings webView settings
    Gtk.windowSetChild win (Just webView)
    installGtkMenu app win
    registerSnapshotHandler webView
    -- runInWebView registers the "jsaddle" script-message handler, which must
    -- only happen once — a page reload would fire LoadEventFinished again
    -- (and leave a dead bridge, same as wkwebview: reload needs a restart).
    started <- newIORef False
    _ <- WK.onWebViewLoadChanged webView $ \case
      WK.LoadEventFinished -> do
        done <- atomicModifyIORef' started (True,)
        unless done $ runInWebView jsm webView
      _ -> return ()
    WK.webViewLoadHtml webView (decodeUtf8 html) (Just (decodeUtf8 url))
    Gtk.windowPresent win
  void $ Gio.applicationRun app Nothing

-- | @leksah-cmd screenshot FILE@: snapshot the WebView to a PNG.  The request
-- arrives on a CmdServer thread; the snapshot must run on the GTK main loop,
-- so marshal over and block on the result.
registerSnapshotHandler :: WK.WebView -> IO ()
registerSnapshotHandler webView =
  registerScreenshotHandler $ \path -> do
    done <- newEmptyMVar
    postGUIAsync $
      WK.webViewGetSnapshot webView WK.SnapshotRegionVisible
          [WK.SnapshotOptionsNone] (Nothing :: Maybe Gio.Cancellable) . Just $
        \_ res ->
          try (WK.webViewGetSnapshotFinish webView res) >>= \case
            Right texture ->
              try (Gdk.textureSaveToPng texture (T.unpack path)) >>= \case
                Right ok -> putMVar done ok
                Left (_ :: SomeException) -> putMVar done False
            Left (_ :: SomeException) -> putMVar done False
    takeMVar done
