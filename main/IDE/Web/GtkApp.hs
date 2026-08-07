{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | The native GTK4 windows for leksah-webkitgtk: one or more
-- GtkApplicationWindows, each hosting a WebKitGTK 6.0 WebView that renders the
-- leksah web UI (served by the warp instance 'IDE.Web.Main.startJSaddle'
-- forks).  The Linux sibling of the AppKit windows leksah-wkwebview gets from
-- jsaddle-wkwebview.
--
-- Multi-window mirrors the macOS ('IDE.Web.MacMenu') design: the shared UI
-- cell ('IDE.Web.Model.webWindows' on the 'App') holds one 'WebWindow' per OS
-- window, each window runs its OWN reflex network ('IDE.Web.Main.jsMain') in
-- its own jsaddle context over that WebView, and the networks coordinate
-- through the shared cells (see 'IDE.Web.WindowBridge').  Only the native
-- window/WebView creation is platform-specific and lives here; everything else
-- (WindowBridge registration, the close-merge) is shared.
--
-- File ▸ New Window mints a 'WindowId' and opens another window; window close
-- merges its tabs into a survivor via the shared 'closeWindowMerge'; the global
-- flipper's cross-window raise brings a window to the front.  These native
-- hooks are registered here through 'IDE.Web.NewWindowRequest' — the same
-- process-global slots the macOS glue registers — so the shared New Window
-- command and flipper drive them with no platform branching.
module IDE.Web.GtkApp
  ( runGtkApp
  ) where

import Control.Exception (SomeException, try)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Lens ((?~), view)
import Control.Monad (forM_, unless, void, when)

import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import qualified Data.Map as M
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8)

import Data.GI.Base (get, on, SignalProxy(PropertyNotify))
import qualified GI.Gdk as Gdk (textureSaveToPng)
import qualified GI.Gio as Gio
       (ApplicationFlags(..), Cancellable, applicationQuit, applicationRun,
        onApplicationActivate)
import qualified GI.Gtk as Gtk
       (Application, ApplicationWindow, applicationNew, applicationWindowNew,
        onWindowCloseRequest, windowSetTitle, windowSetDefaultSize,
        windowSetChild, windowPresent)
import qualified GI.WebKit as WK
       (LoadEvent(..), SnapshotOptions(..), SnapshotRegion(..), WebView,
        onWebViewLoadChanged, setSettingsEnableDeveloperExtras,
        setSettingsEnableJavascript,
        setSettingsEnableWriteConsoleMessagesToStdout, webViewGetSettings,
        webViewGetSnapshot, webViewGetSnapshotFinish, webViewLoadHtml,
        webViewNew, webViewSetSettings)

import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.WebKitGTK (runInWebView)

import IDE.App (App, appUi, getGlobalApp)
import IDE.Reactive (modifyCell, readCell)
import IDE.Web.Model (WindowId(..), activeWindow, webWindows)
import IDE.Web.GtkMenu (installGtkMenu, postGUIAsync)
import IDE.Web.Main (jsMain, mintWindowId)
import IDE.Web.NewWindowRequest
       (setNewWindowHandler, setOpenWindowHandler, setRaiseWindowHandler)
import IDE.Web.ScreenshotRequest (registerScreenshotHandler)
import IDE.Web.WindowBridge (closeWindowMerge)

-- | Run the leksah UI in GTK4 windows.  Matches the runner signature
-- 'IDE.Web.Main.startJSaddle' expects; the @jsm@ it is handed (a 'jsMain'
-- pinned to 'WindowId' 0) is intentionally unused — this back end builds a
-- fresh per-window 'jsMain' for every OS window (including window 0) from the
-- shared 'App' instead, so New Window and session restore go through exactly
-- the same path.  The index HTML is loaded with the warp server's URL as base,
-- so relative fetches (xterm assets, terminal iframes) resolve against it — the
-- GTK counterpart of wkwebview's @runHTMLWithBaseURL@.
runGtkApp :: ByteString -> ByteString -> JSM () -> IO ()
runGtkApp html url _jsm = do
  -- NonUnique: the develop-leksah relaunch (exit 2 + respawn) must start a
  -- fresh instance, not forward its activation to the dying one.
  app <- Gtk.applicationNew (Just "org.leksah.Leksah")
                            [Gio.ApplicationFlagsNonUnique]
  -- WindowId -> its GtkApplicationWindow, so the flipper's raise and the close
  -- handler can find a window by id.  Touched only on the GTK main thread
  -- (window open/close) and from the raise handler (marshalled there via
  -- 'postGUIAsync'), so an IORef with atomic updates suffices.
  windowsRef <- newIORef M.empty
  _ <- Gio.onApplicationActivate app $ getGlobalApp >>= \case
    Nothing     -> return ()   -- newIDE always publishes the App before runJs
    Just theApp -> do
      -- Register the native window hooks the shared New Window command / flipper
      -- drive.  All three must touch GTK on the main loop, so marshal there.
      setNewWindowHandler   $ postGUIAsync (openWindow app html url windowsRef theApp Nothing)
      setOpenWindowHandler  $ \n -> postGUIAsync
                                (openWindow app html url windowsRef theApp (Just (WindowId n)))
      setRaiseWindowHandler $ \n -> postGUIAsync $
        readIORef windowsRef >>= mapM_ Gtk.windowPresent . M.lookup (WindowId n)
      -- Open a native window for every window the shared state was seeded with
      -- (window 0 plus any restored by 'newIDE').  Independent of the earlier
      -- 'requestOpenWindow' calls, which no-op'd because no handler was set yet.
      seeded <- view webWindows <$> readCell (appUi theApp)
      case M.keys seeded of
        []   -> openWindow app html url windowsRef theApp Nothing
        wids -> forM_ wids $ \wid -> openWindow app html url windowsRef theApp (Just wid)
  void $ Gio.applicationRun app Nothing

-- | Create one native GTK4 window + WebKitGTK WebView and attach a fresh reflex
-- network to it.  With 'Nothing' a new 'WindowId' is minted (File ▸ New Window);
-- with 'Just' an already-seeded id is adopted (window 0 / session restore).
-- Must run on the GTK main thread.
openWindow :: Gtk.Application -> ByteString -> ByteString
           -> IORef (M.Map WindowId Gtk.ApplicationWindow) -> App
           -> Maybe WindowId -> IO ()
openWindow app html url windowsRef theApp mbWid = do
  wid <- maybe (mintWindowId theApp) return mbWid
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
  atomicModifyIORef' windowsRef (\m -> (M.insert wid win m, ()))
  -- Track the frontmost window (the AppKit 'leksah_window_activated' analog): on
  -- each is-active transition, if this window just gained focus, record it as the
  -- active window so the process-wide bridges (Close/Save/Find) and the flipper's
  -- in-place actions target it.  ('is-active' is a GtkWindow property; haskell-gi
  -- surfaces its changes only through PropertyNotify.)
  _ <- on win (PropertyNotify #isActive) $ \_ -> do
         active <- get win #isActive
         when active $ setActiveWindow theApp wid
  -- Window close: merge this window's tabs into a survivor (shared logic); the
  -- last window quits the GTK app.  Returning False lets the default close
  -- proceed (True would veto it).
  _ <- Gtk.onWindowCloseRequest win $ do
         closeWindowMerge (Gio.applicationQuit app) wid
         atomicModifyIORef' windowsRef (\m -> (M.delete wid m, ()))
         return False
  -- runInWebView registers the "jsaddle" script-message handler, which must
  -- only happen once — a page reload would fire LoadEventFinished again (and
  -- leave a dead bridge, same as wkwebview: reload needs a restart).  Each
  -- window gets its OWN jsMain pinned to its 'WindowId'; jsMain adopts the id,
  -- registers this window's WindowBridge + resync, and renders its wide0.
  started <- newIORef False
  _ <- WK.onWebViewLoadChanged webView $ \case
    WK.LoadEventFinished -> do
      done <- atomicModifyIORef' started (True,)
      unless done $ runInWebView (jsMain False False (Just wid) theApp) webView
    _ -> return ()
  WK.webViewLoadHtml webView (decodeUtf8 html) (Just (decodeUtf8 url))
  Gtk.windowPresent win

-- | Record @wid@ as the active (frontmost) OS window in the shared state.
setActiveWindow :: App -> WindowId -> IO ()
setActiveWindow theApp wid =
  modifyCell (appUi theApp) (activeWindow ?~ wid)

-- | @leksah-cmd screenshot FILE@: snapshot the WebView to a PNG.  The request
-- arrives on a CmdServer thread; the snapshot must run on the GTK main loop,
-- so marshal over and block on the result.  Process-global (last window to
-- register wins) — good enough for the dev screenshot tool.
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
