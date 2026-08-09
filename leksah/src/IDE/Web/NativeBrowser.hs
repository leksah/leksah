-- | The seam between browser panes and REAL native per-pane web views.
--
-- On the macOS (wkwebview) front end a browser pane is not an @iframe@ — big
-- sites refuse framing (X-Frame-Options \/ CSP frame-ancestors) — but a real
-- native WKWebView overlaid on the pane's DOM rect, so nothing can refuse to
-- render.  The native side (main\/leksah-mac-menu.m) owns those views; this
-- module is the process-global registry of the operations the reflex widget
-- ("IDE.Web.Widget.Browser") needs from it, registered by the mac front end
-- at startup ('IDE.Web.MacMenu.installMacMenu').  Front ends that never
-- register (warp, webkitgtk, the ghcjs demo) leave it empty and the widget
-- falls back to the iframe implementation.
--
-- Only the DRIVING ops go through here (load\/back\/forward\/reload).  The
-- geometry loop is pure JS↔native: a per-window reporter (see
-- 'IDE.Web.Main.browserNativeReporterJs') posts every @.browser-native@
-- element's rect + visibility to the @leksahBrowserFrame@ script-message
-- handler, and the native side reconciles — creating views on first sight,
-- tracking rect\/visibility\/window moves, and destroying views whose pane
-- has left the DOM.  State flows back as @window.__lkNb[bid]@ globals the
-- widget polls.
module IDE.Web.NativeBrowser
  ( NativeBrowserOps(..)
  , setNativeBrowserOps
  , getNativeBrowserOps
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

data NativeBrowserOps = NativeBrowserOps
  { nbLoad    :: Int -> Text -> IO ()  -- ^ load a URL in pane @bid@'s view
                                       --   (created lazily by the reporter;
                                       --   a load before creation is kept
                                       --   pending and applied on creation)
  , nbBack    :: Int -> IO ()
  , nbForward :: Int -> IO ()
  , nbReload  :: Int -> IO ()
  }

{-# NOINLINE nativeBrowserOpsRef #-}
nativeBrowserOpsRef :: IORef (Maybe NativeBrowserOps)
nativeBrowserOpsRef = unsafePerformIO (newIORef Nothing)

-- | Called once by the mac front end before the UI builds.
setNativeBrowserOps :: NativeBrowserOps -> IO ()
setNativeBrowserOps = writeIORef nativeBrowserOpsRef . Just

-- | 'Nothing' = no native backing on this front end — use the iframe.
getNativeBrowserOps :: IO (Maybe NativeBrowserOps)
getNativeBrowserOps = readIORef nativeBrowserOpsRef
