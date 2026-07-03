{-# LANGUAGE LambdaCase #-}
-- | Native colour-picker bridge.
--
-- The web @\<input type="color"\>@ popover mis-anchors inside the
-- transparent-titlebar WKWebView window, so on macOS the Preferences colour
-- swatches open the native NSColorPanel instead.  The native side is wired
-- up by the front end at startup ('setColorPickImpl' — main\/ code, so this
-- crosses the native↔reflex boundary like the other bridge modules); a
-- swatch click calls 'requestColorPick', and every change while the panel is
-- open comes back through 'colorPicked' (called from a foreign export) to
-- the most recent requester's handler.  Front ends without a native picker
-- leave the impl unset ('hasColorPickImpl' = False) and fall back to the web
-- input.
module IDE.Web.ColorPick
  ( setColorPickImpl
  , hasColorPickImpl
  , requestColorPick
  , colorPicked
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE pickImpl #-}
pickImpl :: IORef (Maybe (Text -> IO ()))
pickImpl = unsafePerformIO (newIORef Nothing)

{-# NOINLINE pickHandler #-}
pickHandler :: IORef (Text -> IO ())
pickHandler = unsafePerformIO (newIORef (const (return ())))

-- | Provide the native implementation (called once at startup; the argument
-- opens the native panel seeded with a @#rrggbb@ value).
setColorPickImpl :: (Text -> IO ()) -> IO ()
setColorPickImpl = writeIORef pickImpl . Just

-- | Is a native picker available?  (Decides which control the Preferences
-- pane renders.)
hasColorPickImpl :: IO Bool
hasColorPickImpl = isJust <$> readIORef pickImpl

-- | Open the native picker seeded with @initial@; @onPick@ receives every
-- change while the panel is open.  The panel is shared, so only the most
-- recent requester's handler is live.  'False' = no native picker.
requestColorPick :: Text -> (Text -> IO ()) -> IO Bool
requestColorPick initial onPick = readIORef pickImpl >>= \case
    Nothing   -> return False
    Just impl -> do
        writeIORef pickHandler onPick
        impl initial
        return True

-- | Called by the native side (foreign export) on every colour change.
colorPicked :: Text -> IO ()
colorPicked hex = readIORef pickHandler >>= \h -> h hex
