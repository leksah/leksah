-- | "Is the Command key held right now?" — a question the page can't answer
-- without a key or mouse event, but the native front end can (macOS:
-- @CGEventSourceFlagsState@).  A process-global probe in the bridge-module
-- style ("IDE.Web.CloseRequest" etc.): the library asks 'cmdHeldNow', and the
-- macOS executable installs the real check at startup with
-- 'setCmdHeldProbe'.  Elsewhere it stays 'Nothing' ("can't tell").
--
-- Used to dismiss the tab flipper when its ⌘ keyup never arrives (⌘-Tab took
-- it), polled only while the flipper is up.
module IDE.Web.ModifierProbe
  ( setCmdHeldProbe
  , cmdHeldNow
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE probeRef #-}
probeRef :: IORef (IO (Maybe Bool))
probeRef = unsafePerformIO (newIORef (return Nothing))

-- | Install the native check (macOS executable, at startup).
setCmdHeldProbe :: IO Bool -> IO ()
setCmdHeldProbe probe = writeIORef probeRef (Just <$> probe)

-- | Whether ⌘ is held right now; 'Nothing' when this front end can't tell.
cmdHeldNow :: IO (Maybe Bool)
cmdHeldNow = readIORef probeRef >>= id
