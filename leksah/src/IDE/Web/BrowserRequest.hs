-- | A process-global queue of "open a new browser pane" requests.
--
-- The native menu bars (macOS/Win32/Gtk) can't touch the reflex-level state in
-- 'IDE.Web.Main' that opens the pane, so the native "New Browser Pane" item
-- drops a token here and 'IDE.Web.Main' drains the queue from a background
-- thread into a reflex 'Event'.  Mirrors 'IDE.Web.ShortcutsRequest'.
module IDE.Web.BrowserRequest
  ( requestOpenBrowser
  , nextBrowserRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE browserChan #-}
browserChan :: Chan ()
browserChan = unsafePerformIO newChan

-- | Ask for a new browser pane to be opened (called from the native menu).
requestOpenBrowser :: IO ()
requestOpenBrowser = writeChan browserChan ()

-- | Block until the next request (drained by the reflex bridge).
nextBrowserRequest :: IO ()
nextBrowserRequest = readChan browserChan
