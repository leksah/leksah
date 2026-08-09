-- | A process-global queue of "show the Shortcuts cheat-sheet pane" requests.
--
-- The native menu bars (macOS/Win32/Gtk) can't touch the reflex-level state in
-- 'IDE.Web.Main' that opens the pane, so the native "Keyboard Shortcuts…" item
-- drops a token here and 'IDE.Web.Main' drains the queue from a background
-- thread into a reflex 'Event'.  Mirrors 'IDE.Web.PreferencesRequest'.
module IDE.Web.ShortcutsRequest
  ( requestShowShortcuts
  , nextShortcutsRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE shortcutsChan #-}
shortcutsChan :: Chan ()
shortcutsChan = unsafePerformIO newChan

-- | Ask for the Shortcuts pane to be shown (called from the native menu).
requestShowShortcuts :: IO ()
requestShowShortcuts = writeChan shortcutsChan ()

-- | Block until the next request (drained by the reflex bridge).
nextShortcutsRequest :: IO ()
nextShortcutsRequest = readChan shortcutsChan
