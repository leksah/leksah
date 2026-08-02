-- | A process-global queue of commands for the active window's keymap stream.
--
-- Some 'Command's have no 'IDEAction' — they are handled inside the reflex
-- event network by pattern-matching the keymap event stream (the flipper's
-- 'CommandFlipDown'/'CommandFlipUp', 'CommandNextError'/'CommandPreviousError',
-- 'CommandFocusAlert', …).  A native menu item for such a command can't touch
-- that stream directly, so it drops the 'Command' here and 'IDE.Web.Main'
-- merges the drained queue into the same event the DOM keymap listener feeds
-- ('IDE.Web.WindowBridge' routes each token to the active window).  This is
-- what makes those shortcuts work as real native key equivalents — needed
-- whenever DOM key events can't reach the page-level keymap listener at all,
-- e.g. while keyboard focus is inside a browser pane's cross-origin iframe.
-- Mirrors 'IDE.Web.BrowserRequest', but carries the 'Command'.
module IDE.Web.KeymapRequest
  ( requestKeymapCommand
  , nextKeymapCommand
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Web.Command (Command)

{-# NOINLINE keymapChan #-}
keymapChan :: Chan Command
keymapChan = unsafePerformIO newChan

-- | Ask the active window's keymap stream to carry this command (called from
-- the native menu dispatchers for commands with no 'IDEAction').
requestKeymapCommand :: Command -> IO ()
requestKeymapCommand = writeChan keymapChan

-- | Block until the next request (drained by 'startWindowBridgeDrains').
nextKeymapCommand :: IO Command
nextKeymapCommand = readChan keymapChan
