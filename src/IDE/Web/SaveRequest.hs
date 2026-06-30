-- | A process-global queue of "save the active file" requests.
--
-- File ▸ Save (and the toolbar Save button) saves whichever editor tab is
-- active, which is reflex-level state in 'IDE.Web.Main'.  The native macOS menu
-- only ever runs a command's 'commandAction' ('IDEAction'), so it can't touch
-- the reflex network directly; instead the command drops a token here and
-- 'IDE.Web.Main' drains the queue from a background thread, turning it into a
-- reflex 'Event' (the in-page toolbar/menubar route to the same event without
-- the round-trip).  Mirrors 'IDE.Web.CloseRequest'.
module IDE.Web.SaveRequest
  ( requestSaveActiveFile
  , nextSaveRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE saveChan #-}
saveChan :: Chan ()
saveChan = unsafePerformIO newChan

-- | Ask for the active editor to be saved (called from the menu command action).
requestSaveActiveFile :: IO ()
requestSaveActiveFile = writeChan saveChan ()

-- | Block until the next save is requested (drained by the reflex bridge).
nextSaveRequest :: IO ()
nextSaveRequest = readChan saveChan
