-- | A process-global queue of "toggle the active tmux pane's transparency"
-- requests.
--
-- The Tmux ▸ Toggle Pane Transparency menu item flips whether the active
-- terminal's active tmux pane is rendered as a see-through, click-through hole.
-- That is reflex-level state in 'IDE.Web.Main', but menu commands are delivered
-- as 'IDEAction's (the wkwebview native menu only runs a command's
-- 'commandAction'), so the command can't touch the reflex network directly.
-- Instead its action drops a token here and 'IDE.Web.Main' drains the queue from
-- a background thread, turning it into a reflex 'Event' — exactly like
-- 'IDE.Web.CloseRequest'.
module IDE.Web.TransparencyRequest
  ( requestToggleTransparency
  , nextToggleTransparency
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE toggleChan #-}
toggleChan :: Chan ()
toggleChan = unsafePerformIO newChan

-- | Ask to toggle the active pane's transparency (called from the menu command).
requestToggleTransparency :: IO ()
requestToggleTransparency = writeChan toggleChan ()

-- | Block until the next toggle is requested (drained by the reflex bridge).
nextToggleTransparency :: IO ()
nextToggleTransparency = readChan toggleChan
