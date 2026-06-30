-- | A process-global queue of "toggle the find bar" requests.
--
-- Edit ▸ Find shows/hides the find bar, which is reflex-level state in
-- 'IDE.Web.Main'.  The native macOS menu only runs a command's 'commandAction'
-- ('IDEAction'), so it can't touch the reflex network directly; instead the
-- command drops a token here and 'IDE.Web.Main' drains the queue from a
-- background thread, turning it into a reflex 'Event' (the in-page toolbar /
-- menubar route to the same toggle without the round-trip).  Mirrors
-- 'IDE.Web.SaveRequest' / 'IDE.Web.CloseRequest'.
module IDE.Web.FindRequest
  ( requestToggleFindbar
  , nextFindRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE findChan #-}
findChan :: Chan ()
findChan = unsafePerformIO newChan

-- | Ask for the find bar to be toggled (called from the menu command action).
requestToggleFindbar :: IO ()
requestToggleFindbar = writeChan findChan ()

-- | Block until the next find toggle is requested (drained by the reflex bridge).
nextFindRequest :: IO ()
nextFindRequest = readChan findChan
