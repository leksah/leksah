-- | A process-global queue of "close the active pane" requests.
--
-- The File ▸ Close menu item closes whichever editor or terminal tab is active,
-- which is reflex-level state in 'IDE.Web.Main'.  Menu commands, however, are
-- delivered as 'IDEAction's (the wkwebview native menu only ever runs a
-- command's 'commandAction'), so the command can't touch the reflex network
-- directly.  Instead its action drops a token here and 'IDE.Web.Main' drains the
-- queue from a background thread, turning it into a reflex 'Event'.
module IDE.Web.CloseRequest
  ( requestCloseActivePane
  , nextCloseRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE closeChan #-}
closeChan :: Chan ()
closeChan = unsafePerformIO newChan

-- | Ask for the active pane to be closed (called from the menu command action).
requestCloseActivePane :: IO ()
requestCloseActivePane = writeChan closeChan ()

-- | Block until the next close is requested (drained by the reflex bridge).
nextCloseRequest :: IO ()
nextCloseRequest = readChan closeChan
