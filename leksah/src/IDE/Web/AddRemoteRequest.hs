-- | A process-global queue of "open the Add Remote Project… dialog" requests.
--
-- Adding a remote project is a reflex-level modal in 'IDE.Web.Main'.  The
-- native macOS menu only runs a command's 'commandAction' ('IDEAction'), so it
-- cannot touch the reflex network directly; instead the @CommandProjectAddRemote@
-- menu command drops a token here and 'IDE.Web.Main' drains the queue from a
-- background thread, turning it into a reflex 'Event' that shows the dialog (the
-- in-page web menubar route to the same dialog without the round-trip).
-- Mirrors 'IDE.Web.FindRequest' / 'IDE.Web.SaveRequest'.
module IDE.Web.AddRemoteRequest
  ( requestAddRemoteProject
  , nextAddRemoteRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE addRemoteChan #-}
addRemoteChan :: Chan ()
addRemoteChan = unsafePerformIO newChan

-- | Ask for the Add Remote Project dialog (called from the menu command action).
requestAddRemoteProject :: IO ()
requestAddRemoteProject = writeChan addRemoteChan ()

-- | Block until the next request (drained by the reflex bridge in "IDE.Web.Main").
nextAddRemoteRequest :: IO ()
nextAddRemoteRequest = readChan addRemoteChan
