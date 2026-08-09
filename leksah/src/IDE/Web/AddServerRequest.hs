-- | A process-global queue of "open the Add Server… dialog" requests.
--
-- Adding a server (an ssh host for the 'remoteHosts' preference) is a
-- reflex-level modal in 'IDE.Web.Main'.  The native menus and the Terminals
-- tree only run plain 'IO'/'IDEAction' code, so they cannot touch the reflex
-- network directly; instead they drop a token here and 'IDE.Web.Main' drains
-- the queue from a background thread, turning it into a reflex 'Event' that
-- shows the dialog.  Mirrors 'IDE.Web.AddRemoteRequest'.
module IDE.Web.AddServerRequest
  ( requestAddServer
  , nextAddServerRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE addServerChan #-}
addServerChan :: Chan ()
addServerChan = unsafePerformIO newChan

-- | Ask for the Add Server dialog (called from menu commands and tree rows).
requestAddServer :: IO ()
requestAddServer = writeChan addServerChan ()

-- | Block until the next request (drained by the reflex bridge in "IDE.Web.Main").
nextAddServerRequest :: IO ()
nextAddServerRequest = readChan addServerChan
