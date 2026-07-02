{-# LANGUAGE OverloadedStrings #-}
-- | A process-global queue of remote control-mode terminal requests.
--
-- @leksah-cmd cc-connect HOST@ (see 'IDE.Web.CmdServer') asks for a terminal
-- tab attached to HOST's tmux over ssh in control mode (rendered natively by
-- 'IDE.Web.Widget.TerminalCC').  The socket handler runs outside the reflex
-- network, so the host is dropped here and 'IDE.Web.Main' drains the queue
-- from a background thread into a reflex 'Event' that opens the tab (keyed
-- @TerminalKey (\"ssh:\/\/\" <> host)@).
module IDE.Web.RemoteTermRequest
  ( requestRemoteTerm
  , nextRemoteTerm
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE remoteTermChan #-}
remoteTermChan :: Chan Text
remoteTermChan = unsafePerformIO newChan

-- | Ask for a remote control-mode terminal for this ssh host.
requestRemoteTerm :: Text -> IO ()
requestRemoteTerm = writeChan remoteTermChan

-- | Block until the next requested host (drained by the reflex bridge).
nextRemoteTerm :: IO Text
nextRemoteTerm = readChan remoteTermChan
