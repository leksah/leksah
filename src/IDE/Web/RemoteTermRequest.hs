{-# LANGUAGE OverloadedStrings #-}
-- | A process-global queue of terminal-tab open requests.
--
-- Code outside the reflex network (the @leksah-cmd cc-connect@ socket handler,
-- IDEActions run from workspace-tree buttons) asks for a terminal tab here;
-- 'IDE.Web.Main' drains the queue from a background thread into a reflex
-- 'Event' that opens (or brings up) the tab.  A request carries the final tab
-- key: @ssh:\/\/HOST[#TARGET]@ for a remote control-mode terminal, or a local
-- tmux session id (e.g. @$7@) for a session on leksah's own server.
module IDE.Web.RemoteTermRequest
  ( requestRemoteTerm
  , requestLocalTerm
  , nextTermRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE termRequestChan #-}
termRequestChan :: Chan Text
termRequestChan = unsafePerformIO newChan

-- | Ask for a remote control-mode terminal for this ssh @HOST[#TARGET]@.
requestRemoteTerm :: Text -> IO ()
requestRemoteTerm host = writeChan termRequestChan ("ssh://" <> host)

-- | Ask for (or bring up) the terminal tab of a local tmux session, by its
-- stable session id.
requestLocalTerm :: Text -> IO ()
requestLocalTerm = writeChan termRequestChan

-- | Block until the next requested tab key (drained by the reflex bridge).
nextTermRequest :: IO Text
nextTermRequest = readChan termRequestChan
