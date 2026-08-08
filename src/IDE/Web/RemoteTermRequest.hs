{-# LANGUAGE OverloadedStrings #-}
-- | A process-global queue of terminal-tab open requests.
--
-- Code outside the reflex network (the @leksah-cmd cc-connect@ socket handler,
-- IDEActions run from workspace-tree buttons, the ⌘-drag drop commit) asks for
-- a terminal tab here; 'IDE.Web.Main' drains the queue from a background thread
-- into a reflex 'Event' that opens (or brings up) the tab.  A request carries
-- the final tab key: @ssh:\/\/HOST[#TARGET]@ for a remote control-mode
-- terminal, or a local tmux session id (e.g. @$7@) for a session on leksah's
-- own server.
--
-- __Why a broadcast and not a queue.__  Every OS window runs its own copy of
-- the drain, and a plain 'Control.Concurrent.Chan.Chan' hands each item to
-- exactly ONE of them — whichever reader happened to be first in the queue.
-- Since acting on a request 'IDE.Web.Model.moveTabTo's the tab into the acting
-- window, that made "bring this tab up" MOVE the tab to an arbitrary OS window.
-- The visible bug: cancelling a ⌘-drag (which asks for the source's own tab
-- back, expecting nothing to change) threw the pane into the other window.
--
-- So requests are BROADCAST — every window sees every one — and each window
-- decides for itself whether it is the one to act, from shared state both
-- windows read the same way (see @termRequestMineE@ in "IDE.Web.Main"): the
-- window that already owns the tab serves it, and only a tab no window owns
-- goes to the active window.  Bringing up an existing tab therefore never
-- moves it, exactly like the cross-window flipper.
module IDE.Web.RemoteTermRequest
  ( requestRemoteTerm
  , requestLocalTerm
  , dupTermRequests
  , nextTermRequest
  ) where

import Control.Concurrent.STM
       (TChan, atomically, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

-- | The broadcast end.  Never read from directly (a broadcast 'TChan' throws
-- if you try): 'dupTermRequests' gives each window its own reading copy, and
-- anything written while no window has duped is dropped rather than piling up.
{-# NOINLINE termRequestChan #-}
termRequestChan :: TChan Text
termRequestChan = unsafePerformIO newBroadcastTChanIO

-- | Ask for a remote control-mode terminal for this ssh @HOST[#TARGET]@.
requestRemoteTerm :: Text -> IO ()
requestRemoteTerm host = atomically $ writeTChan termRequestChan ("ssh://" <> host)

-- | Ask for (or bring up) the terminal tab of a local tmux session, by its
-- stable session id — or of a leksah window, by its id.
requestLocalTerm :: Text -> IO ()
requestLocalTerm = atomically . writeTChan termRequestChan

-- | One window's reading copy of the broadcast.  Call once per window, at
-- attach; requests written before the call are not seen by it.
dupTermRequests :: IO (TChan Text)
dupTermRequests = atomically (dupTChan termRequestChan)

-- | Block until the next requested tab key arrives on this window's copy.
nextTermRequest :: TChan Text -> IO Text
nextTermRequest = atomically . readTChan
