-- | A process-global queue of "save the active file" requests.
--
-- File ▸ Save (and the toolbar Save button) saves whichever editor tab is
-- active, which is reflex-level state in 'IDE.Web.Main'.  The native macOS menu
-- only ever runs a command's 'commandAction' ('IDEAction'), so it can't touch
-- the reflex network directly; instead the command drops a token here and
-- 'IDE.Web.Main' drains the queue from a background thread, turning it into a
-- reflex 'Event' (the in-page toolbar/menubar route to the same event without
-- the round-trip).  Mirrors 'IDE.Web.CloseRequest'.
--
-- A request may carry a completion 'MVar': 'requestSaveActiveFileWait' blocks
-- until the save has actually settled (the editor's write handler acks it via
-- Main's routing — or Main acks at once when nothing needed saving), so
-- save-then-act sequences (⌘D opening a terminal beside a dirty editor) need
-- no guessed sleeps.
module IDE.Web.SaveRequest
  ( requestSaveActiveFile
  , requestSaveActiveFileWait
  , nextSaveRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Monad (void)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

{-# NOINLINE saveChan #-}
saveChan :: Chan (Maybe (MVar ()))
saveChan = unsafePerformIO newChan

-- | Ask for the active editor to be saved (called from the menu command
-- action).  Fire and forget.
requestSaveActiveFile :: IO ()
requestSaveActiveFile = writeChan saveChan Nothing

-- | Ask for the active editor to be saved and BLOCK until the write settled
-- (success or failure — the caller only needs "the disk is as current as it
-- is going to get").  The timeout is a failsafe against a torn-down network
-- never acking, not pacing: the normal path completes in milliseconds.
requestSaveActiveFileWait :: IO ()
requestSaveActiveFileWait = do
    done <- newEmptyMVar
    writeChan saveChan (Just done)
    void $ timeout 2000000 (takeMVar done)

-- | Block until the next save is requested (drained by the reflex bridge);
-- yields the completion slot to ack, if the caller is waiting.
nextSaveRequest :: IO (Maybe (MVar ()))
nextSaveRequest = readChan saveChan
