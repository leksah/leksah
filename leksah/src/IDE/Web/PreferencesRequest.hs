-- | A process-global queue of "show the Preferences pane" requests.
--
-- On macOS the Settings… item lives in the application menu and is handled
-- natively; opening the Preferences pane is reflex-level state in
-- 'IDE.Web.Main', which the native menu can't touch directly.  So the native
-- item drops a token here and 'IDE.Web.Main' drains the queue from a
-- background thread into a reflex 'Event'.  Mirrors 'IDE.Web.FindRequest'.
module IDE.Web.PreferencesRequest
  ( requestShowPreferences
  , nextPreferencesRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE prefsChan #-}
prefsChan :: Chan ()
prefsChan = unsafePerformIO newChan

-- | Ask for the Preferences pane to be shown (called from the native menu).
requestShowPreferences :: IO ()
requestShowPreferences = writeChan prefsChan ()

-- | Block until the next request (drained by the reflex bridge).
nextPreferencesRequest :: IO ()
nextPreferencesRequest = readChan prefsChan
