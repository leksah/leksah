-- | A process-global queue of "wrap this fresh tmux window in a new leksah
-- window" requests.
--
-- The session-per-open helpers ('IDE.Web.ReplTmux.openTerminalInDir',
-- 'IDE.Web.Claude.runClaudeCmd') create a detached tmux session and drop its
-- @(session id, window id)@ here; 'IDE.Web.Main' drains the queue from a
-- background thread, mints the 'LeksahWindow' in the shared map and opens its
-- tab — eagerly, so the tab appears at once instead of waiting for the
-- reconcile's stray adoption on the next tree poll.  (The creators live
-- below the Core/SplitLayout layer and cannot mint themselves without an
-- import cycle; this channel is the seam.)
module IDE.Web.NewLwRequest
  ( requestNewLw
  , nextNewLwRequest
  , beginConversion
  , endConversion
  , conversionActive
  , requestFontConvert
  , nextFontConvert
  , requestConsolidate
  , nextConsolidate
  , consolidateLock
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Concurrent.MVar (MVar, newMVar)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE newLwChan #-}
newLwChan :: Chan (Text, Text)
newLwChan = unsafePerformIO newChan

-- | Ask for a leksah window (and its tab) wrapping @(session id, window id)@.
requestNewLw :: (Text, Text) -> IO ()
requestNewLw = writeChan newLwChan

-- | Block until the next request (drained by the reflex bridge in Main).
nextNewLwRequest :: IO (Text, Text)
nextNewLwRequest = readChan newLwChan

-- A minimal-path conversion is mid-flight: its break-pane children must not
-- be adopted as strays by the reconcile, which is gated on this count.
{-# NOINLINE conversionsRef #-}
conversionsRef :: IORef Int
conversionsRef = unsafePerformIO (newIORef 0)

beginConversion, endConversion :: IO ()
beginConversion = atomicModifyIORef' conversionsRef (\n -> (n + 1, ()))
endConversion   = atomicModifyIORef' conversionsRef (\n -> (max 0 (n - 1), ()))

-- | Is any conversion in flight?  (The reconcile skips a pass while so.)
conversionActive :: IO Bool
conversionActive = (> 0) <$> readIORef conversionsRef

-- | A ⌘+/⌘−/⌘0 landed on a tmux pane in a MULTI-pane window: per-pane fonts
-- need the pane isolated first (minimal-path conversion), which the pure
-- command layer cannot do — so it queues @(leksah window id, tmux window id,
-- effective-size → new override)@ here and Main's driver converts, then
-- applies the font to the isolated pane.
{-# NOINLINE fontConvertChan #-}
fontConvertChan :: Chan (Text, Text, Int -> Maybe Int)
fontConvertChan = unsafePerformIO newChan

requestFontConvert :: (Text, Text, Int -> Maybe Int) -> IO ()
requestFontConvert = writeChan fontConvertChan

nextFontConvert :: IO (Text, Text, Int -> Maybe Int)
nextFontConvert = readChan fontConvertChan

-- | "Consolidate this leksah window" requests (see Main's @consolidateLw@:
-- merge adjacent same-font tmux windows into one).  Queued by the command
-- layer after a leaf font change — the same import-cycle seam as the font
-- converts above.
{-# NOINLINE consolidateChan #-}
consolidateChan :: Chan Text
consolidateChan = unsafePerformIO newChan

requestConsolidate :: Text -> IO ()
requestConsolidate = writeChan consolidateChan

nextConsolidate :: IO Text
nextConsolidate = readChan consolidateChan

-- | Serializes consolidation passes (drop handlers, font changes and the
-- restore sweep can otherwise interleave tmux surgery on one window).
{-# NOINLINE consolidateLock #-}
consolidateLock :: MVar ()
consolidateLock = unsafePerformIO (newMVar ())
