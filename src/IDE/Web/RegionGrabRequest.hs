-- | A process-global queue of "grab a screen region" requests.
--
-- The region grab decides at run time between the system crosshair
-- (@screencapture@, needs Screen Recording permission) and a permission-free
-- in-leksah drag overlay + WKWebView snapshot; the overlay path needs the
-- front end's JS + native snapshot, so the orchestration lives in
-- 'IDE.Web.Main'.  The AI ▸ Grab Region menu command and @leksah-cmd
-- grab-region@ just drop a token here; Main drains it.  Mirrors
-- 'IDE.Web.PreferencesRequest'.
module IDE.Web.RegionGrabRequest
  ( requestRegionGrab
  , nextRegionGrab
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE regionGrabChan #-}
regionGrabChan :: Chan (Maybe Text)
regionGrabChan = unsafePerformIO newChan

-- | Ask for a region grab.  @Just target@ overrides the target pane; 'Nothing'
-- uses the 'IDE.Core.Types.regionCaptureTarget' preference.
requestRegionGrab :: Maybe Text -> IO ()
requestRegionGrab = writeChan regionGrabChan

-- | Block until the next request (drained by the reflex bridge in Main).
nextRegionGrab :: IO (Maybe Text)
nextRegionGrab = readChan regionGrabChan
