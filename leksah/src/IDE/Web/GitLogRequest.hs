-- | A process-global queue of "open a git log viewer" requests.
--
-- Clicking a branch in the workspace git tree (deep inside
-- "IDE.Web.Widget.Workspace", in an @IO@ click handler) needs to open a git log
-- tab, but that widget doesn't thread an event back to the tab machinery.  So
-- the click drops a @(repo dir, branch)@ here and 'IDE.Web.Main' drains the
-- queue from a background thread into a reflex 'Event' that opens the tab —
-- mirroring "IDE.Web.OpenFileRequest".
module IDE.Web.GitLogRequest
  ( requestGitLog
  , nextGitLogRequest
  ) where

import Data.Text (Text)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE gitLogChan #-}
gitLogChan :: Chan (FilePath, Text)
gitLogChan = unsafePerformIO newChan

-- | Ask the IDE to open a git log viewer for @branch@ in the checkout at @dir@.
requestGitLog :: FilePath -> Text -> IO ()
requestGitLog dir branch = writeChan gitLogChan (dir, branch)

-- | Block until the next request is available (drained by the reflex bridge).
nextGitLogRequest :: IO (FilePath, Text)
nextGitLogRequest = readChan gitLogChan
