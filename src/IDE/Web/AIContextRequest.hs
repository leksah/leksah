-- | A process-global queue of "send some editor context to the AI terminal"
-- requests.
--
-- The AI menu commands ('IDE.Web.Command') just drop an 'AIAction' token here;
-- the front end ('IDE.Web.Main') drains it, where it has both the live jsaddle
-- context (to read the active CodeMirror editor's file + selection) and the IDE
-- state (to read the current error), builds the text and types it into the
-- 'IDE.Core.Types.regionCaptureTarget' pane.  Mirrors
-- 'IDE.Web.RegionGrabRequest'.
module IDE.Web.AIContextRequest
  ( AIAction(..)
  , requestAIAction
  , nextAIAction
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

-- | What the AI menu asked to do with the current context.
data AIAction
  = SendSelection    -- ^ the active editor's selection as @\@file#Lx-Ly@
  | SendFileRef      -- ^ the active editor's file as @\@file@
  | SendError        -- ^ the current error (location + message)
  | FocusAITerminal  -- ^ focus the target terminal pane
  deriving (Eq, Show)

{-# NOINLINE aiActionChan #-}
aiActionChan :: Chan AIAction
aiActionChan = unsafePerformIO newChan

-- | Ask the front end to act on the current context.
requestAIAction :: AIAction -> IO ()
requestAIAction = writeChan aiActionChan

-- | Block until the next request (drained by the reflex bridge in Main).
nextAIAction :: IO AIAction
nextAIAction = readChan aiActionChan
