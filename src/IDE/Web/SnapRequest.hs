-- | A process-global queue of window-snap requests.
--
--   * 'requestSnapWindow' — from the Tmux ▸ Snap Window to Pane menu: toggle
--     snapping a click-picked window onto the *active* tmux pane.
--   * 'requestSnapPane' — from @leksah-cmd open-browser@: snap the frontmost
--     window (the just-opened browser) onto a *specific* tmux pane, given its
--     @pane_id@ (e.g. @%20@, from @$TMUX_PANE@).
--
-- Both make the target pane transparent so the window shows through (macOS; see
-- @main/leksah-mac-menu.m@).  Menu commands and the control socket run outside
-- the reflex network, so they drop a request here and 'IDE.Web.Main' drains the
-- queue from a background thread into a reflex 'Event' (as 'IDE.Web.CloseRequest').
module IDE.Web.SnapRequest
  ( SnapReq(..)
  , requestSnapWindow
  , requestSnapPane
  , requestUnsnapPane
  , nextSnapRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

-- | A snap request: the active pane (menu, click-to-pick), a specific pane by
-- its tmux @pane_id@ (the @open-browser@ command, frontmost-window bind), or an
-- unsnap of a specific pane by its @\"tid:pid\"@ key (the native Unsnap menu).
data SnapReq = SnapActive | SnapPane Text | SnapUnsnap Text

{-# NOINLINE snapChan #-}
snapChan :: Chan SnapReq
snapChan = unsafePerformIO newChan

-- | Toggle snapping a click-picked window onto the active pane (menu command).
requestSnapWindow :: IO ()
requestSnapWindow = writeChan snapChan SnapActive

-- | Snap the frontmost window onto the pane with the given tmux @pane_id@
-- (@open-browser@).
requestSnapPane :: Text -> IO ()
requestSnapPane = writeChan snapChan . SnapPane

-- | Unsnap the window bound to the pane with the given @\"tid:pid\"@ key (the
-- native Tmux ▸ Underlay ▸ Unsnap menu).
requestUnsnapPane :: Text -> IO ()
requestUnsnapPane = writeChan snapChan . SnapUnsnap

-- | Block until the next snap is requested (drained by the reflex bridge).
nextSnapRequest :: IO SnapReq
nextSnapRequest = readChan snapChan
