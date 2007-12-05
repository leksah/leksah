{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- | The log pane og ghf
--

module Ghf.Log (
    getLog
,   appendLog
,   LogTag(..)
,   markErrorInLog
,   clearLog

) where


import Graphics.UI.Gtk

import Ghf.Core.State

data LogTag = LogTag | ErrorTag | FrameTag

initLog :: PanePath -> Notebook -> GhfAction
getLog :: GhfM GhfLog
appendLog :: GhfLog -> String -> LogTag -> IO Int
markErrorInLog :: (Int,Int) -> GhfAction
clearLog :: GhfAction
