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

import Ghf.Core.State

data LogTag = LogTag | ErrorTag | FrameTag

getLog :: GhfM GhfLog
appendLog :: GhfLog -> String -> LogTag -> IO Int
markErrorInLog :: (Int,Int) -> GhfAction
clearLog :: GhfAction
