{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- | The log pane og ide
--

module IDE.Log (
    getLog
,   appendLog
,   LogTag(..)
,   markErrorInLog
,   clearLog

) where

import IDE.Core.State

data LogTag = LogTag | ErrorTag | FrameTag

getLog :: IDEM IDELog
appendLog :: IDELog -> String -> LogTag -> IO Int
markErrorInLog :: (Int,Int) -> IDEAction
clearLog :: IDEAction
