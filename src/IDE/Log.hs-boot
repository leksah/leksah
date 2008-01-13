{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- | The log pane og ide
--

module IDE.Log (
    LogView(..)
,   IDELog
,   LogState
,   LogTag(..)
) where

import {-# SOURCE #-} IDE.Core.State

data LogTag = LogTag | ErrorTag | FrameTag
instance LogView IDELog

class IDEPaneC alpha => LogView alpha where
    getLog          ::   IDEM alpha
    appendLog       ::   alpha  -> String -> LogTag -> IO Int
    markErrorInLog  ::   alpha  -> (Int, Int) -> IO ()

data IDELog
data LogState               =   LogState
    deriving(Eq,Ord,Read,Show)
