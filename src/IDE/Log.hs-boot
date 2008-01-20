{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- Module      :  IDE.Log
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Log pane
--
-------------------------------------------------------------------------------


module IDE.Log (
    LogView(..)
,   LogAction(..)
,   IDELog(..)
,   LogState
) where

import IDE.Core.State


-------------------------------------------------------------------------------
--
-- * Interface
--

--
-- | The Log Viev
--

class LogAction alpha where
    clearLog        ::   alpha

instance LogAction IDEAction

class IDEPaneC alpha => LogView alpha where
    getLog          ::   IDEM alpha
    appendLog       ::   alpha  -> String -> LogTag -> IO Int
    markErrorInLog  ::   alpha  -> (Int, Int) -> IO ()

instance IDEObject IDELog
instance IDEPaneC IDELog
instance LogView IDELog
instance CastablePane IDELog
instance Recoverable LogState
instance Pane IDELog
instance RecoverablePane IDELog LogState
