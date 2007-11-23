{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Ghf.RecoverPanes
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- |
--
-------------------------------------------------------------------------------
module Ghf.RecoverPanes (
) where

import Data.Maybe

import Ghf.Core.State
import Ghf.Log
import Ghf.SourceEditor
import Ghf.ModulesPane
import Ghf.InfoPane

--instance RecoverablePane GhfPane PaneState where
--    saveState (PaneC p)             =   saveState p
--    recoverState pp (BufferSt s)    =   do recoverState pp s; return Nothing
--    recoverState pp (LogSt s)       =   do recoverState pp s; return Nothing
--    recoverState pp (InfoSt s)      =   do recoverState pp s; return Nothing
--    recoverState pp (ModulesSt s)   =   do recoverState pp s; return Nothing
