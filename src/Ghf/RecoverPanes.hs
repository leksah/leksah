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
    paneStateToGhfState
) where

import Data.Maybe

import Ghf.Core.State
import Ghf.Log
import Ghf.SourceEditor
import Ghf.ModulesPane
import Ghf.InfoPane


paneStateToGhfState :: PaneState -> GhfState
paneStateToGhfState (BufferSt st)                       =   StateC st
paneStateToGhfState (LogSt st)                          =   StateC st
paneStateToGhfState (InfoSt st)                         =   StateC st
paneStateToGhfState (ModulesSt st)                      =   StateC st

