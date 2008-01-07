{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.RecoverPanes
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
module IDE.RecoverPanes (
    paneStateToIDEState
) where


import IDE.Core.State
import IDE.Log()
import IDE.SourceEditor()
import IDE.ModulesPane()
import IDE.InfoPane()
import IDE.ToolbarPane()
import IDE.FindPane()



paneStateToIDEState :: PaneState -> IDEState
paneStateToIDEState (BufferSt st)                       =   StateC st
paneStateToIDEState (LogSt st)                          =   StateC st
paneStateToIDEState (InfoSt st)                         =   StateC st
paneStateToIDEState (ModulesSt st)                      =   StateC st
paneStateToIDEState (CallersSt st)                      =   StateC st
paneStateToIDEState (ToolbarSt st)                      =   StateC st
paneStateToIDEState (FindSt st)                         =   StateC st
