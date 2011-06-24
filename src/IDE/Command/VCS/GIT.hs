-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.VCS.GIT
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Command.VCS.GIT (
    commitAction
    ,viewLogAction
) where

import qualified VCSGui.Git as GitGUI

import IDE.Command.VCS.Common

import IDE.Core.Types
import IDE.Core.State

commitAction :: IDEAction
commitAction = do
    createActionFromContext GitGUI.showCommitGUI

viewLogAction :: IDEAction
viewLogAction = do
    createActionFromContext GitGUI.showLogGUI
