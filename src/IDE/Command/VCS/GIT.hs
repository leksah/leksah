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
    ,pushAction
    ,pullAction
) where

import qualified VCSGui.Git as GitGUI
import qualified VCSWrapper.Git as Git

import IDE.Command.VCS.Common

import IDE.Core.Types
import IDE.Core.State

commitAction :: IDEAction
commitAction = return()
--commitAction = createActionFromContext GitGUI.showCommitGUI

viewLogAction :: IDEAction
viewLogAction = return()
--viewLogAction = createActionFromContext GitGUI.showLogGUI

pushAction :: IDEAction
pushAction = return()
--pushAction = createActionFromContext $ GitGUI.askPassWrapper Git.push

pullAction :: IDEAction
pullAction = return()
--pullAction = createActionFromContext $ GitGUI.askPassWrapper GitGUI.pull
