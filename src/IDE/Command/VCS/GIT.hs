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
    ,mkGITActions
) where

import qualified VCSGui.Git as GitGUI
import qualified VCSWrapper.Git as Git

import IDE.Command.VCS.Common

import IDE.Core.Types
import IDE.Core.State

commitAction :: VCSAction ()
commitAction = createActionFromContext GitGUI.showCommitGUI

viewLogAction :: VCSAction ()
viewLogAction = createActionFromContext GitGUI.showLogGUI

pushAction :: VCSAction ()
pushAction = createActionFromContext $ GitGUI.askPassWrapper Git.push

pullAction :: VCSAction ()
pullAction = createActionFromContext $ GitGUI.askPassWrapper GitGUI.pull

mkGITActions :: [(String, VCSAction ())]
mkGITActions = [
                ("_Commit", commitAction)
                ,("_View Log", viewLogAction)
                ,("_Push", pushAction)
                ,("_Pull", pullAction)
                ]
