{-# LANGUAGE OverloadedStrings #-}
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

import IDE.Core.Types
import IDE.Core.State

import qualified IDE.Command.VCS.Common.Helper as Helper
import qualified IDE.Command.VCS.Types as Types

import qualified VCSGui.Git as GitGUI
import qualified VCSWrapper.Git as Git
import Data.Text (Text)

commitAction :: Types.VCSAction ()
commitAction = Helper.createActionFromContext GitGUI.showCommitGUI

viewLogAction :: Types.VCSAction ()
viewLogAction = Helper.createActionFromContext GitGUI.showLogGUI

pushAction :: Types.VCSAction ()
pushAction = Helper.createActionFromContext $ GitGUI.askPassWrapper Git.push

pullAction :: Types.VCSAction ()
pullAction = Helper.createActionFromContext $ GitGUI.askPassWrapper GitGUI.pull

mkGITActions :: [(Text, Types.VCSAction ())]
mkGITActions = [
                ("_Commit", commitAction)
                ,("_View Log", viewLogAction)
                ,("_Push", pushAction)
                ,("_Pull", pullAction)
                ]
