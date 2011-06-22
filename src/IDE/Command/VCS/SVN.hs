-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.VCS.SVN
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

module IDE.Command.VCS.SVN (
    checkoutAction
    ,commitAction
    ,setupRepoAction
    ,updateAction
    ,viewLogAction

) where

import qualified VCSGui.Svn as SvnGUI
import qualified VCSWrapper.Svn as SvnC

import IDE.Command.VCS.Common

import IDE.Core.Types
import IDE.Core.State
import IDE.Workspaces(workspaceSetVCSConfig)

import Control.Monad.Reader
import Control.Monad.Trans(liftIO)

checkoutAction :: IDEAction
checkoutAction = do
    createActionFromContext' SvnGUI.showCheckoutGUI

commitAction :: IDEAction
commitAction = do
    createActionFromContext' SvnGUI.showCommitGUI

setupRepoAction :: IDEAction
setupRepoAction = do
    ide <- ask
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Just workspace -> do
                             liftIO $ SvnGUI.showSetupConfigGUI (vcsConfig workspace) (setRepo ide)
        Nothing -> return () --TODO show error message (use ..Common for this)
    where
        setRepo :: IDERef -> Maybe (SvnC.VCSType, SvnC.Config) -> IO ()
        setRepo ide mbConfig = do
                        let config = case mbConfig of
                                        Nothing -> Nothing
                                        Just (vcsType,conf) -> Just conf
                        runReaderT (workspaceSetVCSConfig config) ide -- TODO fix this
--        setRepo ide mbConfig = runReaderT (workspaceSetVCSConfig mbConfig) ide

updateAction :: IDEAction
updateAction = do
    createActionFromContext' SvnC.update

viewLogAction :: IDEAction
viewLogAction = do
    createActionFromContext' SvnGUI.showLogGUI

createActionFromContext' ctxt = createActionFromContext ctxt SvnGUI.showErrorGUI



