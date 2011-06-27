-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.VCS.Common
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
module IDE.Command.VCS.Common (
    createActionFromContext
    ,setupRepoAction
    ,addMenuItems
) where

import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI

import IDE.Core.Types
import IDE.Core.State
import IDE.Workspaces(workspaceSetVCSConfig,addMenuItems)


import Control.Monad.Reader
import Control.Monad.Trans(liftIO)
import qualified Control.Exception as Exc



-- | shows a gui for setting up a vcs, adding menu items and persisting the created configuration
setupRepoAction :: IDEAction
setupRepoAction = do
    ide <- ask

    mbWorkspace <- readIDE workspace
    case mbWorkspace of

        Just workspace -> do
                             let config = case vcsConfig workspace of
                                                Nothing -> Nothing
                                                Just (vcs,conf) -> Just (vcs,conf)
                             liftIO $ VCSGUI.showSetupConfigGUI config (callback ide)

        Nothing -> noOpenWorkspace
    where
        callback :: IDERef -> Maybe (VCS.VCSType, VCS.Config) -> IO()
        callback ideRef mbConfig = do
                -- set config in workspace
                runReaderT (workspaceSetVCSConfig mbConfig) ideRef
                -- add menu items
                case mbConfig of
                    Nothing -> return ()
                    Just config -> runReaderT (addMenuItems config) ideRef

-- | retrievs VCS configuration from the workspace and executes given computation using it
createActionFromContext :: VCS.Ctx()    -- ^ computation to execute, i.e. showCommit
                        -> IDEAction
createActionFromContext vcsAction = do
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Just workspace -> do
             let mbConfig = vcsConfig workspace
             case mbConfig of
                Nothing -> liftIO $ VCSGUI.showErrorGUI "No active repository!"
                Just (_,config) -> liftIO $ VCSGUI.defaultVCSExceptionHandler $ VCS.runVcs config $ vcsAction
        Nothing -> noOpenWorkspace


--TODO stub, proper error handling
noOpenWorkspace = do
                    liftIO $ putStrLn "No open workspace"
                    return () --TODO show error message (use ..Common for this)
