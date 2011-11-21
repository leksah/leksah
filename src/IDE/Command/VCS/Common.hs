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
    ,execWithErrHandling
    ,mergeToolSetter
) where

import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI
import qualified Graphics.UI.Gtk as Gtk

import IDE.Core.Types
import IDE.Core.State
import IDE.Command.VCS.Common.Workspaces
import IDE.Workspaces(workspaceSetVCSConfig
    ,workspaceSetMergeTool
    ,getVCSConfForActivePackage
    ,workspaceSetVCSConfigForActivePackage
    ,workspaceSetMergeToolForActivePackage)
import IDE.Utils.GUIUtils



import Control.Monad.Reader
import Control.Monad.Trans(liftIO)
import qualified Control.Exception as Exc
import Data.Maybe



-- | displays a window for setting up a vcs, thereafter adding menu items and persisting the created configuration
setupRepoAction :: IDEAction
setupRepoAction = do
    ide <- ask
    eConfigErr <- getVCSConfForActivePackage
    execWithErrHandling
        eConfigErr
        (\mbConfig -> liftIO $ VCSGUI.showSetupConfigGUI mbConfig (callback ide))
    where
        callback :: IDERef -> Maybe (VCS.VCSType, VCS.Config, Maybe VCSGUI.MergeTool) -> IO()
        callback ideRef mbConfig = do
                -- set config in workspace
                case mbConfig of
                    Nothing -> return() --TODO maybe allow to delete conf
                    Just config -> runReaderT (workspaceSetVCSConfigForActivePackage config) ideRef
                -- add menu items
                case mbConfig of
                    Nothing -> return ()
                    Just config -> do
                                    runReaderT onWorkspaceClose ideRef
                                    runReaderT (onWorkspaceOpen config) ideRef

-- | retrieves VCS configuration from the workspace and executes given computation using it
createActionFromContext :: VCS.Ctx()    -- ^ computation to execute, i.e. showCommit
                        -> IDEAction
createActionFromContext vcsAction = do
    eErrConf <- getVCSConfForActivePackage
    execWithErrHandling
        eErrConf
        (\mbConfig -> case mbConfig of
                        Nothing -> liftIO $ showErrorDialog "No active repository!"
                        Just (_,config, _) -> liftIO $ VCSGUI.defaultVCSExceptionHandler $ VCS.runVcs config $ vcsAction)

execWithErrHandling :: Either String (Maybe VCSConf) ->
                (Maybe VCSConf -> IDEAction)
                -> IDEAction
execWithErrHandling eErrConf fun = do
    case eErrConf of
        Left error -> liftIO $ showErrorDialog error
        Right mbConfig -> fun mbConfig

mergeToolSetter :: IDERef -> VCSGUI.MergeTool -> IO()
mergeToolSetter ideRef mergeTool = do
    -- set mergeTool in config in workspace
    runReaderT (workspaceSetMergeToolForActivePackage mergeTool) ideRef


