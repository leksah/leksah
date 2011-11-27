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
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module IDE.Command.VCS.Common (
    createActionFromContext
    ,setupRepoAction'
    ,execWithErrHandling
    ,mergeToolSetter
    ,runActionWithContext
    ,getVCSConf'

    ,VCSAction
) where

import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI

import IDE.Core.Types
import IDE.Core.State
import qualified IDE.Workspaces.Writer as Writer
import IDE.Utils.GUIUtils

import qualified Graphics.UI.Gtk as Gtk
import Control.Monad.Reader
import Control.Monad.Trans(liftIO)
import qualified Control.Exception as Exc
import Data.Maybe
import qualified Data.Map as Map

newtype VCSAction a = VCSAction (ReaderT VCS.Config IDEM a)
    deriving (Monad, MonadIO, MonadReader VCS.Config)

-- | retrieves VCS configuration from the workspace and executes given computation using it
runActionWithContext :: VCSAction ()    -- ^ computation to execute, i.e. showCommit
                        -> FilePath
                        -> IDEAction
runActionWithContext vcsAction packageFp = do
    (_,config,_) <- getVCSConf'' packageFp
    runVcs config $ vcsAction
    where
    runVcs :: VCS.Config -> VCSAction t -> IDEM t
    runVcs config (VCSAction a) = runReaderT a config

--TODO implement this and refactor/delete below
setupRepoAction' :: VCSAction ()
setupRepoAction' = return ()



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
                                    return()
                                    --TODO set vcs items here
--                                    runReaderT onWorkspaceClose ideRef
--                                    runReaderT (onWorkspaceOpen config) ideRef

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


workspaceSetVCSConfig :: FilePath -> VCSConf -> IDEAction
workspaceSetVCSConfig pathToPackage vcsConf = do
    modifyIDE_ (\ide -> do
        let oldWs = fromJust (workspace ide)
        let oldMap = packageVcsConf oldWs
        let newMap = Map.insert pathToPackage vcsConf oldMap
        let newWs = (fromJust (workspace ide)) { packageVcsConf = newMap }
        ide {workspace = Just newWs })
    newWs <- readIDE workspace
    Writer.writeWorkspace $ fromJust newWs

workspaceSetVCSConfigForActivePackage :: VCSConf -> IDEAction
workspaceSetVCSConfigForActivePackage vcsConf = do
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Nothing -> liftIO $ putStrLn $ "Could not set vcs config. No open workspace. Open Workspace first."
        Just workspace -> do
            let mbPathToActivePackage = wsActivePackFile workspace
            case mbPathToActivePackage of
                Nothing -> liftIO $ putStrLn $ "Could not set vcs config. No active package."
                Just pathToActivePackage -> workspaceSetVCSConfig pathToActivePackage vcsConf


workspaceSetMergeToolForActivePackage :: VCSGUI.MergeTool -> IDEAction
workspaceSetMergeToolForActivePackage mergeTool = do
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Nothing -> liftIO $ putStrLn $ "Could not set mergetool. No open workspace. Open Workspace first."
        Just workspace -> do
            let mbPathToActivePackage = wsActivePackFile workspace
            case mbPathToActivePackage of
                Nothing -> liftIO $ putStrLn $ "Could not set mergetool. No active package."
                Just pathToActivePackage -> workspaceSetMergeTool pathToActivePackage mergeTool

workspaceSetMergeTool :: FilePath -> VCSGUI.MergeTool -> IDEAction
workspaceSetMergeTool pathToPackage mergeTool = do
    modifyIDE_ (\ide -> do
        let oldWs = fromJust (workspace ide)
        let oldMap = packageVcsConf oldWs
        case (Map.lookup pathToPackage oldMap) of
            Nothing -> ide --TODO error
            Just (vcsType,config,_) -> do
                let vcsConf = (vcsType,config,Just mergeTool)
                let newMap = Map.insert pathToPackage vcsConf oldMap
                let newWs = oldWs { packageVcsConf = newMap }
                ide {workspace = Just newWs })
    newWs <- readIDE workspace
    Writer.writeWorkspace $ fromJust newWs

type GetVCSConfReturn = IDEM (Either String (Maybe VCSConf))

-- ^ returns Right configuration for active package or Left description of failure if configuration could not be retrieven
getVCSConfForActivePackage :: GetVCSConfReturn
getVCSConfForActivePackage = do
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Nothing -> return $ Left $ "No open workspace. Open Workspace first."
        Just workspace -> getVCSConfForActivePackage' workspace

-- ^ returns Right configuration for active package or Left description of failure if configuration could not be retrieven
getVCSConfForActivePackage' :: Workspace -> GetVCSConfReturn
getVCSConfForActivePackage' workspace = do
    case (wsActivePackFile workspace) of
        Nothing -> return $ Left $ "Could not find active package for workspace."
        Just pathToActivePackage -> do
            getVCSConf' workspace pathToActivePackage

-- ^ returns Right configuration for active package or Left description of failure if configuration could not be retrieven
getVCSConf :: FilePath -> GetVCSConfReturn
getVCSConf pathToPackage = do
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Nothing -> return $ Left $ "No open workspace. Open Workspace first."
        Just workspace -> getVCSConf' workspace pathToPackage

-- ^ returns Right configuration for active package or Left description of failure if configuration could not be retrieven
getVCSConf' :: Workspace -> FilePath -> GetVCSConfReturn
getVCSConf' workspace pathToPackage = do
            let mbConfig = Map.lookup pathToPackage $ packageVcsConf workspace
            case mbConfig of
            --Left $ "Could not find version-control-system configuration for package "++pathToPackage
                Nothing -> return $ Right $ Nothing
                Just conf -> return $ Right $ Just conf

-- ^ returns vcs configuration, assuming workspace is set and configuration is there
getVCSConf'' :: FilePath -> IDEM VCSConf
getVCSConf'' pathToPackage = do
    (Just workspace) <- readIDE workspace
    return $ fromJust $ Map.lookup pathToPackage $ packageVcsConf workspace

