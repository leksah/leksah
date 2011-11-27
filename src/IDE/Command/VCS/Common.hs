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
    mergeToolSetter
    ,eMergeToolSetter
    ,runActionWithContext
    ,createActionFromContext
    ,runSetupRepoActionWithContext
    ,getVCSConf'
    ,VCSAction
    ,askIDERef
    ,readIDE'
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

newtype VCSSetupAction a = VCSSetupAction (ReaderT (Maybe VCSConf) IDEM a)
    deriving (Monad, MonadIO, MonadReader (Maybe VCSConf))

newtype VCSAction a = VCSAction (ReaderT (VCSConf,FilePath) IDEM a)
    deriving (Monad, MonadIO, MonadReader (VCSConf,FilePath))

askIDERef :: VCSAction IDERef
askIDERef = VCSAction $ lift $ ask

readIDE' :: (IDE -> a) -> VCSAction a
readIDE' f = VCSAction $ lift $ readIDE f

{- |
    Retrieves VCS configuration for given package from current workspace and runs given
    VCS action using it. VCS Configuration must be set before.
-}
runActionWithContext :: VCSAction ()    -- ^ computation to execute, i.e. showCommit
                     -> FilePath        -- ^ filepath to package
                     -> IDEAction
runActionWithContext vcsAction packageFp = do
    config <- getVCSConf'' packageFp
    runVcs config packageFp $ vcsAction
    where
    runVcs :: VCSConf -> FilePath -> VCSAction t -> IDEM t
    runVcs config cabalFp (VCSAction a) = runReaderT a (config,cabalFp)

{- |
    Shows a GUI to set up a VCS. If a vcs-config is set for given package it will be used.
-}
runSetupRepoActionWithContext :: FilePath
                              -> IDEAction
runSetupRepoActionWithContext packageFp = do
    eConfigErr <- getVCSConf packageFp
    case eConfigErr of
        Left error -> liftIO $ showErrorDialog error
        Right mbConfig -> do
            ide <- ask
            liftIO $ VCSGUI.showSetupConfigGUI mbConfig (callback ide packageFp)
    where
    callback :: IDERef -> FilePath -> Maybe (VCS.VCSType, VCS.Config, Maybe VCSGUI.MergeTool) -> IO()
    callback ideRef packageFp mbConfig  = do
            -- set config in workspace
            case mbConfig of
                Nothing -> return() --TODO maybe allow to delete conf
                Just config -> runReaderT (workspaceSetVCSConfig packageFp config) ideRef




{- |
    Runs given vcs-action using the vcs-conf set in the ReaderT.
    Provides a basic exception handler for any errors occuring.
-}
createActionFromContext :: VCS.Ctx()    -- ^ computation to execute, i.e. showCommit
                        -> VCSAction ()
createActionFromContext vcsAction = do
    ((_,conf,_),_) <- ask
    liftIO $ VCSGUI.defaultVCSExceptionHandler $ VCS.runVcs conf $ vcsAction


{- |
    Facility to set a mergetool for a given package.
-}
mergeToolSetter :: IDERef -> FilePath -> VCSGUI.MergeTool -> IO()
mergeToolSetter ideRef cabalFp mergeTool = do
    runReaderT (workspaceSetMergeTool cabalFp mergeTool) ideRef

{- |
    Creates an 'eMergeToolSetter' (Either MergeTool or MT-Setter) from given parameters.
-}
eMergeToolSetter :: IDERef
                -> FilePath
                -> Maybe VCSGUI.MergeTool
                -> Either VCSGUI.MergeTool (VCSGUI.MergeTool -> IO())
eMergeToolSetter ideRef cabalFp mbMergeTool = do
    case mbMergeTool of
                            Nothing -> Right $ mergeToolSetter ideRef cabalFp
                            Just mergeTool -> Left $ mergeTool

--
-- Basic setters and getters
--

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

-- | vcs conf for given package in current workspace.
getVCSConf :: FilePath -> IDEM (Either String (Maybe VCSConf))
getVCSConf pathToPackage = do
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Nothing -> return $ Left $ "No open workspace. Open Workspace first."
        Just workspace -> getVCSConf' workspace pathToPackage

-- | vcs conf for given package in given workspace.
getVCSConf' :: Workspace -> FilePath -> IDEM (Either String (Maybe VCSConf))
getVCSConf' workspace pathToPackage = do
            let mbConfig = Map.lookup pathToPackage $ packageVcsConf workspace
            case mbConfig of
            --Left $ "Could not find version-control-system configuration for package "++pathToPackage
                Nothing -> return $ Right $ Nothing
                Just conf -> return $ Right $ Just conf

-- | vcs conf for given package in current workspace. Workspae and VCS conf must be set before.
getVCSConf'' :: FilePath -> IDEM VCSConf
getVCSConf'' pathToPackage = do
    (Just workspace) <- readIDE workspace
    return $ fromJust $ Map.lookup pathToPackage $ packageVcsConf workspace

