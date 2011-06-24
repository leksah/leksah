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
) where

import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI

import IDE.Core.Types
import IDE.Core.State
import IDE.Workspaces(workspaceSetVCSConfig)
import IDE.Utils.FileUtils(getConfigFilePathForLoad)

import Control.Monad.Reader
import Control.Monad.Trans(liftIO)
import Paths_leksah(getDataDir)
import Data.IORef(writeIORef, readIORef, IORef(..))

import Graphics.UI.Frame.Panes
import Graphics.UI.Gtk.ActionMenuToolbar.UIManager



setupRepoAction :: IDEAction
setupRepoAction = do
    ide <- ask

    mbWorkspace <- readIDE workspace
    case mbWorkspace of

        Just workspace -> do
                             let config = case vcsConfig workspace of
                                                Nothing -> Nothing
                                                Just (_,conf) -> Just conf
                             liftIO $ VCSGUI.showSetupConfigGUI config (callback ide)

        Nothing -> noOpenWorkspace
    where
        callback :: IDERef -> Maybe (VCS.VCSType, VCS.Config) -> IO()
        callback ideRef mbConfig = do
                -- set config in workspace
                runReaderT (workspaceSetVCSConfig mbConfig) ideRef


                case mbConfig of
                    Nothing -> return ()
                    Just config -> runReaderT (addMenuItems config) ideRef
        addMenuItems :: (VCS.VCSType, VCS.Config) -> IDEAction
        addMenuItems (vcsType,config) = do
                -- TODO delete old menu items, see below

                fs <- readIDE frameState
                let manager = uiManager fs
                let file = case vcsType of
                                    VCS.GIT -> "git.menu"
                                    VCS.SVN -> "svn.menu"

                menuItems <- liftIO $ vcsMenuDescription file
                mergeInfo <- liftIO $ uiManagerAddUiFromString manager menuItems

                -- TODO mergeInfo should be saved to framestate to allow removing menuitems from version control menu again
                -- TODO framestate is part of the ltk => forking of ltk is necessary
                -- TODO other solution might be to save mergeInfo to IDE
                return ()
        vcsMenuDescription :: FilePath -> IO String
        vcsMenuDescription file = do
                dataDir     <- getDataDir
                prefsPath   <- getConfigFilePathForLoad file Nothing dataDir
                res         <- readFile prefsPath
                return res

createActionFromContext :: VCS.Ctx()    -- ^ computation to execute, i.e. showCommit
                        -> IDEAction
createActionFromContext vcsAction = do
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Just workspace -> do
             let mbConfig = vcsConfig workspace
             case mbConfig of
                Nothing -> liftIO $ VCSGUI.showErrorGUI "No active repository!"
                Just (_,config) -> liftIO $ VCS.runVcs config $ vcsAction
        Nothing -> noOpenWorkspace


--TODO stub, proper error handling
noOpenWorkspace = do
                    liftIO $ putStrLn "No open workspace"
                    return () --TODO show error message (use ..Common for this)
