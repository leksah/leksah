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
    setMenuForPackage

    --getter
    ,getVCSConf'
) where

import IDE.Core.Types
import IDE.Core.State
import qualified IDE.Utils.GUIUtils as GUIUtils
import qualified IDE.Workspaces.Writer as Writer
import qualified IDE.Command.VCS.Types as Types
import qualified IDE.Command.VCS.GIT as GIT
import qualified IDE.Command.VCS.SVN as SVN


import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI

import qualified Graphics.UI.Gtk as Gtk
import Control.Monad.Reader
import Control.Monad.Trans(liftIO)
import qualified Control.Exception as Exc
import Data.Maybe
import qualified Data.Map as Map



setMenuForPackage :: Gtk.Menu -> FilePath -> Maybe VCSConf -> IDEAction
setMenuForPackage vcsMenu cabalFp mbVCSConf = do
                    ideR <- ask

                    -- create or get packageItem and set it to ide to be able to get it later again
                    (oldMenuItems,pw) <- readIDE vcsData
                    packageItem <- do
                        case (Map.lookup cabalFp oldMenuItems) of
                            Nothing -> liftIO $ Gtk.menuItemNewWithLabel cabalFp
                            Just menuItem -> return menuItem
                    let newMenuItems = Map.insert cabalFp packageItem oldMenuItems
                    modifyIDE_ (\ide -> ide {vcsData = (newMenuItems,pw)})

                    packageMenu <- liftIO $ Gtk.menuNew

                    -- build and set set-up repo action
                    setupActionItem <- liftIO $ Gtk.menuItemNewWithMnemonic "_Setup Repo"
                    liftIO $ setupActionItem `Gtk.onActivateLeaf` (
                            reflectIDE (
                                    runSetupRepoActionWithContext cabalFp
                                ) ideR)
                    liftIO $ Gtk.menuShellAppend packageMenu setupActionItem

                    -- build and set other actions
                    let packageMenuOperations = case mbVCSConf of
                                                    Nothing -> []
                                                    Just (vcsType,_,_) -> mkVCSActions vcsType
                    liftIO $ addActions cabalFp packageMenu ideR packageMenuOperations

                    -- set menus
                    liftIO $ Gtk.menuItemSetSubmenu packageItem packageMenu
                    liftIO $ Gtk.menuShellAppend vcsMenu packageItem
--                    liftIO $ Gtk.widgetShowAll vcsMenu --TODO if view is not reset on switching vcs for package uncomment this
                    return ()
                    where
                    addActions cabalFp packageMenu ideR actions =  mapM_ (\(name,action) -> do
                        -- for each operation add it to menu and connect action
                        actionItem <- Gtk.menuItemNewWithMnemonic name
                        actionItem `Gtk.onActivateLeaf` (reflectIDE (runActionWithContext action cabalFp) ideR)
                        Gtk.menuShellAppend packageMenu actionItem
                        ) actions
                    mkVCSActions :: VCS.VCSType -> [(String, Types.VCSAction ())]
                    mkVCSActions VCS.SVN = SVN.mkSVNActions
                    mkVCSActions VCS.GIT = GIT.mkGITActions




{- |
    Retrieves VCS configuration for given package from current workspace and runs given
    VCS action using it. VCS Configuration must be set before.
-}
runActionWithContext :: Types.VCSAction ()    -- ^ computation to execute, i.e. showCommit
                     -> FilePath        -- ^ filepath to package
                     -> IDEAction
runActionWithContext vcsAction packageFp = do
    config <- getVCSConf'' packageFp
    runVcs config packageFp $ vcsAction
    where
    runVcs :: VCSConf -> FilePath -> Types.VCSAction t -> IDEM t
    runVcs config cabalFp (Types.VCSAction a) = runReaderT a (config,cabalFp)

{- |
    Shows a GUI to set up a VCS. If a vcs-config is set for given package it will be used.
-}
runSetupRepoActionWithContext :: FilePath
                              -> IDEAction
runSetupRepoActionWithContext packageFp = do
    eConfigErr <- getVCSConf packageFp
    case eConfigErr of
        Left error -> liftIO $ GUIUtils.showErrorDialog error
        Right mbConfig -> do
            ide <- ask
            liftIO $ VCSGUI.showSetupConfigGUI mbConfig (callback ide packageFp)
    where
    callback :: IDERef -> FilePath -> Maybe (VCS.VCSType, VCS.Config, Maybe VCSGUI.MergeTool) -> IO()
    callback ideRef packageFp mbConfig  = do
            -- set config in workspace
            runReaderT (workspaceSetVCSConfig packageFp mbConfig) ideRef






--
-- Basic setters and getters
--

workspaceSetVCSConfig :: FilePath -> Maybe VCSConf -> IDEAction
workspaceSetVCSConfig pathToPackage mbVCSConf = do
    vcsItem <- GUIUtils.getVCS
    mbVcsMenu <- liftIO $ Gtk.menuItemGetSubmenu vcsItem
    let vcsMenu = Gtk.castToMenu $ fromJust mbVcsMenu
    setMenuForPackage vcsMenu pathToPackage mbVCSConf
    modifyIDE_ (\ide -> do
        let oldWs = fromJust (workspace ide)
        let oldMap = packageVcsConf oldWs
        let newMap =  case mbVCSConf of
                Nothing -> Map.delete pathToPackage oldMap
                Just vcsConf -> Map.insert pathToPackage vcsConf oldMap
        let newWs = (fromJust (workspace ide)) { packageVcsConf = newMap }
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

