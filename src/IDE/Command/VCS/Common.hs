{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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

import Prelude ()
import Prelude.Compat
import IDE.Core.State
       (packageVcsConf, workspace, reflectIDE, modifyIDE_, ideGtk,
        readIDE, IDERef, IDEM, IDEAction, VCSConf, Workspace, __)
import IDE.Gtk.State (vcsData)
import qualified IDE.Utils.GUIUtils as GUIUtils
import qualified IDE.Workspaces.Writer as Writer
import qualified IDE.Command.VCS.Types as Types
import qualified IDE.Command.VCS.GIT as GIT
import qualified IDE.Command.VCS.SVN as SVN
import qualified IDE.Command.VCS.Mercurial as Mercurial


import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI

import Control.Monad.Reader
import Control.Monad.Trans(liftIO)
import Control.Lens ((.~), (^.), (?~), pre, _Just)
import Data.Maybe
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified GI.Gtk.Objects.Menu as Gtk (menuNew, Menu(..))
import qualified GI.Gtk.Objects.MenuItem as Gtk
       (menuItemGetSubmenu, menuItemSetSubmenu, menuItemNewWithMnemonic,
        menuItemNewWithLabel)
import GI.Gtk.Objects.MenuItem (onMenuItemActivate)
import qualified GI.Gtk.Objects.MenuShell as Gtk (menuShellAppend)
import qualified GI.Gtk.Objects.Widget as Gtk (widgetShowAll)
import Data.GI.Base (unsafeCastTo)
import GI.Gtk.Objects.Menu (Menu(..))
import Graphics.UI.Frame.ViewFrame (getMainWindow)



setMenuForPackage :: Gtk.Menu -> FilePath -> Maybe VCSConf -> IDEAction
setMenuForPackage vcsMenu cabalFp mbVCSConf = do
    ideR <- ask

    -- create or get packageItem and set it to ide to be able to get it later again
    readIDE (pre $ ideGtk . _Just . vcsData) >>= mapM_ (\(oldMenuItems,pw) -> do
        packageItem <-
            case Map.lookup cabalFp oldMenuItems of
                 Nothing -> Gtk.menuItemNewWithLabel $ T.pack cabalFp
                 Just menuItem -> return menuItem
        let newMenuItems = Map.insert cabalFp packageItem oldMenuItems
        modifyIDE_ $ ideGtk . _Just . vcsData .~ (newMenuItems,pw)

        packageMenu <- liftIO Gtk.menuNew

        -- build and set set-up repo action
        setupActionItem <- Gtk.menuItemNewWithMnemonic (__"_Setup Repo")
        _ <- onMenuItemActivate setupActionItem $
                reflectIDE (
                        runSetupRepoActionWithContext cabalFp
                    ) ideR
        Gtk.menuShellAppend packageMenu setupActionItem

        -- build and set other actions
        let packageMenuOperations = case mbVCSConf of
                                        Nothing -> []
                                        Just (vcsType,_,_) -> mkVCSActions vcsType
        addActions packageMenu ideR packageMenuOperations

        -- set menus
        Gtk.menuItemSetSubmenu packageItem (Just packageMenu)
        Gtk.menuShellAppend vcsMenu packageItem
        Gtk.widgetShowAll vcsMenu
        return ())
    where
    addActions packageMenu ideR
       = mapM_
           (\ (name', action') ->
              do actionItem <- Gtk.menuItemNewWithMnemonic name'
                 _ <- onMenuItemActivate actionItem $
                   reflectIDE (runActionWithContext action' cabalFp) ideR
                 Gtk.menuShellAppend packageMenu actionItem)
    mkVCSActions :: VCS.VCSType -> [(Text, Types.VCSAction ())]
    mkVCSActions VCS.SVN = SVN.mkSVNActions
    mkVCSActions VCS.GIT = GIT.mkGITActions
    mkVCSActions VCS.Mercurial = Mercurial.mkMercurialActions




{- |
    Retrieves VCS configuration for given package from current workspace and runs given
    VCS action using it. VCS Configuration must be set before.
-}
runActionWithContext :: Types.VCSAction ()    -- ^ computation to execute, i.e. showCommit
                     -> FilePath        -- ^ filepath to package
                     -> IDEAction
runActionWithContext vcsAction packageFp = do
    config <- getVCSConf'' packageFp
    runVcs config packageFp vcsAction
    where
    runVcs :: VCSConf -> FilePath -> Types.VCSAction t -> IDEM t
    runVcs config cabalFp (Types.VCSAction a) = runReaderT a (config,cabalFp)

{- |
    Shows a GUI to set up a VCS. If a vcs-config is set for given package it will be used.
-}
runSetupRepoActionWithContext :: FilePath
                              -> IDEAction
runSetupRepoActionWithContext packageFp = do
    mainWindow <- getMainWindow
    eConfigErr <- getVCSConf packageFp
    case eConfigErr of
        Left error' -> liftIO $ GUIUtils.showErrorDialog (Just mainWindow) error'
        Right mbConfig -> do
            ide <- ask
            liftIO $ VCSGUI.showSetupConfigGUI mbConfig (callback ide)
    where
    callback :: IDERef -> Maybe (VCS.VCSType, VCS.Config, Maybe VCSGUI.MergeTool) -> IO()
    callback ideRef mbConfig  =
            -- set config in workspace
            runReaderT (workspaceSetVCSConfig packageFp mbConfig) ideRef






--
-- Basic setters and getters
--

workspaceSetVCSConfig :: FilePath -> Maybe VCSConf -> IDEAction
workspaceSetVCSConfig pathToPackage mbVCSConf = do
    vcsItem <- GUIUtils.getVCS
    vcsMenu <- Gtk.menuItemGetSubmenu vcsItem >>= liftIO . unsafeCastTo Menu . fromJust
    setMenuForPackage vcsMenu pathToPackage mbVCSConf
    modifyIDE_ (\ide -> do
        let oldWs = fromJust (ide ^. workspace)
        let oldMap = oldWs ^. packageVcsConf
        let newMap =  case mbVCSConf of
                Nothing -> Map.delete pathToPackage oldMap
                Just vcsConf -> Map.insert pathToPackage vcsConf oldMap
        let newWs = fromJust (ide ^. workspace) & packageVcsConf .~ newMap
        ide & workspace ?~ newWs)
    newWs <- readIDE workspace
    Writer.writeWorkspace $ fromJust newWs



-- | vcs conf for given package in current workspace.
getVCSConf :: FilePath -> IDEM (Either Text (Maybe VCSConf))
getVCSConf pathToPackage =
    readIDE workspace >>= \case
        Nothing -> return $ Left "No open workspace. Open Workspace first."
        Just ws -> getVCSConf' ws pathToPackage

-- | vcs conf for given package in given workspace.
getVCSConf' :: Workspace -> FilePath -> IDEM (Either Text (Maybe VCSConf))
getVCSConf' ws pathToPackage = do
    let mbConfig = Map.lookup pathToPackage $ ws ^. packageVcsConf
    case mbConfig of
    --Left $ "Could not find version-control-system configuration for package "++pathToPackage
        Nothing -> return $ Right Nothing
        Just conf -> return $ Right $ Just conf

-- | vcs conf for given package in current workspace. Workspae and VCS conf must be set before.
getVCSConf'' :: FilePath -> IDEM VCSConf
getVCSConf'' pathToPackage = do
    (Just ws) <- readIDE workspace
    return $ fromJust $ Map.lookup pathToPackage $ ws ^. packageVcsConf

