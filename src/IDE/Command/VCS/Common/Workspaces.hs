{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.VCS.Common.Workspaces
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

module IDE.Command.VCS.Common.Workspaces (
    onWorkspaceOpen
    , onWorkspaceClose
) where

-- VCS imports
import IDE.Utils.FileUtils(getConfigFilePathForLoad)
import qualified IDE.Utils.GUIUtils as GUIUtils
import IDE.Core.Types
import IDE.Core.State
import qualified IDE.Command.VCS.Common as Common
import qualified IDE.Command.VCS.SVN as SVN (mkSVNActions)
import qualified IDE.Command.VCS.GIT as GIT (mkGITActions)
import qualified IDE.Command.VCS.Common.GUI as GUI

import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI

import Data.IORef(writeIORef, readIORef, IORef(..))
import Control.Monad.Reader(liftIO,ask,when)

import Graphics.UI.Frame.Panes

import Data.Maybe
import Data.List
import System.Log.Logger (debugM)
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack, pack)
import GI.Gtk.Objects.MenuItem (menuItemSetSubmenu)
import GI.Gtk.Objects.Menu (noMenu, menuNew)
import GI.Gtk.Objects.Widget (widgetShowAll)

onWorkspaceClose :: IDEAction
onWorkspaceClose = do
    vcsItem <- GUIUtils.getVCS
    menuItemSetSubmenu vcsItem noMenu

onWorkspaceOpen :: Workspace -> IDEAction
onWorkspaceOpen ws = do
    liftIO $ debugM "leksah" "onWorkspaceOpen"
    let mbPackages = wsAllPackages ws
    packages <- mapM (mapper ws)
                             mbPackages
    vcsItem <- GUIUtils.getVCS
    vcsMenu <- liftIO menuNew

    ideR <- ask

    --for each package add an extra menu containing vcs specific menuitems
    mapM_ (\(p,mbVcsConf) -> do
                Common.setMenuForPackage vcsMenu (ipdCabalFile p) mbVcsConf
                menuItemSetSubmenu vcsItem (Just vcsMenu)
                )
           packages

    widgetShowAll vcsItem
    return ()
    where
    mapper :: Workspace -> IDEPackage -> IDEM (IDEPackage, Maybe VCSConf)
    mapper workspace p = do
        let fp = ipdCabalFile p
        eErrConf <- Common.getVCSConf' workspace fp
        case eErrConf of
            Left error -> do
                liftIO . putStrLn . T.unpack $ "Could not retrieve vcs-conf due to '"<>error<>"'."
                return (p, Nothing)
            Right mbConf -> case mbConf of
                                Nothing -> do
                                    liftIO $ putStrLn
                                                "Could not retrieve vcs-conf for active package. No vcs-conf set up."
                                    return (p, Nothing)
                                Just vcsConf -> return (p,  Just vcsConf)


