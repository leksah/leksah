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

import Prelude ()
import Prelude.Compat

import GHC.Stack (HasCallStack)

-- VCS imports
import qualified IDE.Utils.GUIUtils as GUIUtils
import IDE.Core.State
       (IDEAction, MonadIDE, Workspace, IDEPackage, IDEM,
       VCSConf, readIDE, ideGtk, wsAllPackages, ipdCabalFile)
import qualified IDE.Command.VCS.Common as Common

import Control.Monad.Reader(liftIO, when)

import Control.Lens (to, (^.))
import Data.Maybe
import System.Log.Logger (debugM)
import qualified Data.Text as T (unpack)
import GI.Gtk.Objects.MenuItem (menuItemSetSubmenu)
import GI.Gtk.Objects.Menu (noMenu, menuNew)
import GI.Gtk.Objects.Widget (widgetShowAll)

onWorkspaceClose :: IDEAction
onWorkspaceClose = do
    vcsItem <- GUIUtils.getVCS
    menuItemSetSubmenu vcsItem noMenu

whenIDEGtk :: MonadIDE m => m () -> m ()
whenIDEGtk f = readIDE (ideGtk . to isJust) >>= (`when` f)

onWorkspaceOpen :: HasCallStack => Workspace -> IDEAction
onWorkspaceOpen ws = do
    liftIO $ debugM "leksah" "onWorkspaceOpen"
    whenIDEGtk $ do
        let mbPackages = ws ^. wsAllPackages
        packages <- mapM (mapper ws)
                                 mbPackages
        vcsItem <- GUIUtils.getVCS
        vcsMenu <- liftIO menuNew

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
            Left err -> do
                liftIO . putStrLn . T.unpack $ "Could not retrieve vcs-conf due to '"<>err<>"'."
                return (p, Nothing)
            Right mbConf -> case mbConf of
                                Nothing -> do
                                    liftIO $ putStrLn
                                                "Could not retrieve vcs-conf for active package. No vcs-conf set up."
                                    return (p, Nothing)
                                Just vcsConf -> return (p,  Just vcsConf)


