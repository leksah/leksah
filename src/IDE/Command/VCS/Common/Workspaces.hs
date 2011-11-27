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
import IDE.Core.Types
import IDE.Core.State

import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI

import Data.IORef(writeIORef, readIORef, IORef(..))
import Paths_leksah(getDataDir)
import Control.Monad.Reader(liftIO,ask)

import Graphics.UI.Frame.Panes
--import Graphics.UI.Gtk.ActionMenuToolbar.UIManager
import Graphics.UI.Gtk (menuNew, menuItemNewWithLabel, onActivateLeaf, menuShellAppend, menuItemSetSubmenu)

import Data.Maybe
import Data.List

onWorkspaceClose :: IDEAction
onWorkspaceClose = return()
--onWorkspaceClose = do
--        fs <- readIDE frameState
--        let manager = uiManager fs
--
--        (mbMergeInfo, _) <- readIDE vcsData
--        -- remove menuitems
--        case mbMergeInfo of
--            Nothing   -> return()
--            Just info -> liftIO $ uiManagerRemoveUi manager info
--
--        -- reset vcsData
--        modifyIDE_ (\ide -> ide {vcsData = (Nothing,Nothing) })
--        return ()


onWorkspaceOpen :: [(IDEPackage, Maybe VCSConf)] -> IDEAction
onWorkspaceOpen packages = do
        let vcsTypes = nub $ mapMaybe mapper packages

        menuItems <- mapM (\vcsType -> do
                                let file = case vcsType of
                                                VCS.GIT -> "git.menu"
                                                VCS.SVN -> "svn.menu"
                                menuItems <- liftIO $ vcsMenuDescription file
                                return (vcsType, menuItems)
                                )
                           vcsTypes
        --building the gui

        --get the vcsItem (menu entry)
--        vcsItem <- getVCSItem -- in GUIUtils sthing like getVCSItem = getMenuItem "ui/menubar/Version Con_trol"
        vcsMenu <- liftIO $ menuNew
--
        ideR <- ask
        mapM_ (\package ->
            case package of
                (_,Nothing) -> return() -- add some error entry ?
                (p,Just (vcsType, conf,mt)) -> do
                    --for each package add an extra menu containing vcs specific menuitems
                    packageMenu <- liftIO $ menuNew
                    let packageMenuOperations = getVCSActions vcsType --somehow get that

                    let cabalFp = ipdCabalFile p
                    mapM_ (\(name,action) -> do
                        -- for each operation add it to menu and connect action
                        liftIO $ do
                            mi <- menuItemNewWithLabel $ name
                            mi `onActivateLeaf` (reflectIDE (action cabalFp) ideR)
                            menuShellAppend packageMenu mi
                        ) packageMenuOperations
                    liftIO $ menuItemSetSubmenu vcsMenu packageMenu
                    )
               packages

--
--        --
--        menuItemSetSubmenu vcsItem vcsMenu
--        widgetShowAll menuNew

----        TODO remove following 4 lines
----       mergeInfo <- liftIO $ uiManagerAddUiFromString manager menuItems
----        -- set vcsData with new mergeInfo to be able to remove it later
----        (_, pw) <- readIDE vcsData
----        modifyIDE_ (\ide -> ide {vcsData = (Just mergeInfo, pw) })
        return ()
        where
        vcsMenuDescription :: FilePath -> IO String
        vcsMenuDescription file = do
                dataDir     <- getDataDir
                prefsPath   <- getConfigFilePathForLoad file Nothing dataDir
                res         <- readFile prefsPath
                return res
        mapper :: (IDEPackage, Maybe VCSConf) -> (Maybe VCS.VCSType)
        mapper (p, mbConf) = case mbConf of
                                    Nothing -> Nothing
                                    Just (vcsType,_,_) -> Just vcsType

--type OpName = String
--type OpAction = FilePath -> IDEAction

getVCSActions :: VCS.VCSType -> [(String, (FilePath -> IDEAction))]
getVCSActions VCS.GIT = [
                            ("_Setup Repo", setupRepoAction')
                            ,("_Commit", commitAction')
                            ,("_View Log", viewLogAction')
                            ,("_Update", updateAction')
                            ]
setupRepoAction' :: FilePath -> IDEAction
setupRepoAction' _ = return()

commitAction' :: FilePath -> IDEAction
commitAction' _ = return()

viewLogAction' :: FilePath -> IDEAction
viewLogAction' _ = return()

updateAction' :: FilePath -> IDEAction
updateAction' _ = return()
