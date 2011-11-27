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

import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI

import Data.IORef(writeIORef, readIORef, IORef(..))
import Paths_leksah(getDataDir)
import Control.Monad.Reader(liftIO,ask)

import Graphics.UI.Frame.Panes
--import Graphics.UI.Gtk.ActionMenuToolbar.UIManager
import Graphics.UI.Gtk (
    menuNew, menuItemNewWithLabel, onActivateLeaf, menuShellAppend, menuItemSetSubmenu
        ,widgetShowAll, menuItemNewWithMnemonic)

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

onWorkspaceOpen :: Workspace -> IDEAction
onWorkspaceOpen workspace = do
        let mbPackages = wsPackages workspace
        packages <- mapM (mapper workspace)
                                 mbPackages
        vcsItem <- GUIUtils.getVCS
        vcsMenu <- liftIO $ menuNew

        ideR <- ask

        --for each package add an extra menu containing vcs specific menuitems
        mapM_ (\(p,mbVcsConf) -> do
                    let cabalFp = ipdCabalFile p
                    packageItem <- liftIO $ menuItemNewWithLabel cabalFp
                    packageMenu <- liftIO $ menuNew

                    -- set-up repo action
                    let addActions' = addActions cabalFp packageMenu ideR
                    liftIO $ addActions' [getSetupRepoAction] Common.runActionWithContext --change runner
                    -- other actions if repo set
                    case mbVcsConf of
                        Nothing -> return()
                        Just (vcsType, _,_) -> do
                            let packageMenuOperations = getVCSActions vcsType
                            liftIO $ addActions' packageMenuOperations Common.runActionWithContext

                    liftIO $ menuItemSetSubmenu packageItem packageMenu
                    liftIO $ menuShellAppend vcsMenu packageItem
                    )
               packages

--
--        --
        liftIO $ menuItemSetSubmenu vcsItem vcsMenu
        liftIO $ widgetShowAll vcsMenu
        return ()
        where
        addActions cabalFp packageMenu ideR actions runner =  mapM_ (\(name,action) -> do
                    -- for each operation add it to menu and connect action
                    actionItem <- menuItemNewWithMnemonic name
                    actionItem `onActivateLeaf` (reflectIDE (runner action cabalFp) ideR)
                    menuShellAppend packageMenu actionItem
                ) actions
        mapper :: Workspace -> IDEPackage -> IDEM (IDEPackage, Maybe VCSConf)
        mapper workspace p = do
            let fp = ipdCabalFile p
            eErrConf <- Common.getVCSConf' workspace fp
            case eErrConf of
                Left error -> do
                    liftIO $ putStrLn $ "Could not retrieve vcs-conf due to '"++error++"'."
                    return (p, Nothing)
                Right mbConf -> case mbConf of
                                    Nothing -> do
                                        liftIO $ putStrLn $ "Could not retrieve vcs-conf for active package. No vcs-conf set up."
                                        return (p, Nothing)
                                    Just vcsConf -> return $ (p,  Just vcsConf)

--TODO move and retrieve this to/from data file e.g. svn.menu, git.menu
getVCSActions :: VCS.VCSType -> [(String, Common.VCSAction ())]
getVCSActions VCS.SVN = [
                            ("_Commit", commitAction')
                            ,("_View Log", viewLogAction')
                            ,("_Update", updateAction')
                            ]
getVCSActions VCS.GIT = [
                            ("_Commit", commitAction')
                            ,("_View Log", viewLogAction')
                            ,("_Update", updateAction')
                            ]

getSetupRepoAction :: (String,Common.VCSAction ())
getSetupRepoAction = ("_Setup Repo", Common.setupRepoAction')
--
----setupRepoAction' :: VCSAction
----setupRepoAction' = return()
--
commitAction' :: Common.VCSAction ()
commitAction' = return()

viewLogAction' :: Common.VCSAction ()
viewLogAction' = return()

updateAction' :: Common.VCSAction ()
updateAction' = return()

