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
    addMenuItems
) where

-- VCS imports
import IDE.Utils.FileUtils(getConfigFilePathForLoad)
import IDE.Core.Types
import IDE.Core.State

import qualified VCSWrapper.Common as VCS

import Data.IORef(writeIORef, readIORef, IORef(..))
import Paths_leksah(getDataDir)
import Control.Monad.Reader(liftIO)

import Graphics.UI.Frame.Panes
import Graphics.UI.Gtk.ActionMenuToolbar.UIManager


addMenuItems :: (VCS.VCSType, VCS.Config) -> IDEAction
addMenuItems (vcsType,config) = do
        fs <- readIDE frameState
        let manager = uiManager fs

        (mbMergeInfo, pw) <- readIDE vcsData
        case mbMergeInfo of
            Nothing   -> return()
            Just info -> liftIO $ uiManagerRemoveUi manager info



        let file = case vcsType of
                            VCS.GIT -> "git.menu"
                            VCS.SVN -> "svn.menu"

        menuItems <- liftIO $ vcsMenuDescription file
        mergeInfo <- liftIO $ uiManagerAddUiFromString manager menuItems

        modifyIDE_ (\ide -> ide {vcsData = (Just mergeInfo, pw) })
        return ()
        where
        vcsMenuDescription :: FilePath -> IO String
        vcsMenuDescription file = do
                dataDir     <- getDataDir
                prefsPath   <- getConfigFilePathForLoad file Nothing dataDir
                res         <- readFile prefsPath
                return res


