-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.Svn
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
module IDE.Command.Svn (
    commitAction
    ,viewLogAction
    ,setupRepoAction
    ,updateAction
    ,checkoutAction
) where
import qualified VCSGui.Svn as SvnGUI
--import qualified VCSWrapper.Common as SvnC
import qualified VCSWrapper.Svn as SvnC
import IDE.Core.Types
import IDE.Core.State

import Control.Monad.Reader
import Control.Monad.Trans(liftIO)

import IDE.Workspaces(workspaceSetVCSConfig)

commitAction :: IDEAction
commitAction = do
    svnAction SvnGUI.showCommitGUI

viewLogAction :: IDEAction
viewLogAction = do
    svnAction SvnGUI.showLogGUI


setupRepoAction :: IDEAction
setupRepoAction = do
        ide <- ask
        Just workspace <- readIDE workspace
        liftIO $ SvnGUI.showSetupConfigGUI (vcsConfig workspace) (setRepo ide)
    where
    setRepo :: IDERef -> Maybe SvnC.Config -> IO ()
    setRepo ide mbConfig = runReaderT (workspaceSetVCSConfig mbConfig) ide


checkoutAction :: IDEAction
checkoutAction = do
    svnAction SvnGUI.showCheckoutGUI


updateAction :: IDEAction
updateAction = do
    svnAction SvnC.update

svnAction :: SvnC.Ctx()      -- ^ fn to execute, i.e. showCommit
            -> IDEAction
svnAction vcsAction = do
    Just workspace <- readIDE workspace
    let config = vcsConfig workspace
    case config of
        Just conf -> liftIO $ SvnC.runVcs conf $ vcsAction
        Nothing -> liftIO $ SvnGUI.showErrorGUI "No active Repository."


--setupRepoAction :: IDEAction
--setupRepoAction = do
--        return ()
----        ide <- ask
--        Just workspace <- readIDE workspace
--        liftIO $ GitGui.openRepoWindow (gitRepo workspace) ((flip setRepo) ide)
--    where
--    setRepo :: Maybe Git.GitRepo -> IDERef -> IO ()
--    setRepo mbRepo = runReaderT $ workspaceSetGitRepo mbRepo
--
--
--setupRepoAction :: IDEAction
--setupRepoAction = do
--    mbRepoPath <- liftIO $ GitGui.openRepoWindow "Choose repository location"
--    case mbRepoPath of
--        Just repoPath -> do
--            repo <- liftIO $ Git.openRepo repoPath
--            workspaceSetGitRepo $ Just repo
----            triggerEventIDE $ WorkspaceChanged True True
----            return ()
--        Nothing -> return ()
