-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.VCS.SVN
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

module IDE.Command.VCS.SVN (
    commitAction
    ,updateAction
    ,viewLogAction
) where

import Graphics.UI.Gtk.ActionMenuToolbar.UIManager(MergeId)

import qualified VCSGui.Common as VCSGUI
import qualified VCSGui.Svn as GUISvn
import qualified VCSWrapper.Svn as Wrapper.Svn
import qualified VCSWrapper.Common as Wrapper

import IDE.Command.VCS.Common

import IDE.Core.Types
import IDE.Core.State

import IDE.Workspaces as Workspaces

import Control.Monad.Reader(liftIO,ask,runReaderT)
import Data.IORef(atomicModifyIORef, IORef)
import Data.Either

commitAction :: IDEAction
commitAction = do
    ide <- ask
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Just workspace -> do
                             let (Just (_,_,mbMergeTool)) = vcsConfig workspace
                             eMergeToolSetter <- case mbMergeTool of
                                Nothing -> return $ Right $ mergeToolSetter ide
                                Just mergeTool -> return $ Left $ mergeTool
                             createSVNActionFromContext $ GUISvn.showCommitGUI eMergeToolSetter
        Nothing -> do
            noOpenWorkspace



updateAction :: IDEAction
updateAction = do
    ide <- ask
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Just workspace -> do
                             let (Just (_,_,mbMergeTool)) = vcsConfig workspace
                             eMergeToolSetter <- case mbMergeTool of
                                Nothing -> return $ Right $ mergeToolSetter ide
                                Just mergeTool -> return $ Left $ mergeTool
                             createSVNActionFromContext $ GUISvn.showUpdateGUI eMergeToolSetter
        Nothing -> do
            noOpenWorkspace




viewLogAction :: IDEAction
viewLogAction = createSVNActionFromContext GUISvn.showLogGUI

createSVNActionFromContext :: (Either GUISvn.Handler (Maybe String)
                                -> Wrapper.Ctx())
                           -> IDEAction
createSVNActionFromContext action = do
    (mergeInfo, mbPw) <- readIDE vcsData
    e <- ask
    case mbPw of
            Nothing -> createActionFromContext $ action $ Left $ passwordHandler e mergeInfo
            Just mb -> createActionFromContext $ action $ Right mb
    where
        passwordHandler :: IORef IDE-> Maybe MergeId -> ((Maybe (Bool, Maybe String)) -> Wrapper.Ctx ())
        passwordHandler e mbMergeInfo result = liftIO $ do
            case result of
                Just (True, pw) -> modifyIDE_' e (\ide -> ide {vcsData = (mbMergeInfo, Just pw) })
                _               -> return ()
        modifyIDE_' e f = do
                liftIO (atomicModifyIORef e f')
                where
                    f' a  = (f a,())


