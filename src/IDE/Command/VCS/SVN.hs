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
-- | TODO use new runActionWithContext
--
-----------------------------------------------------------------------------

module IDE.Command.VCS.SVN (
    commitAction
    ,updateAction
    ,viewLogAction
    ,commitAction'
    ,updateAction'
    ,viewLogAction'
) where

import Graphics.UI.Gtk.ActionMenuToolbar.UIManager(MergeId)

import qualified VCSGui.Common as VCSGUI
import qualified VCSGui.Svn as GUISvn
import qualified VCSWrapper.Svn as Wrapper.Svn
import qualified VCSWrapper.Common as Wrapper

import IDE.Core.Types
import IDE.Core.State

import qualified IDE.Command.VCS.Common as Common

import Control.Monad.Reader(liftIO,ask,lift)
import Data.IORef(atomicModifyIORef, IORef)
import Data.Either

commitAction' :: Common.VCSAction ()
commitAction' = do
    (_,_,mbMergeTool) <- ask
    ide <- Common.askIDERef
    createSVNActionFromContext $ GUISvn.showCommitGUI $ Common.eMergeToolSetter ide mbMergeTool


commitAction :: IDEAction
commitAction = do
    return()

updateAction' :: Common.VCSAction ()
updateAction' = do
    (_,_,mbMergeTool) <- ask
    ideRef <- Common.askIDERef
    createSVNActionFromContext $ GUISvn.showUpdateGUI $ Common.eMergeToolSetter ideRef mbMergeTool

updateAction :: IDEAction
updateAction = do
    return()
--    eConfigErr <- getVCSConfForActivePackage
--    execWithErrHandling
--        eConfigErr
--        updateAction'
--    ide <- ask
--    mbWorkspace <- readIDE workspace
--    case mbWorkspace of
--        Just workspace -> do
--                             let (Just (_,_,mbMergeTool)) = vcsConfig workspace
--                             eMergeToolSetter <- case mbMergeTool of
--                                Nothing -> return $ Right $ Workspaces.workspaceSetMergeToolForActivePackage
--                                Just mergeTool -> return $ Left $ mergeTool
--                             createSVNActionFromContext $ GUISvn.showUpdateGUI eMergeToolSetter
--        Nothing -> do
--            noOpenWorkspace
--updateAction' (Just (_,_,mbMergeTool)) = do
--    ide <- ask
--    eMergeToolSetter <- case mbMergeTool of
--                                Nothing -> return $ Right $ mergeToolSetter ide
--                                Just mergeTool -> return $ Left $ mergeTool
--    createSVNActionFromContext $ GUISvn.showUpdateGUI eMergeToolSetter


viewLogAction' :: Common.VCSAction ()
viewLogAction' = createSVNActionFromContext GUISvn.showLogGUI

--
viewLogAction :: IDEAction
viewLogAction = return()
--viewLogAction = createSVNActionFromContext GUISvn.showLogGUI


--HELPERS

--TODO use this in commit and update
--createSVNActionFromContextWEMergeToolSetter :: (Either VCSGUI.MergeTool VCSGUI.MergeToolSetter
--                                            -> Either GUISvn.Handler (Maybe String)
--                                            -> Wrapper.Ctx()) -- ^ svn action
--                -> Maybe VCSGUI.MergeTool
--                -> IDEAction
--createSVNActionFromContextWEMergeToolSetter action mbMergeTool = do
--     ide <- ask
--     eMergeToolSetter <- case mbMergeTool of
--                            Nothing -> return $ Right $ Common.mergeToolSetter ide
--                            Just mergeTool -> return $ Left $ mergeTool
--     createSVNActionFromContext $ action eMergeToolSetter

createSVNActionFromContext :: (Either GUISvn.Handler (Maybe String)
                                -> Wrapper.Ctx())
                           -> Common.VCSAction ()
createSVNActionFromContext action = do
    (mergeInfo, mbPw) <- Common.readIDE' vcsData
    ide <-  Common.askIDERef
    case mbPw of
            Nothing -> Common.createActionFromContext $ action $ Left $ passwordHandler ide mergeInfo
            Just mb -> Common.createActionFromContext $ action $ Right mb
    where
        passwordHandler :: IORef IDE-> Maybe MergeId -> ((Maybe (Bool, Maybe String)) -> Wrapper.Ctx ())
        passwordHandler ide mbMergeInfo result = liftIO $ do
            case result of
                Just (True, pw) -> modifyIDE_' ide (\ide -> ide {vcsData = (mbMergeInfo, Just pw) })
                _               -> return ()
        modifyIDE_' ide f = do
                liftIO (atomicModifyIORef ide f')
                where
                    f' a  = (f a,())


