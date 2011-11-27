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
    ,mkSVNActions
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

commitAction :: Common.VCSAction ()
commitAction = do
    ((_,_,mbMergeTool),package) <- ask
    ide <- Common.askIDERef
    createSVNActionFromContext $ GUISvn.showCommitGUI $ Common.eMergeToolSetter ide package mbMergeTool

updateAction :: Common.VCSAction ()
updateAction = do
    ((_,_,mbMergeTool),package) <- ask
    ideRef <- Common.askIDERef
    createSVNActionFromContext $ GUISvn.showUpdateGUI $ Common.eMergeToolSetter ideRef package mbMergeTool

viewLogAction :: Common.VCSAction ()
viewLogAction = createSVNActionFromContext GUISvn.showLogGUI

mkSVNActions :: [(String, Common.VCSAction ())]
mkSVNActions = [
                ("_Commit", commitAction)
                ,("_View Log", viewLogAction)
                ,("_Update", updateAction)
                ]

--HELPERS

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


