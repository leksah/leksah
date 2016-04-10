{-# LANGUAGE OverloadedStrings #-}
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


import IDE.Core.Types
import IDE.Core.State

import qualified IDE.Command.VCS.Common.Helper as Helper
import qualified IDE.Command.VCS.Types as Types

import qualified VCSGui.Common as VCSGUI
import qualified VCSGui.Svn as GUISvn
import qualified VCSWrapper.Svn as Wrapper.Svn
import qualified VCSWrapper.Common as Wrapper

import Control.Monad.Reader(liftIO,ask,lift)
import Data.IORef(atomicModifyIORef, IORef)
import Data.Either
import Data.Text (Text)

commitAction :: Types.VCSAction ()
commitAction = do
    ((_,_,mbMergeTool),package) <- ask
    ide <- Types.askIDERef
    createSVNActionFromContext $ GUISvn.showCommitGUI $ Helper.eMergeToolSetter ide package mbMergeTool

updateAction :: Types.VCSAction ()
updateAction = do
    ((_,_,mbMergeTool),package) <- ask
    ideRef <- Types.askIDERef
    createSVNActionFromContext $ GUISvn.showUpdateGUI $ Helper.eMergeToolSetter ideRef package mbMergeTool

viewLogAction :: Types.VCSAction ()
viewLogAction = createSVNActionFromContext GUISvn.showLogGUI

mkSVNActions :: [(Text, Types.VCSAction ())]
mkSVNActions = [
                ("_Commit", commitAction)
                ,("_View Log", viewLogAction)
                ,("_Update", updateAction)
                ]

--HELPERS

createSVNActionFromContext :: (Either GUISvn.Handler (Maybe Text)
                                -> Wrapper.Ctx())
                           -> Types.VCSAction ()
createSVNActionFromContext action = do
    (mergeInfo, mbPw) <- Types.readIDE' vcsData
    ide <-  Types.askIDERef
    case mbPw of
            Nothing -> Helper.createActionFromContext $ action $ Left $ passwordHandler ide mergeInfo
            Just mb -> Helper.createActionFromContext $ action $ Right mb
    where
--        passwordHandler :: IORef IDE-> Maybe MergeId -> ((Maybe (Bool, Maybe Text)) -> Wrapper.Ctx ())
        passwordHandler ide mbMergeInfo result = liftIO $
            case result of
                Just (True, pw) -> modifyIDE_' ide (\ide -> ide {vcsData = (mbMergeInfo, Just pw) })
                _               -> return ()
        modifyIDE_' ide f =
                liftIO (atomicModifyIORef ide f')
                where
                    f' a  = (f a,())


