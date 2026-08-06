{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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

import Prelude ()
import Prelude.Compat

import IDE.Core.State (ideGtk, modifyIDE_, reflectIDE)
import IDE.Gtk.State (vcsData)

import qualified IDE.Command.VCS.Common.Helper as Helper
import qualified IDE.Command.VCS.Types as Types

import qualified VCSGui.Svn as GUISvn
import qualified VCSWrapper.Common as Wrapper

import Control.Monad.Reader(liftIO,ask)
import Control.Lens (pre, (.~), _Just)
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
    ide <-  Types.askIDERef
    Types.readIDE' (pre $ ideGtk . _Just . vcsData) >>= mapM_ (\case
        (mergeInfo, Nothing) -> Helper.createActionFromContext $ action $ Left $ passwordHandler ide mergeInfo
        (_, Just mb) -> Helper.createActionFromContext $ action $ Right mb)
    where
--        passwordHandler :: IORef IDE-> Maybe MergeId -> ((Maybe (Bool, Maybe Text)) -> Wrapper.Ctx ())
        passwordHandler ide mbMergeInfo result = liftIO $
            case result of
                Just (True, pw) -> reflectIDE (modifyIDE_ $ ideGtk . _Just . vcsData .~ (mbMergeInfo, Just pw)) ide
                _               -> return ()


