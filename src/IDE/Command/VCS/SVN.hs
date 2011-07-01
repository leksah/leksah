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

import qualified VCSGui.Svn as GUISvn
import qualified VCSWrapper.Svn as WSvn
import qualified VCSWrapper.Common as WC

import IDE.Command.VCS.Common

import IDE.Core.Types
import IDE.Core.State

import Control.Monad.Reader(liftIO,ask)
import Data.IORef(atomicModifyIORef, IORef)
import Data.Either

commitAction :: IDEAction
commitAction = createSVNActionFromContext GUISvn.showCommitGUI

updateAction :: IDEAction
updateAction = createSVNActionFromContext $ GUISvn.showUpdateGUI



viewLogAction :: IDEAction
viewLogAction = createSVNActionFromContext GUISvn.showLogGUI

createSVNActionFromContext :: (Either GUISvn.Handler (Maybe String)
                                -> WC.Ctx())
                           -> IDEAction
createSVNActionFromContext action = do
    (mergeInfo, mbPw) <- readIDE vcsData
    e <- ask
    case mbPw of
            Nothing -> createActionFromContext $ action $ Left $ passwordHandler e mergeInfo
            Just mb -> createActionFromContext $ action $ Right mb
    where
        passwordHandler :: IORef IDE-> Maybe MergeId -> ((Maybe (Bool, Maybe String)) -> WC.Ctx ())
        passwordHandler e mbMergeInfo result = liftIO $ do
            case result of
                Just (True, pw) -> modifyIDE_' e (\ide -> ide {vcsData = (mbMergeInfo, Just pw) })
                _               -> return ()
        modifyIDE_' e f = do
                liftIO (atomicModifyIORef e f')
                where
                    f' a  = (f a,())


