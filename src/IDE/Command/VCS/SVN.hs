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
    checkoutAction
    ,commitAction
    ,updateAction
    ,viewLogAction
) where

import Graphics.UI.Gtk.ActionMenuToolbar.UIManager(MergeId)

import qualified VCSGui.Svn as SvnGUI
import qualified VCSWrapper.Svn as SvnC

import IDE.Command.VCS.Common

import IDE.Core.Types
import IDE.Core.State

import Control.Monad.Reader(liftIO,ask)
import Data.IORef(atomicModifyIORef, IORef)
import Data.Either

checkoutAction :: IDEAction
checkoutAction = do
    createActionFromContext SvnGUI.showCheckoutGUI

commitAction :: IDEAction
commitAction = do
    (mergeInfo, mbPw) <- readIDE vcsData
    e <- ask
    case mbPw of
            Nothing -> createActionFromContext $ SvnGUI.showCommitGUI $ Left $ passwordHandler e mergeInfo
            Just mb -> createActionFromContext $ SvnGUI.showCommitGUI $ Right mb
    where
        passwordHandler :: IORef IDE-> Maybe MergeId -> ((Maybe (Bool, Maybe String)) -> SvnC.Ctx ())
        passwordHandler e mbMergeInfo result = liftIO $ do
            case result of
                Just (True, pw) -> modifyIDE_' e (\ide -> ide {vcsData = (mbMergeInfo, Just pw) })
                _               -> return ()
        modifyIDE_' e f = do
                liftIO (atomicModifyIORef e f')
                where
                    f' a  = (f a,())

--pw = Maybe Maybe String, Nothing = not set, Just Nothing = passwort set to not use, Just String = passwort set

updateAction :: IDEAction
updateAction = do
    createActionFromContext SvnC.update

viewLogAction :: IDEAction
viewLogAction = do
    createActionFromContext SvnGUI.showLogGUI




