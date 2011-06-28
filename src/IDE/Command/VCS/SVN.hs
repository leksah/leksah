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

import qualified VCSGui.Svn as SvnGUI
import qualified VCSWrapper.Svn as SvnC

import IDE.Command.VCS.Common

import IDE.Core.Types
import IDE.Core.State

import Control.Monad.Reader(liftIO)

checkoutAction :: IDEAction
checkoutAction = do
    createActionFromContext SvnGUI.showCheckoutGUI

commitAction :: IDEAction
commitAction = do
    let mbPassword = Nothing --  TODO get password here
    case mbPassword of
            Nothing -> createActionFromContext (SvnGUI.showCommitGUI (Just passwordHandler))
            _       -> createActionFromContext (SvnGUI.showCommitGUI Nothing)
    where
        passwordHandler :: (Maybe (Maybe (Bool, String)) -> SvnC.Ctx ())
        passwordHandler result = liftIO $ do
            case result of
                Just (Just (True, pw)) -> return () --  TODO store password here
                _                      -> return ()



updateAction :: IDEAction
updateAction = do
    createActionFromContext SvnC.update

viewLogAction :: IDEAction
viewLogAction = do
    createActionFromContext SvnGUI.showLogGUI




