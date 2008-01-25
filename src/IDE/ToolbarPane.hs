{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.ToolbarPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability  :  portable
--
-- | The pane of ide where modules are presented in tree form with their
--   packages and exports
--
-------------------------------------------------------------------------------

module IDE.ToolbarPane (
    getToolbar
) where

import Graphics.UI.Gtk hiding (get)
import Data.Maybe
import Control.Monad.Reader

import IDE.Core.State
import IDE.Framework.ViewFrame

instance IDEObject IDEToolbar
instance Pane IDEToolbar
    where
    primPaneName _  =   "Toolbar"
    getTopWidget    =   castToWidget . toolbar
    paneId b        =   "*Toolbar"
    makeActive p    =   throwIDE "don't activate toolbar"
    close pane      =   do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  lift $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  lift $ do
                sysMessage Normal "notebook page not found: unexpected"
                return ()
            Just i  ->  do
                deactivatePaneIfActive pane
                lift $notebookRemovePage nb i
                removePaneAdmin pane


instance RecoverablePane IDEToolbar ToolbarState where
    saveState p     =   do
        mbToolbar <- getPane ToolbarCasting
        case mbToolbar of
            Nothing ->  return Nothing
            Just p  ->  lift $ do
                return (Just (StateC ToolbarState))
    recoverState pp ToolbarState  =  do
            nb          <-  getNotebook pp
            initToolbar pp nb

getToolbar :: IDEM IDEToolbar
getToolbar = do
    mbToolbar <- getPane ToolbarCasting
    case mbToolbar of
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (controlPanePath prefs) layout
            nb          <-  getNotebook pp
            initToolbar pp nb
            mbToolbar <- getPane ToolbarCasting
            case mbToolbar of
                Nothing ->  throwIDE "Can't init control pane"
                Just m  ->  return m
        Just m ->   return m

initToolbar :: PanePath -> Notebook -> IDEAction
initToolbar panePath nb = do
    ideR        <-  ask
    st          <- getIDE
    currentInfo <-  readIDE currentInfo
    res         <-  triggerEvent st (GetToolbar [])
    let (tb1,tb2) =   case res of
                        GetToolbar (_:t1:t2:_) -> (t1,t2)
                        _ -> throwIDE "Failed to build toolbar"
    (buf,cids)  <-  lift $ do
        vb      <- vBoxNew False 0
        boxPackStart vb tb1 PackNatural 0
        boxPackStart vb tb2 PackNatural 0
        let toolbar' = IDEToolbar vb
        notebookInsertOrdered nb vb (paneName toolbar')
        widgetShowAll vb
        return (toolbar',[])
    addPaneAdmin buf (BufConnections [] [] []) panePath
    lift $widgetGrabFocus (toolbar buf)

