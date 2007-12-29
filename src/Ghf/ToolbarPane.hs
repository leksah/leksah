{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Ghf.ToolbarPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability  :  portable
--
-- | The pane of ghf where modules are presented in tree form with their
--   packages and exports
--
-------------------------------------------------------------------------------

module Ghf.ToolbarPane (
    getToolbar
) where

import Graphics.UI.Gtk hiding (get)
import Data.Maybe
import Control.Monad.Reader
import Data.List

import Ghf.Core.State
import Ghf.ViewFrame
import Ghf.File
import Ghf.Keymap
import {-# SOURCE #-} Ghf.Menu

instance Pane GhfToolbar
    where
    primPaneName _  =   "Toolbar"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . toolbar
    paneId b        =   "*Toolbar"
    makeActive p    =   error "don't activate toolbar"
    close pane      =   do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  lift $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  lift $ do
                putStrLn "notebook page not found: unexpected"
                return ()
            Just i  ->  do
                deactivatePaneIfActive pane
                lift $notebookRemovePage nb i
                removePaneAdmin pane


instance ModelPane GhfToolbar ToolbarState where
    saveState p     =   do
        mbToolbar <- getPane ToolbarCasting
        case mbToolbar of
            Nothing ->  return Nothing
            Just p  ->  lift $ do
                return (Just (StateC ToolbarState))
    recoverState pp ToolbarState  =  do
            nb          <-  getNotebook pp
            initToolbar pp nb

getToolbar :: GhfM GhfToolbar
getToolbar = do
    mbToolbar <- getPane ToolbarCasting
    case mbToolbar of
        Nothing -> do
            prefs       <-  readGhf prefs
            layout      <-  readGhf layout
            let pp      =   getStandardPanePath (controlPanePath prefs) layout
            nb          <-  getNotebook pp
            initToolbar pp nb
            mbToolbar <- getPane ToolbarCasting
            case mbToolbar of
                Nothing ->  error "Can't init control pane"
                Just m  ->  return m
        Just m ->   return m

initToolbar :: PanePath -> Notebook -> GhfAction
initToolbar panePath nb = do
    ghfR        <-  ask
    prefs       <-  readGhf prefs
    currentInfo <-  readGhf currentInfo
    uiManager   <-  readGhf uiManager
    keysPath    <-  lift $ getConfigFilePathForLoad $keymapName prefs ++ ".keymap"
    keyMap      <-  lift $ parseKeymap keysPath
    let accelActions = setKeymap actions keyMap
    (acc,menus) <-  makeMenu uiManager accelActions menuDescription
    let tb = case menus !! 1 of
                Just m -> m
                Nothing -> error "Failed to build toolbar"
    (buf,cids)  <-  lift $ do
        let toolbar' = GhfToolbar (castToToolbar tb)
        notebookPrependPage nb tb (paneName toolbar')
        widgetShowAll tb
        mbPn <- notebookPageNum nb tb
        case mbPn of
            Just i -> notebookSetCurrentPage nb i
            Nothing -> putStrLn "Notebook page not found"
        return (toolbar',[])
    addPaneAdmin buf (BufConnections [] [] []) panePath
    lift $widgetGrabFocus (toolbar buf)

