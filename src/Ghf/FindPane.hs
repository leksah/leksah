{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Ghf.FindPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The pane of ghf for searching in a text buffer
--
-------------------------------------------------------------------------------

module Ghf.FindPane (
    getFind
,   getFindEntry
,   getCaseSensitive
,   getWrapAround
,   getEntireWord
,   getGotoLineSpin
) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.ModelView as New
import System.Glib.Signals
import Data.Maybe
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree
import Data.List
import Distribution.Package
import Distribution.PackageDescription
import System.Glib.GObject

import Ghf.Core.State
import Ghf.ViewFrame
import Ghf.File
import Ghf.Keymap
import Ghf.SourceEditor


instance Pane GhfFind
    where
    primPaneName _  =   "Find"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . findBox
    paneId b        =   "*Find"
    makeActive p    =   error "don't activate find bar"
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


instance ModelPane GhfFind FindState where
    saveState p     =   do
        mbFind <- getPane FindCasting
        case mbFind of
            Nothing ->  return Nothing
            Just p  ->  lift $ do
                return (Just (StateC FindState))
    recoverState pp FindState  =  do
            nb          <-  getNotebook pp
            initFind pp nb

getFind :: GhfM GhfFind
getFind = do
    mbFind <- getPane FindCasting
    case mbFind of
        Nothing -> do
            prefs       <-  readGhf prefs
            layout      <-  readGhf layout
            let pp      =   getStandardPanePath (controlPanePath prefs) layout
            nb          <-  getNotebook pp
            initFind pp nb
            mbFind <- getPane FindCasting
            case mbFind of
                Nothing ->  error "Can't init find pane"
                Just m  ->  return m
        Just m ->   return m

initFind :: PanePath -> Notebook -> GhfAction
initFind panePath nb = do
    ghfR        <-  ask
    panes       <-  readGhf panes
    paneMap     <-  readGhf paneMap
    prefs       <-  readGhf prefs
    currentInfo <-  readGhf currentInfo
    (buf,cids)  <-  lift $ do
            hBox <- hBoxNew False 10
            bBox <- hButtonBoxNew

            entry <- entryNew
            widgetSetName entry "searchEntry"

            caseSensitiveButton <- toggleButtonNew
            buttonSetLabel caseSensitiveButton "Case sensitive"
            widgetSetName caseSensitiveButton "caseSensitiveButton"

            entireWordButton <- toggleButtonNew
            buttonSetLabel entireWordButton "Entire word"
            widgetSetName entireWordButton "entireWordButton"

            wrapAroundButton <- toggleButtonNew
            buttonSetLabel wrapAroundButton "Wrap around"
            toggleButtonSetActive wrapAroundButton True
            widgetSetName wrapAroundButton "wrapAroundButton"

            spinL <- spinButtonNewWithRange 1.0 1000.0 10.0
            widgetSetName spinL "gotoLineEntry"

            boxPackStart hBox entry PackNatural 10
            boxPackStart bBox caseSensitiveButton PackNatural 10
            boxPackStart bBox entireWordButton PackNatural 10
            boxPackStart bBox wrapAroundButton PackNatural 10
            boxPackStart hBox bBox PackNatural 10
            boxPackStart hBox spinL PackNatural 10

            entry `afterInsertText` (\ _ _ -> do
                runReaderT (editFindInc Insert) ghfR
                t <- entryGetText entry
                return (length t))
            entry `afterDeleteText` (\ _ _ -> do runReaderT (editFindInc Delete) ghfR; return ())
            entry `afterKeyPress`  (\ e -> do runReaderT (editFindKey e) ghfR; return True)

            spinL `afterKeyPress`  (\ e -> do runReaderT (editGotoLineKey e) ghfR; return True)
            spinL `afterEntryActivate` runReaderT editGotoLineEnd ghfR
            spinL `afterFocusOut` (\ _ -> do runReaderT editGotoLineEnd ghfR; return False)
            let find = GhfFind hBox caseSensitiveButton wrapAroundButton entireWordButton
                                    spinL entry
            notebookPrependPage nb hBox (paneName find)
            widgetShowAll hBox
            mbPn <- notebookPageNum nb hBox
            case mbPn of
                Just i -> notebookSetCurrentPage nb i
                Nothing -> putStrLn "Notebook page not found"
            return (find,[])
    addPaneAdmin buf (BufConnections [] [] []) panePath
    lift $widgetGrabFocus (findBox buf)

getFindEntry :: GhfM Entry
getFindEntry = do
    f <- getFind
    return (findEntry f)

getCaseSensitive :: GhfM ToggleButton
getCaseSensitive = do
    f <- getFind
    return (caseSensitive f)

getWrapAround :: GhfM ToggleButton
getWrapAround = do
    f <- getFind
    return (wrapAround f)

getEntireWord :: GhfM ToggleButton
getEntireWord = do
    f <- getFind
    return (entireWord f)

getGotoLineSpin :: GhfM SpinButton
getGotoLineSpin = do
    f <- getFind
    return (gotoLine f)

