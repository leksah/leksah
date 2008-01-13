{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.FindPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The pane of ide for searching in a text buffer
--
-------------------------------------------------------------------------------

module IDE.FindPane (
    getFind
,   getFindEntry
,   getCaseSensitive
,   getWrapAround
,   getEntireWord
,   getGotoLineSpin
) where

import Graphics.UI.Gtk hiding (get)
import Data.Maybe
import Control.Monad.Reader
import Data.List

import {-# SOURCE #-} IDE.Core.State
import IDE.Framework.ViewFrame
import IDE.SourceEditor


instance Pane IDEFind
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
                lift $notebookRemovePage nb i
                removePaneAdmin pane


instance ModelPane IDEFind FindState where
    saveState p     =   do
        mbFind <- getPane FindCasting
        case mbFind of
            Nothing ->  return Nothing
            Just p  ->  lift $ do
                return (Just (StateC FindState))
    recoverState pp FindState  =  do
            nb          <-  getNotebook pp
            initFind pp nb

getFind :: IDEM IDEFind
getFind = do
    mbFind <- getPane FindCasting
    case mbFind of
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (controlPanePath prefs) layout
            nb          <-  getNotebook pp
            initFind pp nb
            mbFind <- getPane FindCasting
            case mbFind of
                Nothing ->  error "Can't init find pane"
                Just m  ->  return m
        Just m ->   return m

initFind :: PanePath -> Notebook -> IDEAction
initFind panePath nb = do
    ideR        <-  ask
    panes       <-  readIDE panes
    paneMap     <-  readIDE paneMap
    prefs       <-  readIDE prefs
    currentInfo <-  readIDE currentInfo
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
                runReaderT (editFindInc Insert) ideR
                t <- entryGetText entry
                return (length t))
            entry `afterDeleteText` (\ _ _ -> do runReaderT (editFindInc Delete) ideR; return ())
            entry `afterKeyPress`  (\ e -> do runReaderT (editFindKey e) ideR; return True)

            spinL `afterKeyPress`  (\ e -> do runReaderT (editGotoLineKey e) ideR; return True)
            spinL `afterEntryActivate` runReaderT editGotoLineEnd ideR
            spinL `afterFocusOut` (\ _ -> do runReaderT editGotoLineEnd ideR; return False)
            let find = IDEFind hBox caseSensitiveButton wrapAroundButton entireWordButton
                                    spinL entry
            notebookInsertOrdered nb hBox (paneName find)
            widgetShowAll hBox
            return (find,[])
    addPaneAdmin buf (BufConnections [] [] []) panePath
    lift $widgetGrabFocus (findBox buf)

getFindEntry :: IDEM Entry
getFindEntry = do
    f <- getFind
    return (findEntry f)

getCaseSensitive :: IDEM ToggleButton
getCaseSensitive = do
    f <- getFind
    return (caseSensitive f)

getWrapAround :: IDEM ToggleButton
getWrapAround = do
    f <- getFind
    return (wrapAround f)

getEntireWord :: IDEM ToggleButton
getEntireWord = do
    f <- getFind
    return (entireWord f)

getGotoLineSpin :: IDEM SpinButton
getGotoLineSpin = do
    f <- getFind
    return (gotoLine f)

