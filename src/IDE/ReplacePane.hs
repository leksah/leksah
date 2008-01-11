{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.ReplacePane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The pane of ide for replacing text in a text buffer
--
-------------------------------------------------------------------------------

module IDE.ReplacePane (
    getReplace
,   doReplace
--,   getFindEntry
--,   getCaseSensitive
--,   getWrapAround
--,   getEntireWord
--,   getGotoLineSpin
) where

import Graphics.UI.Gtk hiding (get)
import Data.Maybe
import Control.Monad.Reader
import Data.List

import IDE.Core.State
import IDE.Framework.ViewFrame
import IDE.Framework.MakeEditor
import IDE.SourceEditor
import IDE.Framework.Parameters
import IDE.Framework.SimpleEditors
import IDE.Framework.EditorBasics

emptyReplaceState = ReplaceState "" "" False False False

instance Pane IDEReplace
    where
    primPaneName _  =   "Replace"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . replaceBox
    paneId b        =   "*Replace"
    makeActive p    =   error "don't activate replace bar"
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

instance ModelPane IDEReplace ReplaceState where
    saveState p     =   do
        mbFind <- getPane ReplaceCasting
        case mbFind of
            Nothing ->  return Nothing
            Just p  ->  return Nothing
                        --lift $ do
                        --return (Just (StateC ReplaceState))
    recoverState pp st  =  do
            nb          <-  getNotebook pp
            initReplace pp nb st

doReplace :: IDEAction
doReplace = do
    replace <- getReplace
    lift $ bringPaneToFront replace
    lift $ widgetGrabFocus (replaceBox replace)

getReplace :: IDEM IDEReplace
getReplace = do
    mbReplace <- getPane ReplaceCasting
    case mbReplace of
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (controlPanePath prefs) layout
            nb          <-  getNotebook pp
            initReplace pp nb emptyReplaceState
            mbReplace <- getPane ReplaceCasting
            case mbReplace of
                Nothing ->  error "Can't init replace pane"
                Just m  ->  return m
        Just m ->   return m

initReplace :: PanePath -> Notebook -> ReplaceState -> IDEAction
initReplace panePath nb replace = do
    ideR        <-  ask
    panes       <-  readIDE panes
    paneMap     <-  readIDE paneMap
    prefs       <-  readIDE prefs
    currentInfo <-  readIDE currentInfo
    (buf,cids)  <-  lift $ do
            hb      <- hBoxNew False 0
            bb      <- hButtonBoxNew

            replAll <- buttonNewWithMnemonic "Replace _all"
            replB   <- buttonNewWithMnemonic "_Replace"
            find    <- buttonNewWithMnemonic "_Find"

            boxPackStart bb replAll PackNatural 0
            boxPackStart bb replB PackNatural 0
            boxPackStart bb find PackNatural 0

            resList <- mapM (\ fd -> (fieldEditor fd) replace) replaceDescription
            let (widgetsP, setInjsP, getExtsP, notifiersP) = unzip4 resList
            mapM_ (\ w -> boxPackStart hb w PackNatural 0) widgetsP
            let fieldNames = map (\fd -> case getParameterPrim paraName (parameters fd) of
                                                Just s -> s
                                                Nothing -> "Unnamed") replaceDescription
            find `onClicked` do
                findOrSearch editFind getExtsP fieldNames replB replAll ideR

            replB `onClicked` do
                findOrSearch editReplace getExtsP fieldNames replB replAll ideR

            replAll `onClicked` do
                findOrSearch editReplaceAll getExtsP fieldNames replB replAll ideR

            let replace = IDEReplace hb
            notebookInsertOrdered nb hb (paneName replace)
            widgetShowAll hb
            return (replace,[])
    addPaneAdmin buf (BufConnections [] [] []) panePath
    lift $widgetGrabFocus (replaceBox buf)
    where
        findOrSearch :: (Bool -> Bool -> Bool -> String -> String -> SearchHint -> IDEM Bool)
                    -> [ReplaceState -> Extractor ReplaceState] -> [String] -> Button ->
                        Button -> IDERef -> IO()
        findOrSearch f getExtsP fieldNames replB replAll ideR=  do
            mbReplaceState <- extractAndValidate replace getExtsP fieldNames
            case mbReplaceState of
                Nothing -> return ()
                Just rs -> do
                    let hint = if searchBackwards rs then Backward else Forward
                    found <- runReaderT (f (matchEntire rs) (matchCase rs) False
                                            (searchFor rs) (replaceWith rs) hint) ideR
                    widgetSetSensitivity replB found
                    widgetSetSensitivity replAll found
                    return ()



replaceDescription :: [FieldDescription ReplaceState]
replaceDescription = [
        mkField
            (paraName <<<- ParaName "Search for" $ emptyParams)
            searchFor
            (\ b a -> a{searchFor = b})
            stringEditor
    ,   mkField
            (paraName <<<- ParaName "Replace with" $ emptyParams)
            replaceWith
            (\ b a -> a{replaceWith = b})
            stringEditor
    ,   mkField
            (paraName <<<- ParaName "Match case" $ emptyParams)
            matchCase
            (\ b a -> a{matchCase = b})
            boolEditor
    ,   mkField
            (paraName <<<- ParaName "Entire word" $ emptyParams)
            matchEntire
            (\ b a -> a{matchEntire = b})
            boolEditor
    ,   mkField
            (paraName <<<- ParaName "Search backwards" $ emptyParams)
            searchBackwards
            (\ b a -> a{searchBackwards = b})
            boolEditor]



{--
replaceDialog :: IDEAction
replaceDialog = do
    ideR <- ask
    lift $replaceDialog' emptyReplaceState replaceDescription ideR


replaceDialog' :: ReplaceState -> [FieldDescription ReplaceState] -> IDERef -> IO ()
replaceDialog' replace replaceDesc ideR  = do
    dialog  <- windowNew
    vb      <- vBoxNew False 0
    bb      <- hButtonBoxNew
    close   <- buttonNewFromStock "gtk-close"
    replAll <- buttonNewWithMnemonic "Replace _all"
    replB   <- buttonNewWithMnemonic "_Replace"
    find    <- buttonNewWithMnemonic "_Find"
    boxPackStart bb close PackNatural 0
    boxPackStart bb replAll PackNatural 0
    boxPackStart bb replB PackNatural 0
    boxPackStart bb find PackNatural 0
    resList <- mapM (\ fd -> (fieldEditor fd) replace) replaceDesc
    let (widgetsP, setInjsP, getExtsP, notifiersP) = unzip4 resList
    mapM_ (\ w -> boxPackStart vb w PackNatural 0) widgetsP
    let fieldNames = map (\fd -> case getParameterPrim paraName (parameters fd) of
                                        Just s -> s
                                        Nothing -> "Unnamed") replaceDesc
    find `onClicked` do
        findOrSearch editFind getExtsP fieldNames replB replAll
    replB `onClicked` do
        findOrSearch editReplace getExtsP fieldNames replB replAll
    replAll `onClicked` do
        findOrSearch editReplaceAll getExtsP fieldNames replB replAll
    close `onClicked` do
        widgetDestroy dialog
        mainQuit
    dialog `onDelete` (\_ -> do
        widgetDestroy dialog
        mainQuit
        return True)
    boxPackEnd vb bb PackNatural 7
    containerAdd dialog vb
    widgetShowAll dialog
    mainGUI
    where


--getFindEntry :: IDEM Entry
--getFindEntry = do
--    f <- getFind
--    return (findEntry f)
--
--getCaseSensitive :: IDEM ToggleButton
--getCaseSensitive = do
--    f <- getFind
--    return (caseSensitive f)
--
--getWrapAround :: IDEM ToggleButton
--getWrapAround = do
--    f <- getFind
--    return (wrapAround f)
--
--getEntireWord :: IDEM ToggleButton
--getEntireWord = do
--    f <- getFind
--    return (entireWord f)
--
--getGotoLineSpin :: IDEM SpinButton
--getGotoLineSpin = do
--    f <- getFind
--    return (gotoLine f)


--}
