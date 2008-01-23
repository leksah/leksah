{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.InfoPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The GUI stuff for infos
--
-------------------------------------------------------------------------------

module IDE.InfoPane (
    setInfos
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Control.Monad.Reader
import System.IO
import Control.Monad
import Control.Monad.Trans
import System.IO
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.IORef(newIORef,readIORef,writeIORef)

import IDE.Core.State
import IDE.Framework.ViewFrame
import IDE.Framework.MakeEditor
import IDE.Framework.SimpleEditors
import IDE.Framework.Parameters
import IDE.SourceEditor
import {-# SOURCE #-} IDE.ModulesPane
import IDE.CallersPane

instance IDEObject IDEInfo

instance Pane IDEInfo
    where
    primPaneName _  =   "Info"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . sw
    paneId b        =   "*Info"
    makeActive pane = activatePane pane (BufConnections[][][])
    close pane     =   do
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

instance RecoverablePane IDEInfo InfoState where
    saveState p     =   do
        currentIDsU <- lift $ readIORef (currentIDs p)
        currentIndU <- lift $ readIORef (currentInd p)
        return (Just (StateC (InfoState currentIDsU currentIndU)))
    recoverState pp (InfoState currentIDsU currentIndu) =   do
        nb <- getNotebook pp
        initInfo pp nb currentIDsU currentIndu

idDescrDescr :: [FieldDescription IdentifierDescr]
idDescrDescr = [
            mkField
            (paraName <<<- ParaName "Symbol"
                $ paraHorizontal <<<- ParaHorizontal StartHorizontal
                    $ emptyParams)
            identifierID
            (\ b a -> a{identifierID = b})
            stringEditor
    ,    mkField
            (paraName <<<- ParaName "Sort"
                    $ emptyParams)
            idType
            (\b a -> a)
            (staticSelectionEditor allIdTypes)
    ,   mkField
            (paraName <<<- ParaName "Exported by"
                $ paraHorizontal <<<- ParaHorizontal StopHorizontal
                        $ emptyParams)
            (\l -> showPackModule (moduleIdID l))
            (\ b a -> a{moduleIdID = parsePackModule b})
            stringEditor
    ,   mkField
            (paraName  <<<- ParaName "Type" $ emptyParams)
            (BS.unpack . typeInfo)
            (\b a -> a)
            multilineStringEditor
    ,   mkField
            (paraName <<<- ParaName "Comment" $ emptyParams)
            (\l -> case mbComment l of
                    Nothing -> ""
                    Just s -> BS.unpack s)
            (\ b a -> case b of
                        "" -> a{mbComment = Nothing}
                        s  -> a{mbComment = Just (BS.pack s)})
            multilineStringEditor]

{--    ,   mkField (emptyParams
            {paraName = Just "Documentation"})
            typeInfo
            (\b a -> a{typeInfo = b})
            multilineStringEditor--}

allIdTypes = [Function,Newtype,Synonym,AbstractData,Foreign]

initInfo :: PanePath -> Notebook -> [IdentifierDescr] -> Int -> IDEAction
initInfo panePath nb idDescrs index = do
    when (length idDescrs > index) $ do
    ideR <- ask
    panes <- readIDE panes
    paneMap <- readIDE paneMap
    prefs <- readIDE prefs
    (pane,cids) <- lift $ do
            nbbox       <- hBoxNew False 0
            ibox        <- vBoxNew False 0
            bb          <- vButtonBoxNew
            buttonBoxSetLayout bb ButtonboxStart
            definitionB <- buttonNewWithLabel "Source"
            moduB       <- buttonNewWithLabel "Selection"
            usesB       <- buttonNewWithLabel "Uses"
            docuB       <- buttonNewWithLabel "Docu"
            widgetSetSensitivity docuB False
            nextB       <- buttonNewWithLabel "Next"
            prevB       <- buttonNewWithLabel "Prev"
            when (length idDescrs < 2) $ widgetSetSensitivity nextB False
            widgetSetSensitivity prevB False
            label       <- labelNew (Just ("1" ++ "/" ++ show (length idDescrs)))
            boxPackStart bb definitionB PackNatural 0
            boxPackStart bb moduB PackNatural 0
            boxPackStart bb usesB PackNatural 0
            boxPackStart bb docuB PackNatural 0
            boxPackStart bb label PackNatural 0
            boxPackStart bb nextB PackNatural 0
            boxPackStart bb prevB PackNatural 0

            resList <- mapM (\ fd -> (fieldEditor fd) (head idDescrs)) idDescrDescr
            let (widgets, setInjs, getExts, notifiers) = unzip4 resList
            foldM_ (\ box (w,mbh)  ->
                case mbh of
                    Keep            ->  do  boxPackStart box w PackGrow 0
                                            return box
                    StartHorizontal ->  do  newBox  <- hBoxNew True 0
                                            boxPackStart box newBox PackNatural 0
                                            boxPackStart newBox w PackGrow 0
                                            return (castToBox newBox)
                    StopHorizontal  ->  do  boxPackStart box w PackGrow 0
                                            par <- widgetGetParent box
                                            case par of
                                                Nothing -> throwIDE "initInfo - no parent"
                                                Just p -> return (castToBox p))
                (castToBox ibox)
                (zip widgets (map (getParameter paraHorizontal . parameters)
                    idDescrDescr))
            boxPackStart nbbox ibox PackGrow 0
            boxPackEnd nbbox bb PackNatural 0
            --openType
            sw <- scrolledWindowNew Nothing Nothing
            scrolledWindowAddWithViewport sw nbbox
            scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
            currentIDs' <-  newIORef idDescrs
            currentInd  <-  newIORef 0
            let info = IDEInfo sw currentIDs' currentInd setInjs getExts nextB prevB label

            --mapM_ (\w -> widgetSetExtensionEvents w [ExtensionEventsAll]) widgets
            cids <- mapM
                (\w -> w `onFocus` --onFocusIn doesn't work here - why?
                    (\_ ->  do  runReaderT (makeActive info) ideR
                                return False))
                        widgets
            definitionB `onClicked` (runReaderT gotoSource ideR)
            moduB `onClicked` (runReaderT gotoModule' ideR)
            usesB `onClicked` (runReaderT calledBy' ideR)
            nextB `onClicked` (next info)
            prevB `onClicked` (prev info)
            notebookInsertOrdered nb sw (paneName info)
            widgetShowAll sw
            return (info,cids)
    addPaneAdmin pane (BufConnections [] [] []) panePath
    lift $widgetGrabFocus (sw pane)
    lift $bringPaneToFront pane

next :: IDEInfo -> IO ()
next (IDEInfo _ currentIDs' currentInd' injectors' _ nextB prevB label)  = do
    currentIdsU <-  readIORef currentIDs'
    currentIndU <-  readIORef currentInd'
    when (length currentIdsU > currentIndU + 1) $ do
        writeIORef currentInd' (currentIndU + 1)
        mapM_ (\ a -> a (currentIdsU !! (currentIndU + 1)))  injectors'
        when (length currentIdsU == currentIndU + 1) $ widgetSetSensitivity nextB False
        widgetSetSensitivity prevB True
        labelSetText label (show (currentIndU + 2) ++ "/" ++ show (length currentIdsU))


prev :: IDEInfo -> IO ()
prev (IDEInfo _ currentIDs' currentInd' injectors' _ nextB prevB label) = do
    currentIdsU <-  readIORef currentIDs'
    currentIndU <-  readIORef currentInd'
    when (currentIndU >= 1) $ do
        writeIORef currentInd' (currentIndU - 1)
        mapM_ (\ a -> a (currentIdsU !! (currentIndU - 1)))  injectors'
        when (currentIndU - 1 == 0) $ widgetSetSensitivity prevB False
        widgetSetSensitivity nextB True
        labelSetText label (show currentIndU ++ "/" ++ show (length currentIdsU))

gotoSource :: IDEAction
gotoSource = do
    mbInfo <- getInfoCont
    case mbInfo of
        Nothing     ->  do  ideMessage Normal "gotoSource:noDefition"
                            return ()
        Just info   ->  do  goToDefinition info
                            return ()

gotoModule' :: IDEAction
gotoModule' = do
    mbInfo <- getInfoCont
    case mbInfo of
        Nothing     ->  return ()
        Just info   ->  do  selectIdentifier info
                            return ()

calledBy' :: IDEAction
calledBy' = do
    mbInfo <- getInfoCont
    case mbInfo of
        Nothing     ->  return ()
        Just info   ->  do  calledBy info
                            return ()

setInfos :: [IdentifierDescr] -> IDEM ()
setInfos identifierDescrs = do
    mbPane <-  getPane InfoCasting
    case mbPane of
        Nothing -> do
            prefs   <- readIDE prefs
            layout  <- readIDE layout
            let pp  =  getStandardPanePath (infoPanePath prefs) layout
            nb      <- getNotebook pp
            initInfo pp nb identifierDescrs 0
        Just info -> lift $ do
            writeIORef (currentInd info) 0
            writeIORef (currentIDs info) identifierDescrs
            mapM_ (\ a -> a (head identifierDescrs))  (injectors info)
            labelSetText (numLabel info) ("1/" ++ show (length identifierDescrs))
            widgetSetSensitivity (prevB info) False
            widgetSetSensitivity (nextB info)
                (if length identifierDescrs > 1 then True else False)
            bringPaneToFront info


getInfoCont ::  IDEM (Maybe (IdentifierDescr))
getInfoCont = do
    mbPane <- getPane InfoCasting
    case mbPane of
        Nothing ->  return Nothing
        Just p  ->  lift $ do
            currentIndU     <-  readIORef (currentInd p)
            currentIDsU     <-  readIORef (currentIDs p)
            if length currentIDsU > currentIndU
                then return (Just (currentIDsU !! currentIndU))
                else return Nothing

