{-# OPTIONS_GHC -XDeriveDataTypeable -XMultiParamTypeClasses
    -XScopedTypeVariables -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Info
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | The GUI stuff for infos
--
-------------------------------------------------------------------------------

module IDE.Pane.Info (
    IDEInfo
,   InfoState
,   setInfo
,   setSymbol
,   replayInfoHistory
,   showInfo
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Control.Monad.Reader
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Data.IORef
import Data.Typeable

import IDE.Core.State
import Control.Event
import IDE.Pane.SourceBuffer
import IDE.Pane.References
import IDE.FileUtils (openBrowser)
import IDE.Metainfo.Provider (getIdentifierDescr)
import Graphics.UI.Gtk.SourceView


-- | An info pane description
--
data IDEInfo        =   IDEInfo {
    sw              ::   VBox
,   currentDescr    ::   IORef Descr
,   descriptionView ::   SourceView
} deriving Typeable

data InfoState              =   InfoState Descr
    deriving(Eq,Ord,Read,Show,Typeable)


instance IDEObject IDEInfo

instance Pane IDEInfo IDEM
    where
    primPaneName _  =   "Info"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . sw
    paneId b        =   "*Info"
    makeActive pane =   activatePane pane []
    close           =   closePane

instance RecoverablePane IDEInfo InfoState IDEM where
    saveState p     =   do
        currentDescr' <-  liftIO $ readIORef (currentDescr p)
        return (Just (InfoState currentDescr'))
    recoverState pp (InfoState descr) =   do
        setInfo descr

showInfo :: IDEAction
showInfo = do
    mbInfo :: Maybe IDEInfo <- getPane
    case mbInfo of
        Nothing -> return ()
        Just p  -> liftIO $ bringPaneToFront p


initInfo :: PanePath -> Notebook -> Descr -> IDEAction
initInfo panePath nb idDescr = do
    panes       <- readIDE panes
    paneMap     <- readIDE paneMap
    prefs       <- readIDE prefs
    (pane,cids) <- reifyIDE $ \ideR  ->  do
            ibox        <- vBoxNew False 0
    -- Buttons
            bb          <- hButtonBoxNew
            buttonBoxSetLayout bb ButtonboxSpread
            definitionB <- buttonNewWithLabel "Source"
            moduB       <- buttonNewWithLabel "Modules"
            usesB       <- buttonNewWithLabel "Refs"
            docuB       <- buttonNewWithLabel "Docu"
            searchB     <- buttonNewWithLabel "Search"
            boxPackStartDefaults bb definitionB
            boxPackStartDefaults bb moduB
            boxPackStartDefaults bb usesB
            boxPackStartDefaults bb docuB
            boxPackStartDefaults bb searchB
    -- Descr View
            font <- case textviewFont prefs of
                Just str -> do
                    fontDescriptionFromString str
                Nothing -> do
                    f <- fontDescriptionNew
                    fontDescriptionSetFamily f "Monospace"
                    return f

            descriptionView <- sourceViewNew
            descriptionBuffer <- (get descriptionView textViewBuffer) >>= (return . castToSourceBuffer)
            lm <- sourceLanguageManagerNew
            mbLang <- sourceLanguageManagerGuessLanguage lm Nothing (Just "text/x-haskell")
            case mbLang of
                Nothing -> return ()
                Just lang -> do sourceBufferSetLanguage descriptionBuffer lang

            -- This call is here because in the past I have had problems where the
            -- language object became invalid if the manager was garbage collected
            sourceLanguageManagerGetLanguageIds lm

            sourceBufferSetHighlightSyntax descriptionBuffer True
            widgetModifyFont descriptionView (Just font)

            case sourceStyle prefs of
                Nothing  -> return ()
                Just str -> do
                    styleManager <- sourceStyleSchemeManagerNew
                    ids <- sourceStyleSchemeManagerGetSchemeIds styleManager
                    when (elem str ids) $ do
                        scheme <- sourceStyleSchemeManagerGetScheme styleManager str
                        sourceBufferSetStyleScheme descriptionBuffer scheme


            sw <- scrolledWindowNew Nothing Nothing
            containerAdd sw descriptionView
            scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic

            boxPackStart ibox sw PackGrow 10
            boxPackEnd ibox bb PackNatural 10

            --openType
            currentDescr' <- newIORef idDescr
            let info = IDEInfo ibox currentDescr' descriptionView
            definitionB `onClicked` (reflectIDE gotoSource ideR )
            moduB `onClicked` (reflectIDE gotoModule' ideR )
            usesB `onClicked` (reflectIDE referencedFrom' ideR )
            searchB `onClicked` (do
                descr <- readIORef currentDescr'
                reflectIDE (do
                    triggerEvent ideR (SearchMeta (descrName descr))
                    showInfo) ideR )
            docuB `onClicked` (do
                descr <- readIORef currentDescr'
                reflectIDE (openBrowser $ docuSearchURL prefs ++ descrName descr) ideR)
            descriptionView `widgetAddEvents` [ButtonReleaseMask]
            id5 <- descriptionView `onButtonRelease`
                (\ e -> do
                    buf     <-  textViewGetBuffer descriptionView
                    (l,r)   <- textBufferGetSelectionBounds buf
                    symbol  <- textBufferGetText buf l r True
                    when (controlIsPressed e)
                        (reflectIDE (do
                            triggerEvent ideR (SelectInfo symbol)
                            return ()) ideR)
                    return False)
            notebookInsertOrdered nb ibox (paneName info) Nothing
            widgetShowAll ibox
            return (info,[])
    addPaneAdmin pane [] panePath
    liftIO $widgetGrabFocus (sw pane)
    liftIO $bringPaneToFront pane

gotoSource :: IDEAction
gotoSource = do
    mbInfo <- getInfoCont
    case mbInfo of
        Nothing     ->  do  ideMessage Normal "gotoSource:noDefition"
                            return ()
        Just info   ->  goToDefinition info >> return ()

gotoModule' :: IDEAction
gotoModule' = do
    mbInfo  <-  getInfoCont
    ideR    <-  ask
    case mbInfo of
        Nothing     ->  return ()
        Just info   ->  triggerEvent ideR (SelectIdent info) >> return ()

referencedFrom' :: IDEAction
referencedFrom' = do
    mbInfo <- getInfoCont
    case mbInfo of
        Nothing     ->  return ()
        Just info   ->  referencedFrom info  >> return ()

setSymbol :: String -> IDEAction
setSymbol symbol = do
    ideR         <- ask
    currentInfo' <- readIDE currentInfo
    case currentInfo' of
        Nothing -> return ()
        Just ((_,symbolTable1),(_,symbolTable2)) ->
            case getIdentifierDescr symbol symbolTable1 symbolTable2 of
                []     -> return ()
                (a:r)  ->  do
                    setInfo a
                    showInfo
                    if length (a:r) > 1
                        then triggerEvent ideR (DescrChoice (a:r)) >> return ()
                        else triggerEvent ideR (SelectIdent a) >> return ()


setInfo :: Descr -> IDEAction
setInfo identifierDescr = do
    mbPane <-  getPane
    case mbPane of
        Nothing -> do
            prefs   <- readIDE prefs
            layout  <- readIDE layout
            let pp  =  getStandardPanePath (logPanePath prefs) layout
            nb      <- getNotebook pp
            initInfo pp nb identifierDescr
        Just info -> do
            oldDescr <- liftIO $ readIORef (currentDescr info)
            liftIO $ do
                writeIORef (currentDescr info) identifierDescr
                tb <- get (descriptionView info) textViewBuffer
                textBufferSetText tb (show (Present identifierDescr))
            recordInfoHistory identifierDescr oldDescr

getInfoCont ::  IDEM (Maybe (Descr))
getInfoCont = do
    mbPane <- getPane
    case mbPane of
        Nothing ->  return Nothing
        Just p  ->  liftIO $ readIORef (currentDescr p) >>= return . Just


-- * GUI History

recordInfoHistory :: Descr -> Descr -> IDEAction
recordInfoHistory  descr oldDescr = do
    ideR        <- ask
    triggerEvent ideR (RecordHistory
        ((InfoElementSelected descr),
         (InfoElementSelected oldDescr)))
    return ()

replayInfoHistory :: Descr -> IDEAction
replayInfoHistory descr = do
    setInfo descr


