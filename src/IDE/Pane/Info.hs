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
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Data.IORef
import Data.Typeable

import IDE.Core.State
import IDE.Pane.SourceBuffer
import IDE.Pane.References
import IDE.FileUtils (openBrowser)
import IDE.Metainfo.Provider (getIdentifierDescr)
import Graphics.UI.Gtk.SourceView


-- | An info pane description
--
data IDEInfo        =   IDEInfo {
    sw              ::   VBox
,   currentDescr    ::   IORef (Maybe Descr)
,   descriptionView ::   SourceView
} deriving Typeable

data InfoState              =   InfoState (Maybe Descr)
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEInfo IDEM
    where
    primPaneName _  =   "Info"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . sw
    paneId b        =   "*Info"

instance RecoverablePane IDEInfo InfoState IDEM where
    saveState p     =   do
        currentDescr' <-  liftIO $ readIORef (currentDescr p)
        return (Just (InfoState currentDescr'))
    recoverState pp (InfoState descr) =   do
        nb <- getNotebook pp
        buildPane pp nb builder
    builder pp nb windows =
        let idDescr = Nothing in do
        prefs <- readIDE prefs
        reifyIDE $ \ ideR -> do
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
                mbDescr <- readIORef currentDescr'
                case mbDescr of
                    Nothing -> return ()
                    Just descr -> reflectIDE (do
                                    triggerEventIDE (SearchMeta (descrName descr))
                                    i :: IDEInfo <- forceGetPane (Right "*Info")
                                    displayPane i False
                                    return ()) ideR )
            docuB `onClicked` (do
                mbDescr <- readIORef currentDescr'
                case mbDescr of
                    Nothing -> return ()
                    Just descr -> reflectIDE (openBrowser $ docuSearchURL prefs ++ descrName descr) ideR)
            descriptionView `widgetAddEvents` [ButtonReleaseMask]
            id5 <- descriptionView `onButtonRelease`
                (\ e -> do
                    buf     <-  textViewGetBuffer descriptionView
                    (l,r)   <- textBufferGetSelectionBounds buf
                    symbol  <- textBufferGetText buf l r True
                    when (controlIsPressed e)
                        (reflectIDE (do
                            triggerEventIDE (SelectInfo symbol)
                            return ()) ideR)
                    return False)
            return (Just info,[])

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
    case mbInfo of
        Nothing     ->  return ()
        Just info   ->  triggerEventIDE (SelectIdent info) >> return ()

referencedFrom' :: IDEAction
referencedFrom' = do
    mbInfo <- getInfoCont
    case mbInfo of
        Nothing     ->  return ()
        Just info   ->  referencedFrom info  >> return ()

setSymbol :: String -> IDEAction
setSymbol symbol = do
    currentInfo' <- readIDE currentInfo
    case currentInfo' of
        Nothing -> return ()
        Just ((_,symbolTable1),(_,symbolTable2)) ->
            case getIdentifierDescr symbol symbolTable1 symbolTable2 of
                []     -> return ()
                (a:r)  ->  do
                    setInfo a
                    p :: IDEInfo <- forceGetPane (Right "*Info")
                    displayPane p False
                    if length (a:r) > 1
                        then triggerEventIDE (DescrChoice (a:r)) >> return ()
                        else triggerEventIDE (SelectIdent a) >> return ()


setInfo :: Descr -> IDEAction
setInfo identifierDescr = do
    info <-  forceGetPane (Right "*Info")
    oldDescr <- liftIO $ readIORef (currentDescr info)
    liftIO $ do
        writeIORef (currentDescr info) (Just identifierDescr)
        tb <- get (descriptionView info) textViewBuffer
        textBufferSetText tb (show (Present identifierDescr))
    recordInfoHistory (Just identifierDescr) oldDescr

getInfoCont ::  IDEM (Maybe (Descr))
getInfoCont = do
    mbPane <- getPane
    case mbPane of
        Nothing ->  return Nothing
        Just p  ->  liftIO $ readIORef (currentDescr p)


-- * GUI History

recordInfoHistory :: Maybe Descr -> Maybe Descr -> IDEAction
recordInfoHistory  descr oldDescr = do
    triggerEventIDE (RecordHistory
        ((InfoElementSelected descr),
         (InfoElementSelected oldDescr)))
    return ()

replayInfoHistory :: Maybe Descr -> IDEAction
replayInfoHistory mbDescr = do
    case mbDescr of
        Nothing    -> return ()
        Just descr -> setInfo descr


