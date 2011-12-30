{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses,
             ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
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
,   replayInfoHistory
,   openDocu
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import Data.Typeable
import Data.Char (isAlphaNum)
import Network.URI (escapeURIString)

import IDE.Core.State
import IDE.SymbolNavigation
import IDE.Pane.SourceBuffer
import IDE.TextEditor (EditorIter(..))
import IDE.Utils.GUIUtils (openBrowser,controlIsPressed)
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
#if MIN_VERSION_gtksourceview2(0,12,0)
            sourceBufferSetLanguage descriptionBuffer mbLang
#else
            case mbLang of
                Nothing -> return ()
                Just lang -> do sourceBufferSetLanguage descriptionBuffer lang
#endif
            -- This call is here because in the past I have had problems where the
            -- language object became invalid if the manager was garbage collected
            sourceLanguageManagerGetLanguageIds lm

            sourceBufferSetHighlightSyntax descriptionBuffer True
            widgetModifyFont descriptionView (Just font)

            case sourceStyle prefs of
                (False,_)  -> return ()
                (True,str) -> do
                    styleManager <- sourceStyleSchemeManagerNew
                    ids <- sourceStyleSchemeManagerGetSchemeIds styleManager
                    when (elem str ids) $ do
                        scheme <- sourceStyleSchemeManagerGetScheme styleManager str
#if MIN_VERSION_gtksourceview2(0,12,0)
                        sourceBufferSetStyleScheme descriptionBuffer $ Just scheme
#else
                        sourceBufferSetStyleScheme descriptionBuffer scheme
#endif


            sw <- scrolledWindowNew Nothing Nothing
            containerAdd sw descriptionView


            createHyperLinkSupport descriptionView sw (\_ _ iter -> do
                    (GtkEditorIter beg,GtkEditorIter en) <- reflectIDE (getIdentifierUnderCursorFromIter (GtkEditorIter iter, GtkEditorIter iter)) ideR
                    return (beg, en)) (\_ _ slice -> do
                                        when (slice /= []) $ do
                                            -- liftIO$ print ("slice",slice)
                                            void $ reflectIDE (triggerEventIDE (SelectInfo slice)) ideR
                                        )

            scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic

            boxPackStart ibox sw PackGrow 10


            --openType
            currentDescr' <- newIORef idDescr
#if MIN_VERSION_gtk(0,10,5)
            cid         <- on descriptionView populatePopup (populatePopupMenu ideR currentDescr')
#else
            cid         <- descriptionView `onPopulatePopup` (populatePopupMenu ideR currentDescr')
#endif
            let info = IDEInfo ibox currentDescr' descriptionView
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
            return (Just info,[ConnectC cid])

gotoSource :: IDEAction
gotoSource = do
    mbInfo <- getInfoCont
    case mbInfo of
        Nothing     ->  do  ideMessage Normal "gotoSource:noDefinition"
                            return ()
        Just info   ->  goToDefinition info >> return ()

gotoModule' :: IDEAction
gotoModule' = do
    mbInfo  <-  getInfoCont
    case mbInfo of
        Nothing     ->  return ()
        Just info   ->  triggerEventIDE (SelectIdent info) >> return ()


setInfo :: Descr -> IDEAction
setInfo identifierDescr = do
    info <-  forceGetPane (Right "*Info")
    oldDescr <- liftIO $ readIORef (currentDescr info)
    liftIO $ do
        writeIORef (currentDescr info) (Just identifierDescr)
        tb <- get (descriptionView info) textViewBuffer
        textBufferSetText tb (show (Present identifierDescr) ++ "\n")   -- EOL for text iters to work
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

openDocu :: IDEAction
openDocu = do
    mbDescr <- getInfoCont
    case mbDescr of
        Nothing -> return ()
        Just descr -> do
            prefs' <- readIDE prefs
            openBrowser $ docuSearchURL prefs' ++ (escapeURIString isAlphaNum $ dscName descr)

populatePopupMenu :: IDERef -> IORef (Maybe Descr) -> Menu -> IO ()
populatePopupMenu ideR currentDescr' menu = do
    items <- containerGetChildren menu
    item0 <- menuItemNewWithLabel "Goto Definition"
    item0 `onActivateLeaf` (reflectIDE gotoSource ideR)
    item1 <- menuItemNewWithLabel "Select Module"
    item1 `onActivateLeaf` (reflectIDE gotoModule' ideR )
    item2 <- menuItemNewWithLabel "Open Documentation"
    item2 `onActivateLeaf` (reflectIDE openDocu ideR )
    menuShellAppend menu item0
    menuShellAppend menu item1
    menuShellAppend menu item2
    widgetShowAll menu
    mapM_ widgetHide $ take 2 (reverse items)
    return ()


