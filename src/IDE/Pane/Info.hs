{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses,
             CPP, ScopedTypeVariables, TypeSynonymInstances, GADTs #-}
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

import Control.Monad
import Data.IORef
import Data.Typeable
import Data.Char (isAlphaNum)
import Network.URI (escapeURIString)

import IDE.Core.State
import IDE.SymbolNavigation
import IDE.Pane.SourceBuffer
import IDE.TextEditor (newDefaultBuffer, TextEditor(..), EditorView(..))
import IDE.Utils.GUIUtils (openBrowser, __)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Graphics.UI.Gtk
       (widgetHide, widgetShowAll, menuShellAppend, menuItemActivate,
        menuItemNewWithLabel, containerGetChildren, Menu,
        boxPackStart, scrolledWindowSetPolicy,
        vBoxNew, castToWidget, VBox)
import Graphics.UI.Gtk.General.Enums (Packing(..), PolicyType(..))
import System.Glib.Signals (on)

-- | An info pane description
--
data IDEInfo        =   forall editor. TextEditor editor => IDEInfo {
    sw              ::   VBox
,   currentDescr    ::   IORef (Maybe Descr)
,   descriptionView ::   EditorView editor
} deriving Typeable

data InfoState              =   InfoState (Maybe Descr)
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEInfo IDEM
    where
    primPaneName _  =   (__ "Info")
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
        ibox              <- liftIO $ vBoxNew False 0
        descriptionBuffer <- newDefaultBuffer Nothing ""
        descriptionView   <- newView descriptionBuffer (textviewFont prefs)

        setStyle descriptionBuffer $ case sourceStyle prefs of
                                        (False,_) -> Nothing
                                        (True,v) -> Just v

        sw <- getScrolledWindow descriptionView

        createHyperLinkSupport descriptionView sw (\_ _ iter -> do
                (beg, en) <- getIdentifierUnderCursorFromIter (iter, iter)
                return (beg, en)) (\_ shift' slice -> do
                                    when (slice /= []) $ do
                                        -- liftIO$ print ("slice",slice)
                                        triggerEventIDE (SelectInfo slice shift')
                                        return ()
                                    )

        liftIO $ scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic

        liftIO $ boxPackStart ibox sw PackGrow 10


        --openType
        currentDescr' <- liftIO $ newIORef idDescr
        cids <- onPopulatePopup descriptionView $ \ menu -> do
            ideR <- ask
            liftIO $ populatePopupMenu ideR currentDescr' menu
        let info = IDEInfo ibox currentDescr' descriptionView
        -- ids5 <- sv `onLookupInfo` selectInfo descriptionView       -- obsolete by hyperlinks
        return (Just info, cids)

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
    setInfo' info
  where
    setInfo' (info@IDEInfo{descriptionView = v}) = do
        oldDescr <- liftIO $ readIORef (currentDescr info)
        liftIO $ writeIORef (currentDescr info) (Just identifierDescr)
        tb <- getBuffer v
        setText tb (show (Present identifierDescr) ++ "\n")   -- EOL for text iters to work
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
    item0 <- menuItemNewWithLabel (__ "Goto Definition")
    item0 `on` menuItemActivate $ reflectIDE gotoSource ideR
    item1 <- menuItemNewWithLabel (__ "Select Module")
    item1 `on` menuItemActivate $ reflectIDE gotoModule' ideR
    item2 <- menuItemNewWithLabel (__ "Open Documentation")
    item2 `on` menuItemActivate $ reflectIDE openDocu ideR
    menuShellAppend menu item0
    menuShellAppend menu item1
    menuShellAppend menu item2
    widgetShowAll menu
    mapM_ widgetHide $ take 2 (reverse items)
    return ()


