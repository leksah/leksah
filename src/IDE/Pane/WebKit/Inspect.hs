{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.WebKit.Inspect
-- Copyright   :  2007-2014 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Pane.WebKit.Inspect (
    IDEInspect(..)
  , InspectState(..)
  , getInspectPane
) where

import Graphics.UI.Frame.Panes
       (RecoverablePane(..), PanePath, RecoverablePane, Pane(..))
import Graphics.UI.Gtk
       (scrolledWindowSetShadowType, entryGetText, entryActivated,
        boxPackStart, entrySetText, Entry, VBox, entryNew, vBoxNew,
        postGUISync, scrolledWindowSetPolicy, scrolledWindowNew,
        castToWidget, ScrolledWindow)
import IDE.Utils.GUIUtils
import Data.Typeable (Typeable)
import IDE.Core.Types (IDEAction, IDEM, IDE(..))
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Frame.ViewFrame (getNotebook)
import IDE.Core.State
       (modifyIDE_, postSyncIDE, reifyIDE, leksahOrPackageDir)
import Graphics.UI.Gtk.General.Enums
       (ShadowType(..), Packing(..), PolicyType(..))

#ifdef WEBKITGTK
import Graphics.UI.Gtk
       (toggleActionActive, castToMenuItem, actionCreateMenuItem,
        toggleActionNew, menuShellAppend, toggleActionSetActive,
        menuItemActivate, menuItemNewWithLabel, eventModifier,
        eventKeyName, keyPressEvent, focusInEvent, containerAdd,
        Modifier(..), after)
import Graphics.UI.Gtk.WebKit.Types (WebView(..))
import Graphics.UI.Gtk.WebKit.WebView
       (populatePopup, webViewGoBack, webViewZoomOut, webViewZoomIn,
        webViewLoadString, webViewZoomLevel, webViewReload, webViewNew,
        webViewLoadUri)
import System.Glib.Attributes (AttrOp(..), set, get)
import System.Glib.Signals (on)
import IDE.Core.State (reflectIDE)
import Graphics.UI.Editor.Basics (Connection(..))
import Text.Show.Pretty
       (HtmlOpts(..), defaultHtmlOpts, valToHtmlPage, parseValue, getDataDir)
import System.FilePath ((</>))
#endif
import Data.IORef (writeIORef, newIORef, readIORef, IORef)
import Control.Applicative ((<$>))
import System.Log.Logger (debugM)
import Graphics.UI.Gtk.WebKit.WebView
       (webViewSetWebSettings, webViewGetWebSettings, loadCommitted,
        webViewGetUri)
import Graphics.UI.Gtk.WebKit.WebFrame (webFrameGetUri)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Graphics.UI.Gtk.WebKit.WebSettings
       (webSettingsMonospaceFontFamily)

data IDEInspect = IDEInspect {
    scrollWin     :: ScrolledWindow
#ifdef WEBKITGTK
  , inspectView   :: WebView
#else
  , inspectState   :: IORef InspectState
#endif
} deriving Typeable

data InspectState = InspectState {
} deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEInspect IDEM
    where
    primPaneName _  =   "Inspect"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrollWin
    paneId b        =   "*Inspect"

instance RecoverablePane IDEInspect InspectState IDEM where
    saveState p     =   liftIO $ do
#ifdef WEBKITGTK
        return (Just InspectState{..})
#else
        Just <$> readIORef (inspectState p)
#endif
    recoverState pp InspectState {..} = do
        nb      <-  getNotebook pp
        mbPane <- buildPane pp nb builder
        case mbPane of
            Nothing -> return ()
            Just p  -> liftIO $ do
#ifdef WEBKITGTK
                return ()
#else
                writeIORef (inspectState p) InspectState {..}
#endif
        return mbPane
    builder pp nb windows = reifyIDE $ \ ideR -> do
        scrollWin <- scrolledWindowNew Nothing Nothing
        scrolledWindowSetShadowType scrollWin ShadowIn

#ifdef WEBKITGTK
        inspectView <- webViewNew
        settings <- webViewGetWebSettings inspectView
        settings `set` [webSettingsMonospaceFontFamily := ("Consolas" :: Text)]
        webViewSetWebSettings inspectView settings
        alwaysHtmlRef <- newIORef False
        containerAdd scrollWin inspectView
#else
        inspectState <- newIORef InspectState {}
#endif

        scrolledWindowSetPolicy scrollWin PolicyAutomatic PolicyAutomatic
        let inspect = IDEInspect {..}

#ifdef WEBKITGTK
        cid1 <- after inspectView focusInEvent $ do
            liftIO $ reflectIDE (makeActive inspect) ideR
            return True

        return (Just inspect, [ConnectC cid1])
#else
        return (Just inspect, [])
#endif


getInspectPane :: Maybe PanePath -> IDEM IDEInspect
getInspectPane Nothing    = forceGetPane (Right "*Inspect")
getInspectPane (Just pp)  = forceGetPane (Left pp)

