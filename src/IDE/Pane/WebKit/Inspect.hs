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
import IDE.Utils.GUIUtils
import Data.Typeable (Typeable)
import IDE.Core.Types (IDEAction, IDEM, IDE(..))
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Frame.ViewFrame (getNotebook)
import IDE.Core.State
       (modifyIDE_, postSyncIDE, reifyIDE, leksahOrPackageDir)
import IDE.Core.State (reflectIDE)
import Graphics.UI.Editor.Basics (Connection(..))
import Text.Show.Pretty
       (HtmlOpts(..), defaultHtmlOpts, valToHtmlPage, parseValue, getDataDir)
import System.FilePath ((</>))
import Data.IORef (writeIORef, newIORef, readIORef, IORef)
import Control.Applicative ((<$>))
import System.Log.Logger (debugM)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew, ScrolledWindow(..))
#ifdef MIN_VERSION_gi_webkit2
import GI.WebKit2.Objects.WebView
       (setWebViewSettings, webViewGetSettings, webViewNew, WebView(..))
import GI.WebKit2.Objects.Settings
       (settingsSetMonospaceFontFamily)
#else
import GI.WebKit.Objects.WebView
       (setWebViewSettings, getWebViewSettings, webViewNew, WebView(..))
import GI.WebKit.Objects.WebSettings
       (setWebSettingsMonospaceFontFamily)
#endif
import GI.Gtk.Objects.Widget (afterWidgetFocusInEvent, toWidget)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Enums (PolicyType(..), ShadowType(..))
import GI.Gtk.Objects.Container (containerAdd)

data IDEInspect = IDEInspect {
    scrollWin     :: ScrolledWindow
  , inspectView   :: WebView
} deriving Typeable

data InspectState = InspectState {
} deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEInspect IDEM
    where
    primPaneName _  =   "Inspect"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . scrollWin
    paneId b        =   "*Inspect"

instance RecoverablePane IDEInspect InspectState IDEM where
    saveState p = return (Just InspectState{})
    recoverState pp InspectState {} = do
        nb <- getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = reifyIDE $ \ ideR -> do
        scrollWin <- scrolledWindowNew noAdjustment noAdjustment
        scrolledWindowSetShadowType scrollWin ShadowTypeIn

        inspectView <- webViewNew
#ifdef MIN_VERSION_gi_webkit2
        settings <- webViewGetSettings inspectView
        settingsSetMonospaceFontFamily settings "Consolas"
#else
        settings <- getWebViewSettings inspectView
        setWebSettingsMonospaceFontFamily settings "Consolas"
#endif
        setWebViewSettings inspectView settings
        alwaysHtmlRef <- newIORef False
        containerAdd scrollWin inspectView

        scrolledWindowSetPolicy scrollWin PolicyTypeAutomatic PolicyTypeAutomatic
        let inspect = IDEInspect {..}

        cid1 <- ConnectC inspectView <$> afterWidgetFocusInEvent inspectView ( \e -> do
            liftIO $ reflectIDE (makeActive inspect) ideR
            return True)

        return (Just inspect, [cid1])


getInspectPane :: Maybe PanePath -> IDEM IDEInspect
getInspectPane Nothing    = forceGetPane (Right "*Inspect")
getInspectPane (Just pp)  = forceGetPane (Left pp)

