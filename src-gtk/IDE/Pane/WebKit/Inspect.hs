{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Prelude ()
import Prelude.Compat
import Graphics.UI.Frame.Panes
       (RecoverablePane(..), PanePath, RecoverablePane, Pane(..))
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Frame.ViewFrame (getNotebook)
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew, ScrolledWindow(..))

#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)

import IDE.Utils.GUIUtils
import IDE.Core.State
       (IDEAction, IDEM, IDE(..), reifyIDE, reflectIDE)
import IDE.Gtk.State ()
import Graphics.UI.Editor.Basics (Connection(..))
import Text.Show.Pretty
       (HtmlOpts(..), defaultHtmlOpts, valToHtmlPage, parseValue, getDataDir)
import Data.IORef (writeIORef, readIORef)
import Control.Applicative ((<$>))
import System.Log.Logger (debugM)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import GI.Gtk.Objects.Widget (afterWidgetFocusInEvent, toWidget)
import GI.Gtk.Objects.Container (containerAdd)

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

#else

import IDE.Core.State (IDEM, reifyIDE)
import IDE.Gtk.State ()
import GI.Gtk.Objects.Widget (toWidget)

#endif

import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Enums (PolicyType(..), ShadowType(..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
data IDEInspect = IDEInspect {
    scrollWin     :: ScrolledWindow
  , inspectView   :: WebView
#else
newtype IDEInspect = IDEInspect {
    scrollWin     :: ScrolledWindow
#endif
} deriving Typeable

data InspectState = InspectState {
} deriving(Eq,Ord,Read,Show,Typeable,Generic)

instance ToJSON InspectState
instance FromJSON InspectState

instance Pane IDEInspect IDEM
    where
    primPaneName _  =   "Inspect"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . scrollWin
    paneId _        =   "*Inspect"

instance RecoverablePane IDEInspect InspectState IDEM where
    saveState _ = return (Just InspectState{})
    recoverState pp InspectState {} = do
        nb <- getNotebook pp
        buildPane pp nb builder
    builder _pp _nb _windows = reifyIDE $ \ _ideR -> do
        scrollWin <- scrolledWindowNew noAdjustment noAdjustment
        scrolledWindowSetShadowType scrollWin ShadowTypeIn

#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
        inspectView <- webViewNew
#ifdef MIN_VERSION_gi_webkit2
        settings <- webViewGetSettings inspectView
        settingsSetMonospaceFontFamily settings "Consolas"
#else
        settings <- getWebViewSettings inspectView
        setWebSettingsMonospaceFontFamily settings "Consolas"
#endif
        setWebViewSettings inspectView settings
        containerAdd scrollWin inspectView
#endif

        scrolledWindowSetPolicy scrollWin PolicyTypeAutomatic PolicyTypeAutomatic
        let inspect = IDEInspect {..}

#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
        cid1 <- ConnectC inspectView <$> afterWidgetFocusInEvent inspectView ( \e -> do
            liftIO $ reflectIDE (makeActive inspect) _ideR
            return True)

        return (Just inspect, [cid1])
#else
        return (Just inspect, [])
#endif

getInspectPane :: Maybe PanePath -> IDEM IDEInspect
getInspectPane Nothing    = forceGetPane (Right "*Inspect")
getInspectPane (Just pp)  = forceGetPane (Left pp)

