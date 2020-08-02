{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.WebKit.Documentation
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Pane.WebKit.Documentation (
    IDEDocumentation(..)
  , DocumentationState(..)
  , getDocumentation
  , showDocumentationPane
  , loadDoc
  , reloadDoc
) where

import Prelude ()
import Prelude.Compat
import Graphics.UI.Frame.Panes
       (RecoverablePane(..), PanePath, RecoverablePane, Pane(..))
import Data.Typeable (Typeable)
import Data.Text (Text)
import IDE.Core.Types (IDEAction, IDEM)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Frame.ViewFrame (getNotebook)
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew, ScrolledWindow(..))
#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)

import IDE.Core.State (reifyIDE, reflectIDE)
import IDE.Gtk.State ()
import GI.Gtk.Objects.Widget
       (onWidgetKeyPressEvent, afterWidgetFocusInEvent, toWidget)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gdk (getEventKeyState, keyvalName, getEventKeyKeyval)
import GI.Gdk.Flags (ModifierType(..))
import Graphics.UI.Editor.Basics (Connection(..))

#ifdef MIN_VERSION_gi_webkit2
import GI.WebKit2.Objects.WebView
       (webViewReload, webViewGoBack,
        webViewNew, webViewLoadUri, setWebViewZoomLevel, webViewGetUri,
        getWebViewZoomLevel, WebView(..))
#else
import GI.WebKit.Objects.WebView
       (webViewReload, webViewGoBack, webViewZoomOut, webViewZoomIn,
        webViewNew, webViewLoadUri, setWebViewZoomLevel, webViewGetUri,
        getWebViewZoomLevel, WebView(..))
#endif

#else
-- Imports when webkit is not included
import IDE.Core.State (reifyIDE)
import IDE.Gtk.State ()
import GI.Gtk.Objects.Widget
       (toWidget)
#endif

import GI.Gtk.Objects.Adjustment (Adjustment)
import GI.Gtk.Enums (PolicyType(..), ShadowType(..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
data IDEDocumentation = IDEDocumentation {
    scrolledView :: ScrolledWindow
  , webView      :: WebView
#else
newtype IDEDocumentation = IDEDocumentation {
    scrolledView :: ScrolledWindow
#endif
} deriving Typeable

data DocumentationState = DocumentationState {
    zoom :: Double
  , uri  :: Maybe Text
} deriving(Eq,Ord,Read,Show,Typeable,Generic)

instance ToJSON DocumentationState
instance FromJSON DocumentationState

instance Pane IDEDocumentation IDEM
    where
    primPaneName _  =   "Doc"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . scrolledView
    paneId _        =   "*Doc"

instance RecoverablePane IDEDocumentation DocumentationState IDEM where
    saveState _p = do
#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
        zoom <- fmap realToFrac <$> getWebViewZoomLevel $ webView _p
        uri  <- webViewGetUri $ webView _p
#else
        let zoom = 1.0
            uri = Nothing
#endif
        return (Just DocumentationState{..})
    recoverState pp DocumentationState {..} = do
        nb     <-  getNotebook pp
        buildPane pp nb builder
#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
          >>= (\mbPane -> do
            case mbPane of
                Nothing -> return ()
                Just p  -> do
                    setWebViewZoomLevel (webView p) (realToFrac zoom)
                    maybe (return ()) (webViewLoadUri (webView p)) uri
            return mbPane)
#endif
    builder _pp _nb _windows = reifyIDE $ \ _ideR -> do
        scrolledView <- scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
        scrolledWindowSetShadowType scrolledView ShadowTypeIn

#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
        webView <- webViewNew
        containerAdd scrolledView webView
#endif

        scrolledWindowSetPolicy scrolledView PolicyTypeAutomatic PolicyTypeAutomatic
        let docs = IDEDocumentation {..}

#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
        cid1 <- ConnectC webView <$> afterWidgetFocusInEvent webView (\e -> do
            liftIO $ reflectIDE (makeActive docs) _ideR
            return True)

        cid2 <- ConnectC webView <$> onWidgetKeyPressEvent webView (\e -> do
            key <- getEventKeyKeyval e >>= keyvalName
            mod <- getEventKeyState e
            case (key, mod) of
                (Just "plus", [ModifierTypeShiftMask,ModifierTypeControlMask]) -> do
                    zoom <- getWebViewZoomLevel webView
                    setWebViewZoomLevel webView (zoom * 1.25)
                    return True
                (Just "minus",[ModifierTypeControlMask]) -> do
                    zoom <- getWebViewZoomLevel webView
                    setWebViewZoomLevel webView (zoom * 0.8)
                    return True
                (Just "BackSpace", []) -> webViewGoBack webView >> return True
                _                      -> return False)
        return (Just docs, [cid1, cid2])
#else
        return (Just docs, [])
#endif

getDocumentation :: Maybe PanePath -> IDEM IDEDocumentation
getDocumentation Nothing    = forceGetPane (Right "*Doc")
getDocumentation (Just pp)  = forceGetPane (Left pp)

showDocumentationPane :: IDEAction
showDocumentationPane =
   getDocumentation Nothing >>= \ p -> displayPane p False

loadDoc :: Text -> IDEAction
loadDoc _uri =
#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
     do doc <- getDocumentation Nothing
        let view = webView doc
        webViewLoadUri view _uri
#endif
        return ()

reloadDoc :: IDEAction
reloadDoc =
#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
     do doc <- getDocumentation Nothing
        let view = webView doc
        webViewReload view
#endif
        return ()
