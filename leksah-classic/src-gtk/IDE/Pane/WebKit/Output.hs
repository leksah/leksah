{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.WebKit.Output
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

module IDE.Pane.WebKit.Output (
    IDEOutput(..)
  , OutputState(..)
  , getOutputPane
  , showOutputPane
  , setOutput
  , loadOutputUri
  , loadOutputHtmlFile
) where

import Prelude ()
import Prelude.Compat
import Graphics.UI.Frame.Panes
       (RecoverablePane(..), PanePath, RecoverablePane, Pane(..))
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Frame.ViewFrame (getNotebook)
import Data.IORef (writeIORef, newIORef, readIORef, IORef)
import Data.Text (Text)
import GI.Gtk.Objects.Box (boxNew, Box(..))

#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)

import IDE.Utils.GUIUtils
import IDE.Core.Types (IDEAction, IDEM, IDE(..))
import IDE.Core.State
       (reflectIDE, modifyIDE_, reifyIDE, leksahOrPackageDir, ideJSM_)
import IDE.Gtk.State ()
import Graphics.UI.Editor.Basics (Connection(..))
import Text.Show.Pretty
       (HtmlOpts(..), defaultHtmlOpts, valToHtmlPage, parseValue, getDataDir)
import IDE.Pane.WebKit.Inspect (getInspectPane, IDEInspect(..))
import System.Log.Logger (debugM)
import qualified Data.Text as T (unpack, pack)
import GI.Gtk.Objects.Entry
       (entryGetText, onEntryActivate, entrySetText, entryNew, Entry(..))
import GI.Gtk.Objects.Widget
       (onWidgetKeyPressEvent, afterWidgetFocusInEvent, toWidget)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gdk (getEventKeyState, getEventKeyKeyval, keyvalName)
import GI.Gdk.Flags (ModifierType(..))
import GI.Gtk.Objects.ToggleAction
       (setToggleActionActive, toggleActionNew)
import GI.Gtk.Objects.Action (actionCreateMenuItem)
import GI.Gtk.Objects.MenuItem
       (MenuItem(..), onMenuItemActivate, toMenuItem)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import qualified Data.Text.IO as T (readFile)

#ifdef MIN_VERSION_gi_webkit2
import GI.WebKit2.Objects.WebView
       (webViewReload, webViewGetUri, webViewLoadHtml,
        webViewGetInspector, setWebViewSettings, webViewGetSettings,
        onWebViewLoadChanged, webViewLoadUri, onWebViewContextMenu,
        webViewGoBack, webViewNew,
        setWebViewZoomLevel, getWebViewZoomLevel, WebView(..))
import GI.WebKit2.Objects.Settings
       (settingsSetEnableDeveloperExtras, settingsSetAllowFileAccessFromFileUrls)
#else
import GI.WebKit.Objects.WebView
       (webViewReload, webViewGetUri, webViewLoadString,
        webViewGetInspector, setWebViewSettings, getWebViewSettings,
        onWebViewLoadCommitted, webViewLoadUri, onWebViewPopulatePopup,
        webViewGoBack, webViewZoomOut, webViewZoomIn, webViewNew,
        setWebViewZoomLevel, getWebViewZoomLevel, WebView(..))
import GI.WebKit.Objects.WebFrame (webFrameGetUri)
import GI.WebKit.Objects.WebSettings
       (setWebSettingsEnableDeveloperExtras)
import GI.WebKit.Objects.WebInspector
       (onWebInspectorInspectWebView)
#endif

#else

import IDE.Core.State
       (IDEAction, IDEM, reifyIDE, ideJSM_)
import IDE.Gtk.State ()
import qualified Data.Text as T (pack)
import GI.Gtk.Objects.Entry
       (entrySetText, entryNew, Entry(..))
import GI.Gtk.Objects.Widget (toWidget)

#endif

import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (Adjustment)
import GI.Gtk.Enums (PolicyType(..), ShadowType(..), Orientation(..))
import Graphics.UI.Editor.Parameters (Packing(..), boxPackStart')
import Data.Aeson (FromJSON, ToJSON, FromJSON)
import GHC.Generics (Generic)
import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Document (getBodyUnchecked)
import GHCJS.DOM.Element (setInnerHTML)

data IDEOutput = IDEOutput {
    vbox          :: Box
  , uriEntry      :: Entry
#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
  , webView       :: WebView
#endif
  , alwaysHtmlRef :: IORef Bool
--  , outState      :: IORef OutputState
} deriving Typeable

data OutputState = OutputState {
    zoom :: Double
  , alwaysHtml :: Bool
} deriving(Eq,Ord,Read,Show,Typeable,Generic)

instance ToJSON OutputState
instance FromJSON OutputState

instance Pane IDEOutput IDEM
    where
    primPaneName _  =   "Out"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . vbox
    paneId _        =   "*Out"

instance RecoverablePane IDEOutput OutputState IDEM where
    saveState p = do
#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
        zoom <- fmap realToFrac <$> getWebViewZoomLevel $ webView p
#else
        let zoom = 1.0
#endif
        alwaysHtml <- liftIO . readIORef $ alwaysHtmlRef p
        return (Just OutputState{..})
    recoverState pp OutputState {..} = do
        nb     <- getNotebook pp
        mbPane <- buildPane pp nb builder
        case mbPane of
            Nothing -> return ()
            Just p  ->
#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
               do setWebViewZoomLevel (webView p) (realToFrac zoom)
#endif
                  liftIO $ writeIORef (alwaysHtmlRef p) alwaysHtml
        return mbPane
    builder _pp _nb _windows = reifyIDE $ \ _ideR -> do
        vbox <- boxNew OrientationVertical 0
        uriEntry <- entryNew
        entrySetText uriEntry "http://"
        scrolledView <- scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
        scrolledWindowSetShadowType scrolledView ShadowTypeIn
        boxPackStart' vbox uriEntry PackNatural 0
        boxPackStart' vbox scrolledView PackGrow 0

#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
        webView <- webViewNew
        containerAdd scrolledView webView
#endif

        alwaysHtmlRef <- newIORef False
        scrolledWindowSetPolicy scrolledView PolicyTypeAutomatic PolicyTypeAutomatic
        let out = IDEOutput {..}

#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
        cid1 <- ConnectC webView <$> afterWidgetFocusInEvent webView (\e -> do
            liftIO $ reflectIDE (makeActive out) _ideR
            return True)

--        webView `set` [webViewZoomLevel := 2.0]
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
                (Just "BackSpace", [ModifierTypeShiftMask]) -> webViewGoBack  webView >> return True
                _                         -> return False)

        -- TODO
#ifndef MIN_VERSION_gi_webkit2
        cid3 <- ConnectC webView <$> onWebViewPopulatePopup webView (\ menu -> do
            alwaysHtml <- readIORef alwaysHtmlRef
            action <- toggleActionNew "AlwaysHTML" (Just $ __"Always HTML") Nothing Nothing
            item <- actionCreateMenuItem action >>= unsafeCastTo MenuItem
            onMenuItemActivate item $ writeIORef alwaysHtmlRef $ not alwaysHtml
            setToggleActionActive action alwaysHtml
            menuShellAppend menu item
            return ())
#endif

        cid4 <- ConnectC uriEntry <$> onEntryActivate uriEntry (do
            uri <- entryGetText uriEntry
            webViewLoadUri webView uri
            (`reflectIDE` _ideR) $ modifyIDE_ (\ide -> ide {_autoURI = Just uri}))

#ifndef MIN_VERSION_gi_webkit2
        cid5 <- ConnectC webView <$> onWebViewLoadCommitted webView (\ frame -> do
            uri <- webFrameGetUri frame
            valueUri <- getValueUri
            if uri /= valueUri
                then do
                    entrySetText uriEntry uri
                    (`reflectIDE` _ideR) $ modifyIDE_ (\ide -> ide {_autoURI = Just uri})
                else
                    (`reflectIDE` _ideR) $ modifyIDE_ (\ide -> ide {_autoURI = Nothing}))
#endif

        cid6 <- ConnectC uriEntry <$> afterWidgetFocusInEvent uriEntry (\e -> do
            liftIO $ reflectIDE (makeActive out) _ideR
            return True)

#ifdef MIN_VERSION_gi_webkit2
        settings <- webViewGetSettings webView
        settingsSetEnableDeveloperExtras settings True
        settingsSetAllowFileAccessFromFileUrls settings True
#else
        settings <- getWebViewSettings webView
        setWebSettingsEnableDeveloperExtras settings True
#endif
        setWebViewSettings webView settings
        inspector <- webViewGetInspector webView

#ifndef MIN_VERSION_gi_webkit2
        cid7 <- ConnectC inspector <$> onWebInspectorInspectWebView inspector (\view -> (`reflectIDE` _ideR) $ do
            inspectPane <- getInspectPane Nothing
            displayPane inspectPane False
            return $ inspectView inspectPane)
#endif

#ifdef MIN_VERSION_gi_webkit2
        return (Just out, [cid1, cid2, cid4, cid6])
#else
        return (Just out, [cid1, cid2, cid3, cid4, cid5, cid6, cid7])
#endif

#else
        return (Just out, [])
#endif


getOutputPane :: Maybe PanePath -> IDEM IDEOutput
getOutputPane Nothing    = forceGetPane (Right "*Out")
getOutputPane (Just pp)  = forceGetPane (Left pp)

showOutputPane :: IDEAction
showOutputPane =
   getOutputPane Nothing >>= \ p -> displayPane p False

#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
getValueUri :: MonadIO m => m Text
getValueUri = do
    dataDir <- liftIO $ map fixSep <$> leksahOrPackageDir "pretty-show" getDataDir
    return . T.pack $ "file://"
        ++ (case dataDir of
                ('/':_) -> dataDir
                _       -> '/':dataDir)
        ++ "/value.html"
  where
    fixSep '\\' = '/'
    fixSep x = x
#endif

setOutput :: Text -> Text -> IDEAction
setOutput command str = do
    out <- getOutputPane Nothing
    entrySetText (uriEntry out) (T.pack $ show command)
    ideJSM_ $ do
        body <- currentDocumentUnchecked >>= getBodyUnchecked
        setInnerHTML body str
#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
    uri <- getValueUri
    alwaysHtml <- liftIO . readIORef $ alwaysHtmlRef out
    let view = webView out
        html = case (alwaysHtml, parseValue $ T.unpack str) of
                    (False, Just value) -> T.pack $ valToHtmlPage defaultHtmlOpts value
                    _                   -> str
#ifdef MIN_VERSION_gi_webkit2
    webViewLoadHtml view html (Just uri)
#else
    webViewLoadString view html "text/html" "UTF-8" uri
#endif
#endif
    return ()

loadOutputUri :: FilePath -> IDEAction
loadOutputUri _uri =
#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
     do out <- getOutputPane Nothing
        let view = webView out
        entrySetText (uriEntry out) (T.pack _uri)
        currentUri <- webViewGetUri view
        if Just (T.pack _uri) == currentUri
            then webViewReload view
            else webViewLoadUri view (T.pack _uri)
#endif
        return ()

loadOutputHtmlFile :: FilePath -> IDEAction
loadOutputHtmlFile _file =
#if defined(MIN_VERSION_gi_webkit2) || defined(MIN_VERSION_gi_webkit2)
 do out <- getOutputPane Nothing
    let view = webView out
    html <- liftIO $ T.readFile _file
    let uri = "file:///" ++ _file
    entrySetText (uriEntry out) (T.pack uri)
    currentUri <- webViewGetUri view
#ifdef MIN_VERSION_gi_webkit2
    webViewLoadHtml view html (Just $ T.pack uri)
#else
    webViewLoadString view html "text/html" "UTF-8" uri
#endif
#endif
    return ()
--    if Just (T.pack uri) == currentUri
--        then webViewReload view
--        else webViewLoadUri view (T.pack uri)

