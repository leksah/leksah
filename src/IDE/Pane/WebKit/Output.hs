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
  , setOutput
  , loadOutputUri
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
import IDE.Pane.WebKit.Inspect (getInspectPane, IDEInspect(..))
import Data.IORef (writeIORef, newIORef, readIORef, IORef)
import Control.Applicative ((<$>))
import System.Log.Logger (debugM)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import GI.Gtk.Objects.VBox (vBoxNew, VBox(..))
import GI.Gtk.Objects.Entry
       (entryGetText, onEntryActivate, entrySetText, entryNew, Entry(..))
import GI.WebKit.Objects.WebView
       (webViewReload, webViewGetUri, webViewLoadString,
        webViewGetInspector, setWebViewSettings, getWebViewSettings,
        onWebViewLoadCommitted, webViewLoadUri, onWebViewPopulatePopup,
        webViewGoBack, webViewZoomOut, webViewZoomIn, webViewNew,
        setWebViewZoomLevel, getWebViewZoomLevel, WebView(..))
import GI.Gtk.Objects.Widget
       (onWidgetKeyPressEvent, afterWidgetFocusInEvent, toWidget)
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Enums (PolicyType(..), ShadowType(..))
import Graphics.UI.Editor.Parameters (Packing(..), boxPackStart')
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gdk (eventKeyReadState, eventKeyReadKeyval, keyvalName)
import GI.Gdk.Flags (ModifierType(..))
import GI.Gtk.Objects.ToggleAction
       (setToggleActionActive, toggleActionNew)
import GI.Gtk.Objects.Action (actionCreateMenuItem)
import GI.Gtk.Objects.MenuItem
       (MenuItem(..), onMenuItemActivate, toMenuItem)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import GI.WebKit.Objects.WebFrame (webFrameGetUri)
import GI.WebKit.Objects.WebSettings
       (setWebSettingsEnableDeveloperExtras)
import GI.WebKit.Objects.WebInspector
       (onWebInspectorInspectWebView)
import Data.GI.Base.ManagedPtr (unsafeCastTo)

data IDEOutput = IDEOutput {
    vbox          :: VBox
  , uriEntry      :: Entry
  , webView       :: WebView
  , alwaysHtmlRef :: IORef Bool
  , outState      :: IORef OutputState
} deriving Typeable

data OutputState = OutputState {
    zoom :: Float
  , alwaysHtml :: Bool
} deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEOutput IDEM
    where
    primPaneName _  =   "Out"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . vbox
    paneId b        =   "*Out"

instance RecoverablePane IDEOutput OutputState IDEM where
    saveState p = do
        zoom <- getWebViewZoomLevel $ webView p
        alwaysHtml <- liftIO . readIORef $ alwaysHtmlRef p
        return (Just OutputState{..})
    recoverState pp OutputState {..} = do
        nb     <- getNotebook pp
        mbPane <- buildPane pp nb builder
        case mbPane of
            Nothing -> return ()
            Just p  -> do
                setWebViewZoomLevel (webView p) zoom
                liftIO $ writeIORef (alwaysHtmlRef p) alwaysHtml
        return mbPane
    builder pp nb windows = reifyIDE $ \ ideR -> do
        vbox <- vBoxNew False 0
        uriEntry <- entryNew
        entrySetText uriEntry "http://"
        scrolledView <- scrolledWindowNew noAdjustment noAdjustment
        scrolledWindowSetShadowType scrolledView ShadowTypeIn
        boxPackStart' vbox uriEntry PackNatural 0
        boxPackStart' vbox scrolledView PackGrow 0

        webView <- webViewNew
        alwaysHtmlRef <- newIORef False
        containerAdd scrolledView webView

        scrolledWindowSetPolicy scrolledView PolicyTypeAutomatic PolicyTypeAutomatic
        let out = IDEOutput {..}

        cid1 <- ConnectC webView <$> afterWidgetFocusInEvent webView (\e -> do
            liftIO $ reflectIDE (makeActive out) ideR
            return True)

--        webView `set` [webViewZoomLevel := 2.0]
        cid2 <- ConnectC webView <$> onWidgetKeyPressEvent webView (\e -> do
            key <- eventKeyReadKeyval e >>= keyvalName
            mod <- eventKeyReadState e
            case (key, mod) of
                ("plus", [ModifierTypeShiftMask,ModifierTypeControlMask]) -> webViewZoomIn  webView >> return True
                ("minus",[ModifierTypeControlMask]) -> webViewZoomOut webView >> return True
                ("BackSpace", [ModifierTypeShiftMask]) -> webViewGoBack  webView >> return True
                _                         -> return False)

        cid3 <- ConnectC webView <$> onWebViewPopulatePopup webView (\ menu -> do
            alwaysHtml <- readIORef alwaysHtmlRef
            action <- toggleActionNew "AlwaysHTML" (Just $ __"Always HTML") Nothing Nothing
            item <- actionCreateMenuItem action >>= unsafeCastTo MenuItem
            onMenuItemActivate item $ writeIORef alwaysHtmlRef $ not alwaysHtml
            setToggleActionActive action alwaysHtml
            menuShellAppend menu item
            return ())

        cid4 <- ConnectC uriEntry <$> onEntryActivate uriEntry (do
            uri <- entryGetText uriEntry
            webViewLoadUri webView uri
            (`reflectIDE` ideR) $ modifyIDE_ (\ide -> ide {autoURI = Just uri}))

        cid5 <- ConnectC webView <$> onWebViewLoadCommitted webView (\ frame -> do
            uri <- webFrameGetUri frame
            valueUri <- getValueUri
            if uri /= valueUri
                then do
                    entrySetText uriEntry uri
                    (`reflectIDE` ideR) $ modifyIDE_ (\ide -> ide {autoURI = Just uri})
                else
                    (`reflectIDE` ideR) $ modifyIDE_ (\ide -> ide {autoURI = Nothing}))

        cid6 <- ConnectC uriEntry <$> afterWidgetFocusInEvent uriEntry (\e -> do
            liftIO $ reflectIDE (makeActive out) ideR
            return True)

        settings <- getWebViewSettings webView
        setWebSettingsEnableDeveloperExtras settings True
        setWebViewSettings webView settings
        inspector <- webViewGetInspector webView
        cid7 <- ConnectC inspector <$> onWebInspectorInspectWebView inspector (\view -> (`reflectIDE` ideR) $ do
            inspectPane <- getInspectPane Nothing
            displayPane inspectPane False
            return $ inspectView inspectPane)

        return (Just out, [cid1, cid2, cid3, cid4, cid5, cid6])


getOutputPane :: Maybe PanePath -> IDEM IDEOutput
getOutputPane Nothing    = forceGetPane (Right "*Out")
getOutputPane (Just pp)  = forceGetPane (Left pp)

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

setOutput :: Text -> Text -> IDEAction
setOutput command str =
     do out <- getOutputPane Nothing
        entrySetText (uriEntry out) (T.pack $ show command)
        uri <- getValueUri
        alwaysHtml <- liftIO . readIORef $ alwaysHtmlRef out
        let view = webView out
            html = case (alwaysHtml, parseValue $ T.unpack str) of
                        (False, Just value) -> T.pack $ valToHtmlPage defaultHtmlOpts value
                        _                   -> str
        webViewLoadString view html "text/html" "UTF-8" uri

loadOutputUri :: FilePath -> IDEAction
loadOutputUri uri =
     do out <- getOutputPane Nothing
        let view = webView out
        entrySetText (uriEntry out) (T.pack uri)
        currentUri <- webViewGetUri view
        if T.pack uri == currentUri
            then webViewReload view
            else webViewLoadUri view (T.pack uri)

