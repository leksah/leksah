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
  , setOutput
  , loadOutputUri
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
import IDE.Pane.WebKit.Inspect (getInspectPane, IDEInspect(..))
import Graphics.UI.Gtk.WebKit.WebSettings
       (webSettingsEnableDeveloperExtras)
import Graphics.UI.Gtk.WebKit.WebInspector (inspectWebView)
#endif
import Data.IORef (writeIORef, newIORef, readIORef, IORef)
import Control.Applicative ((<$>))
import System.Log.Logger (debugM)
import Graphics.UI.Gtk.WebKit.WebView
       (webViewSetWebSettings, webViewGetWebSettings, webViewGetInspector,
        loadCommitted, webViewGetUri)
import Graphics.UI.Gtk.WebKit.WebFrame (webFrameGetUri)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)

data IDEOutput = IDEOutput {
    vbox          :: VBox
  , uriEntry      :: Entry
#ifdef WEBKITGTK
  , webView       :: WebView
  , alwaysHtmlRef :: IORef Bool
#else
  , outState      :: IORef OutputState
#endif
} deriving Typeable

data OutputState = OutputState {
    zoom :: Float
  , alwaysHtml :: Bool
} deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEOutput IDEM
    where
    primPaneName _  =   "Out"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . vbox
    paneId b        =   "*Out"

instance RecoverablePane IDEOutput OutputState IDEM where
    saveState p     =   liftIO $
#ifdef WEBKITGTK
         do zoom <- webView p `get` webViewZoomLevel
            alwaysHtml <- readIORef $ alwaysHtmlRef p
            return (Just OutputState{..})
#else
            Just <$> readIORef (outState p)
#endif
    recoverState pp OutputState {..} =   do
        nb      <-  getNotebook pp
        mbPane <- buildPane pp nb builder
        case mbPane of
            Nothing -> return ()
            Just p  -> liftIO $
#ifdef WEBKITGTK
                 do webView p `set` [webViewZoomLevel := zoom]
                    writeIORef (alwaysHtmlRef p) alwaysHtml
#else
                   writeIORef (outState p) OutputState {..}
#endif
        return mbPane
    builder pp nb windows = reifyIDE $ \ ideR -> do
        vbox <- vBoxNew False 0
        uriEntry <- entryNew
        entrySetText uriEntry ("http://" :: Text)
        scrolledView <- scrolledWindowNew Nothing Nothing
        scrolledWindowSetShadowType scrolledView ShadowIn
        boxPackStart vbox uriEntry PackNatural 0
        boxPackStart vbox scrolledView PackGrow 0

#ifdef WEBKITGTK
        webView <- webViewNew
        alwaysHtmlRef <- newIORef False
        containerAdd scrolledView webView
#else
        outState <- newIORef OutputState {zoom = 1.0}
#endif

        scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic
        let out = IDEOutput {..}

#ifdef WEBKITGTK
        cid1 <- after webView focusInEvent $ do
            liftIO $ reflectIDE (makeActive out) ideR
            return True

        webView `set` [webViewZoomLevel := 2.0]
        cid2 <- on webView keyPressEvent $ do
            key <- eventKeyName
            mod <- eventModifier
            liftIO $ case (key, mod) of
                ("plus", [Shift,Control]) -> webViewZoomIn  webView >> return True
                ("minus",[Control])       -> webViewZoomOut webView >> return True
                ("BackSpace", [Shift])    -> webViewGoBack  webView >> return True
                _                         -> return False

        cid3 <- on webView populatePopup $ \ menu -> do
            alwaysHtml <- readIORef alwaysHtmlRef
            action <- toggleActionNew "AlwaysHTML" (__"Always HTML") Nothing Nothing
            item <- castToMenuItem <$> actionCreateMenuItem action
            item `on` menuItemActivate $ writeIORef alwaysHtmlRef $ not alwaysHtml
            toggleActionSetActive action alwaysHtml
            menuShellAppend menu item
            return ()

        cid4 <- on uriEntry entryActivated $ do
            uri <- entryGetText uriEntry
            webViewLoadUri webView uri
            (`reflectIDE` ideR) $ modifyIDE_ (\ide -> ide {autoURI = Just uri})

        cid5 <- on webView loadCommitted $ \ frame -> do
            mbUri <- webFrameGetUri frame
            valueUri <- getValueUri
            case mbUri of
                Just uri | uri /= valueUri -> do
                    entrySetText uriEntry uri
                    (`reflectIDE` ideR) $ modifyIDE_ (\ide -> ide {autoURI = Just uri})
                Just _ -> do
                    (`reflectIDE` ideR) $ modifyIDE_ (\ide -> ide {autoURI = Nothing})
                Nothing  -> return ()

        cid6 <- uriEntry `after` focusInEvent $ do
            liftIO $ reflectIDE (makeActive out) ideR
            return True

        settings <- webViewGetWebSettings webView
        settings `set` [webSettingsEnableDeveloperExtras := True]
        webViewSetWebSettings webView settings
        inspector <- webViewGetInspector webView
        cid7 <- on inspector inspectWebView $ \view -> (`reflectIDE` ideR) $ do
            inspectPane <- getInspectPane Nothing
            displayPane inspectPane False
            return $ inspectView inspectPane

        return (Just out, [ConnectC cid1, ConnectC cid2, ConnectC cid3, ConnectC cid4, ConnectC cid5, ConnectC cid6])
#else
        return (Just out, [])
#endif


getOutputPane :: Maybe PanePath -> IDEM IDEOutput
getOutputPane Nothing    = forceGetPane (Right "*Out")
getOutputPane (Just pp)  = forceGetPane (Left pp)

getValueUri :: IO Text
getValueUri = do
    dataDir <- leksahOrPackageDir "pretty-show" getDataDir
    return . T.pack $ "file://"
        ++ (case dataDir of
                ('/':_) -> dataDir
                _       -> '/':dataDir)
        ++ "/value.html"

setOutput :: Text -> Text -> IDEAction
setOutput command str =
#ifdef WEBKITGTK
     do out <- getOutputPane Nothing
        liftIO $ do
            entrySetText (uriEntry out) (T.pack $ show command)
            uri <- getValueUri
            alwaysHtml <- readIORef $ alwaysHtmlRef out
            let view = webView out
                html = case (alwaysHtml, parseValue $ T.unpack str) of
                            (False, Just value) -> T.pack $ valToHtmlPage defaultHtmlOpts value
                            _                   -> str
            webViewLoadString view html Nothing uri
#else
        return ()
#endif

loadOutputUri :: FilePath -> IDEAction
loadOutputUri uri =
#ifdef WEBKITGTK
     do out <- getOutputPane Nothing
        let view = webView out
        liftIO $ do
            entrySetText (uriEntry out) (T.pack uri)
            currentUri <- webViewGetUri view
            if Just (T.pack uri) == currentUri
                then webViewReload view
                else webViewLoadUri view (T.pack uri)
#else
        return ()
#endif

