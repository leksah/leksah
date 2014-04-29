{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
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
       (postGUISync, scrolledWindowSetPolicy, scrolledWindowNew,
        castToWidget, ScrolledWindow)
import Data.Typeable (Typeable)
import IDE.Core.Types (IDEAction, IDEM)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Frame.ViewFrame (getNotebook)
import IDE.Core.State (postSyncIDE, reifyIDE, leksahOrPackageDir)
import Graphics.UI.Gtk.General.Enums (PolicyType(..))

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
import Graphics.UI.Gtk.WebKit.WebView (webViewGetUri)

data IDEOutput = IDEOutput {
    scrolledView  :: ScrolledWindow
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
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Out"

instance RecoverablePane IDEOutput OutputState IDEM where
    saveState p     =   liftIO $ do
#ifdef WEBKITGTK
        zoom <- webView p `get` webViewZoomLevel
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
            Just p  -> liftIO $ do
#ifdef WEBKITGTK
                webView p `set` [webViewZoomLevel := zoom]
                writeIORef (alwaysHtmlRef p) alwaysHtml
#else
                writeIORef (outState p) OutputState {..}
#endif
        return mbPane
    builder pp nb windows = reifyIDE $ \ ideR -> do
        scrolledView <- scrolledWindowNew Nothing Nothing

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
        cid2 <- webView `on` keyPressEvent $ do
            key <- eventKeyName
            mod <- eventModifier
            liftIO $ case (key, mod) of
                ("plus", [Shift,Control]) -> webViewZoomIn  webView >> return True
                ("minus",[Control])       -> webViewZoomOut webView >> return True
                ("BackSpace", [])         -> webViewGoBack  webView >> return True
                _                         -> return False

        cid3 <- webView `on` populatePopup $ \ menu -> do
            alwaysHtml <- readIORef alwaysHtmlRef
            action <- toggleActionNew "AlwaysHTML" "Always HTML" Nothing Nothing
            item <- castToMenuItem <$> actionCreateMenuItem action
            item `on` menuItemActivate $ writeIORef alwaysHtmlRef $ not alwaysHtml
            toggleActionSetActive action alwaysHtml
            menuShellAppend menu item
            return ()

        return (Just out, map ConnectC [cid1, cid2, cid3])
#else
        return (Just out, [])
#endif


getOutputPane :: Maybe PanePath -> IDEM IDEOutput
getOutputPane Nothing    = forceGetPane (Right "*Out")
getOutputPane (Just pp)  = forceGetPane (Left pp)

setOutput :: String -> IDEAction
setOutput str = do
#ifdef WEBKITGTK
    out <- getOutputPane Nothing
    liftIO $ do
        dataDir <- leksahOrPackageDir "pretty-show" getDataDir
        alwaysHtml <- readIORef $ alwaysHtmlRef out
        let view = webView out
            html = case (alwaysHtml, parseValue str) of
                        (False, Just value) -> valToHtmlPage defaultHtmlOpts value
                        _                   -> str
        webViewLoadString view html Nothing Nothing ("file://"
            ++ (case dataDir of
                    ('/':_) -> dataDir
                    _       -> '/':dataDir)
            ++ "/value.html")
#else
    return ()
#endif

loadOutputUri :: FilePath -> IDEAction
loadOutputUri uri = do
#ifdef WEBKITGTK
    doc <- getOutputPane Nothing
    let view = webView doc
    liftIO $ do
        currentUri <- webViewGetUri view
        if Just uri == currentUri
            then webViewReload view
            else webViewLoadUri view uri
#else
    return ()
#endif

