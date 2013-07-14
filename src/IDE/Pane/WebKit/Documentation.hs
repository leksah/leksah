{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
  , loadDoc
  , reloadDoc
) where

import Graphics.UI.Frame.Panes
       (RecoverablePane(..), PanePath, RecoverablePane, Pane(..))
import Graphics.UI.Gtk
       (scrolledWindowSetPolicy, scrolledWindowNew, castToWidget,
        ScrolledWindow)
import Data.Typeable (Typeable)
import IDE.Core.Types (IDEAction, IDEM)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Frame.ViewFrame (getNotebook)
import IDE.Core.State (reifyIDE)
import Graphics.UI.Gtk.General.Enums (PolicyType(..))

#ifdef WEBKITGTK
import Graphics.UI.Gtk
       (eventModifier, eventKeyName, keyPressEvent, focusInEvent,
        containerAdd, Modifier(..), after)
import Graphics.UI.Gtk.WebKit.Types (WebView(..))
import Graphics.UI.Gtk.WebKit.WebView
       (webViewUri, webViewGoBack, webViewZoomOut, webViewZoomIn,
        webViewZoomLevel, webViewReload, webViewLoadUri, webViewNew)
import System.Glib.Attributes (AttrOp(..), set, get)
import System.Glib.Signals (on)
import IDE.Core.State (reflectIDE)
import Graphics.UI.Editor.Basics (Connection(..))
#else
import Data.IORef (writeIORef, newIORef, readIORef, IORef)
import Control.Applicative ((<$>))
#endif

data IDEDocumentation = IDEDocumentation {
    scrolledView :: ScrolledWindow
#ifdef WEBKITGTK
  , webView      :: WebView
#else
  , docState     :: IORef DocumentationState
#endif
} deriving Typeable

data DocumentationState = DocumentationState {
    zoom :: Float
  , uri  :: Maybe String
} deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEDocumentation IDEM
    where
    primPaneName _  =   "Doc"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Doc"

instance RecoverablePane IDEDocumentation DocumentationState IDEM where
    saveState p     =   liftIO $ do
#ifdef WEBKITGTK
        zoom <- webView p `get` webViewZoomLevel
        uri  <- webView p `get` webViewUri
        return (Just DocumentationState{..})
#else
        Just <$> readIORef (docState p)
#endif
    recoverState pp DocumentationState {..} =   do
        nb      <-  getNotebook pp
        mbPane <- buildPane pp nb builder
        case mbPane of
            Nothing -> return ()
            Just p  -> liftIO $ do
#ifdef WEBKITGTK
                webView p `set` [webViewZoomLevel := zoom]
                maybe (return ()) (webViewLoadUri (webView p)) uri
#else
                writeIORef (docState p) DocumentationState {..}
#endif
        return mbPane
    builder pp nb windows = reifyIDE $ \ ideR -> do
        scrolledView <- scrolledWindowNew Nothing Nothing

#ifdef WEBKITGTK
        webView <- webViewNew
        containerAdd scrolledView webView
#else
        docState <- newIORef DocumentationState {zoom = 1.0, uri = Nothing}
#endif

        scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic
        let docs = IDEDocumentation {..}

#ifdef WEBKITGTK
        cid1 <- after webView focusInEvent $ do
            liftIO $ reflectIDE (makeActive docs) ideR
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
        return (Just docs, map ConnectC [cid1, cid2])
#else
        return (Just docs, [])
#endif


getDocumentation :: Maybe PanePath -> IDEM IDEDocumentation
getDocumentation Nothing    = forceGetPane (Right "*Doc")
getDocumentation (Just pp)  = forceGetPane (Left pp)

loadDoc :: String -> IDEAction
loadDoc uri = do
#ifdef WEBKITGTK
    doc <- getDocumentation Nothing
    let view = webView doc
    liftIO $ webViewLoadUri view uri
#else
    return ()
#endif

reloadDoc :: IDEAction
reloadDoc = do
#ifdef WEBKITGTK
    doc <- getDocumentation Nothing
    let view = webView doc
    liftIO $ webViewReload view
#else
    return ()
#endif

