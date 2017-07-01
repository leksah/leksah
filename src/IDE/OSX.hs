{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.OSX
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
--  Support for OSX
--
---------------------------------------------------------------------------------

module IDE.OSX (
    applicationNew
,   updateMenu
,   applicationReady
,   allowFullscreen
) where

import Control.Monad.IO.Class (MonadIO(..))
import GI.Gdk.Objects.Window (IsWindow)
import GI.Gtk.Objects.UIManager (uIManagerGetWidget, UIManager(..))
import IDE.Core.State

#if defined(darwin_HOST_OS)

import Control.Monad.Reader.Class (ask)
import IDE.Command (canQuit)
import Data.Text (Text)
import GI.GtkosxApplication
       (Application(..), applicationReady,
        applicationSetUseQuartzAccelerators,
        onApplicationNSApplicationBlockTermination,
        applicationInsertAppMenuItem, applicationSetMenuBar, Application)
import GI.Gtk.Objects.Widget (widgetHide)
import Data.GI.Base (unsafeCastTo, new')
import GI.Gtk.Objects.MenuShell (MenuShell(..))
import GI.Gtk.Objects.MenuItem (MenuItem(..))
import GI.Gtk.Objects.SeparatorMenuItem (separatorMenuItemNew)
import Control.Monad.IO.Class (MonadIO)
import Data.GI.Base.BasicTypes (NullToNothing(..))

applicationNew :: MonadIO m => m Application
applicationNew = new' Application []

updateMenu :: Application -> UIManager -> IDEM ()
updateMenu app uiManager = do
    ideR <- ask
    uIManagerGetWidget uiManager "/ui/menubar" >>= mapM (liftIO . unsafeCastTo MenuShell) >>= \case
        Just menu -> do
            widgetHide menu
            applicationSetMenuBar app menu
        Nothing   -> return ()

    uIManagerGetWidget uiManager "/ui/menubar/_File/_Quit" >>= \case
        Just quit -> widgetHide quit
        Nothing   -> return ()

    uIManagerGetWidget uiManager "/ui/menubar/_Help/_About" >>= mapM (liftIO . unsafeCastTo MenuItem) >>= \case
        Just about -> do
            applicationInsertAppMenuItem app about 0
            sep <- separatorMenuItemNew
            applicationInsertAppMenuItem app sep 1
        Nothing   -> return ()

    uIManagerGetWidget uiManager "/ui/menubar/_Tools/_Preferences" >>= mapM (liftIO . unsafeCastTo MenuItem) >>= \case
        Just prefs -> do
            applicationInsertAppMenuItem app prefs 2
        Nothing   -> return ()

    onApplicationNSApplicationBlockTermination app $ reflectIDE (fmap not canQuit) ideR

    applicationSetUseQuartzAccelerators app True

allowFullscreen :: IsWindow window => window -> IO ()
allowFullscreen _ = return ()

#else

data Application = Application
applicationNew :: MonadIO m => m Application
applicationNew = return Application
updateMenu :: Application -> UIManager -> IDEM ()
updateMenu _ _ = return ()
applicationReady :: MonadIO m => Application -> m ()
applicationReady _ = return ()
allowFullscreen :: MonadIO m => IsWindow window => window -> m ()
allowFullscreen _ = return ()

#endif
