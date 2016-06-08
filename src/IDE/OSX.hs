{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
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
import GI.Gdk.Objects.Window (WindowK)
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
    mbMenu   <- nullToNothing $ uIManagerGetWidget uiManager "/ui/menubar" >>= unsafeCastTo MenuShell
    case mbMenu of
        Just menu -> do
            widgetHide menu
            applicationSetMenuBar app menu
        Nothing   -> return ()

    mbQuit   <- nullToNothing $ uIManagerGetWidget uiManager "/ui/menubar/_File/_Quit"
    case mbQuit of
        Just quit -> widgetHide quit
        Nothing   -> return ()

    mbAbout   <- nullToNothing $ uIManagerGetWidget uiManager "/ui/menubar/_Help/_About" >>= unsafeCastTo MenuItem
    case mbAbout of
        Just about -> do
            applicationInsertAppMenuItem app about 0
            sep <- separatorMenuItemNew
            applicationInsertAppMenuItem app sep 1
        Nothing   -> return ()

    mbPrefs   <- nullToNothing $ uIManagerGetWidget uiManager "/ui/menubar/_Tools/_Preferences" >>= unsafeCastTo MenuItem
    case mbPrefs of
        Just prefs -> do
            applicationInsertAppMenuItem app prefs 2
        Nothing   -> return ()

    onApplicationNSApplicationBlockTermination app $ reflectIDE (fmap not canQuit) ideR

    applicationSetUseQuartzAccelerators app True

allowFullscreen :: WindowK window => window -> IO ()
allowFullscreen _ = return ()

#else

data Application = Application
applicationNew :: MonadIO m => m Application
applicationNew = return Application
updateMenu :: Application -> UIManager -> IDEM ()
updateMenu _ _ = return ()
applicationReady :: MonadIO m => Application -> m ()
applicationReady _ = return ()
allowFullscreen :: MonadIO m => WindowK window => window -> m ()
allowFullscreen _ = return ()

#endif
