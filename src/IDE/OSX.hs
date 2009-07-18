{-# INCLUDE <igemacintegration/ige-mac-menu.h> #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
    updateMenu
) where

import Graphics.UI.Gtk
import Foreign (nullPtr, withForeignPtr, Ptr(..))
import System.Glib (GObject(..))
import System.Glib.Types (toGObject)

foreign import ccall unsafe "ige_mac_menu_set_menu_bar" ige_mac_menu_set_menu_bar :: Ptr GObject -> IO ()
foreign import ccall unsafe "ige_mac_menu_set_quit_menu_item" ige_mac_menu_set_quit_menu_item :: Ptr GObject -> IO ()
foreign import ccall unsafe "ige_mac_menu_add_app_menu_group" ige_mac_menu_add_app_menu_group :: IO (Ptr GObject)
foreign import ccall unsafe "ige_mac_menu_add_app_menu_item" ige_mac_menu_add_app_menu_item :: Ptr GObject -> Ptr GObject -> Ptr GObject -> IO ()

updateMenu :: UIManager -> IO ()
updateMenu uiManager = do
    mbMenu   <- uiManagerGetWidget uiManager "/ui/menubar"
    case mbMenu of
        Just menu -> do
            widgetHide menu
            let (GObject p) = toGObject menu
            withForeignPtr p ige_mac_menu_set_menu_bar
        Nothing   -> return ()

    mbQuit   <- uiManagerGetWidget uiManager "/ui/menubar/_File/_Quit"
    case mbQuit of
        Just quit -> do
            let (GObject p) = toGObject quit
            withForeignPtr p ige_mac_menu_set_quit_menu_item
        Nothing   -> return ()

    mbAbout   <- uiManagerGetWidget uiManager "/ui/menubar/_Help/_About"
    case mbAbout of
        Just about -> do
            let (GObject p) = toGObject about
            group <- ige_mac_menu_add_app_menu_group
            withForeignPtr p (\ptr -> ige_mac_menu_add_app_menu_item group ptr nullPtr)
        Nothing   -> return ()

    mbPrefs   <- uiManagerGetWidget uiManager "/ui/menubar/_Configuration/Edit general Preferences"
    case mbPrefs of
        Just prefs -> do
            let (GObject p) = toGObject prefs
            group <- ige_mac_menu_add_app_menu_group
            withForeignPtr p (\ptr -> ige_mac_menu_add_app_menu_item group ptr nullPtr)
        Nothing   -> return ()

