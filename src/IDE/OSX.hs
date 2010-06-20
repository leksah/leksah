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
import Graphics.UI.Gtk.OSX

updateMenu :: Application -> UIManager -> IO ()
updateMenu app uiManager = do
    mbMenu   <- uiManagerGetWidget uiManager "/ui/menubar"
    case mbMenu of
        Just menu -> do
            widgetHide menu
            applicationSetMenuBar app (castToMenuShell menu)
        Nothing   -> return ()

    mbQuit   <- uiManagerGetWidget uiManager "/ui/menubar/_File/_Quit"
    case mbQuit of
        Just quit -> widgetHide quit
        Nothing   -> return ()

    mbAbout   <- uiManagerGetWidget uiManager "/ui/menubar/_Help/_About"
    case mbAbout of
        Just about -> do
            group <- applicationAddAppMenuGroup app
            applicationAddAppMenuItem app group (castToMenuItem about)
        Nothing   -> return ()

    mbPrefs   <- uiManagerGetWidget uiManager "/ui/menubar/_Configuration/Edit general Preferences"
    case mbPrefs of
        Just prefs -> do
            group <- applicationAddAppMenuGroup app
            applicationAddAppMenuItem app group (castToMenuItem prefs)
        Nothing   -> return ()

    applicationSetUseQuartsAccelerators app True

