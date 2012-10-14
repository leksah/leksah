{-# LANGUAGE CPP #-}
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
) where

import Graphics.UI.Gtk
import IDE.Core.State

#if defined(darwin_HOST_OS)

import Control.Monad.Reader (liftIO)
import Control.Monad.Reader.Class (ask)
import Graphics.UI.Gtk.OSX
import IDE.Command (canQuit)

updateMenu :: Application -> UIManager -> IDEM ()
updateMenu app uiManager = do
    ideR <- ask
    liftIO $ do
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
                applicationInsertAppMenuItem app (castToMenuItem about) 0
                sep <- separatorMenuItemNew
                applicationInsertAppMenuItem app sep 1
            Nothing   -> return ()

        mbPrefs   <- uiManagerGetWidget uiManager "/ui/menubar/_Configuration/Edit general Preferences"
        case mbPrefs of
            Just prefs -> do
                applicationInsertAppMenuItem app (castToMenuItem prefs) 2
            Nothing   -> return ()

        app `on` blockTermination $ reflectIDE (fmap not canQuit) ideR

        applicationSetUseQuartsAccelerators app True

#else

data Application = Application
applicationNew :: IO Application
applicationNew = return Application
updateMenu :: Application -> UIManager -> IDEM ()
updateMenu _ _ = return ()
applicationReady :: Application -> IO ()
applicationReady _ = return ()

#endif
