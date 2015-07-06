{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.GUIUtils
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- |
--
-------------------------------------------------------------------------------
module IDE.Utils.GUIUtils (
    chooseFile
,   chooseDir
,   chooseSaveFile
,   openBrowser
,   showDialog
,   showErrorDialog

,   getCandyState
,   setCandyState
,   getFullScreenState
,   setFullScreenState
,   getDarkState
,   setDarkState
,   getForgetSession

,   getBackgroundBuildToggled
,   setBackgroundBuildToggled
,   getRunUnitTests
,   setRunUnitTests
,   getMakeModeToggled
,   setMakeModeToggled
,   getDebugToggled
,   setDebugToggled

,   getRecentFiles
,   getRecentWorkspaces
,   getVCS

,   stockIdFromType
,   mapControlCommand
,   treeViewContextMenu

,   __

,   fontDescription
) where

import Graphics.UI.Gtk
import IDE.Utils.Tool (runProcess)
import Data.Maybe (fromJust, isJust)
import Control.Monad
import IDE.Core.State
--import Graphics.UI.Gtk.Selectors.FileChooser
--    (FileChooserAction(..))
--import Graphics.UI.Gtk.General.Structs
--    (ResponseId(..))
import Control.Monad.IO.Class (liftIO)
import Control.Exception as E
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack)

#ifdef LOCALIZATION

import Text.I18N.GetText
import System.IO.Unsafe (unsafePerformIO)

#endif

chooseDir :: Window -> Text -> Maybe FilePath -> IO (Maybe FilePath)
chooseDir window prompt mbFolder = do
    dialog <- fileChooserDialogNew
                    (Just prompt)
                    (Just window)
                FileChooserActionSelectFolder
                [("gtk-cancel"
                ,ResponseCancel)
                ,("gtk-open"
                ,ResponseAccept)]
    when (isJust mbFolder) . void $ fileChooserSetCurrentFolder dialog (fromJust mbFolder)
    widgetShow dialog
    response <- dialogRun dialog
    case response of
        ResponseAccept -> do
            fn <- fileChooserGetFilename dialog
            widgetDestroy dialog
            return fn
        ResponseCancel -> do
            widgetDestroy dialog
            return Nothing
        ResponseDeleteEvent -> do
            widgetDestroy dialog
            return Nothing
        _                   -> return Nothing

chooseFile :: Window -> Text -> Maybe FilePath -> IO (Maybe FilePath)
chooseFile window prompt mbFolder = do
    dialog <- fileChooserDialogNew
                    (Just prompt)
                    (Just window)
                FileChooserActionOpen
                [("gtk-cancel"
                ,ResponseCancel)
                ,("gtk-open"
                ,ResponseAccept)]
    when (isJust mbFolder) $ void (fileChooserSetCurrentFolder dialog (fromJust mbFolder))
    widgetShow dialog
    response <- dialogRun dialog
    case response of
        ResponseAccept -> do
            fn <- fileChooserGetFilename dialog
            widgetDestroy dialog
            return fn
        ResponseCancel -> do
            widgetDestroy dialog
            return Nothing
        ResponseDeleteEvent -> do
            widgetDestroy dialog
            return Nothing
        _                   -> return Nothing

chooseSaveFile :: Window -> Text -> Maybe FilePath -> IO (Maybe FilePath)
chooseSaveFile window prompt mbFolder = do
    dialog <- fileChooserDialogNew
              (Just prompt)
              (Just window)
              FileChooserActionSave
              [("gtk-cancel", ResponseCancel)
              ,("gtk-save",   ResponseAccept)]
    when (isJust mbFolder) $ void (fileChooserSetCurrentFolder dialog (fromJust mbFolder))
    widgetShow dialog
    res <- dialogRun dialog
    case res of
        ResponseAccept  ->  do
            mbFileName <- fileChooserGetFilename dialog
            widgetDestroy dialog
            return mbFileName
        _               ->  do
            widgetDestroy dialog
            return Nothing



openBrowser :: Text -> IDEAction
openBrowser url = do
    prefs' <- readIDE prefs
    liftIO (E.catch (do
                runProcess (T.unpack $ browser prefs') [T.unpack url] Nothing Nothing Nothing Nothing Nothing
                return ())
            (\ (_ :: SomeException) -> sysMessage Normal ("Can't find browser executable " <> browser prefs')))
    return ()


showDialog :: Text -> MessageType -> IO ()
showDialog msg msgType = do
    dialog <- messageDialogNew Nothing [] msgType ButtonsOk msg
    _ <- dialogRun dialog
    widgetDestroy dialog
    return ()

showErrorDialog :: Text -> IO ()
showErrorDialog msg = showDialog msg MessageError

-- get widget elements (menu & toolbar)

getCandyState :: PaneMonad alpha => alpha Bool
getCandyState = do
    ui <- getUIAction "ui/menubar/_Configuration/Source Candy" castToToggleAction
    liftIO $toggleActionGetActive ui

setCandyState :: PaneMonad alpha => Bool -> alpha ()
setCandyState b = do
    ui <- getUIAction "ui/menubar/_Configuration/Source Candy" castToToggleAction
    liftIO $toggleActionSetActive ui b

getFullScreenState :: PaneMonad alpha => alpha Bool
getFullScreenState = do
    ui <- getUIAction "ui/menubar/_View/_Full Screen" castToToggleAction
    liftIO $toggleActionGetActive ui

setFullScreenState :: PaneMonad alpha => Bool -> alpha ()
setFullScreenState b = do
    ui <- getUIAction "ui/menubar/_View/_Full Screen" castToToggleAction
    liftIO $toggleActionSetActive ui b

getDarkState :: PaneMonad alpha => alpha Bool
getDarkState = do
    ui <- getUIAction "ui/menubar/_View/Dark" castToToggleAction
    liftIO $toggleActionGetActive ui

setDarkState :: PaneMonad alpha => Bool -> alpha ()
setDarkState b = do
    ui <- getUIAction "ui/menubar/_View/Dark" castToToggleAction
    liftIO $toggleActionSetActive ui b

getForgetSession :: PaneMonad alpha => alpha Bool
getForgetSession = do
    ui <- getUIAction "ui/menubar/_Configuration/Forget Session" castToToggleAction
    liftIO $toggleActionGetActive ui

getMenuItem :: Text -> IDEM MenuItem
getMenuItem path = do
    uiManager' <- getUiManager
    mbWidget   <- liftIO $ uiManagerGetWidget uiManager' path
    case mbWidget of
        Nothing     -> throwIDE ("State.hs>>getMenuItem: Can't find ui path " <> path)
        Just widget -> return (castToMenuItem widget)

getBackgroundBuildToggled :: PaneMonad alpha => alpha Bool
getBackgroundBuildToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/BackgroundBuild" castToToggleAction
    liftIO $ toggleActionGetActive ui

setBackgroundBuildToggled :: PaneMonad alpha => Bool -> alpha ()
setBackgroundBuildToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/BackgroundBuild" castToToggleAction
    liftIO $ toggleActionSetActive ui b

getRunUnitTests :: PaneMonad alpha => alpha Bool
getRunUnitTests = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/RunUnitTests" castToToggleAction
    liftIO $ toggleActionGetActive ui

setRunUnitTests :: PaneMonad alpha => Bool -> alpha ()
setRunUnitTests b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/RunUnitTests" castToToggleAction
    liftIO $ toggleActionSetActive ui b

getMakeModeToggled :: PaneMonad alpha => alpha Bool
getMakeModeToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/MakeMode" castToToggleAction
    liftIO $ toggleActionGetActive ui

setMakeModeToggled :: PaneMonad alpha => Bool -> alpha ()
setMakeModeToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/MakeMode" castToToggleAction
    liftIO $ toggleActionSetActive ui b

getDebugToggled :: PaneMonad alpha => alpha Bool
getDebugToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/Debug" castToToggleAction
    liftIO $ toggleActionGetActive ui

setDebugToggled :: PaneMonad alpha => Bool -> alpha ()
setDebugToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/Debug" castToToggleAction
    liftIO $ toggleActionSetActive ui b

getRecentFiles , getRecentWorkspaces, getVCS :: IDEM MenuItem
getRecentFiles    = getMenuItem "ui/menubar/_File/Open _Recent"
getRecentWorkspaces = getMenuItem "ui/menubar/_Workspace/Open _Recent"
getVCS = getMenuItem "ui/menubar/Version Con_trol" --this could fail, try returning Menu if it does
-- (toolbar)

stockIdFromType :: DescrType -> StockId
stockIdFromType Variable        =   "ide_function"
stockIdFromType Newtype         =   "ide_newtype"
stockIdFromType Type            =   "ide_type"
stockIdFromType Data            =   "ide_data"
stockIdFromType Class           =   "ide_class"
stockIdFromType Instance        =   "ide_instance"
stockIdFromType Constructor     =   "ide_konstructor"
stockIdFromType Field           =   "ide_slot"
stockIdFromType Method          =   "ide_method"
stockIdFromType _               =   "ide_other"

-- maps control key for Macos
#if defined(darwin_HOST_OS)
mapControlCommand Alt = Control
#endif
mapControlCommand a = a

treeViewContextMenu :: TreeViewClass treeView
                    => treeView
                    -> (Menu -> IO ())
                    -> IO (ConnectId treeView, ConnectId treeView)
treeViewContextMenu treeView populateMenu = do
    cid1 <- treeView `on` popupMenuSignal $ showMenu Nothing
    cid2 <- treeView `on` buttonPressEvent $ do
        button    <- eventButton
        click     <- eventClick
        timestamp <- eventTime
        (x, y)    <- eventCoordinates
        case (button, click) of
            (RightButton, SingleClick) -> liftIO $ do
                sel <- treeViewGetSelection treeView
                selCount <- treeSelectionCountSelectedRows sel
                when (selCount <= 1) $ do
                    pathInfo <- treeViewGetPathAtPos treeView (floor x, floor y)
                    case pathInfo of
                        Just (path, _, _) -> do
                            treeSelectionUnselectAll sel
                            treeSelectionSelectPath sel path
                        _ -> return ()
                showMenu (Just (button, timestamp))
            _ -> return False
    return (cid1, cid2)
  where
    showMenu buttonEventDetails = do
        theMenu <- menuNew
        menuAttachToWidget theMenu treeView
        populateMenu theMenu
        menuPopup theMenu buttonEventDetails
        widgetShowAll theMenu
        return True

#ifdef LOCALIZATION

-- | For i18n using hgettext
__ :: Text -> Text
__ = T.pack . unsafePerformIO . getText . T.unpack


#else

-- | For i18n support. Not included in this build.
__ :: Text -> Text
__ = id

#endif

fontDescription :: Maybe Text -> IDEM FontDescription
fontDescription mbFontString = liftIO $
    case mbFontString of
        Just str ->
            fontDescriptionFromString str
        Nothing -> do
            f <- fontDescriptionNew
            fontDescriptionSetFamily f ("Monospace" :: Text)
            return f

