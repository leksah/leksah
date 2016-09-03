{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings, DataKinds, PatternSynonyms #-}
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
,   showDialogOptions
,   showInputDialog

,   getFullScreenState
,   setFullScreenState
,   getDarkState
,   setDarkState

,   getBackgroundBuildToggled
,   setBackgroundBuildToggled
,   getMakeDocs
,   setMakeDocs
,   getRunUnitTests
,   setRunUnitTests
,   getRunBenchmarks
,   setRunBenchmarks
,   getMakeModeToggled
,   setMakeModeToggled
,   getDebugToggled
,   setDebugToggled

,   getRecentFiles
,   getRecentWorkspaces
,   getVCS

,   stockIdFromType
,   mapControlCommand
,   treeViewToggleRow
,   treeViewContextMenu
,   treeViewContextMenu'

,   __

,   fontDescription
) where

import IDE.Utils.Tool (runProcess)
import Data.Maybe
       (listToMaybe, fromMaybe, catMaybes, fromJust, isJust)
import Control.Monad (void, when, unless)
import IDE.Core.State
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception as E
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack, pack)
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.Foldable (forM_)
import Data.Tree (Tree(..), Forest)
import GI.Gtk.Objects.Window
       (setWindowWindowPosition, windowSetTransientFor, setWindowTitle,
        Window(..))
import GI.Gtk.Enums
       (WindowPosition(..), MessageType(..), ButtonsType(..), MessageType,
        ResponseType(..), FileChooserAction(..))
import Data.GI.Base (new', nullToNothing)
import GI.Gtk.Objects.FileChooserDialog (FileChooserDialog(..))
import GI.Gtk.Interfaces.FileChooser
       (fileChooserAddFilter, fileChooserGetFilename,
        fileChooserSetCurrentFolder, fileChooserSetAction)
import GI.Gtk.Objects.Dialog
       (Dialog(..), constructDialogUseHeaderBar, dialogGetContentArea,
        dialogSetDefaultResponse, dialogRun, dialogAddButton)
import GI.Gtk.Objects.FileFilter
       (fileFilterAddPattern, fileFilterSetName, fileFilterNew)
import GI.Gtk.Objects.MessageDialog
       (constructMessageDialogText, constructMessageDialogButtons,
        constructMessageDialogMessageType, MessageDialog(..))
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import GI.Gtk.Objects.Box (boxPackStart, Box(..))
import GI.Gtk.Objects.Label (labelNew)
import GI.Gtk.Objects.Entry (getEntryText, setEntryText, entryNew, onEntryActivate)
import Graphics.UI.Editor.Parameters
       (dialogSetDefaultResponse', dialogRun', dialogAddButton',
        boxPackStart', Packing(..))
import GI.Gtk.Objects.ToggleAction
       (toggleActionSetActive, toggleActionGetActive, ToggleAction(..))
import GI.Gtk.Objects.MenuItem
       (toMenuItem, onMenuItemActivate, menuItemNewWithLabel,
        MenuItem(..))
import GI.Gtk.Objects.UIManager (uIManagerGetWidget)
import GI.Gtk.Objects.TreeView
       (toTreeView, TreeView(..), treeViewGetPathAtPos,
        treeViewGetSelection, treeViewExpandRow,
        treeViewCollapseRow, treeViewRowExpanded)
import GI.Gdk.Flags (ModifierType(..))
import GI.Gtk.Structs.TreePath
       (TreePath(..))
import GI.Gtk.Objects.Widget
       (Widget(..), toWidget, noWidget, onWidgetButtonPressEvent, onWidgetPopupMenu,
        widgetShowAll, widgetHide, widgetDestroy, widgetShow, widgetActivate)
import GI.Gdk
       (pattern BUTTON_SECONDARY, getEventButtonType, getEventButtonY,
        getEventButtonX, getEventButtonTime, getEventButtonButton)
import GI.Gdk.Enums (EventType(..))
import GI.Gtk.Objects.TreeSelection
       (treeSelectionSelectPath, treeSelectionUnselectAll, treeSelectionCountSelectedRows)
import Graphics.UI.Editor.Basics (Connection, Connection)
import Data.GI.Gtk.ModelView.ForestStore
       (forestStoreGetValue, forestStoreGetTree, ForestStore(..))
import GI.Gtk.Objects.Menu
       (Menu(..), menuPopup, menuAttachToWidget, menuNew)
import GI.Gtk.Objects.SeparatorMenuItem (separatorMenuItemNew)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import GI.Pango.Structs.FontDescription
       (fontDescriptionSetFamily, fontDescriptionNew, FontDescription(..))
import GI.Pango (fontDescriptionFromString)
import Data.GI.Base.BasicTypes (UnexpectedNullPointerReturn(..))
import GI.Gtk.Functions (getCurrentEventTime)
import Data.Word (Word32)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathNewFromIndices')

#ifdef LOCALIZATION

import Text.I18N.GetText
import System.IO.Unsafe (unsafePerformIO)

#endif

chooseDir :: Window -> Text -> Maybe FilePath -> IO (Maybe FilePath)
chooseDir window prompt mbFolder = do
    dialog <- new' FileChooserDialog [constructDialogUseHeaderBar 1]
    setWindowTitle dialog prompt
    windowSetTransientFor dialog $ Just window
    fileChooserSetAction dialog FileChooserActionSelectFolder
    dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
    dialogAddButton' dialog "gtk-open" ResponseTypeAccept
    when (isJust mbFolder) . void $ fileChooserSetCurrentFolder dialog (fromJust mbFolder)
    widgetShow dialog
    response <- dialogRun' dialog
    case response of
        ResponseTypeAccept -> do
            fn <- nullToNothing $ fileChooserGetFilename dialog
            widgetDestroy dialog
            return fn
        ResponseTypeCancel -> do
            widgetDestroy dialog
            return Nothing
        ResponseTypeDeleteEvent -> do
            widgetDestroy dialog
            return Nothing
        _                   -> return Nothing

-- | Launch a "choose file" dialog
chooseFile :: Window
           -> Text                   -- ^ Window title
           -> Maybe FilePath         -- ^ Start location
           -> [(String, [String])]   -- ^ File filters, e.g. [("Music Files", ["*.mp3", "*.wav"])]
           -> IO (Maybe FilePath)
chooseFile window prompt mbFolder filters = do
    dialog <- new' FileChooserDialog [constructDialogUseHeaderBar 1]
    setWindowTitle dialog prompt
    windowSetTransientFor dialog $ Just window
    fileChooserSetAction dialog FileChooserActionOpen
    dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
    dialogAddButton' dialog "gtk-open" ResponseTypeAccept
    forM_ mbFolder $ \folder ->
        void (fileChooserSetCurrentFolder dialog folder)
    forM_ filters (addFilter dialog)
    widgetShow dialog
    response <- dialogRun' dialog
    case response of
        ResponseTypeAccept -> do
            fn <- nullToNothing $ fileChooserGetFilename dialog
            widgetDestroy dialog
            return fn
        ResponseTypeCancel -> do
            widgetDestroy dialog
            return Nothing
        ResponseTypeDeleteEvent -> do
            widgetDestroy dialog
            return Nothing
        _                   -> return Nothing

    where
        addFilter dialog (description, exts) = do
            ff <- fileFilterNew
            fileFilterSetName ff . Just $ T.pack description
            forM_ exts (fileFilterAddPattern ff . T.pack)
            fileChooserAddFilter dialog ff

chooseSaveFile :: Window -> Text -> Maybe FilePath -> IO (Maybe FilePath)
chooseSaveFile window prompt mbFolder = do
    dialog <- new' FileChooserDialog [constructDialogUseHeaderBar 1]
    setWindowTitle dialog prompt
    windowSetTransientFor dialog $ Just window
    fileChooserSetAction dialog FileChooserActionSave
    dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
    dialogAddButton' dialog "gtk-open" ResponseTypeAccept
    when (isJust mbFolder) $ void (fileChooserSetCurrentFolder dialog (fromJust mbFolder))
    widgetShow dialog
    res <- dialogRun' dialog
    case res of
        ResponseTypeAccept  ->  do
            fileName <- nullToNothing $ fileChooserGetFilename dialog
            widgetDestroy dialog
            return fileName
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

-- | Show a text dialog with an Ok button and a specific messagetype
showDialog :: Text -> MessageType -> IO ()
showDialog msg msgType = do
    dialog <- new' MessageDialog [
        constructDialogUseHeaderBar 0,
        constructMessageDialogMessageType msgType,
        constructMessageDialogButtons ButtonsTypeOk,
        constructMessageDialogText msg]
    _ <- dialogRun' dialog
    widgetDestroy dialog
    return ()


-- | Show an error dialog with an Ok button
showErrorDialog :: Text -> IO ()
showErrorDialog msg = showDialog msg MessageTypeError


-- | Show a dialog with custom buttons and callbacks
showDialogOptions :: Text             -- ^ the message
                  -> MessageType      -- ^ type of dialog
                  -> [(Text, IO ())]  -- ^ button text and corresponding actions
                  -> Maybe Int        -- ^ index of button that has default focus (0-based)
                  -> IO ()
showDialogOptions msg msgType buttons mbIndex = do
    dialog <- new' MessageDialog [
        constructDialogUseHeaderBar 0,
        constructMessageDialogMessageType msgType,
        constructMessageDialogButtons ButtonsTypeNone,
        constructMessageDialogText msg]

    forM_ (zip [0..] buttons) $ \(n,(text, _)) ->
        dialogAddButton' dialog text (AnotherResponseType n)

    dialogSetDefaultResponse' dialog (AnotherResponseType $ fromMaybe 0 mbIndex)
    setWindowWindowPosition dialog WindowPositionCenterOnParent
    res <- dialogRun' dialog
    widgetHide dialog
    case res of
        AnotherResponseType n | n >= 0 && n < length buttons -> map snd buttons !! n
        _ -> return ()


-- | Show a simple dialog that asks the user for some text
showInputDialog :: Text -- ^ The message text
                -> Text -- ^ The default value
                -> IO (Maybe Text)
showInputDialog msg def = do
    dialog <- new' Dialog [constructDialogUseHeaderBar 1] -- Nothing [] MessageQuestion ButtonsOkCancel msg
    vbox   <- dialogGetContentArea dialog >>= unsafeCastTo Box
    label <- labelNew (Just msg)
    entry <- entryNew
    setEntryText entry def
    boxPackStart' vbox label PackNatural 0
    boxPackStart' vbox entry PackNatural 0
    widgetShowAll vbox

    -- Can't use messageDialog because of https://github.com/gtk2hs/gtk2hs/issues/114
    dialogAddButton' dialog "Cancel" ResponseTypeCancel
    okBtn <- dialogAddButton' dialog "Ok" ResponseTypeOk
    onEntryActivate entry $ widgetActivate okBtn >> return ()
    dialogSetDefaultResponse' dialog ResponseTypeOk

    res <- dialogRun' dialog
    widgetHide dialog

    case res of
        ResponseTypeOk -> do
            text <- getEntryText entry
            widgetDestroy dialog
            return (Just text)
        _ -> widgetDestroy dialog >> return Nothing




-- get widget elements (menu & toolbar)


getFullScreenState :: PaneMonad alpha => alpha Bool
getFullScreenState = do
    ui <- getUIAction "ui/menubar/_View/_Full Screen" (unsafeCastTo ToggleAction)
    toggleActionGetActive ui

setFullScreenState :: PaneMonad alpha => Bool -> alpha ()
setFullScreenState b = do
    ui <- getUIAction "ui/menubar/_View/_Full Screen" (unsafeCastTo ToggleAction)
    toggleActionSetActive ui b

getDarkState :: PaneMonad alpha => alpha Bool
getDarkState = do
    ui <- getUIAction "ui/menubar/_View/_Use Dark Interface" (unsafeCastTo ToggleAction)
    toggleActionGetActive ui

setDarkState :: PaneMonad alpha => Bool -> alpha ()
setDarkState b = do
    ui <- getUIAction "ui/menubar/_View/_Use Dark Interface" (unsafeCastTo ToggleAction)
    toggleActionSetActive ui b

getMenuItem :: Text -> IDEM MenuItem
getMenuItem path = (do
    uiManager' <- getUiManager
    uIManagerGetWidget uiManager' path >>= (liftIO . unsafeCastTo MenuItem))
        `catchIDE` \(_::UnexpectedNullPointerReturn) ->
            throwIDE ("State.hs>>getMenuItem: Can't find ui path " <> path)

getBackgroundBuildToggled :: PaneMonad alpha => alpha Bool
getBackgroundBuildToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/BackgroundBuild" (unsafeCastTo ToggleAction)
    toggleActionGetActive ui

setBackgroundBuildToggled :: PaneMonad alpha => Bool -> alpha ()
setBackgroundBuildToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/BackgroundBuild" (unsafeCastTo ToggleAction)
    toggleActionSetActive ui b

getMakeDocs :: PaneMonad alpha => alpha Bool
getMakeDocs = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/MakeDocs" (unsafeCastTo ToggleAction)
    toggleActionGetActive ui

setMakeDocs :: PaneMonad alpha => Bool -> alpha ()
setMakeDocs b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/MakeDocs" (unsafeCastTo ToggleAction)
    toggleActionSetActive ui b

getRunUnitTests :: PaneMonad alpha => alpha Bool
getRunUnitTests = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/RunUnitTests" (unsafeCastTo ToggleAction)
    toggleActionGetActive ui

setRunUnitTests :: PaneMonad alpha => Bool -> alpha ()
setRunUnitTests b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/RunUnitTests" (unsafeCastTo ToggleAction)
    toggleActionSetActive ui b

getRunBenchmarks :: PaneMonad alpha => alpha Bool
getRunBenchmarks = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/RunBenchmarks" (unsafeCastTo ToggleAction)
    toggleActionGetActive ui

setRunBenchmarks :: PaneMonad alpha => Bool -> alpha ()
setRunBenchmarks b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/RunBenchmarks" (unsafeCastTo ToggleAction)
    toggleActionSetActive ui b

getMakeModeToggled :: PaneMonad alpha => alpha Bool
getMakeModeToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/MakeMode" (unsafeCastTo ToggleAction)
    toggleActionGetActive ui

setMakeModeToggled :: PaneMonad alpha => Bool -> alpha ()
setMakeModeToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/MakeMode" (unsafeCastTo ToggleAction)
    toggleActionSetActive ui b

getDebugToggled :: PaneMonad alpha => alpha Bool
getDebugToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/Debug" (unsafeCastTo ToggleAction)
    toggleActionGetActive ui

setDebugToggled :: PaneMonad alpha => Bool -> alpha ()
setDebugToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/Debug" (unsafeCastTo ToggleAction)
    toggleActionSetActive ui b

getRecentFiles , getRecentWorkspaces, getVCS :: IDEM MenuItem
getRecentFiles    = getMenuItem "ui/menubar/_File/Recent Files"


getRecentWorkspaces = getMenuItem "ui/menubar/_File/Recent Workspaces"
getVCS = getMenuItem "ui/menubar/Version Con_trol" --this could fail, try returning Menu if it does
-- (toolbar)

stockIdFromType :: DescrType -> Text
stockIdFromType Variable        =   "ide_function"
stockIdFromType Newtype         =   "ide_newtype"
stockIdFromType Type            =   "ide_type"
stockIdFromType Data            =   "ide_data"
stockIdFromType Class           =   "ide_class"
stockIdFromType Instance        =   "ide_instance"
stockIdFromType Constructor     =   "ide_konstructor"
stockIdFromType Field           =   "ide_slot"
stockIdFromType Method          =   "ide_method"
stockIdFromType PatternSynonym  =   "ide_konstructor"
stockIdFromType _               =   "ide_other"


-- | Toggles a row in a `TreeView`
treeViewToggleRow treeView path = do
    expanded <- treeViewRowExpanded treeView path
    if expanded
        then treeViewCollapseRow treeView path
        else treeViewExpandRow   treeView path False

-- maps control key for Macos
#if defined(darwin_HOST_OS)
mapControlCommand ModifierTypeMod1Mask = ModifierTypeControlMask
#endif
mapControlCommand a = a

-- | Sets the context menu for a treeView widget
treeViewContextMenu' :: TreeView                 -- ^ The view
                     -> ForestStore a              -- ^ The model
                     -> (a -> TreePath -> ForestStore a -> IDEM [[(Text, IDEAction)]]) -- ^ Produces the menu items for the selected values when right clicking
                                                                                     -- The lists are seperated by a seperator
                     -> IDEM [Connection]
treeViewContextMenu' view store itemsFor = reifyIDE $ \ideRef -> do
    cid1 <- onWidgetPopupMenu view $ do
        t <- getCurrentEventTime
        showMenu 0 t ideRef
    cid2 <- onWidgetButtonPressEvent view $ \e -> do
        button    <- getEventButtonButton e
        click     <- getEventButtonType e
        timestamp <- getEventButtonTime e
        x         <- getEventButtonX e
        y         <- getEventButtonY e
        case (fromIntegral button, click) of
            (BUTTON_SECONDARY, EventTypeButtonPress) -> do
                sel <- treeViewGetSelection view
                selCount <- treeSelectionCountSelectedRows sel
                when (selCount <= 1) $ do
                    pathInfo <- treeViewGetPathAtPos view (floor x) (floor y)
                    case pathInfo of
                        (True, Just path, _, _, _) -> do
                            treeSelectionUnselectAll sel
                            treeSelectionSelectPath sel path
                        _ -> return ()
                showMenu button timestamp ideRef
            _ -> return False
    return $ map (ConnectC view) [cid1, cid2]
  where
    showMenu :: Word32 -> Word32 -> IDERef -> IO Bool
    showMenu button timestamp ideRef = do

        selPaths     <- treeViewGetSelection view >>= treeSelectionGetSelectedRows'
        selValues    <- mapM (forestStoreGetValue store) selPaths
        theMenu      <- menuNew
        menuAttachToWidget theMenu view Nothing
        forM_ (listToMaybe $ zip selValues selPaths) $ \(val, path) -> do
            itemsPerSection     <- flip reflectIDE ideRef $ itemsFor val path store
            menuItemsPerSection <- mapM (mapM (menuItemNewWithLabel . fst)) itemsPerSection


            forM_ (zip itemsPerSection menuItemsPerSection) $ \(section, itemsSection) ->
                forM_ (zip section itemsSection) $ \((_, onActivated), m) ->
                    onMenuItemActivate m $ reflectIDE onActivated ideRef

            unless (null itemsPerSection) $ do
                itemsAndSeparators <- sequence $
                    intercalate [separatorMenuItemNew >>= toMenuItem]
                                (map (map return) menuItemsPerSection)
                mapM_ (menuShellAppend theMenu) itemsAndSeparators
                menuPopup theMenu noWidget noWidget Nothing button timestamp
                widgetShowAll theMenu
        return True

treeViewContextMenu :: MonadIO m
                    => TreeView
                    -> (Menu -> IO ())
                    -> m [Connection]
treeViewContextMenu treeView populateMenu = do
    cid1 <- onWidgetPopupMenu treeView $ showMenu 0 =<< getCurrentEventTime
    cid2 <- onWidgetButtonPressEvent treeView $ \e -> do
        button    <- getEventButtonButton e
        click     <- getEventButtonType e
        timestamp <- getEventButtonTime e
        x         <- getEventButtonX e
        y         <- getEventButtonY e
        case (fromIntegral button, click) of
            (BUTTON_SECONDARY, EventTypeButtonPress) -> do
                sel <- treeViewGetSelection treeView
                selCount <- treeSelectionCountSelectedRows sel
                when (selCount <= 1) $ do
                    pathInfo <- treeViewGetPathAtPos treeView (floor x) (floor y)
                    case pathInfo of
                        (True, Just path, _, _, _) -> do
                            treeSelectionUnselectAll sel
                            treeSelectionSelectPath sel path
                        _ -> return ()
                showMenu button timestamp
            _ -> return False
    return $ map (ConnectC treeView) [cid1, cid2]
  where
    showMenu button timestamp = do
        theMenu <- menuNew
        menuAttachToWidget theMenu treeView Nothing
        populateMenu theMenu
        menuPopup theMenu noWidget noWidget Nothing button timestamp
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
fontDescription mbFontString =
    case mbFontString of
        Just str ->
            fontDescriptionFromString str
        Nothing -> do
            f <- fontDescriptionNew
            fontDescriptionSetFamily f "Monospace"
            return f

