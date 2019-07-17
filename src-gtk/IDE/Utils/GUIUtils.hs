{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings, DataKinds, PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
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
,   showYesNoDialog
,   showConfirmDialog
,   showErrorDialog
,   showDialogOptions
,   showDialogAndGetResponse
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
,   getNativeToggled
,   setNativeToggled
,   getJavaScriptToggled
,   setJavaScriptToggled
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
,   printf

,   fontDescription
) where

import Prelude ()
import Prelude.Compat
import IDE.Utils.Tool (runProcess)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad (void, when, unless)
import IDE.Core.State
       (IDEAction, IDEM, DescrType(..), IDERef, readIDE, prefs,
        browser, sysMessage, MessageLevel(..), throwIDE, catchIDE,
        reifyIDE, reflectIDE, __)
import IDE.Gtk.State (PaneMonad, getUIAction, getUiManager, Connection(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception as E
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Data.List (intercalate)
import Data.Foldable (forM_)
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)
import GI.Gtk.Objects.Window
       (setWindowWindowPosition, windowSetTransientFor, setWindowTitle,
        Window(..))
import GI.Gtk.Enums
       (WindowPosition(..), MessageType(..), ButtonsType(..), MessageType,
        ResponseType(..), FileChooserAction(..))
import Data.GI.Base (new')
import Data.GI.Base.GValue (GValueConstruct)
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import GI.Gtk.Objects.FileChooserDialog (FileChooserDialog(..))
import GI.Gtk.Interfaces.FileChooser
       (fileChooserAddFilter, fileChooserGetFilename,
        fileChooserSetCurrentFolder, fileChooserSetAction)
import GI.Gtk.Objects.Dialog
       (Dialog(..), constructDialogUseHeaderBar, dialogGetContentArea)
import GI.Gtk.Objects.FileFilter
       (fileFilterAddPattern, fileFilterSetName, fileFilterNew)
import GI.Gtk.Objects.MessageDialog
       (constructMessageDialogText, constructMessageDialogButtons, constructMessageDialogMessageType,
        setMessageDialogMessageType, setMessageDialogText, MessageDialog(..))
import GI.Gtk.Objects.Box (Box(..))
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
       (IsTreeView, TreeView(..), treeViewGetPathAtPos,
        treeViewGetSelection, treeViewExpandRow, treeViewCollapseRow,
        treeViewRowExpanded)
import GI.Gdk.Flags (ModifierType(..))
import GI.Gtk.Structs.TreePath
       (TreePath(..))
import GI.Gtk.Objects.Widget
       (widgetShowAll, widgetHide, widgetDestroy, widgetShow, widgetActivate)
import GI.Gtk.Objects.TreeSelection
       (treeSelectionSelectPath, treeSelectionUnselectAll, treeSelectionCountSelectedRows)
import Data.GI.Gtk.ModelView.ForestStore
       (forestStoreGetValue, ForestStore(..))
import GI.Gtk.Objects.Menu
       (Menu(..), menuAttachToWidget, menuNew)
import GI.Gtk.Objects.SeparatorMenuItem (separatorMenuItemNew)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import GI.Pango.Structs.FontDescription
       (fontDescriptionSetFamily, fontDescriptionNew, FontDescription(..))
import GI.Pango (fontDescriptionFromString)
import Data.GI.Base.BasicTypes (UnexpectedNullPointerReturn(..))
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows')
import GHC.Stack (HasCallStack)
import GI.Gdk
       (getEventButtonY, getEventButtonX, getEventButton, Event)
import GI.Gdk.Constants (pattern BUTTON_SECONDARY)
import GI.Gtk
       (menuPopupAtPointer, gestureGetLastEvent, onGestureBegin,
        gestureSingleSetButton, gestureMultiPressNew)

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

chooseDir :: Window -> Text -> Maybe FilePath -> IO (Maybe FilePath)
chooseDir window prompt mbFolder = do
    dialog <- new' FileChooserDialog [constructDialogUseHeaderBar 1]
    setWindowTitle dialog prompt
    windowSetTransientFor dialog $ Just window
    fileChooserSetAction dialog FileChooserActionSelectFolder
    _ <- dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
    _ <- dialogAddButton' dialog "gtk-open" ResponseTypeAccept
    mapM_ (fileChooserSetCurrentFolder dialog) mbFolder
    widgetShow dialog
    response <- dialogRun' dialog
    case response of
        ResponseTypeAccept -> do
            fn <- fileChooserGetFilename dialog
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
    _ <- dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
    _ <- dialogAddButton' dialog "gtk-open" ResponseTypeAccept
    mapM_ (fileChooserSetCurrentFolder dialog) mbFolder
    forM_ filters (addFilter dialog)
    widgetShow dialog
    response <- dialogRun' dialog
    case response of
        ResponseTypeAccept -> do
            fn <- fileChooserGetFilename dialog
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
    _ <- dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
    _ <- dialogAddButton' dialog "gtk-open" ResponseTypeAccept
    mapM_ (fileChooserSetCurrentFolder dialog) mbFolder
    widgetShow dialog
    res <- dialogRun' dialog
    case res of
        ResponseTypeAccept  ->  do
            fileName <- fileChooserGetFilename dialog
            widgetDestroy dialog
            return fileName
        _               ->  do
            widgetDestroy dialog
            return Nothing



openBrowser :: Text -> IDEAction
openBrowser url = do
    prefs' <- readIDE prefs
    liftIO (E.catch (
                void $ runProcess (T.unpack $ browser prefs') [T.unpack url] Nothing Nothing Nothing Nothing Nothing)
            (\ (_ :: SomeException) -> sysMessage Normal ("Can't find browser executable " <> browser prefs')))
    return ()

-- | Show a text dialog with an Ok button and a specific messagetype
showDialog :: MonadIO m => Maybe Window -> Text -> MessageType -> m ()
showDialog parent msg msgType =
    void $ showDialogAndGetResult parent msg msgType ResponseTypeOk [
            constructDialogUseHeaderBar 0,
            constructMessageDialogMessageType msgType,
            constructMessageDialogButtons ButtonsTypeOk,
            constructMessageDialogText msg
        ] [] []

-- | Show a Yes/No dialog
showYesNoDialog :: MonadIO m
                => Maybe Window -- ^ Parent of transient
                -> Bool         -- ^ Should Yes be selected by default?
                -> Text         -- ^ Message
                -> m Bool       -- ^ Was "Yes" chosen as user response?
showYesNoDialog parent defaultYes message =
    isYes <$> showDialogAndGetResponse parent message MessageTypeQuestion defaultResponse
            [ constructDialogUseHeaderBar 0, constructMessageDialogButtons ButtonsTypeYesNo ]
            []
  where
    defaultResponse = if defaultYes then ResponseTypeYes else ResponseTypeNo
    isYes = (== ResponseTypeYes)


-- | Show a confirmation dialog with a cancel button and a custom action
-- button
showConfirmDialog :: MonadIO m
                   => Maybe Window  -- ^ Parent of transient
                   -> Bool          -- ^ Should the custom action be selected by default?
                   -> Text          -- ^ Label for custom action button
                   -> Text          -- ^ Message
                   -> m Bool        -- ^ Was the custom action chosen as user response?
showConfirmDialog parent defaultConfirm action message =
    isConfirm <$> showDialogAndGetResponse parent message MessageTypeQuestion defaultResponse
            [ constructDialogUseHeaderBar 0 , constructMessageDialogButtons ButtonsTypeCancel ]
            [ (action, AnotherResponseType 1) ]
  where
    defaultResponse = if defaultConfirm then AnotherResponseType 1 else ResponseTypeCancel
    isConfirm = (== AnotherResponseType 1)

-- | Show an error dialog with an Ok button
showErrorDialog :: MonadIO m => Maybe Window -> Text -> m ()
showErrorDialog parent msg = showDialog parent msg MessageTypeError


-- | Show a dialog with custom buttons and callbacks
showDialogOptions :: MonadIO m
                  => Maybe Window     -- ^ Parent window to use with `windowSetTransientFor`
                  -> Text             -- ^ the message
                  -> MessageType      -- ^ type of dialog
                  -> [(Text, m ())]   -- ^ button text and corresponding actions
                  -> Maybe Int        -- ^ index of button that has default focus (0-based)
                  -> m ()
showDialogOptions parent msg msgType buttons mbIndex =
    void $ showDialogAndGetResult parent msg msgType (AnotherResponseType $ fromMaybe 0 mbIndex) [
                constructDialogUseHeaderBar 0,
                constructMessageDialogMessageType msgType,
                constructMessageDialogButtons ButtonsTypeNone,
                constructMessageDialogText msg
            ]
            (zip (map fst buttons) responseTypes)
            (zip responseTypes (map (const . snd) buttons))
  where
    responseTypes = (map AnotherResponseType [0..])

-- | Show a dialog with custom buttons, and get back the user response
showDialogAndGetResponse
        :: MonadIO m
        => Maybe Window   -- ^ Parent window to use with `windowSetTransientFor`
        -> Text           -- ^ The message
        -> MessageType    -- ^ The message dialog type
        -> ResponseType   -- ^ The response which is selected by default
        -> [IO (GValueConstruct MessageDialog)]   -- ^ Options to `new'` the window with
        -> [(Text, ResponseType)]     -- ^ List of buttons and their associated ResponseTypes
        -> m ResponseType -- ^ The response type selected by the user
showDialogAndGetResponse parent msg msgType defaultResponse newOptions buttons = do
    either id id <$>
        showDialogAndGetResult parent msg msgType
            defaultResponse newOptions buttons (actions buttons)
  where
    actions = map $ \(_, response) -> (response, const $ return response)

-- | Show a dialog with custom buttons, and perform an action based on the
-- user's response.
--
-- Note that, although the MessageDialog is passed to the actions, it will
-- not survive past the end of the call to `showDialogAndGetResult`, so
-- you should not try to store it somewhere for later use
showDialogAndGetResult
        :: MonadIO m
        => Maybe Window             -- ^ Parent window to use with `windowSetTransientFor`
        -> Text                     -- ^ The message
        -> MessageType              -- ^ The message dialog type
        -> ResponseType             -- ^ The response which is selected by default
        -> [IO (GValueConstruct MessageDialog)]   -- ^ Options to `new'` the window with
        -> [(Text, ResponseType)]   -- ^ List of buttons and their associated responseTypes
        -> [(ResponseType, MessageDialog -> m a)] -- ^ List of response types and their associated actions
        -> m (Either ResponseType a) -- ^ `Right` the result of the action selected by the user,
                                     -- or `Left ResponseType` if the user's response did not match the provided ones
showDialogAndGetResult parent msg msgType defaultResponse newOptions buttons responseActions = do
    dialog <- new' MessageDialog newOptions
    setMessageDialogMessageType dialog msgType
    setMessageDialogText dialog msg
    windowSetTransientFor dialog parent
    mapM_ (addActionButton dialog) buttons
    dialogSetDefaultResponse' dialog defaultResponse
    setWindowWindowPosition dialog WindowPositionCenterOnParent

    response <- dialogRun' dialog
    -- Keep the dialog alive until the caller's action has finished messing with it
    widgetHide dialog

    let action = lookup response responseActions
    result <- case action of
                 Nothing -> return $ Left response
                 Just f -> Right <$> f dialog

    widgetDestroy dialog
    return result
  where
    addActionButton :: MonadIO m => MessageDialog -> (Text, ResponseType) -> m ()
    addActionButton dialog (text, responseType) =
            dialogAddButton' dialog text responseType >> return ()

-- | Show a simple dialog that asks the user for some text
showInputDialog :: Maybe Window -- ^ Parent window to use with `windowSetTransientFor`
                -> Text -- ^ The message text
                -> Text -- ^ The default value
                -> IO (Maybe Text)
showInputDialog parent msg def = do
    dialog <- new' Dialog [constructDialogUseHeaderBar 1] -- Nothing [] MessageQuestion ButtonsOkCancel msg
    windowSetTransientFor dialog parent
    vbox   <- dialogGetContentArea dialog >>= unsafeCastTo Box
    label  <- labelNew (Just msg)
    entry  <- entryNew
    setEntryText entry def
    boxPackStart' vbox label PackNatural 0
    boxPackStart' vbox entry PackNatural 0
    widgetShowAll vbox

    -- Can't use messageDialog because of https://github.com/gtk2hs/gtk2hs/issues/114
    _ <- dialogAddButton' dialog "Cancel" ResponseTypeCancel
    okBtn <- dialogAddButton' dialog "Ok" ResponseTypeOk
    _ <- onEntryActivate entry $ void $ widgetActivate okBtn
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

getMenuItem :: HasCallStack => Text -> IDEM MenuItem
getMenuItem path = (do
    uiManager' <- getUiManager
    mMenuItem <- uIManagerGetWidget uiManager' path
    case mMenuItem of
      Nothing -> throwIDE ("State.hs>>getMenuItem: Can't find ui path " <> path)
      Just item -> liftIO $ unsafeCastTo MenuItem item)
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

getNativeToggled :: PaneMonad alpha => alpha Bool
getNativeToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/Native" (unsafeCastTo ToggleAction)
    toggleActionGetActive ui

setNativeToggled :: PaneMonad alpha => Bool -> alpha ()
setNativeToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/Native" (unsafeCastTo ToggleAction)
    toggleActionSetActive ui b

getJavaScriptToggled :: PaneMonad alpha => alpha Bool
getJavaScriptToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/JavaScript" (unsafeCastTo ToggleAction)
    toggleActionGetActive ui

setJavaScriptToggled :: PaneMonad alpha => Bool -> alpha ()
setJavaScriptToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/JavaScript" (unsafeCastTo ToggleAction)
    toggleActionSetActive ui b

getDebugToggled :: PaneMonad alpha => alpha Bool
getDebugToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/Debug" (unsafeCastTo ToggleAction)
    toggleActionGetActive ui

setDebugToggled :: PaneMonad alpha => Bool -> alpha ()
setDebugToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/Debug" (unsafeCastTo ToggleAction)
    toggleActionSetActive ui b

getRecentFiles , getRecentWorkspaces, getVCS :: HasCallStack => IDEM MenuItem
getRecentFiles    = getMenuItem "ui/menubar/_File/Recent Files"


getRecentWorkspaces = getMenuItem "ui/menubar/_File/Recent Workspaces"
getVCS = getMenuItem "ui/menubar/Version _Control" --this could fail, try returning Menu if it does
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
treeViewToggleRow :: (MonadIO m, IsTreeView treeView) => treeView -> TreePath -> m Bool
treeViewToggleRow treeView path = do
    expanded <- treeViewRowExpanded treeView path
    if expanded
        then treeViewCollapseRow treeView path
        else treeViewExpandRow   treeView path False

-- maps control key for Macos
mapControlCommand :: ModifierType -> ModifierType
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
    g <- gestureMultiPressNew view
    gestureSingleSetButton g (fromIntegral BUTTON_SECONDARY)
    cid2 <- onGestureBegin g $ \s ->
      gestureGetLastEvent g (Just s) >>= mapM_ (\event -> do
        eventButton <- getEventButton event
        x         <- getEventButtonX eventButton
        y         <- getEventButtonY eventButton
        sel <- treeViewGetSelection view
        selCount <- treeSelectionCountSelectedRows sel
        when (selCount <= 1) $ do
            pathInfo <- treeViewGetPathAtPos view (floor x) (floor y)
            case pathInfo of
                (True, Just path, _, _, _) -> do
                    treeSelectionUnselectAll sel
                    treeSelectionSelectPath sel path
                _ -> return ()
        showMenu event ideRef)
    return $ map (ConnectC view) [cid2]
  where
    showMenu :: Event -> IDERef -> IO ()
    showMenu event ideRef = do

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
                widgetShowAll theMenu
                menuPopupAtPointer theMenu (Just event)

treeViewContextMenu :: MonadIO m
                    => TreeView
                    -> (Menu -> IO ())
                    -> m [Connection]
treeViewContextMenu treeView populateMenu = do
    theMenu <- menuNew
    menuAttachToWidget theMenu treeView Nothing
    liftIO $ populateMenu theMenu
    g <- gestureMultiPressNew treeView
    gestureSingleSetButton g (fromIntegral BUTTON_SECONDARY)
    cid2 <- onGestureBegin g $ \s ->
      gestureGetLastEvent g (Just s) >>= mapM_ (\event -> do
        eventButton <- getEventButton event
        x         <- getEventButtonX eventButton
        y         <- getEventButtonY eventButton
        sel <- treeViewGetSelection treeView
        selCount <- treeSelectionCountSelectedRows sel
        when (selCount <= 1) $ do
            pathInfo <- treeViewGetPathAtPos treeView (floor x) (floor y)
            case pathInfo of
                (True, Just path, _, _, _) -> do
                    treeSelectionUnselectAll sel
                    treeSelectionSelectPath sel path
                _ -> return ()
        showMenu event theMenu)
    return $ map (ConnectC treeView) [cid2]
  where
    showMenu event theMenu = do
        widgetShowAll theMenu
        menuPopupAtPointer theMenu (Just event)

fontDescription :: Maybe Text -> IDEM FontDescription
fontDescription mbFontString =
    case mbFontString of
        Just str ->
            fontDescriptionFromString str
        Nothing -> do
            f <- fontDescriptionNew
            fontDescriptionSetFamily f "Monospace"
            return f

