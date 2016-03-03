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
,   showDialogOptions
,   showInputDialog

,   getFullScreenState
,   setFullScreenState
,   getDarkState
,   setDarkState

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
,   treeViewToggleRow
,   treeViewContextMenu
,   treeViewContextMenu'
,   treeStoreGetForest

,   __

,   fontDescription
) where

import Graphics.UI.Gtk
import IDE.Utils.Tool (runProcess)
import Data.Maybe
       (listToMaybe, fromMaybe, catMaybes, fromJust, isJust)
import Control.Monad (void, when, unless)
import IDE.Core.State
--import Graphics.UI.Gtk.Selectors.FileChooser
--    (FileChooserAction(..))
--import Graphics.UI.Gtk.General.Structs
--    (ResponseId(..))
import Control.Monad.IO.Class (liftIO)
import Control.Exception as E
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack
#ifdef LOCALIZATION
                               , pack
#endif
                                )
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.Foldable (forM_)
import Data.Tree (Tree(..), Forest)

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

-- | Launch a "choose file" dialog
chooseFile :: Window
           -> Text                   -- ^ Window title
           -> Maybe FilePath         -- ^ Start location
           -> [(String, [String])]   -- ^ File filters, e.g. [("Music Files", ["*.mp3", "*.wav"])]
           -> IO (Maybe FilePath)
chooseFile window prompt mbFolder filters = do
    dialog <- fileChooserDialogNew
                    (Just prompt)
                    (Just window)
                FileChooserActionOpen
                [("gtk-cancel"
                ,ResponseCancel)
                ,("gtk-open"
                ,ResponseAccept)]
    forM_ mbFolder $ \folder ->
        void (fileChooserSetCurrentFolder dialog folder)
    forM_ filters (addFilter dialog)
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

    where
        addFilter dialog (description, exts) = do
            ff <- fileFilterNew
            fileFilterSetName ff description
            forM_ exts (fileFilterAddPattern ff)
            fileChooserAddFilter dialog ff

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

-- | Show a text dialog with an Ok button and a specific messagetype
showDialog :: Text -> MessageType -> IO ()
showDialog msg msgType = do
    dialog <- messageDialogNew Nothing [] msgType ButtonsOk msg
    _ <- dialogRun dialog
    widgetDestroy dialog
    return ()


-- | Show an error dialog with an Ok button
showErrorDialog :: Text -> IO ()
showErrorDialog msg = showDialog msg MessageError


-- | Show a dialog with custom buttons and callbacks
showDialogOptions :: Text             -- ^ the message
                  -> MessageType      -- ^ type of dialog
                  -> [(Text, IO ())]  -- ^ button text and corresponding actions
                  -> Maybe Int        -- ^ index of button that has default focus (0-based)
                  -> IO ()
showDialogOptions msg msgType buttons mbIndex = do
    dialog <- messageDialogNew Nothing [] msgType ButtonsNone msg

    forM_ (zip [0..] buttons) $ \(n,(text, _)) -> do
        dialogAddButton dialog text (ResponseUser n)

    dialogSetDefaultResponse dialog (ResponseUser (fromMaybe 0 mbIndex))
    set dialog [ windowWindowPosition := WinPosCenterOnParent ]
    res <- dialogRun dialog
    widgetHide dialog
    case res of
        ResponseUser n | n >= 0 && n < length buttons -> map snd buttons !! n
        _ -> return ()


-- | Show a simple dialog that asks the user for some text
showInputDialog :: Text -- ^ The message text
                -> Text -- ^ The default value
                -> IO (Maybe Text)
showInputDialog msg def = do
    dialog <- dialogNew -- Nothing [] MessageQuestion ButtonsOkCancel msg
    vbox   <- castToBox <$> dialogGetContentArea dialog
    label <- labelNew (Just msg)
    entry <- entryNew
    set entry [entryText := def]
    boxPackStart vbox label PackNatural 0
    boxPackStart vbox entry PackNatural 0
    widgetShowAll vbox

    -- Can't use messageDialog because of https://github.com/gtk2hs/gtk2hs/issues/114
    dialogAddButton dialog ("Cancel" :: Text) ResponseCancel
    dialogAddButton dialog ("Ok" :: Text) ResponseOk
    dialogSetDefaultResponse dialog ResponseOk

    res <- dialogRun dialog
    widgetHide dialog

    case res of
        ResponseOk -> do
            text <- get entry entryText
            widgetDestroy dialog
            return (Just text)
        _ -> widgetDestroy dialog >> return Nothing




-- get widget elements (menu & toolbar)


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
    ui <- getUIAction "ui/menubar/_View/_Use Dark Interface" castToToggleAction
    liftIO $toggleActionGetActive ui

setDarkState :: PaneMonad alpha => Bool -> alpha ()
setDarkState b = do
    ui <- getUIAction "ui/menubar/_View/_Use Dark Interface" castToToggleAction
    liftIO $toggleActionSetActive ui b

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
getRecentFiles    = getMenuItem "ui/menubar/_File/Recent Files"


getRecentWorkspaces = getMenuItem "ui/menubar/_File/Recent Workspaces"
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
stockIdFromType PatternSynonym  =   "ide_konstructor"
stockIdFromType _               =   "ide_other"


treeStoreGetForest :: TreeStore a -> IO (Forest a)
treeStoreGetForest store = subForest <$> (treeStoreGetTree store [])

-- | Toggles a row in a `TreeView`
treeViewToggleRow treeView path = do
    expanded <- treeViewRowExpanded treeView path
    if expanded
        then treeViewCollapseRow treeView path
        else treeViewExpandRow   treeView path False

-- maps control key for Macos
#if defined(darwin_HOST_OS)
mapControlCommand Alt = Control
#endif
mapControlCommand a = a

-- | Sets the context menu for a treeView widget
treeViewContextMenu' :: TreeViewClass treeView
                     => treeView                 -- ^ The view
                     -> TreeStore a              -- ^ The model
                     -> (a -> TreePath -> TreeStore a -> IDEM [[(Text, IDEAction)]]) -- ^ Produces the menu items for the selected values when right clicking
                                                                                     -- The lists are seperated by a seperator
                     -> IDEM (ConnectId treeView, ConnectId treeView)
treeViewContextMenu' view store itemsFor = reifyIDE $ \ideRef -> do
    cid1 <- view `on` popupMenuSignal $ do
        showMenu Nothing ideRef
    cid2 <- view `on` buttonPressEvent $ do
        button    <- eventButton
        click     <- eventClick
        timestamp <- eventTime
        (x, y)    <- eventCoordinates
        case (button, click) of
            (RightButton, SingleClick) -> liftIO $ do
                sel <- treeViewGetSelection view
                selCount <- treeSelectionCountSelectedRows sel
                when (selCount <= 1) $ do
                    pathInfo <- treeViewGetPathAtPos view (floor x, floor y)
                    case pathInfo of
                        Just (path, _, _) -> do
                            treeSelectionUnselectAll sel
                            treeSelectionSelectPath sel path
                        _ -> return ()
                showMenu (Just (button, timestamp)) ideRef
            _ -> return False
    return (cid1, cid2)
  where
    showMenu buttonEventDetails ideRef = do

        selPaths  <- treeViewGetSelection view >>= treeSelectionGetSelectedRows
        selValues <- mapM (treeStoreGetValue store) selPaths
        theMenu   <- menuNew
        menuAttachToWidget theMenu view
        forM_ (listToMaybe $ zip selValues selPaths) $ \(val, path) -> do
            itemsPerSection     <- flip reflectIDE ideRef $ itemsFor val path store
            menuItemsPerSection <- mapM (mapM (liftIO . menuItemNewWithLabel . fst)) itemsPerSection


            forM_ (zip itemsPerSection menuItemsPerSection) $ \(section, itemsSection) -> do
                forM_ (zip section itemsSection) $ \((_, onActivated), m) -> do
                    m `on` menuItemActivated $ reflectIDE onActivated ideRef

            unless (null itemsPerSection) $ do
                itemsAndSeparators <- sequence $
                    intercalate [fmap castToMenuItem separatorMenuItemNew]
                                (map (map (return . castToMenuItem)) menuItemsPerSection)
                mapM_ (menuShellAppend theMenu) itemsAndSeparators
                menuPopup theMenu buttonEventDetails
                widgetShowAll theMenu
        return True

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

