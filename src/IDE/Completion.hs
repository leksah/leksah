{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Completion
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  <maintainer@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Completion (complete, cancel, setCompletionSize) where

import Prelude hiding(getChar, getLine)

import Data.List as List (stripPrefix, isPrefixOf, filter)
import Data.Char
import Data.IORef
import Control.Monad
import IDE.Core.State
import IDE.Metainfo.Provider(getDescription,getCompletionOptions)
import IDE.TextEditor as TE
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ask)
import qualified Control.Monad.Reader as Gtk (liftIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Data.Text as T
       (empty, commonPrefixes, pack, unpack, null, stripPrefix,
        isPrefixOf)
import System.Log.Logger (debugM)
import GI.Gtk.Objects.Window
       (windowMove, windowGetScreen, windowGetSize, Window(..),
        windowNew, setWindowTransientFor, setWindowDefaultHeight,
        setWindowDefaultWidth, setWindowResizable, setWindowDecorated,
        setWindowTypeHint, windowResize)
import Data.GI.Base
       (unsafeManagedPtrGetPtr, unsafeCastTo, get, set, nullToNothing)
import GI.Gdk.Enums (GrabStatus(..), WindowTypeHint(..))
import GI.Gtk.Objects.Container
       (containerRemove, containerAdd, containerSetBorderWidth)
import GI.Gtk.Objects.HPaned (hPanedNew)
import GI.Gtk.Objects.ScrolledWindow (scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Widget
       (Widget(..), widgetShowAll, widgetGetAllocation, widgetGetParent,
        widgetHide, onWidgetButtonReleaseEvent, onWidgetMotionNotifyEvent,
        widgetGetWindow, onWidgetButtonPressEvent, getWidgetVisible,
        widgetModifyFont, widgetSetSizeRequest)
import GI.Gtk.Objects.TreeView
       (treeViewSetCursor, onTreeViewRowActivated, treeViewRowActivated,
        treeViewScrollToCell, treeViewGetColumn, treeViewGetModel,
        TreeView(..), treeViewGetSelection, setTreeViewHeadersVisible,
        treeViewAppendColumn, treeViewSetModel, treeViewNew)
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreAppend, seqStoreClear, seqStoreGetValue, SeqStore(..),
        seqStoreNew)
import GI.Pango.Structs.FontDescription
       (fontDescriptionSetFamily, fontDescriptionNew,
        fontDescriptionFromString)
import GI.Gtk.Objects.TreeViewColumn
       (noTreeViewColumn, treeViewColumnPackStart, setTreeViewColumnMinWidth,
        setTreeViewColumnSizing, treeViewColumnNew)
import GI.Gtk.Enums (TreeViewColumnSizing(..), WindowType(..))
import GI.Gtk.Objects.CellRendererText
       (setCellRendererTextText, cellRendererTextNew)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction)
import GI.Gtk.Objects.TreeSelection
       (treeSelectionGetSelected, treeSelectionSelectPath,
        treeSelectionSelectedForeach,
        onTreeSelectionChanged)
import GI.Gtk.Objects.Paned
       (panedSetPosition, panedGetPosition, panedGetChild2, Paned(..),
        panedGetChild1, panedAdd2, panedAdd1)
import GI.Gdk.Structs.EventKey
       (getEventKeyKeyval, getEventKeyState)
import GI.Gdk.Functions
       (pointerUngrab, pointerGrab, keyvalToUnicode, keyvalName)
import GI.Gtk.Interfaces.TreeModel
       (treeModelGetPath, treeModelIterNChildren)
import GI.Gdk.Structs.EventButton
       (getEventButtonTime, getEventButtonY, getEventButtonX,
        getEventButtonButton)
import GI.Gdk.Flags (EventMask(..))
import GI.Gdk.Objects.Cursor (noCursor)
import GI.Gdk.Structs.EventMotion
       (getEventMotionY, getEventMotionX)
import GI.Gtk.Structs.TreePath (treePathGetIndices, TreePath(..))
import Graphics.UI.Frame.Rectangle
       (getRectangleHeight, getRectangleWidth, getRectangleY,
        getRectangleX, Rectangle(..))
import GI.Gdk.Objects.Window (windowGetOrigin)
import qualified GI.Gdk.Objects.Window as Gdk (noWindow)
import GI.Gdk.Objects.Screen
       (screenGetHeight, screenGetWidth, screenGetMonitorAtPoint)
import Data.GI.Gtk.ModelView.Types
       (treePathGetIndices', treePathNewFromIndices')
import Data.Maybe (fromJust)
import Data.Monoid ((<>))

complete :: TextEditor editor => EditorView editor -> Bool -> IDEAction
complete sourceView always = do
    currentState'    <- readIDE currentState
    prefs'           <- readIDE prefs
    (_, completion') <- readIDE completion
    case (currentState',completion') of
        (IsCompleting c, Just (CompletionWindow window tv st)) -> do
                            isWordChar <- getIsWordChar sourceView
                            updateOptions window tv st sourceView c isWordChar always
        (IsRunning,_)   ->  when (always || not (completeRestricted prefs'))
                                (initCompletion sourceView always)
        _               ->  return ()

cancel :: IDEAction
cancel = do
    currentState'    <- readIDE currentState
    (_, completion') <- readIDE completion
    case (currentState',completion') of
        (IsCompleting conn , Just (CompletionWindow window tv st)) ->
            cancelCompletion window tv st conn
        _            -> return ()

setCompletionSize :: Int -> Int -> IDEAction
setCompletionSize x y | x > 10 && y > 10 = do
    (_, completion) <- readIDE completion
    case completion of
        Just (CompletionWindow window _ _) -> windowResize window (fromIntegral x) (fromIntegral y)
        Nothing                            -> return ()
    modifyIDE_ $ \ide -> ide{completion = ((x, y), completion)}
setCompletionSize _ _ = return ()

getIsWordChar :: forall editor. TextEditor editor => EditorView editor -> IDEM (Char -> Bool)
getIsWordChar sourceView = do
    ideR <- ask
    buffer <- getBuffer sourceView
    (_, end) <- getSelectionBounds buffer
    sol <- backwardToLineStartC end
    eol <- forwardToLineEndC end
    line <- getSlice buffer sol eol False

    let isImport = "import " `T.isPrefixOf` line
        isIdent a = isAlphaNum a || a == '\'' || a == '_'  || (isImport && a == '.')
        isOp    a = isSymbol   a || a == ':'  || a == '\\' || a == '*' || a == '/' || a == '-'
                                 || a == '!'  || a == '@'  || a == '%' || a == '&' || a == '?'
    prev <- backwardCharC end
    prevChar <- getChar prev
    case prevChar of
        Just prevChar | isIdent prevChar -> return isIdent
        Just prevChar | isOp    prevChar -> return isOp
        _                                -> return $ const False

initCompletion :: forall editor. TextEditor editor => EditorView editor -> Bool -> IDEAction
initCompletion sourceView always = do
    ideR <- ask
    ((width, height), completion') <- readIDE completion
    isWordChar <- getIsWordChar sourceView
    case completion' of
        Just (CompletionWindow window' tree' store') -> do
            cids <- addEventHandling window' sourceView tree' store' isWordChar always
            modifyIDE_ (\ide -> ide{currentState = IsCompleting cids})
            updateOptions window' tree' store' sourceView cids isWordChar always
        Nothing -> do
            windows    <- getWindows
            prefs      <- readIDE prefs
            window     <- windowNew WindowTypePopup
            setWindowTypeHint      window WindowTypeHintUtility
            setWindowDecorated     window False
            setWindowResizable     window True
            setWindowDefaultWidth  window $ fromIntegral width
            setWindowDefaultHeight window $ fromIntegral height
            setWindowTransientFor  window $ head windows
            containerSetBorderWidth window 3
            paned      <- hPanedNew
            containerAdd window paned
            nameScrolledWindow <- scrolledWindowNew noAdjustment noAdjustment
            widgetSetSizeRequest nameScrolledWindow 250 40
            tree       <- treeViewNew
            containerAdd nameScrolledWindow tree
            store      <- seqStoreNew []
            treeViewSetModel tree (Just store)

            font <- case textviewFont prefs of
                Just str ->
                    fontDescriptionFromString str
                Nothing -> do
                    f <- fontDescriptionNew
                    fontDescriptionSetFamily f "Monospace"
                    return f
            widgetModifyFont tree (Just font)

            column   <- treeViewColumnNew
            setTreeViewColumnSizing   column TreeViewColumnSizingFixed
            setTreeViewColumnMinWidth column 800 -- OSX does not like it if there is no hscroll
            treeViewAppendColumn tree column
            renderer <- cellRendererTextNew
            treeViewColumnPackStart column renderer True
            cellLayoutSetDataFunction column renderer store $ setCellRendererTextText renderer

            setTreeViewHeadersVisible tree False

            descriptionBuffer <- newDefaultBuffer Nothing ""
            descriptionView   <- newView descriptionBuffer (textviewFont prefs)
            updateStyle descriptionBuffer
            descriptionScrolledWindow <- getScrolledWindow descriptionView

            visible    <- liftIO $ newIORef False
            activeView <- liftIO $ newIORef Nothing

            treeSelection <- treeViewGetSelection tree

            onTreeSelectionChanged treeSelection $
                treeSelectionSelectedForeach treeSelection $ \_model treePath _iter ->
                    reflectIDE (withWord store treePath (\name -> do
                            description <- getDescription name
                            setText descriptionBuffer description
                            )) ideR

            panedAdd1 paned nameScrolledWindow
            panedAdd2 paned descriptionScrolledWindow

            cids <- addEventHandling window sourceView tree store isWordChar always

            modifyIDE_ (\ide -> ide{currentState = IsCompleting cids,
                completion = ((width, height), Just (CompletionWindow window tree store))})
            updateOptions window tree store sourceView cids isWordChar always

addEventHandling :: TextEditor editor => Window -> EditorView editor -> TreeView -> SeqStore Text
                 -> (Char -> Bool) -> Bool -> IDEM Connections
addEventHandling window sourceView tree store isWordChar always = do
    ideR      <- ask
    cidsPress <- TE.onKeyPress sourceView $ do
        e           <- lift ask
        keyVal      <- getEventKeyKeyval e
        name        <- keyvalName keyVal
        modifier    <- getEventKeyState e
        char        <- toEnum . fromIntegral <$> keyvalToUnicode keyVal
        Just model  <- treeViewGetModel tree
        selection   <- treeViewGetSelection tree
        count       <- treeModelIterNChildren model Nothing
        Just column <- nullToNothing $ treeViewGetColumn tree 0
        let whenVisible f = getWidgetVisible tree >>= \case
                                True  -> f
                                False -> return False
            down = whenVisible $ do
                maybeRow <- liftIO $ getRow tree
                let newRow = maybe 0 (+ 1) maybeRow
                when (newRow < count) $ do
                    path <- treePathNewFromIndices' [newRow]
                    treeSelectionSelectPath selection path
                    treeViewScrollToCell tree (Just path) noTreeViewColumn False 0 0
                return True
            up = whenVisible $ do
                maybeRow <- liftIO $ getRow tree
                let newRow = maybe 0 (\ row -> row - 1) maybeRow
                when (newRow >= 0) $ do
                    path <- treePathNewFromIndices' [newRow]
                    treeSelectionSelectPath selection path
                    treeViewScrollToCell tree (Just path) noTreeViewColumn False 0 0
                return True
        case (name, modifier, char) of
            (Just "Tab", _, _) -> whenVisible . liftIDE $ do
                tryToUpdateOptions window tree store sourceView True isWordChar always
                return True
            (Just "Return", _, _) -> whenVisible $ do
                maybeRow <- liftIO $ getRow tree
                case maybeRow of
                    Just row -> do
                        path <- treePathNewFromIndices' [row]
                        liftIDE $ withWord store path (replaceWordStart sourceView isWordChar True)
                        liftIDE $ postAsyncIDE cancel
                        return True
                    Nothing -> do
                        liftIDE cancel
                        return False
            (Just "Down", _, _) -> down
            (Just "Up", _, _) -> up
            (Just super, _, 'a') | super `elem` ["Super_L", "Super_R"] -> do
                liftIO $ debugM "leksah" "Completion - Super 'a' key press"
                down
            (Just super, _, 'l') | super `elem` ["Super_L", "Super_R"] -> do
                liftIO $ debugM "leksah" "Completion - Super 'l' key press"
                up
            (_, _, c) | isWordChar c -> return False
            (Just "BackSpace", _, _) -> return False
            (Just key, _, _) | key `elem` ["Shift_L", "Shift_R", "Super_L", "Super_R"] -> return False
            _ -> do liftIDE cancel
                    return False

    cidsRelease <- TE.onKeyRelease sourceView $ do
        e        <- lift ask
        name     <- getEventKeyKeyval e >>= keyvalName
        modifier <- getEventKeyState e
        case (name, modifier) of
            (Just "BackSpace", _) -> do
                liftIDE $ complete sourceView False
                return False
            _ -> return False

    resizeHandler <- liftIO $ newIORef Nothing

    idButtonPress <- ConnectC window <$> onWidgetButtonPressEvent window (\e -> do
        button     <- getEventButtonButton e
        x          <- getEventButtonX e
        y          <- getEventButtonY e
        time       <- getEventButtonTime e

        nullToNothing (widgetGetWindow window) >>= \case
            Nothing -> return ()
            Just drawWindow -> do
                status <- pointerGrab
                    drawWindow
                    False
                    [EventMaskPointerMotionMask, EventMaskButtonReleaseMask]
                    Gdk.noWindow
                    noCursor
                    time
                when (status == GrabStatusSuccess) $ do
                    (width, height) <- windowGetSize window
                    liftIO $ writeIORef resizeHandler $ Just $ \newX newY ->
                        reflectIDE (
                            setCompletionSize (fromIntegral width + floor (newX - x)) (fromIntegral height + floor (newY - y))) ideR

        return True)

    idMotion <- ConnectC window <$> onWidgetMotionNotifyEvent window (\e -> do
        mbResize <- readIORef resizeHandler
        case mbResize of
            Just resize -> do
                x <- getEventMotionX e
                y <- getEventMotionY e
                resize x y
                return True
            Nothing     -> return False)

    idButtonRelease <- ConnectC window <$> onWidgetButtonReleaseEvent window (\e -> do
        mbResize <- liftIO $ readIORef resizeHandler
        case mbResize of
            Just resize -> do
                x <- getEventButtonX e
                y <- getEventButtonY e
                resize x y
                getEventButtonTime e >>= pointerUngrab
                liftIO $ writeIORef resizeHandler Nothing
                return True
            Nothing     -> return False)

    idSelected <- ConnectC tree <$> onTreeViewRowActivated tree (\treePath column -> (`reflectIDE` ideR) $ do
        withWord store treePath (replaceWordStart sourceView isWordChar False)
        postAsyncIDE cancel)

    return $ concat [cidsPress, cidsRelease, [idButtonPress, idMotion, idButtonRelease, idSelected]]

withWord :: SeqStore Text -> TreePath -> (Text -> IDEM ()) -> IDEM ()
withWord store treePath f =
   treePathGetIndices' treePath >>= \case
       [row] -> do
            value <- seqStoreGetValue store row
            f value
       _ -> return ()

replaceWordStart :: TextEditor editor => EditorView editor -> (Char -> Bool) -> Bool -> Text -> IDEM ()
replaceWordStart sourceView isWordChar returnPressed name = do
    buffer <- getBuffer sourceView
    (selStart, selEnd) <- getSelectionBounds buffer
    start <- findWordStart selStart isWordChar
    wordStart <- getText buffer start selEnd True
    case T.stripPrefix wordStart name of
        Just "" | returnPressed -> do
            -- Return key was pressed even though nothing needed to be done
            -- to make the current word match the selection.
            selectRange buffer selEnd selEnd
            insert buffer selEnd "\n"
        Just extra -> do
            end <- findWordEnd selEnd isWordChar
            wordFinish <- getText buffer selEnd end True
            case T.stripPrefix wordFinish extra of
                Just extra2 | not (T.null wordFinish) -> do
                    selectRange buffer end end
                    insert buffer end extra2
                _ -> insert buffer selEnd extra
        Nothing -> return ()

cancelCompletion :: Window -> TreeView -> SeqStore Text -> Connections -> IDEAction
cancelCompletion window tree store connections = do
    seqStoreClear (store :: SeqStore Text)
    signalDisconnectAll connections
    widgetHide window
    modifyIDE_ (\ide -> ide{currentState = IsRunning})

updateOptions :: forall editor. TextEditor editor => Window -> TreeView -> SeqStore Text -> EditorView editor -> Connections -> (Char -> Bool) -> Bool -> IDEAction
updateOptions window tree store sourceView connections isWordChar always = do
    result <- tryToUpdateOptions window tree store sourceView False isWordChar always
    unless result $ cancelCompletion window tree store connections

tryToUpdateOptions :: TextEditor editor => Window -> TreeView -> SeqStore Text -> EditorView editor -> Bool -> (Char -> Bool) -> Bool -> IDEM Bool
tryToUpdateOptions window tree store sourceView selectLCP isWordChar always = do
    ideR <- ask
    seqStoreClear (store :: SeqStore Text)
    buffer <- getBuffer sourceView
    (selStart, end) <- getSelectionBounds buffer
    start <- findWordStart selStart isWordChar
    equal <- iterEqual start end
    if equal
        then return False
        else do
            wordStart <- getText buffer start end True
            liftIO $ do  -- dont use postGUIAsync - it causes bugs related to several repeated tryToUpdateOptions in thread
                reflectIDE (do
                    options <- getCompletionOptions wordStart
                    processResults window tree store sourceView wordStart options selectLCP isWordChar always) ideR
                return ()
            return True

findWordStart :: TextEditor editor => EditorIter editor -> (Char -> Bool) -> IDEM (EditorIter editor)
findWordStart iter isWordChar = do
    maybeWS <- backwardFindCharC iter (not . isWordChar) Nothing
    case maybeWS of
        Nothing -> atOffset iter 0
        Just ws -> forwardCharC ws

findWordEnd :: TextEditor editor => EditorIter editor -> (Char -> Bool) -> IDEM (EditorIter editor)
findWordEnd iter isWordChar = do
    maybeWE <- forwardFindCharC iter (not . isWordChar) Nothing
    case maybeWE of
        Nothing -> forwardToLineEndC iter
        Just we -> return we

longestCommonPrefix a b = case T.commonPrefixes a b of
                            Nothing        -> T.empty
                            Just (p, _, _) -> p

processResults :: TextEditor editor => Window -> TreeView -> SeqStore Text -> EditorView editor -> Text -> [Text]
               -> Bool -> (Char -> Bool) -> Bool -> IDEAction
processResults window tree store sourceView wordStart options selectLCP isWordChar always =
    case options of
        [] -> cancel
        _ | not always && (not . null $ drop 200 options) -> cancel
        _ -> do
            buffer <- getBuffer sourceView
            (selStart, end) <- getSelectionBounds buffer
            start <- findWordStart selStart isWordChar
            currentWordStart <- getText buffer start end True
            let newWordStart = if selectLCP && currentWordStart == wordStart && not (null options)
                                    then foldl1 longestCommonPrefix options
                                    else currentWordStart

            when (T.isPrefixOf wordStart newWordStart) $ do
                seqStoreClear store
                let newOptions = List.filter (T.isPrefixOf newWordStart) options
                forM_ (take 200 newOptions) (seqStoreAppend store)
                rect                 <- getIterLocation sourceView start
                startx               <- getRectangleX rect
                starty               <- getRectangleY rect
                width                <- getRectangleWidth rect
                height               <- getRectangleHeight rect
                (wWindow, hWindow)   <- windowGetSize window
                (x, y)               <- bufferToWindowCoords sourceView (fromIntegral startx, fromIntegral (starty+height))
                mbDrawWindow         <- getWindow sourceView
                case mbDrawWindow of
                    Nothing -> return ()
                    Just drawWindow -> do
                        (_, ox, oy)  <- windowGetOrigin drawWindow
                        Just namesSW <- nullToNothing $ widgetGetParent tree
                        rNames       <- widgetGetAllocation namesSW
                        wNames       <- getRectangleWidth rNames
                        hNames       <- getRectangleHeight rNames
                        paned        <- nullToNothing (widgetGetParent namesSW) >>= liftIO . unsafeCastTo Paned . fromJust
                        Just first   <- nullToNothing $ panedGetChild1 paned
                        Just second  <- nullToNothing $ panedGetChild2 paned
                        screen       <- windowGetScreen window
                        monitor      <- screenGetMonitorAtPoint screen (ox+fromIntegral x) (oy+fromIntegral y)
                        monitorLeft  <- screenGetMonitorAtPoint screen (ox+fromIntegral x-wWindow+wNames) (oy+fromIntegral y)
                        monitorRight <- screenGetMonitorAtPoint screen (ox+fromIntegral x+wWindow) (oy+fromIntegral y)
                        monitorBelow <- screenGetMonitorAtPoint screen (ox+fromIntegral x) (oy+fromIntegral y+hWindow)
                        wScreen      <- screenGetWidth screen
                        hScreen      <- screenGetHeight screen
                        top <- if monitorBelow /= monitor || (oy+fromIntegral y+hWindow) > hScreen
                            then do
                                sourceSW <- getScrolledWindow sourceView
                                hSource <- widgetGetAllocation sourceSW >>= getRectangleHeight
                                scrollToIter sourceView end 0.1 (Just (1.0, 1.0 - (fromIntegral hWindow / fromIntegral hSource)))
                                (_, newy)     <- bufferToWindowCoords sourceView (fromIntegral startx, fromIntegral (starty+height))
                                return (oy+fromIntegral newy)
                            else return (oy+fromIntegral y)
                        liftIO $ debugM "leksah" $ "Completion processResults " <> show (monitorRight /= monitor, monitorLeft /= monitor, ox, x, wWindow, wScreen, wNames)
                        swap <- if (monitorRight /= monitor || (ox+fromIntegral x+wWindow) > wScreen) && monitorLeft == monitor && (ox+fromIntegral x-wWindow+wNames) > 0
                            then do
                                windowMove window (ox+fromIntegral x-wWindow+wNames) top
                                return $ unsafeManagedPtrGetPtr first == unsafeManagedPtrGetPtr namesSW
                            else do
                                windowMove window (ox+fromIntegral x) top
                                return $ unsafeManagedPtrGetPtr first /= unsafeManagedPtrGetPtr namesSW
                        when swap $ do
                            pos <- panedGetPosition paned
                            containerRemove paned first
                            containerRemove paned second
                            panedAdd1 paned second
                            panedAdd2 paned first
                            panedSetPosition paned (wWindow-pos)
                        unless (null newOptions) $ do
                            path <- treePathNewFromIndices' [0]
                            treeViewSetCursor tree path noTreeViewColumn False
                        widgetShowAll window

            when (newWordStart /= currentWordStart) $
                replaceWordStart sourceView isWordChar False newWordStart

getRow tree = do
    Just model <- treeViewGetModel tree
    selection  <- treeViewGetSelection tree
    treeSelectionGetSelected selection >>= \case
        (True, _, iter) -> do
            [row] <- treeModelGetPath model iter >>= treePathGetIndices
            return $ Just row
        _ -> return Nothing
