{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE PatternSynonyms#-}
{-# LANGUAGE LambdaCase #-}
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

module IDE.Completion (complete, cancel, setCompletionSize, smartIndent) where

import Prelude ()
import Prelude.Compat hiding(getChar, getLine)

import Data.List as List (filter)
import Data.Char (isAlphaNum, isSymbol)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad (when, void, unless)
import Control.Monad.Fail.Compat (MonadFail)
import Data.Foldable (forM_)
import Foreign (castPtr)
import IDE.Core.State
       (IDEAction, IDEState(..), IDEM,
        readIDE, currentState, prefs, ideGtk,
        completeRestricted, modifyIDE_,
        textviewFont, reflectIDE, liftIDE,
        tabWidth)
import IDE.Gtk.State
       (CompletionWindow(..), Connections,
        completion, getWindows, postAsyncIDE,
        Connection(..), signalDisconnectAll)
import IDE.Metainfo.Provider
       (keywords, getDescription, getCompletionOptions)
import IDE.TextEditor as TE
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Applicative ((<$>))
import Control.Lens (pre, (.~), _1, _2, _Just)
import Data.Text (Text)
import qualified Data.Text as T
       (replicate, empty, commonPrefixes, pack, unpack, null, stripPrefix,
        isPrefixOf, length)
import qualified Data.Text.Encoding as T (encodeUtf8)
import System.Log.Logger (debugM)
import GI.Gtk.Objects.Window
       (windowMove, windowGetSize, Window(..),
        windowNew, setWindowTransientFor, setWindowDefaultHeight,
        setWindowDefaultWidth, setWindowResizable, setWindowDecorated,
        setWindowTypeHint, windowResize)
import Data.GI.Base
       (unsafeCastTo, newBoxed, withManagedPtr)
import GI.Gdk.Enums (GrabStatus(..), WindowTypeHint(..))
import GI.Gtk.Objects.Container
       (containerRemove, containerAdd, containerSetBorderWidth)
import GI.Gtk.Objects.Paned
       (panedNew, panedSetPosition, panedGetPosition, panedGetChild2, Paned(..),
        panedGetChild1, panedAdd2, panedAdd1)
import GI.Gtk.Objects.ScrolledWindow (scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (Adjustment)
import GI.Gtk.Objects.Widget
       (Widget(..), widgetShowAll, widgetGetAllocation, widgetGetParent,
        widgetHide, onWidgetButtonReleaseEvent, onWidgetMotionNotifyEvent,
        widgetGetWindow, onWidgetButtonPressEvent, getWidgetVisible,
        widgetSetSizeRequest)
import GI.Gtk.Objects.TreeView
       (IsTreeView, treeViewSetCursor, onTreeViewRowActivated,
        treeViewScrollToCell, treeViewGetModel, TreeView(..),
        treeViewGetSelection, setTreeViewHeadersVisible,
        treeViewAppendColumn, treeViewSetModel, treeViewNew)
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreAppend, seqStoreClear, seqStoreGetValue, SeqStore(..),
        seqStoreNew)
import GI.Pango.Structs.FontDescription
       (fontDescriptionSetFamily, fontDescriptionNew,
        fontDescriptionFromString)
import GI.Gtk.Objects.TreeViewColumn
       (TreeViewColumn, treeViewColumnPackStart, setTreeViewColumnMinWidth,
        setTreeViewColumnSizing, treeViewColumnNew)
import GI.Gtk.Enums (TreeViewColumnSizing(..), WindowType(..), Orientation(..))
import GI.Gtk.Objects.CellRendererText
       (setCellRendererTextText, cellRendererTextNew)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction)
import GI.Gtk.Objects.TreeSelection
       (treeSelectionGetSelected, treeSelectionSelectPath,
        treeSelectionSelectedForeach,
        onTreeSelectionChanged)
import GI.Gdk.Structs.EventKey
       (getEventKeyKeyval, getEventKeyState)
import GI.Gdk.Functions
       (keyvalToUnicode, keyvalName)
import GI.Gtk.Interfaces.TreeModel
       (treeModelGetPath, treeModelIterNChildren)
import GI.Gdk.Structs.EventButton
       (getEventButtonY, getEventButtonX)
import GI.Gdk.Flags (SeatCapabilities(..))
import GI.Gdk.Structs.EventMotion
       (getEventMotionY, getEventMotionX)
import GI.Gtk.Structs.TreePath (treePathGetIndices, TreePath(..))
import Graphics.UI.Frame.Rectangle
       (getRectangleHeight, getRectangleWidth, getRectangleY,
        getRectangleX)
import GI.Gdk.Objects.Window (windowGetOrigin)
import Data.GI.Gtk.ModelView.Types
       (equalManagedPtr, treePathGetIndices', treePathNewFromIndices')
import Data.Maybe (fromJust)
import GI.Gtk
       (widgetGetScreen, getCurrentEventDevice, CssProvider(..),
        pattern STYLE_PROVIDER_PRIORITY_APPLICATION, styleContextAddProvider,
        cssProviderLoadFromData, widgetGetStyleContext, cssProviderNew)
import Graphics.UI.Utils (fontDescriptionToCssProps)
import GI.Gdk
       (monitorGetGeometry, displayGetMonitorAtPoint, windowGetDisplay,
        seatUngrab, Event(..), EventButton(..), noSeatGrabPrepareFunc,
        cursorNewFromName, screenGetDisplay, seatGrab, deviceGetSeat,
        Monitor(..))
import Data.Int (Int32)

complete :: TextEditor editor => EditorView editor -> Bool -> IDEAction
complete sourceView always = do
    currentState'    <- readIDE currentState
    prefs'           <- readIDE prefs
    completion'      <- readIDE (pre $ ideGtk . _Just . completion . _2)
    case (currentState',completion') of
        (_, Nothing) -> return ()
        (IsCompleting c, Just (Just (CompletionWindow window tv st))) -> do
                            isWordChar <- getIsWordChar sourceView
                            updateOptions window tv st sourceView c isWordChar always
        (IsRunning,_)   ->  when (always || not (completeRestricted prefs'))
                                (initCompletion sourceView always)
        _               ->  return ()

cancel :: IDEAction
cancel = do
    currentState'    <- readIDE currentState
    completion'      <- readIDE (pre $ ideGtk . _Just . completion . _2)
    case (currentState',completion') of
        (_, Nothing) -> return ()
        (IsCompleting conn , Just (Just (CompletionWindow window tv st))) ->
            cancelCompletion window tv st conn
        _            -> return ()

setCompletionSize :: Int -> Int -> IDEAction
setCompletionSize x y | x > 10 && y > 10 =
    readIDE (pre $ ideGtk . _Just . completion . _2) >>= mapM_ (\c -> do
        case c of
            Just (CompletionWindow window _ _) -> windowResize window (fromIntegral x) (fromIntegral y)
            Nothing                            -> return ()
        modifyIDE_ $ ideGtk . _Just . completion . _1 .~ (x, y))
setCompletionSize _ _ = return ()

getIsWordChar :: forall editor. TextEditor editor => EditorView editor -> IDEM (Char -> Bool)
getIsWordChar sourceView = do
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
    getChar prev >>= \case
        Just prevChar | isIdent prevChar -> return isIdent
                      | isOp    prevChar -> return isOp
        _                                -> return $ const False

initCompletion :: forall editor. TextEditor editor => EditorView editor -> Bool -> IDEAction
initCompletion sourceView always = do
    ideR <- ask
    readIDE (pre $ ideGtk . _Just . completion) >>= mapM_ (\((width, height), completion') -> do
        isWordChar <- getIsWordChar sourceView
        case completion' of
            Just (CompletionWindow window' tree' store') -> do
                cids <- addEventHandling window' sourceView tree' store' isWordChar always
                modifyIDE_ $ currentState .~ IsCompleting cids
                updateOptions window' tree' store' sourceView cids isWordChar always
            Nothing -> do
                windows    <- getWindows
                prefs'     <- readIDE prefs
                window     <- windowNew WindowTypePopup
                setWindowTypeHint      window WindowTypeHintUtility
                setWindowDecorated     window False
                setWindowResizable     window True
                setWindowDefaultWidth  window $ fromIntegral width
                setWindowDefaultHeight window $ fromIntegral height
                setWindowTransientFor  window $ head windows
                containerSetBorderWidth window 3
                paned      <- panedNew OrientationHorizontal
                containerAdd window paned
                nameScrolledWindow <- scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
                widgetSetSizeRequest nameScrolledWindow 250 40
                tree       <- treeViewNew
                containerAdd nameScrolledWindow tree
                store      <- seqStoreNew []
                treeViewSetModel tree (Just store)

                font <- case textviewFont prefs' of
                    Just str ->
                        fontDescriptionFromString str
                    Nothing -> do
                        f <- fontDescriptionNew
                        fontDescriptionSetFamily f "Monospace"
                        return f
                provider <- cssProviderNew
                styleContext <- widgetGetStyleContext tree
                cssProps <- fontDescriptionToCssProps font
                cssProviderLoadFromData provider . T.encodeUtf8
                  $ "treeview { " <> cssProps <> "}"
                styleContextAddProvider styleContext provider (fromIntegral STYLE_PROVIDER_PRIORITY_APPLICATION)

                column   <- treeViewColumnNew
                setTreeViewColumnSizing   column TreeViewColumnSizingFixed
                setTreeViewColumnMinWidth column 800 -- OSX does not like it if there is no hscroll
                _ <- treeViewAppendColumn tree column
                renderer <- cellRendererTextNew
                treeViewColumnPackStart column renderer True
                cellLayoutSetDataFunction column renderer store $ setCellRendererTextText renderer

                setTreeViewHeadersVisible tree False

                descriptionBuffer <- newDefaultBuffer Nothing ""
                (descriptionView, descriptionScrolledWindow) <- newView descriptionBuffer (textviewFont prefs')
                updateStyle descriptionBuffer
                setShowLineMarks descriptionView False
                setHighlightCurrentLine descriptionView False

                treeSelection <- treeViewGetSelection tree

                _ <- onTreeSelectionChanged treeSelection $
                    treeSelectionSelectedForeach treeSelection $ \_model treePath _iter ->
                        reflectIDE (void $ withWord store treePath (\name -> do
                                description <- getDescription name
                                setText descriptionBuffer description
                                )) ideR

                panedAdd1 paned nameScrolledWindow
                panedAdd2 paned descriptionScrolledWindow

                cids <- addEventHandling window sourceView tree store isWordChar always

                modifyIDE_ $ (currentState .~ IsCompleting cids)
                    . (ideGtk . _Just . completion .~ ((width, height), Just (CompletionWindow window tree store)))
                updateOptions window tree store sourceView cids isWordChar always)

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
        -- Just column <- treeViewGetColumn tree 0
        let whenVisible f = getWidgetVisible tree >>= \case
                                True  -> f
                                False -> return False
            down = whenVisible $ do
                maybeRow <- liftIO $ getRow tree
                let newRow = maybe 0 (+ 1) maybeRow
                when (newRow < count) $ do
                    path <- treePathNewFromIndices' [newRow]
                    treeSelectionSelectPath selection path
                    treeViewScrollToCell tree (Just path) (Nothing :: Maybe TreeViewColumn) False 0 0
                return True
            up = whenVisible $ do
                maybeRow <- liftIO $ getRow tree
                let newRow = maybe 0 (\ row -> row - 1) maybeRow
                when (newRow >= 0) $ do
                    path <- treePathNewFromIndices' [newRow]
                    treeSelectionSelectPath selection path
                    treeViewScrollToCell tree (Just path) (Nothing :: Maybe TreeViewColumn) False 0 0
                return True
        case (name, modifier, char) of
            (Just "Tab", _, _) -> whenVisible . liftIDE $ do
                _ <- tryToUpdateOptions window tree store sourceView True isWordChar always
                return True
            (Just "Return", _, _) -> getWidgetVisible tree >>= \case
                True  -> do
                    maybeRow <- liftIO $ getRow tree
                    case maybeRow of
                        Just row -> do
                            path <- treePathNewFromIndices' [row]
                            liftIDE $ withWord store path (replaceWordStart sourceView isWordChar) >>= \case
                                Just True -> liftIDE $ smartIndent sourceView
                                _         -> return ()
                            liftIDE $ postAsyncIDE cancel
                            return True
                        Nothing -> liftIDE $ do
                            cancel
                            smartIndent sourceView
                            return True
                False -> liftIDE $ do
                    smartIndent sourceView
                    return True
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
        x          <- getEventButtonX e
        y          <- getEventButtonY e

        widgetGetWindow window >>= \case
            Nothing -> return ()
            Just drawWindow ->
                getCurrentEventDevice >>= mapM deviceGetSeat >>= \case
                    Nothing -> return ()
                    Just seat -> do
                        screen <- widgetGetScreen window
                        display <- screenGetDisplay screen
                        mbCursor <- cursorNewFromName display "crosshair"
                        e' <- withManagedPtr e $ newBoxed Event . castPtr
                        status <- seatGrab
                            seat
                            drawWindow
                            [SeatCapabilitiesAllPointing]
                            False
                            mbCursor
                            (Just e')
                            noSeatGrabPrepareFunc
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
                getCurrentEventDevice >>= mapM deviceGetSeat >>= \case
                    Nothing -> return ()
                    Just seat -> seatUngrab seat
                liftIO $ writeIORef resizeHandler Nothing
                return True
            Nothing     -> return False)

    idSelected <- ConnectC tree <$> onTreeViewRowActivated tree (\treePath _column -> (`reflectIDE` ideR) $ do
        _ <- withWord store treePath (replaceWordStart sourceView isWordChar)
        postAsyncIDE cancel)

    return $ concat [cidsPress, cidsRelease, [idButtonPress, idMotion, idButtonRelease, idSelected]]

smartIndent :: TextEditor editor => EditorView editor -> IDEM ()
smartIndent sourceView = do
    indentWidth <- tabWidth <$> readIDE prefs
    buffer <- getBuffer sourceView
    (selStart, selEnd) <- getSelectionBounds buffer
    lineStart <- backwardToLineStartC selStart
    line <- getText buffer lineStart selStart True
    let lastWord = reverse . takeWhile (\c -> isAlphaNum c || c `elem` ['\'','_']) . reverse $ T.unpack line
        lastOp = reverse . takeWhile (\c -> not (isAlphaNum c) && c `notElem` ['\'','_','\"',' ',']',')','}',',',';']) . reverse $ T.unpack line
        indentAmount = length . takeWhile (==' ') $ T.unpack line
        extraIndent = T.pack lastWord `elem` keywords || (not (null lastOp) && lastOp `notElem` ["--", "\\"])
        newIndent = if extraIndent
                        then (indentAmount `div` indentWidth + 1) * indentWidth
                        else indentAmount
    delete buffer selStart selEnd
    insert buffer selStart $ "\n" <> T.replicate newIndent " "

withWord :: SeqStore Text -> TreePath -> (Text -> IDEM a) -> IDEM (Maybe a)
withWord store treePath f =
    treePathGetIndices' treePath >>= \case
        [row] -> do
            value <- seqStoreGetValue store row
            Just <$> f value
        _ -> return Nothing

-- Return value indicates if the we did nothing and return key should
-- still be cause a new line to be started.
replaceWordStart :: TextEditor editor => EditorView editor -> (Char -> Bool) -> Text -> IDEM Bool
replaceWordStart sourceView isWordChar name = do
    buffer <- getBuffer sourceView
    (selStart, selEnd) <- getSelectionBounds buffer
    start <- findWordStart selStart isWordChar
    wordStart <- getText buffer start selEnd True
    case T.stripPrefix wordStart name of
        Just "" -> return True
        Just extra -> do
            end <- findWordEnd selEnd isWordChar
            wordFinish <- getText buffer selEnd end True
            case T.stripPrefix wordFinish extra of
                Just extra2 | not (T.null wordFinish) -> do
                    selectRange buffer end end
                    insert buffer end extra2
                _ -> insert buffer selEnd extra
            return False
        Nothing -> return False

cancelCompletion :: Window -> TreeView -> SeqStore Text -> Connections -> IDEAction
cancelCompletion window _tree store connections = do
    seqStoreClear (store :: SeqStore Text)
    signalDisconnectAll connections
    widgetHide window
    modifyIDE_ $ currentState .~ IsRunning

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
            if T.length wordStart < 2
                then return False
                else do
                    liftIO $  -- dont use postGUIAsync - it causes bugs related to several repeated tryToUpdateOptions in thread
                        reflectIDE (do
                            options <- getCompletionOptions wordStart
                            processResults window tree store sourceView wordStart options selectLCP isWordChar always) ideR
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

longestCommonPrefix :: Text -> Text -> Text
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
                _width               <- getRectangleWidth rect
                height               <- getRectangleHeight rect
                (wWindow, hWindow)   <- windowGetSize window
                (x, y)               <- bufferToWindowCoords sourceView (fromIntegral startx, fromIntegral (starty+height))
                mbDrawWindow         <- getWindow sourceView
                case mbDrawWindow of
                    Nothing -> return ()
                    Just drawWindow -> do
                        (_, ox, oy)  <- windowGetOrigin drawWindow
                        Just namesSW <- widgetGetParent tree
                        rNames       <- widgetGetAllocation namesSW
                        wNames       <- getRectangleWidth rNames
                        _hNames      <- getRectangleHeight rNames
                        paned        <- widgetGetParent namesSW >>= liftIO . unsafeCastTo Paned . fromJust
                        Just first   <- panedGetChild1 paned
                        Just second  <- panedGetChild2 paned
                        display      <- windowGetDisplay drawWindow
                        monitor      <- displayGetMonitorAtPoint display (ox+fromIntegral x) (oy+fromIntegral y)
                        monitorLeft  <- displayGetMonitorAtPoint display (ox+fromIntegral x-wWindow+wNames) (oy+fromIntegral y)
                        monitorRight <- displayGetMonitorAtPoint display (ox+fromIntegral x+wWindow) (oy+fromIntegral y)
                        monitorBelow <- displayGetMonitorAtPoint display (ox+fromIntegral x) (oy+fromIntegral y+hWindow)
                        monitorRect  <- monitorGetGeometry monitor
                        monitorW     <- getRectangleWidth monitorRect
                        monitorH     <- getRectangleHeight monitorRect
                        let eqMonitor (Monitor m1) (Monitor m2) = liftIO $
                                withManagedPtr m1 $ \ptr1 ->
                                    withManagedPtr m2 $ \ptr2 ->
                                        return $ ptr1 == ptr2

                        sameMonitorBelow <- eqMonitor monitorBelow monitor
                        top <- if not sameMonitorBelow || (oy+fromIntegral y+hWindow) > monitorH
                            then do
                                sourceSW <- getScrolledWindow sourceView
                                hSource <- widgetGetAllocation sourceSW >>= getRectangleHeight
                                scrollToIter sourceView end 0.1 (Just (1.0, 1.0 - (fromIntegral hWindow / fromIntegral hSource)))
                                (_, newy)     <- bufferToWindowCoords sourceView (fromIntegral startx, fromIntegral (starty+height))
                                return (oy+fromIntegral newy)
                            else return (oy+fromIntegral y)
                        sameMonitorLeft <- eqMonitor monitorLeft monitor
                        sameMonitorRight <- eqMonitor monitorRight monitor
                        liftIO $ debugM "leksah" $ "Completion processResults " <> show (not sameMonitorRight, sameMonitorLeft, ox, x, wWindow, monitorW, wNames)
                        swap <- if (not sameMonitorRight || (ox+fromIntegral x+wWindow) > monitorW) && sameMonitorLeft && (ox+fromIntegral x-wWindow+wNames) > 0
                            then do
                                windowMove window (ox+fromIntegral x-wWindow+wNames) top
                                return $ first `equalManagedPtr` namesSW
                            else do
                                windowMove window (ox+fromIntegral x) top
                                return . not $ first `equalManagedPtr` namesSW
                        when swap $ do
                            pos <- panedGetPosition paned
                            containerRemove paned first
                            containerRemove paned second
                            panedAdd1 paned second
                            panedAdd2 paned first
                            panedSetPosition paned (wWindow-pos)
                        unless (null newOptions) $ do
                            path <- treePathNewFromIndices' [0]
                            treeViewSetCursor tree path (Nothing :: Maybe TreeViewColumn) False
                        widgetShowAll window

            when (newWordStart /= currentWordStart) $
                void $ replaceWordStart sourceView isWordChar newWordStart

getRow
  :: (MonadIO m, MonadFail m, IsTreeView a)
  => a
  -> m (Maybe Int32)
getRow tree = do
    Just model <- treeViewGetModel tree
    selection  <- treeViewGetSelection tree
    treeSelectionGetSelected selection >>= \case
        (True, _, iter) -> do
            Just [row] <- treeModelGetPath model iter >>= treePathGetIndices
            return $ Just row
        _ -> return Nothing
