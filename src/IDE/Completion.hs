-----------------------------------------------------------------------------
--
-- Module      :  IDE.Completion
-- Copyright   :  2007-2010 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  <maintainer@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Completion (complete, cancel) where

import Prelude hiding(getChar, getLine)

import Data.List as List hiding(insert)
import Data.Char
import Data.IORef
import Control.Monad
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk as Gtk hiding(onKeyPress, onKeyRelease)
import Graphics.UI.Gtk.Gdk.EventM as Gtk
import IDE.Core.State
import IDE.Metainfo.Provider(getDescription,getCompletionOptions)
import Control.Monad.Reader.Class (ask)
import IDE.TextEditor

complete :: EditorView -> Bool -> IDEAction
complete sourceView always = do
    currentState' <- readIDE currentState
    prefs'        <- readIDE prefs
    completion'   <- readIDE completion
    case (currentState',completion') of
        (IsCompleting c, Just (CompletionWindow window tv st)) ->
                            updateOptions window tv st sourceView c
        (IsRunning,_)   ->  when (always || not (completeRestricted prefs'))
                                (initCompletion sourceView)
        _               ->  return ()

cancel :: IDEAction
cancel = do
    currentState' <- readIDE currentState
    completion'   <- readIDE completion
    case (currentState',completion') of
        (IsCompleting conn , Just (CompletionWindow window tv st)) ->
            cancelCompletion window tv st conn
        _            -> return ()

initCompletion :: EditorView -> IDEAction
initCompletion sourceView = do
    ideR <- ask
    completion' <- readIDE completion
    case completion' of
        Just (CompletionWindow window' tree' store') -> do
            cids <- addEventHandling window' sourceView tree' store'
            modifyIDE_ (\ide -> ide{currentState = IsCompleting cids})
            updateOptions window' tree' store' sourceView cids
        Nothing -> do
            windows    <- getWindows
            prefs      <- readIDE prefs
            window     <- liftIO $ do
                windowNewPopup
            liftIO $ set window [ windowTypeHint := WindowTypeHintUtility,
                         windowDecorated := False,
                         windowResizable := True ]
            --liftIO $ widgetSetSizeRequest window 700 300
            liftIO $ containerSetBorderWidth window 3
            liftIO $ windowSetTransientFor window (head windows)
            paned      <- liftIO $ hPanedNew
            liftIO $ containerAdd window paned
            nameScrolledWindow <- liftIO $ scrolledWindowNew Nothing Nothing
            liftIO $ widgetSetSizeRequest nameScrolledWindow 250 400
            tree       <- liftIO $ treeViewNew
            liftIO $ containerAdd nameScrolledWindow tree
            store      <- liftIO $ listStoreNew []
            liftIO $ treeViewSetModel tree store

            font <- liftIO $ case textviewFont prefs of
                Just str -> do
                    fontDescriptionFromString str
                Nothing -> do
                    f <- fontDescriptionNew
                    fontDescriptionSetFamily f "Monospace"
                    return f
            liftIO $ widgetModifyFont tree (Just font)

            column   <- liftIO $ treeViewColumnNew
            liftIO $ set column [
                treeViewColumnSizing   := TreeViewColumnAutosize,
                treeViewColumnMinWidth := 800] -- OSX does not like it if there is no hscroll
            liftIO $ treeViewAppendColumn tree column
            renderer <- liftIO $ cellRendererTextNew
            liftIO $ treeViewColumnPackStart column renderer True
            liftIO $ cellLayoutSetAttributes column renderer store (\name -> [ cellText := name ])

            liftIO $ set tree [treeViewHeadersVisible := False]

            descriptionBuffer <- newGtkBuffer Nothing ""
            descriptionView   <- newView descriptionBuffer (textviewFont prefs)
            setStyle descriptionBuffer $ sourceStyle prefs
            descriptionScrolledWindow <- getScrolledWindow descriptionView

            liftIO $ widgetSetSizeRequest descriptionScrolledWindow 500 400
            visible    <- liftIO $ newIORef False
            activeView <- liftIO $ newIORef Nothing

            treeSelection <- liftIO $ treeViewGetSelection tree

            liftIO $ treeSelection `onSelectionChanged` (do
                treeSelectionSelectedForeach treeSelection (\treePath -> (do
                    rows <- treeSelectionGetSelectedRows treeSelection
                    case rows of
                        [treePath] -> reflectIDE (withWord store treePath (\name -> do
                            description <- getDescription name
                            setText descriptionBuffer description
                            )) ideR
                        _ -> return ()
                    ))
                )

            liftIO $ panedAdd1 paned nameScrolledWindow
            liftIO $ panedAdd2 paned descriptionScrolledWindow

            cids <- addEventHandling window sourceView tree store

            modifyIDE_ (\ide -> ide{currentState = IsCompleting cids,
                completion = Just (CompletionWindow window tree store)})
            updateOptions window tree store sourceView cids

addEventHandling :: Window -> EditorView -> TreeView -> ListStore String -> IDEM Connections
addEventHandling window sourceView tree store = do
    ideR      <- ask
    cidsPress <- sourceView `onKeyPress` \name modifier keyVal -> do
        char        <- liftIO $ keyvalToChar keyVal
        Just model  <- liftIO $ treeViewGetModel tree
        selection   <- liftIO $ treeViewGetSelection tree
        count       <- liftIO $ treeModelIterNChildren model Nothing
        Just column <- liftIO $ treeViewGetColumn tree 0
        case (name, modifier, char) of
            ("Tab", _, _) -> (do
                visible <- liftIO $ get tree widgetVisible
                if visible then (do
                    tryToUpdateOptions window tree store sourceView True
                    return True
                    )
                    else return False
                )
            ("Return", _, _) -> (do
                visible <- liftIO $ get tree widgetVisible
                if visible then (do
                    maybeRow <- liftIO $ getRow tree
                    case maybeRow of
                        Just row -> (do
                            liftIO $ treeViewRowActivated tree [row] column
                            return True
                            )
                        Nothing -> (do
                            cancel
                            return False
                            )
                    )
                    else return False
                )
            ("Down", _, _) -> (do
                visible <- liftIO $ get tree widgetVisible
                if visible then (do
                    maybeRow <- liftIO $ getRow tree
                    let newRow = maybe 0 (\row -> row + 1) maybeRow
                    when (newRow < count) $ liftIO $ do
                        treeSelectionSelectPath selection [newRow]
                        treeViewScrollToCell tree [newRow] column Nothing
                        -- Crazy hack to avoid the horizontal scroll
                        treeViewScrollToCell tree [newRow] column Nothing
                    return True
                    )
                    else return False
                )
            ("Up", _, _) -> (do
                visible <- liftIO $ get tree widgetVisible
                if visible then (do
                    maybeRow <- liftIO $ getRow tree
                    let newRow = maybe 0 (\row -> row - 1) maybeRow
                    when (newRow >= 0) $ liftIO $ do
                        treeSelectionSelectPath selection [newRow]
                        treeViewScrollToCell tree [newRow] column Nothing
                        -- Crazy hack to avoid the horizontal scroll
                        treeViewScrollToCell tree [newRow] column Nothing
                    return True
                    )
                    else return False
                )
            (_, _, Just c) | ((isAlphaNum c) || (c == '.') || (c == '_')) -> (do
                return False
                )
            ("BackSpace", _, _) -> (do
                return False
                )
            (shift, _, _) | (shift == "Shift_L") || (shift == "Shift_R") -> (do
                return False
                )
            _ -> (do
                cancel
                return False
                )

    cidsRelease <- sourceView `onKeyRelease` \name modifier keyVal -> do
        case (name, modifier) of
            ("BackSpace", _) -> do
                complete sourceView False
                return False
            _ -> return False

    idButtonPress <- liftIO $ window `on` buttonPressEvent $ do
        button     <- eventButton
        (x, y)     <- eventCoordinates
        time       <- eventTime
        (width, _) <- liftIO $ widgetGetSize window
        liftIO $ windowBeginResizeDrag window
            (if floor x < width `div` 2 then WindowEdgeSouthWest else WindowEdgeSouthEast)
            button (floor x) (floor y) time
        return True

    idSelected <- liftIO $ tree `onRowActivated` (\treePath column -> (do
        reflectIDE (withWord store treePath (replaceWordStart sourceView)) ideR
        liftIO $ postGUIAsync $ reflectIDE cancel ideR))

    return $ concat [cidsPress, cidsRelease, [ConnectC idButtonPress, ConnectC idSelected]]

withWord :: ListStore String -> TreePath -> (String -> IDEM ()) -> IDEM ()
withWord store treePath f = (do
   case treePath of
       [row] -> (do
            value <- liftIO $ listStoreGetValue store row
            f value
            )
       _ -> return ()
   )

replaceWordStart :: EditorView -> String -> IDEM ()
replaceWordStart sourceView name = do
    buffer <- getBuffer sourceView
    (selStart, end) <- getSelectionBounds buffer
    isWordEnd <- endsWord end
    when isWordEnd $ do
        start <- findWordStart selStart
        wordStart <- getText buffer start end True
        case stripPrefix wordStart name of
            Just extra -> insert buffer end extra
            Nothing    -> return ()

cancelCompletion :: Window -> TreeView -> ListStore String -> Connections -> IDEAction
cancelCompletion window tree store connections = do
    liftIO (do
        listStoreClear (store :: ListStore String)
        signalDisconnectAll connections
        widgetHideAll window
        )
    modifyIDE_ (\ide -> ide{currentState = IsRunning})

updateOptions :: Window -> TreeView -> ListStore String -> EditorView -> Connections -> IDEAction
updateOptions window tree store sourceView connections = do
    result <- tryToUpdateOptions window tree store sourceView False
    when (not result) $ cancelCompletion window tree store connections

tryToUpdateOptions :: Window -> TreeView -> ListStore String -> EditorView -> Bool -> IDEM Bool
tryToUpdateOptions window tree store sourceView selectLCP = do
    ideR <- ask
    liftIO $ listStoreClear (store :: ListStore String)
    buffer <- getBuffer sourceView
    (selStart, end) <- getSelectionBounds buffer
    start <- findWordStart selStart
    equal <- iterEqual start end
    if equal
        then return False
        else do
            wordStart <- getText buffer start end True
            liftIO $ postGUIAsync $ do
                reflectIDE (do
                    options <- getCompletionOptions wordStart
                    processResults window tree store sourceView wordStart options selectLCP) ideR
                return ()
            return True

findWordStart :: EditorIter -> IDEM EditorIter
findWordStart iter = do
    maybeWS <- backwardWordStartC iter
    case maybeWS of
        Nothing -> atOffset iter 0
        Just ws -> do
            prev <- backwardCharC ws
            maybeChar <- getChar prev
            case maybeChar of
                Just '_' -> findWordStart prev
                _        -> return ws

longestCommonPrefix (x:xs) (y:ys) | x == y = x : longestCommonPrefix xs ys
longestCommonPrefix _ _ = []

processResults :: Window -> TreeView -> ListStore String -> EditorView -> String -> [String] -> Bool -> IDEAction
processResults window tree store sourceView wordStart options selectLCP = do
    case options of
        [] -> cancel
        _  -> do
            buffer <- getBuffer sourceView
            (selStart, end) <- getSelectionBounds buffer
            isWordEnd <- endsWord end
            when isWordEnd $ do
                start <- findWordStart selStart
                currentWordStart <- getText buffer start end True
                newWordStart <- do
                    if selectLCP && currentWordStart == wordStart && (not $ null options)
                        then do
                            let lcp = foldl1 longestCommonPrefix options
                            return lcp
                        else
                            return currentWordStart

                when (isPrefixOf wordStart newWordStart) $ do
                    liftIO $ listStoreClear store
                    let newOptions = List.filter (isPrefixOf newWordStart) options
                    liftIO $ forM_ (take 200 newOptions) (listStoreAppend store)
                    Rectangle startx starty width height <- getIterLocation sourceView start
                    (wWindow, hWindow)                   <- liftIO $ windowGetSize window
                    (x, y)                               <- bufferToWindowCoords sourceView (startx, starty+height)
                    drawWindow                           <- getDrawWindow sourceView
                    (ox, oy)                             <- liftIO $ drawWindowGetOrigin drawWindow
                    Just namesSW                         <- liftIO $ widgetGetParent tree
                    (wNames, hNames)                     <- liftIO $ widgetGetSize namesSW
                    Just paned                           <- liftIO $ widgetGetParent namesSW
                    Just first                           <- liftIO $ panedGetChild1 (castToPaned paned)
                    Just second                          <- liftIO $ panedGetChild2 (castToPaned paned)
                    screen                               <- liftIO $ windowGetScreen window
                    monitor                              <- liftIO $ screenGetMonitorAtPoint screen (ox+x) (oy+y)
                    monitorLeft                          <- liftIO $ screenGetMonitorAtPoint screen (ox+x-wWindow+wNames) (oy+y)
                    monitorRight                         <- liftIO $ screenGetMonitorAtPoint screen (ox+x+wWindow) (oy+y)
                    monitorBelow                         <- liftIO $ screenGetMonitorAtPoint screen (ox+x) (oy+y+hWindow)
                    wScreen                              <- liftIO $ screenGetWidth screen
                    hScreen                              <- liftIO $ screenGetHeight screen
                    top <- if monitorBelow /= monitor || (oy+y+hWindow) > hScreen
                        then do
                            sourceSW <- getScrolledWindow sourceView
                            (_, hSource)  <- liftIO $ widgetGetSize sourceSW
                            scrollToIter sourceView end 0.1 (Just (1.0, 1.0 - (fromIntegral hWindow / fromIntegral hSource)))
                            (_, newy)     <- bufferToWindowCoords sourceView (startx, starty+height)
                            return (oy+newy)
                        else return (oy+y)
                    swap <- if (monitorRight /= monitor || (ox+x+wWindow) > wScreen) && monitorLeft == monitor && (ox+x-wWindow+wNames) > 0
                        then do
                            liftIO $ windowMove window (ox+x-wWindow+wNames) top
                            return $ first == namesSW
                        else do
                            liftIO $ windowMove window (ox+x) top
                            return $ first /= namesSW
                    when swap $ liftIO $ do
                        pos <- panedGetPosition (castToPaned paned)
                        containerRemove (castToPaned paned) first
                        containerRemove (castToPaned paned) second
                        panedAdd1 (castToPaned paned) second
                        panedAdd2 (castToPaned paned) first
                        panedSetPosition (castToPaned paned) (wWindow-pos)
                    when (not $ null newOptions) $ liftIO $ treeViewSetCursor tree [0] Nothing
                    liftIO $ widgetShowAll window

                when (newWordStart /= currentWordStart) $
                    replaceWordStart sourceView newWordStart

getRow tree = do
    Just model <- treeViewGetModel tree
    selection <- treeViewGetSelection tree
    maybeIter <- treeSelectionGetSelected selection
    case maybeIter of
        Just iter -> (do
            [row] <- treeModelGetPath model iter
            return $ Just row
            )
        Nothing -> return Nothing
