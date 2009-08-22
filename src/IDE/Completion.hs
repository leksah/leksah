-----------------------------------------------------------------------------
--
-- Module      :  IDE.Completion
-- Copyright   :  2007-2009 Hamish Mackenzie, Jürgen Nicklisch-Franken
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
            cids <- liftIO $ addEventHandling window' sourceView tree' store' ideR
            modifyIDE_ (\ide -> ide{currentState = IsCompleting cids})
            updateOptions window' tree' store' sourceView cids
        Nothing -> do
            windows    <- getWindows
            prefs      <- readIDE prefs
            (window', tree', store', cids) <- reifyIDE (\ideR -> do
                window <- windowNewPopup
                set window [ windowTypeHint := WindowTypeHintUtility,
                             windowDecorated := False,
                             windowResizable := True ]
                --widgetSetSizeRequest window 700 300
                containerSetBorderWidth window 3
                windowSetTransientFor window (head windows)
                paned          <- hPanedNew
                containerAdd window paned
                nameScrolledWindow <- scrolledWindowNew Nothing Nothing
                widgetSetSizeRequest nameScrolledWindow 250 400
                tree           <- treeViewNew
                containerAdd nameScrolledWindow tree
                store          <- listStoreNew []
                treeViewSetModel tree store

                font <- case textviewFont prefs of
                    Just str -> do
                        fontDescriptionFromString str
                    Nothing -> do
                        f <- fontDescriptionNew
                        fontDescriptionSetFamily f "Monospace"
                        return f
                widgetModifyFont tree (Just font)

                column   <- treeViewColumnNew
                set column [
                    treeViewColumnSizing   := TreeViewColumnAutosize,
                    treeViewColumnMinWidth := 800] -- OSX does not like it if there is no hscroll
                treeViewAppendColumn tree column
                renderer <- cellRendererTextNew
                treeViewColumnPackStart column renderer True
                cellLayoutSetAttributes column renderer store (\name -> [ cellText := name ])

                set tree [treeViewHeadersVisible := False]

                descriptionBuffer <- newGtkBuffer Nothing
                descriptionView <- newView descriptionBuffer
                descriptionScrolledWindow <- getScrolledWindow descriptionView

                widgetSetSizeRequest descriptionScrolledWindow 500 400
                setFont descriptionView $ textviewFont prefs
                setStyle descriptionBuffer $ sourceStyle prefs
                visible <- newIORef False
                activeView <- newIORef Nothing

                treeSelection <- treeViewGetSelection tree

                treeSelection `onSelectionChanged` (do
                    treeSelectionSelectedForeach treeSelection (\treePath -> (do
                        rows <- treeSelectionGetSelectedRows treeSelection
                        case rows of
                            [treePath] -> withWord store treePath (\name -> do
                                description <- reflectIDE (getDescription name) ideR
                                setText descriptionBuffer description
                                )
                            _ -> return ()
                        ))
                    )

                panedAdd1 paned nameScrolledWindow
                panedAdd2 paned descriptionScrolledWindow

                cids <- liftIO $ addEventHandling window sourceView tree store ideR
                return (window, tree, store, cids)
                )
            modifyIDE_ (\ide -> ide{currentState = IsCompleting cids,
                completion = Just (CompletionWindow window' tree' store')})
            updateOptions window' tree' store' sourceView cids

addEventHandling :: Window -> EditorView -> TreeView -> ListStore String -> IDERef -> IO Connections
addEventHandling window sourceView tree store ideR = do
    cidsPress <- sourceView `onKeyPress` \name modifier keyVal -> do
        char        <- keyvalToChar keyVal
        Just model  <- treeViewGetModel tree
        selection   <- treeViewGetSelection tree
        count       <- treeModelIterNChildren model Nothing
        Just column <- treeViewGetColumn tree 0
        case (name, modifier, char) of
            ("Tab", _, _) -> (do
                visible <- get tree widgetVisible
                if visible then (do
                    reflectIDE (tryToUpdateOptions window tree store sourceView True) ideR
                    return True
                    )
                    else return False
                )
            ("Return", _, _) -> (do
                visible <- get tree widgetVisible
                if visible then (do
                    maybeRow <- getRow tree
                    case maybeRow of
                        Just row -> (do
                            treeViewRowActivated tree [row] column
                            return True
                            )
                        Nothing -> (do
                            reflectIDE cancel ideR
                            return False
                            )
                    )
                    else return False
                )
            ("Down", _, _) -> (do
                visible <- get tree widgetVisible
                if visible then (do
                    maybeRow <- getRow tree
                    let newRow = maybe 0 (\row -> row + 1) maybeRow
                    when (newRow < count) (do
                        treeSelectionSelectPath selection [newRow]
                        treeViewScrollToCell tree [newRow] column Nothing
                        -- Crazy hack to avoid the horizontal scroll
                        treeViewScrollToCell tree [newRow] column Nothing
                        )
                    return True
                    )
                    else return False
                )
            ("Up", _, _) -> (do
                visible <- get tree widgetVisible
                if visible then (do
                    maybeRow <- getRow tree
                    let newRow = maybe 0 (\row -> row - 1) maybeRow
                    when (newRow >= 0) (do
                        treeSelectionSelectPath selection [newRow]
                        treeViewScrollToCell tree [newRow] column Nothing
                        -- Crazy hack to avoid the horizontal scroll
                        treeViewScrollToCell tree [newRow] column Nothing
                        )
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
                reflectIDE cancel ideR
                return False
                )

    cidsRelease <- sourceView `onKeyRelease` \name modifier keyVal -> do
        case (name, modifier) of
            ("BackSpace", _) -> do
                reflectIDE (complete sourceView False) ideR
                return False
            _ -> return False

    idButtonPress <- window `on` buttonPressEvent $ do
        button     <- eventButton
        (x, y)     <- eventCoordinates
        time       <- eventTime
        (width, _) <- liftIO $ widgetGetSize window
        liftIO $ windowBeginResizeDrag window
            (if floor x < width `div` 2 then WindowEdgeSouthWest else WindowEdgeSouthEast)
            button (floor x) (floor y) time
        return True

    idSelected <- tree `onRowActivated` (\treePath column -> (do
        withWord store treePath (replaceWordStart sourceView)
        postGUIAsync $ reflectIDE cancel ideR))

    return $ concat [cidsPress, cidsRelease, [ConnectC idButtonPress, ConnectC idSelected]]

withWord store treePath f = (do
   case treePath of
       [row] -> (do
            value <- listStoreGetValue store row
            f value
            )
       _ -> return ()
   )

replaceWordStart sourceView name = do
    buffer <- getBuffer sourceView
    (start, end) <- getSelectionBounds buffer
    isWordEnd <- endsWord end
    when isWordEnd $ do
        moveToWordStart start
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
tryToUpdateOptions window tree store sourceView selectLCP =
    reifyIDE (\ideR -> do
        buffer <- getBuffer sourceView
        listStoreClear (store :: ListStore String)
        (start, end) <- getSelectionBounds buffer
        moveToWordStart start
        equal <- iterEqual start end
        if equal
            then return False
            else do
                wordStart <- getText buffer start end True
                postGUIAsync $ do
                    options <- reflectIDE (getCompletionOptions wordStart) ideR
                    processResults ideR window tree store sourceView wordStart options selectLCP
                    return ()
                return True)

moveToWordStart iter = do
    backwardWordStart iter
    isStart <- isStart iter
    when (not isStart) $ do
        prev <- copyIter iter
        backwardChar prev
        maybeChar <- getChar prev
        case maybeChar of
            Just '_' -> (do
                backwardChar iter
                moveToWordStart iter
                )
            _ -> return ()

longestCommonPrefix (x:xs) (y:ys) | x == y = x : longestCommonPrefix xs ys
longestCommonPrefix _ _ = []

processResults ideR window tree store sourceView wordStart options selectLCP = do
    case options of
        [] -> reflectIDE cancel ideR
        _  -> do
            buffer <- getBuffer sourceView
            (start, end) <- getSelectionBounds buffer
            isWordEnd <- endsWord end
            when isWordEnd $ do
                moveToWordStart start
                currentWordStart <- getText buffer start end True
                newWordStart <- do
                    if selectLCP && currentWordStart == wordStart && (not $ null options)
                        then do
                            let lcp = foldl1 longestCommonPrefix options
                            return lcp
                        else
                            return currentWordStart

                when (isPrefixOf wordStart newWordStart) $ do
                    listStoreClear store
                    let newOptions = List.filter (isPrefixOf newWordStart) options
                    forM_ (take 200 newOptions) (listStoreAppend store)
                    Rectangle startx starty width height <- getIterLocation sourceView start
                    (wWindow, hWindow)                   <- windowGetSize window
                    (x, y)                               <- bufferToWindowCoords sourceView (startx, starty+height)
                    drawWindow                           <- getDrawWindow sourceView
                    (ox, oy)                             <- drawWindowGetOrigin drawWindow
                    Just namesSW                         <- widgetGetParent tree
                    (wNames, hNames)                     <- widgetGetSize namesSW
                    Just paned                           <- widgetGetParent namesSW
                    Just first                           <- panedGetChild1 (castToPaned paned)
                    Just second                          <- panedGetChild2 (castToPaned paned)
                    screen                               <- windowGetScreen window
                    monitor                              <- screenGetMonitorAtPoint screen (ox+x) (oy+y)
                    monitorLeft                          <- screenGetMonitorAtPoint screen (ox+x-wWindow+wNames) (oy+y)
                    monitorRight                         <- screenGetMonitorAtPoint screen (ox+x+wWindow) (oy+y)
                    monitorBelow                         <- screenGetMonitorAtPoint screen (ox+x) (oy+y+hWindow)
                    wScreen                              <- screenGetWidth screen
                    hScreen                              <- screenGetHeight screen
                    top <- if monitorBelow /= monitor || (oy+y+hWindow) > hScreen
                        then do
                            sourceSW <- getScrolledWindow sourceView
                            (_, hSource)  <- widgetGetSize sourceSW
                            scrollToIter sourceView end 0.1 (Just (1.0, 1.0 - (fromIntegral hWindow / fromIntegral hSource)))
                            (_, newy)     <- bufferToWindowCoords sourceView (startx, starty+height)
                            return (oy+newy)
                        else return (oy+y)
                    swap <- if (monitorRight /= monitor || (ox+x+wWindow) > wScreen) && monitorLeft == monitor && (ox+x-wWindow+wNames) > 0
                        then do
                            windowMove window (ox+x-wWindow+wNames) top
                            return $ first == namesSW
                        else do
                            windowMove window (ox+x) top
                            return $ first /= namesSW
                    when swap $ do
                        pos <- panedGetPosition (castToPaned paned)
                        containerRemove (castToPaned paned) first
                        containerRemove (castToPaned paned) second
                        panedAdd1 (castToPaned paned) second
                        panedAdd2 (castToPaned paned) first
                        panedSetPosition (castToPaned paned) (wWindow-pos)
                    when (not $ null newOptions) $ treeViewSetCursor tree [0] Nothing
                    widgetShowAll window

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
