{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk as Gtk hiding(onKeyPress, onKeyRelease)
import Graphics.UI.Gtk.Gdk.EventM as Gtk
import IDE.Core.State
import IDE.Metainfo.Provider(getDescription,getCompletionOptions)
import Control.Monad.Reader.Class (ask)
import IDE.TextEditor

complete :: EditorView -> Bool -> IDEAction
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
        (IsCompleting conn , Just (CompletionWindow window tv st)) -> do
            cancelCompletion window tv st conn
        _            -> return ()

setCompletionSize :: (Int, Int) -> IDEAction
setCompletionSize (x, y) | x > 10 && y > 10 = do
    (_, completion) <- readIDE completion
    case completion of
        Just (CompletionWindow window _ _) -> liftIO $ windowResize window x y
        Nothing                            -> return ()
    modifyIDE_ $ \ide -> ide{completion = ((x, y), completion)}
setCompletionSize _ = return ()

getIsWordChar :: EditorView -> IDEM (Char -> Bool)
getIsWordChar sourceView = do
    ideR <- ask
    buffer <- getBuffer sourceView
    (_, end) <- getSelectionBounds buffer
    sol <- backwardToLineStartC end
    eol <- forwardToLineEndC end
    line <- getSlice buffer sol eol False

    let isImport = "import " `isPrefixOf` line
        isIdent a = isAlphaNum a || a == '\'' || a == '_'  || (isImport && a == '.')
        isOp    a = isSymbol   a || a == ':'  || a == '\\' || a == '*' || a == '/' || a == '-'
                                 || a == '!'  || a == '@'  || a == '%' || a == '&' || a == '?'
    prev <- backwardCharC end
    prevChar <- getChar prev
    case prevChar of
        Just prevChar | isIdent prevChar -> return isIdent
        Just prevChar | isOp    prevChar -> return isOp
        _                                -> return $ const False

initCompletion :: EditorView -> Bool -> IDEAction
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
            window     <- liftIO windowNewPopup
            liftIO $ windowSetTransientFor window (head windows)
            liftIO $ set window [
                         windowTypeHint      := WindowTypeHintUtility,
                         windowDecorated     := False,
                         windowResizable     := True,
                         windowDefaultWidth  := width,
                         windowDefaultHeight := height]
            liftIO $ containerSetBorderWidth window 3
            paned      <- liftIO $ hPanedNew
            liftIO $ containerAdd window paned
            nameScrolledWindow <- liftIO $ scrolledWindowNew Nothing Nothing
            liftIO $ widgetSetSizeRequest nameScrolledWindow 250 40
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
                treeViewColumnSizing   := TreeViewColumnFixed,
                treeViewColumnMinWidth := 800] -- OSX does not like it if there is no hscroll
            liftIO $ treeViewAppendColumn tree column
            renderer <- liftIO $ cellRendererTextNew
            liftIO $ treeViewColumnPackStart column renderer True
            liftIO $ cellLayoutSetAttributes column renderer store (\name -> [ cellText := name ])

            liftIO $ set tree [treeViewHeadersVisible := False]

            descriptionBuffer <- newGtkBuffer Nothing ""
            descriptionView   <- newView descriptionBuffer (textviewFont prefs)
            setStyle descriptionBuffer $ case sourceStyle prefs of
                                            (False,_) -> Nothing
                                            (True,v) -> Just v
            descriptionScrolledWindow <- getScrolledWindow descriptionView

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

            cids <- addEventHandling window sourceView tree store isWordChar always

            modifyIDE_ (\ide -> ide{currentState = IsCompleting cids,
                completion = ((width, height), Just (CompletionWindow window tree store))})
            updateOptions window tree store sourceView cids isWordChar always

addEventHandling :: Window -> EditorView -> TreeView -> ListStore String
                 -> (Char -> Bool) -> Bool -> IDEM Connections
addEventHandling window sourceView tree store isWordChar always = do
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
                    tryToUpdateOptions window tree store sourceView True isWordChar always
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
            (_, _, Just c) | isWordChar c -> (do
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

    resizeHandler <- liftIO $ newIORef Nothing

    idButtonPress <- liftIO $ window `on` buttonPressEvent $ do
        button     <- eventButton
        (x, y)     <- eventCoordinates
        time       <- eventTime

        drawWindow <- liftIO $ widgetGetDrawWindow window
        status     <- liftIO $ pointerGrab
            drawWindow
            False
            [PointerMotionMask, ButtonReleaseMask]
            (Nothing:: Maybe DrawWindow)
            Nothing
            time
        when (status == GrabSuccess) $ liftIO $ do
            (width, height) <- windowGetSize window
            writeIORef resizeHandler $ Just $ \(newX, newY) -> do
                reflectIDE (
                    setCompletionSize ((width + (floor (newX - x))), (height + (floor (newY - y))))) ideR

        return True

    idMotion <- liftIO $ window `on` motionNotifyEvent $ do
        mbResize <- liftIO $ readIORef resizeHandler
        case mbResize of
            Just resize -> eventCoordinates >>= (liftIO . resize) >> return True
            Nothing     -> return False

    idButtonRelease <- liftIO $ window `on` buttonReleaseEvent $ do
        mbResize <- liftIO $ readIORef resizeHandler
        case mbResize of
            Just resize -> do
                eventCoordinates >>= (liftIO . resize)
                eventTime >>= (liftIO . pointerUngrab)
                liftIO $ writeIORef resizeHandler Nothing
                return True
            Nothing     -> return False

    idSelected <- liftIO $ tree `onRowActivated` (\treePath column -> (do
        reflectIDE (withWord store treePath (replaceWordStart sourceView isWordChar)) ideR
        liftIO $ postGUIAsync $ reflectIDE cancel ideR))

    return $ concat [cidsPress, cidsRelease, [ConnectC idButtonPress, ConnectC idMotion, ConnectC idButtonRelease, ConnectC idSelected]]

withWord :: ListStore String -> TreePath -> (String -> IDEM ()) -> IDEM ()
withWord store treePath f = (do
   case treePath of
       [row] -> (do
            value <- liftIO $ listStoreGetValue store row
            f value
            )
       _ -> return ()
   )

replaceWordStart :: EditorView -> (Char -> Bool) -> String -> IDEM ()
replaceWordStart sourceView isWordChar name = do
    buffer <- getBuffer sourceView
    (selStart, selEnd) <- getSelectionBounds buffer
    start <- findWordStart selStart isWordChar
    wordStart <- getText buffer start selEnd True
    case stripPrefix wordStart name of
        Just extra -> do
            end <- findWordEnd selEnd isWordChar
            wordFinish <- getText buffer selEnd end True
            case (wordFinish, stripPrefix wordFinish extra) of
                (_:_,Just extra2) -> do
                    selectRange buffer end end
                    insert buffer end extra2
                _                 -> insert buffer selEnd extra
        Nothing    -> return ()

cancelCompletion :: Window -> TreeView -> ListStore String -> Connections -> IDEAction
cancelCompletion window tree store connections = do
    liftIO (do
        listStoreClear (store :: ListStore String)
        signalDisconnectAll connections
        widgetHideAll window
        )
    modifyIDE_ (\ide -> ide{currentState = IsRunning})

updateOptions :: Window -> TreeView -> ListStore String -> EditorView -> Connections -> (Char -> Bool) -> Bool -> IDEAction
updateOptions window tree store sourceView connections isWordChar always = do
    result <- tryToUpdateOptions window tree store sourceView False isWordChar always
    when (not result) $ cancelCompletion window tree store connections

tryToUpdateOptions :: Window -> TreeView -> ListStore String -> EditorView -> Bool -> (Char -> Bool) -> Bool -> IDEM Bool
tryToUpdateOptions window tree store sourceView selectLCP isWordChar always = do
    ideR <- ask
    liftIO $ listStoreClear (store :: ListStore String)
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

findWordStart :: EditorIter -> (Char -> Bool) -> IDEM EditorIter
findWordStart iter isWordChar = do
    maybeWS <- backwardFindCharC iter (not . isWordChar) Nothing
    case maybeWS of
        Nothing -> atOffset iter 0
        Just ws -> forwardCharC ws

findWordEnd :: EditorIter -> (Char -> Bool) -> IDEM EditorIter
findWordEnd iter isWordChar = do
    maybeWE <- forwardFindCharC iter (not . isWordChar) Nothing
    case maybeWE of
        Nothing -> forwardToLineEndC iter
        Just we -> return we

longestCommonPrefix (x:xs) (y:ys) | x == y = x : longestCommonPrefix xs ys
longestCommonPrefix _ _ = []

processResults :: Window -> TreeView -> ListStore String -> EditorView -> String -> [String]
               -> Bool -> (Char -> Bool) -> Bool -> IDEAction
processResults window tree store sourceView wordStart options selectLCP isWordChar always = do
    case options of
        [] -> cancel
        _ | not always && (not . null $ drop 200 options) -> cancel
        _ -> do
            buffer <- getBuffer sourceView
            (selStart, end) <- getSelectionBounds buffer
            start <- findWordStart selStart isWordChar
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
                replaceWordStart sourceView isWordChar newWordStart

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
