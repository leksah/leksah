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

import Data.List as List
import Data.Char
import Data.IORef
import Control.Monad
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Gdk.Events as Gtk
import IDE.Core.State
import IDE.Metainfo.Provider(getDescription,getCompletionOptions)
import Control.Monad.Reader.Class (ask)

complete :: SourceView -> Bool ->  IDEAction
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

initCompletion :: SourceView -> IDEAction
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
                set window [ windowTypeHint := WindowTypeHintTooltip,
                             windowDecorated := False ]
                --widgetSetSizeRequest window 700 300
                windowSetTransientFor window (head windows)
                paned          <- hPanedNew
                containerAdd window paned
                scrolledWindow <- scrolledWindowNew Nothing Nothing
                widgetSetSizeRequest scrolledWindow 300 300
                containerAdd paned scrolledWindow
                tree           <- treeViewNew
                containerAdd scrolledWindow tree
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
                    treeViewColumnMinWidth := 301] -- OSX does not like it if there is no hscroll
                treeViewAppendColumn tree column
                renderer <- cellRendererTextNew
                treeViewColumnPackStart column renderer True
                cellLayoutSetAttributes column renderer store (\name -> [ cellText := name ])

                set tree [treeViewHeadersVisible := False]

                descriptionView <- sourceViewNew
                descriptionBuffer <- (get descriptionView textViewBuffer) >>= (return . castToSourceBuffer)
                lm <- sourceLanguageManagerNew
                mbLang <- sourceLanguageManagerGuessLanguage lm Nothing (Just "text/x-haskell")
                case mbLang of
                    Nothing -> return ()
                    Just lang -> do sourceBufferSetLanguage descriptionBuffer lang

                -- This call is here because in the past I have had problems where the
                -- language object became invalid if the manager was garbage collected
                sourceLanguageManagerGetLanguageIds lm

                sourceBufferSetHighlightSyntax descriptionBuffer True
                widgetModifyFont descriptionView (Just font)

                containerAdd paned descriptionView

                visible <- newIORef False
                activeView <- newIORef Nothing

                treeSelection <- treeViewGetSelection tree

                treeSelection `onSelectionChanged` (do
                    treeSelectionSelectedForeach treeSelection (\treePath -> (do
                        rows <- treeSelectionGetSelectedRows treeSelection
                        case rows of
                            [treePath] -> withWord store treePath (\name -> do
                                description <- reflectIDE (getDescription name) ideR
                                textBufferSetText descriptionBuffer description
                                )
                            _ -> return ()
                        ))
                    )

                cids <- liftIO $ addEventHandling window sourceView tree store ideR
                return (window, tree, store, cids)
                )
            modifyIDE_ (\ide -> ide{currentState = IsCompleting cids,
                completion = Just (CompletionWindow window' tree' store')})
            updateOptions window' tree' store' sourceView cids

addEventHandling :: Window -> SourceView -> TreeView -> ListStore String -> IDERef -> IO Connections
addEventHandling window sourceView tree store ideR = do
    cidPress <- sourceView `onKeyPress` (\event -> do
        let Key { eventKeyName = name, eventModifier = modifier, eventKeyChar = char } = event
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
        )

    cidRelease <- sourceView `onKeyRelease` (\event -> do
        let Key { eventKeyName = name, eventModifier = modifier, eventKeyChar = char } = event
        case (name, modifier, char) of
            ("BackSpace", _, _) -> (do
                reflectIDE (complete sourceView False) ideR
                return False
                )
            _ -> return False
        )
    cidSelected <- tree `onRowActivated` (\treePath column -> (do
        withWord store treePath (replaceWordStart sourceView)
        reflectIDE cancel ideR))
    return [ConnectC cidPress,ConnectC cidRelease, ConnectC cidSelected]

withWord store treePath f = (do
   case treePath of
       [row] -> (do
            value <- listStoreGetValue store row
            f value
            )
       _ -> return ()
   )

replaceWordStart sourceView name = do
    buffer <- textViewGetBuffer sourceView
    (start, end) <- textBufferGetSelectionBounds buffer
    isWordEnd <- textIterEndsWord end
    if isWordEnd then (do
        moveToWordStart start
        wordStart <- textBufferGetText buffer start end True
        if (isPrefixOf wordStart name) then (do
            textBufferDelete buffer start end
            textBufferInsert buffer start name
            )
            else return ()
        )
        else return ()

cancelCompletion :: TreeViewClass alpha => Window -> alpha ->
    ListStore String -> Connections -> IDEAction
cancelCompletion window tree store connections = do
    liftIO (do
        listStoreClear (store :: ListStore String)
        signalDisconnectAll connections
        widgetHideAll window
        )
    modifyIDE_ (\ide -> ide{currentState = IsRunning})

updateOptions :: TreeViewClass alpha => Window -> alpha -> ListStore String -> SourceView -> Connections -> IDEAction
updateOptions window tree store sourceView connections = do
    result <- tryToUpdateOptions window tree store sourceView False
    when (not result) $ cancelCompletion window tree store connections

tryToUpdateOptions :: TreeViewClass alpha => Window -> alpha -> ListStore String -> SourceView -> Bool -> IDEM Bool
tryToUpdateOptions window tree store sourceView selectLCP =
    reifyIDE (\ideR -> do
        buffer <- textViewGetBuffer sourceView
        listStoreClear (store :: ListStore String)
        (start, end) <- textBufferGetSelectionBounds buffer
        moveToWordStart start
        equal <- textIterEqual start end
        if equal
            then return False
            else do
                wordStart <- textBufferGetText buffer start end True
                postGUIAsync $ do
                    options <- reflectIDE (getCompletionOptions wordStart) ideR
                    processResults ideR window tree store sourceView wordStart options selectLCP
                    return ()
                return True)

moveToWordStart iter = do
    textIterBackwardWordStart iter
    isStart <- textIterIsStart iter
    when (not isStart) $ do
        prev <- textIterCopy iter
        textIterBackwardChar prev
        maybeChar <- textIterGetChar prev
        case maybeChar of
            Just '_' -> (do
                textIterBackwardChar iter
                moveToWordStart iter
                )
            _ -> return ()

longestCommonPrefix (x:xs) (y:ys) | x == y = x : longestCommonPrefix xs ys
longestCommonPrefix _ _ = []

processResults ideR window tree store sourceView wordStart options selectLCP = do
    case options of
        [] -> reflectIDE cancel ideR
        _  -> do
            buffer <- textViewGetBuffer sourceView
            (start, end) <- textBufferGetSelectionBounds buffer
            isWordEnd <- textIterEndsWord end
            when isWordEnd (do
                moveToWordStart start
                newWordStart <- do
                    currentWordStart <- textBufferGetText buffer start end True
                    if selectLCP && currentWordStart == wordStart && (not $ null options)
                        then do
                            let lcp = foldl1 longestCommonPrefix options
                            when (lcp /= wordStart) $ do
                                replaceWordStart sourceView lcp
                            return lcp
                        else
                            return currentWordStart

                when (isPrefixOf wordStart newWordStart) (do
                    listStoreClear store
                    let newOptions = List.filter (isPrefixOf newWordStart) options
                    forM_ (take 200 newOptions) (listStoreAppend store)
                    Rectangle startx starty width height <- textViewGetIterLocation sourceView start
                    (x, y)                               <- textViewBufferToWindowCoords sourceView TextWindowWidget (startx, starty+height)
                    drawWindow                           <- widgetGetDrawWindow sourceView
                    (ox, oy)                             <- drawWindowGetOrigin drawWindow
                    windowMove window (ox+x) (oy+y)
                    when (not $ null newOptions) $ treeViewSetCursor tree [0] Nothing
                    widgetShowAll window))

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
