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
import Control.Concurrent
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Gdk.Events as Gtk
import IDE.Core.State
import IDE.Metainfo.Provider(getDescription,getCompletionOptions)

complete :: SourceView -> Bool ->  IDEAction
complete sourceView always = do
    currentState' <- readIDE currentState
    prefs'        <- readIDE prefs
    case currentState' of
        IsCompleting window tv ls _ -> updateOptions window tv ls sourceView
        IsRunning                   -> when (always || not (completeRestricted prefs'))
                                            (initCompletion sourceView)
        _                           -> return ()


cancel :: IDEAction
cancel = do
    currentState' <- readIDE currentState
    case currentState' of
        IsCompleting window tv ls c -> cancelCompletion window tv ls c
        _                           -> return ()

initCompletion :: SourceView -> IDEAction
initCompletion sourceView = do
    windows <- getWindows
    prefs      <- readIDE prefs
    (window', tree', store', cids) <- reifyIDE (\ideR -> do
        window <- windowNewPopup
        --set window [ windowTypeHint := WindowTypeHintDialog ] --,
          -- windowDecorated := False ]
        --widgetSetSizeRequest window 700 300
        windowSetTransientFor window (head windows)
        paned <- hPanedNew
        containerAdd window paned
        scrolledWindow <- scrolledWindowNew Nothing Nothing
        widgetSetSizeRequest scrolledWindow 300 300
        containerAdd paned scrolledWindow
        tree <- treeViewNew
        containerAdd scrolledWindow tree
        store <- listStoreNew []
        treeViewSetModel tree store

        font <- case textviewFont prefs of
            Just str -> do
                fontDescriptionFromString str
            Nothing -> do
                f <- fontDescriptionNew
                fontDescriptionSetFamily f "Monospace"
                return f
        widgetModifyFont tree (Just font)

        column <- treeViewColumnNew
        set column [ treeViewColumnSizing := TreeViewColumnAutosize ]
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

        tree `onRowActivated` (\treePath column -> (do
            withWord store treePath (\name -> do
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
                )
            reflectIDE cancel ideR
            ))

        cidPress <- sourceView `onKeyPress` (\event -> do
            let Key { eventKeyName = name, eventModifier = modifier, eventKeyChar = char } = event
            Just model <- treeViewGetModel tree
            selection <- treeViewGetSelection tree
            count <- treeModelIterNChildren model Nothing
            Just column <- treeViewGetColumn tree 0
            case (name, modifier, char) of
--                ("space", [Gtk.Control], _) -> (do
--                    reflectIDE (complete sourceView ) ideR
--                    return True
--                    )
                ("Tab", _, _) -> (do
                    visible <- get tree widgetVisible
                    if visible then (do
                        maybeRow <- getRow tree
                        case maybeRow of
                            Just row -> treeViewRowActivated tree [row] column
                            Nothing -> return ()
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

        return (window, tree, store, [cidPress, cidRelease])
        )

    modifyIDE_ (\ide -> return (ide{currentState = IsCompleting window' tree' store' (map ConnectC cids)}))
    updateOptions window' tree' store' sourceView

    where
       withWord store treePath f = (do
           case treePath of
               [row] -> (do
                    value <- listStoreGetValue store row
                    f value
                    )
               _ -> return ()
           )

cancelCompletion :: TreeViewClass alpha => Window -> alpha ->
    ListStore String -> Connections -> IDEAction
cancelCompletion window tree store connections = do
    liftIO (do
        signalDisconnectAll connections
        widgetHideAll window
        widgetDestroy window
        )
    modifyIDE_ (\ide -> return (ide{currentState = IsRunning}))

updateOptions :: TreeViewClass alpha => Window -> alpha -> ListStore String -> SourceView -> IDEAction
updateOptions window tree store sourceView =
    reifyIDE (\ideR -> do
        buffer <- textViewGetBuffer sourceView
        listStoreClear (store :: ListStore String)
        (start, end) <- textBufferGetSelectionBounds buffer
        isWordEnd <- textIterEndsWord end
        when isWordEnd (do
            moveToWordStart start
            wordStart <- textBufferGetText buffer start end True
            forkIO (do
                options <- reflectIDE (getCompletionOptions wordStart) ideR
                postGUIAsync $ processResults ideR window tree store sourceView wordStart options
                return ()
                )
            return ()
            )
        )

moveToWordStart iter = do
    textIterBackwardWordStart iter
    isStart <- textIterIsStart iter
    when (not isStart) (do
        prev <- textIterCopy iter
        textIterBackwardChar prev
        maybeChar <- textIterGetChar prev
        case maybeChar of
            Just '_' -> (do
                textIterBackwardChar iter
                moveToWordStart iter
                )
            _ -> return ()
        )

processResults ideR window tree store sourceView wordStart options = do
    buffer <- textViewGetBuffer sourceView
    (start, end) <- textBufferGetSelectionBounds buffer
    isWordEnd <- textIterEndsWord end
    when isWordEnd (do
        moveToWordStart start
        newWordStart <- textBufferGetText buffer start end True
        when (isPrefixOf wordStart newWordStart) (do
            listStoreClear store
            forM_ (take 200 (List.filter (isPrefixOf newWordStart) options)) (listStoreAppend store)
            Rectangle startx starty width height <- textViewGetIterLocation sourceView start
            (x, y) <- textViewBufferToWindowCoords sourceView TextWindowWidget (startx, starty+height)
            drawWindow <- widgetGetDrawWindow sourceView
            (ox, oy) <- drawWindowGetOrigin drawWindow
            windowMove window (ox+x) (oy+y)
            when ((length options) == 1) $ treeViewSetCursor tree [0] Nothing
            case options of
                [] -> reflectIDE cancel ideR
                -- [(wordStart,_)] -> (cancel completion) getRow
                _ -> widgetShowAll window
            )
        )

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


