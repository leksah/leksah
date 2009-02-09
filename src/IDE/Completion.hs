-----------------------------------------------------------------------------
--
-- Module      :  IDE.NotebookFlipper
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
import Data.Map as Map
import Data.Set as Set
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Concurrent
import Control.Exception
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.SourceView.SourceView
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Graphics.UI.Gtk.ModelView as New
import Graphics.UI.Gtk.Gdk.Events as Gtk
import IDE.Core.State

complete :: SourceView -> IDEAction
complete sourceView = do
    currentState' <- readIDE currentState
    case currentState' of
        IsCompleting window tv ls _ _ -> updateOptions window tv ls sourceView
        _                  -> initCompletion sourceView

cancel :: IDEAction
cancel = do
    currentState' <- readIDE currentState
    case currentState' of
        IsCompleting window tv ls cidPress cidRelease ->
            cancelCompletion window tv ls cidPress cidRelease
        _ -> return ()

-- TODO work out where these two should go (currently duplicated in Info.hs)
--
-- | Lookup of an identifier description
--
getIdentifierDescr :: String -> SymbolTable -> SymbolTable -> [Descr]
getIdentifierDescr str st1 st2 =
    let r1 = case str `Map.lookup` st1 of
                Nothing -> []
                Just r -> r
        r2 = case str `Map.lookup` st2 of
                Nothing -> []
                Just r -> r
    in r1 ++ r2

--
-- | Lookup of an identifiers starting with the specified prefix and return a list.
--
getIdentifiersStartingWith :: String -> SymbolTable -> SymbolTable -> [String]
getIdentifiersStartingWith prefix st1 st2 =
    takeWhile (isPrefixOf prefix) $
        if memberLocal || memberGlobal then
            prefix : Set.toAscList names
            else
            Set.toAscList names
    where
        (_, memberLocal, localNames) = Set.splitMember prefix (Map.keysSet st1)
        (_, memberGlobal, globalNames) = Set.splitMember prefix (Map.keysSet st2)
        names = Set.union globalNames localNames

getCompletionOptions prefix = do
    currentInfo' <- readIDE currentInfo
    case currentInfo' of
        Nothing -> return []
        Just ((_,symbolTable1),(_,symbolTable2)) ->
            return $ getIdentifiersStartingWith prefix symbolTable1 symbolTable2

getDescription :: String -> IDEM String
getDescription name = do
    currentInfo' <- readIDE currentInfo
    case currentInfo' of
        Nothing -> return ""
        Just ((_,symbolTable1),(_,symbolTable2)) ->
            return $ foldl (\result description ->
                result
                ++ case description of
                    Descr _ _ _ _ _ _ ->
                        (BS.unpack $ typeInfo description)
                        ++ case mbComment description of
                            Just comment -> "\n" ++ (BS.unpack comment)
                            Nothing -> ""
                        ++ "\n"
                    _ -> ""
                ) "" $ getIdentifierDescr name symbolTable1 symbolTable2

initCompletion :: SourceView -> IDEAction
initCompletion sourceView = do
    mainWindow <- readIDE window
    prefs      <- readIDE prefs
    (window', tree', store', cidPress', cidRelease') <- reifyIDE (\ideR -> do
                window <- windowNewPopup
                --set window [ windowTypeHint := WindowTypeHintDialog ] --,
                  -- windowDecorated := False ]
                --widgetSetSizeRequest window 700 300
                windowSetTransientFor window mainWindow
                paned <- hPanedNew
                containerAdd window paned
                scrolledWindow <- scrolledWindowNew Nothing Nothing
                widgetSetSizeRequest scrolledWindow 300 300
                containerAdd paned scrolledWindow
                tree <- New.treeViewNew
                containerAdd scrolledWindow tree
                store <- New.listStoreNew []
                New.treeViewSetModel tree store

                font <- case textviewFont prefs of
                    Just str -> do
                        fontDescriptionFromString str
                    Nothing -> do
                        f <- fontDescriptionNew
                        fontDescriptionSetFamily f "Monospace"
                        return f
                widgetModifyFont tree (Just font)

                column <- New.treeViewColumnNew
                set column [ New.treeViewColumnSizing := New.TreeViewColumnAutosize ]
                New.treeViewAppendColumn tree column
                renderer <- New.cellRendererTextNew
                New.treeViewColumnPackStart column renderer True
                cellLayoutSetAttributes column renderer store (\name -> [ New.cellText := name ])

                set tree [New.treeViewHeadersVisible := False]

                descriptionView <- sourceViewNew
                descriptionBuffer <- (get descriptionView textViewBuffer) >>= (return . castToSourceBuffer)

                widgetModifyFont descriptionView (Just font)

                containerAdd paned descriptionView

                visible <- newIORef False
                activeView <- newIORef Nothing

                -- let completion = Completion window scrolledWindow tree store descriptionBuffer descriptionView activeView getCompletionOptions getDescription

                treeSelection <- New.treeViewGetSelection tree

                treeSelection `onSelectionChanged` (do
                    New.treeSelectionSelectedForeach treeSelection (\treePath -> (do
                        rows <- New.treeSelectionGetSelectedRows treeSelection
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
                        Just model <- New.treeViewGetModel tree
                        selection <- New.treeViewGetSelection tree
                        count <- New.treeModelIterNChildren model Nothing
                        Just column <- New.treeViewGetColumn tree 0
                        case (name, modifier, char) of
                            ("space", [Gtk.Control], _) -> (do
                                reflectIDE (complete sourceView) ideR
                                return True
                                )
                            ("Tab", _, _) -> (do
                                visible <- get tree widgetVisible
                                if visible then (do
                                    maybeRow <- getRow tree
                                    case maybeRow of
                                        Just row -> New.treeViewRowActivated tree [row] column
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
                                            New.treeViewRowActivated tree [row] column
                                            return True
                                            )
                                        Nothing -> (do
                                            widgetHideAll window
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
                                        New.treeSelectionSelectPath selection [newRow]
                                        New.treeViewScrollToCell tree [newRow] column Nothing
                                        -- Crazy hack to avoid the horizontal scroll
                                        New.treeViewScrollToCell tree [newRow] column Nothing
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
                                        New.treeSelectionSelectPath selection [newRow]
                                        New.treeViewScrollToCell tree [newRow] column Nothing
                                        -- Crazy hack to avoid the horizontal scroll
                                        New.treeViewScrollToCell tree [newRow] column Nothing
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
                                visible <- get tree widgetVisible
                                when visible (do
                                    widgetHideAll window
                                    return ()
                                    )
                                return False
                                )
                    )

                cidRelease <- sourceView `onKeyRelease` (\event -> do
                    let Key { eventKeyName = name, eventModifier = modifier, eventKeyChar = char } = event
                    case (name, modifier, char) of
                        ("BackSpace", _, _) -> (do
                            reflectIDE (complete sourceView) ideR
                            return False
                            )
                        _ -> return False
                    )

                return (window, tree, store, cidPress, cidRelease)
                )

    modifyIDE_ (\ide -> return (ide{currentState = IsCompleting window' tree' store' cidPress' cidRelease'}))
    updateOptions window' tree' store' sourceView

    where
       withWord store treePath f = (do
           case treePath of
               [row] -> (do
                    value <- New.listStoreGetValue store row
                    f value
                    )
               _ -> return ()
           )

cancelCompletion :: TreeViewClass alpha => Window -> alpha ->
    ListStore String -> ConnectId SourceView -> ConnectId SourceView -> IDEAction
cancelCompletion window tree store cidPress cidRelease = do
    liftIO (do
        signalDisconnect cidPress
        signalDisconnect cidRelease
        widgetHideAll window
        widgetDestroy window
        )
    modifyIDE_ (\ide -> return (ide{currentState = IsRunning}))

updateOptions :: TreeViewClass alpha => Window -> alpha -> ListStore String -> SourceView -> IDEAction
updateOptions window tree store sourceView =
    reifyIDE (\ideR -> do
        buffer <- textViewGetBuffer sourceView
        New.listStoreClear (store :: ListStore String)
        (start, end) <- textBufferGetSelectionBounds buffer
        isWordEnd <- textIterEndsWord end
        when isWordEnd (do
            moveToWordStart start
            wordStart <- textBufferGetText buffer start end True
            forkIO (do
                options <- reflectIDE (getCompletionOptions wordStart) ideR
                evaluate options
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
            New.listStoreClear store
            forM_ (take 200 (List.filter (isPrefixOf newWordStart) options)) (New.listStoreAppend store)
            Rectangle startx starty width height <- textViewGetIterLocation sourceView start
            (x, y) <- textViewBufferToWindowCoords sourceView TextWindowWidget (startx, starty+height)
            drawWindow <- widgetGetDrawWindow sourceView
            (ox, oy) <- drawWindowGetOrigin drawWindow
            windowMove window (ox+x) (oy+y)
            when ((length options) == 1) $ New.treeViewSetCursor tree [0] Nothing
            case options of
                [] -> reflectIDE cancel ideR
                -- [(wordStart,_)] -> (cancel completion) getRow
                _ -> widgetShowAll window
            )
        )

getRow tree = do
    Just model <- New.treeViewGetModel tree
    selection <- New.treeViewGetSelection tree
    maybeIter <- New.treeSelectionGetSelected selection
    case maybeIter of
        Just iter -> (do
            [row] <- New.treeModelGetPath model iter
            return $ Just row
            )
        Nothing -> return Nothing


