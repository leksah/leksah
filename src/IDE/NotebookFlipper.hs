{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.NotebookFlipper
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

module IDE.NotebookFlipper (
    flipDown
,   flipUp
) where

import IDE.Core.State hiding (window, name)
import Control.Monad (when)
import IDE.Pane.SourceBuffer(recentSourceBuffers)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Reader as Gtk (liftIO)
import GI.Gtk.Objects.TreeView
       (treeViewRowActivated, treeViewGetColumn, onTreeViewRowActivated,
        treeViewGetSelection, treeViewHeadersVisible, treeViewAppendColumn,
        treeViewSetModel, treeViewNew, treeViewSetCursor,
        treeViewGetCursor, treeViewGetModel, TreeViewK)
import GI.Gtk.Interfaces.TreeModel (treeModelIterNChildren)
import GI.Gtk.Objects.Window
       (windowWindowPosition, windowTransientFor, windowDefaultHeight,
        windowDefaultWidth, windowResizable, windowDecorated,
        windowTypeHint, windowNew, windowGetSize)
import GI.Gtk.Enums (WindowPosition(..), WindowType(..))
import Data.GI.Base (on, set)
import Data.GI.Base.Attributes (AttrOp(..))
import GI.Gdk.Enums (WindowTypeHint(..))
import GI.Gtk.Objects.ScrolledWindow (scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Container (containerAdd)
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreGetValue, seqStoreNew, SeqStore(..))
import GI.Gtk.Objects.TreeViewColumn
       (noTreeViewColumn, treeViewColumnPackStart, treeViewColumnNew)
import GI.Gtk.Objects.CellRendererText
       (cellRendererTextText, cellRendererTextNew)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetAttributes)
import GI.Gtk.Objects.TreeSelection
       (onTreeSelectionChanged)
import GI.Gtk.Objects.Widget
       (widgetShowAll, widgetDestroy, widgetHide, onWidgetKeyReleaseEvent)
import GI.GObject.Functions (signalHandlerDisconnect)
import GI.Gdk.Structs.EventKey
       (eventKeyReadKeyval, eventKeyKeyval, EventKey(..))
import GI.Gdk.Functions (keyvalName)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathGetIndices',
        treePathNewFromIndices')

flipDown :: IDEAction
flipDown = do
    currentState' <- readIDE currentState
    case currentState' of
        IsFlipping tv -> moveFlipperDown tv
        IsRunning     -> initFlipper True
        _             -> return ()

flipUp :: IDEAction
flipUp = do
    currentState' <- readIDE currentState
    case currentState' of
        IsFlipping  tv -> moveFlipperUp tv
        IsRunning      -> initFlipper False
        _              -> return ()

-- | Moves down in the Flipper state
moveFlipperDown :: TreeViewK alpha => alpha -> IDEAction
moveFlipperDown tree = do
    store <- treeViewGetModel tree
    n <- treeModelIterNChildren store Nothing
    when (n /= 0) $ do
        indices <- treeViewGetCursor tree >>= \case
            (Just path, _) ->
                treePathGetIndices' path >>= \case
                    (current:_) ->  let next =  if current == n - 1
                                                    then 0
                                                    else current + 1
                                    in return [min (n-1) next]
                    [] -> return [1]
            _ -> return [1]
        p <- treePathNewFromIndices' indices
        treeViewSetCursor tree p noTreeViewColumn False

-- | Moves up in the Flipper state
moveFlipperUp :: TreeViewK alpha => alpha  -> IDEAction
moveFlipperUp tree = liftIO $ do
    store <- treeViewGetModel tree
    n <- treeModelIterNChildren store Nothing
    when (n /= 0) $ do
        indices <- treeViewGetCursor tree >>= \case
            (Just path, _) ->
                treePathGetIndices' path >>= \case
                    (current:_) ->  let next =  if current == 0
                                                    then n - 1
                                                    else current - 1
                                    in return [min (n-1) next]
                    [] -> return [1]
            _ -> return [1]
        p <- treePathNewFromIndices' indices
        treeViewSetCursor tree p noTreeViewColumn False

-- | Initiate Filpper , If True moves down, if false up
initFlipper :: Bool -> IDEAction
initFlipper direction = do
    mainWindow   <- getMainWindow
    recentPanes' <-  recentSourceBuffers
    (tree', store') <- reifyIDE $ \ideR -> do
        window <- windowNew WindowTypePopup
        (_, height) <- windowGetSize mainWindow
        set window [
            windowTypeHint      := WindowTypeHintUtility,
            windowDecorated     := False,
            windowResizable     := True,
            windowDefaultWidth  := 200,
            windowDefaultHeight := height,
            windowTransientFor  := mainWindow]

        scrolledWindow <- scrolledWindowNew noAdjustment noAdjustment
        containerAdd window scrolledWindow

        tree <- treeViewNew
        containerAdd scrolledWindow tree

        store <- seqStoreNew recentPanes'
        treeViewSetModel tree (Just store)
        column <- treeViewColumnNew
        _ <- treeViewAppendColumn tree column
        renderer <- cellRendererTextNew
        treeViewColumnPackStart column renderer True
        cellLayoutSetAttributes column renderer store
            (\str -> [ cellRendererTextText := str])

        set tree [treeViewHeadersVisible := False]

        cid <- onWidgetKeyReleaseEvent mainWindow (handleKeyRelease tree ideR)

        _ <- onTreeViewRowActivated tree (\treePath _column -> do
            signalHandlerDisconnect mainWindow cid
            [row] <- treePathGetIndices' treePath
            string <- seqStoreGetValue store row
            reflectIDE (do
                mbPane <- mbPaneFromName string
                case mbPane of
                    Just (PaneC pane) -> do
                        makeActive pane
                        modifyIDE_ $ \ide -> ide{
                            recentPanes = paneName pane : filter (/= paneName pane) (recentPanes ide)}
                    Nothing   -> return ()) ideR
            widgetHide window
            widgetDestroy window
            reflectIDE (modifyIDE_ (\ide -> ide{currentState = IsRunning})) ideR)

        treeSelection <- treeViewGetSelection tree
        _ <- onTreeSelectionChanged treeSelection $ do
            rows <- treeSelectionGetSelectedRows' treeSelection >>= mapM treePathGetIndices'
            case rows of
                [[row]] -> do
                    string <- seqStoreGetValue store row
                    reflectIDE (do
                        mbPane <- mbPaneFromName string
                        case mbPane of
                            Just (PaneC pane) -> makeActive pane
                            Nothing   -> return ()) ideR
                _ -> return ()

        set window [windowWindowPosition := WindowPositionCenterOnParent]
        widgetShowAll window
        return (tree, store)
    modifyIDE_ (\ide -> ide{currentState = IsFlipping tree'})
    -- This is done after currentState is set so we know not to update the
    -- previous panes list
    n <- treeModelIterNChildren store' Nothing
    p <- treePathNewFromIndices' [if direction then min 1 (n - 1) else n - 1]
    treeViewSetCursor tree' p noTreeViewColumn False

handleKeyRelease :: TreeViewK alpha => alpha -> IDERef -> EventKey -> IO Bool
handleKeyRelease tree ideR e = do
    name <- eventKeyReadKeyval e >>= keyvalName
    case name of
        ctrl | (ctrl == "Control_L") || (ctrl == "Control_R") -> do
            currentState' <- reflectIDE (readIDE currentState) ideR
            case currentState' of
                IsFlipping _tv ->
                    treeViewGetCursor tree >>= \case
                        (Just treePath, _) -> do
                            column <- treeViewGetColumn tree 0
                            treeViewRowActivated tree treePath column
                            return False
                        _ -> return False
                _ -> return False
        _ -> return False


