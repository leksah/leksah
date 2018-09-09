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

import Prelude ()
import Prelude.Compat
import IDE.Core.State hiding (window, name)
import Control.Monad (when)
import IDE.Pane.SourceBuffer(recentSourceBuffers)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Reader as Gtk (liftIO)
import GI.Gtk.Objects.TreeView
       (treeViewRowActivated, treeViewGetColumn, onTreeViewRowActivated,
        treeViewGetSelection, setTreeViewHeadersVisible, treeViewAppendColumn,
        treeViewSetModel, treeViewNew, treeViewSetCursor,
        treeViewGetCursor, treeViewGetModel, IsTreeView)
import GI.Gtk.Interfaces.TreeModel (treeModelIterNChildren)
import GI.Gtk.Objects.Window
       (setWindowWindowPosition, setWindowTransientFor, setWindowDefaultHeight,
        setWindowDefaultWidth, setWindowResizable, setWindowDecorated,
        setWindowTypeHint, windowNew, windowGetSize)
import GI.Gtk.Enums (WindowPosition(..), WindowType(..))
import Data.GI.Base (on, set)
import GI.Gdk.Enums (WindowTypeHint(..))
import GI.Gtk.Objects.ScrolledWindow (scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Container (containerAdd)
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreGetValue, seqStoreNew, SeqStore(..))
import GI.Gtk.Objects.TreeViewColumn
       (noTreeViewColumn, treeViewColumnPackStart, treeViewColumnNew)
import GI.Gtk.Objects.CellRendererText
       (setCellRendererTextText, cellRendererTextNew)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction)
import GI.Gtk.Objects.TreeSelection
       (onTreeSelectionChanged)
import GI.Gtk.Objects.Widget
       (widgetShowAll, widgetDestroy, widgetHide, onWidgetKeyReleaseEvent)
import GI.GObject.Functions (signalHandlerDisconnect)
import GI.Gdk.Structs.EventKey
       (getEventKeyKeyval, EventKey(..))
import GI.Gdk.Functions (keyvalName)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathGetIndices',
        treePathNewFromIndices')

flipDown :: IDEAction
flipDown =
    readIDE flipper >>= maybe (initFlipper True) moveFlipperDown

flipUp :: IDEAction
flipUp =
    readIDE flipper >>= maybe (initFlipper False) moveFlipperUp

-- | Moves down in the Flipper state
moveFlipperDown :: IsTreeView alpha => alpha -> IDEAction
moveFlipperDown tree = do
    Just store <- treeViewGetModel tree
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
moveFlipperUp :: IsTreeView alpha => alpha  -> IDEAction
moveFlipperUp tree = liftIO $ do
    Just store <- treeViewGetModel tree
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
        setWindowTypeHint      window WindowTypeHintUtility
        setWindowDecorated     window False
        setWindowResizable     window True
        setWindowDefaultWidth  window 200
        setWindowDefaultHeight window height
        setWindowTransientFor  window mainWindow

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
        cellLayoutSetDataFunction column renderer store $
            setCellRendererTextText renderer

        setTreeViewHeadersVisible tree False

        cid <- onWidgetKeyReleaseEvent mainWindow (handleKeyRelease tree ideR)

        _ <- onTreeViewRowActivated tree (\treePath _column -> do
            signalHandlerDisconnect mainWindow cid
            [row] <- treePathGetIndices' treePath
            string <- seqStoreGetValue store row
            reflectIDE (do
                mbPane <- mbPaneFromName string
                case mbPane of
                    Just (PaneC pane) -> makeActive pane
                    Nothing   -> return ()) ideR
            widgetHide window
            widgetDestroy window
            reflectIDE (modifyIDE_ (\ide -> ide{flipper = Nothing})) ideR)

        treeSelection <- treeViewGetSelection tree
        _ <- onTreeSelectionChanged treeSelection $ do
            rows <- treeSelectionGetSelectedRows' treeSelection >>= mapM treePathGetIndices'
            case rows of
                [[row]] -> do
                    string <- seqStoreGetValue store row
                    reflectIDE (do
                        mbPane <- mbPaneFromName string
                        case mbPane of
                            Just (PaneC pane) -> do
                                -- Activate the pane but do not update the activePane order yet
                                save <- readIDE (activePane . frameState)
                                modifyIDE_ $ \ide -> ide { frameState = (frameState ide) { activePane = (Nothing, snd save) } }
                                makeActive pane
                                modifyIDE_ $ \ide -> ide { frameState = (frameState ide) { activePane = save } }
                            Nothing   -> return ()) ideR
                _ -> return ()

        setWindowWindowPosition window WindowPositionCenterOnParent
        widgetShowAll window
        return (tree, store)
    modifyIDE_ (\ide -> ide{flipper = Just tree'})
    -- This is done after flipper is set so we know not to update the
    -- previous panes list
    n <- treeModelIterNChildren store' Nothing
    p <- treePathNewFromIndices' [if direction then min 1 (n - 1) else n - 1]
    treeViewSetCursor tree' p noTreeViewColumn False

handleKeyRelease :: IsTreeView alpha => alpha -> IDERef -> EventKey -> IO Bool
handleKeyRelease tree ideR e = do
    name <- getEventKeyKeyval e >>= keyvalName
    case name of
        Just ctrl | (ctrl == "Control_L") || (ctrl == "Control_R") ->
            reflectIDE (readIDE flipper) ideR >>= \case
                Just _tv ->
                    treeViewGetCursor tree >>= \case
                        (Just treePath, _) -> do
                            Just column <- treeViewGetColumn tree 0
                            treeViewRowActivated tree treePath column
                            return False
                        _ -> return False
                _ -> return False
        _ -> return False


