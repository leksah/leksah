{-# LANGUAGE OverloadedStrings #-}
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

import Graphics.UI.Gtk
       (treeSelectionGetSelectedRows,
        treeSelectionSelectionChanged, treeViewGetSelection, rowActivated,
        treeViewSetCursor, treeViewGetCursor, treeModelIterNChildren,
        treeViewGetModel, treeViewRowActivated, treeViewGetColumn,
        widgetShowAll, windowWindowPosition, widgetDestroy, widgetHide,
        listStoreGetValue, keyReleaseEvent,
        treeViewHeadersVisible, cellText, cellLayoutSetAttributes,
        treeViewColumnPackStart, cellRendererTextNew, treeViewAppendColumn,
        treeViewColumnNew, treeViewSetModel, listStoreNew, treeViewNew,
        containerAdd, windowResizable, windowTransientFor,
        windowNewPopup, TreeViewClass, WindowPosition(..),
        signalDisconnect, AttrOp(..), set, EventM, EKey, eventKeyName,
        windowGetSize, windowTypeHint, WindowTypeHint(..), windowDecorated,
        windowDefaultWidth, windowDefaultHeight, scrolledWindowNew)
import IDE.Core.State hiding (window, name)
import Control.Monad (when)
import IDE.Pane.SourceBuffer(recentSourceBuffers)
import Control.Monad.IO.Class (MonadIO(..))
import System.Glib.Signals (on)
import qualified Control.Monad.Reader as Gtk (liftIO)

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
moveFlipperDown :: TreeViewClass alpha => alpha -> IDEAction
moveFlipperDown tree = liftIO $ do
    mbStore <- treeViewGetModel tree
    case mbStore of
        Nothing -> throwIDE "NotebookFlipper>>setFlipper: no store"
        Just store -> do
            n <- treeModelIterNChildren store Nothing
            when (n /= 0) $ do
                (cl, _) <- treeViewGetCursor tree
                case cl of
                    (current:_) ->  let next =  if current == n - 1
                                                    then 0
                                                    else current + 1
                                    in treeViewSetCursor tree [min (n-1) next] Nothing
                    []          ->  treeViewSetCursor tree [1] Nothing

-- | Moves up in the Flipper state
moveFlipperUp :: TreeViewClass alpha => alpha  -> IDEAction
moveFlipperUp tree = liftIO $ do
    mbStore <- treeViewGetModel tree
    case mbStore of
        Nothing -> throwIDE "NotebookFlipper>>setFlipper: no store"
        Just store -> do
            n <- treeModelIterNChildren store Nothing
            when (n /= 0) $ do
                (cl, _) <- treeViewGetCursor tree
                case cl of
                    (current:_) ->  let next =  if current == 0
                                                    then n - 1
                                                    else current - 1
                                    in treeViewSetCursor tree [min (n-1) next] Nothing
                    []          ->  treeViewSetCursor tree [n-1] Nothing

-- | Initiate Filpper , If True moves down, if false up
initFlipper :: Bool -> IDEAction
initFlipper direction = do
    mainWindow   <- getMainWindow
    recentPanes' <-  recentSourceBuffers
    (tree', store') <- reifyIDE $ \ideR -> do
        window <- windowNewPopup
        (_, height) <- windowGetSize mainWindow
        set window [
            windowTypeHint      := WindowTypeHintUtility,
            windowDecorated     := False,
            windowResizable     := True,
            windowDefaultWidth  := 200,
            windowDefaultHeight := height,
            windowTransientFor  := mainWindow]

        scrolledWindow <- scrolledWindowNew Nothing Nothing
        containerAdd window scrolledWindow

        tree <- treeViewNew
        containerAdd scrolledWindow tree

        store <- listStoreNew recentPanes'
        treeViewSetModel tree store
        column <- treeViewColumnNew
        _ <- treeViewAppendColumn tree column
        renderer <- cellRendererTextNew
        treeViewColumnPackStart column renderer True
        cellLayoutSetAttributes column renderer store
            (\str -> [ cellText := str])

        set tree [treeViewHeadersVisible := False]

        cid <- mainWindow `on` keyReleaseEvent $ handleKeyRelease tree ideR

        _ <- tree `on` rowActivated $ \treePath _column -> do
            signalDisconnect cid
            let [row] = treePath
            string <- listStoreGetValue store row
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
            reflectIDE (modifyIDE_ (\ide -> ide{currentState = IsRunning})) ideR

        treeSelection <- treeViewGetSelection tree
        _ <- treeSelection `on` treeSelectionSelectionChanged $ do
            rows <- treeSelectionGetSelectedRows treeSelection
            case rows of
                [[row]] -> do
                    string <- listStoreGetValue store row
                    reflectIDE (do
                        mbPane <- mbPaneFromName string
                        case mbPane of
                            Just (PaneC pane) -> makeActive pane
                            Nothing   -> return ()) ideR
                _ -> return ()

        set window [windowWindowPosition := WinPosCenterOnParent]
        widgetShowAll window
        return (tree, store)
    modifyIDE_ (\ide -> ide{currentState = IsFlipping tree'})
    liftIO $ do
        -- This is done after currentState is set so we know not to update the
        -- previous panes list
        n <- treeModelIterNChildren store' Nothing
        treeViewSetCursor tree' [if direction then min 1 (n - 1) else n - 1] Nothing
    return ()

handleKeyRelease :: TreeViewClass alpha => alpha -> IDERef -> EventM EKey Bool
handleKeyRelease tree ideR = do
    name <- eventKeyName
    Gtk.liftIO $ case name of
        ctrl | (ctrl == "Control_L") || (ctrl == "Control_R") -> do
            currentState' <- reflectIDE (readIDE currentState) ideR
            case currentState' of
                IsFlipping _tv -> do
                    (treePath, _) <- treeViewGetCursor tree
                    Just column <- treeViewGetColumn tree 0
                    treeViewRowActivated tree treePath column
                    return False
                _ -> return False
        _ -> return False


