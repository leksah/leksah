{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module IDE.Pane.LeksahState where

import System.Log.Logger (debugM)

import Data.Text as T (pack, Text, null)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Int (Int32)
import Data.Maybe
import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.IORef

import Data.GI.Gtk.ModelView.ForestStore
import Data.GI.Gtk.ModelView.CellLayout
import Data.GI.Gtk.ModelView.Types
import Data.GI.Gtk.ModelView.CustomStore
import GI.Gtk.Enums
import GI.Gtk.Objects.Box
import GI.Gtk.Objects.Button
import GI.Gtk.Objects.Label
import GI.Gtk.Objects.TreeView
import GI.Gtk.Objects.TreeViewColumn
import GI.Gtk.Objects.Widget
import GI.Gtk.Objects.ScrolledWindow
import GI.Gtk.Objects.Container
import GI.Gtk.Objects.CellRendererText
import GI.Gtk.Objects.Adjustment
import GI.Gtk.Objects.ScrolledWindow


import IDE.Core.State
import IDE.Pane.LeksahState.Inspectable
import IDE.Utils.GUIUtils (__)
import Graphics.UI.Frame.Panes


data LeksahStatePane = LeksahStatePane
    { box :: Box
    }

data TreeItem = TreeItem {itemLeft :: Text, itemRight :: Text}

formatItem :: TreeItem -> Text
formatItem (TreeItem itemLeft itemRight)
    | T.null itemLeft = itemRight
    | otherwise       = itemLeft <> " = " <> itemRight

data LeksahStatePaneState = LeksahStatePaneState
    deriving (Show, Read, Eq)

instance Pane LeksahStatePane IDEM where
    primPaneName _  =   __ "Leksah State"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . box
    paneId b        =   "*LeksahState"

instance RecoverablePane LeksahStatePane LeksahStatePaneState IDEM where
    saveState p = return Nothing
    recoverState pp _ =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = do
        ideVar <- liftIDE ask
        pane <- mkStatePane (readMVar ideVar)
        return (Just pane, [])

displayStatePane :: IDEAction
displayStatePane = do
    pane <- forceGetPane (Right "*Files")
    displayPane (pane :: LeksahStatePane) False

mkStatePane :: Inspectable a => IO a -> IDEM LeksahStatePane
mkStatePane getValue = do
    store <- forestStoreNew []

    treeView <-  treeViewNew
    treeViewSetModel treeView (Just store)
    treeViewSetHeadersVisible treeView False
    treeViewSetEnableSearch treeView True
    treeViewSetEnableTreeLines treeView True

    col <- treeViewColumnNew
    treeViewColumnSetSizing col TreeViewColumnSizingAutosize
    treeViewColumnSetResizable col True
    treeViewColumnSetReorderable col False
    treeViewAppendColumn treeView col

    renderer <- cellRendererTextNew
    setCellRendererTextFont renderer "Mono"
    cellLayoutPackStart col renderer True
    cellLayoutSetDataFunction col renderer store $ \item -> do
        setCellRendererTextText renderer (formatItem item)

    liftIO $ refreshView treeView store

    -- Scrolled view
    scrolledView <- scrolledWindowNew noAdjustment noAdjustment
    scrolledWindowSetShadowType scrolledView ShadowTypeIn
    scrolledWindowSetPolicy scrolledView PolicyTypeAutomatic PolicyTypeAutomatic
    containerAdd scrolledView treeView

    buttonBox <- boxNew OrientationHorizontal 0
    refreshButton <- buttonNewWithLabel "Refresh"
    boxPackStart buttonBox refreshButton False False 0
    onButtonClicked refreshButton $
         refreshView treeView store

    -- Box, top-level widget of the pane
    box <- boxNew OrientationVertical 0
    boxPackStart box buttonBox False True 0
    boxPackStart box scrolledView True True 0

    let pane = LeksahStatePane {box}

    return pane
    where
        refreshView treeView store = do
            x <- getValue
            populateStore store x
            path <- treePathNewFromIndices' [0]
            treeViewExpandToPath treeView path

populateStore :: Inspectable a => ForestStore TreeItem -> a -> IO ()
populateStore store x = do
    forestStoreClear store
    populateVal [] 0 (toInspectorNode x)
    where
        populateVal path index (InspectorNode str []) = insertItem path index (TreeItem "" str)
        populateVal path index (InspectorNode name fields) = do
            insertItem path index (TreeItem "" name)
            for_ (zip fields [0 .. ]) $ \(field, n) -> do
                populateField (path ++ [fromIntegral index]) n field

        populateField path index (InspectorProperty mName (InspectorNode str [])) = insertItem path index (TreeItem (fromMaybe "" mName) str)
        populateField path index (InspectorProperty mFieldName (InspectorNode constrName fields)) = do
            insertItem path index (TreeItem (fromMaybe "" mFieldName) constrName)
            for_ (zip fields [0 ..]) $ \(field, n) -> do
                populateField (path ++ [fromIntegral index]) n field

        insertItem path index item = do
            path' <- treePathNewFromIndices' path
            forestStoreInsert store path' index item