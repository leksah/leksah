{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Breakpoints
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | A pane to display the set breakpoints
--
-----------------------------------------------------------------------------

module IDE.Pane.Breakpoints (
    IDEBreakpoints
,   BreakpointsState
,   showBreakpoints
,   fillBreakpointList
,   selectBreak
) where

import Prelude ()
import Prelude.Compat
import Data.Typeable (Typeable(..))
import IDE.Core.State
import IDE.Debug
    (debugShowBreakpoints,
     debugDeleteBreakpoint,
     debugDeleteAllBreakpoints)
import IDE.LogRef (showSourceSpan)
import Data.List (elemIndex)
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (treeViewContextMenu, __)
import qualified Data.Text as T (words, unpack)
import qualified Data.Foldable as F (toList)
import qualified Data.Sequence as Seq (elemIndexL)
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew, ScrolledWindow(..))
import GI.Gtk.Objects.TreeView
       (onTreeViewRowActivated, treeViewGetSelection,
        treeViewSetHeadersVisible, treeViewAppendColumn, treeViewSetModel,
        treeViewNew, TreeView(..))
import Data.GI.Gtk.ModelView.ForestStore
       (forestStoreGetValue, ForestStore(..),
        forestStoreInsert, forestStoreClear, forestStoreNew)
import GI.Gtk.Objects.Widget (afterWidgetFocusInEvent, toWidget)
import GI.Gtk.Objects.CellRendererText
       (setCellRendererTextText, cellRendererTextNew)
import GI.Gtk.Objects.TreeViewColumn
       (TreeViewColumn(..), treeViewColumnSetReorderable,
        treeViewColumnSetResizable, treeViewColumnSetSizing,
        treeViewColumnSetTitle, treeViewColumnNew)
import GI.Gtk.Enums
       (PolicyType(..), ShadowType(..), SelectionMode(..),
        TreeViewColumnSizing(..))
import GI.Gtk.Interfaces.CellLayout (cellLayoutPackStart)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction)
import GI.Gtk.Objects.TreeSelection
       (treeSelectionSelectPath, treeSelectionUnselectAll,
        treeSelectionSetMode)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.Menu (Menu(..))
import GI.Gtk.Objects.SeparatorMenuItem (separatorMenuItemNew, SeparatorMenuItem(..))
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import GI.Gtk.Structs.TreePath
       (TreePath(..))
import Control.Monad.Reader (MonadReader(..))
import GI.Gtk.Objects.MenuItem
       (toMenuItem, onMenuItemActivate, menuItemNewWithLabel)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathNewFromIndices')


-- | Represents the Breakpoints pane
data IDEBreakpoints    =   IDEBreakpoints {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   breakpoints     ::   ForestStore LogRef
} deriving Typeable


-- | The additional state used when recovering the pane
data BreakpointsState  =   BreakpointsState
    deriving(Eq,Ord,Read,Show,Typeable)


instance Pane IDEBreakpoints IDEM
    where
    primPaneName _  =   __ "Breakpoints"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . scrolledView
    paneId b        =   "*Breakpoints"


instance RecoverablePane IDEBreakpoints BreakpointsState IDEM where
    saveState p     =   return (Just BreakpointsState)
    recoverState pp BreakpointsState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = do
        ideR <- ask
        breakpoints <-  forestStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView (Just breakpoints)

        rendererA    <- cellRendererTextNew
        colA         <- treeViewColumnNew
        treeViewColumnSetTitle colA (__ "Location")
        treeViewColumnSetSizing colA TreeViewColumnSizingAutosize
        treeViewColumnSetResizable colA True
        treeViewColumnSetReorderable colA True
        treeViewAppendColumn treeView colA
        cellLayoutPackStart colA rendererA False
        cellLayoutSetDataFunction colA rendererA breakpoints
            $ \row -> setCellRendererTextText rendererA $ showSourceSpan row

        rendererB    <- cellRendererTextNew
        colB         <- treeViewColumnNew
        treeViewColumnSetTitle colB (__ "Breakpoints")
        treeViewColumnSetSizing colB TreeViewColumnSizingAutosize
        treeViewColumnSetResizable colB True
        treeViewColumnSetReorderable colB True
        treeViewAppendColumn treeView colB
        cellLayoutPackStart colB rendererB False
        cellLayoutSetDataFunction colB rendererB breakpoints
            $ setCellRendererTextText rendererB . refDescription

        treeViewSetHeadersVisible treeView True
        selB <- treeViewGetSelection treeView
        treeSelectionSetMode selB SelectionModeSingle
        scrolledView <- scrolledWindowNew noAdjustment noAdjustment
        scrolledWindowSetShadowType scrolledView ShadowTypeIn
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyTypeAutomatic PolicyTypeAutomatic
        let pane = IDEBreakpoints scrolledView treeView breakpoints
        cid1 <- onIDE afterWidgetFocusInEvent treeView $ do
            liftIDE $ makeActive pane
            return True
        cids2 <- treeViewContextMenu treeView $ breakpointsContextMenu ideR breakpoints treeView
        cid4 <- ConnectC treeView <$> onTreeViewRowActivated treeView (breakpointsSelect ideR breakpoints)
        return (Just pane, [cid1, cid4] ++ cids2)


-- | Get the Breakpoints pane
getBreakpoints :: IDEM IDEBreakpoints
getBreakpoints = forceGetPane (Right "*Breakpoints")


-- | Display the Breakpoints pane
showBreakpoints :: IDEAction
showBreakpoints = do
    pane <- getBreakpoints
    displayPane pane False


-- | Fill the pane with the breakpoint 'LogRef's from the IDE state
fillBreakpointList :: IDEAction
fillBreakpointList = do
    mbBreakpoints <- getPane
    case mbBreakpoints of
        Nothing -> return ()
        Just b  -> do
            refs <- readIDE breakpointRefs
            forestStoreClear (breakpoints b)
            emptyPath <- treePathNewFromIndices' []
            mapM_ (\ (lr,index) -> forestStoreInsert (breakpoints b) emptyPath index lr)
                (zip (F.toList refs) [0..])


-- | Try to get the currently selected breakpoint
getSelectedBreakpoint ::  TreeView
    -> ForestStore LogRef
    -> IO (Maybe LogRef)
getSelectedBreakpoint treeView forestStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows' treeSelection
    case paths of
        a:r ->  do
            val     <-  forestStoreGetValue forestStore a
            return (Just val)
        _  ->  return Nothing


-- | Select a specific 'LogRef' or none at all
selectBreak :: Maybe LogRef  -- If @Nothing@, no breakpoints are selected
            -> IDEAction
selectBreak mbLogRef = do
    breakRefs' <- readIDE breakpointRefs
    breaks     <- forceGetPane (Right "*Breakpoints")
    selection <- treeViewGetSelection (treeView breaks)
    case mbLogRef of
        Nothing -> treeSelectionUnselectAll selection
        Just lr -> case lr `Seq.elemIndexL` breakRefs' of
                    Nothing  -> return ()
                    Just ind -> treeSelectionSelectPath selection =<< treePathNewFromIndices' [fromIntegral ind]


-- | Constructs the context menu for the breakpoint
breakpointsContextMenu :: IDERef
                       -> ForestStore LogRef
                       -> TreeView
                       -> Menu
                       -> IO ()
breakpointsContextMenu ideR store treeView theMenu = do
    item1 <- menuItemNewWithLabel (__ "Remove breakpoint")
    onMenuItemActivate item1 $ do
        sel <- getSelectedBreakpoint treeView store
        case sel of
            Just ref  -> reflectIDE (deleteBreakpoint ref) ideR
            otherwise -> sysMessage Normal (__ "Debugger>> breakpointViewPopup: no selection2")
    sep1 <- separatorMenuItemNew >>= liftIO . toMenuItem
    item2 <- menuItemNewWithLabel (__ "Remove all breakpoints")
    onMenuItemActivate item2 $ reflectIDE debugDeleteAllBreakpoints ideR
    item3 <- menuItemNewWithLabel (__ "Update")
    onMenuItemActivate item3 $ reflectIDE debugShowBreakpoints ideR
    mapM_ (menuShellAppend theMenu) [item1, sep1, item2, item3]


-- | Set the current breakpoint to a specific entry
--   pointed to by the supplied 'TreePath'
breakpointsSelect :: IDERef
                  -> ForestStore LogRef
                  -> TreePath
                  -> TreeViewColumn
                  -> IO ()
breakpointsSelect ideR store path _ = do
    ref <- forestStoreGetValue store path
    reflectIDE (setCurrentBreak (Just ref)) ideR


-- | Remove a breakpoint from the pane
deleteBreakpoint :: LogRef -> IDEAction
deleteBreakpoint logRef =
    case logRefType logRef of
        BreakpointRef -> debugDeleteBreakpoint (T.words (refDescription logRef) !! 1) logRef
        _   -> sysMessage Normal (__ "Debugger>>deleteBreakpoint: Not a breakpoint")






