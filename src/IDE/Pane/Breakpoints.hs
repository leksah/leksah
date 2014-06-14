{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
   MultiParamTypeClasses, DeriveDataTypeable #-}
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
-- |
--
-----------------------------------------------------------------------------

module IDE.Pane.Breakpoints (
    IDEBreakpoints
,   BreakpointsState
,   showBreakpoints
,   fillBreakpointList
,   selectBreak
) where

import Graphics.UI.Gtk
import Data.Typeable (Typeable(..))
import IDE.Core.State
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import Graphics.UI.Gtk.General.Enums
    (Click(..), MouseButton(..))
import IDE.Debug
    (debugShowBreakpoints,
     debugDeleteBreakpoint,
     debugDeleteAllBreakpoints)
import IDE.LogRef (showSourceSpan)
import Data.List (elemIndex)
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (treeViewContextMenu, __)
import qualified Data.Text as T (unpack)


-- | A breakpoints pane description
--
data IDEBreakpoints    =   IDEBreakpoints {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   breakpoints     ::   TreeStore LogRef
} deriving Typeable

data BreakpointsState  =   BreakpointsState {
}   deriving(Eq,Ord,Read,Show,Typeable)


instance Pane IDEBreakpoints IDEM
    where
    primPaneName _  =   __ "Breakpoints"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Breakpoints"

instance RecoverablePane IDEBreakpoints BreakpointsState IDEM where
    saveState p     =   return (Just BreakpointsState)
    recoverState pp BreakpointsState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = reifyIDE $ \ ideR -> do
        breakpoints <-  treeStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView breakpoints

        rendererA    <- cellRendererTextNew
        colA         <- treeViewColumnNew
        treeViewColumnSetTitle colA (__ "Location")
        treeViewColumnSetSizing colA TreeViewColumnAutosize
        treeViewColumnSetResizable colA True
        treeViewColumnSetReorderable colA True
        treeViewAppendColumn treeView colA
        cellLayoutPackStart colA rendererA False
        cellLayoutSetAttributes colA rendererA breakpoints
            $ \row -> [cellText := showSourceSpan row]

        rendererB    <- cellRendererTextNew
        colB         <- treeViewColumnNew
        treeViewColumnSetTitle colB (__ "Breakpoints")
        treeViewColumnSetSizing colB TreeViewColumnAutosize
        treeViewColumnSetResizable colB True
        treeViewColumnSetReorderable colB True
        treeViewAppendColumn treeView colB
        cellLayoutPackStart colB rendererB False
        cellLayoutSetAttributes colB rendererB breakpoints
            $ \row -> [ cellText := T.unpack $ refDescription row]

        treeViewSetHeadersVisible treeView True
        selB <- treeViewGetSelection treeView
        treeSelectionSetMode selB SelectionSingle
        scrolledView <- scrolledWindowNew Nothing Nothing
        scrolledWindowSetShadowType scrolledView ShadowIn
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic
        let pane = IDEBreakpoints scrolledView treeView breakpoints
        cid1 <- after treeView focusInEvent $ do
            liftIO $ reflectIDE (makeActive pane) ideR
            return True
        (cid2, cid3) <- treeViewContextMenu treeView $ breakpointsContextMenu ideR breakpoints treeView
        cid4 <- treeView `on` rowActivated $ breakpointsSelect ideR breakpoints
        return (Just pane, map ConnectC [cid1, cid2, cid3, cid4])

getBreakpoints :: IDEM IDEBreakpoints
getBreakpoints = forceGetPane (Right "*Breakpoints")

showBreakpoints :: IDEAction
showBreakpoints = do
    pane <- getBreakpoints
    displayPane pane False

fillBreakpointList :: IDEAction
fillBreakpointList = do
    mbBreakpoints <- getPane
    case mbBreakpoints of
        Nothing -> return ()
        Just b  -> do
            refs <- readIDE breakpointRefs
            liftIO $ do
                treeStoreClear (breakpoints b)
                mapM_ (\ (lr,index) -> treeStoreInsert (breakpoints b) [] index lr)
                    (zip refs [0..length refs])

getSelectedBreakpoint ::  TreeView
    -> TreeStore LogRef
    -> IO (Maybe LogRef)
getSelectedBreakpoint treeView treeStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        a:r ->  do
            val     <-  treeStoreGetValue treeStore a
            return (Just val)
        _  ->  return Nothing

selectBreak :: Maybe LogRef -> IDEAction
selectBreak mbLogRef = do
    breakRefs' <- readIDE breakpointRefs
    breaks     <- forceGetPane (Right "*Breakpoints")
    liftIO $ do
        selection <- treeViewGetSelection (treeView breaks)
        case mbLogRef of
            Nothing -> treeSelectionUnselectAll selection
            Just lr -> case lr `elemIndex` breakRefs' of
                        Nothing  -> return ()
                        Just ind -> treeSelectionSelectPath selection [ind]

breakpointsContextMenu :: IDERef
                       -> TreeStore LogRef
                       -> TreeView
                       -> Menu
                       -> IO ()
breakpointsContextMenu ideR store treeView theMenu = do
    item1           <-  menuItemNewWithLabel (__ "Remove breakpoint")
    item1 `on` menuItemActivate $ do
        sel         <-  getSelectedBreakpoint treeView store
        case sel of
            Just ref  -> reflectIDE (deleteBreakpoint ref) ideR
            otherwise -> sysMessage Normal (__ "Debugger>> breakpointViewPopup: no selection2")
    sep1 <- separatorMenuItemNew
    item2           <-  menuItemNewWithLabel (__ "Remove all breakpoints")
    item2 `on` menuItemActivate $ reflectIDE debugDeleteAllBreakpoints ideR
    item3           <-  menuItemNewWithLabel (__ "Update")
    item3 `on` menuItemActivate $ reflectIDE debugShowBreakpoints ideR
    mapM_ (menuShellAppend theMenu) [castToMenuItem item1, castToMenuItem sep1,
        castToMenuItem item2, castToMenuItem item3]


breakpointsSelect :: IDERef
                  -> TreeStore LogRef
                  -> TreePath
                  -> TreeViewColumn
                  -> IO ()
breakpointsSelect ideR store path _ = do
    ref <- treeStoreGetValue store path
    reflectIDE (setCurrentBreak (Just ref)) ideR


deleteBreakpoint :: LogRef -> IDEAction
deleteBreakpoint logRef =
    case logRefType logRef of
        BreakpointRef -> debugDeleteBreakpoint (words (T.unpack $ refDescription logRef) !! 1) logRef
        _   -> sysMessage Normal (__ "Debugger>>deleteBreakpoint: Not a breakpoint")






