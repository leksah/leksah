{-# OPTIONS_GHC -XRecordWildCards -XTypeSynonymInstances -XMultiParamTypeClasses
    -XDeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Breakpoints
-- Copyright   :  2007-2010 Juergen Nicklisch-Franken, Hamish Mackenzie
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
,   fillBreakpointList
,   selectBreak
) where

import Graphics.UI.Gtk
import Data.Typeable (Typeable(..))
import IDE.Core.State
import Control.Monad.Reader
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import Graphics.UI.Gtk.General.Enums
    (Click(..), MouseButton(..))
import IDE.Debug
    (debugShowBreakpoints,
     debugDeleteBreakpoint,
     debugDeleteAllBreakpoints)
import IDE.LogRef (showSourceSpan)
import Debug.Trace (trace)
import Data.List (elemIndex)


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
    primPaneName _  =   "Breakpoints"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Breakpoints"

instance RecoverablePane IDEBreakpoints BreakpointsState IDEM where
    saveState p     =   do
        return (Just BreakpointsState)
    recoverState pp BreakpointsState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = reifyIDE $ \ ideR -> do
        breakpoints <-  treeStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView breakpoints

        rendererA    <- cellRendererTextNew
        colA         <- treeViewColumnNew
        treeViewColumnSetTitle colA "Location"
        treeViewColumnSetSizing colA TreeViewColumnAutosize
        treeViewColumnSetResizable colA True
        treeViewColumnSetReorderable colA True
        treeViewAppendColumn treeView colA
        cellLayoutPackStart colA rendererA False
        cellLayoutSetAttributes colA rendererA breakpoints
            $ \row -> [cellText := showSourceSpan row]

        rendererB    <- cellRendererTextNew
        colB         <- treeViewColumnNew
        treeViewColumnSetTitle colB "Breakpoints"
        treeViewColumnSetSizing colB TreeViewColumnAutosize
        treeViewColumnSetResizable colB True
        treeViewColumnSetReorderable colB True
        treeViewAppendColumn treeView colB
        cellLayoutPackStart colB rendererB False
        cellLayoutSetAttributes colB rendererB breakpoints
            $ \row -> [ cellText := refDescription row]

        treeViewSetHeadersVisible treeView True
        selB <- treeViewGetSelection treeView
        treeSelectionSetMode selB SelectionSingle
        scrolledView <- scrolledWindowNew Nothing Nothing
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic
        let pane = IDEBreakpoints scrolledView treeView breakpoints
        treeView `onButtonPress` (breakpointViewPopup ideR breakpoints treeView)
        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive pane) ideR ; return True)
        return (Just pane,[ConnectC cid1])

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
getSelectedBreakpoint treeView treeStore = trace "Get selected breakpoint" $do
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

breakpointViewPopup :: IDERef
    -> TreeStore LogRef
    -> TreeView
    -> Event
    -> IO (Bool)
breakpointViewPopup ideR  store treeView (Button _ click _ _ _ _ button _ _)
    = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Remove breakpoint"
            item1 `onActivateLeaf` do
                sel         <-  getSelectedBreakpoint treeView store
                case sel of
                    Just ref      -> reflectIDE (deleteBreakpoint ref) ideR
                    otherwise     -> sysMessage Normal "Debugger>> breakpointViewPopup: no selection2"
            sep1 <- separatorMenuItemNew
            item2           <-  menuItemNewWithLabel "Remove all breakpoints"
            item2 `onActivateLeaf` (reflectIDE debugDeleteAllBreakpoints ideR)
            item3           <-  menuItemNewWithLabel "Update"
            item3 `onActivateLeaf` (reflectIDE debugShowBreakpoints ideR)
            mapM_ (menuShellAppend theMenu) [castToMenuItem item1, castToMenuItem sep1,
                castToMenuItem item2, castToMenuItem item3]
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do sel         <-  getSelectedBreakpoint treeView store
                        case sel of
                            Just ref      -> reflectIDE (setCurrentBreak (Just ref)) ideR
                            otherwise     -> sysMessage Normal "Debugger>> breakpointViewPopup: no selection2"
                        return True
                else return False
breakpointViewPopup _ _ _ _ = throwIDE "breakpointViewPopup wrong event type"


deleteBreakpoint :: LogRef -> IDEAction
deleteBreakpoint logRef =
    case logRefType logRef of
        BreakpointRef -> debugDeleteBreakpoint ((words (refDescription logRef)) !! 1) logRef
        _   -> sysMessage Normal "Debugger>>deleteBreakpoint: Not a breakpoint"






