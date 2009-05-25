{-# OPTIONS_GHC -XRecordWildCards -XTypeSynonymInstances -XMultiParamTypeClasses
    -XDeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Breakpoints
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
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
) where

import Graphics.UI.Gtk
import Data.Typeable (Typeable(..))
import IDE.Core.State
import Control.Monad.Reader
import Outputable (ppr, showSDoc)
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import Graphics.UI.Gtk.General.Enums
    (Click(..), MouseButton(..))
import IDE.Debug
    (debugShowBreakpoints,
     debugDeleteBreakpoint,
     debugDeleteAllBreakpoints)
import IDE.LogRef (selectRef)

-- | A breakpoints pane description
--
data IDEBreakpoints    =   IDEBreakpoints {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   breakpoints     ::   ListStore LogRef
} deriving Typeable

data BreakpointsState  =   BreakpointsState {
}   deriving(Eq,Ord,Read,Show,Typeable)


instance IDEObject IDEBreakpoints

instance Pane IDEBreakpoints IDEM
    where
    primPaneName _  =   "Breakpoints"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Breakpoints"
    makeActive pane =   activatePane pane []
    close           =   closePane

instance RecoverablePane IDEBreakpoints BreakpointsState IDEM where
    saveState p     =   do
        return (Just BreakpointsState)
    recoverState pp BreakpointsState =   do
        nb      <-  getNotebook pp
        initBreakpoints pp nb

showBreakpoints :: IDEAction
showBreakpoints = do
    debugShowBreakpoints
    m <- getBreakpoints
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeView m)

getBreakpoints :: IDEM IDEBreakpoints
getBreakpoints = do
    mbBreakpoints <- getPane
    case mbBreakpoints of
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (debugPanePath prefs) layout
            nb          <-  getNotebook pp
            initBreakpoints pp nb
            mbBreakpoints <- getPane
            case mbBreakpoints of
                Nothing ->  throwIDE "Can't init breakpoints"
                Just m  ->  return m
        Just m ->   return m

initBreakpoints :: PanePath -> Notebook -> IDEAction
initBreakpoints panePath nb = do
    panes       <- readIDE panes
    paneMap     <- readIDE paneMap
    prefs       <- readIDE prefs
    (pane,cids) <- reifyIDE $ \ideR  ->  do

        breakpoints <-  listStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView breakpoints

        rendererB    <- cellRendererTextNew
        colB         <- treeViewColumnNew
        treeViewColumnSetTitle colB "Breakpoints"
        treeViewColumnSetSizing colB TreeViewColumnAutosize
        treeViewAppendColumn treeView colB
        cellLayoutPackStart colB rendererB False
        cellLayoutSetAttributes colB rendererB breakpoints
            $ \row -> [ cellText := showSDoc (ppr (logRefSrcSpan row))]

        treeViewSetHeadersVisible treeView False
        selB <- treeViewGetSelection treeView
        treeSelectionSetMode selB SelectionSingle

        scrolledView <- scrolledWindowNew Nothing Nothing
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic

        let pane = IDEBreakpoints {..}
        notebookInsertOrdered nb scrolledView (paneName pane) Nothing

        treeView `onButtonPress` (breakpointViewPopup ideR breakpoints treeView)

        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive pane) ideR ; return True)

        return (pane,[ConnectC cid1])
    addPaneAdmin pane cids panePath
    liftIO $ widgetShowAll (scrolledView pane)
    liftIO $ widgetGrabFocus (scrolledView pane)
    liftIO $ bringPaneToFront pane

fillBreakpointList :: IDEAction
fillBreakpointList = do
    mbBreakpoints <- getPane
    case mbBreakpoints of
        Nothing -> return ()
        Just b  -> do
            refs <- readIDE breakpointRefs
            liftIO $ do
                --trace ("breakpointList " ++ show refs) $ return ()
                listStoreClear (breakpoints b)
                mapM_ (listStoreAppend (breakpoints b)) refs

breakpointViewPopup :: IDERef
    -> ListStore LogRef
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
            mapM_ (menuShellAppend theMenu) [castToMenuItem item1, castToMenuItem sep1, castToMenuItem item2]
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do sel         <-  getSelectedBreakpoint treeView store
                        case sel of
                            Just ref      -> reflectIDE (selectRef ref) ideR
                            otherwise       -> sysMessage Normal "Debugger>> breakpointViewPopup: no selection2"
                        return True
                else return False
breakpointViewPopup _ _ _ _ = throwIDE "breakpointViewPopup wrong event type"

getSelectedBreakpoint ::  TreeView
    ->  ListStore LogRef
    -> IO (Maybe LogRef)
getSelectedBreakpoint treeView listStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        a:r ->  do
            val     <-  listStoreGetValue listStore (head a)
            return (Just val)
        _  ->  return Nothing

deleteBreakpoint :: LogRef -> IDEAction
deleteBreakpoint logRef =
    case logRefType logRef of
        BreakpointRef -> debugDeleteBreakpoint ((words (refDescription logRef)) !! 1) logRef
        _   -> sysMessage Normal "Debugger>>deleteBreakpoint: Not a breakpoint"



