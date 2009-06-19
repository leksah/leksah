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
,   showBreakpointList
,   fillBreakpointList
,   selectBreak

,   showDebugger
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
import IDE.Pane.Errors (showErrors')
import IDE.Pane.Variables (showVariables')
import Graphics.UI.Editor.Parameters (Direction(..))
import IDE.Pane.SourceBuffer (newTextBuffer)

-- | A breakpoints pane description
--
data IDEBreakpoints    =   IDEBreakpoints {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   breakpoints     ::   TreeStore LogRef
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
        newPane pp nb builder
        return ()

showBreakpointList :: IDEAction
showBreakpointList = trace "showBreakpointList!!" $ do
--    debugShowBreakpoints
    m <- trace "now get breakpoints" getBreakpoints
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeView m)

getBreakpoints :: IDEM IDEBreakpoints
getBreakpoints = do
    mbBreakpoints <- getPane
    case mbBreakpoints of
        Nothing -> do
            pp          <-  getBestPathForId "*Breakpoints"
            nb          <-  getNotebook pp
            newPane pp nb builder
            mbBreakpoints <- getPane
            case mbBreakpoints of
                Nothing ->  throwIDE "Can't init breakpoints"
                Just m  ->  return m
        Just m ->   return m

showBreakpointList' :: PanePath -> IDEAction
showBreakpointList' pp = do
--    debugShowBreakpoints
    m <- getBreakpoints' pp
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeView m)

getBreakpoints' :: PanePath -> IDEM IDEBreakpoints
getBreakpoints' pp = do
    mbBreakpoints <- getPane
    case mbBreakpoints of
        Nothing -> do
            layout        <- getLayout
            nb            <-  getNotebook (getBestPanePath pp layout)
            newPane pp nb builder
            mbBreakpoints <- getPane
            case mbBreakpoints of
                Nothing ->  throwIDE "Can't init breakpoints"
                Just m  ->  return m
        Just m ->   return m

builder :: PanePath ->
    Notebook ->
    Window ->
    IDERef ->
    IO (IDEBreakpoints, Connections)
builder pp nb windows ideR = do
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
    return (pane,[ConnectC cid1])

fillBreakpointList :: IDEAction
fillBreakpointList = do
    mbBreakpoints <- getPane
    case mbBreakpoints of
        Nothing -> return ()
        Just b  -> do
            refs <- readIDE breakpointRefs
            liftIO $ do
                treeStoreClear (breakpoints b)
                mapM_ (insertBreak (breakpoints b)) (zip refs [0..length refs])
    where
        insertBreak treeStore (lr,index) = treeStoreInsert treeStore [] index lr

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
    breaks     <- getBreakpoints
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

showDebugger :: IDEAction
showDebugger = do
    pp   <- panePathForGroup "*Debug"
    ret  <- newGroupOrBringToFront "Debug" pp
    case ret of
        (Just rpp, True) -> do
            viewSplit' rpp Horizontal
            let lowerP =  rpp ++ [SplitP BottomP]
            let upperP =  rpp ++ [SplitP TopP]
            lower <- getNotebook lowerP
            upper <- getNotebook upperP
            liftIO $ do
                notebookSetTabPos lower PosLeft
                notebookSetTabPos upper PosLeft
                notebookSetShowTabs upper False
            showBreakpointList' (rpp ++ [SplitP BottomP])
            showErrors' (rpp ++ [SplitP BottomP])
            showVariables' (rpp ++ [SplitP BottomP])
            newTextBuffer (rpp ++ [SplitP TopP]) "EvalL.hs" Nothing
            return ()
        (Just rpp, False) -> do
            let lowerP =  rpp ++ [SplitP BottomP]
            let upperP =  rpp ++ [SplitP TopP]
            showBreakpointList' lowerP
            showErrors' lowerP
            showVariables' lowerP
            newTextBuffer upperP "EvalL.hs" Nothing
            return ()
        _ -> return ()




