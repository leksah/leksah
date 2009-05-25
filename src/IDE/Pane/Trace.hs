{-# OPTIONS_GHC -XRecordWildCards -XTypeSynonymInstances -XMultiParamTypeClasses
    -XDeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Trace
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

module IDE.Pane.Trace (
    IDETrace
,   TraceState
,   showTrace
,   fillTracepointList
) where

import Graphics.UI.Gtk
import Data.Typeable (Typeable(..))
import IDE.Core.State
import Control.Monad.Reader
import Outputable (ppr, showSDoc)
import IDE.LogRef (selectRef)
import IDE.Debug (debugHistory)

-- | A debugger pane description
--
data IDETrace    =   IDETrace {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   tracepoints     ::   ListStore LogRef
} deriving Typeable

data TraceState  =   TraceState {
}   deriving(Eq,Ord,Read,Show,Typeable)


instance IDEObject IDETrace

instance Pane IDETrace IDEM
    where
    primPaneName _  =   "Trace"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Trace"
    makeActive pane =   activatePane pane []
    close           =   closePane

instance RecoverablePane IDETrace TraceState IDEM where
    saveState p     =   do
        return (Just TraceState)
    recoverState pp TraceState =   do
        nb      <-  getNotebook pp
        initTrace pp nb

showTrace :: IDEAction
showTrace = do
    debugHistory
    m <- getTrace
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeView m)

getTrace :: IDEM IDETrace
getTrace = do
    mbTrace <- getPane
    case mbTrace of
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (debugPanePath prefs) layout
            nb          <-  getNotebook pp
            initTrace pp nb
            mbTrace <- getPane
            case mbTrace of
                Nothing ->  throwIDE "Can't init breakpoints"
                Just m  ->  return m
        Just m ->   return m

initTrace :: PanePath -> Notebook -> IDEAction
initTrace panePath nb = do
    panes       <- readIDE panes
    paneMap     <- readIDE paneMap
    prefs       <- readIDE prefs
    (pane,cids) <- reifyIDE $ \ideR  ->  do

        tracepoints <-  listStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView tracepoints

        rendererB    <- cellRendererTextNew
        colB         <- treeViewColumnNew
        treeViewColumnSetTitle colB "Trace"
        treeViewColumnSetSizing colB TreeViewColumnAutosize
        treeViewAppendColumn treeView colB
        cellLayoutPackStart colB rendererB False
        cellLayoutSetAttributes colB rendererB tracepoints
            $ \row -> [ cellText := showSDoc (ppr (logRefSrcSpan row))]

        treeViewSetHeadersVisible treeView False
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionSingle

        scrolledView <- scrolledWindowNew Nothing Nothing
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic

        let pane = IDETrace {..}
        notebookInsertOrdered nb scrolledView (paneName pane) Nothing

        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive pane) ideR ; return True)
        sel `onSelectionChanged` do
            sel <- getSelectedTracepoint treeView tracepoints
            case sel of
                Just ref -> reflectIDE (selectRef ref) ideR
                Nothing -> return ()

        return (pane,[ConnectC cid1])
    addPaneAdmin pane cids panePath
    liftIO $ widgetShowAll (scrolledView pane)
    liftIO $ widgetGrabFocus (scrolledView pane)
    liftIO $ bringPaneToFront pane

fillTracepointList :: IDEAction
fillTracepointList = do
    mbTrace <- getPane
    case mbTrace of
        Nothing -> return ()
        Just b  -> do
            refs <- readIDE contextRefs
            liftIO $ do
                --trace ("breakpointList " ++ show refs) $ return ()
                listStoreClear (tracepoints b)
                mapM_ (listStoreAppend (tracepoints b)) refs

getSelectedTracepoint ::  TreeView
    ->  ListStore LogRef
    -> IO (Maybe LogRef)
getSelectedTracepoint treeView listStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        a:r ->  do
            val     <-  listStoreGetValue listStore (head a)
            return (Just val)
        _  ->  return Nothing

