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
import IDE.Debug (debugCommand', debugHistory)
import IDE.Tool (ToolOutput(..))

-- | A debugger pane description
--
data IDETrace    =   IDETrace {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   tracepoints     ::   TreeStore TraceHist
} deriving Typeable

data TraceState  =   TraceState {
}   deriving(Eq,Ord,Read,Show,Typeable)

data TraceHist = TraceHist {
    str             ::  String}

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
        newPane pp nb builder
        return ()

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
            pp          <-  getBestPathForId "*Trace"
            nb          <-  getNotebook pp
            newPane pp nb builder
            mbTrace <- getPane
            case mbTrace of
                Nothing ->  throwIDE "Can't init breakpoints"
                Just m  ->  return m
        Just m ->   return m

builder :: PanePath ->
    Notebook ->
    Window ->
    IDERef ->
    IO (IDETrace,Connections)
builder pp nb windows ideR = do
    tracepoints <-  treeStoreNew []
    treeView    <-  treeViewNew
    treeViewSetModel treeView tracepoints

    rendererB    <- cellRendererTextNew
    colB         <- treeViewColumnNew
    treeViewColumnSetTitle colB "Trace"
    treeViewColumnSetSizing colB TreeViewColumnAutosize
    treeViewColumnSetResizable colB True
    treeViewColumnSetReorderable colB True
    treeViewAppendColumn treeView colB
    cellLayoutPackStart colB rendererB False
    cellLayoutSetAttributes colB rendererB tracepoints
        $ \row -> [ cellText := str row]

    treeViewSetHeadersVisible treeView True
    sel <- treeViewGetSelection treeView
    treeSelectionSetMode sel SelectionSingle

    scrolledView <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledView treeView
    scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic

    let pane = IDETrace scrolledView treeView tracepoints

    cid1 <- treeView `afterFocusIn`
        (\_ -> do reflectIDE (makeActive pane) ideR ; return True)
    sel `onSelectionChanged` do
        sel <- getSelectedTracepoint treeView tracepoints
        case sel of
            Just ref -> return () -- TODO reflectIDE (selectRef (Just ref)) ideR
            Nothing -> return ()

    return (pane,[ConnectC cid1])

fillTracepointList :: IDEAction
fillTracepointList = do
    mbTraces <- getPane
    case mbTraces of
        Nothing -> return ()
        Just tracePane -> debugCommand' ":history" (\to -> liftIO
                        $ postGUIAsync (do
                            let strings = selectStrings to
                            treeStoreClear (tracepoints tracePane)
                            mapM_ (insertTrace (tracepoints tracePane))
                                        (zip strings [0..length strings])))
    where
    insertTrace treeStore (str,index)  = treeStoreInsert treeStore [] index (TraceHist str)

getSelectedTracepoint ::  TreeView
    -> TreeStore TraceHist
    -> IO (Maybe TraceHist)
getSelectedTracepoint treeView treeStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        a:r ->  do
            val     <-  treeStoreGetValue treeStore a
            return (Just val)
        _  ->  return Nothing

selectStrings :: [ToolOutput] -> [String]
selectStrings (ToolOutput str:r)  = str : selectStrings r
selectStrings (_:r)               = selectStrings r
selectStrings []                  = []
