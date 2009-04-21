{-# OPTIONS_GHC -XScopedTypeVariables -XTypeSynonymInstances -XMultiParamTypeClasses
    -XDeriveDataTypeable#-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Debugger
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :  hasConfigs
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Pane.Debugger (
    IDEDebugger
,   DebuggerState
,   showDebugger
,   updateDebugger
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Data.Typeable (Typeable(..))
import IDE.Core.State
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk.General.Enums
    (Click(..),
     MouseButton(..),
     Packing(..),
     PolicyType(..),
     ButtonBoxStyle(..))
import Graphics.UI.Gtk.Gdk.Enums (EventMask(..))
import System.Glib.Attributes (get)
import IDE.LogRef (selectRef, logOutput)
import Outputable (ppr, showSDoc)
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import IDE.Debug
    (debugCommand',
     debugTraceExpr,
     debugStepExpr,
     debugStepModule,
     debugStepLocal,
     debugDeleteBreakpoint,
     debugContinue,
     debugStep,
     debugDeleteAllBreakpoints,
     debugCommand)
import Control.Monad (unless, when)
import Control.Event (triggerEvent)
import IDE.Tool (ToolOutput(..),toolline)
import IDE.SourceCandy (getCandylessText)


-- | A debugger pane description
--
data IDEDebugger    =   IDEDebugger {
    sw              ::   VBox
,   workspaceView   ::   SourceView
,   breakpoints     ::   ListStore LogRef
,   variables     ::   ListStore String
,   hpaned      ::  HPaned
,   vpaned      ::  VPaned
} deriving Typeable

data DebuggerState  =   DebuggerState {
    workspace :: String
,   horizontalSplit :: Int
,   verticalSplit :: Int
}   deriving(Eq,Ord,Read,Show,Typeable)


instance IDEObject IDEDebugger

instance Pane IDEDebugger IDEM
    where
    primPaneName _  =   "Debug"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . sw
    paneId b        =   "*Debug"
    makeActive pane =   activatePane pane []
    close           =   closePane

instance RecoverablePane IDEDebugger DebuggerState IDEM where
    saveState p     =   do
        ct          <-  readIDE candy
        liftIO $ do
            buf         <-  textViewGetBuffer $ workspaceView p
            text        <-  getCandylessText ct buf
            hi          <-  panedGetPosition (hpaned p)
            vi          <-  panedGetPosition (vpaned p)
            return (Just (DebuggerState text hi vi))
    recoverState pp (DebuggerState text hi vi) =   do
        prefs       <-  readIDE prefs
        layout      <-  readIDE layout
        let pp      =   getStandardPanePath (modulesPanePath prefs) layout
        nb          <-  getNotebook pp
        initDebugger pp nb text
        debugger    <- getDebugger
        liftIO $ panedSetPosition (hpaned debugger) hi
        liftIO $ panedSetPosition (vpaned debugger) vi

showDebugger :: IDEAction
showDebugger = do
    m <- getDebugger
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (workspaceView m)

getDebugger :: IDEM IDEDebugger
getDebugger = do
    mbDeb <- getPane
    case mbDeb of
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (modulesPanePath prefs) layout
            nb          <-  getNotebook pp
            initDebugger pp nb ""
            mbDeb <- getPane
            case mbDeb of
                Nothing ->  throwIDE "Can't init debugger"
                Just m  ->  return m
        Just m ->   return m

initDebugger :: PanePath -> Notebook -> String -> IDEAction
initDebugger panePath nb wstext = do
    panes       <- readIDE panes
    paneMap     <- readIDE paneMap
    prefs       <- readIDE prefs
    (pane,cids) <- reifyIDE $ \ideR  ->  do
        ibox        <- vBoxNew False 0
    -- Buttons
        bb          <- hButtonBoxNew
        buttonBoxSetLayout bb ButtonboxSpread
        stepB <- buttonNewWithLabel "Step"
        stepLB <- buttonNewWithLabel "StepLocal"
        stepMB <- buttonNewWithLabel "StepModule"
        continueB <- buttonNewWithLabel "Continue"

        boxPackStartDefaults bb stepB
        boxPackStartDefaults bb stepLB
        boxPackStartDefaults bb stepMB
        boxPackStartDefaults bb continueB

    -- BreakpointView 1 + 2
        listStoreB   <-  listStoreNew []
        treeViewB    <-  treeViewNew
        treeViewSetModel treeViewB listStoreB

        rendererB    <- cellRendererTextNew
        colB         <- treeViewColumnNew
        treeViewColumnSetTitle colB "Breakpoints"
        treeViewColumnSetSizing colB TreeViewColumnAutosize
        treeViewAppendColumn treeViewB colB
        cellLayoutPackStart colB rendererB False
        cellLayoutSetAttributes colB rendererB listStoreB
            $ \row -> [ cellText := showSDoc (ppr (logRefSrcSpan row))]

        treeViewSetHeadersVisible treeViewB False
        selB <- treeViewGetSelection treeViewB
        treeSelectionSetMode selB SelectionSingle

        swBreak <- scrolledWindowNew Nothing Nothing
        containerAdd swBreak treeViewB
        scrolledWindowSetPolicy swBreak PolicyAutomatic PolicyAutomatic

    -- VariablesView
        listStoreV   <-  listStoreNew []
        treeViewV    <-  treeViewNew
        treeViewSetModel treeViewV listStoreV

        rendererV    <- cellRendererTextNew
        colV         <- treeViewColumnNew
        treeViewColumnSetTitle colV "Variables"
        treeViewColumnSetSizing colV TreeViewColumnAutosize
        treeViewAppendColumn treeViewV colV
        cellLayoutPackStart colV rendererV False
        cellLayoutSetAttributes colV rendererV listStoreV
            $ \row -> [ cellText := row]

        treeViewSetHeadersVisible treeViewV True
        selV <- treeViewGetSelection treeViewV
        treeSelectionSetMode selV SelectionSingle

        swVariables <- scrolledWindowNew Nothing Nothing
        containerAdd swVariables treeViewV
        scrolledWindowSetPolicy swVariables PolicyAutomatic PolicyAutomatic

    -- TracesView
        listStoreT   <-  listStoreNew []
        treeViewT    <-  treeViewNew
        treeViewSetModel treeViewT listStoreT

        rendererT    <- cellRendererTextNew
        colT         <- treeViewColumnNew
        treeViewColumnSetTitle colT "Traces"
        treeViewColumnSetSizing colT TreeViewColumnAutosize
        treeViewAppendColumn treeViewT colT
        cellLayoutPackStart colT rendererT False
        cellLayoutSetAttributes colT rendererT listStoreT
            $ \row -> [ cellText := row]

        treeViewSetHeadersVisible treeViewT False
        selT <- treeViewGetSelection treeViewT
        treeSelectionSetMode selT SelectionSingle

        swTraces <- scrolledWindowNew Nothing Nothing
        containerAdd swTraces treeViewT
        scrolledWindowSetPolicy swTraces PolicyAutomatic PolicyAutomatic

    -- Workspace View
        wbox        <- hBoxNew False 0
        font <- case textviewFont prefs of
            Just str -> do
                fontDescriptionFromString str
            Nothing -> do
                f <- fontDescriptionNew
                fontDescriptionSetFamily f "Monospace"
                return f

        workspaceView <- sourceViewNew
        workspaceBuffer <- (get workspaceView textViewBuffer) >>= (return . castToSourceBuffer)
        lm <- sourceLanguageManagerNew
        mbLang <- sourceLanguageManagerGuessLanguage lm Nothing (Just "text/x-haskell")
        case mbLang of
            Nothing -> return ()
            Just lang -> do sourceBufferSetLanguage workspaceBuffer lang

        -- This call is here because in the past I have had problems where the
        -- language object became invalid if the manager was garbage collected
        sourceLanguageManagerGetLanguageIds lm

        sourceBufferSetHighlightSyntax workspaceBuffer True
        widgetModifyFont workspaceView (Just font)

        case sourceStyle prefs of
            Nothing  -> return ()
            Just str -> do
                styleManager <- sourceStyleSchemeManagerNew
                ids <- sourceStyleSchemeManagerGetSchemeIds styleManager
                when (elem str ids) $ do
                    scheme <- sourceStyleSchemeManagerGetScheme styleManager str
                    sourceBufferSetStyleScheme workspaceBuffer scheme
        textBufferSetText workspaceBuffer wstext

        swWorkspace <- scrolledWindowNew Nothing Nothing
        containerAdd swWorkspace workspaceView
        scrolledWindowSetPolicy swWorkspace PolicyAutomatic PolicyAutomatic
        boxPackStart wbox swWorkspace PackGrow 10

        wbbox       <- vBoxNew False 0
        exeB        <- buttonNewWithLabel "Execute"
        stepExpB    <- buttonNewWithLabel "Step Expression"
        traceExpB   <- buttonNewWithLabel "Trace Expression"

        boxPackStart wbbox exeB PackNatural 10
        boxPackStart wbbox stepExpB PackNatural 10
        boxPackStart wbbox traceExpB PackNatural 10
        boxPackStart wbox wbbox PackNatural 10

        nbBreakAndTrace <- newNotebook
        notebookInsertPage nbBreakAndTrace swTraces "Trace" 0
        notebookInsertPage nbBreakAndTrace swBreak "Breakpoints" 1

        paned0  <- hPanedNew
        panedAdd1 paned0 swVariables
        panedAdd2 paned0 nbBreakAndTrace

        paned1           <-  vPanedNew
        panedAdd1 paned1 wbox
        panedAdd2 paned1 paned0

        boxPackStart ibox paned1 PackGrow 10
        boxPackEnd ibox bb PackNatural 10

        --openType
        let deb = IDEDebugger ibox workspaceView listStoreB listStoreV paned0 paned1
        exeB `onClicked` (do
            maybeText <- selectedDebuggerText workspaceView
            reflectIDE (
                case maybeText of
                    Just text -> debugCommand text (\o -> do
                        logOutput o
                        liftIO $ postGUIAsync (setDebuggerText deb o))
                    Nothing   -> ideMessage Normal "Please select some text in the editor to execute")
                        ideR)
        stepExpB `onClicked` (do
            t <- selectedDebuggerText workspaceView
            reflectIDE (debugStepExpr t) ideR)
        traceExpB `onClicked` (do
            t <- selectedDebuggerText workspaceView
            reflectIDE (debugTraceExpr t) ideR)
        stepB `onClicked` (reflectIDE debugStep ideR)
        stepLB `onClicked` (reflectIDE debugStepLocal ideR)
        stepMB `onClicked` (reflectIDE debugStepModule ideR)
        continueB `onClicked` (reflectIDE debugContinue ideR)

        workspaceView `widgetAddEvents` [ButtonReleaseMask]
        id5 <- workspaceView `onButtonRelease`
            (\ e -> do
                buf     <-  textViewGetBuffer workspaceView
                (l,r)   <- textBufferGetSelectionBounds buf
                symbol  <- textBufferGetText buf l r True
                when (controlIsPressed e)
                    (reflectIDE (do
                        triggerEvent ideR (SelectInfo symbol)
                        return ()) ideR)
                return False)
        treeViewB  `onButtonPress` (breakpointViewPopup ideR listStoreB treeViewB)

        notebookInsertOrdered nb ibox (paneName deb) Nothing
        widgetShowAll ibox
        return (deb,[])
    addPaneAdmin pane [] panePath
    liftIO $ widgetGrabFocus (workspaceView pane)
    liftIO $ bringPaneToFront pane

selectedDebuggerText :: SourceView -> IO (Maybe String)
selectedDebuggerText workspaceView = do
    gtkbuf       <-  textViewGetBuffer workspaceView
    hasSelection <- liftIO $ textBufferHasSelection gtkbuf
    if hasSelection
        then do
            (i1,i2)   <- liftIO $ textBufferGetSelectionBounds gtkbuf
            text      <- textBufferGetText gtkbuf i1 i2 False
            return $ Just text
        else return Nothing

setDebuggerText :: IDEDebugger -> [ToolOutput] -> IO ()
setDebuggerText deb tol = do
    let text = (concatMap toolline $ (filter (\t -> case t of
                                                                ToolOutput _ -> True
                                                                _ -> False) tol))
    unless (null text) $ do
        gtkbuf       <- textViewGetBuffer $ workspaceView deb
        hasSelection <- liftIO $ textBufferHasSelection gtkbuf
        if hasSelection
            then do
                (i1,i2)   <- liftIO $ textBufferGetSelectionBounds gtkbuf
                textBufferInsert gtkbuf i2 (" >> " ++ text)
            else do
                textBufferInsertAtCursor gtkbuf (" >> " ++ text)
                return ()

fillBreakpointList :: IDEAction
fillBreakpointList = do
    mbDebugger <- getPane
    case mbDebugger of
        Nothing -> return ()
        Just deb -> do
            refs <- readIDE breakpointRefs
            liftIO $ do
                --trace ("breakpointList " ++ show refs) $ return ()
                listStoreClear (breakpoints deb)
                mapM_ (listStoreAppend (breakpoints deb)) refs

fillVariablesList :: IDEAction
fillVariablesList = do
    mbDebugger <- getPane
    case mbDebugger of
        Nothing -> return ()
        Just deb -> do
            refs <- readIDE contextRefs
            liftIO $ listStoreClear (variables deb)
            debugCommand' ":show bindings" (\to -> liftIO
                $ postGUIAsync
                    $ mapM_ (listStoreAppend (variables deb))
                        $ concatMap selectString to)
    where
    selectString :: ToolOutput -> [String]
    selectString (ToolOutput str)   = lines str
    selectString _                  = []


updateDebugger :: IDEAction
updateDebugger = do
    fillBreakpointList
    fillVariablesList

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


