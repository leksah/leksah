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
    (Packing(..),
     PolicyType(..),
     ButtonBoxStyle(..))
import Graphics.UI.Gtk.Gdk.Enums (EventMask(..))
import System.Glib.Attributes (get)
import IDE.LogRef (logOutput)
import IDE.Debug
    (debugCommand',
     debugTraceExpr,
     debugStepExpr,
     debugStepModule,
     debugStepLocal,
     debugContinue,
     debugStep,
     debugCommand)
import Control.Monad (unless, when)
import Control.Event (triggerEvent)
import IDE.Tool (ToolOutput(..),toolline)
import IDE.SourceCandy (getCandylessText)
import IDE.Pane.Breakpoints (fillBreakpointList)


-- | A debugger pane description
--
data IDEDebugger    =   IDEDebugger {
    sw              ::   VBox
,   workspaceView   ::   SourceView
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
        let pp      =   getStandardPanePath (debugPanePath prefs) layout
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
            let pp      =   getStandardPanePath (debugPanePath prefs) layout
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

        paned0  <- hPanedNew
        panedAdd1 paned0 swVariables
        panedAdd2 paned0 nbBreakAndTrace

        paned1           <-  vPanedNew
        panedAdd1 paned1 wbox
        panedAdd2 paned1 paned0

        boxPackStart ibox paned1 PackGrow 10
        boxPackEnd ibox bb PackNatural 10

        --openType
        let deb = IDEDebugger ibox workspaceView listStoreV paned0 paned1
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

        cid1 <- workspaceView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive deb) ideR ; return True)

        notebookInsertOrdered nb ibox (paneName deb) Nothing
        widgetShowAll ibox
        return (deb,[ConnectC cid1])
    addPaneAdmin pane cids panePath
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
