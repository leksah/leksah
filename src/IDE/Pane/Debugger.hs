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

import IDE.Debug
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
import Control.Monad (when)
import Control.Event (triggerEvent)
import Graphics.UI.Gtk.Gdk.Enums (EventMask(..))
import System.Glib.Attributes (get)
import IDE.LogRef (selectRef, logOutput)
import Debug.Trace (trace)
import Outputable (ppr, showSDoc)
import Graphics.UI.Gtk.Gdk.Events (Event(..))


-- | A debugger pane description
--
data IDEDebugger    =   IDEDebugger {
    sw              ::   VBox
,   workspaceView   ::   SourceView
,   breakpoints     ::   ListStore LogRef
} deriving Typeable

data DebuggerState  =   DebuggerState String
    deriving(Eq,Ord,Read,Show,Typeable)


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
        return (Just (DebuggerState ""))
    recoverState pp (DebuggerState str) =   do
        prefs       <-  readIDE prefs
        layout      <-  readIDE layout
        let pp      =   getStandardPanePath (modulesPanePath prefs) layout
        nb          <-  getNotebook pp
        initDebugger pp nb str

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
initDebugger panePath nb workSpaceCont = do
    panes       <- readIDE panes
    paneMap     <- readIDE paneMap
    prefs       <- readIDE prefs
    (pane,cids) <- reifyIDE $ \ideR  ->  do
        ibox        <- vBoxNew False 0
    -- Buttons
        bb          <- hButtonBoxNew
        buttonBoxSetLayout bb ButtonboxSpread
        exeB <- buttonNewWithLabel "Execute"
        boxPackStartDefaults bb exeB

    -- BreakpointView
        listStore   <-  listStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView listStore

        renderer    <- cellRendererTextNew
        col         <- treeViewColumnNew
        treeViewColumnSetTitle col "Breakpoints"
        treeViewColumnSetSizing col TreeViewColumnAutosize
        treeViewAppendColumn treeView col
        cellLayoutPackStart col renderer False
        cellLayoutSetAttributes col renderer listStore
            $ \row -> [ cellText := showSDoc (ppr (logRefSrcSpan row))]

        treeViewSetHeadersVisible treeView True
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionSingle

        swBreak <- scrolledWindowNew Nothing Nothing
        containerAdd swBreak treeView
        scrolledWindowSetPolicy swBreak PolicyAutomatic PolicyAutomatic

    -- Workspace View
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


        swWorkspace <- scrolledWindowNew Nothing Nothing
        containerAdd swWorkspace workspaceView
        scrolledWindowSetPolicy swWorkspace PolicyAutomatic PolicyAutomatic

        boxPackStart ibox swBreak PackGrow 10
        boxPackStart ibox swWorkspace PackGrow 10
        boxPackEnd ibox bb PackNatural 10

        --openType
        let deb = IDEDebugger ibox workspaceView listStore
        exeB `onClicked` (do
            maybeText <- selectedDebuggerText workspaceView
            reflectIDE (
                case maybeText of
                    Just text -> debugCommand text logOutput
                    Nothing   -> ideMessage Normal "Please select some text in the editor to execute")
                        ideR)
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
        treeView  `onButtonPress` (breakpointViewPopup ideR listStore treeView)

        notebookInsertOrdered nb ibox (paneName deb) Nothing
        widgetShowAll ibox
        return (deb,[])
    addPaneAdmin pane [] panePath
    liftIO $widgetGrabFocus (workspaceView pane)
    liftIO $bringPaneToFront pane

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

fillBreakpointList :: IDEAction
fillBreakpointList = do
    mbDebugger <- getPane
    case mbDebugger of
        Nothing -> return ()
        Just deb -> do
            refs <- readIDE breakpointRefs
            liftIO $ do
                trace ("breakpointList " ++ show refs) $ return ()
                listStoreClear (breakpoints deb)
                mapM_ (listStoreAppend (breakpoints deb)) refs

updateDebugger :: IDEAction
updateDebugger = do
    trace "updateDebugger" $ fillBreakpointList

breakpointViewPopup :: IDERef
    -> ListStore LogRef
    -> TreeView
    -> Event
    -> IO (Bool)
breakpointViewPopup ideR  store treeView (Button _ click _ _ _ _ button _ _) = do
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
        BreakpointRef -> debugDeleteBreakpoint (read $ (words (refDescription logRef)) !! 2) logRef
        _   -> sysMessage Normal "Debugger>>deleteBreakpoint: Not a breakpoint"


