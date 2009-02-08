{-# OPTIONS_GHC -XDeriveDataTypeable -XMultiParamTypeClasses -XTypeSynonymInstances
    -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Search
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
-- Portability  :  portable
--
-- | The pane of ide where metadata searches can be done
--
-------------------------------------------------------------------------------

module IDE.Pane.Search (
    IDESearch(..)
,   SearchState
,   showSearch
,   setChoices
,   searchMetaGUI
) where

import Graphics.UI.Gtk hiding (get)
import qualified Graphics.UI.Gtk.ModelView as New
import Graphics.UI.Gtk.Gdk.Events
import Data.IORef (newIORef)
import Data.IORef (writeIORef,readIORef,IORef(..))
import IDE.Pane.Info (setInfo,IDEInfo(..))
import IDE.Pane.SourceBuffer (goToDefinition)
--import Debug.Trace (trace)
import IDE.Metainfo.Provider (searchMeta)
import Data.Maybe
import Control.Monad.Reader
--import Distribution.Package
import Data.Typeable
import IDE.Core.State
import Distribution.Text(display)
import Control.Event (triggerEvent)
import Debug.Trace (trace)

-- | A search pane description
--

data IDESearch      =   IDESearch {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   New.TreeView
,   searchStore     ::   New.ListStore Descr
,   searchStringRef ::   IORef String
,   searchScopeRef  ::   IORef Scope
,   searchModeRef   ::   IORef SearchMode
,   topBox          ::   VBox
} deriving Typeable

data SearchState    =   SearchState {
    searchString    ::   String
,   searchScope     ::   Scope
,   searchMode      ::   SearchMode
}   deriving(Eq,Ord,Read,Show,Typeable)

instance IDEObject IDESearch

instance Pane IDESearch IDEM
    where
    primPaneName _  =   "Search"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . topBox
    paneId b        =   "*Search"
    makeActive p    =   activatePane p []
    close           =   closePane

instance RecoverablePane IDESearch SearchState IDEM where
    saveState p     =   do
        str     <-  liftIO $ readIORef (searchStringRef p)
        mode    <-  liftIO $ readIORef (searchModeRef p)
        scope   <-  liftIO $ readIORef (searchScopeRef p)
        return (Just (SearchState str scope mode))
    recoverState pp (SearchState str scope mode) =   do
        nb      <-  getNotebook pp
        initSearch pp nb scope mode
        searchMetaGUI str

showSearch :: IDEAction
showSearch = do
    m <- getSearch
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeView m)

getSearch :: IDEM IDESearch
getSearch = do
    mbSearch <- getPane
    case mbSearch of
        Just m ->   return m
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (modulesPanePath prefs) layout
            nb          <-  getNotebook pp
            initSearch pp nb System (Prefix False)
            mbSearch <- getPane
            case mbSearch of
                Nothing ->  throwIDE "Can't init search"
                Just m  ->  return m


initSearch :: PanePath -> Notebook -> Scope -> SearchMode -> IDEAction
initSearch panePath nb scope mode = do
    panes       <-  readIDE panes
    paneMap     <-  readIDE paneMap
    prefs       <-  readIDE prefs
    currentInfo <-  readIDE currentInfo
    (buf,cids)  <-  reifyIDE $ \ideR  -> do

        scopebox        <-  hBoxNew True 2
        rb1             <-  radioButtonNewWithLabel "Local"
        rb2             <-  radioButtonNewWithLabelFromWidget rb1 "Package"
        rb3             <-  radioButtonNewWithLabelFromWidget rb1 "System"
        toggleButtonSetActive
            (case scope of
                Local   -> rb1
                Package -> rb2
                System   -> rb3) True
        boxPackStart scopebox rb1 PackNatural 2
        boxPackStart scopebox rb2 PackNatural 2
        boxPackEnd scopebox rb3 PackNatural 2

        modebox         <-  hBoxNew True 2
        mb1             <-  radioButtonNewWithLabel "Exact"
        mb2             <-  radioButtonNewWithLabelFromWidget mb1 "Prefix"
        mb3             <-  radioButtonNewWithLabelFromWidget mb1 "Regex"
        toggleButtonSetActive
            (case mode of
                Exact _  -> mb1
                Prefix _ -> mb2
                Regex _  -> mb3) True
        mb4             <-  checkButtonNewWithLabel "Case sensitive"
        toggleButtonSetActive mb4 (caseSense mode)
        boxPackStart modebox mb1 PackNatural 2
        boxPackStart modebox mb2 PackNatural 2
        boxPackStart modebox mb3 PackNatural 2
        boxPackEnd modebox mb4 PackNatural 2

        listStore   <-  New.listStoreNew []
        treeView    <-  New.treeViewNew
        New.treeViewSetModel treeView listStore


        renderer3    <- New.cellRendererTextNew
        renderer30   <- New.cellRendererPixbufNew
        col3         <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col3 "Symbol"
        New.treeViewColumnSetSizing col3 TreeViewColumnAutosize
        New.treeViewColumnSetResizable col3 True
        New.treeViewColumnSetReorderable col3 True
        New.treeViewAppendColumn treeView col3
        New.cellLayoutPackStart col3 renderer30 False
        New.cellLayoutPackStart col3 renderer3 True
        New.cellLayoutSetAttributes col3 renderer3 listStore
            $ \row -> [ New.cellText := descrName row]
        New.cellLayoutSetAttributes col3 renderer30 listStore
            $ \row -> [
            New.cellPixbufStockId  := stockIdFromType ((descrType . details) row)]


        renderer1    <- New.cellRendererTextNew
        renderer10   <- New.cellRendererPixbufNew
        col1         <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col1 "Module"
        New.treeViewColumnSetSizing col1 TreeViewColumnAutosize
        New.treeViewColumnSetResizable col1 True
        New.treeViewColumnSetReorderable col1 True
        New.treeViewAppendColumn treeView col1
        New.cellLayoutPackStart col1 renderer10 False
        New.cellLayoutPackStart col1 renderer1 True
        New.cellLayoutSetAttributes col1 renderer1 listStore
            $ \row -> [ New.cellText := display $ modu $ descrModu' row]
        New.cellLayoutSetAttributes col1 renderer10 listStore
            $ \row -> [
            New.cellPixbufStockId  := if isReexported row
                                    then "ide_reexported"
                                        else if isJust (mbLocation row)
                                            then "ide_source"
                                            else ""]

        renderer2   <- New.cellRendererTextNew
        col2        <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col2 "Package"
        New.treeViewColumnSetSizing col2 TreeViewColumnAutosize
        New.treeViewColumnSetResizable col2 True
        New.treeViewColumnSetReorderable col2 True
        New.treeViewAppendColumn treeView col2
        New.cellLayoutPackStart col2 renderer2 True
        New.cellLayoutSetAttributes col2 renderer2 listStore
            $ \row -> [ New.cellText := display $ pack $ descrModu' row]

        New.treeViewSetHeadersVisible treeView True
        sel <- New.treeViewGetSelection treeView
        New.treeSelectionSetMode sel SelectionSingle

        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw treeView
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic

        box             <-  vBoxNew False 2
        boxPackStart box scopebox PackNatural 0
        boxPackStart box sw PackGrow 0
        boxPackEnd box modebox PackNatural 0
        stringRef <- newIORef ""
        scopeRef  <- newIORef scope
        modeRef   <- newIORef mode
        let search = IDESearch sw treeView listStore stringRef scopeRef modeRef box
        notebookInsertOrdered nb box (paneName search) Nothing
        widgetShowAll box

        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive search) ideR ; return True)
        rb1 `onToggled` (reflectIDE (scopeSelection Local) ideR )
        rb2 `onToggled` (reflectIDE (scopeSelection Package) ideR )
        rb3 `onToggled` (reflectIDE (scopeSelection System) ideR )
        mb1 `onToggled` do
            widgetSetSensitivity mb4 False
            active <- toggleButtonGetActive mb4
            (reflectIDE (modeSelection (Exact active)) ideR )
        mb2 `onToggled`do
            widgetSetSensitivity mb4 True
            active <- toggleButtonGetActive mb4
            (reflectIDE (modeSelection (Prefix active)) ideR )
        mb3 `onToggled` do
            widgetSetSensitivity mb4 True
            active <- toggleButtonGetActive mb4
            (reflectIDE (modeSelection (Regex active)) ideR )
        mb4 `onToggled` do
            active <- toggleButtonGetActive mb4
            (reflectIDE (modeSelectionCase active) ideR )
        treeView `onButtonPress` (listViewPopup ideR  listStore treeView)
        sel `New.onSelectionChanged` do
            fillInfo treeView listStore ideR
            trace "search pane selection changes" $ return ()
        return (search,[ConnectC cid1])
    addPaneAdmin buf cids panePath
    liftIO $widgetGrabFocus (scrolledView buf)

getScope :: IDESearch -> IO Scope
getScope search = readIORef (searchScopeRef search)

getMode :: IDESearch -> IO SearchMode
getMode search = readIORef (searchModeRef search)

scopeSelection :: Scope -> IDEAction
scopeSelection scope = do
    search <- getSearch
    liftIO $ writeIORef (searchScopeRef search) scope
    text   <- liftIO $ readIORef (searchStringRef search)
    searchMetaGUI text

modeSelection :: SearchMode -> IDEAction
modeSelection mode = do
    search <- getSearch
    liftIO $ writeIORef (searchModeRef search) mode
    text   <- liftIO $ readIORef (searchStringRef search)
    searchMetaGUI text

modeSelectionCase :: Bool -> IDEAction
modeSelectionCase caseSense = do
    search <- getSearch
    oldMode <- liftIO $ readIORef (searchModeRef search)
    liftIO $ writeIORef (searchModeRef search) oldMode{caseSense = caseSense}
    text   <- liftIO $ readIORef (searchStringRef search)
    searchMetaGUI text

searchMetaGUI :: String -> IDEAction
searchMetaGUI str = do
    search <- getSearch
    liftIO $ bringPaneToFront search
    liftIO $ writeIORef (searchStringRef search) str
    scope  <- liftIO $ getScope search
    mode   <- liftIO $ getMode search
    let mode' = if length str > 2 then mode else Exact (caseSense mode)
    descrs <- if null str
                then return []
                else searchMeta scope str mode'
    liftIO $ do
        New.listStoreClear (searchStore search)
        mapM_ (New.listStoreAppend (searchStore search)) descrs

listViewPopup :: IDERef
    -> New.ListStore Descr
    -> New.TreeView
    -> Event
    -> IO (Bool)
listViewPopup ideR  store descrView (Button _ click _ _ _ _ button _ _) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Go to definition"
            item1 `onActivateLeaf` do
                sel         <-  getSelectionDescr descrView store
                case sel of
                    Just descr      ->  reflectIDE
                                            (goToDefinition descr) ideR
                    otherwise       ->  sysMessage Normal "Search >> listViewPopup: no selection"
            menuShellAppend theMenu item1
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do sel         <-  getSelectionDescr descrView store
                        case sel of
                            Just descr      ->  reflectIDE (triggerEvent ideR (SelectIdent descr))
                                                    ideR  >> return ()
                            otherwise       ->  sysMessage Normal "Search >> listViewPopup: no selection2"
                        return True
                else do
                    mbPane :: Maybe IDEInfo <- reflectIDE getPane ideR
                    when (isJust mbPane) $ bringPaneToFront (fromJust mbPane)
                    return False
listViewPopup _ _ _ _ = throwIDE "listViewPopup wrong event type"

getSelectionDescr ::  New.TreeView
    ->  New.ListStore Descr
    -> IO (Maybe Descr)
getSelectionDescr treeView listStore = do
    treeSelection   <-  New.treeViewGetSelection treeView
    paths           <-  New.treeSelectionGetSelectedRows treeSelection
    case paths of
        [a]:r ->  do
            val     <-  New.listStoreGetValue listStore a
            return (Just val)
        _  ->  return Nothing

fillInfo :: New.TreeView
    -> New.ListStore Descr
    -> IDERef
    -> IO ()
fillInfo treeView listStore ideR  = do
    sel <- getSelectionDescr treeView listStore
    case sel of
        Just descr      ->  reflectIDE (setInfo descr) ideR
        otherwise       ->  sysMessage Normal "Search>>fillInfo:no selection"

setChoices :: [Descr] -> IDEAction
setChoices descrs = do
    search <- getSearch
    liftIO $ do
        New.listStoreClear (searchStore search)
        mapM_ (New.listStoreAppend (searchStore search)) descrs
        bringPaneToFront search
        writeIORef (searchStringRef search) ""


