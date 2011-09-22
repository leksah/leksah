{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses,
             TypeSynonymInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Search
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability  :  portable
--
-- | The pane of ide where metadata searches can be done
--
-------------------------------------------------------------------------------

module IDE.Pane.Search (
    IDESearch(..)
,   SearchState
,   setChoices
,   buildSearchPane
,   searchMetaGUI
,   getSearch
) where

import Graphics.UI.Gtk
       (listStoreGetValue, treeSelectionGetSelectedRows, widgetShowAll,
        menuPopup, menuShellAppend, onActivateLeaf, menuItemNewWithLabel,
        menuNew, listStoreAppend, listStoreClear, entrySetText,
        afterKeyRelease, onKeyPress, onButtonPress, toggleButtonGetActive,
        widgetSetSensitivity, onToggled, afterFocusIn, vBoxNew, entryNew,
        scrolledWindowSetPolicy, containerAdd, scrolledWindowNew,
        treeSelectionSetMode, treeViewGetSelection,
        treeViewSetHeadersVisible, cellPixbufStockId, cellText,
        cellLayoutSetAttributes, cellLayoutPackStart, treeViewAppendColumn,
        treeViewColumnSetReorderable, treeViewColumnSetResizable,
        treeViewColumnSetSizing, treeViewColumnSetTitle, treeViewColumnNew,
        cellRendererPixbufNew, cellRendererTextNew, treeViewSetModel,
        treeViewNew, listStoreNew, boxPackEnd, boxPackStart,
        checkButtonNewWithLabel, toggleButtonSetActive,
        radioButtonNewWithLabelFromWidget, radioButtonNewWithLabel,
        hBoxNew, entryGetText, castToWidget, Entry, VBox, ListStore,
        TreeView, ScrolledWindow, PolicyType(..), SelectionMode(..),
        TreeViewColumnSizing(..), AttrOp(..),
        Packing(..))
import Graphics.UI.Gtk.Gdk.Events
import Data.IORef (newIORef)
import Data.IORef (writeIORef,readIORef,IORef(..))
-- import IDE.Pane.SourceBuffer (goToDefinition)
import IDE.Metainfo.Provider (searchMeta)
import Data.Maybe
import Control.Monad.Reader
import Data.Typeable
import IDE.Core.State
import IDE.Utils.GUIUtils
import Distribution.Text(display)
import Control.Event (triggerEvent)

-- | A search pane description
--

data IDESearch      =   IDESearch {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   searchStore     ::   ListStore Descr
,   searchScopeRef  ::   IORef Scope
,   searchModeRef   ::   IORef SearchMode
,   topBox          ::   VBox
,   entry           ::   Entry
} deriving Typeable

data SearchState    =   SearchState {
    searchString    ::   String
,   searchScope     ::   Scope
,   searchMode      ::   SearchMode
}   deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDESearch IDEM
    where
    primPaneName _  =   "Search"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . topBox
    paneId b        =   "*Search"

instance RecoverablePane IDESearch SearchState IDEM where
    saveState p     =   do
        str     <-  liftIO $ entryGetText (entry p)
        mode    <-  liftIO $ readIORef (searchModeRef p)
        scope   <-  liftIO $ readIORef (searchScopeRef p)
        return (Just (SearchState str scope mode))
    recoverState pp (SearchState str scope mode) =   do
        nb      <-  getNotebook pp
        mbP     <-  buildPane pp nb builder
        scopeSelection scope
        modeSelection mode
        searchMetaGUI str
        return mbP
    builder pp nb windows = buildSearchPane


buildSearchPane :: IDEM (Maybe IDESearch,Connections)
buildSearchPane =
    let scope   = SystemScope
        mode    = Prefix False
    in reifyIDE $ \ ideR -> do

        scopebox        <-  hBoxNew True 2
        rb1             <-  radioButtonNewWithLabel "Package"
        rb2             <-  radioButtonNewWithLabelFromWidget rb1 "Workspace"
        rb3             <-  radioButtonNewWithLabelFromWidget rb1 "System"
        toggleButtonSetActive rb3 True
        cb2             <-  checkButtonNewWithLabel "Imports"

        boxPackStart scopebox rb1 PackGrow 2
        boxPackStart scopebox rb2 PackGrow 2
        boxPackStart scopebox rb3 PackGrow 2
        boxPackEnd scopebox cb2 PackNatural 2

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

        listStore   <-  listStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView listStore

        renderer3    <- cellRendererTextNew
        renderer30   <- cellRendererPixbufNew
        col3         <- treeViewColumnNew
        treeViewColumnSetTitle col3 "Symbol"
        treeViewColumnSetSizing col3 TreeViewColumnAutosize
        treeViewColumnSetResizable col3 True
        treeViewColumnSetReorderable col3 True
        treeViewAppendColumn treeView col3
        cellLayoutPackStart col3 renderer30 False
        cellLayoutPackStart col3 renderer3 True
        cellLayoutSetAttributes col3 renderer3 listStore
            $ \row -> [ cellText := dscName row]
        cellLayoutSetAttributes col3 renderer30 listStore
            $ \row -> [
            cellPixbufStockId  := stockIdFromType ((descrType . dscTypeHint) row)]


        renderer1    <- cellRendererTextNew
        renderer10   <- cellRendererPixbufNew
        col1         <- treeViewColumnNew
        treeViewColumnSetTitle col1 "Module"
        treeViewColumnSetSizing col1 TreeViewColumnAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        treeViewAppendColumn treeView col1
        cellLayoutPackStart col1 renderer10 False
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetAttributes col1 renderer1 listStore
            $ \row -> [ cellText := case dsMbModu row of
                                        Nothing -> ""
                                        Just pm -> display $ modu pm]
        cellLayoutSetAttributes col1 renderer10 listStore
            $ \row -> [
            cellPixbufStockId  := if isReexported row
                                    then "ide_reexported"
                                        else if isJust (dscMbLocation row)
                                            then "ide_source"
                                            else ""]

        renderer2   <- cellRendererTextNew
        col2        <- treeViewColumnNew
        treeViewColumnSetTitle col2 "Package"
        treeViewColumnSetSizing col2 TreeViewColumnAutosize
        treeViewColumnSetResizable col2 True
        treeViewColumnSetReorderable col2 True
        treeViewAppendColumn treeView col2
        cellLayoutPackStart col2 renderer2 True
        cellLayoutSetAttributes col2 renderer2 listStore
            $ \row -> [ cellText := case dsMbModu row of
                                        Nothing -> ""
                                        Just pm -> display $ pack pm]
        treeViewSetHeadersVisible treeView True
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionSingle

        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw treeView
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic

        entry   <-  entryNew

        box             <-  vBoxNew False 2
        boxPackStart box scopebox PackNatural 0
        boxPackStart box sw PackGrow 0
        boxPackStart box modebox PackNatural 0
        boxPackEnd box entry PackNatural 0

        scopeRef  <- newIORef scope
        modeRef   <- newIORef mode
        let search = IDESearch sw treeView listStore scopeRef modeRef box entry

        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive search) ideR ; return True)
        rb1 `onToggled` (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
        rb2 `onToggled` (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
        rb3 `onToggled` (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
        cb2 `onToggled` (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR)
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
        treeView `onButtonPress` (handleEvent ideR  listStore treeView)
        treeView `onButtonPress` (handleEvent ideR  listStore treeView)
        treeView `onKeyPress` (handleEvent ideR  listStore treeView)
--            sel `onSelectionChanged` do
--                fillInfo search ideR
        entry `afterKeyRelease` (\ event -> do
            text <- entryGetText entry
            reflectIDE (searchMetaGUI text) ideR
            return False)
        return (Just search,[ConnectC cid1])


getScope :: IDESearch -> IO Scope
getScope search = readIORef (searchScopeRef search)

getMode :: IDESearch -> IO SearchMode
getMode search = readIORef (searchModeRef search)

getSearch :: Maybe PanePath -> IDEM IDESearch
getSearch Nothing = forceGetPane (Right "*Search")
getSearch (Just pp)  = forceGetPane (Left pp)

scopeSelection' rb1 rb2 rb3 cb2 = do
    scope <- liftIO $ do
        withImports <-  toggleButtonGetActive cb2
        s1 <- toggleButtonGetActive rb1
        s2 <- toggleButtonGetActive rb2
        s3 <- toggleButtonGetActive rb3
        if s1
            then return (PackageScope withImports)
            else if s2
                    then return (WorkspaceScope withImports)
                    else return (SystemScope)
    scopeSelection scope

scopeSelection :: Scope -> IDEAction
scopeSelection scope = do
    search <- getSearch Nothing
    liftIO $ writeIORef (searchScopeRef search) scope
    text   <- liftIO $ entryGetText (entry search)
    searchMetaGUI text

modeSelection :: SearchMode -> IDEAction
modeSelection mode = do
    search <- getSearch Nothing
    liftIO $ writeIORef (searchModeRef search) mode
    text   <- liftIO $ entryGetText (entry search)
    searchMetaGUI text

modeSelectionCase :: Bool -> IDEAction
modeSelectionCase caseSense = do
    search <- getSearch Nothing
    oldMode <- liftIO $ readIORef (searchModeRef search)
    liftIO $ writeIORef (searchModeRef search) oldMode{caseSense = caseSense}
    text   <- liftIO $ entryGetText (entry search)
    searchMetaGUI text

searchMetaGUI :: String -> IDEAction
searchMetaGUI str = do
    search <- getSearch Nothing
    liftIO $ bringPaneToFront search
    liftIO $ entrySetText (entry search) str
    scope  <- liftIO $ getScope search
    mode   <- liftIO $ getMode search
--    let mode' = if length str > 2 then mode else Exact (caseSense mode)
    descrs <- if null str
                then return []
                else searchMeta scope str mode
    liftIO $ do
        listStoreClear (searchStore search)
        mapM_ (listStoreAppend (searchStore search)) (take 500 descrs)

handleEvent :: IDERef
    -> ListStore Descr
    -> TreeView
    -> Event
    -> IO (Bool)
handleEvent ideR  store descrView (Button {eventClick = click, eventButton = button}) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Go to definition"
            item1 `onActivateLeaf` (goToDef ideR store descrView)

            menuShellAppend theMenu item1
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then selectDescr ideR store descrView
                else return False
handleEvent ideR  store descrView (Key { eventKeyName = "Return"}) =
    selectDescr ideR store descrView
handleEvent _ _ _ _ = return False


goToDef ideR store descrView = do
    sel         <-  getSelectionDescr descrView store
    case sel of
        Just descr      ->  reflectIDE (triggerEvent ideR (GotoDefinition descr)) ideR >> return ()
                                -- (goToDefinition descr) ideR
        otherwise       ->  sysMessage Normal "Search >> listViewPopup: no selection"

selectDescr ideR store descrView= do
    sel <-  getSelectionDescr descrView store
    case sel of
        Just descr      ->  reflectIDE (triggerEvent ideR (SelectIdent descr))
                                ideR  >> return ()
        otherwise       ->  sysMessage Normal "Search >> listViewPopup: no selection2"
    return True

getSelectionDescr ::  TreeView
    ->  ListStore Descr
    -> IO (Maybe Descr)
getSelectionDescr treeView listStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        [a]:r ->  do
            val     <-  listStoreGetValue listStore a
            return (Just val)
        _  ->  return Nothing

--fillInfo :: IDESearch
--    -> IDERef
--    -> IO ()
--fillInfo search ideR  = do
--    sel <- getSelectionDescr (treeView search) (searchStore search)
--    case sel of
--        Just descr      ->  do
--            reflectIDE (setInfo descr) ideR
--            entrySetText (entry search) (descrName descr)
--        otherwise       ->  return ()

setChoices :: [Descr] -> IDEAction
setChoices descrs = do
    search <- getSearch Nothing
    liftIO $ do
        listStoreClear (searchStore search)
        mapM_ (listStoreAppend (searchStore search)) descrs
        bringPaneToFront search
        entrySetText (entry search)
            (case descrs of
                []    -> ""
                hd: _ -> dscName hd)


launchSymbolNavigationDialog :: String -> (Descr -> IDEM ()) -> IDEM ()
launchSymbolNavigationDialog txt act = do
    return ()
