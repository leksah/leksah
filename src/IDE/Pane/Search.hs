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
,   buildSearchPane
--,   launchSymbolNavigationDialog
,   getSearch
) where

import Graphics.UI.Gtk
       (scrolledWindowSetShadowType, rowActivated, Menu,
        menuAttachToWidget, cellTextScaleSet, cellTextScale,
        listStoreGetValue, treeSelectionGetSelectedRows, widgetShowAll,
        menuPopup, menuShellAppend, menuItemActivate, menuItemNewWithLabel,
        menuNew, listStoreAppend, listStoreClear, entrySetText,
        toggleButtonGetActive, widgetSetSensitivity, vBoxNew, entryNew,
        scrolledWindowSetPolicy, containerAdd, scrolledWindowNew,
        treeSelectionSetMode, treeViewGetSelection,
        treeViewSetHeadersVisible, cellPixbufStockId, cellText,
        cellLayoutSetAttributes, cellLayoutPackStart, treeViewAppendColumn,
        treeViewColumnSetReorderable, treeViewColumnSetResizable,
        treeViewColumnSetSizing, treeViewColumnSetTitle, treeViewColumnNew,
        cellRendererPixbufNew, cellRendererTextNew, treeViewSetModel,
        treeViewNew, listStoreNew, boxPackEnd, boxPackStart,
        checkButtonNewWithLabel, toggleButtonSetActive, ResponseId(..),
        dialogRun, radioButtonNewWithLabelFromWidget,
        radioButtonNewWithLabel, buttonNewFromStock, windowTransientFor,
        hButtonBoxNew, dialogGetActionArea, widgetGrabDefault, set, get,
        dialogNew, dialogResponse, widgetHide, buttonSetLabel,
        widgetCanDefault, hBoxNew, entryGetText, castToWidget, Entry, VBox,
        ListStore, TreeView, ScrolledWindow, PolicyType(..),
        SelectionMode(..), TreeViewColumnSizing(..), AttrOp(..),
        Packing(..), focusInEvent, toggled, buttonPressEvent,
        keyPressEvent, keyReleaseEvent)
import Graphics.UI.Gtk.Gdk.EventM
import System.Glib.Signals (on, after)
import Data.IORef (newIORef, writeIORef, readIORef, IORef(..))
-- import IDE.Pane.SourceBuffer (goToDefinition)
import IDE.Metainfo.Provider (searchMeta)
import Data.Maybe
import Data.Typeable
import IDE.Core.State
import IDE.Utils.GUIUtils
import Distribution.Text(display)
import Control.Event (triggerEvent)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Char8 as BS (empty, unpack)
import System.Glib.Properties (newAttrFromMaybeStringProperty)
import Control.Monad (void)
import Graphics.UI.Gtk.General.Enums (ShadowType(..))

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
,   scopeSelection  ::   Scope -> IDEAction
,   modeSelection   ::   SearchMode -> IDEAction
,   searchMetaGUI   ::   String -> IDEAction
,   setChoices     ::   [Descr] -> IDEAction
} deriving Typeable

data SearchState    =   SearchState {
    searchString    ::   String
,   searchScope     ::   Scope
,   searchMode      ::   SearchMode
}   deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDESearch IDEM
    where
    primPaneName _  =   __ "Search"
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
        mbP@(Just search)     <-  buildPane pp nb builder
        scopeSelection search scope
        modeSelection search mode
        searchMetaGUI search str
        return mbP
    builder pp nb windows = buildSearchPane


buildSearchPane :: IDEM (Maybe IDESearch,Connections)
buildSearchPane =
    let scope   = SystemScope
        mode    = Prefix False
    in reifyIDE $ \ ideR -> do

        scopebox        <-  hBoxNew True 2
        rb1             <-  radioButtonNewWithLabel (__ "Package")
        rb2             <-  radioButtonNewWithLabelFromWidget rb1 (__ "Workspace")
        rb3             <-  radioButtonNewWithLabelFromWidget rb1 (__ "System")
        toggleButtonSetActive rb3 True
        cb2             <-  checkButtonNewWithLabel (__ "Imports")

        boxPackStart scopebox rb1 PackGrow 2
        boxPackStart scopebox rb2 PackGrow 2
        boxPackStart scopebox rb3 PackGrow 2
        boxPackEnd scopebox cb2 PackNatural 2

        modebox         <-  hBoxNew True 2
        mb1             <-  radioButtonNewWithLabel (__ "Exact")
        mb2             <-  radioButtonNewWithLabelFromWidget mb1 (__ "Prefix")
        mb3             <-  radioButtonNewWithLabelFromWidget mb1 (__ "Regex")
        toggleButtonSetActive
            (case mode of
                Exact _  -> mb1
                Prefix _ -> mb2
                Regex _  -> mb3) True
        mb4             <-  checkButtonNewWithLabel (__ "Case sensitive")
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
        treeViewColumnSetTitle col3 (__ "Symbol")
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
        treeViewColumnSetTitle col1 (__ "Module")
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
            $ \row -> [newAttrFromMaybeStringProperty "stock-id"
                         := if isReexported row
                                    then Just "ide_reexported"
                                        else if isJust (dscMbLocation row)
                                            then Just "ide_source"
                                            else Nothing]

        renderer2   <- cellRendererTextNew
        col2        <- treeViewColumnNew
        treeViewColumnSetTitle col2 (__ "Package")
        treeViewColumnSetSizing col2 TreeViewColumnAutosize
        treeViewColumnSetResizable col2 True
        treeViewColumnSetReorderable col2 True
        treeViewAppendColumn treeView col2
        cellLayoutPackStart col2 renderer2 True
        cellLayoutSetAttributes col2 renderer2 listStore
            $ \row -> [ cellText := case dsMbModu row of
                                        Nothing -> ""
                                        Just pm -> display $ pack pm]

        renderer3   <- cellRendererTextNew
        col3        <- treeViewColumnNew
        treeViewColumnSetTitle col3 (__ "Type/Kind")
        treeViewColumnSetSizing col3 TreeViewColumnAutosize
        treeViewColumnSetResizable col3 True
        treeViewColumnSetReorderable col3 True
        treeViewAppendColumn treeView col3
        cellLayoutPackStart col3 renderer3 True
        cellLayoutSetAttributes col3 renderer3 listStore
            $ \row -> [ cellText := BS.unpack $ fromMaybe BS.empty $
                            dscMbTypeStr row,
                        cellTextScale := 0.8, cellTextScaleSet := True    ]

        treeViewSetHeadersVisible treeView True
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionSingle

        sw <- scrolledWindowNew Nothing Nothing
        scrolledWindowSetShadowType sw ShadowIn
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
        let search = IDESearch sw treeView listStore scopeRef modeRef box entry scopeSelection_ modeSelection_ searchMetaGUI_ setChoices_
            scopeSelection_ :: Scope -> IDEAction
            scopeSelection_ scope = do
                liftIO $ writeIORef (searchScopeRef search) scope
                text   <- liftIO $ entryGetText entry
                searchMetaGUI_ text

            modeSelection_ :: SearchMode -> IDEAction
            modeSelection_ mode = do
                liftIO $ writeIORef (searchModeRef search) mode
                text   <- liftIO $ entryGetText entry
                searchMetaGUI_ text
            searchMetaGUI_ :: String -> IDEAction
            searchMetaGUI_ str = do
                liftIO $ bringPaneToFront search
                liftIO $ entrySetText entry str
                scope  <- liftIO $ getScope search
                mode   <- liftIO $ getMode search
            --    let mode' = if length str > 2 then mode else Exact (caseSense mode)
                descrs <- if null str
                            then return []
                            else searchMeta scope str mode
                liftIO $ do
                    listStoreClear (searchStore search)
                    mapM_ (listStoreAppend (searchStore search)) (take 500 descrs)
            modeSelectionCase :: Bool -> IDEAction
            modeSelectionCase caseSense = do
                oldMode <- liftIO $ readIORef (searchModeRef search)
                liftIO $ writeIORef (searchModeRef search) oldMode{caseSense = caseSense}
                text   <- liftIO $ entryGetText entry
                searchMetaGUI_ text
            setChoices_ :: [Descr] -> IDEAction
            setChoices_ descrs =
                liftIO $ do
                    listStoreClear (searchStore search)
                    mapM_ (listStoreAppend (searchStore search)) descrs
                    bringPaneToFront search
                    entrySetText entry
                        (case descrs of
                            []    -> ""
                            hd: _ -> dscName hd)
            scopeSelection' rb1 rb2 rb3 cb2 = do
                scope <- liftIO $ do
                    withImports <-  toggleButtonGetActive cb2
                    s1 <- toggleButtonGetActive rb1
                    s2 <- toggleButtonGetActive rb2
                    s3 <- toggleButtonGetActive rb3
                    return $ if s1
                                then PackageScope withImports
                                else if s2
                                        then WorkspaceScope withImports
                                        else SystemScope
                scopeSelection_ scope

        cid1 <- treeView `after` focusInEvent $ liftIO $ do
            reflectIDE (makeActive search) ideR
            return True
        rb1 `on` toggled $ liftIO (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
        rb2 `on` toggled $ liftIO (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
        rb3 `on` toggled $ liftIO (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
        cb2 `on` toggled $ liftIO (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR)
        mb1 `on` toggled $ liftIO $ do
            widgetSetSensitivity mb4 False
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelection_ (Exact active)) ideR
        mb2 `on` toggled $ liftIO $ do
            widgetSetSensitivity mb4 True
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelection_ (Prefix active)) ideR
        mb3 `on` toggled $ liftIO $ do
            widgetSetSensitivity mb4 True
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelection_ (Regex active)) ideR
        mb4 `on` toggled $ liftIO $ do
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelectionCase active) ideR
        (cid2, cid3) <- treeViewContextMenu treeView $ searchContextMenu ideR listStore treeView
        cid4 <- treeView `on` rowActivated $ selectDescr ideR listStore
--            sel `onSelectionChanged` do
--                fillInfo search ideR
        entry `on` keyReleaseEvent $ liftIO $ do
            text <- entryGetText entry
            reflectIDE (searchMetaGUI_ text) ideR
            return False
        return (Just search, map ConnectC [cid1, cid2, cid3, cid4])


getScope :: IDESearch -> IO Scope
getScope search = readIORef (searchScopeRef search)

getMode :: IDESearch -> IO SearchMode
getMode search = readIORef (searchModeRef search)

getSearch :: Maybe PanePath -> IDEM IDESearch
getSearch Nothing = forceGetPane (Right "*Search")
getSearch (Just pp)  = forceGetPane (Left pp)

searchContextMenu :: IDERef
                  -> ListStore Descr
                  -> TreeView
                  -> Menu
                  -> IO ()
searchContextMenu ideR store descrView theMenu = do
    item1           <-  menuItemNewWithLabel (__ "Go to definition")
    item1 `on` menuItemActivate $ liftIO $ goToDef ideR store descrView
    menuShellAppend theMenu item1

goToDef ideR store descrView = do
    sel         <-  getSelectionDescr descrView store
    case sel of
        Just descr      ->  void (reflectIDE (triggerEvent ideR (GotoDefinition descr)) ideR)
                                -- (goToDefinition descr) ideR
        otherwise       ->  sysMessage Normal (__ "Search >> listViewPopup: no selection")

selectDescr ideR store [i] col = do
    descr <- listStoreGetValue store i
    liftIO $ reflectIDE (triggerEvent ideR (SelectIdent descr)) ideR
    return ()

selectDescr _ _ _ _ = liftIO $ sysMessage Normal (__ "Search >> selectDescr: invalid path")

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

{--
launchSymbolNavigationDialog :: String -> (Descr -> IDEM ()) -> IDEM ()
launchSymbolNavigationDialog txt act = do
    dia                        <-   liftIO $ dialogNew
    win <- getMainWindow
    (Just searchPane, _) <- buildSearchPane
    liftIO $ do
        windowSetTransientFor dia win
        upper                      <-   dialogGetUpper dia
        lower                      <-   dialogGetActionArea dia
        boxPackStart upper (topBox searchPane) PackNatural 7

        bb      <-  hButtonBoxNew
        closeB  <-  buttonNewFromStock "gtk-cancel"
        okB    <-  buttonNewFromStock "gtk-ok"
        okB `onClicked` do
            dialogResponse dia ResponseOk
            widgetHideAll dia
        closeB `onClicked` do
            dialogResponse dia ResponseCancel
            widgetHideAll dia
        boxPackEnd bb closeB PackNatural 0
        boxPackEnd bb okB PackNatural 0
        boxPackStart lower bb PackNatural 7
        set okB [widgetCanDefault := True]
        buttonSetLabel okB "Goto"
        widgetGrabDefault okB
        widgetShowAll dia
        resp  <- dialogRun dia
        return ()
    return ()
--}
