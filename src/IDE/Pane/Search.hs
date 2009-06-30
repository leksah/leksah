{-# OPTIONS_GHC -XDeriveDataTypeable -XMultiParamTypeClasses -XTypeSynonymInstances
    -XScopedTypeVariables #-}
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
,   showSearch
,   setChoices
,   searchMetaGUI
) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Gdk.Events
import Data.IORef (newIORef)
import Data.IORef (writeIORef,readIORef,IORef(..))
import IDE.Pane.Info (setInfo,IDEInfo(..))
import IDE.Pane.SourceBuffer (goToDefinition)
import IDE.Metainfo.Provider (searchMeta)
import Data.Maybe
import Control.Monad.Reader
import Data.Typeable
import IDE.Core.State
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
        str     <-  liftIO $ entryGetText (entry p)
        mode    <-  liftIO $ readIORef (searchModeRef p)
        scope   <-  liftIO $ readIORef (searchScopeRef p)
        return (Just (SearchState str scope mode))
    recoverState pp (SearchState str scope mode) =   do
        nb      <-  getNotebook pp
        newPane pp nb (builder scope mode)
        searchMetaGUI str

showSearch :: IDEAction
showSearch = do
    m <- getSearch
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (entry m)

getSearch :: IDEM IDESearch
getSearch = do
    mbSearch <- getPane
    case mbSearch of
        Just m ->   return m
        Nothing -> do
            pp  <- getBestPathForId "*Search"
            nb  <-  getNotebook pp
            newPane pp nb (builder System (Prefix False))
            mbSearch <- getPane
            case mbSearch of
                Nothing ->  throwIDE "Can't init search"
                Just m  ->  return m


builder :: Scope ->
    SearchMode ->
    PanePath ->
    Notebook ->
    Window ->
    IDERef ->
    IO (IDESearch,Connections)
builder scope mode pp nb windows ideR = do
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
        $ \row -> [ cellText := descrName row]
    cellLayoutSetAttributes col3 renderer30 listStore
        $ \row -> [
        cellPixbufStockId  := stockIdFromType ((descrType . details) row)]


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
        $ \row -> [ cellText := display $ modu $ descrModu' row]
    cellLayoutSetAttributes col1 renderer10 listStore
        $ \row -> [
        cellPixbufStockId  := if isReexported row
                                then "ide_reexported"
                                    else if isJust (mbLocation row)
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
        $ \row -> [ cellText := display $ pack $ descrModu' row]

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
    sel `onSelectionChanged` do
        fillInfo search ideR
    entry `afterKeyRelease` (\ event -> do
        text <- entryGetText entry
        reflectIDE (searchMetaGUI text) ideR
        return False)
    return (search,[ConnectC cid1])

getScope :: IDESearch -> IO Scope
getScope search = readIORef (searchScopeRef search)

getMode :: IDESearch -> IO SearchMode
getMode search = readIORef (searchModeRef search)

scopeSelection :: Scope -> IDEAction
scopeSelection scope = do
    search <- getSearch
    liftIO $ writeIORef (searchScopeRef search) scope
    text   <- liftIO $ entryGetText (entry search)
    searchMetaGUI text

modeSelection :: SearchMode -> IDEAction
modeSelection mode = do
    search <- getSearch
    liftIO $ writeIORef (searchModeRef search) mode
    text   <- liftIO $ entryGetText (entry search)
    searchMetaGUI text

modeSelectionCase :: Bool -> IDEAction
modeSelectionCase caseSense = do
    search <- getSearch
    oldMode <- liftIO $ readIORef (searchModeRef search)
    liftIO $ writeIORef (searchModeRef search) oldMode{caseSense = caseSense}
    text   <- liftIO $ entryGetText (entry search)
    searchMetaGUI text

searchMetaGUI :: String -> IDEAction
searchMetaGUI str = do
    search <- getSearch
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

listViewPopup :: IDERef
    -> ListStore Descr
    -> TreeView
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

fillInfo :: IDESearch
    -> IDERef
    -> IO ()
fillInfo search ideR  = do
    sel <- getSelectionDescr (treeView search) (searchStore search)
    case sel of
        Just descr      ->  do
            reflectIDE (setInfo descr) ideR
            entrySetText (entry search) (descrName descr)
        otherwise       ->  return ()

setChoices :: [Descr] -> IDEAction
setChoices descrs = do
    search <- getSearch
    liftIO $ do
        listStoreClear (searchStore search)
        mapM_ (listStoreAppend (searchStore search)) descrs
        bringPaneToFront search
        entrySetText (entry search) ""


