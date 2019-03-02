{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
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

import Prelude ()
import Prelude.Compat
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
-- import IDE.Pane.SourceBuffer (goToDefinition)
import IDE.Metainfo.Provider (searchMeta)
import Data.Maybe (isJust, fromMaybe)
import Data.Typeable (Typeable)
import IDE.Core.State
       (MessageLevel(..), sysMessage, reflectIDE, reifyIDE, IDE, IDERef,
        caseSense, IDEM, IDEAction, SearchMode, Scope, Descr,
        SearchMode(..), Scope(..), dscMbTypeStr, pack, dscMbLocation, isReexported, modu,
        dsMbModu, dscTypeHint, descrType, dscName, IDEEvent(..))
import IDE.Gtk.State
       (bringPaneToFront, RecoverablePane(..), Pane(..), PanePath, Connections,
        getNotebook, Connection(..))
import IDE.Utils.GUIUtils (__, stockIdFromType, treeViewContextMenu)
import Distribution.Text(display)
import Control.Event (triggerEvent)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Char8 as BS (empty, unpack)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack, null)
import System.Log.Logger (debugM)
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew, ScrolledWindow(..))
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreGetValue, seqStoreAppend, seqStoreClear, seqStoreNew,
        SeqStore(..))
import GI.Gtk.Objects.Entry
       (entrySetText, entryNew, entryGetText, Entry(..))
import GI.Gtk.Objects.TreeView
       (onTreeViewRowActivated, treeViewGetSelection,
        treeViewSetHeadersVisible, treeViewAppendColumn, treeViewSetModel,
        treeViewNew, TreeView(..))
import GI.Gtk.Objects.Widget
       (toWidget, setWidgetSensitive, onWidgetKeyReleaseEvent,
        afterWidgetFocusInEvent)
import GI.Gtk.Objects.RadioButton
       (RadioButton(..), radioButtonNewWithLabelFromWidget,
        radioButtonNewWithLabel)
import GI.Gtk.Objects.ToggleButton
       (onToggleButtonToggled, toggleButtonGetActive,
        toggleButtonSetActive)
import GI.Gtk.Objects.CheckButton (checkButtonNewWithLabel)
import GI.Gtk.Objects.Box (Box(..), boxNew)
import Graphics.UI.Editor.Parameters
       (boxPackEnd', boxPackStart', Packing(..))
import GI.Gtk.Objects.CellRendererText (cellRendererTextNew)
import GI.Gtk.Objects.CellRendererPixbuf (cellRendererPixbufNew)
import GI.Gtk.Objects.TreeViewColumn
       (treeViewColumnSetReorderable, treeViewColumnSetResizable,
        treeViewColumnSetSizing, treeViewColumnSetTitle, treeViewColumnNew)
import GI.Gtk.Interfaces.CellLayout (cellLayoutPackStart)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction)
import GI.Gtk.Objects.TreeSelection
       (treeSelectionSetMode)
import GI.Gtk.Enums
       (PolicyType(..), ShadowType(..), SelectionMode(..),
        TreeViewColumnSizing(..), Orientation(..))
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.Menu (Menu(..))
import GI.Gtk.Objects.MenuItem
       (onMenuItemActivate, menuItemNewWithLabel)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathGetIndices')
import GI.Gtk
       (setCellRendererTextScaleSet, setCellRendererTextScale,
        setCellRendererPixbufStockId, setCellRendererTextText)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Concurrent (MVar)
import Data.Int (Int32)

-- | A search pane description
--

data IDESearch      =   IDESearch {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   searchStore     ::   SeqStore Descr
,   searchScopeRef  ::   IORef Scope
,   searchModeRef   ::   IORef SearchMode
,   topBox          ::   Box
,   entry           ::   Entry
,   scopeSelection  ::   Scope -> IDEAction
,   modeSelection   ::   SearchMode -> IDEAction
,   searchMetaGUI   ::   Text -> IDEAction
,   setChoices      ::   [Descr] -> IDEAction
} deriving Typeable

data SearchState    =   SearchState {
    searchString    ::   Text
,   searchScope     ::   Scope
,   searchMode      ::   SearchMode
}   deriving(Eq,Ord,Read,Show,Typeable,Generic)

instance ToJSON SearchState
instance FromJSON SearchState

instance Pane IDESearch IDEM
    where
    primPaneName _  =   __ "Search"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . topBox
    paneId _        =   "*Search"

instance RecoverablePane IDESearch SearchState IDEM where
    saveState p     =   do
        str     <-  entryGetText (entry p)
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
    builder _pp _nb _windows = buildSearchPane


buildSearchPane :: IDEM (Maybe IDESearch,Connections)
buildSearchPane =
    let scope   = SystemScope
        mode    = Prefix False
    in reifyIDE $ \ ideR -> do

        scopebox        <-  boxNew OrientationHorizontal 2
        rb1             <-  radioButtonNewWithLabel ([]::[RadioButton]) (__ "Package")
        rb2             <-  radioButtonNewWithLabelFromWidget (Just rb1) (__ "Workspace")
        rb3             <-  radioButtonNewWithLabelFromWidget (Just rb1) (__ "System")
        toggleButtonSetActive rb3 True
        cb2             <-  checkButtonNewWithLabel (__ "Imports")

        boxPackStart' scopebox rb1 PackGrow 2
        boxPackStart' scopebox rb2 PackGrow 2
        boxPackStart' scopebox rb3 PackGrow 2
        boxPackEnd' scopebox cb2 PackNatural 2

        modebox         <-  boxNew OrientationHorizontal 2
        mb1             <-  radioButtonNewWithLabel ([]::[RadioButton]) (__ "Exact")
        mb2             <-  radioButtonNewWithLabelFromWidget (Just mb1) (__ "Prefix")
        mb3             <-  radioButtonNewWithLabelFromWidget (Just mb1) (__ "Regex")
        toggleButtonSetActive
            (case mode of
                Exact _  -> mb1
                Prefix _ -> mb2
                Regex _  -> mb3) True
        mb4             <-  checkButtonNewWithLabel (__ "Case sensitive")
        toggleButtonSetActive mb4 (caseSense mode)
        boxPackStart' modebox mb1 PackNatural 2
        boxPackStart' modebox mb2 PackNatural 2
        boxPackStart' modebox mb3 PackNatural 2
        boxPackEnd' modebox mb4 PackNatural 2

        searchStore <-  seqStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView (Just searchStore)

        renderer3    <- cellRendererTextNew
        renderer30   <- cellRendererPixbufNew
        col3         <- treeViewColumnNew
        treeViewColumnSetTitle col3 (__ "Symbol")
        treeViewColumnSetSizing col3 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col3 True
        treeViewColumnSetReorderable col3 True
        _ <- treeViewAppendColumn treeView col3
        cellLayoutPackStart col3 renderer30 False
        cellLayoutPackStart col3 renderer3 True
        cellLayoutSetDataFunction col3 renderer3 searchStore
            $ setCellRendererTextText renderer3 . dscName
        cellLayoutSetDataFunction col3 renderer30 searchStore
            $ setCellRendererPixbufStockId renderer30
                . stockIdFromType . descrType . dscTypeHint


        renderer1    <- cellRendererTextNew
        renderer10   <- cellRendererPixbufNew
        col1         <- treeViewColumnNew
        treeViewColumnSetTitle col1 (__ "Module")
        treeViewColumnSetSizing col1 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        _ <- treeViewAppendColumn treeView col1
        cellLayoutPackStart col1 renderer10 False
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetDataFunction col1 renderer1 searchStore
            $ \row -> setCellRendererTextText renderer1 $
                            case dsMbModu row of
                                Nothing -> ""
                                Just pm -> T.pack . display $ modu pm
        cellLayoutSetDataFunction col1 renderer10 searchStore
            $ \row -> setCellRendererPixbufStockId renderer10 $
                            if isReexported row
                                then "ide_reexported"
                                    else if isJust (dscMbLocation row)
                                        then "ide_source"
                                        else ""

        renderer2   <- cellRendererTextNew
        col2        <- treeViewColumnNew
        treeViewColumnSetTitle col2 (__ "Package")
        treeViewColumnSetSizing col2 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col2 True
        treeViewColumnSetReorderable col2 True
        _ <- treeViewAppendColumn treeView col2
        cellLayoutPackStart col2 renderer2 True
        cellLayoutSetDataFunction col2 renderer2 searchStore
            $ \row -> setCellRendererTextText renderer2 $
                            case dsMbModu row of
                                Nothing -> ""
                                Just pm -> T.pack . display $ pack pm

        renderer4   <- cellRendererTextNew
        col4        <- treeViewColumnNew
        treeViewColumnSetTitle col4 (__ "Type/Kind")
        treeViewColumnSetSizing col4 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col4 True
        treeViewColumnSetReorderable col4 True
        _ <- treeViewAppendColumn treeView col4
        cellLayoutPackStart col4 renderer4 True
        cellLayoutSetDataFunction col4 renderer4 searchStore
            $ \row -> do
                setCellRendererTextText renderer4 $ T.pack . BS.unpack . fromMaybe BS.empty $
                            dscMbTypeStr row
                setCellRendererTextScale renderer4 0.8
                setCellRendererTextScaleSet renderer4 True

        treeViewSetHeadersVisible treeView True
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionModeSingle

        scrolledView <- scrolledWindowNew noAdjustment noAdjustment
        scrolledWindowSetShadowType scrolledView ShadowTypeIn
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyTypeAutomatic PolicyTypeAutomatic

        entry   <-  entryNew

        topBox  <-  boxNew OrientationVertical 2
        boxPackStart' topBox scopebox PackNatural 0
        boxPackStart' topBox scrolledView PackGrow 0
        boxPackStart' topBox modebox PackNatural 0
        boxPackEnd' topBox entry PackNatural 0

        searchScopeRef  <- newIORef scope
        searchModeRef   <- newIORef mode
        let scopeSelection :: Scope -> IDEAction
            scopeSelection scope' = do
                liftIO $ writeIORef searchScopeRef scope'
                text   <- liftIO $ entryGetText entry
                searchMetaGUI text

            modeSelection :: SearchMode -> IDEAction
            modeSelection mode' = do
                liftIO $ writeIORef searchModeRef mode'
                text   <- liftIO $ entryGetText entry
                searchMetaGUI text
            searchMetaGUI :: Text -> IDEAction
            searchMetaGUI str = do
                liftIO $ debugM "leksah" $ "searchMetGUI_ " <> T.unpack str
                bringPaneToFront search
                entrySetText entry str
                scope' <- getScope search
                mode'  <- getMode search
            --    let mode' = if length str > 2 then mode' else Exact (caseSense mode')
                descrs <- if T.null str
                            then return []
                            else searchMeta scope' str mode'
                seqStoreClear searchStore
                mapM_ (seqStoreAppend searchStore) (take 500 descrs)
            modeSelectionCase :: Bool -> IDEAction
            modeSelectionCase caseSense = do
                oldMode <- liftIO $ readIORef searchModeRef
                liftIO $ writeIORef searchModeRef oldMode{caseSense = caseSense}
                text   <- entryGetText entry
                searchMetaGUI text
            setChoices :: [Descr] -> IDEAction
            setChoices descrs = do
                    seqStoreClear searchStore
                    mapM_ (seqStoreAppend searchStore) descrs
                    bringPaneToFront search
                    entrySetText entry
                        (case descrs of
                            []    -> ""
                            hd: _ -> dscName hd)
            scopeSelection' = do
                withImports <-  toggleButtonGetActive cb2
                s1 <- toggleButtonGetActive rb1
                s2 <- toggleButtonGetActive rb2
                _s3 <- toggleButtonGetActive rb3
                scopeSelection $
                    if s1
                        then PackageScope withImports
                        else if s2
                                then WorkspaceScope withImports
                                else SystemScope
            search = IDESearch {..}

        cid1 <- ConnectC treeView <$> afterWidgetFocusInEvent treeView (\_e -> do
            reflectIDE (makeActive search) ideR
            return True)
        _ <- onToggleButtonToggled rb1 $ liftIO (reflectIDE scopeSelection' ideR)
        _ <- onToggleButtonToggled rb2 $ liftIO (reflectIDE scopeSelection' ideR)
        _ <- onToggleButtonToggled rb3 $ liftIO (reflectIDE scopeSelection' ideR)
        _ <- onToggleButtonToggled cb2 $ liftIO (reflectIDE scopeSelection' ideR)
        _ <- onToggleButtonToggled mb1 $ liftIO $ do
            setWidgetSensitive mb4 False
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelection (Exact active)) ideR
        _ <- onToggleButtonToggled mb2 $ liftIO $ do
            setWidgetSensitive mb4 True
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelection (Prefix active)) ideR
        _ <- onToggleButtonToggled mb3 $ liftIO $ do
            setWidgetSensitive mb4 True
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelection (Regex active)) ideR
        _ <- onToggleButtonToggled mb4 $ liftIO $ do
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelectionCase active) ideR
        cids <- treeViewContextMenu treeView $ searchContextMenu ideR searchStore treeView
        cid4 <- ConnectC treeView <$> onTreeViewRowActivated treeView (\path col -> do
            p <- treePathGetIndices' path
            selectDescr ideR searchStore p col)
--            sel `onSelectionChanged` do
--                fillInfo search ideR
        _ <- onWidgetKeyReleaseEvent entry $ \_e -> do
            text <- entryGetText entry
            reflectIDE (searchMetaGUI text) ideR
            return False
        return (Just search, cid1 : cid4 : cids)


getScope :: MonadIO m => IDESearch -> m Scope
getScope search = liftIO . readIORef $ searchScopeRef search

getMode :: MonadIO m => IDESearch -> m SearchMode
getMode search = liftIO . readIORef $ searchModeRef search

getSearch :: Maybe PanePath -> IDEM IDESearch
getSearch Nothing = forceGetPane (Right "*Search")
getSearch (Just pp)  = forceGetPane (Left pp)

searchContextMenu :: MonadIO m
                  => IDERef
                  -> SeqStore Descr
                  -> TreeView
                  -> Menu
                  -> m ()
searchContextMenu ideR store descrView theMenu = do
    item1           <-  menuItemNewWithLabel (__ "Go to definition")
    _ <- onMenuItemActivate item1 $ liftIO $ goToDef ideR store descrView
    menuShellAppend theMenu item1

goToDef
  :: MVar (IDE -> IO (), IDE)
  -> SeqStore Descr
  -> TreeView
  -> IO ()
goToDef ideR store descrView = do
    sel         <-  getSelectionDescr descrView store
    case sel of
        Just descr ->  void (reflectIDE (triggerEvent ideR (GotoDefinition descr)) ideR)
                                -- (goToDefinition descr) ideR
        _          ->  sysMessage Normal (__ "Search >> listViewPopup: no selection")

selectDescr
  :: MonadIO m
  => MVar (IDE -> IO (), IDE)
  -> SeqStore Descr
  -> [Int32]
  -> p
  -> m ()
selectDescr ideR store [i] _col = do
    descr <- seqStoreGetValue store i
    void . liftIO $ reflectIDE (triggerEvent ideR (SelectIdent descr)) ideR

selectDescr _ _ _ _ = liftIO $ sysMessage Normal (__ "Search >> selectDescr: invalid path")

getSelectionDescr :: (Applicative m, MonadIO m)
                  => TreeView
                  ->  SeqStore Descr
                  -> m (Maybe Descr)
getSelectionDescr treeView seqStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows' treeSelection
    mapM treePathGetIndices' paths >>= \case
        [a]:_ ->  do
            val     <-  seqStoreGetValue seqStore a
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
launchSymbolNavigationDialog :: Text -> (Descr -> IDEM ()) -> IDEM ()
launchSymbolNavigationDialog txt act = do
    dia                        <-   liftIO $ dialogNew
    win <- getMainWindow
    (Just searchPane, _) <- buildSearchPane
    liftIO $ do
        windowSetTransientFor dia win
        upper                      <-   dialogGetUpper dia
        lower                      <-   dialogGetActionArea dia
        boxPackStart' upper (topBox searchPane) PackNatural 7

        bb      <-  hButtonBoxNew
        closeB  <-  buttonNewFromStock "gtk-cancel"
        okB    <-  buttonNewFromStock "gtk-ok"
        okB `onClicked` do
            dialogResponse dia ResponseTypeOk
            widgetHideAll dia
        closeB `onClicked` do
            dialogResponse dia ResponseTypeCancel
            widgetHideAll dia
        boxPackEnd' bb closeB PackNatural 0
        boxPackEnd' bb okB PackNatural 0
        boxPackStart' lower bb PackNatural 7
        setWidgetCanDefault okB True
        buttonSetLabel okB "Goto"
        widgetGrabDefault okB
        widgetShowAll dia
        resp  <- dialogRun' dia
        return ()
    return ()
--}
