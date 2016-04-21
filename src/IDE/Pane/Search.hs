{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack, null)
import System.Log.Logger (debugM)
import Data.Monoid ((<>))
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew, ScrolledWindow(..))
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreGetValue, seqStoreAppend, seqStoreClear, seqStoreNew,
        SeqStore(..))
import GI.Gtk.Objects.VBox (vBoxNew, VBox(..))
import GI.Gtk.Objects.Entry
       (entrySetText, entryNew, entryGetText, Entry(..))
import GI.Gtk.Objects.TreeView
       (onTreeViewRowActivated, treeViewGetSelection,
        treeViewSetHeadersVisible, treeViewAppendColumn, treeViewSetModel,
        treeViewNew, TreeView(..))
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import GI.Gtk.Objects.Widget
       (toWidget, setWidgetSensitive, onWidgetKeyReleaseEvent,
        afterWidgetFocusInEvent, Widget(..))
import GI.Gtk.Objects.HBox (hBoxNew)
import GI.Gtk.Objects.RadioButton
       (RadioButton(..), radioButtonNewWithLabelFromWidget,
        radioButtonNewWithLabel)
import GI.Gtk.Objects.ToggleButton
       (toggleButtonGetActive, toggleButtonSetActive)
import GI.Gtk.Objects.CheckButton (checkButtonNewWithLabel)
import GI.Gtk.Objects.Box (boxPackEnd, boxPackStart)
import Graphics.UI.Editor.Parameters
       (boxPackEnd', boxPackStart', Packing(..))
import GI.Gtk.Objects.CellRendererText (cellRendererTextNew)
import GI.Gtk.Objects.CellRendererPixbuf (cellRendererPixbufNew)
import GI.Gtk.Objects.TreeViewColumn
       (treeViewColumnSetReorderable, treeViewColumnSetResizable,
        treeViewColumnSetSizing, treeViewColumnSetTitle, treeViewColumnNew)
import GI.Gtk.Interfaces.CellLayout (cellLayoutPackStart)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetAttributes)
import Data.GI.Base.Attributes (AttrLabelProxy(..), AttrOp(..))
import GI.Gtk.Objects.TreeSelection
       (treeSelectionSetMode)
import GI.Gtk.Enums
       (PolicyType(..), ShadowType(..), SelectionMode(..),
        TreeViewColumnSizing(..))
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.ToggleButton (onToggleButtonToggled)
import GI.Gtk.Objects.Menu (Menu(..))
import GI.Gtk.Objects.MenuItem
       (onMenuItemActivate, menuItemNewWithLabel)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathGetIndices')

_text = AttrLabelProxy :: AttrLabelProxy "text"
_stockId = AttrLabelProxy :: AttrLabelProxy "stockId"
_scale = AttrLabelProxy :: AttrLabelProxy "scale"
_scaleSet = AttrLabelProxy :: AttrLabelProxy "scaleSet"

-- | A search pane description
--

data IDESearch      =   IDESearch {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   searchStore     ::   SeqStore Descr
,   searchScopeRef  ::   IORef Scope
,   searchModeRef   ::   IORef SearchMode
,   topBox          ::   VBox
,   entry           ::   Entry
,   scopeSelection  ::   Scope -> IDEAction
,   modeSelection   ::   SearchMode -> IDEAction
,   searchMetaGUI   ::   Text -> IDEAction
,   setChoices     ::   [Descr] -> IDEAction
} deriving Typeable

data SearchState    =   SearchState {
    searchString    ::   Text
,   searchScope     ::   Scope
,   searchMode      ::   SearchMode
}   deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDESearch IDEM
    where
    primPaneName _  =   __ "Search"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . topBox
    paneId b        =   "*Search"

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
    builder pp nb windows = buildSearchPane


buildSearchPane :: IDEM (Maybe IDESearch,Connections)
buildSearchPane =
    let scope   = SystemScope
        mode    = Prefix False
    in reifyIDE $ \ ideR -> do

        scopebox        <-  hBoxNew True 2
        rb1             <-  radioButtonNewWithLabel ([]::[RadioButton]) (__ "Package")
        rb2             <-  radioButtonNewWithLabelFromWidget (Just rb1) (__ "Workspace")
        rb3             <-  radioButtonNewWithLabelFromWidget (Just rb1) (__ "System")
        toggleButtonSetActive rb3 True
        cb2             <-  checkButtonNewWithLabel (__ "Imports")

        boxPackStart' scopebox rb1 PackGrow 2
        boxPackStart' scopebox rb2 PackGrow 2
        boxPackStart' scopebox rb3 PackGrow 2
        boxPackEnd' scopebox cb2 PackNatural 2

        modebox         <-  hBoxNew True 2
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

        seqStore   <-  seqStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView (Just seqStore)

        renderer3    <- cellRendererTextNew
        renderer30   <- cellRendererPixbufNew
        col3         <- treeViewColumnNew
        treeViewColumnSetTitle col3 (__ "Symbol")
        treeViewColumnSetSizing col3 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col3 True
        treeViewColumnSetReorderable col3 True
        treeViewAppendColumn treeView col3
        cellLayoutPackStart col3 renderer30 False
        cellLayoutPackStart col3 renderer3 True
        cellLayoutSetAttributes col3 renderer3 seqStore
            $ \row -> [ _text := dscName row]
        cellLayoutSetAttributes col3 renderer30 seqStore
            $ \row -> [
            _stockId := stockIdFromType ((descrType . dscTypeHint) row)]


        renderer1    <- cellRendererTextNew
        renderer10   <- cellRendererPixbufNew
        col1         <- treeViewColumnNew
        treeViewColumnSetTitle col1 (__ "Module")
        treeViewColumnSetSizing col1 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        treeViewAppendColumn treeView col1
        cellLayoutPackStart col1 renderer10 False
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetAttributes col1 renderer1 seqStore
            $ \row -> [ _text := case dsMbModu row of
                                        Nothing -> ""
                                        Just pm -> T.pack . display $ modu pm]
        cellLayoutSetAttributes col1 renderer10 seqStore
            $ \row -> [ _stockId
                         := if isReexported row
                                    then "ide_reexported"
                                        else if isJust (dscMbLocation row)
                                            then "ide_source"
                                            else ""]

        renderer2   <- cellRendererTextNew
        col2        <- treeViewColumnNew
        treeViewColumnSetTitle col2 (__ "Package")
        treeViewColumnSetSizing col2 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col2 True
        treeViewColumnSetReorderable col2 True
        treeViewAppendColumn treeView col2
        cellLayoutPackStart col2 renderer2 True
        cellLayoutSetAttributes col2 renderer2 seqStore
            $ \row -> [ _text := case dsMbModu row of
                                        Nothing -> ""
                                        Just pm -> T.pack . display $ pack pm]

        renderer3   <- cellRendererTextNew
        col3        <- treeViewColumnNew
        treeViewColumnSetTitle col3 (__ "Type/Kind")
        treeViewColumnSetSizing col3 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col3 True
        treeViewColumnSetReorderable col3 True
        treeViewAppendColumn treeView col3
        cellLayoutPackStart col3 renderer3 True
        cellLayoutSetAttributes col3 renderer3 seqStore
            $ \row -> [ _text := T.pack . BS.unpack . fromMaybe BS.empty $
                            dscMbTypeStr row,
                        _scale := 0.8,
                        _scaleSet := True    ]

        treeViewSetHeadersVisible treeView True
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionModeSingle

        sw <- scrolledWindowNew noAdjustment noAdjustment
        scrolledWindowSetShadowType sw ShadowTypeIn
        containerAdd sw treeView
        scrolledWindowSetPolicy sw PolicyTypeAutomatic PolicyTypeAutomatic

        entry   <-  entryNew

        box             <-  vBoxNew False 2
        boxPackStart' box scopebox PackNatural 0
        boxPackStart' box sw PackGrow 0
        boxPackStart' box modebox PackNatural 0
        boxPackEnd' box entry PackNatural 0

        scopeRef  <- newIORef scope
        modeRef   <- newIORef mode
        let search = IDESearch sw treeView seqStore scopeRef modeRef box entry scopeSelection_ modeSelection_ searchMetaGUI_ setChoices_
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
            searchMetaGUI_ :: Text -> IDEAction
            searchMetaGUI_ str = do
                liftIO $ debugM "leksah" $ "searchMetGUI_ " <> T.unpack str
                bringPaneToFront search
                entrySetText entry str
                scope <- getScope search
                mode  <- getMode search
            --    let mode' = if length str > 2 then mode else Exact (caseSense mode)
                descrs <- if T.null str
                            then return []
                            else searchMeta scope str mode
                seqStoreClear (searchStore search)
                mapM_ (seqStoreAppend (searchStore search)) (take 500 descrs)
            modeSelectionCase :: Bool -> IDEAction
            modeSelectionCase caseSense = do
                oldMode <- liftIO $ readIORef (searchModeRef search)
                liftIO $ writeIORef (searchModeRef search) oldMode{caseSense = caseSense}
                text   <- entryGetText entry
                searchMetaGUI_ text
            setChoices_ :: [Descr] -> IDEAction
            setChoices_ descrs = do
                    seqStoreClear (searchStore search)
                    mapM_ (seqStoreAppend (searchStore search)) descrs
                    bringPaneToFront search
                    entrySetText entry
                        (case descrs of
                            []    -> ""
                            hd: _ -> dscName hd)
            scopeSelection' rb1 rb2 rb3 cb2 = do
                withImports <-  toggleButtonGetActive cb2
                s1 <- toggleButtonGetActive rb1
                s2 <- toggleButtonGetActive rb2
                s3 <- toggleButtonGetActive rb3
                scopeSelection_ $
                    if s1
                        then PackageScope withImports
                        else if s2
                                then WorkspaceScope withImports
                                else SystemScope

        cid1 <- ConnectC treeView <$> afterWidgetFocusInEvent treeView (\e -> do
            reflectIDE (makeActive search) ideR
            return True)
        onToggleButtonToggled rb1 $ liftIO (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
        onToggleButtonToggled rb2 $ liftIO (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
        onToggleButtonToggled rb3 $ liftIO (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
        onToggleButtonToggled cb2 $ liftIO (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR)
        onToggleButtonToggled mb1 $ liftIO $ do
            setWidgetSensitive mb4 False
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelection_ (Exact active)) ideR
        onToggleButtonToggled mb2 $ liftIO $ do
            setWidgetSensitive mb4 True
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelection_ (Prefix active)) ideR
        onToggleButtonToggled mb3 $ liftIO $ do
            setWidgetSensitive mb4 True
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelection_ (Regex active)) ideR
        onToggleButtonToggled mb4 $ liftIO $ do
            active <- toggleButtonGetActive mb4
            reflectIDE (modeSelectionCase active) ideR
        cids <- treeViewContextMenu treeView $ searchContextMenu ideR seqStore treeView
        cid4 <- ConnectC treeView <$> onTreeViewRowActivated treeView (\path col -> do
            p <- treePathGetIndices' path
            selectDescr ideR seqStore p col)
--            sel `onSelectionChanged` do
--                fillInfo search ideR
        onWidgetKeyReleaseEvent entry $ \e -> do
            text <- entryGetText entry
            reflectIDE (searchMetaGUI_ text) ideR
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
    onMenuItemActivate item1 $ liftIO $ goToDef ideR store descrView
    menuShellAppend theMenu item1

goToDef ideR store descrView = do
    sel         <-  getSelectionDescr descrView store
    case sel of
        Just descr ->  void (reflectIDE (triggerEvent ideR (GotoDefinition descr)) ideR)
                                -- (goToDefinition descr) ideR
        _          ->  sysMessage Normal (__ "Search >> listViewPopup: no selection")

selectDescr ideR store [i] col = do
    descr <- seqStoreGetValue store i
    liftIO $ reflectIDE (triggerEvent ideR (SelectIdent descr)) ideR
    return ()

selectDescr _ _ _ _ = liftIO $ sysMessage Normal (__ "Search >> selectDescr: invalid path")

getSelectionDescr :: (Applicative m, MonadIO m)
                  => TreeView
                  ->  SeqStore Descr
                  -> m (Maybe Descr)
getSelectionDescr treeView seqStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows' treeSelection
    mapM treePathGetIndices' paths >>= \case
        [a]:r ->  do
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
        set okB [widgetCanDefault := True]
        buttonSetLabel okB "Goto"
        widgetGrabDefault okB
        widgetShowAll dia
        resp  <- dialogRun' dia
        return ()
    return ()
--}
