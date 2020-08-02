{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Modules
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info@leksah.org>
-- Stability   :  provisional
-- Portability  :  portable
--
-- | The pane of ide where modules are presented in tree form with their
--   packages and exports
--
-------------------------------------------------------------------------------

module IDE.Pane.Modules (
    IDEModules(..)
,   ModulesState(..)
,   ExpanderState(..)
,   showModules
,   selectIdentifier
,   reloadKeepSelection
,   replaySelHistory
,   replayScopeHistory
,   addModule
) where

import Prelude ()
import Prelude.Compat
import Control.Monad.Fail.Compat (MonadFail)
import Data.Maybe (isNothing, fromJust, isJust)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Tree (flatten, Forest, Tree(..))
import Data.List (find, elemIndex, foldl', nub, partition, sort)
import Distribution.Package (Dependency(..), pkgName, pkgVersion)
import Distribution.Version (withinRange)
import Data.IORef
       (modifyIORef, newIORef, writeIORef, readIORef, IORef)
import IDE.Core.State
       (ModuleDescr, PackageDescr, Descr, IDEM, Scope(..),
        IDEAction, GenScope(..), PackModule, PackScope(..), IDERef,
        SymbolTable, Descr(..), RealDescr(..), TypeDescr(..), DescrType, SimpleDescr(..),
        ReexportedDescr(..), PackageAction, modu, mdModuleId, dscName,
        mdMbSourcePath, pdPackage, isReexported, dscMbLocation, dscExported, liftIDE,
        reflectIDE, dsMbModu, pack, sysMessage, MessageLevel(..),
        showPackModule, Present(..), readIDE, prefs,
        packageBlacklist, symEmpty, descrType, dscTypeHint, mdIdDescriptions,
        pdModules, pdMbSourcePath, throwIDE, currentState,
        ideMessage, ipdPackageDir, triggerEventIDE_,
        IDEEvent(..))
import IDE.Gtk.State
       (Pane(..), RecoverablePane(..), PanePath, onIDE, Connection(..),
        bringPaneToFront, withoutRecordingDo, isStartingOrClosing, getMainWindow,
        IDEGtkEvent(..), GUIHistory'(..), getNotebook)
import IDE.Pane.Info
import IDE.Pane.SourceBuffer
import Distribution.ModuleName
import Distribution.Text (simpleParse,display)
import Data.Typeable (Typeable)
import Control.Exception (SomeException(..),catch)
import IDE.Package (addModuleToPackageDescr,delModuleFromPackageDescr,getEmptyModuleTemplate,getPackageDescriptionAndPath, ModuleLocation(..))
import Distribution.PackageDescription
       (PackageDescription, BuildInfo, hsSourceDirs,
        hasLibs, executables, testSuites, exeName, testName, benchmarks,
        benchmarkName, libBuildInfo, library, buildInfo, testBuildInfo,
        benchmarkBuildInfo)
import System.FilePath (takeBaseName, (</>),dropFileName,makeRelative,takeDirectory)
import System.Directory (doesFileExist,createDirectoryIfMissing, removeFile)
import Graphics.UI.Editor.MakeEditor (buildEditor,FieldDescription(..),mkField)
import Graphics.UI.Editor.Parameters
       (dialogRun', dialogSetDefaultResponse', dialogAddButton',
        Packing(..), boxPackEnd', boxPackStart', paraMinSize, paraMultiSel,
        Parameter(..), emptyParams, (<<<-), paraName, dialogResponse')
import Graphics.UI.Editor.Simple
       (textEditor, boolEditor, staticListEditor)
import Graphics.UI.Editor.Composite (maybeEditor)
import IDE.Utils.GUIUtils
       (stockIdFromType, __, printf, treeViewContextMenu, treeViewToggleRow, showConfirmDialog)
import IDE.Metainfo.Provider
       (getSystemInfo, getWorkspaceInfo, getPackageInfo)
import System.Log.Logger (debugM)
import Data.Default (Default(..))
import IDE.Gtk.Workspaces (packageTry)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when, void)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text, toCaseFold)
import qualified Data.Text as T (unpack, isInfixOf, toLower, pack)
import qualified Data.Text.IO as T (writeFile)
import GI.Gtk.Objects.Paned
       (panedAdd2, panedAdd1, panedSetPosition, panedGetPosition,
        panedNew, Paned(..))
import Data.GI.Gtk.ModelView.ForestStore
       (forestStoreClear, forestStoreNew, forestStoreInsertTree,
        forestStoreGetTree, forestStoreGetValue, ForestStore(..))
import GI.Gtk.Objects.RadioButton
       (radioButtonNewWithLabelFromWidget, radioButtonNewWithLabel,
        RadioButton(..))
import GI.Gtk.Objects.CheckButton
       (checkButtonNewWithLabel, CheckButton(..))
import GI.Gtk.Structs.TreePath
       (TreePath(..))
import GI.Gtk.Objects.Widget
       (widgetShowAll, widgetGrabDefault, setWidgetCanDefault, widgetShow,
        widgetSetName, widgetDestroy, widgetSetSensitive,
        afterWidgetFocusInEvent, widgetGetAllocation, toWidget)
import GI.Gtk.Objects.CellRendererPixbuf
       (setCellRendererPixbufStockId, cellRendererPixbufNew)
import Data.GI.Base (unsafeCastTo)
import Data.GI.Base.GObject (new')
import GI.Gtk.Objects.CellRendererText
       (setCellRendererTextText, cellRendererTextNew)
import GI.Gtk.Objects.TreeViewColumn
       (TreeViewColumn(..), treeViewColumnSetSortColumnId,
        treeViewColumnSetReorderable, treeViewColumnSetResizable,
        treeViewColumnSetSizing, treeViewColumnSetTitle, treeViewColumnNew)
import GI.Gtk.Enums
       (ButtonsType(..), MessageType(..), WindowPosition(..),
        ResponseType(..), ButtonBoxStyle(..), PolicyType(..),
        ShadowType(..), SortType(..), TreeViewColumnSizing(..),
        Orientation(..))
import GI.Gtk.Interfaces.CellLayout (cellLayoutPackStart)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction)
import GI.Gtk.Interfaces.TreeModel
       (treeModelIterNext, treeModelIterChildren, treeModelGetIterFirst,
        treeModelGetPath, treeModelSortNewWithModel)
import GI.Gtk.Interfaces.TreeSortable
       (treeSortableSetSortColumnId, treeSortableSetSortFunc)
import Data.GI.Gtk.ModelView.CustomStore
       (customStoreGetRow)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathGetIndices',
        treePathNewFromIndices', TypedTreeModelSort(..))
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (Adjustment)
import GI.Gtk.Objects.Container (containerAdd)
import Graphics.UI.Frame.Rectangle
       (getRectangleY, getRectangleX)
import GI.Gtk.Objects.Box (boxReorderChild, Box(..), boxSetSpacing, boxNew)
import GI.Gtk.Objects.ButtonBox (buttonBoxSetLayout, buttonBoxNew)
import GI.Gtk.Objects.ToggleButton
       (toggleButtonGetActive, onToggleButtonToggled,
        toggleButtonSetActive)
import GI.Gtk.Objects.TreeView
       (treeViewSetSearchColumn, treeViewCollapseRow, treeViewCollapseAll,
        treeViewExpandAll, treeViewExpandRow, treeViewRowExpanded,
        treeViewScrollToCell, treeViewGetColumn, treeViewExpandToPath,
        treeViewGetSelection, onTreeViewRowActivated,
        treeViewSetSearchEqualFunc, treeViewSetEnableSearch,
        treeViewSetHeadersVisible, treeViewAppendColumn, treeViewSetModel,
        treeViewNew, TreeView(..))
import GI.Gtk.Objects.TreeSelection
       (treeSelectionUnselectAll, treeSelectionSelectPath, onTreeSelectionChanged)
import GI.Gtk.Objects.TreeModelSort
       (treeModelSortConvertPathToChildPath,
        treeModelSortConvertChildPathToPath)
import GI.Gtk.Structs.TreeIter (TreeIter(..))
import GI.Gtk.Objects.Menu (Menu(..))
import GI.Gtk.Objects.MenuItem
       (toMenuItem, onMenuItemActivate, menuItemNewWithLabel)
import GI.Gtk.Objects.SeparatorMenuItem (separatorMenuItemNew)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import GI.Gtk.Objects.Dialog
       (Dialog(..), dialogGetContentArea, constructDialogUseHeaderBar)
import GI.Gtk.Objects.Window
       (windowSetDefaultSize, setWindowTitle, setWindowTransientFor, Window(..),
        setWindowWindowPosition, windowSetTransientFor)
import GI.Gtk.Objects.Button (Button(..), onButtonClicked)
import GI.Gtk.Objects.Label
       (labelSetText, labelSetLineWrap, labelNew)
import Data.Int (Int32)
import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)
import Distribution.Types.UnqualComponentName
       (unUnqualComponentName)
import IDE.Utils.GHCUtils (viewDependency)

-- | A modules pane description
--

type ModuleRecord = (Text, Maybe (ModuleDescr,PackageDescr))

data IDEModules     =   IDEModules {
    outer            ::   Box
,   paned            ::   Paned
,   treeView         ::   TreeView
,   forestStore      ::   ForestStore ModuleRecord
,   descrView        ::   TreeView
,   descrStore       ::   ForestStore Descr
,   descrSortedStore ::   TypedTreeModelSort Descr -- ^ The sorted model for descrs
,   packageScopeB    ::   RadioButton
,   workspaceScopeB  ::   RadioButton
,   systemScopeB     ::   RadioButton
,   dependsB         ::   CheckButton
,   blacklistB       ::   CheckButton
,   oldSelection     ::   IORef SelectionState
,   expanderState    ::   IORef ExpanderState
} deriving Typeable

instance ToJSON ModuleName where
    toJSON = toJSON . T.pack . show
instance FromJSON ModuleName where
    parseJSON v = read . T.unpack <$> parseJSON v

data ModulesState           =   ModulesState Int (Scope,Bool)
                                    (Maybe ModuleName, Maybe Text) ExpanderState
    deriving(Eq,Ord,Read,Show,Typeable,Generic)

instance ToJSON ModulesState
instance FromJSON ModulesState

data ExpanderState =  ExpanderState {
    packageExp              :: ExpanderFacet
,   packageExpNoBlack       :: ExpanderFacet
,   packageDExp             :: ExpanderFacet
,   packageDExpNoBlack      :: ExpanderFacet
,   workspaceExp            :: ExpanderFacet
,   workspaceExpNoBlack     :: ExpanderFacet
,   workspaceDExp           :: ExpanderFacet
,   workspaceDExpNoBlack    :: ExpanderFacet
,   systemExp               :: ExpanderFacet
,   systemExpNoBlack        :: ExpanderFacet
}   deriving (Eq,Ord,Show,Read,Generic)

instance ToJSON ExpanderState
instance FromJSON ExpanderState

type ExpanderFacet      = ([[Int]], [[Int]])

data SelectionState = SelectionState {
    moduleS'        ::   Maybe ModuleName
,   facetS'         ::   Maybe Text
,   scope'          ::   Scope
,   blacklist'      ::   Bool}
 deriving (Eq,Ord,Show)

instance Pane IDEModules IDEM
    where
    primPaneName _  =   __ "Modules"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . outer
    paneId _        =   "*Modules"
        --liftIO $ widgetGrabFocus (descrView p)

getModules :: Maybe PanePath -> IDEM IDEModules
getModules Nothing    = forceGetPane (Right "*Modules")
getModules (Just pp)  = forceGetPane (Left pp)

showModules :: IDEAction
showModules = do
    pane <- getModules Nothing
    displayPane pane False

orderToInt32 :: Ordering -> Int32
orderToInt32 LT = -1
orderToInt32 EQ = 0
orderToInt32 GT = 1

instance RecoverablePane IDEModules ModulesState IDEM where
    saveState p'     =   do
        m           <-  getModules Nothing
        sc          <-  getScope
        mbModules   <-  getPane
        recordExpanderState
        expander    <-  liftIO $ readIORef (expanderState p')
        case mbModules of
            Nothing ->  return Nothing
            Just p  ->  do
                i   <-  panedGetPosition (paned p)
                mbTreeSelection     <-  getSelectionTree (treeView m) (forestStore m)
                mbFacetSelection    <-  getSelectionDescr (descrView m) (descrStore m) (descrSortedStore m)
                let mbs = (case mbTreeSelection of
                            Just (_,Just (md,_)) -> Just (modu $ mdModuleId md)
                            _                    -> Nothing,
                            case mbFacetSelection of
                                Nothing -> Nothing
                                Just fw -> Just (dscName fw))
                return (Just (ModulesState (fromIntegral i) sc mbs expander))
    recoverState pp (ModulesState i (scope',useBlacklist) se exp')  =  do
        nb          <-  getNotebook pp
        p           <-  buildPane pp nb builder
        mod'        <-  getModules Nothing
        liftIO $ writeIORef (expanderState mod') exp'
        liftIO $ writeIORef (oldSelection mod') (uncurry SelectionState se scope' useBlacklist)
        panedSetPosition (paned mod') (fromIntegral i)
        return p
    builder _pp nb _windows = do
        packageInfo' <- getPackageInfo
        ideR <- ask
        let forest  = case packageInfo' of
                        Nothing     ->  []
                        Just (GenScopeC fst', GenScopeC snd')
                            ->  subForest (buildModulesTree (fst',snd'))
        forestStore   <-  forestStoreNew forest
        treeView    <-  treeViewNew
        treeViewSetModel treeView (Just forestStore)
        --treeViewSetRulesHint treeView True

        renderer0    <- cellRendererPixbufNew
        setCellRendererPixbufStockId renderer0 ""

        renderer    <- cellRendererTextNew
        colModule   <- treeViewColumnNew
        treeViewColumnSetTitle colModule (__ "Module")
        treeViewColumnSetSizing colModule TreeViewColumnSizingAutosize
        treeViewColumnSetResizable colModule True
        treeViewColumnSetReorderable colModule True
        _ <- treeViewAppendColumn treeView colModule
        cellLayoutPackStart colModule renderer0 False
        cellLayoutPackStart colModule renderer True
        cellLayoutSetDataFunction colModule renderer forestStore
            $ setCellRendererTextText renderer . fst
        cellLayoutSetDataFunction colModule renderer0 forestStore
            $ \row ->
                setCellRendererPixbufStockId renderer0 $
                    case snd row of
                        Nothing -> ""
                        Just pair -> if isJust (mdMbSourcePath (fst pair))
                                         then "ide_source"
                                         else ""

        renderer2   <- cellRendererTextNew
        colPackage  <- treeViewColumnNew
        treeViewColumnSetTitle colPackage (__ "Package")
        treeViewColumnSetSizing colPackage TreeViewColumnSizingAutosize
        treeViewColumnSetResizable colPackage True
        treeViewColumnSetReorderable colPackage True
        _ <- treeViewAppendColumn treeView colPackage
        cellLayoutPackStart colPackage renderer2 True
        cellLayoutSetDataFunction colPackage renderer2 forestStore
            $ \row ->
                setCellRendererTextText renderer2 $
                    case snd row of
                        Nothing -> ""
                        Just pair -> (T.pack . display . pdPackage . snd) pair

        treeViewSetHeadersVisible treeView True
        treeViewSetEnableSearch treeView True
        treeViewSetSearchEqualFunc treeView
            (\_ _ key iter -> treeViewSearch treeView forestStore key iter)
        treeViewSetSearchColumn treeView 0

    -- Facet view

        descrView   <-  treeViewNew
        descrStore  <-  forestStoreNew []
        -- sorted model
        descrSortedStore <- treeModelSortNewWithModel descrStore
            >>= liftIO . unsafeCastTo TypedTreeModelSort
        treeViewSetModel descrView (Just descrSortedStore)
        renderer30    <- cellRendererPixbufNew
        renderer31    <- cellRendererPixbufNew
        renderer3   <- cellRendererTextNew
        colInterface <- treeViewColumnNew
        treeViewColumnSetTitle colInterface (__ "Interface")
        -- treeViewColumnSetSizing colInterface TreeViewColumnSizingAutosize
        _ <- treeViewAppendColumn descrView colInterface
        cellLayoutPackStart colInterface renderer30 False
        cellLayoutPackStart colInterface renderer31 False
        cellLayoutPackStart colInterface renderer3 True
        cellLayoutSetDataFunction colInterface renderer3 descrStore
            $ setCellRendererTextText renderer3 . descrTreeText
        cellLayoutSetDataFunction colInterface renderer30 descrStore
            $ setCellRendererPixbufStockId renderer30 . stockIdFromType . descrIdType
        cellLayoutSetDataFunction colInterface renderer31 descrStore
            $ \row ->
                setCellRendererPixbufStockId renderer31 $
                    if isReexported row
                        then "ide_reexported"
                        else if isJust (dscMbLocation row)
                            then
                                if dscExported row
                                    then "ide_source"
                                    else "ide_source_local"
                            else "ide_empty"
        -- sort definitions on name, ignoring case
        treeSortableSetSortFunc descrSortedStore 2 $ \_ iter1 iter2 -> do
            d1 <- customStoreGetRow descrStore iter1
            d2 <- customStoreGetRow descrStore iter2
            let cafName = toCaseFold . descrTreeText
            return . orderToInt32 $ compare (cafName d1) (cafName d2)
        treeViewColumnSetSortColumnId colInterface 2
        treeSortableSetSortColumnId descrSortedStore 2 SortTypeAscending
        treeViewSetHeadersVisible descrView True
        treeViewSetEnableSearch descrView True
        treeViewSetSearchEqualFunc descrView
            (\_ _ key iter -> descrViewSearch descrView descrStore key iter)
        paned           <-  panedNew OrientationHorizontal
        sw              <-  scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
        scrolledWindowSetShadowType sw ShadowTypeIn
        containerAdd sw treeView
        scrolledWindowSetPolicy sw PolicyTypeAutomatic PolicyTypeAutomatic
        sw2             <-  scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
        scrolledWindowSetShadowType sw2 ShadowTypeIn
        containerAdd sw2 descrView
        scrolledWindowSetPolicy sw2 PolicyTypeAutomatic PolicyTypeAutomatic
        panedAdd1 paned sw
        panedAdd2 paned sw2
        rect <- widgetGetAllocation nb
        x <- getRectangleX rect
        _y <- getRectangleY rect
        panedSetPosition paned (max 200 (x `quot` 2))
        box <- buttonBoxNew OrientationHorizontal
        boxSetSpacing box 2
        buttonBoxSetLayout box ButtonBoxStyleSpread
        packageScopeB <- radioButtonNewWithLabel ([]::[RadioButton]) (__ "Package")
        workspaceScopeB <- radioButtonNewWithLabelFromWidget (Just packageScopeB) (__ "Workspace")
        systemScopeB <- radioButtonNewWithLabelFromWidget (Just packageScopeB) (__ "System")
        toggleButtonSetActive systemScopeB True
        dependsB <- checkButtonNewWithLabel (__ "Imports")
        blacklistB <- checkButtonNewWithLabel (__ "Blacklist")

        boxPackStart' box packageScopeB PackGrow 0
        boxPackStart' box workspaceScopeB PackGrow 0
        boxPackStart' box systemScopeB PackGrow 0
        boxPackEnd' box blacklistB PackNatural 0
        boxPackEnd' box dependsB PackNatural 0


        outer <-  boxNew OrientationVertical 0
        boxPackStart' outer box PackNatural 2
        boxPackStart' outer paned PackGrow 0
        oldSelection <- liftIO $ newIORef $ SelectionState Nothing Nothing SystemScope False
        expanderState <- liftIO $ newIORef emptyExpansion
        let modules = IDEModules {..}
        cid1 <- onIDE afterWidgetFocusInEvent treeView $ do
            liftIDE $ makeActive modules
            return True
        cid2 <- onIDE afterWidgetFocusInEvent descrView $ do
            liftIDE $ makeActive modules
            return True
        cids3 <- treeViewContextMenu treeView $ modulesContextMenu ideR forestStore treeView
        cid5 <- ConnectC treeView <$> onTreeViewRowActivated treeView (modulesSelect ideR forestStore treeView)
        cids6 <- treeViewContextMenu descrView $ descrViewContextMenu ideR descrStore descrSortedStore descrView
        cid8 <- ConnectC descrView <$> onTreeViewRowActivated descrView (descrViewSelect ideR descrStore descrSortedStore)
        _ <- onToggleButtonToggled packageScopeB (reflectIDE scopeSelection ideR)
        _ <- onToggleButtonToggled workspaceScopeB (reflectIDE scopeSelection ideR)
        _ <- onToggleButtonToggled systemScopeB (reflectIDE scopeSelection ideR)
        _ <- onToggleButtonToggled blacklistB  (reflectIDE scopeSelection ideR)
        _ <- onToggleButtonToggled dependsB (reflectIDE scopeSelection ideR)
        sel     <-  treeViewGetSelection treeView
        _ <- onTreeSelectionChanged sel $ do
            fillFacets treeView forestStore descrView descrStore descrSortedStore
            reflectIDE recordSelHistory ideR
            return ()
        sel2    <-  treeViewGetSelection descrView
        _ <- onTreeSelectionChanged sel2 $ do
            fillInfo descrView descrStore descrSortedStore ideR
            reflectIDE recordSelHistory ideR
            return ()
        return (Just modules, [cid1, cid2, cid5, cid8] ++ cids3 ++ cids6)

selectIdentifier :: Descr -> Bool -> Bool -> IDEAction
selectIdentifier idDescr activatePanes openSource= do
    liftIO $ debugM "leksah" "selectIdentifier"
    systemScope     <- getSystemInfo
    workspaceScope  <- getWorkspaceInfo
    packageScope    <- getPackageInfo
    currentScope    <- getScope
    case dsMbModu idDescr of
        Nothing -> return ()
        Just pm -> case scopeForDescr pm packageScope workspaceScope systemScope of
                        Nothing -> return ()
                        Just sc -> do
                            when (fst currentScope < sc) (setScope activatePanes (sc,snd currentScope))
                            selectIdentifier' (modu pm) (dscName idDescr) activatePanes
    when openSource (goToDefinition idDescr)

scopeForDescr :: PackModule -> Maybe (GenScope,GenScope) ->
    Maybe (GenScope,GenScope) -> Maybe GenScope -> Maybe Scope
scopeForDescr pm packageScope workspaceScope systemScope =
    case ps of
        (True, r) -> Just (PackageScope r)
        _         ->
            case ws of
                (True, r) -> Just (WorkspaceScope r)
                _         -> case systemScope of
                                Nothing -> Nothing
                                Just (GenScopeC(PackScope ssc _)) -> if Map.member pid ssc
                                                                        then Just SystemScope
                                                                        else Nothing
    where
    pid = pack pm
    ps  = case packageScope of
                    Nothing -> (False,False)
                    Just
                      (GenScopeC (PackScope psc1 _),
                       GenScopeC (PackScope psc2 _)) | Map.member pid psc1 ->
                                                       (True, False)
                                                     | Map.member pid psc2 -> (True, True)
                                                     | otherwise -> (False, False)
    ws  = case workspaceScope of
                        Nothing -> (False,False)
                        Just
                          (GenScopeC (PackScope wsc1 _),
                           GenScopeC (PackScope wsc2 _)) | Map.member pid wsc1 ->
                                                           (True, False)
                                                         | Map.member pid wsc2 -> (True, True)
                                                         | otherwise -> (False, False)


selectIdentifier' :: ModuleName -> Text -> Bool -> IDEAction
selectIdentifier'  moduleName symbol activatePanes =
    let nameArray = map T.pack $ components moduleName
    in do
        liftIO $ debugM "leksah" $ "selectIdentifier' " <> show moduleName <> " " <> T.unpack symbol <> " " <> show activatePanes
        mods            <- getModules Nothing
        mbTree          <- forestStoreGetTreeSave (forestStore mods) =<< treePathNewFromIndices' []
        case treePathFromNameArray mbTree nameArray [] of
            Just treePath'  ->  do
                treePath <- treePathNewFromIndices' (map fromIntegral treePath')
                treeViewExpandToPath (treeView mods) treePath
                sel         <-  treeViewGetSelection (treeView mods)
                treeSelectionSelectPath sel treePath
                Just col    <-  treeViewGetColumn (treeView mods) 0
                treeViewScrollToCell (treeView mods) (Just treePath) (Just col) True 0.3 0.3
                mbFacetTree <-  forestStoreGetTreeSave (descrStore mods) =<< treePathNewFromIndices' []
                selF        <-  treeViewGetSelection (descrView mods)
                case  findPathFor symbol mbFacetTree of
                    Nothing     ->  sysMessage Normal (__ "no path found")
                    Just childPath   ->  do
                        Just path <-  treeModelSortConvertChildPathToPath (descrSortedStore mods) =<< treePathNewFromIndices' (map fromIntegral childPath)
                        treeViewExpandToPath (descrView mods) path
                        treeSelectionSelectPath selF path
                        Just col' <- treeViewGetColumn (descrView mods) 0
                        treeViewScrollToCell (descrView mods) (Just path) (Just col') True 0.3 0.3
                when activatePanes $ bringPaneToFront mods
            Nothing         ->  return ()

findPathFor :: Text -> Maybe (Tree Descr) -> Maybe [Int]
findPathFor symbol (Just (Node _ forest)) =
    foldr ( \i mbTreePath -> findPathFor' [i] (forest !! i) mbTreePath)
                            Nothing  [0 .. (length forest - 1)]
    where
    findPathFor' :: [Int] -> Tree Descr -> Maybe [Int] -> Maybe [Int]
    findPathFor' _ _node (Just p)                  =   Just p
    findPathFor' path (Node wrap sub) Nothing     =
        if dscName wrap == symbol
            then Just (reverse path)
            else
                foldr ( \i mbTreePath -> findPathFor' (i:path) (sub !! i) mbTreePath)
                            Nothing     [0 .. (length sub - 1)]
findPathFor _symbol Nothing = Nothing

treePathFromNameArray :: Maybe ModTree -> [Text] -> [Int] -> Maybe [Int]
treePathFromNameArray (Just _) [] accu      =   Just (reverse accu)
treePathFromNameArray (Just tree) (h:t) accu   =
    let names   =   map (fst . rootLabel) (subForest tree)
        mbIdx   =   elemIndex h names
    in case mbIdx of
            Nothing ->  Nothing
            Just i  ->  treePathFromNameArray (Just (subForest tree !! i)) t (i:accu)
treePathFromNameArray Nothing _ _  = Nothing

treeViewSearch :: TreeView
    -> ForestStore ModuleRecord
    -> Text
    -> TreeIter
    -> IO Bool
treeViewSearch treeView forestStore string iter =  do
    liftIO $ debugM "leksah" "treeViewSearch"
    path   <- treeModelGetPath forestStore iter
    val    <- forestStoreGetValue forestStore path
    mbTree <- forestStoreGetTreeSave forestStore path
    exp'   <- treeViewRowExpanded treeView path
    when (isJust mbTree && not (null (subForest (fromJust mbTree))) && not exp') $
        let found = searchInModSubnodes (fromJust mbTree) string
        in when found $
              void $ treeViewExpandRow treeView path False
    let str2      =  case snd val of
                        Just (mod',_) ->  showPackModule  (mdModuleId mod')
                        Nothing -> ""
    let res       =  T.isInfixOf (T.toLower string) (T.toLower str2)
    return res

searchInModSubnodes :: ModTree -> Text -> Bool
searchInModSubnodes tree str =
    any
      (\ (_, mbPair) ->
         case mbPair of
             Nothing -> False
             Just (mod', _) -> let cstr
                                    = T.pack $ show (Present (mdModuleId mod'))
                                in T.isInfixOf (T.toLower str) (T.toLower cstr))
      (concatMap flatten (subForest tree))

descrViewSearch :: TreeView
    -> ForestStore Descr
    -> Text
    -> TreeIter
    -> IO Bool
descrViewSearch descrView descrStore string iter = do
    liftIO $ debugM "leksah" "descrViewSearch"
    path    <- treeModelGetPath descrStore iter
    val     <- forestStoreGetValue descrStore path
    tree <- forestStoreGetTree descrStore path
    exp' <- treeViewRowExpanded descrView path
    when (not (null (subForest tree)) && not exp') $
        let found = searchInFacetSubnodes tree string
        in when found $
              void $ treeViewExpandRow descrView path False
    return (T.isInfixOf (T.toLower string) (T.toLower (descrTreeText val)))

searchInFacetSubnodes :: DescrTree -> Text -> Bool
searchInFacetSubnodes tree str =
    any
      (T.isInfixOf (T.toLower str) . T.toLower . descrTreeText)
      (concatMap flatten (subForest tree))

-- | Fill facet view with descrs from selected module
fillFacets :: (Applicative m, MonadIO m)
           => TreeView
           -> ForestStore ModuleRecord
           -> TreeView
           -> ForestStore Descr
           -> TypedTreeModelSort Descr
           -> m ()
fillFacets treeView forestStore descrView descrStore descrSortedStore = do
    liftIO $ debugM "leksah" "fillFacets"
    sel        <- getSelectionTree treeView forestStore
    emptyModel <- forestStoreNew []
    treeViewSetModel descrView (Just emptyModel)
    forestStoreClear descrStore
    emptyPath <- treePathNewFromIndices' []
    case sel of
        Just (_,Just (mod',_package))
            ->  mapM_ (\(e,i) -> forestStoreInsertTree descrStore emptyPath i e)
                                    $ zip (buildFacetForrest mod') [0 ..]
        _   ->  return ()
    treeViewSetModel descrView (Just descrSortedStore)
    treeViewSetEnableSearch descrView True
    treeViewSetSearchEqualFunc descrView
        (\_ _ key iter -> descrViewSearch descrView descrStore key iter)
    liftIO $ debugM "leksah" "fillFacets done"

getSelectionTree :: (Applicative m, MonadIO m)
                 => TreeView
                 -> ForestStore ModuleRecord
                 -> m (Maybe ModuleRecord)
getSelectionTree treeView forestStore = do
    liftIO $ debugM "leksah" "getSelectionTree"
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows' treeSelection
    case paths of
        []  ->  return Nothing
        a:_ ->  Just <$> forestStoreGetValue forestStore a

-- | Get selected Descr, if any
getSelectionDescr :: (Applicative m, MonadFail m, MonadIO m)
                  => TreeView
                  -> ForestStore Descr
                  -> TypedTreeModelSort Descr
                  -> m (Maybe Descr)
getSelectionDescr treeView forestStore descrSortedStore = do
    liftIO $ debugM "leksah" "getSelectionDescr"
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows' treeSelection
    case paths of
        a:_ ->  do
            Just unsorteda <- treeModelSortConvertPathToChildPath descrSortedStore a
            Just <$> forestStoreGetValue forestStore unsorteda
        _  ->  return Nothing

-- | Fill info pane with selected Descr if any
fillInfo :: (Applicative m, MonadFail m, MonadIO m)
         => TreeView
         -> ForestStore Descr
         -> TypedTreeModelSort Descr
         -> IDERef
         -> m ()
fillInfo treeView lst descrSortedStore ideR  = do
    liftIO $ debugM "leksah" "fillInfo"
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows' treeSelection
    case paths of
        []      ->  return ()
        [a]     ->  do
            Just unsorteda <- treeModelSortConvertPathToChildPath descrSortedStore a
            descr    <-  forestStoreGetValue lst unsorteda
            liftIO $ reflectIDE (setInfo descr) ideR
            return ()
        _       ->  return ()

--findDescription :: SymbolTable alpha => PackModule -> alpha -> Text -> Maybe (Text,Descr)
--findDescription md st s     =
--    case filter (\id' -> case dsMbModu id' of
--                            Nothing -> False
--                            Just pm -> md == pm) (symLookup s st) of
--        l:_  -> Just (s, l)
--        _ -> Nothing

getEmptyDefaultScope :: Map Text [Descr]
getEmptyDefaultScope = Map.empty

fillModulesList :: (Scope,Bool) -> IDEAction
fillModulesList (scope,useBlacklist) = do
    liftIO $ debugM "leksah" "fillModulesList"
    mods  <-  getModules Nothing
    prefs'                       <-  readIDE prefs
    case scope of
        SystemScope -> do
            accessibleInfo'             <-  getSystemInfo
            case accessibleInfo' of
                Nothing ->  forestStoreClear (forestStore mods)
                                     --forestStoreInsertTree (forestStore mods) [] 0 (Node ("",[]) [])
                Just (GenScopeC ai@(PackScope pm ps)) ->
                    let p2  =   if useBlacklist
                                    then PackScope (Map.filter (filterBlacklist
                                            (packageBlacklist prefs')) pm) ps
                                    else ai
                        (Node _ li) = buildModulesTree (PackScope Map.empty getEmptyDefaultScope,p2)
                    in insertIt li mods
        WorkspaceScope withImports -> do
            workspaceInfo'           <-  getWorkspaceInfo
            -- packageInfo'             <-  getPackageInfo
            case workspaceInfo' of
                Nothing ->  insertIt [] mods
                Just (GenScopeC l,GenScopeC p) ->
                    let (l',p'@(PackScope pm ps)) =   (l, if withImports
                                                        then p
                                                        else PackScope Map.empty symEmpty)
                        p2              =   if useBlacklist
                                                then PackScope (Map.filter (filterBlacklist
                                                        (packageBlacklist prefs')) pm) ps
                                                else p'
                        (Node _ li)     =   buildModulesTree (l', p2)
                    in insertIt li mods
        PackageScope withImports -> do
            packageInfo' <- getPackageInfo
            case packageInfo' of
                Nothing             ->   insertIt [] mods
                Just (GenScopeC l,GenScopeC p) ->
                    let (l',p'@(PackScope pm ps)) =   if withImports
                                                        then (l,p)
                                                        else (l, PackScope Map.empty symEmpty)
                        p2              =   if useBlacklist
                                                then PackScope (Map.filter (filterBlacklist
                                                        (packageBlacklist prefs')) pm) ps
                                                else p'
                        (Node _ li)     =   buildModulesTree (l', p2)
                    in insertIt li mods
    where
        insertIt li mods = do
            emptyModel <- forestStoreNew []
            treeViewSetModel (treeView mods) (Just emptyModel)
            forestStoreClear (forestStore mods)
            emptyPath <- treePathNewFromIndices' []
            mapM_ (\(e,i) -> forestStoreInsertTree (forestStore mods) emptyPath i e)
                    $ zip li [0 .. length li]
            treeViewSetModel (treeView mods) (Just $ forestStore mods)
            treeViewSetEnableSearch (treeView mods) True
            treeViewSetSearchEqualFunc (treeView mods)
                (\_ _ key iter -> treeViewSearch (treeView mods) (forestStore mods) key iter)


filterBlacklist :: [Dependency] -> PackageDescr -> Bool
filterBlacklist dependencies packageDescr =
    let packageId'  =   pdPackage packageDescr
        name        =   pkgName packageId'
        version     =   pkgVersion packageId'
    in  isNothing $ find (\ (viewDependency -> (str, vr, _)) -> str == name && withinRange version vr)
                    dependencies


type DescrForest = Forest Descr
type DescrTree = Tree Descr


descrTreeText :: Descr -> Text
descrTreeText (Real (RealDescr id' _ _ _ _ (InstanceDescr binds) _)) = id' <> " " <> printBinds binds
    where
        printBinds []       =   ""
        printBinds [a]   =   a
        printBinds (a:b)    =   a <> " " <> printBinds b
descrTreeText d = dscName d

descrIdType :: Descr -> DescrType
descrIdType = descrType . dscTypeHint

buildFacetForrest ::  ModuleDescr -> DescrForest
buildFacetForrest modDescr =
    let (instances,other)       =   partition (\id' -> case dscTypeHint id' of
                                                        InstanceDescr _ -> True
                                                        _   -> False)
                                            $ take 2000 $  mdIdDescriptions modDescr
                                -- TODO: Patch for toxioc TypeLevel package with 28000 aliases
        forestWithoutInstances  =   map buildFacet other
        (forest2,orphaned)      =   foldl' addInstances (forestWithoutInstances,[])
                                         instances
        orphanedNodes           =   map (\ inst -> Node inst []) orphaned
        in forest2 ++ reverse orphanedNodes
    where
    -- expand nodes in a module desciption for the description tree
    buildFacet :: Descr -> DescrTree
    buildFacet descr =
            case dscTypeHint descr of
                DataDescr constructors fields ->
                    Node descr (map
                                  (\ (SimpleDescr fn ty loc comm exp') ->
                                     Node
                                       (makeReexported descr
                                          (Real
                                             RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                                                       dscMbModu' = dsMbModu descr, dscMbLocation' = loc,
                                                       dscMbComment' = comm, dscTypeHint' = ConstructorDescr descr,
                                                       dscExported' = exp'}))
                                       [])
                                  constructors
                                  ++
                                  map
                                    (\ (SimpleDescr fn ty loc comm exp') ->
                                       Node
                                         (makeReexported descr
                                            (Real
                                               RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                                                         dscMbModu' = dsMbModu descr, dscMbLocation' = loc,
                                                         dscMbComment' = comm, dscTypeHint' = FieldDescr descr,
                                                         dscExported' = exp'}))
                                         [])
                                    fields)
                NewtypeDescr (SimpleDescr fn ty loc comm exp') mbField ->
                    Node descr (Node (makeReexported descr (Real
                                                              RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                                                                        dscMbModu' = dsMbModu descr, dscMbLocation' = loc,
                                                                        dscMbComment' = comm, dscTypeHint' = ConstructorDescr descr,
                                                                        dscExported' = exp'})) []
                                : case  mbField of
                                    Just (SimpleDescr fn' ty' loc' comm' exp'') ->
                                        [Node (makeReexported descr (Real
                                                                       RealDescr{dscName' = fn', dscMbTypeStr' = ty',
                                                                                 dscMbModu' = dsMbModu descr, dscMbLocation' = loc',
                                                                                 dscMbComment' = comm', dscTypeHint' = FieldDescr descr,
                                                                                 dscExported' = exp''})) []]
                                    Nothing -> [])
                ClassDescr _ methods ->
                    Node descr (map (\(SimpleDescr fn ty loc comm exp') ->
                        Node (makeReexported descr (Real
                                                      RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                                                                dscMbModu' = dsMbModu descr, dscMbLocation' = loc,
                                                                dscMbComment' = comm, dscTypeHint' = MethodDescr descr,
                                                                dscExported' = exp'})) [])
                                methods)
                _ -> Node descr []
        where
            makeReexported :: Descr -> Descr -> Descr
            makeReexported (Reexported d1) d2 = Reexported ReexportedDescr{dsrMbModu = dsrMbModu d1, dsrDescr = d2}
            makeReexported _ d2               = d2

    addInstances :: (DescrForest,[Descr])
        -> Descr
        -> (DescrForest,[Descr])
    addInstances (forest,orphaned) instDescr =
        case foldr (matches instDescr) ([],False) forest of
            (f,True)    -> (f,orphaned)
            (_,False)   -> (forest, instDescr:orphaned)

    matches :: Descr
        ->  DescrTree
        -> (DescrForest,Bool)
        -> (DescrForest,Bool)
    matches (Real instDescr@RealDescr{dscTypeHint' = InstanceDescr binds})
                (Node dd@(Real (RealDescr id' _ _ _ _ (DataDescr _ _) _)) sub) (forest,False)
        | not (null binds) && id' == head binds
            =   (Node dd (sub ++ [Node newInstDescr []]) : forest,True)
        where newInstDescr = if isNothing (dscMbLocation' instDescr)
                                then Real $ instDescr{dscMbLocation' = dscMbLocation dd}
                                else Real instDescr
    matches (Real instDescr@RealDescr{dscTypeHint' = InstanceDescr binds})
                (Node dd@(Real (RealDescr id' _ _ _ _ (NewtypeDescr _ _) _)) sub) (forest,False)
        | not (null binds) && id' == head binds
            =   (Node dd (sub ++ [Node newInstDescr []]) : forest,True)
        where newInstDescr = if isNothing (dscMbLocation' instDescr)
                                then Real $ instDescr{dscMbLocation' = dscMbLocation dd}
                                else Real instDescr
    matches _ node (forest,b)  = (node:forest,b)


defaultRoot :: Tree ModuleRecord
defaultRoot = Node ("", Just (def, def)) []

type ModTree = Tree ModuleRecord
--
-- | Make a Tree with a module desription, package description pairs tree to display.
--   Their are nodes with a label but without a module (like e.g. Data).
--
buildModulesTree :: (SymbolTable alpha, SymbolTable beta) =>  (PackScope alpha,PackScope beta ) -> ModTree
buildModulesTree (PackScope localMap _,PackScope otherMap _) =
    let modDescrPackDescr =   concatMap (\p -> map (, p) (pdModules p))
                                    (Map.elems localMap ++ Map.elems otherMap)
        resultTree        =   foldl' insertPairsInTree defaultRoot modDescrPackDescr
        in sortTree resultTree

-- | Insert module and package info into the tre
insertPairsInTree :: ModTree -> (ModuleDescr,PackageDescr) -> ModTree
insertPairsInTree tree pair =
    let nameArray           =   modTreeName $ fst pair
        (startArray,last')   =   splitAt (length nameArray - 1) nameArray
        pairedWith          =   map (, Nothing) startArray ++ [(head last', Just pair)]
    in  insertNodesInTree pairedWith tree
    where modTreeName modDescr = let
                    modId = mdModuleId modDescr
                    modName = modu modId
                    mFilePath = mdMbSourcePath modDescr
                    -- show relative file path for Main modules
                    -- since we can have several
                    in case (components modName,mFilePath) of
                        (["Main"],Just fp) ->
                            let sfp = case pdMbSourcePath (snd pair) of
                                        Nothing -> fp
                                        Just pfp -> makeRelative (takeDirectory pfp) fp
                            in  [T.pack $ "Main ("++sfp++")"]
                        (cmps,_) -> map T.pack cmps

insertNodesInTree :: [ModuleRecord] -> ModTree -> ModTree
insertNodesInTree  [p1@(str1,Just _pair)] (Node p2 forest2) =
    case partition (\ (Node (s,_) _) -> s == str1) forest2 of
        ([found],rest) -> case found of
                            Node (_,Nothing) forest3 ->
                                Node p2 (Node p1 forest3 : rest)
                            Node p3@(_,Just _pair3) forest3 ->
                                Node p2 (Node p1 [] : Node p3 forest3 : rest)
        ([],_rest)      -> Node p2 (Node p1 [] : forest2)
        (found,rest)   -> case head found of
                            Node (_,Nothing) forest3 ->
                                Node p2 (Node p1 forest3 : tail found ++ rest)
                            Node p3@(_,Just _pair3) forest3 ->
                                Node p2 (Node p1 [] : Node p3 forest3 : tail found ++ rest)

insertNodesInTree li@((str1,Nothing):tl) (Node p forest) =
    case partition (\ (Node (s,_) _) -> s == str1) forest of
        ([found],rest) -> Node p  (insertNodesInTree tl found : rest)
        ([],_rest)      -> Node p  (makeNodes li : forest)
        (found,rest)   -> Node p  (insertNodesInTree tl (head found) : tail found ++ rest)

insertNodesInTree [] n = n
insertNodesInTree _ _      =   error (T.unpack $ __ "Modules>>insertNodesInTree: Should not happen2")


makeNodes :: [(Text,Maybe (ModuleDescr,PackageDescr))] -> ModTree
makeNodes [(str,mbPair)]    =   Node (str,mbPair) []
makeNodes ((str,mbPair):tl) =   Node (str,mbPair) [makeNodes tl]
makeNodes _                 =   throwIDE (__ "Impossible in makeNodes")

instance Ord a => Ord (Tree a) where
    compare (Node l1 _) (Node l2 _) =  compare l1 l2

sortTree :: Ord a => Tree a -> Tree a
sortTree (Node l forest)    =   Node l (sort (map sortTree forest))

getSelectedModuleFile :: Maybe ModuleRecord -> Maybe FilePath
getSelectedModuleFile sel =
    case sel of
        Just (_,Just (m,p)) -> case (mdMbSourcePath m, pdMbSourcePath p) of
                                (Just fp, Just pp) -> Just $ dropFileName pp </> fp
                                (Just fp, Nothing) -> Just fp
                                _     ->  Nothing
        _                   ->  Nothing

modulesContextMenu :: IDERef
                   -> ForestStore ModuleRecord
                   -> TreeView
                   -> Menu
                   -> IO ()
modulesContextMenu ideR store treeView theMenu = do
    liftIO $ debugM "leksah" "modulesContextMenu"
    item1 <- menuItemNewWithLabel (__ "Edit source")
    _ <- onMenuItemActivate item1 $ do
        mbFile <- getSelectedModuleFile <$> getSelectionTree treeView store
        case mbFile of
            Nothing -> return ()
            Just fp -> void $ reflectIDE (selectSourceBuf fp) ideR
    sep1 <- separatorMenuItemNew >>= liftIO . toMenuItem
    item2 <- menuItemNewWithLabel (__ "Expand here")
    _ <- onMenuItemActivate item2 $ expandHere treeView
    item3 <- menuItemNewWithLabel (__ "Collapse here")
    _ <- onMenuItemActivate item3 $ collapseHere treeView
    item4 <- menuItemNewWithLabel (__ "Expand all")
    _ <- onMenuItemActivate item4 $ treeViewExpandAll treeView
    item5 <- menuItemNewWithLabel (__ "Collapse all")
    _ <- onMenuItemActivate item5 $ treeViewCollapseAll treeView
    sep2 <- separatorMenuItemNew >>= liftIO . toMenuItem
    item6 <- menuItemNewWithLabel (__ "Add module")
    _ <- onMenuItemActivate item6 $ reflectIDE (packageTry $ addModule' treeView store) ideR
    item7 <- menuItemNewWithLabel (__ "Delete module")
    _ <- onMenuItemActivate item7 $ do
        mbFile <- getSelectedModuleFile <$> getSelectionTree treeView store
        case mbFile of
            Nothing     ->  return ()
            Just fp     ->  do
                resp <- reflectIDE respDelModDialog ideR
                when resp $ do
                    exists <- doesFileExist fp
                    if exists
                       then do
                         reflectIDE (liftIO $ removeFile fp) ideR
                         reflectIDE (packageTry $ delModule treeView store)ideR
                       else
                         reflectIDE (packageTry $ delModule treeView store)ideR
                    return ()
    sel <- getSelectionTree treeView store
    case sel of
        Just (_, Nothing) ->
            mapM_ (menuShellAppend theMenu) [item1, sep1, item2,
                item3, item4, item5, sep2, item6]
        Just (_,Just (_,_)) ->
            mapM_ (menuShellAppend theMenu) [item1, sep1, item2,
                item3, item4, item5, sep2, item6, item7]
        _ -> return ()

modulesSelect :: IDERef
              -> ForestStore ModuleRecord
              -> TreeView
              -> TreePath
              -> TreeViewColumn
              -> IO ()
modulesSelect ideR store treeView path _ = do
    liftIO $ debugM "leksah" "modulesSelect"
    _ <- treeViewToggleRow treeView path
    mbFile <- getSelectedModuleFile <$> getSelectionTree treeView store
    case mbFile of
        Nothing -> return ()
        Just fp -> liftIO $ void (reflectIDE (selectSourceBuf fp) ideR)

-- | Build contextual menu on selected Descr
descrViewContextMenu :: IDERef
                   -> ForestStore Descr
                   -> TypedTreeModelSort Descr
                   -> TreeView
                   -> Menu
                   -> IO ()
descrViewContextMenu ideR store descrSortedStore descrView theMenu = do
    liftIO $ debugM "leksah" "descrViewContextMenu"
    item1 <- menuItemNewWithLabel (__ "Go to definition")
    _ <- onMenuItemActivate item1 $ do
        sel         <-  getSelectionDescr descrView store descrSortedStore
        case sel of
            Just descr      ->  reflectIDE (goToDefinition descr) ideR
            _               ->  sysMessage Normal (__ "Modules>> descrViewPopup: no selection")
    item2 <-  menuItemNewWithLabel (__ "Insert in buffer")
    _ <- onMenuItemActivate item2 $ do
        sel         <-  getSelectionDescr descrView store descrSortedStore
        case sel of
            Just descr      ->  reflectIDE (insertInBuffer descr) ideR
            _               ->  sysMessage Normal (__ "Modules>> descrViewPopup: no selection")
    mapM_ (menuShellAppend theMenu) [item1, item2]

-- | Selects the Descr referenced by the path
descrViewSelect :: IDERef
              -> ForestStore Descr
              -> TypedTreeModelSort Descr
              -> TreePath
              -> TreeViewColumn
              -> IO ()
descrViewSelect ideR store descrSortedStore path _ = do
    liftIO $ debugM "leksah" "descrViewSelect"
    Just unsortedp <- treeModelSortConvertPathToChildPath descrSortedStore path
    descr <- forestStoreGetValue store unsortedp
    reflectIDE (goToDefinition descr) ideR

setScope :: Bool -> (Scope,Bool) -> IDEAction
setScope bringToFront (sc,bl) = do
    liftIO $ debugM "leksah" "setScope"
    mods  <-  getModules Nothing
    case sc of
        (PackageScope False) -> do
            toggleButtonSetActive (packageScopeB mods) True
            widgetSetSensitive (dependsB mods) True
            toggleButtonSetActive (dependsB mods) False
        (PackageScope True) -> do
            toggleButtonSetActive (packageScopeB mods) True
            widgetSetSensitive (dependsB mods) True
            toggleButtonSetActive (dependsB mods) True
        (WorkspaceScope False) -> do
            toggleButtonSetActive (workspaceScopeB mods) True
            widgetSetSensitive (dependsB mods) True
            toggleButtonSetActive (dependsB mods) False
        (WorkspaceScope True) -> do
            toggleButtonSetActive (workspaceScopeB mods) True
            widgetSetSensitive (dependsB mods) True
            toggleButtonSetActive (dependsB mods) True
        SystemScope -> do
            toggleButtonSetActive (systemScopeB mods) True
            widgetSetSensitive (dependsB mods) False
    toggleButtonSetActive (blacklistB mods) bl
    selectScope bringToFront (sc,bl)

getScope :: IDEM (Scope,Bool)
getScope = do
    liftIO $ debugM "leksah" "getScope"
    mods  <- getModules Nothing
    rb1s  <- toggleButtonGetActive (packageScopeB   mods)
    rb2s  <- toggleButtonGetActive (workspaceScopeB mods)
    _rb3s <- toggleButtonGetActive (systemScopeB    mods)
    cb1s  <- toggleButtonGetActive (dependsB        mods)
    cbs   <- toggleButtonGetActive (blacklistB      mods)
    let scope
          | rb1s      = PackageScope cb1s
          | rb2s      = WorkspaceScope cb1s
          | otherwise = SystemScope
    return (scope,cbs)

scopeSelection :: IDEAction
scopeSelection = do
    liftIO $ debugM "leksah" "scopeSelection"
    (sc,bl) <- getScope
    setScope False (sc,bl)

selectScope :: Bool -> (Scope,Bool) -> IDEAction
selectScope brintToFront (sc,bl) = do
    liftIO $ debugM "leksah" "selectScope"
    recordExpanderState
    mods                <-  getModules Nothing
    mbTreeSelection     <-  getSelectionTree (treeView mods) (forestStore mods)
    mbDescrSelection    <-  getSelectionDescr (descrView mods) (descrStore mods) (descrSortedStore mods)

    ts                  <-  treeViewGetSelection (treeView mods)
    withoutRecordingDo $ do
        treeSelectionUnselectAll ts
        fillModulesList (sc,bl)
        let mbs = (case mbTreeSelection of
                    Just (_,Just (md,_)) -> Just (modu $ mdModuleId md)
                    _ -> Nothing,
                         case mbDescrSelection of
                            Nothing -> Nothing
                            Just fw -> Just (dscName fw))

        selectNames mbs
    recordScopeHistory
    applyExpanderState
    when brintToFront $ bringPaneToFront mods

selectNames :: (Maybe ModuleName, Maybe Text) -> IDEAction
selectNames (mbModuleName, mbIdName) = do
    liftIO $ debugM "leksah" "selectIdentifier"
    mods <- getModules Nothing
    case mbModuleName of
        Nothing -> do
            sel         <-  treeViewGetSelection (treeView mods)
            treeSelectionUnselectAll  sel
            selF        <-  treeViewGetSelection (descrView mods)
            treeSelectionUnselectAll  selF
        Just moduleName ->
            let nameArray = map T.pack $ components moduleName
            in do
                mbTree              <-  forestStoreGetTreeSave (forestStore mods) =<< treePathNewFromIndices' []
                case treePathFromNameArray mbTree nameArray [] of
                    Nothing         ->  return ()
                    Just treePath'  ->  do
                        treePath <- treePathNewFromIndices' (map fromIntegral treePath')
                        treeViewExpandToPath (treeView mods) treePath
                        sel         <-  treeViewGetSelection (treeView mods)
                        treeSelectionSelectPath sel treePath
                        Just col    <-  treeViewGetColumn (treeView mods) 0
                        treeViewScrollToCell (treeView mods) (Just treePath) (Just col)
                            True 0.3 0.3
                        case mbIdName of
                            Nothing -> do
                                selF          <-  treeViewGetSelection (descrView mods)
                                treeSelectionUnselectAll  selF
                            Just symbol -> do
                                mbDescrTree   <-  forestStoreGetTreeSave (descrStore mods) =<< treePathNewFromIndices' []
                                selF          <-  treeViewGetSelection (descrView mods)
                                case  findPathFor symbol mbDescrTree of
                                    Nothing     ->  sysMessage Normal (__ "no path found")
                                    Just childPath   ->  do
                                        Just path <- treeModelSortConvertChildPathToPath (descrSortedStore mods) =<< treePathNewFromIndices' (map fromIntegral childPath)
                                        treeSelectionSelectPath selF path
                                        Just col'  <- treeViewGetColumn (descrView mods) 0
                                        treeViewScrollToCell (descrView mods) (Just path) (Just col')
                                            True 0.3 0.3

reloadKeepSelection :: Bool -> IDEAction
reloadKeepSelection isInitial = do
    liftIO . debugM "leksah" $ T.unpack (__ ">>>Info Changed!!! ") ++ show isInitial
    mbMod <- getPane
    case mbMod of
        Nothing -> return ()
        Just mods -> do
            state <- readIDE currentState
            if not $ isStartingOrClosing state
                then do
                    mbTreeSelection     <-  getSelectionTree (treeView mods) (forestStore mods)
                    mbDescrSelection    <-  getSelectionDescr (descrView mods) (descrStore mods) (descrSortedStore mods)
                    sc                  <-  getScope
                    recordExpanderState
                    fillModulesList sc
                    forestStoreClear (descrStore mods)
                    let mbs =  (case mbTreeSelection of
                                    Just (_,Just (md,_)) -> Just (modu $ mdModuleId md)
                                    _ -> Nothing,
                                 case mbDescrSelection of
                                    Nothing -> Nothing
                                    Just fw -> Just (dscName fw))

                    applyExpanderState
                    selectNames mbs
                else when isInitial $ do
                            SelectionState moduleS' facetS' sc bl <- liftIO $ readIORef (oldSelection mods)
                            setScope False (sc,bl)
                            fillModulesList (sc, bl)
                            selectNames (moduleS', facetS')
                            applyExpanderState

forestStoreGetTreeSave :: MonadIO m => ForestStore a -> TreePath -> m (Maybe (Tree a))
forestStoreGetTreeSave forestStore treePath = liftIO $ catch (do
    debugM "leksah" "forestStoreGetTreeSave"
    res <- forestStoreGetTree forestStore treePath
    return (Just res)) (\ (_ :: SomeException) -> return Nothing)

expandHere :: TreeView -> IO ()
expandHere treeView = do
    liftIO $ debugM "leksah" "expandHere"
    sel   <- treeViewGetSelection treeView
    paths <- treeSelectionGetSelectedRows' sel
    case paths of
        []     -> return ()
        (hd:_) -> void (treeViewExpandRow treeView hd True)

collapseHere :: TreeView -> IO ()
collapseHere treeView = do
    liftIO $ debugM "leksah" "collapseHere"
    sel   <- treeViewGetSelection treeView
    paths <- treeSelectionGetSelectedRows' sel
    case paths of
        []     -> return ()
        (hd:_) -> void (treeViewCollapseRow treeView hd)

delModule :: TreeView -> ForestStore ModuleRecord -> PackageAction
delModule treeview store = do
    liftIO $ debugM "leksah" "delModule"
    sel   <- treeViewGetSelection treeview
    paths <- treeSelectionGetSelectedRows' sel
    categories <- case paths of
        []     -> return []
        (treePath':_) -> do
            treePath <- treePathGetIndices' treePath'
            mapM (\n ->
                treePathNewFromIndices' (take n treePath) >>=
                    forestStoreGetValue store)
                                   [1 .. length treePath]

    liftIDE $ ideMessage Normal (T.pack $ printf (__ "categories: %s") (show categories))

    let modPacDescr = snd(last categories)
    case modPacDescr of
        Nothing     ->   liftIDE $ ideMessage Normal (__ "This should never be shown!")
        Just(md,_)  -> do
                         let modName = modu.mdModuleId $ md
                         liftIDE $ ideMessage Normal ("modName: " <> T.pack (show modName))
                         delModuleFromPackageDescr modName

respDelModDialog :: IDEM Bool
respDelModDialog = do
    liftIO $ debugM "leksah" "respDelModDialog"
    window <- getMainWindow
    isConfirmed <- showConfirmDialog (Just window) False (__ "_Delete Module") $ __ "Are you sure?"
    return isConfirmed

addModule' :: TreeView -> ForestStore ModuleRecord -> PackageAction
addModule' treeView store = do
    liftIO $ debugM "leksah" "addModule'"
    sel   <- treeViewGetSelection treeView
    paths <- treeSelectionGetSelectedRows' sel
    categories <- case paths of
        []     -> return []
        (treePath':_) -> do
            treePath <- treePathGetIndices' treePath'
            mapM (\n ->
                treePathNewFromIndices' (take n treePath) >>=
                    forestStoreGetValue store)
                                   [1 .. length treePath]
    addModule (map fst categories)

-- Includes non buildable
allBuildInfo' :: PackageDescription -> [BuildInfo]
allBuildInfo' pkg_descr = [ libBuildInfo lib       | Just lib <- [library pkg_descr] ]
                       ++ [ buildInfo exe          | exe <- executables pkg_descr ]
                       ++ [ testBuildInfo tst      | tst <- testSuites pkg_descr ]
                       ++ [ benchmarkBuildInfo tst | tst <- benchmarks pkg_descr ]

addModule :: [Text] -> PackageAction
addModule modulePrefix = do
    liftIO $ debugM "leksah" "selectIdentifier"
    mbPD <- liftIDE getPackageDescriptionAndPath
    case mbPD of
        Nothing             -> liftIDE $ ideMessage Normal (__ "No package description")
        Just (pd,_cabalPath) -> let srcPaths = nub $ concatMap hsSourceDirs $ allBuildInfo' pd
--                                    rootPath = dropFileName cabalPath
                                    modPath' = foldr (\a b -> a <> "." <> b) ""
                                                    modulePrefix
                               in do
            window' <- liftIDE getMainWindow
            mbResp <- addModuleDialog window' modPath' srcPaths (hasLibs pd) $
                      map (T.pack . unUnqualComponentName . exeName)  (executables pd)
                   ++ map (T.pack . unUnqualComponentName . testName) (testSuites pd)
                   ++ map (T.pack . unUnqualComponentName . benchmarkName) (benchmarks pd)
            case mbResp of
                Nothing                -> return ()
                Just addMod@(AddModule modPath srcPath _libExposed _exesAndTests) ->
                    case simpleParse $ T.unpack modPath of
                        Nothing         -> liftIDE $ ideMessage Normal (T.pack $ printf (__ "Not a valid module name : %s") (T.unpack modPath))
                        Just moduleName -> do
                            package <- ask
                            let  target = ipdPackageDir package </> srcPath </> toFilePath moduleName ++ ".hs"
                            liftIO $ createDirectoryIfMissing True (dropFileName target)
                            alreadyExists <- liftIO $ doesFileExist target
                            if alreadyExists
                                then do
                                    liftIDE $ ideMessage Normal (T.pack $ printf (__ "File already exists! Importing existing file %s.hs") (takeBaseName target))
                                    addModuleToPackageDescr moduleName $ addModuleLocations addMod
                                else do
                                    template <- liftIO $ getEmptyModuleTemplate pd modPath
                                    liftIO $ T.writeFile target template
                                    addModuleToPackageDescr moduleName $ addModuleLocations addMod
                                    liftIDE $ fileOpenThis target


-- | The dialog for adding a new module
data AddModule = AddModule {
    moduleName   :: Text,
    sourceRoot   :: FilePath,
    libExposed   :: Maybe Bool,
    exesAndTests :: Set Text}

addModuleLocations :: AddModule -> [ModuleLocation]
addModuleLocations addMod = lib (libExposed addMod)
    ++ map ExeOrTestMod (Set.toList $ exesAndTests addMod)
  where
    lib (Just True) = [LibExposedMod]
    lib (Just False) = [LibOtherMod]
    lib Nothing = []

-- | Creates and runs a "new module" dialog
addModuleDialog :: (Applicative m, MonadIO m)
                => Window -- ^ The parent window
                -> Text   -- ^ Will be set as default value for the module name
                -> [FilePath] -- ^ Possible source directories to add it to
                -> Bool       -- ^ Whether the active package has a library
                -> [Text]     -- ^ The components of the active package
                -> m (Maybe AddModule)
addModuleDialog parent modString sourceRoots hasLib exesTests = do
    liftIO $ debugM "leksah" "addModuleDialog"
    dia                <- new' Dialog [constructDialogUseHeaderBar 1]
    setWindowTransientFor dia parent
    setWindowTitle dia $ __ "Construct new module"
    windowSetDefaultSize dia 400 100
    upper              <- dialogGetContentArea dia >>= liftIO . unsafeCastTo Box
    (widget,_inj,ext,_) <- liftIO $ buildEditor (moduleFields sourceRoots hasLib exesTests)
                                        (AddModule modString (head sourceRoots) (Just False) Set.empty)
    okButton <- dialogAddButton' dia (__"Add Module") ResponseTypeOk >>= liftIO . unsafeCastTo Button
    dialogSetDefaultResponse' dia ResponseTypeOk
    _ <- dialogAddButton' dia (__"Cancel") ResponseTypeCancel

    errorLabel <- labelNew Nothing
    labelSetLineWrap errorLabel True
    widgetSetName errorLabel "errorLabel"

    _ <- onButtonClicked okButton $ do
        mbAddModule <- ext (AddModule modString (head sourceRoots) (Just False) Set.empty)
        case mbAddModule of
            Nothing -> return ()
            Just am -> do
                let mbModName = simpleParse $ T.unpack (moduleName am) :: Maybe ModuleName
                case mbModName of
                    Just _  -> dialogResponse' dia ResponseTypeOk
                    Nothing -> do
                        boxPackStart' upper errorLabel PackNatural 0
                        boxReorderChild upper errorLabel 0
                        labelSetText errorLabel "Invalid module name, use uppercase identifiers seperated by dots. For example Some.New.Module"
                        widgetShow errorLabel


    boxPackStart' upper widget PackGrow 0
    setWidgetCanDefault okButton True
    widgetGrabDefault okButton
    widgetShowAll dia
    resp  <- dialogRun' dia
    value <- liftIO $ ext (AddModule modString (head sourceRoots) (Just False) Set.empty)
    widgetDestroy dia
    --find
    case resp of
        ResponseTypeOk    -> return value
        _             -> return Nothing

moduleFields :: [FilePath] -> Bool -> [Text] -> FieldDescription AddModule
moduleFields list hasLibs' exesTests = VFD emptyParams $ [
        mkField
            (paraName <<<- ParaName (__ "New module ")
                    $ emptyParams)
            moduleName
            (\ a b -> b{moduleName = a})
            (textEditor (const True) True),
        mkField
            (paraName <<<- ParaName (__ "Root of the source path")
                $ paraMultiSel <<<- ParaMultiSel False
                    $ paraMinSize <<<- ParaMinSize (-1, 120)
                        $ emptyParams)
            (T.pack . sourceRoot)
            (\ a b -> b{sourceRoot = T.unpack a})
            (staticListEditor (map T.pack list) id)]
        ++ [mkField
              (paraName <<<- ParaName (__ "Library should") $ emptyParams)
              libExposed
              (\ a b -> b{libExposed = a})
              (maybeEditor True
                 (boolEditor,
                  paraName <<<- ParaName (__ "Expose module") $ emptyParams)
                 True
                 (__ "Include module"))
            | hasLibs']
        ++ map (\ name ->
            mkField
                (paraName <<<- ParaName (__ "Include in " <> name)
                        $ emptyParams)
                (Set.member name . exesAndTests)
                (\ a b -> b{exesAndTests = (if a then Set.insert else Set.delete) name (exesAndTests b)})
                boolEditor) exesTests

-- * Expander State
emptyExpansion :: ExpanderState
emptyExpansion = ExpanderState  ([],[])  ([],[])  ([],[])  ([],[])  ([],[]) ([],[])  ([],[])  ([],[])  ([],[]) ([],[])

recordExpanderState :: IDEAction
recordExpanderState = do
    liftIO $ debugM "leksah" "recordExpanderState"
    m       <- getModules Nothing
    liftIO $ do
        oldSel  <- readIORef (oldSelection m)
        let (sc,bl) =  (scope' oldSel, blacklist' oldSel)
        paths1 <-  getExpandedRows (treeView m) (forestStore m)
        paths2 <-  getExpandedRows (descrView m) (descrStore m)
        modifyIORef (expanderState m) (\ es ->
            case (sc,bl) of
                (PackageScope False, True)  -> es{packageExp         = (paths1,paths2)}
                (PackageScope False, False) -> es{packageExpNoBlack  = (paths1,paths2)}
                (PackageScope True, True)   -> es{packageDExp        = (paths1,paths2)}
                (PackageScope True, False)  -> es{packageDExpNoBlack = (paths1,paths2)}

                (WorkspaceScope False, True)  -> es{workspaceExp         = (paths1,paths2)}
                (WorkspaceScope False, False) -> es{workspaceExpNoBlack  = (paths1,paths2)}
                (WorkspaceScope True,True)    -> es{workspaceDExp        = (paths1,paths2)}
                (WorkspaceScope True,False)   -> es{workspaceDExpNoBlack = (paths1,paths2)}

                (SystemScope,True)   -> es{systemExp         = (paths1,paths2)}
                (SystemScope,False)  -> es{systemExpNoBlack  = (paths1,paths2)})
        -- st <- readIORef (expanderState m)
        return ()


getExpandedRows :: TreeView -> ForestStore alpha -> IO [[Int]]
getExpandedRows view store = do
    liftIO $ debugM "leksah" "getExpandedRows"
    mbIter <- treeModelGetIterFirst store
    case mbIter of
        (False, _)   -> return []
        (True, iter) -> expandedFor iter [] >>=
                            mapM (fmap (map fromIntegral) . treePathGetIndices')
    where
        expandedFor :: TreeIter -> [TreePath] -> IO [TreePath]
        expandedFor iter accu = do
            path <- treeModelGetPath store iter
            expanded <- treeViewRowExpanded view path
            res      <-
                if expanded
                    then do
                        mbIter <- treeModelIterChildren store (Just iter)
                        case mbIter of
                            (False, _)   -> return (path : accu)
                            (True, iter') -> expandedFor iter' (path : accu)
                    else return accu
            treeModelIterNext store iter >>= \case
                False -> return res
                True  -> expandedFor iter res

applyExpanderState :: IDEAction
applyExpanderState = do
    liftIO $ debugM "leksah" "applyExpanderState"
    m       <- getModules Nothing
    (sc,bl) <- getScope
    liftIO $ do
        es <- readIORef (expanderState m)
        let (paths1,paths2) = case (sc,bl) of
                                (PackageScope False, True)      -> packageExp es
                                (PackageScope False, False)     -> packageExpNoBlack es
                                (PackageScope True,True)        -> packageDExp es
                                (PackageScope True,False)       -> packageDExpNoBlack es
                                (WorkspaceScope False, True)    -> workspaceExp es
                                (WorkspaceScope False, False)   -> workspaceExpNoBlack es
                                (WorkspaceScope True,True)      -> workspaceDExp es
                                (WorkspaceScope True,False)     -> workspaceDExpNoBlack es
                                (SystemScope,True)              -> systemExp es
                                (SystemScope,False)             -> systemExpNoBlack es
        mapM_ (\p -> treePathNewFromIndices' (map fromIntegral p) >>= treeViewExpandToPath (treeView m)) paths1
        mapM_ (\p -> treePathNewFromIndices' (map fromIntegral p) >>= treeViewExpandToPath (descrView m)) paths2

-- * GUI History

recordSelHistory :: IDEAction
recordSelHistory = do
    liftIO $ debugM "leksah" "selectIdentifier"
    mods <- getModules Nothing
    selTree <- getSelectionTree (treeView mods) (forestStore mods)
    selDescr <- getSelectionDescr (descrView mods) (descrStore mods) (descrSortedStore mods)
    let selMod = case selTree of
                        Just (_,Just (md,_)) -> Just (modu $ mdModuleId md)
                        _ -> Nothing
    let selFacet = case selDescr of
                        Nothing -> Nothing
                        Just descr -> Just (dscName descr)
    oldSel       <- liftIO $ readIORef (oldSelection mods)
    triggerEventIDE_ (GtkEvent $ RecordHistory (ModuleSelected selMod selFacet,
                                    ModuleSelected (moduleS' oldSel) (facetS' oldSel)))
    liftIO $ writeIORef (oldSelection mods) (oldSel{moduleS'= selMod, facetS' = selFacet})
    return ()

replaySelHistory :: Maybe ModuleName -> Maybe Text -> IDEAction
replaySelHistory mbModName mbFacetName = do
    liftIO $ debugM "leksah" "replaySelHistory"
    mods <- getModules Nothing
    selectNames (mbModName, mbFacetName)
    oldSel <- liftIO $ readIORef (oldSelection mods)
    liftIO $ writeIORef (oldSelection mods)
        (oldSel{moduleS'= mbModName, facetS' = mbFacetName})

recordScopeHistory :: IDEAction
recordScopeHistory = do
    liftIO $ debugM "leksah" "recordScopeHistory"
    (sc,bl)                 <-  getScope
    mods                    <-  getModules Nothing
    oldSel                  <-  liftIO $ readIORef (oldSelection mods)
    triggerEventIDE_ (GtkEvent $ RecordHistory (ScopeSelected sc bl,
                                    ScopeSelected (scope' oldSel) (blacklist' oldSel)))
    liftIO $ writeIORef (oldSelection mods) (oldSel{scope'= sc, blacklist' = bl})
    return ()

replayScopeHistory :: Scope -> Bool -> IDEAction
replayScopeHistory sc bl = do
    liftIO $ debugM "leksah" "selectIdentifier"
    mods <-  getModules Nothing
    liftIO $ do
        toggleButtonSetActive (blacklistB mods) bl
        toggleButtonSetActive (packageScopeB mods)   (sc == PackageScope False)
        toggleButtonSetActive (workspaceScopeB mods) (sc == PackageScope True)
        toggleButtonSetActive (systemScopeB mods)    (sc == SystemScope)
    setScope False (sc,bl)
    oldSel <- liftIO $ readIORef (oldSelection mods)
    liftIO $ writeIORef (oldSelection mods) (oldSel{scope'= sc, blacklist' = bl})
