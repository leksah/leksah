{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Gdk.Events
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Tree
import Data.List
import Distribution.Package
import Distribution.Version
import Data.Char (toLower)
import Prelude hiding (catch)
import Data.IORef
import IDE.Core.State
import IDE.Pane.Info
import IDE.Pane.SourceBuffer
import Distribution.ModuleName
import Distribution.Text (simpleParse,display)
import Data.Typeable (Typeable(..))
import Control.Exception (SomeException(..),catch)
import Control.Applicative ((<$>))
import IDE.Package (packageConfig,addModuleToPackageDescr,delModuleFromPackageDescr,getEmptyModuleTemplate,getPackageDescriptionAndPath, ModuleLocation(..))
import Distribution.PackageDescription
       (PackageDescription, BuildInfo, hsSourceDirs,
        hasLibs, executables, testSuites, exeName, testName, benchmarks,
        benchmarkName, libBuildInfo, library, buildInfo, testBuildInfo,
        benchmarkBuildInfo)
import System.FilePath (takeBaseName, (</>),dropFileName)
import System.Directory (doesFileExist,createDirectoryIfMissing, removeFile)
import Graphics.UI.Editor.MakeEditor (buildEditor,FieldDescription(..),mkField)
import Graphics.UI.Editor.Parameters
       (paraMinSize, paraMultiSel, Parameter(..), emptyParams, (<<<-),
        paraName)
import Graphics.UI.Editor.Simple
       (textEditor, boolEditor, staticListEditor)
import Graphics.UI.Editor.Composite (maybeEditor)
import IDE.Utils.GUIUtils (stockIdFromType, __)
import IDE.Metainfo.Provider
       (getSystemInfo, getWorkspaceInfo, getPackageInfo)
import System.Log.Logger (debugM)
import Default (Default(..))
import IDE.Workspaces (packageTry)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when, void)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ask)
import IDE.Utils.GUIUtils (treeViewContextMenu)
import System.Glib.Properties (newAttrFromMaybeStringProperty)
import Data.Text (Text)
import qualified Data.Text as T (unpack, isInfixOf, toLower, pack)
import Data.Monoid ((<>))
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)
import qualified Data.Text.IO as T (writeFile)

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

-- | A modules pane description
--

type ModuleRecord = (Text, Maybe (ModuleDescr,PackageDescr))

data IDEModules     =   IDEModules {
    outer           ::   VBox
,   paned           ::   HPaned
,   treeView        ::   TreeView
,   treeStore       ::   TreeStore ModuleRecord
,   descrView       ::   TreeView
,   descrStore      ::   TreeStore Descr
,   packageScopeB   ::   RadioButton
,   workspaceScopeB ::   RadioButton
,   systemScopeB    ::   RadioButton
,   dependsB        ::   CheckButton
,   blacklistB      ::   CheckButton
,   oldSelection    ::   IORef SelectionState
,   expanderState   ::   IORef ExpanderState
} deriving Typeable


data ModulesState           =   ModulesState Int (Scope,Bool)
                                    (Maybe ModuleName, Maybe Text) ExpanderState
    deriving(Eq,Ord,Read,Show,Typeable)

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
}   deriving (Eq,Ord,Show,Read)

type ExpanderFacet      = ([TreePath], [TreePath])

data SelectionState = SelectionState {
    moduleS'        ::   Maybe ModuleName
,   facetS'         ::   Maybe Text
,   scope'          ::   Scope
,   blacklist'      ::   Bool}
 deriving (Eq,Ord,Show)

instance Pane IDEModules IDEM
    where
    primPaneName _  =   (__ "Modules")
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . outer
    paneId b        =   "*Modules"
        --liftIO $ widgetGrabFocus (descrView p)

getModules :: Maybe PanePath -> IDEM IDEModules
getModules Nothing    = forceGetPane (Right "*Modules")
getModules (Just pp)  = forceGetPane (Left pp)

showModules :: IDEAction
showModules = do
    pane <- getModules Nothing
    displayPane pane False

instance RecoverablePane IDEModules ModulesState IDEM where
    saveState p     =   do
        m           <-  getModules Nothing
        sc          <-  getScope
        mbModules   <-  getPane
        recordExpanderState
        expander    <-  liftIO $ readIORef (expanderState p)
        case mbModules of
            Nothing ->  return Nothing
            Just p  ->  liftIO $ do
                i   <-  panedGetPosition (paned p)
                mbTreeSelection     <-  getSelectionTree (treeView m) (treeStore m)
                mbFacetSelection    <-  getSelectionDescr (descrView m) (descrStore m)
                let mbs = (case mbTreeSelection of
                            Just (_,Just (md,_)) -> Just (modu $ mdModuleId md)
                            otherwise            -> Nothing,
                            case mbFacetSelection of
                                Nothing -> Nothing
                                Just fw -> Just (dscName fw))
                return (Just (ModulesState i sc mbs expander))
    recoverState pp (ModulesState i sc@(scope,useBlacklist) se exp)  =  do
        nb          <-  getNotebook pp
        p           <-  buildPane pp nb builder
        mod         <-  getModules Nothing
        liftIO $ writeIORef (expanderState mod) exp
        liftIO $ writeIORef (oldSelection mod) (SelectionState (fst se) (snd se) scope useBlacklist)
        liftIO $ panedSetPosition (paned mod) i
        return p
    builder pp nb windows = do
        packageInfo' <- getPackageInfo
        reifyIDE $ \ ideR -> do
            let forest  = case packageInfo' of
                            Nothing     ->  []
                            Just (GenScopeC fst,GenScopeC snd)
                                ->  subForest (buildModulesTree (fst,snd))
            treeStore   <-  treeStoreNew forest
            treeView    <-  treeViewNew
            treeViewSetModel treeView treeStore
            --treeViewSetRulesHint treeView True

            renderer0    <- cellRendererPixbufNew
            set renderer0 [ newAttrFromMaybeStringProperty "stock-id"  := (Nothing :: Maybe Text) ]

            renderer    <- cellRendererTextNew
            col         <- treeViewColumnNew
            treeViewColumnSetTitle col (__ "Module")
            treeViewColumnSetSizing col TreeViewColumnAutosize
            treeViewColumnSetResizable col True
            treeViewColumnSetReorderable col True
            treeViewAppendColumn treeView col
            cellLayoutPackStart col renderer0 False
            cellLayoutPackStart col renderer True
            cellLayoutSetAttributes col renderer treeStore
                $ \row -> [ cellText := fst row]
            cellLayoutSetAttributes col renderer0 treeStore
                $ \row -> [
                newAttrFromMaybeStringProperty "stock-id" :=
                   case snd row of
                        Nothing -> Nothing
                        Just pair -> if isJust (mdMbSourcePath (fst pair))
                                         then Just ("ide_source" :: Text)
                                         else Nothing]

            renderer2   <- cellRendererTextNew
            col2        <- treeViewColumnNew
            treeViewColumnSetTitle col2 (__ "Package")
            treeViewColumnSetSizing col2 TreeViewColumnAutosize
            treeViewColumnSetResizable col2 True
            treeViewColumnSetReorderable col2 True
            treeViewAppendColumn treeView col2
            cellLayoutPackStart col2 renderer2 True
            cellLayoutSetAttributes col2 renderer2 treeStore
                $ \row -> [
                    cellText := case snd row of
                                    Nothing -> ""
                                    Just pair -> (T.pack . display . pdPackage . snd) pair]

            treeViewSetHeadersVisible treeView True
            treeViewSetEnableSearch treeView True
            treeViewSetSearchEqualFunc treeView (Just (treeViewSearch treeView treeStore))

        -- Facet view

            descrView   <-  treeViewNew
            descrStore  <-  treeStoreNew []
            treeViewSetModel descrView descrStore
            renderer30    <- cellRendererPixbufNew
            renderer31    <- cellRendererPixbufNew
            renderer3   <- cellRendererTextNew
            col         <- treeViewColumnNew
            treeViewColumnSetTitle col (__ "Interface")
            --treeViewColumnSetSizing col TreeViewColumnAutosize
            treeViewAppendColumn descrView col
            cellLayoutPackStart col renderer30 False
            cellLayoutPackStart col renderer31 False
            cellLayoutPackStart col renderer3 True
            cellLayoutSetAttributes col renderer3 descrStore
                $ \row -> [ cellText := descrTreeText row]
            cellLayoutSetAttributes col renderer30 descrStore
                $ \row -> [
                cellPixbufStockId  := stockIdFromType (descrIdType row)]
            cellLayoutSetAttributes col renderer31 descrStore
                $ \row -> [
                cellPixbufStockId  := if isReexported row
                                        then "ide_reexported"
                                            else if isJust (dscMbLocation row)
                                                then
                                                    if dscExported row
                                                        then ("ide_source" :: Text)
                                                        else "ide_source_local"
                                                else "ide_empty"]
            treeViewSetHeadersVisible descrView True
            treeViewSetEnableSearch descrView True
            treeViewSetSearchEqualFunc descrView (Just (descrViewSearch descrView descrStore))
            pane'           <-  hPanedNew
            sw              <-  scrolledWindowNew Nothing Nothing
            scrolledWindowSetShadowType sw ShadowIn
            containerAdd sw treeView
            scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
            sw2             <-  scrolledWindowNew Nothing Nothing
            scrolledWindowSetShadowType sw2 ShadowIn
            containerAdd sw2 descrView
            scrolledWindowSetPolicy sw2 PolicyAutomatic PolicyAutomatic
            panedAdd1 pane' sw
            panedAdd2 pane' sw2
            (Rectangle _ _ x y) <- liftIO $ widgetGetAllocation nb
            panedSetPosition pane' (max 200 (x `quot` 2))
            box             <-  hButtonBoxNew
            boxSetSpacing box 2
            buttonBoxSetLayout box ButtonboxSpread
            rb1             <-  radioButtonNewWithLabel (__ "Package")
            rb2             <-  radioButtonNewWithLabelFromWidget rb1 (__ "Workspace")
            rb3             <-  radioButtonNewWithLabelFromWidget rb1 (__ "System")
            toggleButtonSetActive rb3 True
            cb2             <-  checkButtonNewWithLabel (__ "Imports")
            cb              <-  checkButtonNewWithLabel (__ "Blacklist")

            boxPackStart box rb1 PackGrow 0
            boxPackStart box rb2 PackGrow 0
            boxPackStart box rb3 PackGrow 0
            boxPackEnd box cb PackNatural 0
            boxPackEnd box cb2 PackNatural 0


            boxOuter        <-  vBoxNew False 0
            boxPackStart boxOuter box PackNatural 2
            boxPackStart boxOuter pane' PackGrow 0
            oldState <- liftIO $ newIORef $ SelectionState Nothing Nothing SystemScope False
            expanderState <- liftIO $ newIORef emptyExpansion
            scopeRef <- newIORef (SystemScope,True)
            let modules = IDEModules boxOuter pane' treeView treeStore descrView descrStore
                                rb1 rb2 rb3 cb2 cb oldState expanderState
            cid1 <- after treeView focusInEvent $ do
                liftIO $ reflectIDE (makeActive modules) ideR
                return True
            cid2 <- after descrView focusInEvent $ do
                liftIO $ reflectIDE (makeActive modules) ideR
                return True
            (cid3, cid4) <- treeViewContextMenu treeView $ modulesContextMenu ideR treeStore treeView
            cid5 <- treeView `on` rowActivated $ modulesSelect ideR treeStore treeView
            (cid6, cid7) <- treeViewContextMenu descrView $ descrViewContextMenu ideR descrStore descrView
            cid8 <- descrView `on` rowActivated $ descrViewSelect ideR descrStore
            on rb1 toggled (reflectIDE scopeSelection ideR)
            on rb2 toggled (reflectIDE scopeSelection ideR)
            on rb3 toggled (reflectIDE scopeSelection ideR)
            on cb  toggled (reflectIDE scopeSelection ideR)
            on cb2 toggled (reflectIDE scopeSelection ideR)
            sel     <-  treeViewGetSelection treeView
            on sel treeSelectionSelectionChanged $ do
                fillFacets treeView treeStore descrView descrStore
                reflectIDE recordSelHistory ideR
                return ()
            sel2    <-  treeViewGetSelection descrView
            on sel2 treeSelectionSelectionChanged $ do
                fillInfo descrView descrStore ideR
                reflectIDE recordSelHistory ideR
                return ()
            return (Just modules,map ConnectC [cid1, cid2, cid3, cid4, cid5, cid6, cid7, cid8])

selectIdentifier :: Descr -> Bool -> IDEAction
selectIdentifier idDescr openSource= do
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
                            when (fst currentScope < sc) (setScope (sc,snd currentScope))
                            selectIdentifier' (modu pm) (dscName idDescr)
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
                    Just (GenScopeC (PackScope psc1 _), GenScopeC (PackScope psc2 _)) ->
                                    if Map.member pid psc1
                                        then (True,False)
                                        else if Map.member pid psc2
                                            then (True,True)
                                            else (False, False)
    ws  = case workspaceScope of
                        Nothing -> (False,False)
                        Just (GenScopeC (PackScope wsc1 _), GenScopeC (PackScope wsc2 _)) ->
                                        if Map.member pid wsc1
                                            then (True,False)
                                            else if Map.member pid wsc2
                                                then (True,True)
                                                else (False, False)


selectIdentifier' :: ModuleName -> Text -> IDEAction
selectIdentifier'  moduleName symbol =
    let nameArray = map T.pack $ components moduleName
    in do
        liftIO $ debugM "leksah" "selectIdentifier'"
        mods            <- getModules Nothing
        mbTree          <-  liftIO $ treeStoreGetTreeSave (treeStore mods) []
        case treePathFromNameArray mbTree nameArray [] of
            Just treePath   ->  liftIO $ do
                treeViewExpandToPath (treeView mods) treePath
                sel         <-  treeViewGetSelection (treeView mods)
                treeSelectionSelectPath sel treePath
                col         <-  treeViewGetColumn (treeView mods) 0
                treeViewScrollToCell (treeView mods) (Just treePath) col (Just (0.3,0.3))
                mbFacetTree <-  treeStoreGetTreeSave (descrStore mods) []
                selF        <-  treeViewGetSelection (descrView mods)
                case  findPathFor symbol mbFacetTree of
                    Nothing     ->  sysMessage Normal (__ "no path found")
                    Just path   ->  do
                        treeViewExpandToPath (descrView mods) path
                        treeSelectionSelectPath selF path
                        col     <-  treeViewGetColumn (descrView mods) 0
                        treeViewScrollToCell (descrView mods) (Just path) col (Just (0.3,0.3))
                bringPaneToFront mods
            Nothing         ->  return ()

findPathFor :: Text -> Maybe (Tree Descr) -> Maybe TreePath
findPathFor symbol (Just (Node _ forest)) =
    foldr ( \i mbTreePath -> findPathFor' [i] (forest !! i) mbTreePath)
                            Nothing  [0 .. ((length forest) - 1)]
    where
    findPathFor' :: TreePath -> Tree Descr -> Maybe TreePath -> Maybe TreePath
    findPathFor' _ node (Just p)                  =   Just p
    findPathFor' path (Node wrap sub) Nothing     =
        if dscName wrap == symbol
            then Just (reverse path)
            else
                foldr ( \i mbTreePath -> findPathFor' (i:path) (sub !! i) mbTreePath)
                            Nothing     [0 .. ((length sub) - 1)]
findPathFor symbol Nothing = Nothing

treePathFromNameArray :: Maybe ModTree -> [Text] -> [Int] -> Maybe [Int]
treePathFromNameArray (Just tree) [] accu      =   Just (reverse accu)
treePathFromNameArray (Just tree) (h:t) accu   =
    let names   =   map (\t -> fst $ rootLabel t) (subForest tree)
        mbIdx   =   elemIndex h names
    in case mbIdx of
            Nothing ->  Nothing
            Just i  ->  treePathFromNameArray (Just (subForest tree !! i)) t (i:accu)
treePathFromNameArray Nothing _ _  = Nothing

treeViewSearch :: TreeView
    -> TreeStore ModuleRecord
    -> Text
    -> TreeIter
    -> IO Bool
treeViewSearch treeView treeStore string iter =  do
    liftIO $ debugM "leksah" "treeViewSearch"
    path   <- treeModelGetPath treeStore iter
    val    <- treeStoreGetValue treeStore path
    mbTree <- treeStoreGetTreeSave treeStore path
    exp    <- treeViewRowExpanded treeView path
    when (isJust mbTree && (not (null (subForest (fromJust mbTree)))) && not exp) $
        let found = searchInModSubnodes (fromJust mbTree) string
        in when found $ do
            treeViewExpandRow treeView path False
            return ()
    let str2      =  case snd val of
                        Just (mod,_) ->  showPackModule  (mdModuleId mod)
                        Nothing -> ""
    let res       =  T.isInfixOf (T.toLower string) (T.toLower str2)
    return res

searchInModSubnodes :: ModTree -> Text -> Bool
searchInModSubnodes tree str =
    not $ null
        $ filter (\ (_,mbPair) ->
            case mbPair of
                Nothing -> False
                Just (mod,_) ->
                    let cstr = T.pack $ show (Present (mdModuleId mod))
                    in  T.isInfixOf (T.toLower str) (T.toLower cstr))
                            $ concatMap flatten (subForest tree)

descrViewSearch :: TreeView
    -> TreeStore Descr
    -> Text
    -> TreeIter
    -> IO Bool
descrViewSearch descrView descrStore string iter = do
    liftIO $ debugM "leksah" "descrViewSearch"
    path    <- treeModelGetPath descrStore iter
    val     <- treeStoreGetValue descrStore path
    tree <- treeStoreGetTree descrStore path
    exp  <- treeViewRowExpanded descrView path
    when (not (null (subForest tree)) && not exp) $
        let found = searchInFacetSubnodes tree string
        in when found $ do
            treeViewExpandRow descrView path False
            return ()
    return (T.isInfixOf (T.toLower string) (T.toLower (descrTreeText val)))

searchInFacetSubnodes :: DescrTree -> Text -> Bool
searchInFacetSubnodes tree str =
    not $ null
        $ filter (\ val ->
            T.isInfixOf (T.toLower str) (T.toLower (descrTreeText val)))
                $ concatMap flatten (subForest tree)

fillFacets :: TreeView
    -> TreeStore ModuleRecord
    -> TreeView
    -> TreeStore Descr
    -> IO ()
fillFacets treeView treeStore descrView descrStore = do
    liftIO $ debugM "leksah" "fillFacets"
    sel             <-  getSelectionTree treeView treeStore
    case sel of
        Just (_,Just (mod,package))
            ->  let forest = buildFacetForrest mod in do
                        emptyModel <- treeStoreNew []
                        treeViewSetModel descrView emptyModel
                        treeStoreClear descrStore
                        mapM_ (\(e,i) -> treeStoreInsertTree descrStore [] i e)
                                    $ zip forest [0 .. length forest]
                        treeViewSetModel descrView descrStore
                        treeViewSetEnableSearch descrView True
                        treeViewSetSearchEqualFunc descrView (Just (descrViewSearch descrView descrStore))
        otheriwse
            ->  treeStoreClear descrStore

getSelectionTree ::  TreeView
    ->  TreeStore ModuleRecord
    -> IO (Maybe ModuleRecord)
getSelectionTree treeView treeStore = do
    liftIO $ debugM "leksah" "getSelectionTree"
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        []  ->  return Nothing
        a:r ->  do
            val     <-  treeStoreGetValue treeStore a
            return (Just val)

getSelectionDescr ::  TreeView
    ->  TreeStore Descr
    -> IO (Maybe Descr)
getSelectionDescr treeView treeStore = do
    liftIO $ debugM "leksah" "getSelectionDescr"
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        a:r ->  do
            val     <-  treeStoreGetValue treeStore a
            return (Just val)
        _  ->  return Nothing


fillInfo :: TreeView
    -> TreeStore Descr
    -> IDERef
    -> IO ()
fillInfo treeView lst ideR  = do
    liftIO $ debugM "leksah" "fillInfo"
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        []      ->  return ()
        [a]     ->  do
            descr    <-  treeStoreGetValue lst a
            reflectIDE (setInfo descr) ideR
            return ()
        _       ->  return ()

findDescription :: SymbolTable alpha => PackModule -> alpha -> Text -> Maybe (Text,Descr)
findDescription md st s     =
    case filter (\id -> case dsMbModu id of
                            Nothing -> False
                            Just pm -> md == pm) (symLookup s st) of
        [] -> Nothing
        l  -> Just (s,head l)

getEmptyDefaultScope :: Map Text [Descr]
getEmptyDefaultScope = Map.empty

fillModulesList :: (Scope,Bool) -> IDEAction
fillModulesList (scope,useBlacklist) = do
    liftIO $ debugM "leksah" "fillModulesList"
    mods  <-  getModules Nothing
    prefs                       <-  readIDE prefs
    case scope of
        SystemScope -> do
            accessibleInfo'             <-  getSystemInfo
            case accessibleInfo' of
                Nothing ->  liftIO $ do
                                        treeStoreClear (treeStore mods)
                                        --treeStoreInsertTree (treeStore mods) [] 0 (Node ("",[]) [])
                Just (GenScopeC ai@(PackScope pm ps)) ->
                    let p2  =   if useBlacklist
                                    then PackScope (Map.filter (filterBlacklist
                                            (packageBlacklist prefs)) pm) ps
                                    else ai
                        (Node _ li) = buildModulesTree (PackScope Map.empty getEmptyDefaultScope,p2)
                    in insertIt li mods
        WorkspaceScope withImports -> do
            workspaceInfo'           <-  getWorkspaceInfo
            packageInfo'             <-  getPackageInfo
            case workspaceInfo' of
                Nothing ->  insertIt [] mods
                Just (GenScopeC l,GenScopeC p) ->
                    let (l',p'@(PackScope pm ps)) =   if withImports
                                                        then (l, p)
                                                        else (l, PackScope Map.empty symEmpty)
                        p2              =   if useBlacklist
                                                then PackScope (Map.filter (filterBlacklist
                                                        (packageBlacklist prefs)) pm) ps
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
                                                        (packageBlacklist prefs)) pm) ps
                                                else p'
                        (Node _ li)     =   buildModulesTree (l', p2)
                    in insertIt li mods
    where
        insertIt li mods = liftIO $ do
            emptyModel <- treeStoreNew []
            treeViewSetModel (treeView mods) emptyModel
            treeStoreClear (treeStore mods)
            mapM_ (\(e,i) -> treeStoreInsertTree (treeStore mods) [] i e)
                    $ zip li [0 .. length li]
            treeViewSetModel (treeView mods) (treeStore mods)
            treeViewSetEnableSearch (treeView mods) True
            treeViewSetSearchEqualFunc (treeView mods)
                (Just (treeViewSearch (treeView mods) (treeStore mods)))


filterBlacklist :: [Dependency] -> PackageDescr -> Bool
filterBlacklist dependencies packageDescr =
    let packageId   =   pdPackage packageDescr
        name        =   pkgName packageId
        version     =   pkgVersion packageId
    in  isNothing $ find (\ (Dependency str vr) -> str == name && withinRange version vr)
                    dependencies


type DescrForest = Forest Descr
type DescrTree = Tree Descr


descrTreeText :: Descr -> Text
descrTreeText (Real (RealDescr id _ _ _ _ (InstanceDescr binds) _)) = id <> " " <> printBinds binds
    where
        printBinds []       =   ""
        printBinds (a:[])   =   a
        printBinds (a:b)    =   a <> " " <> printBinds b
descrTreeText d = dscName d

descrIdType :: Descr -> DescrType
descrIdType = descrType . dscTypeHint

buildFacetForrest ::  ModuleDescr -> DescrForest
buildFacetForrest modDescr =
    let (instances,other)       =   partition (\id -> case dscTypeHint id of
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
                    Node descr ((map (\(SimpleDescr fn ty loc comm exp) ->
                        Node (makeReexported descr (Real $ RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                            dscMbModu' = dsMbModu descr, dscMbLocation' = loc,
                            dscMbComment' = comm, dscTypeHint' = ConstructorDescr descr, dscExported' = exp})) [])
                                constructors)
                                    ++  (map (\(SimpleDescr fn ty loc comm exp) ->
                                        Node (makeReexported descr (Real $
                                        RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                                        dscMbModu' = dsMbModu descr, dscMbLocation' = loc,
                                        dscMbComment' = comm, dscTypeHint' = FieldDescr descr, dscExported' = exp})) [])
                                    fields))
                NewtypeDescr (SimpleDescr fn ty loc comm exp) mbField ->
                    Node descr (Node (makeReexported descr (Real $
                            RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                                dscMbModu' = dsMbModu descr, dscMbLocation' = loc,
                                dscMbComment' = comm, dscTypeHint' = ConstructorDescr descr, dscExported' = exp})) []
                                : case  mbField of
                                    Just (SimpleDescr fn ty loc comm exp) ->
                                        [Node (makeReexported descr (Real $ RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                                        dscMbModu' = dsMbModu descr, dscMbLocation' = loc,
                                        dscMbComment' = comm, dscTypeHint' = FieldDescr descr, dscExported' = exp})) []]
                                    Nothing -> [])
                ClassDescr _ methods ->
                    Node descr (map (\(SimpleDescr fn ty loc comm exp) ->
                        Node (makeReexported descr (Real $ RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                            dscMbModu' = dsMbModu descr, dscMbLocation' = loc,
                            dscMbComment' = comm, dscTypeHint' = MethodDescr descr, dscExported' = exp})) [])
                                methods)
                _ -> Node descr []
        where
            makeReexported :: Descr -> Descr -> Descr
            makeReexported (Reexported d1) d2 = Reexported $ ReexportedDescr{dsrMbModu = dsrMbModu d1, dsrDescr = d2}
            makeReexported _ d2               = d2

    addInstances :: (DescrForest,[Descr])
        -> Descr
        -> (DescrForest,[Descr])
    addInstances (forest,orphaned) instDescr =
        case foldr (matches instDescr) ([],False) forest of
            (f,True)    -> (f,orphaned)
            (f,False)   -> (forest, instDescr:orphaned)

    matches :: Descr
        ->  DescrTree
        -> (DescrForest,Bool)
        -> (DescrForest,Bool)
    matches (Real (instDescr@RealDescr{dscTypeHint' = InstanceDescr binds}))
                (Node dd@(Real (RealDescr id _ _ _ _ (DataDescr _ _) _)) sub) (forest,False)
        | not (null binds) && id == head binds
            =   ((Node dd (sub ++ [Node newInstDescr []])):forest,True)
        where newInstDescr = if isNothing (dscMbLocation' instDescr)
                                then Real $ instDescr{dscMbLocation' = dscMbLocation dd}
                                else Real $ instDescr
    matches (Real (instDescr@RealDescr{dscTypeHint' = InstanceDescr binds}))
                (Node dd@(Real (RealDescr id _ _ _ _ (NewtypeDescr _ _) _)) sub) (forest,False)
        | not (null binds) && id == head binds
            =   ((Node dd (sub ++ [Node newInstDescr []])):forest,True)
        where newInstDescr = if isNothing (dscMbLocation' instDescr)
                                then Real $ instDescr{dscMbLocation' = dscMbLocation dd}
                                else Real $ instDescr
    matches _ node (forest,b)  = (node:forest,b)


defaultRoot :: Tree ModuleRecord
defaultRoot = Node ("",Just (getDefault,getDefault)) []

type ModTree = Tree ModuleRecord
--
-- | Make a Tree with a module desription, package description pairs tree to display.
--   Their are nodes with a label but without a module (like e.g. Data).
--
buildModulesTree :: (SymbolTable alpha, SymbolTable beta) =>  (PackScope alpha,PackScope beta ) -> ModTree
buildModulesTree (PackScope localMap _,PackScope otherMap _) =
    let modDescrPackDescr =   concatMap (\p -> map (\m -> (m,p)) (pdModules p))
                                    (Map.elems localMap ++ Map.elems otherMap)
        resultTree        =   foldl' insertPairsInTree defaultRoot modDescrPackDescr
        in sortTree resultTree

insertPairsInTree :: ModTree -> (ModuleDescr,PackageDescr) -> ModTree
insertPairsInTree tree pair =
    let nameArray           =   map T.pack . components . modu . mdModuleId $ fst pair
        (startArray,last)   =   splitAt (length nameArray - 1) nameArray
        pairedWith          =   (map (\n -> (n,Nothing)) startArray) ++ [(head last,Just pair)]
    in  insertNodesInTree pairedWith tree


insertNodesInTree :: [ModuleRecord] -> ModTree -> ModTree
insertNodesInTree  [p1@(str1,Just pair)] (Node p2@(str2,mbPair) forest2) =
    case partition (\ (Node (s,_) _) -> s == str1) forest2 of
        ([found],rest) -> case found of
                            Node p3@(_,Nothing) forest3 ->
                                Node p2 (Node p1 forest3 : rest)
                            Node p3@(_,Just pair3) forest3 ->
                                Node p2 (Node p1 [] : Node p3 forest3 : rest)
        ([],rest)      -> Node p2 (Node p1 [] : forest2)
        (found,rest)   -> case head found of
                            Node p3@(_,Nothing) forest3 ->
                                Node p2 (Node p1 forest3 : tail found ++ rest)
                            Node p3@(_,Just pair3) forest3 ->
                                Node p2 (Node p1 [] : Node p3 forest3 : tail found ++ rest)

insertNodesInTree li@(hd@(str1,Nothing):tl) (Node p@(str2,mbPair) forest) =
    case partition (\ (Node (s,_) _) -> s == str1) forest of
        ([found],rest) -> Node p  (insertNodesInTree tl found : rest)
        ([],rest)      -> Node p  (makeNodes li : forest)
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
        otherwise       ->  Nothing

modulesContextMenu :: IDERef
                   -> TreeStore ModuleRecord
                   -> TreeView
                   -> Menu
                   -> IO ()
modulesContextMenu ideR store treeView theMenu = do
    liftIO $ debugM "leksah" "modulesContextMenu"
    item1           <-  menuItemNewWithLabel (__ "Edit source")
    item1 `on` menuItemActivate $ do
        mbFile <- getSelectedModuleFile <$> getSelectionTree treeView store
        case mbFile of
            Nothing -> return ()
            Just fp -> void $ reflectIDE (selectSourceBuf fp) ideR
    sep1 <- separatorMenuItemNew
    item2           <-  menuItemNewWithLabel (__ "Expand here")
    item2 `on` menuItemActivate $ expandHere treeView
    item3           <-  menuItemNewWithLabel (__ "Collapse here")
    item3 `on` menuItemActivate $ collapseHere treeView
    item4           <-  menuItemNewWithLabel (__ "Expand all")
    item4 `on` menuItemActivate $ treeViewExpandAll treeView
    item5           <-  menuItemNewWithLabel (__ "Collapse all")
    item5 `on` menuItemActivate $ treeViewCollapseAll treeView
    sep2 <- separatorMenuItemNew
    item6           <-  menuItemNewWithLabel (__ "Add module")
    item6 `on` menuItemActivate $ reflectIDE (packageTry $ addModule' treeView store) ideR
    item7           <-  menuItemNewWithLabel (__ "Delete module")
    item7 `on` menuItemActivate $ do
        mbFile <- getSelectedModuleFile <$> getSelectionTree treeView store
        case mbFile of
            Nothing     ->  return ()
            Just fp     ->  do
                resp <- reflectIDE (respDelModDialog)ideR
                if (resp == False) then return ()
                    else do
                        exists <- doesFileExist fp
                        if exists
                           then do
                             reflectIDE (liftIO $ removeFile fp) ideR
                             reflectIDE (packageTry $ delModule treeView store)ideR
                           else do
                             reflectIDE (packageTry $ delModule treeView store)ideR
                        reflectIDE (packageTry packageConfig) ideR
                        return ()
    sel         <-  getSelectionTree treeView store
    case sel of
        Just (s, Nothing) -> do
            mapM_ (menuShellAppend theMenu) [castToMenuItem item1, castToMenuItem sep1, castToMenuItem item2,
                castToMenuItem item3, castToMenuItem item4, castToMenuItem item5, castToMenuItem sep2,
                castToMenuItem item6]
        Just (_,Just (m,_)) -> do
            mapM_ (menuShellAppend theMenu) [castToMenuItem item1, castToMenuItem sep1, castToMenuItem item2,
                castToMenuItem item3, castToMenuItem item4, castToMenuItem item5, castToMenuItem sep2,
                castToMenuItem item6, castToMenuItem item7]
        otherwise -> return ()

modulesSelect :: IDERef
              -> TreeStore ModuleRecord
              -> TreeView
              -> TreePath
              -> TreeViewColumn
              -> IO ()
modulesSelect ideR store treeView path _ = do
    liftIO $ debugM "leksah" "modulesSelect"
    treeViewExpandRow treeView path False
    mbFile <- getSelectedModuleFile <$> getSelectionTree treeView store
    case mbFile of
        Nothing -> return ()
        Just fp -> liftIO $ reflectIDE (selectSourceBuf fp) ideR >> return ()

descrViewContextMenu :: IDERef
                   -> TreeStore Descr
                   -> TreeView
                   -> Menu
                   -> IO ()
descrViewContextMenu ideR store descrView theMenu = do
    liftIO $ debugM "leksah" "descrViewContextMenu"
    item1           <-  menuItemNewWithLabel (__ "Go to definition")
    item1 `on` menuItemActivate $ do
        sel         <-  getSelectionDescr descrView store
        case sel of
            Just descr      ->  reflectIDE (goToDefinition descr) ideR
            otherwise       ->  sysMessage Normal (__ "Modules>> descrViewPopup: no selection")
    item2           <-  menuItemNewWithLabel (__ "Insert in buffer")
    item2 `on` menuItemActivate $ do
        sel         <-  getSelectionDescr descrView store
        case sel of
            Just descr      ->  reflectIDE (insertInBuffer descr) ideR
            otherwise       ->  sysMessage Normal (__ "Modules>> descrViewPopup: no selection")
    mapM_ (menuShellAppend theMenu) [item1, item2]

descrViewSelect :: IDERef
              -> TreeStore Descr
              -> TreePath
              -> TreeViewColumn
              -> IO ()
descrViewSelect ideR store path _ = do
    liftIO $ debugM "leksah" "descrViewSelect"
    descr <- treeStoreGetValue store path
    reflectIDE (goToDefinition descr) ideR

setScope :: (Scope,Bool) -> IDEAction
setScope (sc,bl) = do
    liftIO $ debugM "leksah" "setScope"
    mods  <-  getModules Nothing
    case sc of
        (PackageScope False) -> liftIO $ do
            toggleButtonSetActive (packageScopeB mods) True
            widgetSetSensitive (dependsB mods) True
            toggleButtonSetActive (dependsB mods) False
        (PackageScope True) -> liftIO $ do
            toggleButtonSetActive (packageScopeB mods) True
            widgetSetSensitive (dependsB mods) True
            toggleButtonSetActive (dependsB mods) True
        (WorkspaceScope False) -> liftIO $ do
            toggleButtonSetActive (workspaceScopeB mods) True
            widgetSetSensitive (dependsB mods) True
            toggleButtonSetActive (dependsB mods) False
        (WorkspaceScope True) -> liftIO $ do
            toggleButtonSetActive (workspaceScopeB mods) True
            widgetSetSensitive (dependsB mods) True
            toggleButtonSetActive (dependsB mods) True
        SystemScope -> liftIO $ do
            toggleButtonSetActive (systemScopeB mods) True
            widgetSetSensitive (dependsB mods) False
    liftIO $ toggleButtonSetActive (blacklistB mods) bl
    selectScope (sc,bl)

getScope :: IDEM (Scope,Bool)
getScope = do
    liftIO $ debugM "leksah" "getScope"
    mods  <-  getModules Nothing
    rb1s                <-  liftIO $ toggleButtonGetActive (packageScopeB mods)
    rb2s                <-  liftIO $ toggleButtonGetActive (workspaceScopeB mods)
    rb3s                <-  liftIO $ toggleButtonGetActive (systemScopeB mods)
    cb1s                <-  liftIO $ toggleButtonGetActive (dependsB mods)
    cbs                 <-  liftIO $ toggleButtonGetActive (blacklistB mods)
    let scope           =   if rb1s
                                then PackageScope cb1s
                                else if rb2s
                                    then WorkspaceScope cb1s
                                    else SystemScope
    return (scope,cbs)

scopeSelection :: IDEAction
scopeSelection = do
    liftIO $ debugM "leksah" "scopeSelection"
    (sc,bl) <- getScope
    setScope (sc,bl)
    selectScope (sc,bl)

selectScope :: (Scope,Bool) -> IDEAction
selectScope (sc,bl) = do
    liftIO $ debugM "leksah" "selectScope"
    recordExpanderState
    mods                <-  getModules Nothing
    mbTreeSelection     <-  liftIO $ getSelectionTree (treeView mods) (treeStore mods)
    mbDescrSelection    <-  liftIO $ getSelectionDescr (descrView mods) (descrStore mods)

    ts                  <-  liftIO $ treeViewGetSelection (treeView mods)
    withoutRecordingDo $ do
        liftIO $ treeSelectionUnselectAll ts
        fillModulesList (sc,bl)
        let mbs = (case mbTreeSelection of
                    Just (_,Just (md,_)) -> Just (modu $ mdModuleId md)
                    otherwise -> Nothing,
                         case mbDescrSelection of
                            Nothing -> Nothing
                            Just fw -> Just (dscName fw))

        selectNames mbs
    recordScopeHistory
    applyExpanderState
    liftIO $ bringPaneToFront mods

selectNames :: (Maybe ModuleName, Maybe Text) -> IDEAction
selectNames (mbModuleName, mbIdName) = do
    liftIO $ debugM "leksah" "selectIdentifier"
    mods <- getModules Nothing
    case mbModuleName of
        Nothing -> liftIO $ do
            sel         <-  treeViewGetSelection (treeView mods)
            treeSelectionUnselectAll  sel
            selF        <-  treeViewGetSelection (descrView mods)
            treeSelectionUnselectAll  selF
        Just moduleName ->
            let nameArray = map T.pack $ components moduleName
            in do
                mbTree              <-  liftIO $ treeStoreGetTreeSave (treeStore mods) []
                case treePathFromNameArray mbTree nameArray [] of
                    Nothing         ->  return ()
                    Just treePath   ->  liftIO $ do
                        treeViewExpandToPath (treeView mods) treePath
                        sel         <-  treeViewGetSelection (treeView mods)
                        treeSelectionSelectPath sel treePath
                        col         <-  treeViewGetColumn (treeView mods) 0
                        treeViewScrollToCell (treeView mods) (Just treePath) col
                            (Just (0.3,0.3))
                        case mbIdName of
                            Nothing -> do
                                selF          <-  treeViewGetSelection (descrView mods)
                                treeSelectionUnselectAll  selF
                            Just symbol -> do
                                mbDescrTree   <-  treeStoreGetTreeSave (descrStore mods) []
                                selF          <-  treeViewGetSelection (descrView mods)
                                case  findPathFor symbol mbDescrTree of
                                    Nothing     ->  sysMessage Normal (__ "no path found")
                                    Just path   ->  do
                                        treeSelectionSelectPath selF path
                                        col     <-  treeViewGetColumn (descrView mods) 0
                                        treeViewScrollToCell (descrView mods) (Just path) col
                                            (Just (0.3,0.3))

reloadKeepSelection :: Bool -> IDEAction
reloadKeepSelection isInitial = do
    liftIO . debugM "leksah" $ (T.unpack $ __ ">>>Info Changed!!! ") ++ show isInitial
    mbMod <- getPane
    case mbMod of
        Nothing -> return ()
        Just mods -> do
            state <- readIDE currentState
            if not $ isStartingOrClosing state
                then do
                    mbTreeSelection     <-  liftIO $ getSelectionTree (treeView mods) (treeStore mods)
                    mbDescrSelection    <-  liftIO $ getSelectionDescr (descrView mods) (descrStore mods)
                    sc                  <-  getScope
                    recordExpanderState
                    fillModulesList sc
                    liftIO $ treeStoreClear (descrStore mods)
                    let mbs =  (case mbTreeSelection of
                                    Just (_,Just (md,_)) -> Just (modu $ mdModuleId md)
                                    otherwise -> Nothing,
                                 case mbDescrSelection of
                                    Nothing -> Nothing
                                    Just fw -> Just (dscName fw))

                    applyExpanderState
                    selectNames mbs
                else if isInitial == True
                        then do
                            SelectionState moduleS' facetS' sc bl <- liftIO $ readIORef (oldSelection mods)
                            setScope (sc,bl)
                            fillModulesList (sc, bl)
                            selectNames (moduleS', facetS')
                            applyExpanderState
                        else return ()

treeStoreGetTreeSave :: TreeStore a -> TreePath -> IO (Maybe (Tree a))
treeStoreGetTreeSave treeStore treePath = catch (do
    liftIO $ debugM "leksah" "treeStoreGetTreeSave"
    res <- treeStoreGetTree treeStore treePath
    return (Just res)) (\ (_ :: SomeException) -> return Nothing)

expandHere :: TreeView -> IO ()
expandHere treeView = do
    liftIO $ debugM "leksah" "expandHere"
    sel   <- treeViewGetSelection treeView
    paths <- treeSelectionGetSelectedRows sel
    case paths of
        []     -> return ()
        (hd:_) -> treeViewExpandRow treeView hd True >> return ()

collapseHere :: TreeView -> IO ()
collapseHere treeView = do
    liftIO $ debugM "leksah" "collapseHere"
    sel   <- treeViewGetSelection treeView
    paths <- treeSelectionGetSelectedRows sel
    case paths of
        []     -> return ()
        (hd:_) -> treeViewCollapseRow treeView hd >> return ()

delModule :: TreeView -> TreeStore ModuleRecord -> PackageAction
delModule treeview store = do
    liftIO $ debugM "leksah" "delModule"
    window <- liftIDE $ getMainWindow
    sel   <- liftIO $ treeViewGetSelection treeview
    paths <- liftIO $ treeSelectionGetSelectedRows sel
    categories <- case paths of
        []     -> return []
        (treePath:_) -> liftIO $ mapM (treeStoreGetValue store)
                                    $ map (\n -> take n treePath)  [1 .. length treePath]

    liftIDE $ ideMessage Normal (T.pack $ printf (__ "categories: %s") (show categories))

    let modPacDescr = snd(last categories)
    case modPacDescr of
        Nothing     ->   liftIDE $ ideMessage Normal (__ "This should never be shown!")
        Just(md,_)  -> do
                         let modName = modu.mdModuleId $ md
                         liftIDE $ ideMessage Normal ("modName: " <> T.pack (show modName))
                         delModuleFromPackageDescr modName

respDelModDialog :: IDEM (Bool)
respDelModDialog = do
    liftIO $ debugM "leksah" "respDelModDialog"
    window <- getMainWindow
    resp <- liftIO $ do
        dia <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel (__ "Are you sure?")
        dialogAddButton dia (__ "_Delete Module") (ResponseUser 1)
        dialogSetDefaultResponse dia ResponseCancel
        set dia [ windowWindowPosition := WinPosCenterOnParent ]
        resp <- dialogRun dia
        widgetDestroy dia
        return resp
    return $ resp == ResponseUser 1

addModule' :: TreeView -> TreeStore ModuleRecord -> PackageAction
addModule' treeView store = do
    liftIO $ debugM "leksah" "addModule'"
    sel   <- liftIO $ treeViewGetSelection treeView
    paths <- liftIO $ treeSelectionGetSelectedRows sel
    categories <- case paths of
        []     -> return []
        (treePath:_) -> liftIO $ mapM (treeStoreGetValue store)
                                    $ map (\n -> take n treePath)  [1 .. length treePath]
    addModule categories

-- Includes non buildable
allBuildInfo' :: PackageDescription -> [BuildInfo]
allBuildInfo' pkg_descr = [ libBuildInfo lib       | Just lib <- [library pkg_descr] ]
                       ++ [ buildInfo exe          | exe <- executables pkg_descr ]
                       ++ [ testBuildInfo tst      | tst <- testSuites pkg_descr ]
                       ++ [ benchmarkBuildInfo tst | tst <- benchmarks pkg_descr ]

addModule :: [ModuleRecord] -> PackageAction
addModule categories = do
    liftIO $ debugM "leksah" "selectIdentifier"
    mbPD <- liftIDE $ getPackageDescriptionAndPath
    case mbPD of
        Nothing             -> liftIDE $ ideMessage Normal (__ "No package description")
        Just (pd,cabalPath) -> let srcPaths = nub $ concatMap hsSourceDirs $ allBuildInfo' pd
                                   rootPath = dropFileName cabalPath
                                   modPath  = foldr (\a b -> a <> "." <> b) ""
                                                (map fst categories)
                               in do
            window' <- liftIDE getMainWindow
            mbResp <- liftIO $ addModuleDialog window' modPath srcPaths (hasLibs pd) $
                      map (T.pack . exeName)  (executables pd)
                   ++ map (T.pack . testName) (testSuites pd)
                   ++ map (T.pack . benchmarkName) (benchmarks pd)
            case mbResp of
                Nothing                -> return ()
                Just addMod@(AddModule modPath srcPath libExposed exesAndTests) ->
                    case simpleParse $ T.unpack modPath of
                        Nothing         -> liftIDE $ ideMessage Normal (T.pack $ printf (__ "Not a valid module name : %s") (T.unpack modPath))
                        Just moduleName -> do
                            let  target = srcPath </> toFilePath moduleName ++ ".hs"
                            liftIO $ createDirectoryIfMissing True (dropFileName target)
                            alreadyExists <- liftIO $ doesFileExist target
                            if alreadyExists
                                then do
                                    liftIDE $ ideMessage Normal (T.pack $ printf (__ "File already exists! Importing existing file %s.hs") (takeBaseName target))
                                    addModuleToPackageDescr moduleName $ addModuleLocations addMod
                                    packageConfig
                                else do
                                    template <- liftIO $ getEmptyModuleTemplate pd modPath
                                    liftIO $ T.writeFile target template
                                    addModuleToPackageDescr moduleName $ addModuleLocations addMod
                                    packageConfig
                                    liftIDE $ fileOpenThis target


--  Yet another stupid little dialog

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

addModuleDialog :: Window -> Text -> [FilePath] -> Bool -> [Text] -> IO (Maybe AddModule)
addModuleDialog parent modString sourceRoots hasLib exesTests = do
    liftIO $ debugM "leksah" "addModuleDialog"
    dia                        <-   dialogNew
    set dia [ windowTransientFor := parent
            , windowTitle := (__ "Construct new module") ]
#ifdef MIN_VERSION_gtk3
    upper                      <-   dialogGetContentArea dia
#else
    upper                      <-   dialogGetUpper dia
#endif
    lower                      <-   dialogGetActionArea dia
    (widget,inj,ext,_)         <-   buildEditor (moduleFields sourceRoots hasLib exesTests)
                                        (AddModule modString (head sourceRoots) (Just False) Set.empty)
    bb      <-  hButtonBoxNew
    boxSetSpacing bb 6
    buttonBoxSetLayout bb ButtonboxSpread
    closeB  <-  buttonNewFromStock "gtk-cancel"
    save    <-  buttonNewFromStock "gtk-ok"
    boxPackEnd bb closeB PackNatural 0
    boxPackEnd bb save PackNatural 0
    on save buttonActivated (dialogResponse dia ResponseOk)
    on closeB buttonActivated (dialogResponse dia ResponseCancel)
    boxPackStart (castToBox upper) widget PackGrow 0
    boxPackStart (castToBox lower) bb PackNatural 5
    set save [widgetCanDefault := True]
    widgetGrabDefault save
    widgetShowAll dia
    resp  <- dialogRun dia
    value <- ext (AddModule modString (head sourceRoots) (Just False) Set.empty)
    widgetDestroy dia
    --find
    case resp of
        ResponseOk    -> return value
        _             -> return Nothing

moduleFields :: [FilePath] -> Bool -> [Text] -> FieldDescription AddModule
moduleFields list hasLibs exesTests = VFD emptyParams $ [
        mkField
            (paraName <<<- ParaName ((__ "New module "))
                    $ emptyParams)
            moduleName
            (\ a b -> b{moduleName = a})
            (textEditor (const True) True),
        mkField
            (paraName <<<- ParaName ((__ "Root of the source path"))
                $ paraMultiSel <<<- ParaMultiSel False
                    $ paraMinSize <<<- ParaMinSize (-1, 120)
                        $ emptyParams)
            (\a -> T.pack $ sourceRoot a)
            (\ a b -> b{sourceRoot = T.unpack a})
            (staticListEditor (map T.pack list) id)]
        ++ (if hasLibs
                then [
                    mkField
                        (paraName <<<- ParaName ((__ "Is this an exposed library module"))
                                $ emptyParams)
                        libExposed
                        (\ a b -> b{libExposed = a})
                        (maybeEditor (boolEditor, emptyParams) True (__ "Expose module"))]
                else [])
        ++ map (\ name ->
            mkField
                (paraName <<<- ParaName ((__ "Include in ") <> name)
                        $ emptyParams)
                (Set.member name . exesAndTests)
                (\ a b -> b{exesAndTests = (if a then Set.insert else Set.delete) name (exesAndTests b)})
                boolEditor) exesTests

-- * Expander State

emptyExpansion      = ExpanderState  ([],[])  ([],[])  ([],[])  ([],[])  ([],[]) ([],[])  ([],[])  ([],[])  ([],[]) ([],[])

recordExpanderState :: IDEAction
recordExpanderState = do
    liftIO $ debugM "leksah" "recordExpanderState"
    m       <- getModules Nothing
    liftIO $ do
        oldSel  <- readIORef (oldSelection m)
        let (sc,bl) =  (scope' oldSel, blacklist' oldSel)
        paths1 <-  getExpandedRows (treeView m) (treeStore m)
        paths2 <-  getExpandedRows (descrView m) (descrStore m)
        modifyIORef (expanderState m) (\ es ->
            case (sc,bl) of
                ((PackageScope False),True)    -> es{packageExp          = (paths1,paths2)}
                ((PackageScope False),False)   -> es{packageExpNoBlack   = (paths1,paths2)}
                (PackageScope True,True)  -> es{packageDExp        = (paths1,paths2)}
                (PackageScope True,False) -> es{packageDExpNoBlack = (paths1,paths2)}

                ((WorkspaceScope False),True)    -> es{workspaceExp          = (paths1,paths2)}
                ((WorkspaceScope False),False)   -> es{workspaceExpNoBlack   = (paths1,paths2)}
                (WorkspaceScope True,True)  -> es{workspaceDExp        = (paths1,paths2)}
                (WorkspaceScope True,False) -> es{workspaceDExpNoBlack = (paths1,paths2)}

                (SystemScope,True)   -> es{systemExp         = (paths1,paths2)}
                (SystemScope,False)  -> es{systemExpNoBlack  = (paths1,paths2)})
        st <- readIORef (expanderState m)
        return ()


getExpandedRows :: TreeView -> TreeStore alpha -> IO [TreePath]
getExpandedRows view store = do
    liftIO $ debugM "leksah" "getExpandedRows"
    mbIter <- treeModelGetIterFirst store
    case mbIter of
        Nothing   -> return []
        Just iter -> expandedFor iter []
    where
        expandedFor :: TreeIter -> [TreePath] -> IO [TreePath]
        expandedFor iter accu = do
            path <- treeModelGetPath store iter
            expanded <- treeViewRowExpanded view path
            res      <-
                if expanded
                    then do
                        mbIter <- treeModelIterChildren store iter
                        case mbIter of
                            Nothing   -> return (path : accu)
                            Just iter -> expandedFor iter (path : accu)
                    else return accu
            next <- treeModelIterNext store iter
            case next of
                Nothing   -> return res
                Just iter -> expandedFor iter res

applyExpanderState :: IDEAction
applyExpanderState = do
    liftIO $ debugM "leksah" "applyExpanderState"
    m       <- getModules Nothing
    (sc,bl) <- getScope
    liftIO $ do
        es <- readIORef (expanderState m)
        let (paths1,paths2) = case (sc,bl) of
                                ((PackageScope False),True)      -> packageExp es
                                ((PackageScope False),False)     -> packageExpNoBlack es
                                (PackageScope True,True)         -> packageDExp es
                                (PackageScope True,False)        -> packageDExpNoBlack es
                                ((WorkspaceScope False),True)    -> workspaceExp es
                                ((WorkspaceScope False),False)   -> workspaceExpNoBlack es
                                (WorkspaceScope True,True)       -> workspaceDExp es
                                (WorkspaceScope True,False)      -> workspaceDExpNoBlack es
                                (SystemScope,True)               -> systemExp es
                                (SystemScope,False)              -> systemExpNoBlack es
        mapM_ (\p -> treeViewExpandToPath (treeView m) p) paths1
        mapM_ (\p -> treeViewExpandToPath (descrView m) p) paths2

-- * GUI History

recordSelHistory :: IDEAction
recordSelHistory = do
    liftIO $ debugM "leksah" "selectIdentifier"
    mods <- getModules Nothing
    ideR <- ask
    selTree <- liftIO $ getSelectionTree (treeView mods) (treeStore mods)
    selDescr <- liftIO $ getSelectionDescr (descrView mods) (descrStore mods)
    let selMod = case selTree of
                        Just (_,Just (md,_)) -> Just (modu $ mdModuleId md)
                        otherwise -> Nothing
    let selFacet = case selDescr of
                        Nothing -> Nothing
                        Just descr -> Just (dscName descr)
    oldSel       <- liftIO $ readIORef (oldSelection mods)
    triggerEventIDE (RecordHistory ((ModuleSelected selMod selFacet),
        (ModuleSelected (moduleS' oldSel) (facetS' oldSel))))
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
    ideR                    <-  ask
    mods                    <-  getModules Nothing
    oldSel                  <-  liftIO $ readIORef (oldSelection mods)
    triggerEventIDE (RecordHistory ((ScopeSelected sc bl),
        (ScopeSelected (scope' oldSel) (blacklist' oldSel))))
    liftIO $ writeIORef (oldSelection mods) (oldSel{scope'= sc, blacklist' = bl})
    return ()

replayScopeHistory :: Scope -> Bool -> IDEAction
replayScopeHistory sc bl = do
    liftIO $ debugM "leksah" "selectIdentifier"
    mods <-  getModules Nothing
    liftIO $ do
        toggleButtonSetActive (blacklistB mods) bl
        toggleButtonSetActive (packageScopeB mods) (sc == (PackageScope False))
        toggleButtonSetActive (workspaceScopeB mods) (sc == PackageScope True)
        toggleButtonSetActive (systemScopeB mods) (sc == SystemScope)
    setScope (sc,bl)
    oldSel <- liftIO $ readIORef (oldSelection mods)
    liftIO $ writeIORef (oldSelection mods) (oldSel{scope'= sc, blacklist' = bl})
