{-# OPTIONS_GHC -XDeriveDataTypeable -XMultiParamTypeClasses
    -XScopedTypeVariables -XTypeSynonymInstances #-}
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
--,   showInterface
,   selectIdentifier
,   reloadKeepSelection
,   replaySelHistory
,   replayScopeHistory
) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Gdk.Events
import Data.Maybe
import Control.Monad.Reader
import qualified Data.Map as Map
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
import Control.Event hiding (Event)
import Distribution.ModuleName
import Distribution.Text (simpleParse,display)
import Data.Typeable (Typeable(..))
import Control.Exception (SomeException(..),catch)
import IDE.Package (packageConfig,addModuleToPackageDescr,getModuleTemplate,getPackageDescriptionAndPath)
import Distribution.PackageDescription (allBuildInfo,hsSourceDirs)
import System.FilePath ((</>),dropFileName)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import Graphics.UI.Editor.MakeEditor (buildEditor,FieldDescription(..),mkField)
import Graphics.UI.Editor.Parameters (paraMultiSel,Parameter(..),emptyParams,(<<<-),paraName)
import Graphics.UI.Editor.Simple (boolEditor,okCancelFields,staticListEditor,stringEditor)
import Graphics.UI.Editor.Basics (eventPaneName,GUIEventSelector(..))
import IDE.Metainfo.Provider (rebuildPackageInfo)
import qualified System.IO.UTF8 as UTF8  (writeFile)


-- | A modules pane description
--

data IDEModules     =   IDEModules {
    outer           ::   VBox
,   paned           ::   HPaned
,   treeView        ::   TreeView
,   treeStore       ::   TreeStore (String, [(ModuleDescr,PackageDescr)])
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
                                    (Maybe ModuleName, Maybe Symbol) ExpanderState
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
,   facetS'         ::   Maybe String
,   scope'          ::   Scope
,   blacklist'      ::   Bool}
 deriving (Eq,Ord,Show)

instance Pane IDEModules IDEM
    where
    primPaneName _  =   "Modules"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . outer
    paneId b        =   "*Modules"
        --liftIO $ widgetGrabFocus (descrView p)

getModules :: Maybe PanePath -> IDEM IDEModules
getModules Nothing    = forceGetPane (Right "*Modules")
getModules (Just pp)  = forceGetPane (Left pp)


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
                            Nothing -> Nothing
                            Just (_,[]) -> Nothing
                            Just (_,((md,_):_)) -> Just (modu $ moduleIdMD md),
                                 case mbFacetSelection of
                                    Nothing -> Nothing
                                    Just fw -> Just (descrName fw))
                return (Just (ModulesState i sc mbs expander))
    recoverState pp (ModulesState i sc@(scope,useBlacklist) se exp)  =  do
        nb          <-  getNotebook pp
        p           <- buildPane pp nb builder
        mod         <-  getModules Nothing
        liftIO $ writeIORef (expanderState mod) exp
        setScope sc
        liftIO $ panedSetPosition (paned mod) i
        fillModulesList sc
        selectNames se
        applyExpanderState
        return p
    builder pp nb windows = do
        packageInfo' <- readIDE packageInfo
        reifyIDE $ \ ideR -> do
            let forest  = case packageInfo' of
                            Nothing     ->  []
                            Just pair   ->  subForest (buildModulesTree pair)
            treeStore   <-  treeStoreNew forest
            treeView    <-  treeViewNew
            treeViewSetModel treeView treeStore
            --treeViewSetRulesHint treeView True

            renderer0    <- cellRendererPixbufNew
            set renderer0 [ cellPixbufStockId  := "" ]

            renderer    <- cellRendererTextNew
            col         <- treeViewColumnNew
            treeViewColumnSetTitle col "Modules"
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
                cellPixbufStockId  :=
                    if null (snd row)
                        then ""
                        else if isJust (mbSourcePathMD (fst (head (snd row))))
                                then "ide_source"
                                else ""]

            renderer2   <- cellRendererTextNew
            col2        <- treeViewColumnNew
            treeViewColumnSetTitle col2 "Packages"
            treeViewColumnSetSizing col2 TreeViewColumnAutosize
            treeViewColumnSetResizable col2 True
            treeViewColumnSetReorderable col2 True
            treeViewAppendColumn treeView col2
            cellLayoutPackStart col2 renderer2 True
            cellLayoutSetAttributes col2 renderer2 treeStore
                $ \row -> [ cellText := (concat . intersperse  ", ")
                            $ map (display . packagePD . snd) (snd row)]
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
            treeViewColumnSetTitle col "Interface"
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
                                            else if isJust (mbLocation row)
                                                then "ide_source"
                                                else ""]
            treeViewSetHeadersVisible descrView True
            treeViewSetEnableSearch descrView True
            treeViewSetSearchEqualFunc descrView (Just (descrViewSearch descrView descrStore))
            pane'           <-  hPanedNew
            sw              <-  scrolledWindowNew Nothing Nothing
            containerAdd sw treeView
            scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
            sw2             <-  scrolledWindowNew Nothing Nothing
            containerAdd sw2 descrView
            scrolledWindowSetPolicy sw2 PolicyAutomatic PolicyAutomatic
            panedAdd1 pane' sw
            panedAdd2 pane' sw2
            (x,y) <- widgetGetSize nb
            panedSetPosition pane' (x `quot` 2)
            box             <-  hBoxNew True 2
            rb1             <-  radioButtonNewWithLabel "Package"
            rb2             <-  radioButtonNewWithLabelFromWidget rb1 "Workspace"
            rb3             <-  radioButtonNewWithLabelFromWidget rb1 "System"
            toggleButtonSetActive rb3 True
            cb2             <-  checkButtonNewWithLabel "Imports"
            cb              <-  checkButtonNewWithLabel "Blacklist"

            boxPackStart box rb1 PackGrow 2
            boxPackStart box rb2 PackGrow 2
            boxPackStart box rb3 PackGrow 2
            boxPackEnd box cb PackNatural 2
            boxPackEnd box cb2 PackNatural 2


            boxOuter        <-  vBoxNew False 2
            boxPackStart boxOuter box PackNatural 2
            boxPackStart boxOuter pane' PackGrow 2
            oldState <- liftIO $ newIORef $ SelectionState Nothing Nothing SystemScope False
            expanderState <- liftIO $ newIORef emptyExpansion
            scopeRef <- newIORef (SystemScope,True)
            let modules = IDEModules boxOuter pane' treeView treeStore descrView descrStore
                                rb1 rb2 rb3 cb2 cb oldState expanderState
            cid3 <- treeView `onRowActivated`
                (\ treePath _ -> do
                    treeViewExpandRow treeView treePath False
                    return ())
            cid1 <- treeView `afterFocusIn`
                (\_ -> do reflectIDE (makeActive modules) ideR; return True)
            cid2 <- descrView `afterFocusIn`
                (\_ -> do reflectIDE (makeActive modules) ideR; return True)
            treeView  `onButtonPress` (treeViewPopup ideR treeStore treeView)
            descrView `onButtonPress` (descrViewPopup ideR descrStore descrView)
            rb1 `onToggled` (reflectIDE scopeSelection ideR)
            rb2 `onToggled` (reflectIDE scopeSelection ideR)
            rb3 `onToggled` (reflectIDE scopeSelection ideR)
            cb  `onToggled` (reflectIDE scopeSelection ideR)
            cb2 `onToggled` (reflectIDE scopeSelection ideR)
            sel     <-  treeViewGetSelection treeView
            sel `onSelectionChanged` do
                fillFacets treeView treeStore descrView descrStore
                reflectIDE recordSelHistory ideR
                return ()
            sel2    <-  treeViewGetSelection descrView
            sel2 `onSelectionChanged` do
                fillInfo descrView descrStore ideR
                reflectIDE recordSelHistory ideR
                return ()
            return (Just modules,[ConnectC cid1,ConnectC cid2, ConnectC cid3])


selectIdentifier :: Descr -> IDEAction
selectIdentifier idDescr = do
    systemInfo'     <-  readIDE systemInfo
    currentInfo'    <-  readIDE packageInfo
    currentScope    <-  getScope
    case descrModu idDescr of
        Nothing -> return ()
        Just pm -> do
            let pid = pack pm
            let mbNeededScope = case currentInfo' of
                                Just (localScope,packageScope) ->
                                    if Map.member pid (fst localScope)
                                        then Just (PackageScope False)
                                        else if Map.member pid (fst packageScope)
                                                then Just (PackageScope True)
                                                else case systemInfo' of
                                                        Just worldScope ->
                                                            if Map.member pid (fst worldScope)
                                                                then Just SystemScope
                                                                else Nothing
                                                        Nothing -> Nothing
                                Nothing -> case systemInfo' of
                                                Just worldScope ->
                                                    if Map.member pid (fst worldScope)
                                                        then Just SystemScope
                                                        else Nothing
                                                Nothing -> Nothing
            case mbNeededScope of
                Nothing -> return ()
                Just sc -> do
                    when (fst currentScope < sc) (setScope (sc,snd currentScope))
                    selectIdentifier' (modu pm) (descrName idDescr)

selectIdentifier' :: ModuleName -> Symbol -> IDEAction
selectIdentifier'  moduleName symbol =
    let nameArray = components moduleName
    in do
        mods            <- getModules Nothing
        mbTree          <-  liftIO $ treeStoreGetTreeSave (treeStore mods) []
        case treePathFromNameArray mbTree nameArray [] of
            Just treePath   ->  liftIO $ do
                treeViewExpandToPath (treeView mods) treePath
                sel         <-  treeViewGetSelection (treeView mods)
                treeSelectionSelectPath sel treePath
                col         <-  treeViewGetColumn (treeView mods) 0
                treeViewScrollToCell (treeView mods) treePath (fromJust col) (Just (0.3,0.3))
                mbFacetTree <-  treeStoreGetTreeSave (descrStore mods) []
                selF        <-  treeViewGetSelection (descrView mods)
                case  findPathFor symbol mbFacetTree of
                    Nothing     ->  sysMessage Normal "no path found"
                    Just path   ->  do
                        treeViewExpandToPath (descrView mods) path
                        treeSelectionSelectPath selF path
                        col     <-  treeViewGetColumn (descrView mods) 0
                        treeViewScrollToCell (descrView mods) path (fromJust col) (Just (0.3,0.3))
                bringPaneToFront mods
            Nothing         ->  return ()

findPathFor :: Symbol -> Maybe (Tree Descr) -> Maybe TreePath
findPathFor symbol (Just (Node _ forest)) =
    foldr ( \i mbTreePath -> findPathFor' [i] (forest !! i) mbTreePath)
                            Nothing  [0 .. ((length forest) - 1)]
    where
    findPathFor' :: TreePath -> Tree Descr -> Maybe TreePath -> Maybe TreePath
    findPathFor' _ node (Just p)                  =   Just p
    findPathFor' path (Node wrap sub) Nothing     =
        if descrName wrap == symbol
            then Just (reverse path)
            else
                foldr ( \i mbTreePath -> findPathFor' (i:path) (sub !! i) mbTreePath)
                            Nothing     [0 .. ((length sub) - 1)]
findPathFor symbol Nothing = Nothing

treePathFromNameArray :: Maybe ModTree -> [String] -> [Int] -> Maybe [Int]
treePathFromNameArray (Just tree) [] accu      =   Just (reverse accu)
treePathFromNameArray (Just tree) (h:t) accu   =
    let names   =   map (\t -> fst $ rootLabel t) (subForest tree)
        mbIdx   =   elemIndex h names
    in case mbIdx of
            Nothing ->  Nothing
            Just i  ->  treePathFromNameArray (Just (subForest tree !! i)) t (i:accu)
treePathFromNameArray Nothing _ _  = Nothing

--showInterface :: Maybe PanePath -> IDEAction
--showInterface mbPP = do
--    m <- getModules mbPP
--    liftIO $ bringPaneToFront m
----    liftIO $ widgetGrabFocus (descrView m)


treeViewSearch :: TreeView
    -> TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> String
    -> TreeIter
    -> IO Bool
treeViewSearch treeView treeStore string iter =  do
    path   <- treeModelGetPath treeStore iter
    val    <- treeStoreGetValue treeStore path
    mbTree <- treeStoreGetTreeSave treeStore path
    exp    <- treeViewRowExpanded treeView path
    when (isJust mbTree && (not (null (subForest (fromJust mbTree)))) && not exp) $
        let found = searchInModSubnodes (fromJust mbTree) string
        in when found $ do
            treeViewExpandRow treeView path False
            return ()
    let str2      = case snd val of
                        []      -> fst val
                        (m,_):_ -> showPackModule  (moduleIdMD m)
    let res       =  isInfixOf (map toLower string) (map toLower str2)
    return res

searchInModSubnodes :: ModTree -> String -> Bool
searchInModSubnodes tree str =
    not $ null
        $ filter (\ val ->
            let cstr = case snd val of
                    [] -> fst val
                    (m,_):_ -> show (Present (moduleIdMD m))
            in  isInfixOf (map toLower str) (map toLower cstr))
                $ concatMap flatten (subForest tree)

descrViewSearch :: TreeView
    -> TreeStore Descr
    -> String
    -> TreeIter
    -> IO Bool
descrViewSearch descrView descrStore string iter = do
    path    <- treeModelGetPath descrStore iter
    val     <- treeStoreGetValue descrStore path
    tree <- treeStoreGetTree descrStore path
    exp  <- treeViewRowExpanded descrView path
    when (not (null (subForest tree)) && not exp) $
        let found = searchInFacetSubnodes tree string
        in when found $ do
            treeViewExpandRow descrView path False
            return ()
    return (isInfixOf (map toLower string) (map toLower (descrTreeText val)))

searchInFacetSubnodes :: DescrTree -> String -> Bool
searchInFacetSubnodes tree str =
    not $ null
        $ filter (\ val ->
            isInfixOf (map toLower str) (map toLower (descrTreeText val)))
                $ concatMap flatten (subForest tree)

fillFacets :: TreeView
    -> TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> TreeView
    -> TreeStore Descr
    -> IO ()
fillFacets treeView treeStore descrView descrStore = do
    sel             <-  getSelectionTree treeView treeStore
    case sel of
        Just val
            ->  case snd val of
                    ((mod,package):_)
                        ->  let forest = buildFacetForrest mod in do
                                emptyModel <- treeStoreNew []
                                treeViewSetModel descrView emptyModel
                                treeStoreClear descrStore
                                mapM_ (\(e,i) -> treeStoreInsertTree descrStore [] i e)
                                            $ zip forest [0 .. length forest]
                                treeViewSetModel descrView descrStore
                                treeViewSetEnableSearch descrView True
                                treeViewSetSearchEqualFunc descrView (Just (descrViewSearch descrView descrStore))
                    []  ->      treeStoreClear descrStore
        Nothing
            ->  treeStoreClear descrStore

getSelectionTree ::  TreeView
    ->  TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> IO (Maybe (String, [(ModuleDescr,PackageDescr)]))
getSelectionTree treeView treeStore = do
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
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        []      ->  return ()
        [a]     ->  do
            descr    <-  treeStoreGetValue lst a
            reflectIDE (setInfo descr) ideR
            return ()
        _       ->  return ()

findDescription :: PackModule -> SymbolTable -> Symbol -> Maybe (Symbol,Descr)
findDescription md st s     =
    case Map.lookup s st  of
        Nothing ->  Nothing
        Just l  ->  case filter (\id -> case descrModu id of
                                            Nothing -> False
                                            Just pm -> md == pm) l of
                         [] -> Nothing
                         l  -> Just (s,head l)

fillModulesList :: (Scope,Bool) -> IDEAction
fillModulesList (scope,useBlacklist) = do
    mods  <-  getModules Nothing
    prefs                       <-  readIDE prefs
    case scope of
        SystemScope -> do
            accessibleInfo'             <-  readIDE systemInfo
            case accessibleInfo' of
                Nothing ->  liftIO $ do
                                        treeStoreClear (treeStore mods)
                                        treeStoreInsertTree (treeStore mods) [] 0 (Node ("",[]) [])
                Just (ai@(pm,ps)) ->
                    let p2  =   if useBlacklist
                                    then (Map.filter (filterBlacklist
                                            (packageBlacklist prefs)) pm, ps)
                                    else ai
                        (Node _ li) = buildModulesTree ((Map.empty,Map.empty),p2)
                    in insertIt li mods
        WorkspaceScope withImports -> do
            workspaceInfo'           <-  readIDE workspaceInfo
            packageInfo'             <-  readIDE packageInfo
            case workspaceInfo' of
                Nothing ->  insertIt [] mods
                Just (l,p) ->
                    let (l',p'@(pm,ps)) =   if withImports
                                                then (l,p)
                                                else (l,(Map.empty,Map.empty))
                        p2              =   if useBlacklist
                                                then (Map.filter (filterBlacklist
                                                        (packageBlacklist prefs)) pm, ps)
                                                else p'
                        (Node _ li)     =   buildModulesTree (l',p2)
                    in insertIt li mods
        PackageScope withImports -> do
            packageInfo' <- readIDE packageInfo
            case packageInfo' of
                Nothing             ->   insertIt [] mods
                Just (l,p) ->
                    let (l',p'@(pm,ps)) =   if withImports
                                                then (l,p)
                                                else (l,(Map.empty,Map.empty))
                        p2              =   if useBlacklist
                                                then (Map.filter (filterBlacklist
                                                        (packageBlacklist prefs)) pm, ps)
                                                else p'
                        (Node _ li)     =   buildModulesTree (l',p2)
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
    let packageId   =   packagePD packageDescr
        name        =   pkgName packageId
        version     =   pkgVersion packageId
    in  isNothing $ find (\ (Dependency str vr) -> str == name && withinRange version vr)
                    dependencies


type DescrForest = Forest Descr
type DescrTree = Tree Descr


descrTreeText :: Descr -> String
descrTreeText (Descr id _ _ _ _ (InstanceDescr binds)) = id ++ " " ++ printBinds binds
    where
        printBinds []       =   ""
        printBinds (a:[])   =   a
        printBinds (a:b)    =   a ++ " " ++ printBinds b
descrTreeText d = descrName d

descrIdType :: Descr -> DescrType
descrIdType = descrType . details

buildFacetForrest ::  ModuleDescr -> DescrForest
buildFacetForrest modDescr =
    let (instances,other)       =   partition (\id -> case details id of
                                                        InstanceDescr _ -> True
                                                        _   -> False)
                                            $ take 2000 $  idDescriptionsMD modDescr
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
            case details descr of
                DataDescr constructors fields ->
                    Node descr ((map (\(fn,ty) -> Node (makeReexported descr (Descr{descrName' = fn, typeInfo' = ty,
                            descrModu' = descrModu descr, mbLocation' = mbLocation descr,
                            mbComment' = mbComment descr, details' = ConstructorDescr {typeDescrC = descr}})) [])
                                constructors)
                                    ++  (map (\(fn,ty) -> Node (makeReexported descr (Descr{descrName' = fn, typeInfo' = ty,
                                descrModu' = descrModu descr, mbLocation' = mbLocation descr,
                                mbComment' = mbComment descr, details' = FieldDescr {typeDescrF = descr}})) [])
                                    fields))
                NewtypeDescr constr mbField ->
                    Node descr (Node (makeReexported descr (Descr{descrName' = fst constr, typeInfo' = snd constr,
                            descrModu' = descrModu descr, mbLocation' = mbLocation descr,
                            mbComment' = mbComment descr, details' = ConstructorDescr {typeDescrC = descr}})) []
                                : case  mbField of
                                    Just fld ->
                                        [Node (makeReexported descr (Descr{descrName' = fst fld, typeInfo' = snd fld,
                                descrModu' = descrModu descr, mbLocation' = mbLocation descr,
                                mbComment' = mbComment descr, details' = FieldDescr {typeDescrF = descr}})) []]
                                    Nothing -> [])
                ClassDescr _ methods ->
                    Node descr (map (\(fn,ty) -> Node (makeReexported descr (Descr{descrName' = fn, typeInfo' = ty,
                        descrModu' = descrModu descr, mbLocation' = mbLocation descr,
                        mbComment' = mbComment descr, details' = MethodDescr {classDescrM = descr}})) [])
                            methods)
                _ -> Node descr []
        where
            makeReexported :: Descr -> Descr -> Descr
            makeReexported d1 d2
                | isReexported d1   =   Reexported{descrModu' = descrModu' d1, impDescr = d2}
                | otherwise         =   d2

    addInstances :: (DescrForest,[Descr])
        -> Descr
        -> (DescrForest,[Descr])
    addInstances (forest,orphaned) instDescr =
        case foldl' (matches instDescr) ([],False) forest of
            (f,True)    -> (f,orphaned)
            (f,False)   -> (forest, instDescr:orphaned)

    matches :: Descr
        -> (DescrForest,Bool)
        ->  DescrTree
        -> (DescrForest,Bool)
    matches instDescr (forest,False) (Node dd@(Descr id _ _ _ _ (DataDescr _ _)) sub)
        | not (null (binds (details instDescr))) &&
                id == head (binds (details instDescr))
            =   ((Node dd (sub ++ [Node newInstDescr []])):forest,True)
        where newInstDescr = if isNothing (mbLocation instDescr)
                                then instDescr{mbLocation' = mbLocation dd}
                                else instDescr
    matches instDescr (forest,False) (Node dd@(Descr id _ _ _ _ (NewtypeDescr _ _)) sub)
        | not (null (binds (details instDescr))) &&
                id == head (binds (details instDescr))
            =   ((Node dd (sub ++ [Node newInstDescr []])):forest,True)
        where newInstDescr = if isNothing (mbLocation instDescr)
                                then instDescr{mbLocation' = mbLocation dd}
                                else instDescr
    matches _ (forest,b) node = (node:forest,b)


type ModTree = Tree (String, [(ModuleDescr,PackageDescr)])
--
-- | Make a Tree with a module desription, package description pairs tree to display.
--   Their are nodes with a label but without a module (like e.g. Data).
--
buildModulesTree :: (PackageScope,PackageScope) -> ModTree
buildModulesTree ((localMap,_),(otherMap,_)) =
    let flatPairs           =   concatMap (\p -> map (\m -> (m,p)) (exposedModulesPD p))
                                    (Map.elems localMap ++ Map.elems otherMap)
        emptyTree           =   (Node ("",[]) [])
        resultTree          =   foldl' insertPairsInTree emptyTree flatPairs
        in sortTree resultTree
    where
    insertPairsInTree :: ModTree -> (ModuleDescr,PackageDescr) -> ModTree
    insertPairsInTree tree pair =
        let nameArray           =   components $ modu $ moduleIdMD $ fst pair
            pairedWith          =   map (\n -> (n,pair)) nameArray
        in  insertNodesInTree pairedWith tree

    insertNodesInTree :: [(String,(ModuleDescr,PackageDescr))] -> ModTree -> ModTree
    insertNodesInTree list@[(str2,pair)] (Node (str1,pairs) forest) =
        case partition (\ (Node (s,_) _) -> s == str2) forest of
            ([],_)              ->  (Node (str1,pairs) (makeNodes list : forest))
            ([(Node (_,pairsf) l)],rest)
                                ->  (Node (str1,pairs) ((Node (str2,pair : pairsf) l) : rest))
            (_,_)               ->  throwIDE "insertNodesInTree: impossible1"
    insertNodesInTree  list@((str2,pair):tl) (Node (str1,pairs) forest) =
        case partition (\ (Node (s,_) _) -> s == str2) forest of
            ([],_)              ->  (Node (str1,pairs)  (makeNodes list : forest))
            ([found],rest)      ->  (Node (str1,pairs) (insertNodesInTree tl found : rest))
            (_,_)               ->  throwIDE "insertNodesInTree: impossible2"
    insertNodesInTree [] t      =   t

    makeNodes :: [(String,(ModuleDescr,PackageDescr))] -> ModTree
    makeNodes [(str,pair)]      =   Node (str,[pair]) []
    makeNodes ((str,_):tl)      =   Node (str,[]) [makeNodes tl]
    makeNodes _                 =   throwIDE "Impossible in makeNodes"

breakAtDots :: [String] -> String -> [String]
breakAtDots res []          =   reverse res
breakAtDots res toBreak     =   let (newRes,newToBreak) = span (\c -> c /= '.') toBreak
                                in  if null newToBreak
                                        then reverse (newRes : res)
                                        else breakAtDots (newRes : res) (tail newToBreak)


instance Ord a => Ord (Tree a) where
    compare (Node l1 _) (Node l2 _) =  compare l1 l2

sortTree :: Ord a => Tree a -> Tree a
sortTree (Node l forest)    =   Node l (sort (map sortTree forest))

treeViewPopup :: IDERef
    -> TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> TreeView
    -> Event
    -> IO (Bool)
treeViewPopup ideR  store treeView (Button _ click _ _ _ _ button _ _) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Edit source"
            item1 `onActivateLeaf` do
                sel         <-  getSelectionTree treeView store
                case sel of
                    Just (_,[(m,_)]) -> case mbSourcePathMD m of
                                            Nothing     ->  return ()
                                            Just fp     ->  do
                                                reflectIDE (selectSourceBuf fp) ideR
                                                return ()
                    otherwise       ->  return ()
            sep1 <- separatorMenuItemNew
            item2           <-  menuItemNewWithLabel "Expand here"
            item2 `onActivateLeaf` (expandHere treeView)
            item3           <-  menuItemNewWithLabel "Collapse here"
            item3 `onActivateLeaf` (collapseHere treeView)
            item4           <-  menuItemNewWithLabel "Expand all"
            item4 `onActivateLeaf` (treeViewExpandAll treeView)
            item5           <-  menuItemNewWithLabel "Collapse all"
            item5 `onActivateLeaf` (treeViewCollapseAll treeView)
            sep2 <- separatorMenuItemNew
            item6           <-  menuItemNewWithLabel "Add module"
            item6 `onActivateLeaf` (reflectIDE (addModule treeView store) ideR)

            mapM_ (menuShellAppend theMenu) [castToMenuItem item1, castToMenuItem sep1, castToMenuItem item2,
                castToMenuItem item3, castToMenuItem item4, castToMenuItem item5, castToMenuItem sep2,
                castToMenuItem item6{--,item7,item8--}]
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do sel         <-  getSelectionTree treeView store
                        case sel of
                            Just (_,[(m,_)]) -> case mbSourcePathMD m of
                                                    Nothing     ->  return ()
                                                    Just fp     ->  do
                                                        reflectIDE (selectSourceBuf fp) ideR
                                                        return ()
                            otherwise       ->  return ()
                        return True
                else return False
treeViewPopup _ _ _ _ = throwIDE "treeViewPopup wrong event type"

descrViewPopup :: IDERef
    -> TreeStore Descr
    -> TreeView
    -> Event
    -> IO (Bool)
descrViewPopup ideR  store descrView (Button _ click _ _ _ _ button _ _) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Go to definition"
            item1 `onActivateLeaf` do
                sel         <-  getSelectionDescr descrView store
                case sel of
                    Just descr      ->  reflectIDE (goToDefinition descr) ideR
                    otherwise       ->  sysMessage Normal "Modules>> descrViewPopup: no selection"
            item2           <-  menuItemNewWithLabel "Insert in buffer"
            item2 `onActivateLeaf` do
                sel         <-  getSelectionDescr descrView store
                case sel of
                    Just descr      ->  reflectIDE (insertInBuffer descr) ideR
                    otherwise       ->  sysMessage Normal "Modules>> descrViewPopup: no selection"
            mapM_ (menuShellAppend theMenu) [item1, item2]
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do sel         <-  getSelectionDescr descrView store
                        case sel of
                            Just descr      -> reflectIDE (goToDefinition descr) ideR
                            otherwise       ->  sysMessage Normal "Modules>> descrViewPopup: no selection2"
                        return True
                else do
                    mbPane :: Maybe IDEInfo <- reflectIDE getPane ideR
                    when (isJust mbPane) $ bringPaneToFront (fromJust mbPane)
                    return False

descrViewPopup _ _ _ _ = throwIDE "descrViewPopup wrong event type"

setScope :: (Scope,Bool) -> IDEAction
setScope (sc,bl) = do
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
    (sc,bl) <- getScope
    setScope (sc,bl)
    selectScope (sc,bl)

selectScope :: (Scope,Bool) -> IDEAction
selectScope (sc,bl) = do
    recordExpanderState
    mods                <-  getModules Nothing
    mbTreeSelection     <-  liftIO $ getSelectionTree (treeView mods) (treeStore mods)
    mbDescrSelection    <-  liftIO $ getSelectionDescr (descrView mods) (descrStore mods)

    ts                  <-  liftIO $ treeViewGetSelection (treeView mods)
    withoutRecordingDo $ do
        liftIO $ treeSelectionUnselectAll ts
        fillModulesList (sc,bl)
        let mbs = (case mbTreeSelection of
                                Nothing -> Nothing
                                Just (_,[]) -> Nothing
                                Just (_,((md,_):_)) -> Just (modu $ moduleIdMD md),
                                     case mbDescrSelection of
                                        Nothing -> Nothing
                                        Just fw -> Just (descrName fw))
        selectNames mbs
    recordScopeHistory
    applyExpanderState
    liftIO $ bringPaneToFront mods

selectNames :: (Maybe ModuleName, Maybe Symbol) -> IDEAction
selectNames (mbModuleName, mbIdName) = do
    mods <- getModules Nothing
    case mbModuleName of
        Nothing -> liftIO $ do
            sel         <-  treeViewGetSelection (treeView mods)
            treeSelectionUnselectAll  sel
            selF        <-  treeViewGetSelection (descrView mods)
            treeSelectionUnselectAll  selF
        Just moduleName ->
            let nameArray = components moduleName
            in do
                mbTree              <-  liftIO $ treeStoreGetTreeSave (treeStore mods) []
                case treePathFromNameArray mbTree nameArray [] of
                    Nothing         ->  return ()
                    Just treePath   ->  liftIO $ do
                        treeViewExpandToPath (treeView mods) treePath
                        sel         <-  treeViewGetSelection (treeView mods)
                        treeSelectionSelectPath sel treePath
                        col         <-  treeViewGetColumn (treeView mods) 0
                        treeViewScrollToCell (treeView mods) treePath (fromJust col)
                            (Just (0.3,0.3))
                        case mbIdName of
                            Nothing -> do
                                selF          <-  treeViewGetSelection (descrView mods)
                                treeSelectionUnselectAll  selF
                            Just symbol -> do
                                mbDescrTree   <-  treeStoreGetTreeSave (descrStore mods) []
                                selF          <-  treeViewGetSelection (descrView mods)
                                case  findPathFor symbol mbDescrTree of
                                    Nothing     ->  sysMessage Normal "no path found"
                                    Just path   ->  do
                                        treeSelectionSelectPath selF path
                                        col     <-  treeViewGetColumn (descrView mods) 0
                                        treeViewScrollToCell (descrView mods) path (fromJust col)
                                            (Just (0.3,0.3))

reloadKeepSelection :: IDEAction
reloadKeepSelection = do
    mbMod <- getPane
    case mbMod of
        Nothing -> return ()
        Just mods
            -> do
            mbTreeSelection     <-  liftIO $ getSelectionTree (treeView mods) (treeStore mods)
            mbDescrSelection    <-  liftIO $ getSelectionDescr (descrView mods) (descrStore mods)
            sc                  <-  getScope
            recordExpanderState
            fillModulesList sc
            liftIO $ treeStoreClear (descrStore mods)
            let mbs = (case mbTreeSelection of
                            Nothing -> Nothing
                            Just (_,[]) -> Nothing
                            Just (_,((md,_):_)) -> Just (modu $ moduleIdMD md),
                                 case mbDescrSelection of
                                    Nothing -> Nothing
                                    Just fw -> Just (descrName fw))
            applyExpanderState
            selectNames mbs



treeStoreGetTreeSave :: TreeStore a -> TreePath -> IO (Maybe (Tree a))
treeStoreGetTreeSave treeStore treePath = catch (do
    res <- treeStoreGetTree treeStore treePath
    return (Just res)) (\ (_ :: SomeException) -> return Nothing)

expandHere :: TreeView -> IO ()
expandHere treeView = do
    sel   <- treeViewGetSelection treeView
    paths <- treeSelectionGetSelectedRows sel
    case paths of
        []     -> return ()
        (hd:_) -> treeViewExpandRow treeView hd True >> return ()

collapseHere :: TreeView -> IO ()
collapseHere treeView = do
    sel   <- treeViewGetSelection treeView
    paths <- treeSelectionGetSelectedRows sel
    case paths of
        []     -> return ()
        (hd:_) -> treeViewCollapseRow treeView hd >> return ()

addModule :: TreeView -> TreeStore (String, [(ModuleDescr,PackageDescr)]) -> IDEAction
addModule treeView store = do
    sel   <- liftIO $ treeViewGetSelection treeView
    paths <- liftIO $ treeSelectionGetSelectedRows sel
    categories <- case paths of
        []     -> return []
        (treePath:_) -> liftIO $ mapM (treeStoreGetValue store)
                                    $ map (\n -> take n treePath)  [1 .. length treePath]
    mbPD <- getPackageDescriptionAndPath
    case mbPD of
        Nothing             -> ideMessage Normal "No package description"
        Just (pd,cabalPath) -> let srcPaths = nub $ concatMap hsSourceDirs $ allBuildInfo pd
                                   rootPath = dropFileName cabalPath
                                   modPath  = foldr (\a b -> a ++ "." ++ b) ""
                                                (map fst categories)
                               in do
            window' <- getMainWindow
            mbResp <- liftIO $ addModuleDialog window' modPath srcPaths
            case mbResp of
                Nothing                -> return ()
                Just (AddModule modPath srcPath isExposed) ->
                    case simpleParse modPath of
                        Nothing         -> ideMessage Normal ("Not a valid module name :" ++ modPath)
                        Just moduleName -> do
                            let  target = srcPath </> toFilePath moduleName ++ ".hs"
                            liftIO $ createDirectoryIfMissing True (dropFileName target)
                            alreadyExists <- liftIO $ doesFileExist target
                            if alreadyExists
                                then ideMessage Normal "File already exists"
                                else do
                                    template <- liftIO $ getModuleTemplate pd modPath
                                    liftIO $ UTF8.writeFile target template
                                    addModuleToPackageDescr moduleName isExposed
                                    packageConfig
                                    rebuildPackageInfo
                                    fileOpenThis target


--  Yet another stupid little dialog

data AddModule = AddModule {moduleName :: String, sourceRoot :: FilePath, isExposed :: Bool}

addModuleDialog :: Window -> String -> [String] -> IO (Maybe AddModule)
addModuleDialog parent modString sourceRoots = do
    dia                        <-   dialogNew
    windowSetTransientFor dia parent
    windowSetTitle dia "Construct new module"
    upper                      <-   dialogGetUpper dia
    lower                      <-   dialogGetActionArea dia
    (widget,inj,ext,_)         <-   buildEditor (moduleFields sourceRoots)
                                        (AddModule modString (head sourceRoots) False)
    (widget2,_,_,notifier)     <-   buildEditor okCancelFields ()
    registerEvent notifier Clicked (Left (\e -> do
            case eventPaneName e of
                "Ok"    ->  dialogResponse dia ResponseOk
                _       ->  dialogResponse dia ResponseCancel
            return e))
    boxPackStart upper widget PackGrow 7
    boxPackStart lower widget2 PackNatural 7
    widgetShowAll dia
    resp <- dialogRun dia
    value                      <- ext (AddModule modString (head sourceRoots) False)
    widgetDestroy dia
    --find
    case resp of
        ResponseOk    -> return value
        _             -> return Nothing

moduleFields :: [FilePath] -> FieldDescription AddModule
moduleFields list = VFD emptyParams [
        mkField
            (paraName <<<- ParaName ("New module ")
                    $ emptyParams)
            moduleName
            (\ a b -> b{moduleName = a})
            (stringEditor (\s -> True)),
        mkField
            (paraName <<<- ParaName ("Root of the source path")
                $ paraMultiSel <<<- ParaMultiSel False
                    $ emptyParams)
            (\a -> sourceRoot a)
            (\ a b -> b{sourceRoot = a})
            (staticListEditor list id),
        mkField
            (paraName <<<- ParaName ("Is this an exposed library module")
                    $ emptyParams)
            isExposed
            (\ a b -> b{isExposed = a})
            boolEditor]

-- * Expander State

emptyExpansion      = ExpanderState  ([],[])  ([],[])  ([],[])  ([],[])  ([],[]) ([],[])  ([],[])  ([],[])  ([],[]) ([],[])

recordExpanderState :: IDEAction
recordExpanderState = do
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
    mods <- getModules Nothing
    ideR <- ask
    selTree <- liftIO $ getSelectionTree (treeView mods) (treeStore mods)
    selDescr <- liftIO $ getSelectionDescr (descrView mods) (descrStore mods)
    let selMod = case selTree of
                        Nothing -> Nothing
                        Just (_,[]) -> Nothing
                        Just (_,((md,_):_)) -> Just (modu $ moduleIdMD md)
    let selFacet = case selDescr of
                        Nothing -> Nothing
                        Just descr -> Just (descrName descr)
    oldSel       <- liftIO $ readIORef (oldSelection mods)
    triggerEventIDE (RecordHistory ((ModuleSelected selMod selFacet),
        (ModuleSelected (moduleS' oldSel) (facetS' oldSel))))
    liftIO $ writeIORef (oldSelection mods) (oldSel{moduleS'= selMod, facetS' = selFacet})
    return ()

replaySelHistory :: Maybe ModuleName -> Maybe Symbol -> IDEAction
replaySelHistory mbModName mbFacetName = do
    mods <- getModules Nothing
    selectNames (mbModName, mbFacetName)
    oldSel <- liftIO $ readIORef (oldSelection mods)
    liftIO $ writeIORef (oldSelection mods)
        (oldSel{moduleS'= mbModName, facetS' = mbFacetName})

recordScopeHistory :: IDEAction
recordScopeHistory = do
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
    mods <-  getModules Nothing
    liftIO $ do
        toggleButtonSetActive (blacklistB mods) bl
        toggleButtonSetActive (packageScopeB mods) (sc == (PackageScope False))
        toggleButtonSetActive (workspaceScopeB mods) (sc == PackageScope True)
        toggleButtonSetActive (systemScopeB mods) (sc == SystemScope)
    setScope (sc,bl)
    oldSel <- liftIO $ readIORef (oldSelection mods)
    liftIO $ writeIORef (oldSelection mods) (oldSel{scope'= sc, blacklist' = bl})
