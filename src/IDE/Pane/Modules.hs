{-# OPTIONS_GHC -XDeriveDataTypeable -XMultiParamTypeClasses
    -XScopedTypeVariables -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Modules
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info@leksah.org>
-- Stability   :  experimental
-- Portability  :  portable
--
-- | The pane of ide where modules are presented in tree form with their
--   packages and exports
--
-------------------------------------------------------------------------------

module IDE.Pane.Modules (
    IDEModules(..)
,   ModulesState(..)

,   showModules
,   showInterface
,   selectIdentifier
,   reloadKeepSelection
,   replaySelHistory
,   replayScopeHistory
) where

import Graphics.UI.Gtk hiding (get)
import qualified Graphics.UI.Gtk.ModelView as New
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
import MyMissing (forceJust,forceHead,split)
import IDE.Metainfo.Provider (rebuildActiveInfo)
import Debug.Trace (trace)


-- | A modules pane description
--

data IDEModules     =   IDEModules {
    outer           ::   VBox
,   paned           ::   HPaned
,   treeView        ::   New.TreeView
,   treeStore       ::   New.TreeStore (String, [(ModuleDescr,PackageDescr)])
,   descrView       ::   New.TreeView
,   descrStore      ::   New.TreeStore Descr
,   localScopeB     ::   RadioButton
,   packageScopeB   ::   RadioButton
,   worldScopeB     ::   RadioButton
,   blacklistB      ::   CheckButton
,   oldSelection    ::   IORef SelectionState
} deriving Typeable


data ModulesState           =   ModulesState Int (Scope,Bool)
                                    (Maybe ModuleName, Maybe Symbol)
    deriving(Eq,Ord,Read,Show,Typeable)


instance IDEObject IDEModules

instance Pane IDEModules IDEM
    where
    primPaneName _  =   "Modules"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . outer
    paneId b        =   "*Modules"
    makeActive p    =   do
        activatePane p []
        --liftIO $ widgetGrabFocus (descrView p)
    close           =   closePane

instance RecoverablePane IDEModules ModulesState IDEM where
    saveState p     =   do
        (IDEModules _ _ treeView treeStore descrView descrStore _ _ _ _ _) <- getModules
        sc          <-  getScope
        mbModules   <-  getPane
        case mbModules of
            Nothing ->  return Nothing
            Just p  ->  liftIO $ do
                i   <-  panedGetPosition (paned p)
                mbTreeSelection     <-  getSelectionTree treeView treeStore
                mbFacetSelection    <-  getSelectionDescr descrView descrStore
                let mbs = (case mbTreeSelection of
                            Nothing -> Nothing
                            Just (_,[]) -> Nothing
                            Just (_,((md,_):_)) -> Just (modu $ moduleIdMD md),
                                 case mbFacetSelection of
                                    Nothing -> Nothing
                                    Just fw -> Just (descrName fw))
                return (Just (ModulesState i sc mbs))
    recoverState pp (ModulesState i sc@(scope,useBlacklist) se)  =  do
            nb          <-  getNotebook pp
            initModules pp nb
            mod@(IDEModules _ _ treeView treeStore descrView descrStore lb pb wb blb _)
                        <-  getModules
            case scope of
                Local   -> liftIO $ toggleButtonSetActive lb True
                Package -> liftIO $ toggleButtonSetActive pb True
                System   -> liftIO $ toggleButtonSetActive wb True
            liftIO $ toggleButtonSetActive blb useBlacklist
            liftIO $ panedSetPosition (paned mod) i
            fillModulesList sc
            selectNames se



selectIdentifier :: Descr -> IDEAction
selectIdentifier idDescr = do
    accessibleInfo' <-  readIDE accessibleInfo
    currentInfo'    <-  readIDE currentInfo
    currentScope    <-  getScope
    let pid = pack (descrModu idDescr)
    let mbNeededScope = case currentInfo' of
                        Just (localScope,packageScope) ->
                            if Map.member pid (fst localScope)
                                then Just Local
                                else if Map.member pid (fst packageScope)
                                        then Just Package
                                        else case accessibleInfo' of
                                                Just worldScope ->
                                                    if Map.member pid (fst worldScope)
                                                        then Just System
                                                        else Nothing
                                                Nothing -> Nothing
                        Nothing -> case accessibleInfo' of
                                        Just worldScope ->
                                            if Map.member pid (fst worldScope)
                                                then Just System
                                                else Nothing
                                        Nothing -> Nothing
    case mbNeededScope of
        Nothing -> return ()
        Just sc -> do
            when (fst currentScope < sc) (setScope (sc,snd currentScope))
            selectIdentifier' (modu $ descrModu idDescr) (descrName idDescr)

selectIdentifier' :: ModuleName -> Symbol -> IDEAction
selectIdentifier'  moduleName symbol =
    let nameArray = components moduleName
    in do
        mods@(IDEModules _ _ treeView treeStore descrView descrStore _ _ _ _ _) <- getModules
        mbTree          <-  liftIO $ treeStoreGetTreeSave treeStore []
        case treePathFromNameArray mbTree nameArray [] of
            Just treePath   ->  liftIO $ do
                New.treeViewExpandToPath treeView treePath
                sel         <-  New.treeViewGetSelection treeView
                New.treeSelectionSelectPath sel treePath
                col         <-  New.treeViewGetColumn treeView 0
                New.treeViewScrollToCell treeView treePath (fromJust col) (Just (0.3,0.3))
                mbFacetTree <-  treeStoreGetTreeSave descrStore []
                selF        <-  New.treeViewGetSelection descrView
                case  findPathFor symbol mbFacetTree of
                    Nothing     ->  sysMessage Normal "no path found"
                    Just path   ->  do
                        New.treeViewExpandToPath descrView path
                        New.treeSelectionSelectPath selF path
                        col     <-  New.treeViewGetColumn descrView 0
                        New.treeViewScrollToCell descrView path (fromJust col) (Just (0.3,0.3))
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

showModules :: IDEAction
showModules = do
    m <- getModules
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeView m)

showInterface :: IDEAction
showInterface = do
    m <- getModules
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (descrView m)

getModules :: IDEM IDEModules
getModules = do
    mbMod <- getPane
    case mbMod of
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (modulesPanePath prefs) layout
            nb          <-  getNotebook pp
            initModules pp nb
            mbMod <- getPane
            case mbMod of
                Nothing ->  throwIDE "Can't init modules"
                Just m  ->  return m
        Just m ->   return m

initModules :: PanePath -> Notebook -> IDEAction
initModules panePath nb = do
    panes       <-  readIDE panes
    paneMap     <-  readIDE paneMap
    prefs       <-  readIDE prefs
    currentInfo <-  readIDE currentInfo
    (buf,cids)  <-  reifyIDE $ \ideR -> do
-- Modules List
        let forest  = case currentInfo of
                        Nothing     ->  []
                        Just pair   ->  subForest (buildModulesTree pair)
        treeStore   <-  New.treeStoreNew forest
        treeView    <-  New.treeViewNew
        New.treeViewSetModel treeView treeStore
        --New.treeViewSetRulesHint treeView True

        renderer0    <- New.cellRendererPixbufNew
        set renderer0 [ New.cellPixbufStockId  := "" ]

        renderer    <- New.cellRendererTextNew
        col         <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col "Modules"
        New.treeViewColumnSetSizing col TreeViewColumnAutosize
        New.treeViewColumnSetResizable col True
        New.treeViewColumnSetReorderable col True
        New.treeViewAppendColumn treeView col
        New.cellLayoutPackStart col renderer0 False
        New.cellLayoutPackStart col renderer True
        New.cellLayoutSetAttributes col renderer treeStore
            $ \row -> [ New.cellText := fst row]
        New.cellLayoutSetAttributes col renderer0 treeStore
            $ \row -> [
            New.cellPixbufStockId  :=
                if null (snd row)
                    then ""
                    else if isJust (mbSourcePathMD (fst (head (snd row))))
                            then "ide_source"
                            else ""]

        renderer2   <- New.cellRendererTextNew
        col2        <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col2 "Packages"
        New.treeViewColumnSetSizing col2 TreeViewColumnAutosize
        New.treeViewColumnSetResizable col2 True
        New.treeViewColumnSetReorderable col2 True
        New.treeViewAppendColumn treeView col2
        New.cellLayoutPackStart col2 renderer2 True
        New.cellLayoutSetAttributes col2 renderer2 treeStore
            $ \row -> [ New.cellText := (concat . intersperse  ", ")
                        $ map (display . packagePD . snd) (snd row)]
        New.treeViewSetHeadersVisible treeView True
        New.treeViewSetEnableSearch treeView True
        New.treeViewSetSearchEqualFunc treeView (Just (treeViewSearch treeView treeStore))

-- Facet view

        descrView   <-  New.treeViewNew
        descrStore  <-  New.treeStoreNew []
        New.treeViewSetModel descrView descrStore
        renderer30    <- New.cellRendererPixbufNew
        renderer31    <- New.cellRendererPixbufNew
        renderer3   <- New.cellRendererTextNew
        col         <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col "Interface"
        --New.treeViewColumnSetSizing col TreeViewColumnAutosize
        New.treeViewAppendColumn descrView col
        New.cellLayoutPackStart col renderer30 False
        New.cellLayoutPackStart col renderer31 False
        New.cellLayoutPackStart col renderer3 True
        New.cellLayoutSetAttributes col renderer3 descrStore
            $ \row -> [ New.cellText := descrTreeText row]
        New.cellLayoutSetAttributes col renderer30 descrStore
            $ \row -> [
            New.cellPixbufStockId  := stockIdFromType (descrIdType row)]
        New.cellLayoutSetAttributes col renderer31 descrStore
            $ \row -> [
            New.cellPixbufStockId  := if isReexported row
                                    then "ide_reexported"
                                        else if isJust (mbLocation row)
                                            then "ide_source"
                                            else ""]
        New.treeViewSetHeadersVisible descrView True
        New.treeViewSetEnableSearch descrView True
        New.treeViewSetSearchEqualFunc descrView (Just (descrViewSearch descrView descrStore))
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
        rb1             <-  radioButtonNewWithLabel "Local"
        rb2             <-  radioButtonNewWithLabelFromWidget rb1 "Package"
        rb3             <-  radioButtonNewWithLabelFromWidget rb1 "System"
        toggleButtonSetActive rb3 True
        cb              <-  checkButtonNewWithLabel "Blacklist"

        boxPackStart box rb1 PackGrow 2
        boxPackStart box rb2 PackGrow 2
        boxPackStart box rb3 PackGrow 2
        boxPackEnd box cb PackNatural 2

        boxOuter        <-  vBoxNew False 2
        boxPackStart boxOuter box PackNatural 2
        boxPackStart boxOuter pane' PackGrow 2
        oldState <- liftIO $ newIORef $ SelectionState Nothing Nothing System False
        scopeRef <- newIORef (System,True)
        let modules = IDEModules boxOuter pane' treeView treeStore descrView descrStore
                            rb1 rb2 rb3 cb oldState
        notebookInsertOrdered nb boxOuter (paneName modules) Nothing
        widgetShowAll boxOuter
        cid3 <- treeView `New.onRowActivated`
            (\ treePath _ -> do
                New.treeViewExpandRow treeView treePath False
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
        sel     <-  New.treeViewGetSelection treeView
        sel `New.onSelectionChanged` do
            fillFacets treeView treeStore descrView descrStore
            reflectIDE recordSelHistory ideR
            return ()
        sel2    <-  New.treeViewGetSelection descrView
        sel2 `New.onSelectionChanged` do
            fillInfo descrView descrStore ideR
            reflectIDE recordSelHistory ideR
            return ()
        return (modules,[ConnectC cid1,ConnectC cid2, ConnectC cid3])
    addPaneAdmin buf cids panePath
    liftIO $widgetGrabFocus (paned buf)

treeViewSearch :: TreeView
    -> New.TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> String
    -> New.TreeIter
    -> IO Bool
treeViewSearch treeView treeStore string iter =  do
    path   <- New.treeModelGetPath treeStore iter
    val    <- New.treeStoreGetValue treeStore path
    mbTree <- treeStoreGetTreeSave treeStore path
    exp    <- New.treeViewRowExpanded treeView path
    when (isJust mbTree && (not (null (subForest (fromJust mbTree)))) && not exp) $
        let found = searchInModSubnodes (fromJust mbTree) string
        in when found $ do
            New.treeViewExpandRow treeView path False
            return ()
    let str2      = case snd val of
                        []      -> fst val
                        (m,_):_ -> showPackModule (moduleIdMD m)
    let res       =  isInfixOf (map toLower string) (map toLower str2)
    return res

searchInModSubnodes :: ModTree -> String -> Bool
searchInModSubnodes tree str =
    not $ null
        $ filter (\ val ->
            let cstr = case snd val of
                    [] -> fst val
                    (m,_):_ -> showPackModule (moduleIdMD m)
            in  isInfixOf (map toLower str) (map toLower cstr))
                $ concatMap flatten (subForest tree)

descrViewSearch :: TreeView
    -> New.TreeStore Descr
    -> String
    -> New.TreeIter
    -> IO Bool
descrViewSearch descrView descrStore string iter = do
    path    <- New.treeModelGetPath descrStore iter
    val     <- New.treeStoreGetValue descrStore path
    tree <- New.treeStoreGetTree descrStore path
    exp  <- New.treeViewRowExpanded descrView path
    when (not (null (subForest tree)) && not exp) $
        let found = searchInFacetSubnodes tree string
        in when found $ do
            New.treeViewExpandRow descrView path False
            return ()
    return (isInfixOf (map toLower string) (map toLower (descrTreeText val)))

searchInFacetSubnodes :: DescrTree -> String -> Bool
searchInFacetSubnodes tree str =
    not $ null
        $ filter (\ val ->
            isInfixOf (map toLower str) (map toLower (descrTreeText val)))
                $ concatMap flatten (subForest tree)

fillFacets :: New.TreeView
    -> New.TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> New.TreeView
    -> New.TreeStore Descr
    -> IO ()
fillFacets treeView treeStore descrView descrStore = do
    sel             <-  getSelectionTree treeView treeStore
    case sel of
        Just val
            ->  case snd val of
                    ((mod,package):_)
                        ->  let forest = buildFacetForrest mod in do
                                emptyModel <- New.treeStoreNew []
                                New.treeViewSetModel descrView emptyModel
                                New.treeStoreClear descrStore
                                mapM_ (\(e,i) -> New.treeStoreInsertTree descrStore [] i e)
                                            $ zip forest [0 .. length forest]
                                New.treeViewSetModel descrView descrStore
                                New.treeViewSetEnableSearch descrView True
                                New.treeViewSetSearchEqualFunc descrView (Just (descrViewSearch descrView descrStore))
                    []  ->      New.treeStoreClear descrStore
        Nothing
            ->  New.treeStoreClear descrStore

getSelectionTree ::  New.TreeView
    ->  New.TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> IO (Maybe (String, [(ModuleDescr,PackageDescr)]))
getSelectionTree treeView treeStore = do
    treeSelection   <-  New.treeViewGetSelection treeView
    paths           <-  New.treeSelectionGetSelectedRows treeSelection
    case paths of
        []  ->  return Nothing
        a:r ->  do
            val     <-  New.treeStoreGetValue treeStore a
            return (Just val)

getSelectionDescr ::  New.TreeView
    ->  New.TreeStore Descr
    -> IO (Maybe Descr)
getSelectionDescr treeView treeStore = do
    treeSelection   <-  New.treeViewGetSelection treeView
    paths           <-  New.treeSelectionGetSelectedRows treeSelection
    case paths of
        a:r ->  do
            val     <-  New.treeStoreGetValue treeStore a
            return (Just val)
        _  ->  return Nothing


fillInfo :: New.TreeView
    -> New.TreeStore Descr
    -> IDERef
    -> IO ()
fillInfo treeView lst ideR  = do
    treeSelection   <-  New.treeViewGetSelection treeView
    paths           <-  New.treeSelectionGetSelectedRows treeSelection
    case paths of
        []      ->  return ()
        [a]     ->  do
            descr    <-  New.treeStoreGetValue lst a
            reflectIDE (setInfo descr) ideR
            return ()
        _       ->  return ()

findDescription :: PackModule -> SymbolTable -> Symbol -> Maybe (Symbol,Descr)
findDescription md st s     =
    case Map.lookup s st  of
        Nothing ->  Nothing
        Just l  ->  case filter (\id -> md == descrModu id) l of
                         [] -> Nothing
                         l  -> Just (s,head l)

fillModulesList :: (Scope,Bool) -> IDEAction
fillModulesList (scope,useBlacklist) = do
    (IDEModules _ _ treeView treeStore _ _ _ _ _ _ _)  <-  getModules
    prefs                       <-  readIDE prefs
    currentInfo'                <-  readIDE currentInfo
    accessibleInfo'             <-  readIDE accessibleInfo
    case currentInfo' of
        Nothing             ->  case (scope,accessibleInfo') of
                                    (System,Just ai@(pm,ps))   ->
                                        let p2  =   if useBlacklist
                                                        then (Map.filter (filterBlacklist
                                                                (packageBlacklist prefs)) pm, ps)
                                                        else ai
                                            (Node _ li) = buildModulesTree
                                                                    ((Map.empty,Map.empty),p2)
                                        in liftIO $ do
                                            New.treeStoreClear treeStore
                                            mapM_ (\(e,i) -> New.treeStoreInsertTree treeStore [] i e)
                                                $ zip li [0 .. length li]
                                    _       -> liftIO $ do
                                        New.treeStoreClear treeStore
                                        New.treeStoreInsertTree treeStore [] 0 (Node ("",[]) [])
        Just (l,p)          ->  let (l',p'@(pm,ps)) =   case scope of
                                                    Local   -> (l,(Map.empty,Map.empty))
                                                    Package -> (l,p)
                                                    System   -> case accessibleInfo' of
                                                                Just ai ->  (l,ai)
                                                                Nothing ->  (l,p)
                                    p2      =   if useBlacklist
                                                    then (Map.filter (filterBlacklist
                                                            (packageBlacklist prefs)) pm, ps)
                                                    else p'
                                    (Node _ li) = buildModulesTree (l',p2)
                                in liftIO $ do
                                    emptyModel <- New.treeStoreNew []
                                    New.treeViewSetModel treeView emptyModel
                                    New.treeStoreClear treeStore
                                    mapM_ (\(e,i) -> New.treeStoreInsertTree treeStore [] i e)
                                            $ zip li [0 .. length li]
                                    New.treeViewSetModel treeView treeStore
                                    New.treeViewSetEnableSearch treeView True
                                    New.treeViewSetSearchEqualFunc treeView (Just (treeViewSearch treeView treeStore))


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
                                            $ idDescriptionsMD modDescr
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
    -> New.TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> New.TreeView
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
            item4 `onActivateLeaf` (New.treeViewExpandAll treeView)
            item5           <-  menuItemNewWithLabel "Collapse all"
            item5 `onActivateLeaf` (New.treeViewCollapseAll treeView)
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
    -> New.TreeStore Descr
    -> New.TreeView
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
            menuShellAppend theMenu item1
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
    (IDEModules _ _ treeView treeStore descrView descrStore localScopeB
        packageScopeB worldScopeB blacklistB _)  <-  getModules
    case sc of
        Local -> liftIO $ toggleButtonSetActive localScopeB True
        Package -> liftIO $ toggleButtonSetActive packageScopeB True
        System -> liftIO $ toggleButtonSetActive worldScopeB True
    liftIO $ toggleButtonSetActive blacklistB bl
    selectScope (sc,bl)

getScope :: IDEM (Scope,Bool)
getScope = do
    (IDEModules _ _ treeView treeStore descrView descrStore localScopeB
        packageScopeB worldScopeB blacklistB _)  <-  getModules
    rb1s                <-  liftIO $ toggleButtonGetActive localScopeB
    rb2s                <-  liftIO $ toggleButtonGetActive packageScopeB
    rb3s                <-  liftIO $ toggleButtonGetActive worldScopeB
    cbs                 <-  liftIO $ toggleButtonGetActive blacklistB
    let scope           =   if rb1s
                                then Local
                                else if rb2s
                                    then Package
                                    else System
    return (scope,cbs)

scopeSelection :: IDEAction
scopeSelection = do
    (sc,bl) <- getScope
    selectScope (sc,bl)

selectScope :: (Scope,Bool) -> IDEAction
selectScope (sc,bl) = do
    mods@(IDEModules _ _ treeView treeStore descrView descrStore _ _ _ _ _)
                        <-  getModules
    mbTreeSelection     <-  liftIO $ getSelectionTree treeView treeStore
    mbDescrSelection    <-  liftIO $ getSelectionDescr descrView descrStore

    ts                  <-  liftIO $ New.treeViewGetSelection treeView
    withoutRecordingDo $ do
        liftIO $ New.treeSelectionUnselectAll ts
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
    liftIO $ bringPaneToFront mods

selectNames :: (Maybe ModuleName, Maybe Symbol) -> IDEAction
selectNames (mbModuleName, mbIdName) = do
    (IDEModules _ _ treeView treeStore descrView descrStore _ _ _ _ _)
                        <-  getModules
    case mbModuleName of
        Nothing -> liftIO $ do
            sel         <-  New.treeViewGetSelection treeView
            New.treeSelectionUnselectAll  sel
            selF        <-  New.treeViewGetSelection descrView
            New.treeSelectionUnselectAll  selF
        Just moduleName ->
            let nameArray = components moduleName
            in do
                mbTree              <-  liftIO $ treeStoreGetTreeSave treeStore []
                case treePathFromNameArray mbTree nameArray [] of
                    Nothing         ->  return ()
                    Just treePath   ->  liftIO $ do
                        New.treeViewExpandToPath treeView treePath
                        sel         <-  New.treeViewGetSelection treeView
                        New.treeSelectionSelectPath sel treePath
                        col         <-  New.treeViewGetColumn treeView 0
                        New.treeViewScrollToCell treeView treePath (fromJust col)
                            (Just (0.3,0.3))
                        case mbIdName of
                            Nothing -> do
                                selF          <-  New.treeViewGetSelection descrView
                                New.treeSelectionUnselectAll  selF
                            Just symbol -> do
                                mbDescrTree   <-  treeStoreGetTreeSave descrStore []
                                selF          <-  New.treeViewGetSelection descrView
                                case  findPathFor symbol mbDescrTree of
                                    Nothing     ->  sysMessage Normal "no path found"
                                    Just path   ->  do
                                        New.treeSelectionSelectPath selF path
                                        col     <-  New.treeViewGetColumn descrView 0
                                        New.treeViewScrollToCell descrView path (fromJust col)
                                            (Just (0.3,0.3))

reloadKeepSelection :: IDEAction
reloadKeepSelection = do
    mbMod <- getPane
    case mbMod of
        Nothing -> return ()
        Just mods@(IDEModules _ _ treeView treeStore descrView descrStore _ _ _ _ _)
            -> do
            mbTreeSelection     <-  liftIO $ getSelectionTree treeView treeStore
            mbDescrSelection    <-  liftIO $ getSelectionDescr descrView descrStore
            sc                  <-  getScope
            fillModulesList sc
            liftIO $ New.treeStoreClear descrStore
            let mbs = (case mbTreeSelection of
                            Nothing -> Nothing
                            Just (_,[]) -> Nothing
                            Just (_,((md,_):_)) -> Just (modu $ moduleIdMD md),
                                 case mbDescrSelection of
                                    Nothing -> Nothing
                                    Just fw -> Just (descrName fw))
            selectNames mbs


treeStoreGetTreeSave :: New.TreeStore a -> TreePath -> IO (Maybe (Tree a))
treeStoreGetTreeSave treeStore treePath = catch (do
    res <- New.treeStoreGetTree treeStore treePath
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
        Just (pd,cabalPath) -> let srcPaths = concatMap hsSourceDirs $ allBuildInfo pd
                                   rootPath = dropFileName cabalPath
                                   modPath  = foldr (\a b -> a ++ "." ++ b) ""
                                                (map fst categories)
                               in do
            mbResp <- liftIO $ addModuleDialog modPath srcPaths
            case mbResp of
                Nothing                -> return ()
                Just (AddModule modPath srcPath isExposed) ->
                    let splitter            = split '.' modPath
                        (modPaths,[modName])= splitAt (length splitter - 1) splitter
                        targetPath          = foldl' (</>) (rootPath </> srcPath) modPaths
                        targetFile          = targetPath </> (modName ++ ".hs")
                    in do
                    liftIO $ createDirectoryIfMissing True targetPath
                    alreadyExists <- liftIO $ doesFileExist targetFile
                    if alreadyExists
                        then ideMessage Normal "File already exists"
                        else do
                            template <- liftIO $ getModuleTemplate pd modName
                            liftIO $ writeFile targetFile template
                            addModuleToPackageDescr
                                (forceJust (simpleParse modPath)
                                    ("Modules>>addModule Can't parse module name " ++ modPath))
                                isExposed
                            packageConfig
                            rebuildActiveInfo
                            trace ("now openening " ++ targetFile) $ return ()
                            fileOpenThis targetFile


-- |* Yet another stupid little dialog

data AddModule = AddModule {moduleName :: String, sourceRoot :: FilePath, isExposed :: Bool}

addModuleDialog :: String -> [String] -> IO (Maybe AddModule)
addModuleDialog modString sourceRoots = do
    dia                        <-   dialogNew
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
            (\a -> [sourceRoot a])
            (\ a b -> b{sourceRoot = forceHead a "Modules>>moduleFields"})
            (staticListEditor list),
        mkField
            (paraName <<<- ParaName ("Is this an exposed library module")
                    $ emptyParams)
            isExposed
            (\ a b -> b{isExposed = a})
            boolEditor]

-- * GUI History

data SelectionState = SelectionState {
    moduleS'        ::   Maybe ModuleName
,   facetS'         ::   Maybe String
,   scope'          ::   Scope
,   blacklist'      ::   Bool}
 deriving (Eq,Ord,Show)

recordSelHistory :: IDEAction
recordSelHistory = do
    mods <- getModules
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
    triggerEvent ideR (RecordHistory ((ModuleSelected selMod selFacet),
        (ModuleSelected (moduleS' oldSel) (facetS' oldSel))))
    liftIO $ writeIORef (oldSelection mods) (oldSel{moduleS'= selMod, facetS' = selFacet})
    return ()

replaySelHistory :: Maybe ModuleName -> Maybe Symbol -> IDEAction
replaySelHistory mbModName mbFacetName = do
    mods <- getModules
    selectNames (mbModName, mbFacetName)
    oldSel <- liftIO $ readIORef (oldSelection mods)
    liftIO $ writeIORef (oldSelection mods)
        (oldSel{moduleS'= mbModName, facetS' = mbFacetName})

recordScopeHistory :: IDEAction
recordScopeHistory = do
    (sc,bl)                 <-  getScope
    ideR                    <-  ask
    mods                    <-  getModules
    oldSel                  <-  liftIO $ readIORef (oldSelection mods)
    triggerEvent ideR (RecordHistory ((ScopeSelected sc bl),
        (ScopeSelected (scope' oldSel) (blacklist' oldSel))))
    liftIO $ writeIORef (oldSelection mods) (oldSel{scope'= sc, blacklist' = bl})
    return ()

replayScopeHistory :: Scope -> Bool -> IDEAction
replayScopeHistory sc bl = do
    mods@(IDEModules _ _ _ _ _ _ localScopeB packageScopeB worldScopeB blacklistB _)
                        <-  getModules
    liftIO $ do
        toggleButtonSetActive blacklistB bl
        toggleButtonSetActive localScopeB (sc == Local)
        toggleButtonSetActive packageScopeB (sc == Package)
        toggleButtonSetActive worldScopeB (sc == System)
    setScope (sc,bl)
    oldSel <- liftIO $ readIORef (oldSelection mods)
    liftIO $ writeIORef (oldSelection mods) (oldSel{scope'= sc, blacklist' = bl})
