-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.ClassHierarchy
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability  :  portable
--
-- | The pane of ide where modules are presented in tree form with their
--   packages and exports
--
-------------------------------------------------------------------------------

module IDE.Pane.ClassHierarchy (
    IDEClassHierarchy(..)
,   ClassHierarchyState(..)
,   showClasses
--,   showInstances
,   selectClass
--,   reloadKeepSelection
) where

import Graphics.UI.Gtk hiding (get)
import Data.Maybe
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Tree
import Data.List
import Data.Typeable
import Prelude hiding (catch)


import IDE.Core.State

-- | A modules pane description
--

data IDEClassHierarchy     =   IDEClassHierarchy {
    outer           ::   VBox
,   paned           ::   HPaned
,   treeView        ::   TreeView
,   treeStore       ::   TreeStore ClassWrapper
--,   facetView       ::   TreeView
--,   facetStore      ::   TreeStore FacetWrapper
,   localScopeB     ::   RadioButton
,   packageScopeB   ::   RadioButton
,   worldScopeB     ::   RadioButton
,   blacklistB      ::   CheckButton
} deriving Typeable

data ClassHierarchyState =   ClassHierarchyState Int (Scope,Bool)
                                    (Maybe String, Maybe String)
    deriving(Eq,Ord,Read,Show,Typeable)

instance IDEObject IDEClassHierarchy

instance Pane IDEClassHierarchy IDEM
    where
    primPaneName _  =   "ClassHierarchy"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . outer
    paneId b        =   "*ClassHierarchy"
    makeActive p    =   activatePane p []
    close           =   closePane

instance RecoverablePane IDEClassHierarchy ClassHierarchyState IDEM where
    saveState p     =   return Nothing
    recoverState pp _  =  return ()

{--
instance RecoverablePane IDEClassHierarchy ClassHierarchyState where
    saveState p     =   do
        (IDEModules _ _ treeView treeStore facetView facetStore _ _ _ _) <- getModules
        sc          <-  getScope
        mbModules   <-  getPane
        case mbModules of
            Nothing ->  return Nothing
            Just p  ->  liftIO $ do
                i   <-  panedGetPosition (paned p)
                mbTreeSelection     <-  getSelectionTree treeView treeStore
                mbFacetSelection    <-  getSelectionFacet facetView facetStore
                let mbs = (case mbTreeSelection of
                            Nothing -> Nothing
                            Just (_,[]) -> Nothing
                            Just (_,((md,_):_)) -> Just (modu $ moduleIdMD md),
                                 case mbFacetSelection of
                                    Nothing -> Nothing
                                    Just fw -> Just (symbolFromFacetWrapper fw))
                return (Just (ModulesState i sc mbs))
    recoverState pp (ModulesState i sc@(scope,useBlacklist) se)  =  do
            nb          <-  getNotebook pp
            initModules pp nb
            mod@(IDEModules _ _ treeView treeStore facetView facetStore lb pb wb blb)
                        <-  getModules
            case scope of
                Local   -> liftIO $ toggleButtonSetActive lb True
                Package -> liftIO $ toggleButtonSetActive pb True
                World   -> liftIO $ toggleButtonSetActive wb True
            liftIO $ toggleButtonSetActive blb useBlacklist
            liftIO $ panedSetPosition (paned mod) i
            fillModulesList sc
            selectNames se
--}


selectClass :: Descr -> IDEAction
selectClass d@(Descr descrName _ descrModu _ _ details) =
    case details of
        (ClassDescr _ _)-> selectClass' descrModu descrName
        _               -> return ()
selectClass _                                           = return ()

selectClass'  moduleName symbol = return ()
{--
selectClass' :: ModuleIdentifier -> Symbol -> IDEAction
selectClass'  moduleName symbol =
    let nameArray = breakAtDots [] moduleName
    in do
        mods@(IDEModules _ _ treeView treeStore facetView facetStore _ _ _ _) <- getModules
        mbTree          <-  liftIO $ treeStoreGetTreeSave treeStore []
        case treePathFromNameArray mbTree nameArray [] of
            Just treePath   ->  liftIO $ do
                treeViewExpandToPath treeView treePath
                sel         <-  treeViewGetSelection treeView
                treeSelectionSelectPath sel treePath
                col         <-  treeViewGetColumn treeView 0
                treeViewScrollToCell treeView treePath (fromJust col) (Just (0.3,0.3))
                mbFacetTree <-  treeStoreGetTreeSave facetStore []
                selF        <-  treeViewGetSelection facetView
                case  findPathFor symbol mbFacetTree of
                    Nothing     ->  sysMessage Normal "no path found"
                    Just path   ->  do
                        treeSelectionSelectPath selF path
                        col     <-  treeViewGetColumn facetView 0
                        treeViewScrollToCell facetView path (fromJust col) (Just (0.3,0.3))
                bringPaneToFront mods
            Nothing         ->  return ()
--}

showClasses :: IDEAction
showClasses = do
    m <- getClassHierarchy
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeView m)

--showInstances :: IDEAction
--showInstances = do
--    m <- getClassHierarchy
--    liftIO $ bringPaneToFront m
--    liftIO $ widgetGrabFocus (facetView m)

getClassHierarchy :: IDEM IDEClassHierarchy
getClassHierarchy = do
    mbCH <- getPane
    case mbCH of
        Nothing -> do
            pp  <- getBestPathForId  "*ClassHierarchy"
            nb  <-  getNotebook pp
            ci  <- readIDE currentInfo
            newPane pp nb (builder ci)
            mbCH <- getPane
            case mbCH of
                Nothing ->  throwIDE "Can't init class hierarchy"
                Just m  ->  return m
        Just m ->   return m

type ClassHierarchy = Forest ClassWrapper
type ClassWrapper = (Symbol, [Symbol], Descr)

--
-- | Make a Tree with a class hierarchy for display.
--
buildClassHierarchyTree :: (PackageScope,PackageScope) -> ClassHierarchy
buildClassHierarchyTree ((_,sc1),(_,sc2)) =
    let allClasses          =   nub
                                    $ filter isClassDescr
                                        $ concat (Map.elems sc1)
                                                ++ concat (Map.elems sc2)
        wrappers            =   map asClassWrapper allClasses
        (basics,other)      =   partition (\(_,sc,_) -> null sc) wrappers
        basicForest        =   map (\ n -> Node n []) basics
        resultForest       =   insertInForest basicForest other
        in sortForest resultForest
    where
        insertInForest :: ClassHierarchy -> [ClassWrapper] -> ClassHierarchy
        insertInForest basicForest [] = basicForest
        insertInForest basicForest other =
            let (newForest,rest)    =   foldl' insertInForest' (basicForest,[]) other
            in if length rest >= length other
                then throwIDE "ClassHierarchy>>buildClassHierarchyTree: Can't build tree"
                else insertInForest newForest rest

        insertInForest' :: (ClassHierarchy,[ClassWrapper]) -> ClassWrapper
                                -> (ClassHierarchy,[ClassWrapper])
        insertInForest' (forest,rest) wrap@(id,superList,idDescr) =
            let (newForest,newSuperList) = foldl' (insertInForest2 wrap)
                                            (forest, []) superList
            in if null newSuperList
                then (newForest,rest)
                else (newForest,(id,newSuperList,idDescr): rest)

        insertInForest2 :: ClassWrapper -> (ClassHierarchy,[String]) -> String
                                -> (ClassHierarchy,[String])
        insertInForest2 wrapper (forest,rest) super =
            let (newForest,success) =  foldl' (insertInTree wrapper super) ([],False) forest
            in if success
                    then (newForest,rest)
                    else (newForest, super : rest)

        insertInTree :: ClassWrapper -> String -> (ClassHierarchy,Bool)
            -> Tree ClassWrapper -> (ClassHierarchy,Bool)
        insertInTree wrapper superS (forest,bool) n@(Node w@(symbol,super,idDescr) subForest) =
            if superS == symbol
                then (Node w ((Node wrapper []) : subForest) : forest, True)
                else
                    let (newSubForest,newBool) = foldl' (insertInTree wrapper superS) ([],False)
                                                    subForest
                    in if newBool
                        then ((Node w newSubForest) : forest, True)
                        else (n: forest, bool)


isClassDescr :: Descr -> Bool
isClassDescr descr =   case details descr of
                            ClassDescr _ _  -> True
                            _               -> False
asClassWrapper :: Descr -> ClassWrapper
asClassWrapper descr   =
    case details descr of
        ClassDescr  super _ ->  (descrName descr, super, descr)
        _                   ->  throwIDE "ClassHierarchy>>asClassWrapper: No class"

instance Ord a => Ord (Tree a) where
    compare (Node l1 _) (Node l2 _) =  compare l1 l2

sortForest :: Ord a => Forest a -> Forest a
sortForest forest = sort (map sortTree forest)

sortTree :: Ord a => Tree a -> Tree a
sortTree (Node l forest)    =   Node l (sort (map sortTree forest))

builder :: Maybe (PackageScope, PackageScope) ->
    PanePath ->
    Notebook ->
    Window ->
    IDERef ->
    IO (IDEClassHierarchy, Connections)
builder currentInfo pp nb windows ideR = do
    let forest  = case currentInfo of
                    Nothing     ->  []
                    Just pair   ->  buildClassHierarchyTree pair
    treeStore   <-  treeStoreNew forest
    treeView    <-  treeViewNew
    treeViewSetModel treeView treeStore
    --treeViewSetRulesHint treeView True

    renderer0    <- cellRendererPixbufNew
    set renderer0 [ cellPixbufStockId  := "ide_no_source" ]

    renderer    <- cellRendererTextNew
    col         <- treeViewColumnNew
    treeViewColumnSetTitle col "Classes"
    treeViewColumnSetSizing col TreeViewColumnAutosize
    treeViewColumnSetResizable col True
    treeViewColumnSetReorderable col True
    treeViewAppendColumn treeView col
    cellLayoutPackStart col renderer0 False
    cellLayoutPackStart col renderer True
    cellLayoutSetAttributes col renderer treeStore
        $ \(s,_,_) -> [ cellText := s]
    cellLayoutSetAttributes col renderer0 treeStore
        $ \(_,_,d) -> [
        cellPixbufStockId  :=
            if isJust (mbLocation d)
                then "ide_source"
                else "ide_no_source"]

    treeViewSetHeadersVisible treeView True
--     treeViewSetEnableSearch treeView True
--     treeViewSetSearchColumn treeView 0
--     treeViewSetSearchEqualFunc treeView (treeViewSearch treeView treeStore)

-- Facet view
{--
    facetView   <-  treeViewNew
    facetStore  <-  treeStoreNew []
    treeViewSetModel facetView facetStore
    renderer30    <- cellRendererPixbufNew
    renderer31    <- cellRendererPixbufNew
    renderer3   <- cellRendererTextNew
    col         <- treeViewColumnNew
    treeViewColumnSetTitle col "Interface"
    --treeViewColumnSetSizing col TreeViewColumnAutosize
    treeViewAppendColumn facetView col
    cellLayoutPackStart col renderer30 False
    cellLayoutPackStart col renderer31 False
    cellLayoutPackStart col renderer3 True
    cellLayoutSetAttributes col renderer3 facetStore
        $ \row -> [ cellText := facetTreeText row]
    cellLayoutSetAttributes col renderer30 facetStore
        $ \row -> [
        cellPixbufStockId  := stockIdFromType (facetIdType row)]
    cellLayoutSetAttributes col renderer31 facetStore
        $ \row -> [
        cellPixbufStockId  := if isJust (mbLocation(facetIdDescr row))
                                then "ide_source"
                                else ""]
    treeViewSetHeadersVisible facetView True
    treeViewSetEnableSearch facetView True
    treeViewSetSearchColumn facetView 0
    treeViewSetSearchEqualFunc facetView (facetViewSearch facetView facetStore)
--}
    pane'           <-  hPanedNew
    sw              <-  scrolledWindowNew Nothing Nothing
    scrolledWindowSetShadowType sw ShadowIn
    containerAdd sw treeView
    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic

{--        sw2             <-  scrolledWindowNew Nothing Nothing
    containerAdd sw2 facetView
    scrolledWindowSetPolicy sw2 PolicyAutomatic PolicyAutomatic--}
    panedAdd1 pane' sw
--        panedAdd2 pane' sw2
    (x,y) <- widgetGetSize nb
    panedSetPosition pane' (x `quot` 2)
    box             <-  hBoxNew True 2
    rb1             <-  radioButtonNewWithLabel "Local"
    rb2             <-  radioButtonNewWithLabelFromWidget rb1 "Package"
    rb3             <-  radioButtonNewWithLabelFromWidget rb1 "World"
    toggleButtonSetActive rb3 True
    cb              <-  checkButtonNewWithLabel "Blacklist"

    boxPackStart box rb1 PackGrow 2
    boxPackStart box rb2 PackGrow 2
    boxPackStart box rb3 PackGrow 2
    boxPackEnd box cb PackNatural 2

    boxOuter        <-  vBoxNew False 2
    boxPackStart boxOuter box PackNatural 2
    boxPackStart boxOuter pane' PackGrow 2

    let classes = IDEClassHierarchy boxOuter pane' treeView treeStore
                    {--facetView facetStore--} rb1 rb2 rb3 cb
    cid3 <- treeView `onRowActivated`
        (\ treePath _ -> do
            treeViewExpandRow treeView treePath False
            return ())
    cid1 <- treeView `afterFocusIn`
        (\_ -> do reflectIDE (makeActive classes) ideR; return True)
--        cid2 <- facetView `afterFocusIn`
--            (\_ -> do runReaderT (makeActive classes) ideR; return True)
--        treeView  `onButtonPress` (treeViewPopup ideR treeStore treeView)
--        facetView `onButtonPress` (facetViewPopup ideR facetStore facetView)
--        rb1 `onToggled` (runReaderT scopeSelection ideR)
--        rb2 `onToggled` (runReaderT scopeSelection ideR)
--        rb3 `onToggled` (runReaderT scopeSelection ideR)
--        cb  `onToggled` (runReaderT scopeSelection ideR)
    sel     <-  treeViewGetSelection treeView
--        sel `onSelectionChanged` (fillFacets treeView treeStore facetView facetStore)
--        sel2    <-  treeViewGetSelection facetView
--        sel2 `onSelectionChanged` (fillInfo facetView facetStore ideR)

    return (classes,[ConnectC cid1{--,ConnectC cid2--}, ConnectC cid3])


{--
treeViewSearch :: TreeView
    -> TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> Int
    -> String
    -> TreeIter
    -> IO Bool
treeViewSearch treeView treeStore _ string iter =  do
    path <- treeModelGetPath treeStore iter
    val  <- treeStoreGetValue treeStore path
    mbTree <- treeStoreGetTreeSave treeStore path
    exp  <- treeViewRowExpanded treeView path
    when (isJust mbTree && (not (null (subForest (fromJust mbTree)))) && not exp) $
        let found = searchInModSubnodes (fromJust mbTree) string
        in when found $ do
            treeViewExpandRow treeView path False
            return ()
    let str2 = case snd val of
                    [] -> fst val
                    (m,_):_ -> showPackModule (moduleIdMD m)
    return (isInfixOf (map toLower string) (map toLower str2))

searchInModSubnodes :: ModTree -> String -> Bool
searchInModSubnodes tree str =
    not $ null
        $ filter (\ val ->
            let cstr = case snd val of
                    [] -> fst val
                    (m,_):_ -> showPackModule (moduleIdMD m)
            in  isInfixOf (map toLower str) (map toLower cstr))
                $ concatMap flatten (subForest tree)

facetViewSearch :: TreeView
    -> TreeStore FacetWrapper
    -> Int
    -> String
    -> TreeIter
    -> IO Bool
facetViewSearch facetView facetStore _ string iter = do
    path    <- treeModelGetPath facetStore iter
    val     <- treeStoreGetValue facetStore path
    tree <- treeStoreGetTree facetStore path
    exp  <- treeViewRowExpanded facetView path
    when (not (null (subForest tree)) && not exp) $
        let found = searchInFacetSubnodes tree string
        in when found $ do
            treeViewExpandRow facetView path False
            return ()
    return (isInfixOf (map toLower string) (map toLower (facetTreeText val)))

searchInFacetSubnodes :: FacetTree -> String -> Bool
searchInFacetSubnodes tree str =
    not $ null
        $ filter (\ val ->
            isInfixOf (map toLower str) (map toLower (facetTreeText val)))
                $ concatMap flatten (subForest tree)
--}

{--
fillFacets :: TreeView
    -> TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> TreeView
    -> TreeStore FacetWrapper
    -> IO ()
fillFacets treeView treeStore facetView facetStore = do
    sel             <-  getSelectionTree treeView treeStore
    case sel of
        Just val
            ->  case snd val of
                    ((mod,package):_)
                        ->  let forest = buildFacetForest mod in do
                                emptyModel <- treeStoreNew []
                                treeViewSetModel facetView emptyModel
                                treeStoreClear facetStore
                                mapM_ (\(e,i) -> treeStoreInsertTree facetStore [] i e)
                                            $ zip forest [0 .. length forest]
                                treeViewSetModel facetView facetStore
                                treeViewSetEnableSearch facetView True
                                treeViewSetSearchColumn facetView 0
                                treeViewSetSearchEqualFunc facetView (facetViewSearch facetView facetStore)

                    []  -> return ()
        Nothing
            ->  do
                    treeStoreClear facetStore
                    return ()
--}
{--
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

getSelectionFacet ::  TreeView
    ->  TreeStore FacetWrapper
    -> IO (Maybe FacetWrapper)
getSelectionFacet treeView treeStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        a:r ->  do
            val     <-  treeStoreGetValue treeStore a
            return (Just val)
        _  ->  return Nothing


fillInfo :: TreeView
    -> TreeStore FacetWrapper
    -> IDERef
    -> IO ()
fillInfo treeView lst ideR = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        []      ->  return ()
        [a]     ->  do
            wrapper     <-  treeStoreGetValue lst a
            runReaderT (setInfos [facetIdDescr wrapper]) ideR
            return ()
        _       ->  return ()

findDescription :: PackModule -> SymbolTable -> Symbol -> Maybe (Symbol,IdentifierDescr)
findDescription md st s     =
    case Map.lookup s st  of
        Nothing ->  Nothing
        Just l  ->  case filter (\id -> md == moduleIdID id) l of
                         [] -> Nothing
                         l  -> Just (s,head l)

fillModulesList :: (Scope,Bool) -> IDEAction
fillModulesList (scope,useBlacklist) = do
    (IDEModules _ _ treeView treeStore _ _ _ _ _ _)  <-  getModules
    prefs                       <-  readIDE prefs
    currentInfo'                <-  readIDE currentInfo
    accessibleInfo'             <-  readIDE accessibleInfo
    case currentInfo' of
        Nothing             ->  case (scope,accessibleInfo') of
                                    (World,Just ai@(pm,ps))   ->
                                        let p2  =   if useBlacklist
                                                        then (Map.filter (filterBlacklist
                                                                (packageBlacklist prefs)) pm, ps)
                                                        else ai
                                            (Node _ li) = buildModulesTree
                                                                    ((Map.empty,Map.empty),p2)
                                        in liftIO $ do
                                            treeStoreClear treeStore
                                            mapM_ (\(e,i) -> treeStoreInsertTree treeStore [] i e)
                                                $ zip li [0 .. length li]
                                    _       -> liftIO $ do
                                        treeStoreClear treeStore
                                        treeStoreInsertTree treeStore [] 0 (Node ("",[]) [])
        Just (l,p)          ->  let (l',p'@(pm,ps)) =   case scope of
                                                    Local   -> (l,(Map.empty,Map.empty))
                                                    Package -> (l,p)
                                                    World   -> case accessibleInfo' of
                                                                Just ai ->  (l,ai)
                                                                Nothing ->  (l,p)
                                    p2      =   if useBlacklist
                                                    then (Map.filter (filterBlacklist
                                                            (packageBlacklist prefs)) pm, ps)
                                                    else p'
                                    (Node _ li) = buildModulesTree (l',p2)
                                in liftIO $ do
                                    emptyModel <- treeStoreNew []
                                    treeViewSetModel treeView emptyModel
                                    treeStoreClear treeStore
                                    mapM_ (\(e,i) -> treeStoreInsertTree treeStore [] i e)
                                            $ zip li [0 .. length li]
                                    treeViewSetModel treeView treeStore
                                    treeViewSetEnableSearch treeView True
                                    treeViewSetSearchColumn treeView 0
                                    treeViewSetSearchEqualFunc treeView (treeViewSearch treeView treeStore)

    where
    filterBlacklist :: [Dependency] -> PackageDescr -> Bool
    filterBlacklist dependencies packageDescr =
        let packageId   =   packagePD packageDescr
            name        =   pkgName packageId
            version     =   pkgVersion packageId
        in  isNothing $ find (\ (Dependency str vr) -> str == name && withinRange version vr)
                        dependencies


type FacetForest = Forest FacetWrapper
type FacetTree = Tree FacetWrapper


facetTreeText :: FacetWrapper -> String
facetTreeText (Itself (SimpleDescr id FunctionS _ _ _ _))   =  {-- "function " ++ --} id
facetTreeText (Itself (SimpleDescr id NewtypeS _ _ _ _))    =  {-- "newtype " ++ --} id
facetTreeText (Itself (SimpleDescr id TypeS _ _ _ _))       =  {-- "type " ++ --} id
facetTreeText (Itself (SimpleDescr id _ _ _ _ _))           =  id
facetTreeText (Itself (DataDescr id _ _ _ _ _ _))           =  {-- "data " ++ --} id
facetTreeText (Itself (ClassDescr id _ _ _ _ _))            =  {-- "class " ++ --} id
facetTreeText (Itself (InstanceDescr cl _ _ _ _ ))          =  {-- "instance " ++ --} cl
facetTreeText (ConstructorW s _)                            =  {-- "constructor " ++ --} s
facetTreeText (FieldW s _)                                  =  {-- "slot " ++ --} s
facetTreeText (MethodW s _)                                 =  {-- "method " ++ --} s
facetTreeText (OrphanedData (InstanceDescr cl binds _ _ _)) =  {-- "instance " ++ --} cl
                                                                    ++ " " ++ printBinds binds
    where
        printBinds []       =   ""
        printBinds (a:[])   =   a
        printBinds (a:b)    =   a ++ " " ++ printBinds b
facetTreeText _                      =  throwIDE "impossible in facetTreeText"

facetIdType :: FacetWrapper -> IdType
facetIdType (Itself descr)                                  =  idType descr
facetIdType (ConstructorW _ _)                              =  Constructor
facetIdType (FieldW _ _)                                    =  Field
facetIdType (MethodW _ _)                                 =  Method
facetIdType (OrphanedData _)                                =  OrphanedInstance

facetIdDescr :: FacetWrapper -> IdentifierDescr
facetIdDescr (Itself descr)                                 =  descr
facetIdDescr (ConstructorW _ descr)                         =  descr
facetIdDescr (FieldW _ descr)                               =  descr
facetIdDescr (MethodW _ descr)                              =  descr
facetIdDescr (OrphanedData descr)                           =  descr

buildFacetForest ::  ModuleDescr -> FacetForest
buildFacetForest modDescr =
    let (instances,other)       =   partition (\id -> case id of
                                                        InstanceDescr _ _ _ _ _ -> True
                                                        _   -> False)
                                            $ idDescriptionsMD modDescr
        forestWithoutInstances  =   map buildFacet other
        (forest2,orphaned)      =   foldl' addInstances (forestWithoutInstances,[])
                                        instances
        orphanedNodes           =   map (\ inst -> Node (OrphanedData inst) []) orphaned
        in forest2 ++ reverse orphanedNodes
    where
    buildFacet :: IdentifierDescr -> FacetTree
    buildFacet d@(SimpleDescr _ _ _ _ _ _)
        =   Node (Itself d) []
    buildFacet d@(DataDescr _ _ _ constID fieldsID _ _)
        =   (Node (Itself d) ((map (\ s -> Node (ConstructorW s d) [])  constID)
                ++  (map (\ s -> Node (FieldW s d) [])  fieldsID)))
    buildFacet d@(ClassDescr _  _ _ classOpsID _ _)
        =   Node (Itself d) (map (\ s -> Node (MethodW s d) []) classOpsID)
    buildFacet d@(InstanceDescr _ _ _ _ _)
        =   throwIDE "Impossible in buildFacet"

    addInstances :: (FacetForest,[IdentifierDescr])
        -> IdentifierDescr
        -> (FacetForest,[IdentifierDescr])
    addInstances (forest,orphaned) instDescr =
        case foldl' (matches instDescr) ([],False) forest of
            (f,True)    -> (f,orphaned)
            (f,False)   -> (forest, instDescr:orphaned)

    matches :: IdentifierDescr
        -> (FacetForest,Bool)
        -> FacetTree
        -> (FacetForest,Bool)
    matches instDescr (forest,False) (Node (Itself dd@(DataDescr id _ _ _ _ _ _)) sub)
        | [id] == binds instDescr
            =   ((Node (Itself dd) (sub ++ [Node (Itself instDescr) []])):forest,True)
    matches instDescr (forest,False) (Node (Itself dd@(SimpleDescr id ty _ _ _ _ )) sub)
        | [id] == binds instDescr &&  ty == NewtypeS
            =   ((Node (Itself dd) (sub ++ [Node (Itself instDescr) []])):forest,True)
    matches _ (forest,b) node = (node:forest,b)



--}

{--
treeViewPopup :: IDERef
    -> TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> TreeView
    -> Event
    -> IO (Bool)
treeViewPopup ideR store treeView (Button _ click _ _ _ _ button _ _) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            menuAttachToWidget theMenu treeView
            item1           <-  menuItemNewWithLabel "Edit"
            item1 `onActivateLeaf` do
                sel         <-  getSelectionTree treeView store
                case sel of
                    Just (_,[(m,_)]) -> case mbSourcePathMD m of
                                            Nothing     ->  return ()
                                            Just fp     ->  do
                                                runReaderT (selectSourceBuf fp) ideR
                                                return ()
                    otherwise       ->  return ()
            item2           <-  menuItemNewWithLabel "ExpandAll"
            item2 `onActivateLeaf` (treeViewExpandAll treeView)
            item3           <-  menuItemNewWithLabel "CollapseAll"
            item3 `onActivateLeaf` (treeViewCollapseAll treeView)
            mapM_ (menuShellAppend theMenu) [item1,item2,item3]
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do sel         <-  getSelectionTree treeView store
                        case sel of
                            Just (_,[(m,_)]) -> case mbSourcePathMD m of
                                                    Nothing     ->  return ()
                                                    Just fp     ->  do
                                                        runReaderT (selectSourceBuf fp) ideR
                                                        return ()
                            otherwise       ->  return ()
                        return True
                else return False
treeViewPopup _ _ _ _ = throwIDE "treeViewPopup wrong event type"

facetViewPopup :: IDERef
    -> TreeStore FacetWrapper
    -> TreeView
    -> Event
    -> IO (Bool)
facetViewPopup ideR store facetView (Button _ click _ _ _ _ button _ _) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            menuAttachToWidget theMenu treeView
            item1           <-  menuItemNewWithLabel "Go to definition"
            item1 `onActivateLeaf` do
                sel         <-  getSelectionFacet facetView store
                case sel of
                    Just wrapper    ->  runReaderT
                                            (goToDefinition (facetIdDescr wrapper)) ideR
                    otherwise       ->  sysMessage Normal "no selection"
            menuShellAppend theMenu item1
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do sel         <-  getSelectionFacet facetView store
                        case sel of
                            Just wrapper  -> runReaderT (goToDefinition
                                                (facetIdDescr wrapper)) ideR
                            otherwise       ->  sysMessage Normal "no selection"
                        return True
                else do
                    mbPane :: Maybe IDEInfo <- runReaderT getPane ideR
                    when (isJust mbPane) $ bringPaneToFront (fromJust mbPane)
                    return False

facetViewPopup _ _ _ _ = throwIDE "facetViewPopup wrong event type"

--}
{--
getScope :: IDEM (Scope,Bool)
getScope = do
    (IDEModules _ _ treeView treeStore facetView facetStore localScopeB
        packageScopeB worldScopeB blacklistB)  <-  getModules
    rb1s                <-  liftIO $ toggleButtonGetActive localScopeB
    rb2s                <-  liftIO $ toggleButtonGetActive packageScopeB
    rb3s                <-  liftIO $ toggleButtonGetActive worldScopeB
    cbs                 <-  liftIO $ toggleButtonGetActive blacklistB
    let scope           =   if rb1s
                                then Local
                                else if rb2s
                                    then Package
                                    else if rb3s
                                        then World
                                        else throwIDE
                                    "ModulesPane.scopeSelection: No check button selected"
    return (scope,cbs)

scopeSelection :: IDEAction
scopeSelection = do
    mods@(IDEModules _ _ treeView treeStore facetView facetStore _ _ _ _)
                        <-  getModules
    mbTreeSelection     <-  liftIO $ getSelectionTree treeView treeStore
    mbFacetSelection    <-  liftIO $ getSelectionFacet facetView facetStore

    sc                  <-  getScope
    ts                  <-  liftIO $ treeViewGetSelection treeView
    liftIO $ treeSelectionUnselectAll ts
    fillModulesList sc
    let mbs = (case mbTreeSelection of
                            Nothing -> Nothing
                            Just (_,[]) -> Nothing
                            Just (_,((md,_):_)) -> Just (modu $ moduleIdMD md),
                                 case mbFacetSelection of
                                    Nothing -> Nothing
                                    Just fw -> Just (symbolFromFacetWrapper fw))
    selectNames mbs
    liftIO $ bringPaneToFront mods

selectNames :: (Maybe String, Maybe Symbol) -> IDEAction
selectNames (mbModuleName, mbIdName) = do
    (IDEModules _ _ treeView treeStore facetView facetStore _ _ _ _)
                        <-  getModules
    case mbModuleName of
        Nothing -> return ()
        Just moduleName ->
            let nameArray = breakAtDots [] moduleName
            in do
                mbTree              <-  liftIO $ treeStoreGetTreeSave treeStore []
                case treePathFromNameArray mbTree nameArray [] of
                    Nothing         ->  return ()
                    Just treePath   ->  liftIO $ do
                        treeViewExpandToPath treeView treePath
                        sel         <-  treeViewGetSelection treeView
                        treeSelectionSelectPath sel treePath
                        col         <-  treeViewGetColumn treeView 0
                        treeViewScrollToCell treeView treePath (fromJust col)
                            (Just (0.3,0.3))
                        case mbIdName of
                            Nothing -> return ()
                            Just symbol -> do
                                mbFacetTree   <-  treeStoreGetTreeSave facetStore []
                                selF        <-  treeViewGetSelection facetView
                                case  findPathFor symbol mbFacetTree of
                                    Nothing     ->  sysMessage Normal "no path found"
                                    Just path   ->  do
                                        treeSelectionSelectPath selF path
                                        col     <-  treeViewGetColumn facetView 0
                                        treeViewScrollToCell facetView path (fromJust col)
                                            (Just (0.3,0.3))


symbolFromFacetWrapper :: FacetWrapper -> Symbol
symbolFromFacetWrapper  (Itself idDescr)            =   identifierID idDescr
symbolFromFacetWrapper  (ConstructorW _ idDescr)    =   identifierID idDescr
symbolFromFacetWrapper  (FieldW _ idDescr)          =   identifierID idDescr
symbolFromFacetWrapper  (MethodW _ idDescr)         =   identifierID idDescr
symbolFromFacetWrapper  (OrphanedData idDescr)      =   identifierID idDescr

reloadKeepSelection :: IDEAction
reloadKeepSelection = do
    mbMod <- getPane
    case mbMod of
        Nothing -> return ()
        Just mods@(IDEModules _ _ treeView treeStore facetView facetStore _ _ _ _)
            -> do
            mbTreeSelection     <-  liftIO $ getSelectionTree treeView treeStore
            mbFacetSelection    <-  liftIO $ getSelectionFacet facetView facetStore
            sc                  <-  getScope
            fillModulesList sc
            liftIO $ treeStoreClear facetStore
            let mbs = (case mbTreeSelection of
                            Nothing -> Nothing
                            Just (_,[]) -> Nothing
                            Just (_,((md,_):_)) -> Just (modu $ moduleIdMD md),
                                 case mbFacetSelection of
                                    Nothing -> Nothing
                                    Just fw -> Just (symbolFromFacetWrapper fw))
            selectNames mbs


treeStoreGetTreeSave :: TreeStore a -> TreePath -> IO (Maybe (Tree a))
treeStoreGetTreeSave treeStore treePath = catch (do
    res <- treeStoreGetTree treeStore treePath
    return (Just res)) (\ _ -> return Nothing)


findPathFor :: Symbol -> Maybe (Tree FacetWrapper) -> Maybe TreePath
findPathFor symbol (Just (Node _ forest)) =
    foldr ( \i mbTreePath -> findPathFor' [i] (forest !! i) mbTreePath)
                            Nothing  [0 .. ((length forest) - 1)]
    where
    findPathFor' :: TreePath -> Tree FacetWrapper -> Maybe TreePath -> Maybe TreePath
    findPathFor' _ node (Just p)                  =   Just p
    findPathFor' path (Node wrap sub) Nothing     =
        if identifierID (facetIdDescr wrap) == symbol
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

--}
{--
extractSuperclasses :: String -> [String]
extractSuperclasses str =
    let parseRes = trace ("now extracting superclasses for " ++ show str)
                    parse superclassParser "" str
    in case parseRes of
            Left err    ->  throwIDE $show err
            Right l     ->  trace ("found " ++ show l) l

lexer = haskell
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
symbol = P.symbol lexer

superclassParser :: CharParser () [String]
superclassParser = do
    symbol "class"
    whiteSpace
    try (do
            sc <- classDefParser
            symbol "=>"
            return [sc])
    <|> try (do
            symbol "("
            scs <- sepBy classDefParser (char ',')
            symbol ")"
            symbol "=>"
            return scs)
    <|> return []
    <?> "superclasses"

classDefParser :: CharParser () String
classDefParser = do
    whiteSpace
    c <- oneOf['A'..'Z']
    cs <- many (alphaNum <|> oneOf "_'.")
    many typeVarParser
    return (c:cs)
    <?> "classDef"

typeVarParser :: CharParser () String
typeVarParser = do
    whiteSpace
    c <- oneOf['a'..'z']
    cs <- many (alphaNum <|> oneOf "_'.")
    return (c:cs)
    <?> "typeVar"
--}
