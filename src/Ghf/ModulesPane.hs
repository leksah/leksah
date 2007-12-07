{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Ghf.ModulesPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability  :  portable
--
-- | The pane of ghf where modules are presented in tree form with their
--   packages and exports
--
-------------------------------------------------------------------------------

module Ghf.ModulesPane (
    showModules
,   gotoModule
) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.ModelView as New
import System.Glib.Signals
import Data.Maybe
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree
import Data.List
import Distribution.Package
import Distribution.PackageDescription
import System.Glib.GObject
import Data.Char (toLower)

import Ghf.Core.State
import Ghf.ViewFrame
import Ghf.InfoPane
import Ghf.SourceEditor

instance Pane GhfModules
    where
    primPaneName _  =   "Modules"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . paned
    paneId b        =   "*Modules"
    makeActive p    = do
        activatePane p (BufConnections[][] [])
    close pane     =   do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  lift $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  lift $ do
                putStrLn "notebook page not found: unexpected"
                return ()
            Just i  ->  do
                deactivatePaneIfActive pane
                lift $notebookRemovePage nb i
                removePaneAdmin pane

instance ModelPane GhfModules ModulesState where
    saveState p     =   do
        mbModules <- getPane ModulesCasting
        case mbModules of
            Nothing ->  return Nothing
            Just p  ->  lift $ do
                i <- panedGetPosition (paned p)
                return (Just (StateC (ModulesState i)))
    recoverState pp (ModulesState i)  =  do
            nb          <-  getNotebook pp
            initModules pp nb
            mbMod <- getPane ModulesCasting
            case mbMod of
                Nothing -> return ()
                Just p  -> do   lift $ panedSetPosition (paned p) i
                                fillModulesList
                                return ()

gotoModule :: IdentifierDescr -> GhfAction
gotoModule idDescr =
    let moduleName = modu $ moduleIdID idDescr
        nameArray = breakAtDots [] moduleName
    in do
        message "goto Module called"
        mods@(GhfModules _ treeView treeStore _) <- getModules
        tree            <-  lift $ New.treeStoreGetTree treeStore []
        let mbTreePath  =   treePathFromNameArray tree nameArray []
        lift $ putStrLn (show mbTreePath)
        case mbTreePath of
            Just treePath   ->  lift $ do
                New.treeViewExpandToPath treeView treePath
                sel         <-  New.treeViewGetSelection treeView
                New.treeSelectionSelectPath sel treePath
                col         <- New.treeViewGetColumn treeView 0
                New.treeViewScrollToCell treeView treePath (fromJust col) (Just (0.3,0.3))
                bringPaneToFront mods
            Nothing         ->  return ()

treePathFromNameArray :: ModTree -> [String] -> [Int] -> Maybe [Int]
treePathFromNameArray tree [] accu      =   Just (reverse accu)
treePathFromNameArray tree (h:t) accu   =
    let names   =   map (\t -> fst $ rootLabel t) (subForest tree)
        mbIdx   =   elemIndex h names
    in case mbIdx of
            Nothing ->  Nothing
            Just i  ->  treePathFromNameArray (subForest tree !! i) t (i:accu)

showModules :: GhfAction
showModules = do
    fillModulesList
    m <- getModules
    lift $ bringPaneToFront m

getModules :: GhfM GhfModules
getModules = do
    mbMod <- getPane ModulesCasting
    case mbMod of
        Nothing -> do
            prefs       <-  readGhf prefs
            layout      <-  readGhf layout
            let pp      =   getStandardPanePath (modulesPanePath prefs) layout
            nb          <-  getNotebook pp
            initModules pp nb
            mbMod <- getPane ModulesCasting
            case mbMod of
                Nothing ->  error "Can't init modules"
                Just m  ->  return m
        Just m ->   return m

initModules :: PanePath -> Notebook -> GhfAction
initModules panePath nb = do
    lift $ putStrLn "now init modules"
    ghfR        <-  ask
    panes       <-  readGhf panes
    paneMap     <-  readGhf paneMap
    prefs       <-  readGhf prefs
    currentInfo <-  readGhf currentInfo
    (buf,cids)  <-  lift $ do
        let forest  = case currentInfo of
                        Nothing     ->  []
                        Just pair   ->  subForest (buildModulesTree pair)
        treeStore   <-  New.treeStoreNew forest
        treeView    <-  New.treeViewNew
        New.treeViewSetModel treeView treeStore
        New.treeViewSetEnableSearch treeView True
        New.treeViewSetSearchColumn treeView 0
        New.treeViewSetSearchEqualFunc treeView (treeViewSearch treeView treeStore)

        --New.treeViewSetRulesHint treeView True

        renderer0    <- New.cellRendererPixbufNew
        set renderer0 [ cellPixbufStockId  := stockYes ]

        renderer    <- New.cellRendererTextNew
        col         <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col "Modules"
        New.treeViewColumnSetSizing col TreeViewColumnAutosize
        New.treeViewColumnSetReorderable col True
        New.treeViewAppendColumn treeView col
        New.cellLayoutPackStart col renderer0 False
        New.cellLayoutPackStart col renderer True
        New.cellLayoutSetAttributes col renderer treeStore
            $ \row -> [ New.cellText := fst row]
        New.cellLayoutSetAttributes col renderer0 treeStore
            $ \row -> [
            cellPixbufStockId  :=
                if null (snd row)
                    then ""
                    else if isJust (mbSourcePathMD (fst (head (snd row))))
                            then stockJumpTo
                            else stockYes]

        renderer2   <- New.cellRendererTextNew
        col2        <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col2 "Packages"
        New.treeViewColumnSetSizing col2 TreeViewColumnAutosize
        New.treeViewColumnSetReorderable col2 True
        New.treeViewAppendColumn treeView col2
        New.cellLayoutPackStart col2 renderer2 True
        New.cellLayoutSetAttributes col2 renderer2 treeStore
            $ \row -> [ New.cellText :=
                concat
                    $ intersperse  ", "
                        $ map (showPackageId . packagePD . snd) (snd row)]

        facetView   <-  New.treeViewNew
        facetStore  <-  New.listStoreNew []
        New.treeViewSetModel facetView facetStore
        New.treeViewSetEnableSearch facetView True
        New.treeViewSetSearchColumn facetView 0
        New.treeViewSetSearchEqualFunc facetView
            (\ _ string iter -> do
                [ind]   <- New.treeModelGetPath facetStore iter
                val     <- New.listStoreGetValue facetStore ind
                return (isInfixOf (map toLower string) (map toLower (fst val))))

        renderer30    <- New.cellRendererPixbufNew
        renderer31    <- New.cellRendererPixbufNew
        renderer3   <- New.cellRendererTextNew
        col         <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col "Identifiers"
        --New.treeViewColumnSetSizing col TreeViewColumnAutosize
        -- New.treeViewColumnSetReorderable col True
        New.treeViewAppendColumn facetView col
        New.cellLayoutPackStart col renderer30 False
        New.cellLayoutPackStart col renderer31 False
        New.cellLayoutPackStart col renderer3 True
        New.cellLayoutSetAttributes col renderer3 facetStore
            $ \row -> [ New.cellText := fst row]
        New.cellLayoutSetAttributes col renderer30 facetStore
            $ \row -> [
            cellPixbufStockId  := stockIdFromType (identifierTypeID(snd row))]
        New.cellLayoutSetAttributes col renderer31 facetStore
            $ \row -> [
            cellPixbufStockId  := if isJust (mbLocation(snd row))
                                    then stockJumpTo
                                    else ""]
        New.treeViewSetHeadersVisible treeView True

        pane <- hPanedNew
        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw treeView
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        sw2 <- scrolledWindowNew Nothing Nothing
        containerAdd sw2 facetView
        scrolledWindowSetPolicy sw2 PolicyAutomatic PolicyAutomatic
        panedAdd1 pane sw
        panedAdd2 pane sw2
        (x,y) <- widgetGetSize nb
        panedSetPosition pane (x `quot` 2)
        let modules = GhfModules pane treeView treeStore facetStore
        notebookPrependPage nb pane (paneName modules)
        widgetShowAll pane
        mbPn <- notebookPageNum nb pane
        case mbPn of
            Just i  -> notebookSetCurrentPage nb i
            Nothing -> putStrLn "Notebook page not found"
        cid0 <- treeView `New.onStartInteractiveSearch`
            (do putStrLn "onStartInteractiveSearchNew"
                New.treeViewExpandAll treeView)
        cid3 <- treeView `New.onRowActivated`
            (\ treePath _ -> do
                New.treeViewExpandRow treeView treePath False
                return ())
        cid1 <- treeView `afterFocusIn`
            (\_ -> do runReaderT (makeActive modules) ghfR; return True)
        cid2 <- facetView `afterFocusIn`
            (\_ -> do runReaderT (makeActive modules) ghfR; return True)
        treeView  `onButtonPress` (treeViewPopup ghfR treeStore treeView)
        facetView `onButtonPress` (facetViewPopup ghfR facetStore facetView)
        sel     <-  New.treeViewGetSelection treeView
        sel `New.onSelectionChanged` (fillFacets treeView treeStore facetStore)
        sel2    <-  New.treeViewGetSelection facetView
        sel2 `New.onSelectionChanged` (fillInfo facetView facetStore ghfR)

        return (modules,[cid1,cid2])
    addPaneAdmin buf (BufConnections [] [] []) panePath
    lift $widgetGrabFocus (paned buf)

stockIdFromType :: IdType -> StockId
stockIdFromType Function    =   stockGoForward
stockIdFromType _           =   ""

treeViewSearch :: TreeView
    -> New.TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> Int
    -> String
    -> TreeIter
    -> IO Bool
treeViewSearch treeView treeStore _ string iter =  do
    path <- New.treeModelGetPath treeStore iter
    val  <- New.treeStoreGetValue treeStore path
    tree <- New.treeStoreGetTree treeStore path
    exp  <- New.treeViewRowExpanded treeView path
    when (not (null (subForest tree)) && not exp) $
        let found = searchInSubnodes tree string
        in when found $ do
            New.treeViewExpandRow treeView path False
            return ()
    let str2 = case snd val of
                    [] -> fst val
                    (m,_):_ -> showPackModule (moduleIdMD m)
    return (isInfixOf (map toLower string) (map toLower str2))

searchInSubnodes :: ModTree -> String -> Bool
searchInSubnodes tree str =
    not $ null
        $ filter (\ val ->
            let cstr = case snd val of
                    [] -> fst val
                    (m,_):_ -> showPackModule (moduleIdMD m)
            in  isInfixOf (map toLower str) (map toLower cstr))
                $ concatMap flatten (subForest tree)

fillFacets :: New.TreeView
    -> New.TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> New.ListStore (String, IdentifierDescr)
    -> IO ()
fillFacets treeView tst lst = do
    sel             <-  getSelectionTree treeView tst
    case sel of
        Just val -> case snd val of
                        ((mod,package):_)   ->  do
                            let exportedDescr = exportedNamesMD mod
                            let pairs = map fromJust
                                                $ filter isJust
                                                    $ map (findDescription
                                                            (moduleIdMD mod)
           	                                                 (idDescriptionsPD package))
                                                        (Set.toList exportedDescr)
                            New.listStoreClear lst
                            --putStrLn $ "Now fill " ++ show (length pairs)
                            mapM_ (New.listStoreAppend lst) pairs
                        []  -> return ()
        Nothing -> return ()

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

getSelectionFacet ::  New.TreeView
    ->  New.ListStore (String, IdentifierDescr)
    -> IO (Maybe (String, IdentifierDescr))
getSelectionFacet treeView listStore = do
    treeSelection   <-  New.treeViewGetSelection treeView
    paths           <-  New.treeSelectionGetSelectedRows treeSelection
    case paths of
        []  ->  return Nothing
        [a]:r ->  do
            val     <-  New.listStoreGetValue listStore a
            return (Just val)

fillInfo :: New.TreeView
    -> New.ListStore (String, IdentifierDescr)
    -> GhfRef
    -> IO ()
fillInfo treeView lst ghfR = do
    treeSelection   <-  New.treeViewGetSelection treeView
    paths           <-  New.treeSelectionGetSelectedRows treeSelection
    case paths of
        []      ->  return ()
        [[a]]   ->  do
            (_,id)     <-  New.listStoreGetValue lst a
            runReaderT (setInfo id) ghfR
            return ()
        _       ->  return ()

findDescription :: PackModule -> SymbolTable -> Symbol -> Maybe (Symbol,IdentifierDescr)
findDescription md st s     =
    case Map.lookup s st  of
        Nothing ->  Nothing
        Just l  ->  case filter (\id -> md == moduleIdID id) l of
                         [] -> Nothing
                         l  -> Just (s,head l)

fillModulesList :: GhfAction
fillModulesList = do
    (GhfModules _ treeView treeStore _)  <-  getModules
    currentInfo                 <-  readGhf currentInfo
    case currentInfo of
        Nothing             ->  lift $ do
                                    New.treeStoreClear treeStore
        Just pair           ->  let (Node _ li) = buildModulesTree pair
                                in lift $ do
                                    New.treeStoreClear treeStore
                                    mapM_ (\(e,i) -> New.treeStoreInsertTree treeStore [] i e)
                                        $ zip li [0 .. length li]
                                    --New.treeViewExpandAll treeView

type ModTree = Tree (String, [(ModuleDescr,PackageDescr)])

--
-- | Make a Tree with a module desription, package description pairs tree to display.
--   Their are nodes with a label but without a module (like e.g. Data).
--
buildModulesTree :: (PackageScope,PackageScope) -> ModTree
buildModulesTree ((localMap,_),(otherMap,_)) =
    let flatPairs           =   concatMap (\e -> map (\f -> (f,e)) (exposedModulesPD e))
                                    (Map.elems localMap ++ Map.elems otherMap)
        emptyTree           =   (Node ("",[]) [])
        resultTree          =   foldl insertPairsInTree emptyTree flatPairs
        in sortTree resultTree

insertPairsInTree :: ModTree -> (ModuleDescr,PackageDescr) -> ModTree
insertPairsInTree tree pair =
    let nameArray           =   breakAtDots [] $ modu $ moduleIdMD $ fst pair
        pairedWith          =   map (\n -> (n,pair)) nameArray
    in  insertNodesInTree pairedWith tree

breakAtDots :: [String] -> String -> [String]
breakAtDots res []          =   reverse res
breakAtDots res toBreak     =   let (newRes,newToBreak) = span (\c -> c /= '.') toBreak
                                in  if null newToBreak
                                        then reverse (newRes : res)
                                        else breakAtDots (newRes : res) (tail newToBreak)

insertNodesInTree :: [(String,(ModuleDescr,PackageDescr))] -> ModTree -> ModTree
insertNodesInTree list@[(str2,pair)] (Node (str1,pairs) forest) =
    case partition (\ (Node (s,_) _) -> s == str2) forest of
        ([],_)              ->  (Node (str1,pairs) (makeNodes list : forest))
        ([(Node (_,pairsf) l)],rest)
                            ->  (Node (str1,pairs) ((Node (str2,pair : pairsf) l) : rest))
        (_,_)               ->  error "insertNodesInTree: impossible1"
insertNodesInTree  list@((str2,pair):tl) (Node (str1,pairs) forest) =
    case partition (\ (Node (s,_) _) -> s == str2) forest of
        ([],_)              ->  (Node (str1,pairs)  (makeNodes list : forest))
        ([found],rest)      ->  (Node (str1,pairs) (insertNodesInTree tl found : rest))
        (_,_)               ->  error "insertNodesInTree: impossible2"
insertNodesInTree [] t      =   t

makeNodes :: [(String,(ModuleDescr,PackageDescr))] -> ModTree
makeNodes [(str,pair)]      =   Node (str,[pair]) []
makeNodes ((str,_):tl)      =   Node (str,[]) [makeNodes tl]

instance Ord a => Ord (Tree a) where
    compare (Node l1 _) (Node l2 _) =  compare l1 l2

sortTree :: Ord a => Tree a -> Tree a
sortTree (Node l forest)    =   Node l (sort (map sortTree forest))

treeViewPopup :: GhfRef
    -> New.TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> New.TreeView
    -> Event
    -> IO (Bool)
treeViewPopup ghfR store treeView (Button _ click _ _ _ _ button _ _) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Edit"
            item1 `onActivateLeaf` do
                sel         <-  getSelectionTree treeView store
                case sel of
                    Just (_,[(m,_)]) -> case mbSourcePathMD m of
                                            Nothing     ->  return ()
                                            Just fp     ->  do
                                                runReaderT (selectSourceBuf fp) ghfR
                                                return ()
                    otherwise       ->  return ()
            item2           <-  menuItemNewWithLabel "ExpandAll"
            item2 `onActivateLeaf` (New.treeViewExpandAll treeView)
            item3           <-  menuItemNewWithLabel "CollapseAll"
            item3 `onActivateLeaf` (New.treeViewCollapseAll treeView)
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
                                                        runReaderT (selectSourceBuf fp) ghfR
                                                        return ()
                            otherwise       ->  return ()
                        return True
                else return False
treeViewPopup _ _ _ _ = error "treeViewPopup wrong event type"

facetViewPopup :: GhfRef
    -> New.ListStore (String, IdentifierDescr)
    -> New.TreeView
    -> Event
    -> IO (Bool)
facetViewPopup ghfR store facetView (Button _ click _ _ _ _ button _ _) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Go to definition"
            item1 `onActivateLeaf` do
                sel         <-  getSelectionFacet facetView store
                case sel of
                    Just (_,descr)  -> runReaderT (goToDefinition descr) ghfR
                    otherwise       ->  trace "no selection" $ return ()
            menuShellAppend theMenu item1
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do sel         <-  getSelectionFacet facetView store
                        case sel of
                            Just (_,descr)  -> runReaderT (goToDefinition descr) ghfR
                            otherwise       ->  trace "no selection" $ return ()
                        return True
                else return False
facetViewPopup _ _ _ _ = error "facetViewPopup wrong event type"




