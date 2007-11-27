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


import Ghf.Core.State
import Ghf.ViewFrame
import Ghf.InfoPane
import Ghf.SourceModel

instance Pane GhfModules
    where
    primPaneName _  =   "Mod"
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
        --New.treeViewSetEnableSearch treeView True
        --New.treeViewSetRulesHint treeView True

        renderer    <- New.cellRendererTextNew
        col         <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col "Modules"
        New.treeViewColumnSetSizing col TreeViewColumnAutosize
        New.treeViewColumnSetReorderable col True
        New.treeViewAppendColumn treeView col
        New.cellLayoutPackStart col renderer True
        New.cellLayoutSetAttributes col renderer treeStore
            $ \row -> [ New.cellText := fst row]

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

        renderer3   <- New.cellRendererTextNew
        col         <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col "Identifiers"
        --New.treeViewColumnSetSizing col TreeViewColumnAutosize
        -- New.treeViewColumnSetReorderable col True
        New.treeViewAppendColumn facetView col
        New.cellLayoutPackStart col renderer True
        New.cellLayoutSetAttributes col renderer facetStore
            $ \row -> [ New.cellText := fst row]

        New.treeViewSetHeadersVisible treeView True
        sel         <-  New.treeViewGetSelection treeView
        sel `New.onSelectionChanged` (fillFacets treeView treeStore facetStore)
        treeView `onButtonPress` (treeViewPopup ghfR treeStore treeView)

        sel2        <-  New.treeViewGetSelection facetView
        sel2 `New.onSelectionChanged` (fillInfo facetView facetStore ghfR)

        pane <- hPanedNew
        sw <- scrolledWindowNew Nothing Nothing
        scrolledWindowAddWithViewport sw treeView
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        sw2 <- scrolledWindowNew Nothing Nothing
        scrolledWindowAddWithViewport sw2 facetView
        scrolledWindowSetPolicy sw2 PolicyAutomatic PolicyAutomatic
        panedAdd1 pane sw
        panedAdd2 pane sw2
        (x,y) <- widgetGetSize nb
        panedSetPosition pane (x `quot` 2)
        let modules = GhfModules pane treeStore facetStore
        notebookPrependPage nb pane (paneName modules)
        widgetShowAll pane
        mbPn <- notebookPageNum nb pane
        case mbPn of
            Just i -> notebookSetCurrentPage nb i
            Nothing -> putStrLn "Notebook page not found"
        cid1 <- treeView `afterFocusIn`
            (\_ -> do runReaderT (makeActive modules) ghfR; return True)
        cid2 <- facetView `afterFocusIn`
            (\_ -> do runReaderT (makeActive modules) ghfR; return True)
        return (modules,[cid1,cid2])
    addPaneAdmin buf (BufConnections [] [] []) panePath
    lift $widgetGrabFocus (paned buf)

fillFacets :: New.TreeView
    -> New.TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> New.ListStore (String, IdentifierDescr)
    -> IO ()
fillFacets treeView tst lst = do
    sel             <-  getSelection treeView tst
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
                            putStrLn $ "Now fill " ++ show (length pairs)
                            mapM_ (New.listStoreAppend lst) pairs
                        []  -> return ()
        Nothing -> return ()

getSelection ::  New.TreeView
    ->  New.TreeStore (String, [(ModuleDescr,PackageDescr)])
    -> IO (Maybe (String, [(ModuleDescr,PackageDescr)]))
getSelection treeView treeStore = do
    treeSelection   <-  New.treeViewGetSelection treeView
    paths           <-  New.treeSelectionGetSelectedRows treeSelection
    case paths of
        []  ->  return Nothing
        a:r ->  do
            val     <-  New.treeStoreGetValue treeStore a
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
        Just l  ->  case filter (\id -> elem md $ moduleIdID id) l of
                         [] -> Nothing
                         l  -> Just (s,head l)

fillModulesList :: GhfAction
fillModulesList = do
    (GhfModules _ treeStore _)  <-  getModules
    currentInfo                 <-  readGhf currentInfo
    case currentInfo of
        Nothing             ->  lift $ do
                                    New.treeStoreClear treeStore
        Just pair           ->  let (Node _ li) = buildModulesTree pair
                                in lift $ do
                                    New.treeStoreClear treeStore
                                    mapM_ (\(e,i) -> New.treeStoreInsertTree treeStore [] i e)
                                        $ zip li [0 .. length li]

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
    where
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
treeViewPopup ghfR store treeView (Button _ _ _ _ _ _ button _ _) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Edit"
            item1 `onActivateLeaf` do
                sel         <-  getSelection treeView store
                case sel of
                    Just (_,[(m,_)]) -> case mbSourcePathMD m of
                                            Nothing     ->  return ()
                                            Just fp     ->  do
                                                runReaderT (selectSourceBuf fp) ghfR
                                                return ()
                    otherwise       ->  return ()
            menuShellAppend theMenu item1
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else return False
treeViewPopup _ _ _ _ = error "treeViewPopup wrong event type"

