{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Ghf.ModulesPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
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
import Data.Tree
import Data.List


import Ghf.Core
import Ghf.ViewFrame

instance Pane GhfModules
    where
    primPaneName _  =   "Mod"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . boxM
    paneId b        =   "*Modules"

instance Castable GhfModules where
    casting _               =   ModulesCasting
    downCast _ (PaneC a)    =   case casting a of
                                    ModulesCasting  -> Just a
                                    _               -> Nothing

showModules :: GhfAction
showModules = do
    m <- getModules
    lift $ bringPaneToFront m

getModules :: GhfM GhfModules
getModules = do
    panesST     <-  readGhf panes
    prefs       <-  readGhf prefs
    layout      <-  readGhf layout
    let mods    =   catMaybes $ map (downCast ModulesCasting) $ Map.elems panesST
    if null mods || length mods > 1
        then do
            let pp      =   getStandardPanePath (modulesPanePath prefs) layout
            nb          <-  getNotebook pp
            initModules pp nb
            panesST     <- readGhf panes
            let mods    =   catMaybes $ map (downCast ModulesCasting) $ Map.elems panesST
            if null mods || length mods > 1
                then error "Can't init modules"
                else return (head mods)
        else return (head mods)

initModules :: PanePath -> Notebook -> GhfAction
initModules panePath nb = do
    lift $ putStrLn "now init modules"
    ghfR        <-  ask
    panes       <-  readGhf panes
    paneMap     <-  readGhf paneMap
    prefs       <-  readGhf prefs
    (buf,cids)  <-  lift $ do
        treeView    <-  New.treeViewNew
        treeStore   <-  New.treeStoreNew []
        New.treeViewSetModel treeView treeStore
        facetView   <-  New.treeViewNew
        facetStore  <-  New.listStoreNew []
        New.treeViewSetModel facetView facetStore
        box <- hBoxNew False 0
        boxPackStart box treeView PackGrow 2
        boxPackStart box facetView PackGrow 2

        let modules = GhfModules box treeStore facetStore
        notebookPrependPage nb box (paneName modules)
        widgetShowAll box
        mbPn <- notebookPageNum nb box
        case mbPn of
            Just i -> notebookSetCurrentPage nb i
            Nothing -> putStrLn "Notebook page not found"
        cid1 <- box `afterFocusIn`
            (\_ -> do runReaderT (makeModulesActive modules) ghfR; return True)
        return (modules,[cid1])
    let newPaneMap  =  Map.insert (paneName buf)
                            (panePath, BufConnections [] [] []) paneMap
    let newPanes = Map.insert (paneName buf) (PaneC buf) panes
    modifyGhf_ (\ghf -> return (ghf{panes = newPanes,
                                    paneMap = newPaneMap}))
    lift $widgetGrabFocus (boxM buf)

makeModulesActive :: GhfModules -> GhfAction
makeModulesActive mods = do
    activatePane mods (BufConnections[][][])

type ModTree = Tree (String, Maybe (ModuleDescr,PackageDescr))

fillModulesPane :: (PackageScope,PackageScope) -> ModTree
fillModulesPane ((localMap,_),(otherMap)) =
    let modules     =   concatMap (\e -> map (\f -> (f,e)) $ exposedModulesPD e)
                        $ Map.elems localMap
    in  foldr foldf (Node ("",Nothing) []) modules
    where
        foldf :: (ModuleDescr,PackageDescr) -> ModTree -> ModTree
        foldf (md,pd) fo =
            let nameArray   =   breakAtDots [] $ moduleIdMD md
            in  repNode fo nameArray
        breakAtDots :: [String] -> String -> [String]
        breakAtDots res []          =   res
        breakAtDots res toBreak     =   let (newRes,newToBreak) = span (\c -> c /= '.') toBreak
                                        in  breakAtDots (newRes : res) newToBreak
        repNode :: ModTree -> [String] -> ModTree
        repNode node []     =   node
        repNode node list   =   case find (\e -> fst (rootLabel e) == head list)
                                        $ subForest node of
                                    Nothing     -> (Node (rootLabel node)
                                                         (head (makeNodes list)
                                                            : (subForest node)))
                                    Just node2  -> (Node (rootLabel node)
                                                        ((deleteBy (\(e,_) -> e == node2)
                                                            (subForest node))
                                                            : repNode node list))
        makeNodes :: [String] -> Forest (String, Maybe (ModuleDescr,PackageDescr))
        makeNodes [hd]      =    []
        makeNodes (hd:tl)   =    [Node hd (makeNodes tl)]



