{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.CallersPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability  :  portable
--
-- | The pane of ide where modules are presented in tree form with their
--   packages and exports
--
-------------------------------------------------------------------------------

module IDE.CallersPane (
    calledBy
) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.ModelView as New
import Data.Maybe
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Distribution.Package

import IDE.Core.State
import IDE.Framework.ViewFrame

instance Pane IDECallers
    where
    primPaneName _  =   "Callers"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Callers"
    makeActive p    = do
        activatePane p (BufConnections[][] [])
    close pane     =   do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  lift $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  lift $ do
                sysMessage Normal "notebook page not found: unexpected"
                return ()
            Just i  ->  do
                deactivatePaneIfActive pane
                lift $notebookRemovePage nb i
                removePaneAdmin pane

-- | We don't recover this pane
instance RecoverablePane IDECallers CallersState where
    saveState p     =   return Nothing
    recoverState pp _  =  return ()


-- | Open a pane with the callers of this identifier
calledBy :: IdentifierDescr
    -> IDEAction
calledBy idDescr = do
    mbCurrentInfo   <- readIDE currentInfo
    case mbCurrentInfo of
        Nothing             ->  return ()
        Just currentInfo'   ->
            let symModPair      =   (identifierID idDescr, modu $ moduleIdID idDescr)
                modulesList1    =   modulesForCallerFromPackages
                                        (Map.elems $ fst $ fst currentInfo') symModPair
                modulesList2    =   modulesForCallerFromPackages
                                        (Map.elems $ fst $ snd currentInfo') symModPair
                modulesList     =   nub $ modulesList1 ++ modulesList2
                finalList       =   zip modulesList (repeat (identifierID idDescr))
            in do   callers             <-  getCallers
                    lift $ do
                    New.listStoreClear (callersStore callers)
                    mapM_ (New.listStoreAppend (callersStore callers)) finalList
                    bringPaneToFront callers

modulesForCallerFromPackages :: [PackageDescr] -> (Symbol,ModuleIdentifier) -> [ModuleDescr]
modulesForCallerFromPackages []        _            =  []
modulesForCallerFromPackages (p :rest) (sym,mod)    =
    (filter (\ md -> case mod `Map.lookup` (usagesMD md) of
                        Nothing     -> False
                        Just syms   -> sym `Set.member` syms) (exposedModulesPD p))
        ++ modulesForCallerFromPackages rest (sym,mod)

getCallers :: IDEM IDECallers
getCallers = do
    mbMod <- getPane CallersCasting
    case mbMod of
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (modulesPanePath prefs) layout
            nb          <-  getNotebook pp
            initCallers pp nb
            mbMod <- getPane CallersCasting
            case mbMod of
                Nothing ->  throwIDE "Can't init callers"
                Just m  ->  return m
        Just m ->   return m

initCallers :: PanePath -> Notebook -> IDEAction
initCallers panePath nb = do
    ideR        <-  ask
    panes       <-  readIDE panes
    paneMap     <-  readIDE paneMap
    prefs       <-  readIDE prefs
    currentInfo <-  readIDE currentInfo
    (buf,cids)  <-  lift $ do
        treeStore   <-  New.listStoreNew []
        treeView    <-  New.treeViewNew
        New.treeViewSetModel treeView treeStore

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
            $ \row -> [ New.cellText := modu $ moduleIdMD $ fst row]
        New.cellLayoutSetAttributes col renderer0 treeStore
            $ \row -> [cellPixbufStockId  :=
                        if isJust (mbSourcePathMD $ fst row)
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
            $ \row -> [ New.cellText := showPackageId $ pack $ moduleIdMD $ fst row]

        New.treeViewSetHeadersVisible treeView True

        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw treeView
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        let modules = IDECallers sw treeView treeStore
        notebookInsertOrdered nb sw (paneName modules)
        widgetShowAll sw

--        cid1 <- treeView `afterFocusIn`
--            (\_ -> do runReaderT (makeActive modules) ideR; return True)
--        cid2 <- facetView `afterFocusIn`
--            (\_ -> do runReaderT (makeActive modules) ideR; return True)
--        treeView `onButtonPress` (treeViewPopup ideR treeStore treeView)
--        facetView `onButtonPress` (facetViewPopup ideR facetStore facetView)
--        sel         <-  New.treeViewGetSelection treeView
--        sel `New.onSelectionChanged` (fillFacets treeView treeStore facetStore)
--        sel2        <-  New.treeViewGetSelection facetView
--        sel2 `New.onSelectionChanged` (fillInfo facetView facetStore ideR)

        return (modules,[])
    addPaneAdmin buf (BufConnections [] [] []) panePath
    lift $widgetGrabFocus (scrolledView buf)


--getSelectionTree ::  New.TreeView
--    ->  New.ListStore (String, [(ModuleDescr,PackageDescr)])
--    -> IO (Maybe (String, [(ModuleDescr,PackageDescr)]))
--getSelectionTree treeView listStore = do
--    treeSelection   <-  New.treeViewGetSelection treeView
--    paths           <-  New.treeSelectionGetSelectedRows treeSelection
--    case paths of
--        []  ->  return Nothing
--        a:r ->  do
--            val     <-  New.listStoreGetValue listStore a
--            return (Just val)

--treeViewPopup :: IDERef
--    -> New.TreeStore (String, [(ModuleDescr,PackageDescr)])
--    -> New.TreeView
--    -> Event
--    -> IO (Bool)
--treeViewPopup ideR store treeView (Button _ click _ _ _ _ button _ _) = do
--    if button == RightButton
--        then do
--            theMenu         <-  menuNew
--            item1           <-  menuItemNewWithLabel "Edit"
--            item1 `onActivateLeaf` do
--                sel         <-  getSelectionTree treeView store
--                case sel of
--                    Just (_,[(m,_)]) -> case mbSourcePathMD m of
--                                            Nothing     ->  return ()
--                                            Just fp     ->  do
--                                                runReaderT (selectSourceBuf fp) ideR
--                                                return ()
--                    otherwise       ->  return ()
--            menuShellAppend theMenu item1
--            menuPopup theMenu Nothing
--            widgetShowAll theMenu
--            return True
--        else if button == LeftButton && click == DoubleClick
--                then do sel         <-  getSelectionTree treeView store
--                        case sel of
--                            Just (_,[(m,_)]) -> case mbSourcePathMD m of
--                                                    Nothing     ->  return ()
--                                                    Just fp     ->  do
--                                                        runReaderT (selectSourceBuf fp) ideR
--                                                        return ()
--                            otherwise       ->  return ()
--                        return True
--                else return False
--treeViewPopup _ _ _ _ = throwIDE "treeViewPopup wrong event type"

