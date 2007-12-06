{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Ghf.CallersPane
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

module Ghf.CallersPane (
    calledBy
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
import Ghf.SourceEditor

instance Pane GhfCallers
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
                putStrLn "notebook page not found: unexpected"
                return ()
            Just i  ->  do
                deactivatePaneIfActive pane
                lift $notebookRemovePage nb i
                removePaneAdmin pane

-- | We don't recover this pane
instance ModelPane GhfCallers CallersState where
    saveState p     =   return Nothing
    recoverState pp _  =  return ()

--showCallers :: GhfAction
--showCallers = do
--    m <- getCallers
--    lift $ bringPaneToFront m

calledBy :: IdentifierDescr -> GhfAction
calledBy idDescr = return ()

getCallers :: GhfM GhfCallers
getCallers = do
    mbMod <- getPane CallersCasting
    case mbMod of
        Nothing -> do
            prefs       <-  readGhf prefs
            layout      <-  readGhf layout
            let pp      =   getStandardPanePath (modulesPanePath prefs) layout
            nb          <-  getNotebook pp
            initCallers pp nb
            mbMod <- getPane CallersCasting
            case mbMod of
                Nothing ->  error "Can't init callers"
                Just m  ->  return m
        Just m ->   return m

initCallers :: PanePath -> Notebook -> GhfAction
initCallers panePath nb = do
    ghfR        <-  ask
    panes       <-  readGhf panes
    paneMap     <-  readGhf paneMap
    prefs       <-  readGhf prefs
    currentInfo <-  readGhf currentInfo
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

        New.treeViewSetHeadersVisible treeView True

        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw treeView
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        let modules = GhfCallers sw treeView treeStore
        notebookPrependPage nb sw (paneName modules)
        widgetShowAll sw
        mbPn <- notebookPageNum nb sw
        case mbPn of
            Just i -> notebookSetCurrentPage nb i
            Nothing -> putStrLn "Notebook page not found"

--        cid1 <- treeView `afterFocusIn`
--            (\_ -> do runReaderT (makeActive modules) ghfR; return True)
--        cid2 <- facetView `afterFocusIn`
--            (\_ -> do runReaderT (makeActive modules) ghfR; return True)
--        treeView `onButtonPress` (treeViewPopup ghfR treeStore treeView)
--        facetView `onButtonPress` (facetViewPopup ghfR facetStore facetView)
--        sel         <-  New.treeViewGetSelection treeView
--        sel `New.onSelectionChanged` (fillFacets treeView treeStore facetStore)
--        sel2        <-  New.treeViewGetSelection facetView
--        sel2 `New.onSelectionChanged` (fillInfo facetView facetStore ghfR)

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

--treeViewPopup :: GhfRef
--    -> New.TreeStore (String, [(ModuleDescr,PackageDescr)])
--    -> New.TreeView
--    -> Event
--    -> IO (Bool)
--treeViewPopup ghfR store treeView (Button _ click _ _ _ _ button _ _) = do
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
--                                                runReaderT (selectSourceBuf fp) ghfR
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
--                                                        runReaderT (selectSourceBuf fp) ghfR
--                                                        return ()
--                            otherwise       ->  return ()
--                        return True
--                else return False
--treeViewPopup _ _ _ _ = error "treeViewPopup wrong event type"

