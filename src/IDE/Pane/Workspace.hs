{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances,
             MultiParamTypeClasses, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Workspace
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Pane.Workspace (
    WorkspaceState
,   IDEWorkspace
,   updateWorkspace
,   getWorkspace
,   showWorkspace
) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Gdk.Events
import Data.Maybe
import Data.Typeable
import IDE.Core.State
import IDE.Workspaces
import qualified Data.Map as Map (empty)
import Data.List (sortBy)
import IDE.Pane.Files (refreshFiles)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))

-- | Workspace pane state
--

data IDEWorkspace   =   IDEWorkspace {
    scrolledView        ::   ScrolledWindow
,   treeViewC           ::   TreeView
,   workspaceStore      ::   ListStore (Bool,IDEPackage)
,   topBox              ::   VBox
} deriving Typeable

instance Pane IDEWorkspace IDEM
    where
    primPaneName _  =   "Workspace"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . topBox
    paneId b        =   "*Workspace"

-- | Nothing to remember here, everything comes from the IDE state
data WorkspaceState           =   WorkspaceState
    deriving(Eq,Ord,Read,Show,Typeable)

instance RecoverablePane IDEWorkspace WorkspaceState IDEM where
    saveState p     =   do
        return (Just WorkspaceState)
    recoverState pp WorkspaceState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    buildPane pp nb builder  =   do
        res <- buildThisPane pp nb builder
        when (isJust res) $ updateWorkspace True False
        return res
    builder pp nb windows = reifyIDE $ \ideR -> do
        listStore   <-  listStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView listStore

        renderer0    <- cellRendererPixbufNew
        col0        <- treeViewColumnNew
        treeViewColumnSetTitle col0 "Active"
        treeViewColumnSetSizing col0 TreeViewColumnAutosize
        treeViewColumnSetResizable col0 True
        treeViewColumnSetReorderable col0 True
        treeViewAppendColumn treeView col0
        cellLayoutPackStart col0 renderer0 True
        cellLayoutSetAttributes col0 renderer0 listStore
            $ \row -> [cellPixbufStockId  :=
                        if (\(b,_)-> b) row
                            then stockYes
                            else ""]

        renderer1   <- cellRendererTextNew
        col1        <- treeViewColumnNew
        treeViewColumnSetTitle col1 "Package"
        treeViewColumnSetSizing col1 TreeViewColumnAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        treeViewAppendColumn treeView col1
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetAttributes col1 renderer1 listStore
            $ \row -> [ cellText := (\(_,pack)-> (packageIdentifierToString . ipdPackageId) pack) row ]

        renderer2   <- cellRendererTextNew
        col2        <- treeViewColumnNew
        treeViewColumnSetTitle col2 "File path"
        treeViewColumnSetSizing col2 TreeViewColumnAutosize
        treeViewColumnSetResizable col2 True
        treeViewColumnSetReorderable col2 True
        treeViewAppendColumn treeView col2
        cellLayoutPackStart col2 renderer2 True
        cellLayoutSetAttributes col2 renderer2 listStore
            $ \row -> [ cellText := (\(_,pack)-> ipdCabalFile pack) row ]

        treeViewSetHeadersVisible treeView True
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionSingle

        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw treeView
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        box             <-  vBoxNew False 2
        boxPackEnd box sw PackGrow 0
        let workspacePane = IDEWorkspace sw treeView listStore box
        widgetShowAll box
        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive workspacePane) ideR ; return True)
        treeView `onButtonPress` (treeViewPopup ideR  workspacePane)
        return (Just workspacePane,[ConnectC cid1])

getWorkspace :: Maybe PanePath -> IDEM IDEWorkspace
getWorkspace Nothing = forceGetPane (Right "*Workspace")
getWorkspace (Just pp)  = forceGetPane (Left pp)

showWorkspace :: IDEAction
showWorkspace = do
    l <- getWorkspace Nothing
    displayPane l False

getSelectionTree ::  TreeView
    -> ListStore (Bool, IDEPackage)
    -> IO (Maybe (Bool, IDEPackage))
getSelectionTree treeView listStore = do
    treeSelection   <-  treeViewGetSelection treeView
    rows           <-  treeSelectionGetSelectedRows treeSelection
    case rows of
        [[n]]   ->  do
            val     <-  listStoreGetValue listStore n
            return (Just val)
        _       ->  return Nothing

treeViewPopup :: IDERef
    -> IDEWorkspace
    -> Event
    -> IO (Bool)
treeViewPopup ideR  workspacePane (Button _ click _ _ _ _ button _ _) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Activate Package"
            item2           <-  menuItemNewWithLabel "Add Package"
            item3           <-  menuItemNewWithLabel "Remove Package"
            item1 `onActivateLeaf` do
                sel         <-  getSelectionTree (treeViewC workspacePane)
                                                (workspaceStore workspacePane)
                case sel of
                    Just (_,ideP)      -> reflectIDE (workspaceTry_ $ workspaceActivatePackage ideP) ideR

                    otherwise         -> return ()
            item2 `onActivateLeaf` reflectIDE (workspaceTry_ $ workspaceAddPackage) ideR
            item3 `onActivateLeaf` do
                sel            <-  getSelectionTree (treeViewC workspacePane)
                                                    (workspaceStore workspacePane)
                case sel of
                    Just (_,ideP)      -> reflectIDE (workspaceTry_ $ workspaceRemovePackage ideP) ideR
                    otherwise          -> return ()
            menuShellAppend theMenu item1
            menuShellAppend theMenu item2
            menuShellAppend theMenu item3
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do sel         <-  getSelectionTree (treeViewC workspacePane)
                                            (workspaceStore workspacePane)
                        case sel of
                            Just (_,ideP)   ->  reflectIDE (workspaceTry_ $ workspaceActivatePackage ideP) ideR
                                                    >> return True
                            otherwise       ->  return False
                else return False
treeViewPopup _ _ _ = throwIDE "treeViewPopup wrong event type"

updateWorkspace :: Bool -> Bool -> IDEAction
updateWorkspace showPane updateFileCache = do
    mbWs <- readIDE workspace
    case mbWs of
        Nothing -> do
            when updateFileCache $ modifyIDE_ (\ide -> ide{bufferProjCache = Map.empty})
            mbMod <- getPane
            case mbMod of
                Nothing -> return ()
                Just (p :: IDEWorkspace)  -> do
                    liftIO $ listStoreClear (workspaceStore p)
                    when showPane $ displayPane p False
        Just ws -> do
            when updateFileCache $ modifyIDE_ (\ide -> ide{bufferProjCache = Map.empty})
            mbMod <- getPane
            case mbMod of
                Nothing -> return ()
                Just (p :: IDEWorkspace)  -> do
                    liftIO $ listStoreClear (workspaceStore p)
                    let objs = map (\ ideP -> (Just (ipdCabalFile ideP) == wsActivePackFile ws, ideP))
                                        (wsPackages ws)
                    let sorted = sortBy (\ (_,f) (_,s) -> compare (ipdPackageId f) (ipdPackageId s)) objs
                    liftIO $ mapM_ (listStoreAppend (workspaceStore p)) sorted
                    when showPane $ displayPane p False
    refreshFiles

