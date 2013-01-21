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
import Graphics.UI.Gtk.Gdk.EventM
import Data.Maybe
import Data.Typeable
import IDE.Core.State
import IDE.Workspaces
import qualified Data.Map as Map (empty)
import Data.List (sortBy)
import IDE.Pane.Files (refreshFiles)
import IDE.Pane.HLint (refreshHLint)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (treeViewContextMenu, __)
import System.Glib.Properties (newAttrFromMaybeStringProperty)

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
    primPaneName _  =   (__ "Workspace")
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
        treeViewColumnSetTitle col0 (__ "Active")
        treeViewColumnSetSizing col0 TreeViewColumnAutosize
        treeViewColumnSetResizable col0 True
        treeViewColumnSetReorderable col0 True
        treeViewAppendColumn treeView col0
        cellLayoutPackStart col0 renderer0 True
        cellLayoutSetAttributes col0 renderer0 listStore
            $ \row -> [newAttrFromMaybeStringProperty "stock-id" :=
                        if (\(b,_)-> b) row
                            then Just stockYes
                            else Nothing]

        renderer1   <- cellRendererTextNew
        col1        <- treeViewColumnNew
        treeViewColumnSetTitle col1 (__ "Package")
        treeViewColumnSetSizing col1 TreeViewColumnAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        treeViewAppendColumn treeView col1
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetAttributes col1 renderer1 listStore
            $ \row -> [ cellText := (\(_,pack)-> (packageIdentifierToString . ipdPackageId) pack) row ]

        renderer2   <- cellRendererTextNew
        col2        <- treeViewColumnNew
        treeViewColumnSetTitle col2 (__ "File path")
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
        cid1 <- treeView `after` focusInEvent $ do
            liftIO $ reflectIDE (makeActive workspacePane) ideR
            return True
        (cid2, cid3) <- treeViewContextMenu treeView $ workspaceContextMenu ideR workspacePane
        cid4 <- treeView `on` rowActivated $ workspaceSelect ideR workspacePane
        return (Just workspacePane, map ConnectC [cid1, cid2, cid3, cid4])

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

workspaceContextMenu :: IDERef
                     -> IDEWorkspace
                     -> Menu
                     -> IO ()
workspaceContextMenu ideR workspacePane theMenu = do
    item1 <- menuItemNewWithLabel (__ "Activate Package")
    item2 <- menuItemNewWithLabel (__ "Add Package")
    item3 <- menuItemNewWithLabel (__ "Remove Package")
    item1 `on` menuItemActivate $ do
        sel <- getSelectionTree (treeViewC workspacePane)
                                (workspaceStore workspacePane)
        case sel of
            Just (_,ideP) -> reflectIDE (workspaceTry $ workspaceActivatePackage ideP) ideR

            otherwise     -> return ()
    item2 `on` menuItemActivate $ reflectIDE (workspaceTry $ workspaceAddPackage) ideR
    item3 `on` menuItemActivate $ do
        sel <- getSelectionTree (treeViewC workspacePane)
                                (workspaceStore workspacePane)
        case sel of
            Just (_,ideP) -> reflectIDE (workspaceTry $ workspaceRemovePackage ideP) ideR
            otherwise     -> return ()
    menuShellAppend theMenu item1
    menuShellAppend theMenu item2
    menuShellAppend theMenu item3

workspaceSelect :: IDERef
                -> IDEWorkspace
                -> TreePath
                -> TreeViewColumn
                -> IO ()
workspaceSelect ideR workspacePane [n] _ = do
    (_,ideP) <- listStoreGetValue (workspaceStore workspacePane) n
    reflectIDE (workspaceTry $ workspaceActivatePackage ideP) ideR
workspaceSelect _ _ _ _ = liftIO $ sysMessage Normal (__ "Workspace >> workspaceSelect: invalid path")

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
    workspaceTry refreshHLint

