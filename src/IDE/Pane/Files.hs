{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Files
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability  :  portable
--
-- | The pane of ide that shows a list of all the files in the workspace
--
-------------------------------------------------------------------------------

module IDE.Pane.Files (
    IDEFiles(..)
,   FilesState(..)
,   getFiles
,   refreshFiles
) where

import Prelude hiding (catch)
import Graphics.UI.Gtk
       (scrolledWindowSetShadowType, treeSelectionSelectionChanged,
        treeStoreRemove, treeModelIterNext, treeModelGetRow,
        treeStoreInsert, treeModelIterNthChild, treeModelGetPath, TreeIter,
        treeModelGetIter, TreePath, treeSelectionGetSelectedRows,
        rowActivated, treeStoreGetValue, rowExpanded, on, after,
        focusInEvent, scrolledWindowSetPolicy, containerAdd,
        scrolledWindowNew, treeSelectionSetMode, treeViewGetSelection,
        treeViewSetHeadersVisible, cellText, cellLayoutSetAttributes,
        cellLayoutPackStart, treeViewAppendColumn,
        treeViewColumnSetReorderable, treeViewColumnSetResizable,
        treeViewColumnSetSizing, treeViewColumnSetTitle, treeViewColumnNew,
        cellRendererPixbufNew, cellRendererTextNew, treeViewSetModel,
        treeViewNew, treeStoreNew, castToWidget, TreeStore, TreeView,
        ScrolledWindow)
import Data.Maybe (isJust)
import Control.Monad (void, forM_, when)
import Data.Typeable (Typeable)
import IDE.Core.State
       (MessageLevel(..), ipdBuildDir, ipdPackageId, wsPackages,
        workspace, readIDE, IDEAction, ideMessage, reflectIDE, reifyIDE,
        IDEM, IDEPackage, ipdSandboxSources)
import IDE.Pane.SourceBuffer
    (goToSourceDefinition')
import Control.Applicative ((<$>))
import System.FilePath ((</>), takeFileName, dropFileName)
import Distribution.Package (PackageIdentifier(..))
import System.Directory (doesDirectoryExist, getDirectoryContents, getPermissions, readable)
import IDE.Core.CTypes
       (Location(..), packageIdentifierToString)
import Graphics.UI.Frame.Panes
       (RecoverablePane(..), PanePath, RecoverablePane, Pane(..))
import Graphics.UI.Frame.ViewFrame (getNotebook)
import Graphics.UI.Editor.Basics (Connection(..))
import Graphics.UI.Gtk.General.Enums
       (ShadowType(..), PolicyType(..), SelectionMode(..),
        TreeViewColumnSizing(..))
import System.Glib.Attributes (AttrOp(..))
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (__)
import Control.Exception (catch)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Monoid ((<>))

data FileRecord =
    FileRecord FilePath
  | DirRecord FilePath
  | PackageRecord IDEPackage
  | PlaceHolder deriving(Eq)

file :: FileRecord -> Text
file (FileRecord f) = T.pack $ takeFileName f
file (DirRecord f) = T.pack $ takeFileName f
file (PackageRecord p) = packageIdentifierToString (ipdPackageId p) <> " " <> T.pack (ipdBuildDir p)
file PlaceHolder = ""

-- | A files pane description
--

data IDEFiles        =   IDEFiles {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   fileStore       ::   TreeStore FileRecord
} deriving Typeable

data FilesState      =   FilesState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEFiles IDEM
    where
    primPaneName _  =   __ "Files"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Files"

instance RecoverablePane IDEFiles FilesState IDEM where
    saveState p     =   return (Just FilesState)
    recoverState pp FilesState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = reifyIDE $ \ ideR -> do
        fileStore   <-  treeStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView fileStore

        renderer1    <- cellRendererTextNew
        renderer10   <- cellRendererPixbufNew
        col1         <- treeViewColumnNew
        treeViewColumnSetTitle col1 (__ "File")
        treeViewColumnSetSizing col1 TreeViewColumnAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        treeViewAppendColumn treeView col1
        cellLayoutPackStart col1 renderer10 False
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetAttributes col1 renderer1 fileStore
            $ \row -> [ cellText := file row]

        treeViewSetHeadersVisible treeView False
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionSingle

        scrolledView <- scrolledWindowNew Nothing Nothing
        scrolledWindowSetShadowType scrolledView ShadowIn
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic

        let files = IDEFiles {..}

        cid1 <- after treeView focusInEvent $ do
            liftIO $ reflectIDE (makeActive files) ideR
            return True
        cid2 <- on treeView rowExpanded $ \ iter path -> do
            record <- treeStoreGetValue fileStore path
            reflectIDE (
                case record of
                    DirRecord f     -> liftIO $ refreshDir fileStore path f
                    PackageRecord p -> liftIO $ refreshPackage fileStore path p
                    _               -> ideMessage Normal (__ "Unexpected Expansion in Files Pane")) ideR
        on treeView rowActivated $ \ path col -> do
            record <- treeStoreGetValue fileStore path
            reflectIDE (
                case record of
                    FileRecord f    -> void (goToSourceDefinition' f (Location "" 1 0 1 0))
                    DirRecord f     -> liftIO $ refreshDir fileStore path f
                    PackageRecord p -> liftIO $ refreshPackage fileStore path p
                    _               -> ideMessage Normal (__ "Unexpected Activation in Files Pane")) ideR
        on sel treeSelectionSelectionChanged $ do
            paths <- treeSelectionGetSelectedRows sel
            forM_ paths $ \ path -> do
                record <- treeStoreGetValue fileStore path
                reflectIDE (
                    case record of
                        FileRecord _    -> return ()
                        DirRecord f     -> liftIO $ refreshDir fileStore path f
                        PackageRecord p -> liftIO $ refreshPackage fileStore path p
                        _               -> ideMessage Normal (__ "Unexpected Selection in Files Pane")) ideR

        return (Just files,[ConnectC cid1])

getFiles :: Maybe PanePath -> IDEM IDEFiles
getFiles Nothing    = forceGetPane (Right "*Files")
getFiles (Just pp)  = forceGetPane (Left pp)

getSelectionFileRecord ::  TreeView
    ->  TreeStore FileRecord
    -> IO (Maybe FileRecord)
getSelectionFileRecord treeView fileStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        p:_ ->  Just <$> treeStoreGetValue fileStore p
        _   ->  return Nothing

refreshFiles :: IDEAction
refreshFiles = do
    files <- getFiles Nothing
    let store = fileStore files
    mbWS <- readIDE workspace
    liftIO $ setDirectories store Nothing $ map PackageRecord $ maybe [] wsPackages mbWS

dirContents :: FilePath -> IO [FileRecord]
dirContents dir =
   (filter ((/= '.') . head) <$> getDirectoryContents dir >>=
               mapM
                 (\ f ->
                    do let full = dir </> f
                       isDir <- doesDirectoryExist full
                       return $ if isDir then DirRecord full else FileRecord full))
             `catch` \ (e :: IOError) -> return []

refreshPackage :: TreeStore FileRecord -> TreePath -> IDEPackage -> IO ()
refreshPackage store path p = do
    let dir = ipdBuildDir p
    mbIter <- treeModelGetIter store path
    when (isJust mbIter) $ do
        contents <- dirContents dir
        setDirectories store mbIter $ map PackageRecord (ipdSandboxSources p) ++ contents

refreshDir :: TreeStore FileRecord -> TreePath -> FilePath -> IO ()
refreshDir store path dir = do
    mbIter <- treeModelGetIter store path
    when (isJust mbIter) $ do
        contents <- dirContents dir
        setDirectories store mbIter contents

setDirectories :: TreeStore FileRecord -> Maybe TreeIter -> [FileRecord] -> IO ()
setDirectories store parent records = do
    parentPath <- case parent of
                Just i -> treeModelGetPath store i
                _      -> return []
    forM_ (zip [0..] records) $ \(n, record) -> do
        mbChild <- treeModelIterNthChild store parent n
        findResult <- find record store mbChild
        case (mbChild, findResult) of
            (_, WhereExpected _) -> return ()
            (Just iter, Found _) -> do
                path <- treeModelGetPath store iter
                removeUntil record store path
            _ -> do
                treeStoreInsert store parentPath n record
                case record of
                    DirRecord _     -> treeStoreInsert store (parentPath++[n]) 0 PlaceHolder
                    PackageRecord _ -> treeStoreInsert store (parentPath++[n]) 0 PlaceHolder
                    _               -> return ()
    removeRemaining store (parentPath++[length records])

data FindResult = WhereExpected TreeIter | Found TreeIter | NotFound

find :: Eq a => a -> TreeStore a -> Maybe TreeIter -> IO FindResult
find _ _ Nothing = return NotFound
find a store (Just iter) = do
    row <- treeModelGetRow store iter
    if row == a
        then return $ WhereExpected iter
        else treeModelIterNext store iter >>= find'
  where
    find' :: Maybe TreeIter -> IO FindResult
    find' Nothing = return NotFound
    find' (Just iter) = do
        row <- treeModelGetRow store iter
        if row == a
            then return $ Found iter
            else treeModelIterNext store iter >>= find'

removeUntil :: Eq a => a -> TreeStore a -> TreePath -> IO ()
removeUntil a store path = do
    row <- treeStoreGetValue store path
    when (row /= a) $ do
        found <- treeStoreRemove store path
        when found $ removeUntil a store path

removeRemaining :: TreeStore a -> TreePath -> IO ()
removeRemaining store path = do
    found <- treeStoreRemove store path
    when found $ removeRemaining store path

















