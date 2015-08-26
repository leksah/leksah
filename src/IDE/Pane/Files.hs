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
,   rebuildFilesPane
) where

import Prelude hiding (catch)
import Graphics.UI.Gtk
       (treeViewLevelIndentation, treeViewShowExpanders,
        treeViewCollapseRow, treeViewExpandRow, treeSelectionUnselectAll,
        cellLayoutPackEnd, cellTextMarkup, cellPixbufStockId,
        scrolledWindowSetShadowType, treeSelectionSelectionChanged,
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
        ScrolledWindow, treeViewRowExpanded, treeStoreGetTree)
import Data.Maybe (listToMaybe, isJust)
import Control.Monad (forM, void, forM_, when)
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
import System.Glib.Attributes (set, AttrOp(..))
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (__)
import Control.Exception (catch)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Monoid ((<>))
import Data.List (stripPrefix, isPrefixOf, sortBy, sort)
import Data.Ord (comparing)
import Data.Char (toLower)
import IDE.Core.Types
       (wsAllPackages, WorkspaceM, runPackage, runWorkspace,
        PackageAction, PackageM, IDEPackage(..), IDE(..), Prefs(..))
import Data.Tree (Tree(..))
import System.Log.Logger (debugM)
import System.Glib.Properties (newAttrFromMaybeStringProperty)
import System.FilePath
       (addTrailingPathSeparator, takeDirectory, takeExtension)
import Control.Monad.Reader.Class (MonadReader(..))
import IDE.Workspaces (workspaceTryQuiet)



-- * A record in the Files Pane

-- | The data for a single cell in the file tree
data FileRecord =
    FileRecord FilePath
  | DirRecord FilePath
              Bool -- Whether it is a source directory
  | PackageRecord IDEPackage
                  Bool -- Whether the package is active
  | PlaceHolder -- ^ Used as child node of directories that are not yet expanded,
                --   so that the expansion arrow becomes visible
  deriving (Eq)


instance Ord FileRecord where
    -- | The ordering used for displaying the records in the filetree
    compare (DirRecord _ _) (FileRecord _) = LT
    compare (FileRecord _) (DirRecord _ _) = GT
    compare (FileRecord p1) (FileRecord p2) = comparing (map toLower) p1 p2
    compare (DirRecord p1 _) (DirRecord p2 _) = comparing (map toLower) p1 p2
    compare _ _ = LT


bold str = "<b>" <> str <> "</b>"
gray str = "<span foreground=\"#999999\">" <> str <> "</span>"
size str = "<span font=\"9\">" <> str <> "</span>"

-- | The markup to show in the file tree for a file record
toMarkup :: FileRecord -> Text
toMarkup (PackageRecord p active) =
    let pkg = (if active then bold else id)
                  (packageIdentifierToString (ipdPackageId p))
        dir = gray (T.pack (ipdBuildDir p))
    in size $ pkg <> " " <> dir
toMarkup (FileRecord f) = size $ T.pack $ takeFileName f
toMarkup (DirRecord f _) = size $ T.pack $ takeFileName f
toMarkup PlaceHolder = ""

-- * The Files pane

-- | The representation of the Files pane
data IDEFiles        =   IDEFiles {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   fileStore       ::   TreeStore FileRecord
} deriving Typeable


-- | The additional state used when recovering the pane
--   (none, the package directories come from the IDE state)
data FilesState      =   FilesState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEFiles IDEM where
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



        col1         <- treeViewColumnNew
        treeViewColumnSetTitle col1 (__ "File")
        treeViewColumnSetSizing col1 TreeViewColumnAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        treeViewAppendColumn treeView col1


        prefs <- reflectIDE (readIDE prefs) ideR
        when (showFileIcons prefs) $ do
            renderer2    <- cellRendererPixbufNew
            cellLayoutPackStart col1 renderer2 False
            set renderer2 [ newAttrFromMaybeStringProperty "stock-id"  := (Nothing :: Maybe Text) ]
            cellLayoutSetAttributes col1 renderer2 fileStore
                $ \record -> [ newAttrFromMaybeStringProperty "stock-id" :=
                                   case record of
                                        FileRecord path | takeExtension path == ".hs" -> Just ("ide_source" :: Text)
                                                        | takeExtension path == ".cabal" -> Just ("ide_cabal_file" :: Text)
                                        DirRecord p isSrc | isSrc     -> Just ("ide_source_folder" :: Text)
                                                          | otherwise -> Just ("ide_folder" :: Text)
                                        PackageRecord _ _ -> Just ("ide_package" :: Text)
                                        _ -> Nothing
                                        ]

        renderer1    <- cellRendererTextNew
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetAttributes col1 renderer1 fileStore
            $ \record -> [ cellTextMarkup := Just (toMarkup record)]

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
                    DirRecord f _     -> workspaceTryQuiet $ do
                        mbPkg <- fileGetPackage f
                        forM_ mbPkg $ \package ->
                             runPackage (refreshDir fileStore path f) package
                    PackageRecord p _ -> workspaceTryQuiet $
                                             runPackage (refreshPackage fileStore path) p
                    _                 -> ideMessage Normal (__ "Unexpected Expansion in Files Pane")) ideR
        on treeView rowActivated $ \ path col -> do
            record <- treeStoreGetValue fileStore path
            reflectIDE (
                case record of
                    FileRecord f      -> void (goToSourceDefinition' f (Location "" 1 0 1 0))
                    DirRecord f _     -> workspaceTryQuiet $ do
                        mbPkg <- fileGetPackage f
                        forM_ mbPkg $ \package ->
                             runPackage (refreshDir fileStore path f) package
                    PackageRecord p _ -> workspaceTryQuiet $
                                             runPackage (refreshPackage fileStore path) p
                    _                  -> ideMessage Normal (__ "Unexpected Activation in Files Pane")) ideR

        on sel treeSelectionSelectionChanged $ do
            paths <- treeSelectionGetSelectedRows sel
            forM_ paths $ \ path -> do
                record <- treeStoreGetValue fileStore path
                reflectIDE (
                    case record of
                        FileRecord f      -> void (goToSourceDefinition' f (Location "" 1 0 1 0))
                        DirRecord f _     -> workspaceTryQuiet $ do
                            mbPkg <- fileGetPackage f
                            forM_ mbPkg $ \package ->
                                 runPackage (refreshDir fileStore path f) package
                            liftIO $ treeViewToggleRow treeView path
                            liftIO $ treeSelectionUnselectAll sel
                        PackageRecord p _ -> do
                            workspaceTryQuiet $
                                runPackage (refreshPackage fileStore path) p
                            liftIO $ treeViewToggleRow treeView path
                            liftIO $ treeSelectionUnselectAll sel

                        _                 -> ideMessage Normal (__ "Unexpected Selection in Files Pane")) ideR
        return (Just files,[ConnectC cid1])

-- | Get the Files pane
getFiles :: Maybe PanePath -> IDEM IDEFiles
getFiles Nothing    = forceGetPane (Right "*Files")
getFiles (Just pp)  = forceGetPane (Left pp)


-- | Toggles a row in a `TreeView`
treeViewToggleRow treeView path = do
    expanded <- treeViewRowExpanded treeView path
    if expanded
        then treeViewCollapseRow treeView path
        else treeViewExpandRow   treeView path False


-- | Deletes the filepane and rebuilds it
rebuildFilesPane :: IDEAction
rebuildFilesPane = do
    mbFilePane <- getPane :: IDEM (Maybe IDEFiles)
    forM_ mbFilePane closePane
    getOrBuildPane (Right "*Files") :: IDEM (Maybe IDEFiles)
    refreshFiles
    return ()


-- | Searches the workspace packages if it is part of any
fileGetPackage :: FilePath -> WorkspaceM (Maybe IDEPackage)
fileGetPackage path = do
    packages <- wsAllPackages <$> ask
    let dirs     = [p | p <- packages, takeDirectory (ipdCabalFile p) `isPrefixOf` path]
    return (listToMaybe dirs)


-- | Refreshes the Files pane, lists all package directories and synchronizes the expanded
-- folders with the file system
refreshFiles :: IDEAction
refreshFiles = do
    files <- getFiles Nothing
    let store = fileStore files
    let view  = treeView files

    workspaceTryQuiet $ do
        workspace <- ask
        let packages = wsPackages workspace
        packageEntries <- forM packages $ \p -> do
            active <- readIDE activePack
            return (PackageRecord p (Just p == active))
        liftIO $ setDirectories store Nothing packageEntries

        forM_ (zip [0..] packages) $ \(n, package) ->
             runPackage (refreshRecursively store [n] view) package


-- | Returns the 'FileRecord's at the given 'FilePath'.
dirContents :: FilePath -> PackageM [FileRecord]
dirContents dir = do
   prefs <- readIDE prefs
   contents <- liftIO $ getDirectoryContents dir
                            `catch` \ (e :: IOError) -> return []
   let filtered = if showHiddenFiles prefs
                      then filter (`notElem` [".", ".."]) contents
                      else filter ((/= '.') . head) contents
   records <- forM filtered $ \f -> do
                  let full = dir </> f
                  isDir <- liftIO $ doesDirectoryExist full
                  if isDir
                      then do
                          -- find out if it is a source directory of the project
                          pkgDir <- (addTrailingPathSeparator . takeDirectory . ipdCabalFile) <$> ask
                          case stripPrefix pkgDir full of
                              Just relativeToPackage -> do
                                  srcDirs <- ipdSrcDirs <$> ask
                                  return $ DirRecord full (relativeToPackage `elem` srcDirs)
                              Nothing -> do
                                  ideMessage Normal ("Could not compare file " <> T.pack full <> " with its package directory " <> T.pack pkgDir)
                                  return $ DirRecord full False
                      else return $ FileRecord full
   return (sort records)


-- | Recursively refreshes the file tree with the given TreePath as root. Only refreshes contents
-- of expanded folders.
refreshRecursively :: TreeStore FileRecord -> TreePath -> TreeView -> PackageAction
refreshRecursively store path view = do
    isExpanded <- liftIO $ treeViewRowExpanded view path
    record     <- liftIO $ treeStoreGetValue store path

    when isExpanded $
        case record of
            DirRecord dir _ -> do
                refreshDir store path dir
                nChildren  <- length . subForest <$> liftIO (treeStoreGetTree store path)
                forM_ [0..nChildren-1] (\n -> refreshRecursively store (path ++ [n]) view)

            PackageRecord package _ -> local (const package) $ do
                refreshPackage store path
                nChildren  <- length . subForest <$> liftIO (treeStoreGetTree store path)
                forM_ [0..nChildren-1] (\n -> refreshRecursively store (path ++ [n]) view)

            _ -> return ()

-- | Refreshes the child nodes of the package node at the given 'TreePath'. Also adds sandbox
-- add-source dependencies as a 'PackageRecord'.
refreshPackage :: TreeStore FileRecord -> TreePath -> PackageAction
refreshPackage store path = do
    p <- ask
    liftIO $ debugM "leksah" $ "refreshPackage " <> ipdCabalFile p
    let dir = ipdBuildDir p
    mbIter <- liftIO $ treeModelGetIter store path
    when (isJust mbIter) $ do
        contents <- dirContents dir
        liftIO $ setDirectories store mbIter $ map (\p -> PackageRecord p False) (ipdSandboxSources p) ++ contents


-- | Refreshes the child nodes of the node at the given 'TreePath'.
refreshDir :: TreeStore FileRecord -> TreePath -> FilePath -> PackageAction
refreshDir store path dir =  do
    liftIO $ debugM "leksah" $ "refreshPackage " <> dir
    mbIter <- liftIO $ treeModelGetIter store path
    when (isJust mbIter) $ do
        contents <- dirContents dir
        liftIO $ setDirectories store mbIter contents

-- | Sets the child nodes of the given 'TreeIter' to the provided list of 'FileRecord's. If a record
-- is already present, it is kept in the same (expanded) state.
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

                -- Insert placeholder children for dirs and packages so they can
                -- be expanded on clicking, and have the contents loaded "lazily".
                case record of
                    DirRecord _ _     -> treeStoreInsert store (parentPath++[n]) 0 PlaceHolder
                    PackageRecord _ _ -> treeStoreInsert store (parentPath++[n]) 0 PlaceHolder
                    _                 -> return ()
    removeRemaining store (parentPath++[length records])

data FindResult = WhereExpected TreeIter | Found TreeIter | NotFound

-- | Tries to find the given value in the 'TreeStore'. Only looks at the given 'TreeIter' and its
-- sibling nodes to the right.
-- Returns @WhereExpected iter@ if the records is found at the provided 'TreeIter'
-- Returns @Found iter@ if the record is found at a sibling iter
-- Returns @NotFound@ otherwise
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


-- | Starting at the node at the given 'TreePath', removes all sibling nodes to the right
--   until the given value is found.
removeUntil :: Eq a => a -> TreeStore a -> TreePath -> IO ()
removeUntil a store path = do
    row <- treeStoreGetValue store path
    when (row /= a) $ do
        found <- treeStoreRemove store path
        when found $ removeUntil a store path


-- | Starting at the node at the given 'TreePath', removes all sibling nodes to the right
removeRemaining :: TreeStore a -> TreePath -> IO ()
removeRemaining store path = do
    found <- treeStoreRemove store path
    when found $ removeRemaining store path

















