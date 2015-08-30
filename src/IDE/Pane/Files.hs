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
,   dirToModulePath
) where

import Prelude hiding (catch)
import Graphics.UI.Gtk
       (treeStoreLookup, cellLayoutSetAttributeFunc, treeViewRowActivated,
        treeStoreClear, treeModelGetIterFirst, treeViewLevelIndentation,
        treeViewShowExpanders, treeViewCollapseRow, treeViewExpandRow,
        treeSelectionUnselectAll, cellLayoutPackEnd, cellTextMarkup,
        cellPixbufStockId, scrolledWindowSetShadowType,
        treeSelectionSelectionChanged, treeStoreRemove, treeModelIterNext,
        treeModelGetRow, treeStoreInsert, treeModelIterNthChild,
        treeModelGetPath, TreeIter, treeModelGetIter, TreePath,
        treeSelectionGetSelectedRows, rowActivated, treeStoreGetValue,
        rowExpanded, on, after, focusInEvent, scrolledWindowSetPolicy,
        containerAdd, scrolledWindowNew, treeSelectionSetMode,
        treeViewGetSelection, treeViewSetHeadersVisible, cellText,
        cellLayoutSetAttributes, cellLayoutPackStart, treeViewAppendColumn,
        treeViewColumnSetReorderable, treeViewColumnSetResizable,
        treeViewColumnSetSizing, treeViewColumnSetTitle, treeViewColumnNew,
        cellRendererPixbufNew, cellRendererTextNew, treeViewSetModel,
        treeViewNew, treeStoreNew, castToWidget, TreeStore, TreeView,
        ScrolledWindow, treeViewRowExpanded, treeStoreGetTree, Menu(..),
        MenuItem(..))
import Data.Maybe (fromMaybe, maybeToList, listToMaybe, isJust)
import Control.Monad (forM, void, forM_, when)
import Data.Typeable (Typeable)
import IDE.Core.State
       (window, getIDE, MessageLevel(..), ipdPackageId,
        wsPackages, workspace, readIDE, IDEAction, ideMessage, reflectIDE,
        reifyIDE, IDEM, IDEPackage, ipdSandboxSources)
import IDE.Pane.SourceBuffer (fileNew, goToSourceDefinition')
import Control.Applicative ((<$>))
import System.FilePath ((</>), takeFileName, dropFileName)
import Distribution.Package (PackageIdentifier(..))
import System.Directory
       (removeDirectoryRecursive, removeDirectory, createDirectory,
        doesFileExist, removeFile, doesDirectoryExist,
        getDirectoryContents, getPermissions, readable)
import IDE.Core.CTypes
       (Location(..), packageIdentifierToString)
import Graphics.UI.Frame.Panes
       (RecoverablePane(..), PanePath, RecoverablePane, Pane(..))
import Graphics.UI.Frame.ViewFrame (getMainWindow, getNotebook)
import Graphics.UI.Editor.Basics (Connection(..))
import Graphics.UI.Gtk.General.Enums
       (ShadowType(..), PolicyType(..), SelectionMode(..),
        TreeViewColumnSizing(..))
import System.Glib.Attributes (set, AttrOp(..))
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils
       (showErrorDialog, showInputDialog, treeViewContextMenu', __,
        showDialogOptions)
import Control.Exception (catch)
import Data.Text (Text)
import qualified Data.Text as T
       (isPrefixOf, words, isSuffixOf, unpack, pack)
import Data.Monoid ((<>))
import IDE.Core.Types
       (ipdLib, WorkspaceAction, Workspace(..), wsAllPackages, WorkspaceM,
        runPackage, runWorkspace, PackageAction, PackageM, IDEPackage(..),
        IDE(..), Prefs(..), MonadIDE(..), ipdPackageDir)
import System.Glib.Properties (newAttrFromMaybeStringProperty)
import System.FilePath
       (addTrailingPathSeparator, takeDirectory, takeExtension,
       makeRelative, splitDirectories)
import Control.Monad.Reader.Class (MonadReader(..))
import IDE.Workspaces
       (workspaceRemovePackage, workspaceActivatePackage, workspaceTry,
        workspaceTryQuiet, packageTry)
import Data.List
       (isSuffixOf, find, stripPrefix, isPrefixOf, sortBy, sort)
import Data.Ord (comparing)
import Data.Char (toLower)
import System.Log.Logger (debugM)
import Data.Tree (Forest, Tree(..))
import Graphics.UI.Gtk.MenuComboToolbar.MenuItem
       (menuItemActivate, menuItemNewWithLabel)
import IDE.Pane.Modules (addModule)
import Graphics.UI.Gtk.Windows.MessageDialog
       (ButtonsType(..), MessageType(..), messageDialogNew)
import Graphics.UI.Gtk.ModelView.CellRenderer
       (CellRendererMode(..), cellMode)
import IDE.Pane.PackageEditor (packageEditText)



-- * A record in the Files Pane

-- | The data for a single cell in the file tree.
data FileRecord =
    FileRecord FilePath
  | DirRecord FilePath
              Bool -- Whether it is a source directory
  | PackageRecord IDEPackage Bool
  | AddSourcesRecord
  | AddSourceRecord IDEPackage
  | ComponentsRecord
  | ComponentRecord Text
  | PlaceHolder -- ^ Used as child node of directories that are not yet expanded,
                --   so that the expansion arrow becomes visible
    deriving (Eq)

instance Ord FileRecord where
    -- | The ordering used for displaying the records in the filetree
    compare (DirRecord _ _) (FileRecord _) = LT
    compare (FileRecord _) (DirRecord _ _) = GT
    compare (FileRecord p1) (FileRecord p2) = comparing (map toLower) p1 p2
    compare (DirRecord p1 _) (DirRecord p2 _) = comparing (map toLower) p1 p2
    compare (PackageRecord p1 _) (PackageRecord p2 _) = comparing (map toLower . ipdPackageDir) p1 p2
    compare (AddSourceRecord p1) (AddSourceRecord p2) = comparing (map toLower . ipdPackageDir) p1 p2
    compare (ComponentRecord t1) (ComponentRecord t2) = comparing (map toLower . T.unpack) t1 t2
    compare _ _ = LT


bold str = "<b>" <> str <> "</b>"
italic str = "<i>" <> str <> "</i>"
gray str = "<span foreground=\"#999999\">" <> str <> "</span>"
size str = "<span font=\"9\">" <> str <> "</span>"


-- | The markup to show in the file tree for a record
toMarkup :: FileRecord
         -> IDEPackage
         -> IDEM Text
toMarkup record pkg = do
    mbActivePackage   <- readIDE activePack
    mbActiveComponent <- readIDE activeExe

    return . size $ case record of
     (PackageRecord p _) ->
        let active = Just pkg == mbActivePackage
            pkgText = (if active then bold else id)
                          (packageIdentifierToString (ipdPackageId p))
            mbLib   = ipdLib p
            componentText = if active
                                then maybe ("(" <> "lib:" <> fromMaybe "" mbLib <> ")")
                                           (\comp -> "(" <> comp <> ")") mbActiveComponent
                                else ""
            pkgDir = gray . T.pack $ ipdPackageDir p
        in (pkgText <> " " <> componentText <> " " <> pkgDir)
     (FileRecord f) -> T.pack $ takeFileName f
     (DirRecord f _) | ipdPackageDir pkg == f -> "Files"
                     | otherwise -> T.pack $ last (splitDirectories f)
     AddSourcesRecord -> "Source Dependencies"
     (AddSourceRecord p) -> do
        let pkgText = packageIdentifierToString (ipdPackageId p)
            dirText = gray (T.pack (ipdPackageDir p))
        pkgText <> " " <> dirText
     ComponentsRecord -> "Components"
     (ComponentRecord comp) -> do
        let active = Just pkg == mbActivePackage &&
                         (mbActiveComponent == Nothing &&
                         "lib:" `T.isPrefixOf` comp
                            ||
                                 Just comp == mbActiveComponent)
        (if active then bold else id) comp
     PlaceHolder -> "Placeholder"

-- | The icon to show for a record in the file tree
toIcon :: FileRecord -> Maybe Text
toIcon record = case record of
    FileRecord path
        | takeExtension path == ".hs"    -> Just "ide_source"
        | takeExtension path == ".cabal" -> Just "ide_cabal_file"
    DirRecord p isSrc
        | isSrc     -> Just "ide_source_folder"
        | otherwise -> Just "ide_folder"
    PackageRecord _ _ -> Just "ide_package"
    ComponentsRecord -> Just "ide_component"
    AddSourcesRecord -> Just "ide_source_dependency"
    AddSourceRecord _ -> Just "ide_package"
    _ -> Nothing


-- | Gets the package to which a node in the tree belongs
iterToPackage :: TreeStore FileRecord -> TreeIter -> IDEM (Maybe IDEPackage)
iterToPackage store iter = do
    path <- liftIO $ treeModelGetPath store iter
    treePathToPackage store path

-- | Gets the package to which a node in the tree belongs
treePathToPackage :: TreeStore FileRecord -> TreePath -> IDEM (Maybe IDEPackage)
treePathToPackage store (n:_) = do
    record <- liftIO $ treeStoreGetValue store [n]
    case record of
        (PackageRecord pkg _) -> return (Just pkg)
        _                     -> do
            ideMessage Normal "treePathToPackage: Unexpected entry at root forest"
            return Nothing
treePathToPackage _ _ = do
    ideMessage Normal "treePathToPackage is called with empty path"
    return Nothing


-- | Determines whether the 'FileRecord' can expand, i.e. whether
-- it should get an expander.
canExpand :: FileRecord -> IDEPackage -> IDEM Bool
canExpand record pkg = case record of
    (PackageRecord _ _) -> return True
    (DirRecord fp _)     -> do
        mbWs <- readIDE workspace
        case mbWs of
            Just ws -> not . null <$> (flip runWorkspace ws . flip runPackage pkg $ dirRecords fp)
            Nothing -> return False
    ComponentsRecord    -> return . not . null $ components
    AddSourcesRecord    -> return . not . null $ ipdSandboxSources pkg
    (AddSourceRecord _) -> return True
    _                   -> return False

    where components = maybeToList (ipdLib pkg) ++ ipdExes pkg ++ ipdTests pkg ++ ipdBenchmarks pkg

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
                $ \record -> [ newAttrFromMaybeStringProperty "stock-id" := toIcon record]

        renderer1    <- cellRendererTextNew
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetAttributeFunc col1 renderer1 fileStore $ \iter -> do
            record <- treeModelGetRow fileStore iter
            mbPkg  <- flip reflectIDE ideR $ iterToPackage fileStore iter
            forM_ mbPkg $ \pkg -> do
                -- The cellrenderer is stateful, so it knows which cell this markup will be for (the cell at iter)
                markup <- flip reflectIDE ideR $ toMarkup record pkg
                forM_ mbPkg $ \pkg -> set renderer1 [ cellTextMarkup := Just markup]

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

--        on sel treeSelectionSelectionChanged $ do
        on treeView rowExpanded $ \iter path -> do
            record <- treeStoreGetValue fileStore path
            mbPkg    <- flip reflectIDE ideR $ iterToPackage fileStore iter
            forM_ mbPkg $ \pkg -> do
                flip reflectIDE ideR $ do
                    case record of
                        DirRecord f _ -> workspaceTryQuiet $ do
                            runPackage (refreshPackageTreeFrom fileStore treeView path) pkg
                        PackageRecord _ _ -> workspaceTryQuiet . flip runPackage pkg $ do
                                records <- packageRecords
                                liftIDE $ setEntries fileStore path records
                        AddSourcesRecord -> workspaceTryQuiet $
                            runPackage (refreshPackageTreeFrom fileStore treeView path) pkg
                        ComponentsRecord -> workspaceTryQuiet $
                            runPackage (refreshPackageTreeFrom fileStore treeView path) pkg
                        ComponentRecord _ -> return ()
                        AddSourceRecord pkg -> do
                            workspaceTryQuiet $ do
                                runPackage (refreshPackageTreeFrom fileStore treeView path) pkg
                        PlaceHolder -> ideMessage Normal (__ "Unexpected Selection in Files Pane")
                        _ -> return ()

        on treeView rowActivated $ \path col -> do
            record <- treeStoreGetValue fileStore path
            mbPkg    <- flip reflectIDE ideR $ treePathToPackage fileStore path
            forM_ mbPkg $ \pkg -> do
                expandable <- flip reflectIDE ideR $ canExpand record pkg
                case record of
                        FileRecord f  -> void . flip reflectIDE ideR $
                                             goToSourceDefinition' f (Location "" 1 0 1 0)
                        ComponentRecord name -> flip reflectIDE ideR $ workspaceTryQuiet $
                                                          if any (`T.isPrefixOf` name) ["exe:", "test:", "bench:"]
                                                              then workspaceActivatePackage pkg (Just name)
                                                              else workspaceActivatePackage pkg Nothing
                        _ -> when expandable $ do
                                 void $ treeViewToggleRow treeView path



        return (Just files,[ConnectC cid1])
        (cid3, cid4) <- reflectIDE (treeViewContextMenu' treeView fileStore contextMenuItems) ideR

        return (Just files, map ConnectC [cid1, cid3, cid4])

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



-- * Actions for refreshing the Files pane



-- | Refreshes the Files pane, lists all packages and synchronizes the expanded
-- nodes with the file system and workspace
refreshFiles :: IDEAction
refreshFiles = do
    files <- getFiles Nothing
    let store = fileStore files
    let view  = treeView files

    workspaceTryQuiet $ do
            packages <- sort . wsPackages <$> ask

            activePackage <- readIDE activePack
            let forest = map (\pkg -> leaf $ PackageRecord pkg (activePackage == Just pkg)) packages
            liftIDE $ setEntries store [] forest

            forM_ (zip [0..] packages) $ \(n, pkg) -> do
                records <- runPackage packageRecords pkg
                liftIDE $ setEntries store [n] records
                forM_ (zip [0..] records) $ \(m, _) -> do
                    runPackage (refreshPackageTreeFrom store view [n, m]) pkg


-- | Recursively mutates the 'TreeStore' with the given TreePath as root to attach new
-- entries to. Expects a 'TreePath' that is non-empty (thus points into a package).
-- Only refreshes content of expanded folders.
refreshPackageTreeFrom :: TreeStore FileRecord -> TreeView -> TreePath -> PackageAction
refreshPackageTreeFrom store view path = do
    isExpanded <- liftIO $ treeViewRowExpanded view path
    record     <- liftIO $ treeStoreGetValue store path

    Just pkg <- liftIDE $ treePathToPackage store path
    expandable <- liftIDE $ canExpand record pkg
    forest <- if isExpanded
                  then do
                      forest <- subTrees record
                      when (null forest) $ do
                          -- it has no children, collapse it
                          liftIO $ treeViewCollapseRow view path
                          liftIO $ treeStoreRemoveChildren store path
                      return forest
                  else return $ if expandable then [leaf PlaceHolder] else []


    liftIDE $ setEntries store path forest
    forM_ [0..length forest-1] $ \n -> do
        refreshPackageTreeFrom store view (path ++ [n])


-- | Returns the subtrees of the 'FileRecord'. Non-expanded
-- folders do not have their contents as subtrees but a
-- dummy 'PlaceHolder' entry instead.
subTrees :: FileRecord -> PackageM (Forest FileRecord)
subTrees record = case record of
    DirRecord dir _     -> map leaf <$> dirRecords dir
    ComponentsRecord    -> map leaf <$> componentsRecords
    AddSourcesRecord    -> map leaf <$> addSourcesRecords
    AddSourceRecord pkg -> local (\_ -> pkg) packageRecords
    PackageRecord pkg _ -> ideMessage Normal "Unexpected package node in file tree" >> return []
    _                   -> return []


-- | Returns the direct children, the add source dependencies
addSourcesRecords :: PackageM [FileRecord]
addSourcesRecords = do
    pkg <- ask
    return $ sort $ map AddSourceRecord (ipdSandboxSources pkg)

-- | Returns the direct child records for the package, this includes
-- an 'AddSourcesRecord' and 'ComponentsRecord'
packageRecords :: PackageM (Forest FileRecord)
packageRecords = do
    p <- ask
    liftIO $ debugM "leksah" $ "packageRecords" <> ipdCabalFile p
    let dir = ipdPackageDir p
    return [ Node ComponentsRecord []
           , Node AddSourcesRecord []
           , Node (DirRecord dir False)  []]


-- | Returns the contents at the given 'FilePath' as 'FileRecord's.
-- Runs in the PackageM monad to determine if directories are
-- source directories (as specified in the cabal file)
dirRecords :: FilePath -> PackageM [FileRecord]
dirRecords dir = do
   prefs    <- readIDE prefs
   contents <- liftIO $ getDirectoryContents dir
                            `catch` \(e :: IOError) -> return []
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


-- | Refreshes the components for a specific package
componentsRecords :: PackageM [FileRecord]
componentsRecords = do
    package         <- ask
    mbActivePackage <- readIDE activePack
    activeComponent <- readIDE activeExe

    return $ sort $ map (\comp -> ComponentRecord comp) (components package)

    where
        components package = map ("lib:" <>) (maybeToList (ipdLib package))
                          ++ map ("exe:" <>) (ipdExes package)
                          ++ map ("test:" <>) (ipdTests package)
                          ++ map ("bench:" <>)  (ipdBenchmarks package)


-- | Sets the subtrees of the given 'TreePath' to the provided tree of 'FileRecord's. If a record
-- is already present, it is kept in the same (expanded) state.
setEntries :: TreeStore FileRecord -> TreePath -> Forest FileRecord -> IDEAction
setEntries store parentPath records = reifyIDE $ \ideR -> do
    forM_ (zip [0..] (map rootLabel records)) $ \(n, record) -> do
        mbParentIter <- treeModelGetIter store parentPath
        mbChildIter <- treeModelIterNthChild store mbParentIter n
        findResult <- searchToRight record store mbChildIter
        case (mbChildIter, findResult) of
            (_, WhereExpected _) -> return () -- it's already there
            (Just iter, Found _) -> do -- it's already there at a later sibling
                path <- treeModelGetPath store iter
                removeUntil record store path
            _ -> do
                treeStoreInsert store parentPath n record

    -- Recursively set the subforests
    forM_ (zip [0..] (map subForest records)) $ \(n, forest) -> do
        reflectIDE (setEntries store (parentPath ++ [n]) forest) ideR

    when (not (null records)) $
        void $ removeRemaining store (parentPath++[length records])



-- * Context menu

contextMenuItems :: FileRecord -> TreePath -> TreeStore FileRecord -> IDEM [(Text, IDEAction)]
contextMenuItems record path store = do
    case record of
        (FileRecord fp) -> do
            let onDeleteFile = reifyIDE $ \ideRef -> do
                    showDialogOptions
                        ("Are you sure you want to delete " <> T.pack (takeFileName fp) <> "?")
                        MessageQuestion
                        [ ("Delete File", removeFile fp >> reflectIDE refreshFiles ideRef)
                        , ("Cancel", return ())
                        ]
                        (Just 0)
            return [("Delete File", onDeleteFile)]

        DirRecord fp _ -> do

            let onNewModule = do
                    mbModulePath <- dirToModulePath fp
                    let modulePrefix = fromMaybe [] mbModulePath
                    packageTry $ addModule modulePrefix
                    refreshFiles

            let onNewTextFile = reifyIDE $ \ideRef -> do
                    mbText <- showInputDialog "File name:" ""
                    case mbText of
                        Just t  -> do
                            let path = fp </> T.unpack t
                            exists <- doesFileExist path
                            if exists
                                then showErrorDialog "File already exists"
                                else do
                                    writeFile path ""
                                    void $ reflectIDE (refreshFiles >> goToSourceDefinition' path (Location "" 1 0 1 0)) ideRef
                        Nothing -> return ()

            let onNewDir = reifyIDE $ \ideRef -> do
                    mbText <- showInputDialog "Directory name:" ""
                    case mbText of
                        Just t  -> do
                            let path = fp </> T.unpack t
                            exists <- doesDirectoryExist path
                            if exists
                                then showErrorDialog "Directory already exists"
                                else do
                                    createDirectory path
                                    void $ reflectIDE refreshFiles ideRef
                        Nothing -> return ()

            let onDeleteDir = reifyIDE $ \ideRef -> do
                    showDialogOptions
                        ("Are you sure you want to delete " <> T.pack (takeFileName fp) <> "?")
                        MessageQuestion
                        [ ("Delete directory", removeDirectoryRecursive fp >> reflectIDE refreshFiles ideRef)
                        , ("Cancel", return ())
                        ]
                        (Just 0)

            return [ ("New Module", onNewModule)
                   , ("New Text File", onNewTextFile)
                   , ("New Directory", onNewDir)
                   , ("Delete Directory", onDeleteDir)]

        PackageRecord p _ -> do

            let onSetActive = workspaceTryQuiet $ workspaceActivatePackage p Nothing
                onAddModule = workspaceTryQuiet $ runPackage (addModule []) p
                onOpenCabalFile = workspaceTryQuiet $ runPackage packageEditText p
                onRemoveFromWs = workspaceTryQuiet $ do
                    workspaceRemovePackage p
                    liftIDE refreshFiles

            return [ ("Set As Active Package", onSetActive)
                   , ("Add Module", onAddModule)
                   , ("Open .cabal File", onOpenCabalFile)
                   , ("Remove From Workspace", onRemoveFromWs)]
        ComponentRecord comp -> do
            Just pkg <- treePathToPackage store path
            let onSetActive = workspaceTryQuiet $
                                  if any (`T.isPrefixOf` comp) ["exe:", "test:", "bench:"]
                                      then workspaceActivatePackage pkg (Just comp)
                                      else workspaceActivatePackage pkg Nothing
            return [ ("Activate component", onSetActive) ]

        _ -> return []


-- | Searches the source folders too determine what the corresponding
--   module path is
dirToModulePath :: FilePath -> IDEM (Maybe [Text])
dirToModulePath fp = do
     mbWorkspace <- readIDE workspace
     return $ do
        ws     <- mbWorkspace
        let srcDirs = concatMap (\pkg -> map (ipdPackageDir pkg </>) (ipdSrcDirs pkg))
                                (wsAllPackages ws)
        srcDir <- find (`isPrefixOf` fp) srcDirs
        let suffix = makeRelative srcDir fp
        let dirs = map T.pack (splitDirectories suffix)
        return dirs



-- * Utility functions for operating on 'TreeStore'



leaf :: a -> Tree a
leaf x = Node x []

treeStoreRemoveChildren :: TreeStore a -> TreePath -> IO ()
treeStoreRemoveChildren store path = do
    tree <- treeStoreGetTree store path
    treeStoreRemove store path
    when (not (null path)) $
        treeStoreInsert store (init path) (last path) (rootLabel tree)

data FindResult = WhereExpected TreeIter | Found TreeIter | NotFound

-- | Tries to find the given value in the 'TreeStore'. Only looks at the given 'TreeIter' and its
-- sibling nodes to the right.
-- Returns @WhereExpected iter@ if the records is found at the provided 'TreeIter'
-- Returns @Found iter@ if the record is found at a sibling iter
-- Returns @NotFound@ otherwise
searchToRight :: Eq a => a -> TreeStore a -> Maybe TreeIter -> IO FindResult
searchToRight _ _ Nothing = return NotFound
searchToRight a store (Just iter) = do
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
