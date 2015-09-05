{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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
-- | The pane of the IDE that shows the cabal packages in the workspace
--   and their components, source dependencies and files
--
-----------------------------------------------------------------------------

module IDE.Pane.Workspace (
    WorkspaceState(..)
,   WorkspacePane
,   getWorkspacePane
,   showWorkspacePane
,   refreshWorkspacePane
,   rebuildWorkspacePane
) where

import Prelude hiding (catch)
import Graphics.UI.Gtk
       (treeSelectionSelectIter, treeStoreLookup,
        cellLayoutSetAttributeFunc, treeViewRowActivated, treeStoreClear,
        treeModelGetIterFirst, treeViewLevelIndentation,
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
        MenuItem(..), treeStoreSetValue)
import Data.Maybe (fromMaybe, maybeToList, listToMaybe, isJust)
import Control.Monad (forM, void, forM_, when)
import Data.Typeable (Typeable)
import IDE.Core.State
       (catchIDE, window, getIDE, MessageLevel(..), ipdPackageId,
        wsPackages, workspace, readIDE, IDEAction, ideMessage, reflectIDE,
        reifyIDE, IDEM, IDEPackage, ipdSandboxSources)
import IDE.Pane.SourceBuffer (fileNew, goToSourceDefinition')
import IDE.Sandbox
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
import Control.Exception (SomeException(..), catch)
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
       (makePackage, workspaceAddPackage', workspaceRemovePackage,
        workspaceActivatePackage, workspaceTry, workspaceTryQuiet,
        packageTry)
import Data.List
       (isSuffixOf, find, stripPrefix, isPrefixOf, sortBy, sort)
import Data.Ord (comparing)
import Data.Char (toUpper, toLower)
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
import IDE.Utils.GtkBindings (treeViewSetActiveOnSingleClick)
import IDE.Package (packageTest, packageRun, packageClean)


-- | The data for a single record in the Workspace Pane
data WorkspaceRecord =
    FileRecord FilePath
  | DirRecord FilePath
              Bool -- Whether it is a source directory
  | PackageRecord IDEPackage
  | AddSourcesRecord
  | AddSourceRecord IDEPackage
  | ComponentsRecord
  | ComponentRecord Text
  deriving (Eq)

instance Ord WorkspaceRecord where
    -- | The ordering used for displaying the records
    compare (DirRecord _ _) (FileRecord _) = LT
    compare (FileRecord _) (DirRecord _ _) = GT
    compare (FileRecord p1) (FileRecord p2) = comparing (map toLower) p1 p2
    compare (DirRecord p1 _) (DirRecord p2 _) = comparing (map toLower) p1 p2
    compare (PackageRecord p1) (PackageRecord p2) = comparing (map toLower . ipdPackageDir) p1 p2
    compare (AddSourceRecord p1) (AddSourceRecord p2) = comparing (map toLower . ipdPackageDir) p1 p2
    compare (ComponentRecord t1) (ComponentRecord t2) = comparing (map toLower . T.unpack) t1 t2
    compare _ _ = LT


-- | The markup to show for a record
toMarkup :: WorkspaceRecord
         -> IDEPackage
         -> IDEM Text
toMarkup record pkg = do
    mbActivePackage   <- readIDE activePack
    mbActiveComponent <- readIDE activeExe

    return . size $ case record of
     (PackageRecord p) ->
        let active = Just pkg == mbActivePackage
            pkgText = (if active then bold else id)
                          (packageIdentifierToString (ipdPackageId p))
            mbLib   = ipdLib p
            componentText = if active
                                then maybe (if isJust mbLib then "(library)" else "")
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
                         (mbActiveComponent == Nothing && comp == "library"
                             ||
                          Just comp == mbActiveComponent)
        (if active then bold else id) comp
    where
        bold str = "<b>" <> str <> "</b>"
        italic str = "<i>" <> str <> "</i>"
        gray str = "<span foreground=\"#999999\">" <> str <> "</span>"
        size str = "<span font=\"9\">" <> str <> "</span>"


-- | The icon to show for a record
toIcon :: WorkspaceRecord -> Maybe Text
toIcon record = case record of
    FileRecord path
        | takeExtension path == ".hs"    -> Just "ide_source"
        | takeExtension path == ".cabal" -> Just "ide_cabal_file"
    DirRecord p isSrc
        | isSrc     -> Just "ide_source_folder"
        | otherwise -> Just "ide_folder"
    PackageRecord _ -> Just "ide_package"
    ComponentsRecord -> Just "ide_component"
    AddSourcesRecord -> Just "ide_source_dependency"
    AddSourceRecord _ -> Just "ide_package"
    _ -> Nothing


-- | Gets the package to which a node in the tree belongs
iterToPackage :: TreeStore WorkspaceRecord -> TreeIter -> IDEM (Maybe IDEPackage)
iterToPackage store iter = do
    path <- liftIO $ treeModelGetPath store iter
    treePathToPackage store path

-- | Gets the package to which a node in the tree belongs
treePathToPackage :: TreeStore WorkspaceRecord -> TreePath -> IDEM (Maybe IDEPackage)
treePathToPackage store (n:_) = do
    record <- liftIO $ treeStoreGetValue store [n]
    case record of
        (PackageRecord pkg) -> return (Just pkg)
        _                     -> do
            liftIO $ debugM "leksah" "treePathToPackage: Unexpected entry at root forest"
            return Nothing
treePathToPackage _ _ = do
    liftIO $ debugM "leksah" "treePathToPackage is called with empty path"
    return Nothing


-- | Determines whether the 'WorkspaceRecord' can expand, i.e. whether
-- it should get an expander.
canExpand :: WorkspaceRecord -> IDEPackage -> IDEM Bool
canExpand record pkg = case record of
    (PackageRecord _) -> return True
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

-- * The Workspace pane

-- | The representation of the Workspace pane
data WorkspacePane        =   WorkspacePane {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   recordStore     ::   TreeStore WorkspaceRecord
} deriving Typeable


-- | The additional state used when recovering the pane
--   (none)
data WorkspaceState = WorkspaceState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane WorkspacePane IDEM where
    primPaneName _  =   __ "Workspace"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Workspace"

instance RecoverablePane WorkspacePane WorkspaceState IDEM where
    saveState p     =   return (Just WorkspaceState)
    recoverState pp WorkspaceState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = reifyIDE $ \ ideR -> do
        recordStore   <-  treeStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView recordStore



        col1         <- treeViewColumnNew
        treeViewColumnSetSizing col1 TreeViewColumnAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        treeViewAppendColumn treeView col1


        prefs <- reflectIDE (readIDE prefs) ideR
        when (showWorkspaceIcons prefs) $ do
            renderer2    <- cellRendererPixbufNew
            cellLayoutPackStart col1 renderer2 False
            set renderer2 [ newAttrFromMaybeStringProperty "stock-id"  := (Nothing :: Maybe Text) ]
            cellLayoutSetAttributes col1 renderer2 recordStore
                $ \record -> [ newAttrFromMaybeStringProperty "stock-id" := toIcon record]

        renderer1    <- cellRendererTextNew
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetAttributeFunc col1 renderer1 recordStore $ \iter -> do
            record <- treeModelGetRow recordStore iter
            mbPkg  <- flip reflectIDE ideR $ iterToPackage recordStore iter
            forM_ mbPkg $ \pkg -> do
                -- The cellrenderer is stateful, so it knows which cell this markup will be for (the cell at iter)
                markup <- flip reflectIDE ideR $ toMarkup record pkg
                forM_ mbPkg $ \pkg -> set renderer1 [ cellTextMarkup := Just markup]

        -- treeViewSetActiveOnSingleClick treeView True
        treeViewSetHeadersVisible treeView False
        sel <- treeViewGetSelection treeView
        -- treeSelectionSetMode sel SelectionSingle

        scrolledView <- scrolledWindowNew Nothing Nothing
        scrolledWindowSetShadowType scrolledView ShadowIn
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic

        let wsPane = WorkspacePane {..}

        cid1 <- after treeView focusInEvent $ do
            liftIO $ reflectIDE (makeActive wsPane) ideR
            return True

        on treeView rowExpanded $ \iter path -> do
            record <- treeStoreGetValue recordStore path
            mbPkg  <- flip reflectIDE ideR $ iterToPackage recordStore iter
            forM_ mbPkg $ \pkg -> do
                flip reflectIDE ideR $ do
                    workspaceTryQuiet $ do
                        runPackage (refreshPackageTreeFrom recordStore treeView path) pkg

        on treeView rowActivated $ \path col -> do
            record <- treeStoreGetValue recordStore path
            mbPkg    <- flip reflectIDE ideR $ treePathToPackage recordStore path
            forM_ mbPkg $ \pkg -> do
                expandable <- flip reflectIDE ideR $ canExpand record pkg
                case record of
                        FileRecord f  -> void . flip reflectIDE ideR $
                                             goToSourceDefinition' f (Location "" 1 0 1 0)
                        ComponentRecord name -> flip reflectIDE ideR $ workspaceTryQuiet $
                                                          workspaceActivatePackage pkg (Just name)
                        _ -> when expandable $ do
                                 void $ treeViewToggleRow treeView path

        (cid3, cid4) <- reflectIDE (treeViewContextMenu' treeView recordStore contextMenuItems) ideR
        reflectIDE (refresh wsPane) ideR

        return (Just wsPane, map ConnectC [cid1, cid3, cid4])

-- | Get the Workspace pane
getWorkspacePane :: IDEM WorkspacePane
getWorkspacePane = forceGetPane (Right "*Workspace")


-- | Show the Workspace pane
showWorkspacePane :: IDEAction
showWorkspacePane = do
    l <- getWorkspacePane
    displayPane l False


-- | Toggles a row in a `TreeView`
treeViewToggleRow treeView path = do
    expanded <- treeViewRowExpanded treeView path
    if expanded
        then treeViewCollapseRow treeView path
        else treeViewExpandRow   treeView path False


-- | Deletes the Workspace pane and rebuilds it (used when enabling/disabling
-- icons, since it requires extra/fewer cellrenderers)
rebuildWorkspacePane :: IDEAction
rebuildWorkspacePane = do
    mbWsPane <- getPane :: IDEM (Maybe WorkspacePane)
    forM_ mbWsPane closePane
    getOrBuildPane (Right "*Workspace") :: IDEM (Maybe WorkspacePane)
    return ()


-- | Searches the workspace packages if it is part of any of them
fileGetPackage :: FilePath -> WorkspaceM (Maybe IDEPackage)
fileGetPackage path = do
    packages <- wsAllPackages <$> ask
    let dirs     = [p | p <- packages, takeDirectory (ipdCabalFile p) `isPrefixOf` path]
    return (listToMaybe dirs)



-- * Actions for refreshing the Workspace pane



-- | Refreshes the Workspace pane, lists all packages and synchronizes the expanded
-- nodes with the file system and workspace
refreshWorkspacePane :: IDEAction
refreshWorkspacePane = do
    liftIO $ debugM "leksah" "refreshWorkspacePane"
    workspace <- getWorkspacePane
    refresh workspace

-- | Needed when building the pane, since getWorkspacePane does not
-- work before the building is finished
refresh :: WorkspacePane -> IDEAction
refresh pane = do
    let store = recordStore pane
    let view  = treeView pane

    workspaceTryQuiet $ do
        packages <- sort . wsPackages <$> ask

        activePackage <- readIDE activePack
        forest <- forM packages $ \pkg -> do
            let record = PackageRecord pkg
            subForest <- flip runPackage pkg $ subTrees record
            return (Node record subForest)

        liftIDE $ setEntries store [] forest


-- | Mutates the 'TreeStore' with the given TreePath as root to attach new
-- entries to. Walks the directory tree recursively when refreshing directories.
refreshPackageTreeFrom :: TreeStore WorkspaceRecord -> TreeView -> TreePath -> PackageAction
refreshPackageTreeFrom store view path = do
    record     <- liftIO $ treeStoreGetValue store path
    Just pkg   <- liftIDE $ treePathToPackage store path
    expandable <- liftIDE $ canExpand record pkg

    forest     <- subTrees record

    liftIDE $ setEntries store path forest





-- | Returns the subtrees of the 'WorkspaceRecord'.
subTrees :: WorkspaceRecord -> PackageM (Forest WorkspaceRecord)
subTrees record = case record of
    DirRecord dir _     -> do
        records <- dirRecords dir
        mapM (\r -> Node r <$> subTrees r) records
    ComponentsRecord    -> do
        records <- componentsRecords
        mapM (\r -> Node r <$> subTrees r) records
    AddSourcesRecord    -> do
        records <- addSourcesRecords
        mapM (\r -> Node r <$> subTrees r) records
    AddSourceRecord pkg -> return []
    PackageRecord pkg   -> do
        p <- ask
        let dirRecord = DirRecord (ipdPackageDir p) False
        compForest <- subTrees ComponentsRecord
        addSourcesForest <- subTrees AddSourcesRecord
        dirForest <- subTrees dirRecord
        return [ Node ComponentsRecord compForest
               , Node AddSourcesRecord addSourcesForest
               , Node dirRecord dirForest]
    _                   -> return []


-- | Gets the direct children, the add source dependencies
addSourcesRecords :: PackageM [WorkspaceRecord]
addSourcesRecords = do
    pkg <- ask
    return $ sort $ map AddSourceRecord (ipdSandboxSources pkg)


-- | Returns the contents at the given 'FilePath' as 'WorkspaceRecord's.
-- Runs in the PackageM monad to determine if directories are
-- source directories (as specified in the cabal file)
dirRecords :: FilePath -> PackageM [WorkspaceRecord]
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
                                  -- It's not a descendant of the package directory (e.g. in a source dependency)
                                  return $ DirRecord full False
                      else return $ FileRecord full
   return (sort records)


-- | Get the components for a specific package
componentsRecords :: PackageM [WorkspaceRecord]
componentsRecords = do
    package         <- ask
    mbActivePackage <- readIDE activePack
    activeComponent <- readIDE activeExe

    return $ sort $ map (\comp -> ComponentRecord comp) (components package)

    where
        components package = maybeToList (ipdLib package)
                          ++ ipdExes package
                          ++ ipdTests package
                          ++ ipdBenchmarks package


-- | Recursively sets the subtrees of the given 'TreePath' to the provided tree of 'WorkspaceRecord's. If a record
-- is already present, it is kept in the same (expanded) state.
setEntries :: TreeStore WorkspaceRecord -> TreePath -> Forest WorkspaceRecord -> IDEAction
setEntries store [] [] = liftIO $ treeStoreClear store
setEntries store parentPath forest = reifyIDE $ \ideR -> do
    forM_ (zip [0..] (map rootLabel forest)) $ \(n, record) -> do
        mbParentIter <- treeModelGetIter store parentPath
        mbChildIter <- treeModelIterNthChild store mbParentIter n
        let compare rec1 rec2 = case (rec1, rec2) of
                (PackageRecord p1, PackageRecord p2) -> ipdCabalFile p1 == ipdCabalFile p2
                _ -> rec1 == rec2
        findResult <- searchToRight compare record store mbChildIter
        case (mbChildIter, findResult) of
            (_, WhereExpected iter) -> do -- it's already there
                path <- treeModelGetPath store iter
                treeStoreSetValue store path record
            (Just iter, Found _) -> do -- it's already there at a later sibling
                path <- treeModelGetPath store iter
                removeUntil record store path
            _ -> do
                treeStoreInsert store parentPath n record

    -- Recursively set the subforests
    forM_ (zip [0..] (map subForest forest)) $ \(n, subForest) -> do
        reflectIDE (setEntries store (parentPath ++ [n]) subForest) ideR


    if null forest then do
        treeStoreRemoveChildren store parentPath
    else do
        void $ removeRemaining store (parentPath++[length forest])


-- * Context menu

contextMenuItems :: WorkspaceRecord -> TreePath -> TreeStore WorkspaceRecord -> IDEM [[(Text, IDEAction)]]
contextMenuItems record path store = do
    case record of
        (FileRecord fp) -> do
            let onDeleteFile = flip catchIDE (\(e :: SomeException) -> print e) $ reifyIDE $ \ideRef -> do
                    showDialogOptions
                        ("Are you sure you want to delete " <> T.pack (takeFileName fp) <> "?")
                        MessageQuestion
                        [ ("Delete File", removeFile fp >> reflectIDE refreshWorkspacePane ideRef)
                        , ("Cancel", return ())
                        ]
                        (Just 0)
            return [[("Open File...", void $ goToSourceDefinition' fp (Location "" 1 0 1 0))]
                   ,[("Delete File...", onDeleteFile)]]

        DirRecord fp _ -> do

            let onNewModule = flip catchIDE (\(e :: SomeException) -> print e) $ do
                    mbPkg <- treePathToPackage store path
                    forM_ mbPkg $ \pkg -> do
                        mbWs <- readIDE workspace
                        forM_ mbWs $ \ws -> do
                            mbModulePath <- flip runWorkspace ws $ runPackage (dirToModulePath fp) pkg
                            let modulePrefix = fromMaybe [] mbModulePath
                            packageTry $ addModule modulePrefix
                            refreshWorkspacePane

            let onNewTextFile = flip catchIDE (\(e :: SomeException) -> print e) $ reifyIDE $ \ideRef -> do
                    mbText <- showInputDialog "File name:" ""
                    case mbText of
                        Just t  -> do
                            let path = fp </> T.unpack t
                            exists <- doesFileExist path
                            if exists
                                then showErrorDialog "File already exists"
                                else do
                                    writeFile path ""
                                    void $ reflectIDE (refreshWorkspacePane >> goToSourceDefinition' path (Location "" 1 0 1 0)) ideRef
                        Nothing -> return ()

            let onNewDir = flip catchIDE (\(e :: SomeException) -> print e) $ reifyIDE $ \ideRef -> do
                    mbText <- showInputDialog "Directory name:" ""
                    case mbText of
                        Just t  -> do
                            let path = fp </> T.unpack t
                            exists <- doesDirectoryExist path
                            if exists
                                then showErrorDialog "Directory already exists"
                                else do
                                    createDirectory path
                                    void $ reflectIDE refreshWorkspacePane ideRef
                        Nothing -> return ()

            let onDeleteDir = flip catchIDE (\(e :: SomeException) -> print e) $ reifyIDE $ \ideRef -> do
                    showDialogOptions
                        ("Are you sure you want to delete " <> T.pack (takeFileName fp) <> "?")
                        MessageQuestion
                        [ ("Delete directory", removeDirectoryRecursive fp >> reflectIDE refreshWorkspacePane ideRef)
                        , ("Cancel", return ())
                        ]
                        (Just 0)

            return [ [ ("New Module...", onNewModule)
                     , ("New Text File...", onNewTextFile)
                     , ("New Directory...", onNewDir)
                     ]
                   , [ ("Delete Directory...", onDeleteDir)
                     ]
                   ]

        PackageRecord p -> do

            let onSetActive = workspaceTryQuiet $ workspaceActivatePackage p Nothing
                onAddModule = workspaceTryQuiet $ runPackage (addModule []) p
                onOpenCabalFile = workspaceTryQuiet $ runPackage packageEditText p
                onRemoveFromWs = workspaceTryQuiet $ do
                    workspaceRemovePackage p
                    liftIDE refreshWorkspacePane

            return [ [ ("New Module...", onAddModule)
                     , ("Set As Active Package", onSetActive)

                     ]
                   , [ ("Build", workspaceTryQuiet $ runPackage makePackage p)
                     , ("Run", workspaceTryQuiet $ runPackage packageRun p)
                     , ("Test", workspaceTryQuiet $ runPackage packageTest p)
                     , ("Clean", workspaceTryQuiet $ runPackage packageClean p)
                     , ("Open Package File", onOpenCabalFile)
                     ]
                   , [
                     ("Remove From Workspace", onRemoveFromWs)
                     ]
                   ]

        ComponentRecord comp -> do
            Just pkg <- treePathToPackage store path
            let onSetActive = workspaceTryQuiet $
                                  workspaceActivatePackage pkg (Just comp)
            return [[ ("Activate component", onSetActive) ]]

        AddSourcesRecord -> do
            Just pkg <- treePathToPackage store path
            let onAddSource snapshot = workspaceTryQuiet $ flip runPackage pkg (sandboxAddSource snapshot)
            return [ [ ("Add Source Dependency...", onAddSource False >> refreshWorkspacePane)
                     , ("Add Source Dependency Snapshot...", onAddSource True >> refreshWorkspacePane)
                     ]
                   ]


        AddSourceRecord p -> do
            Just pkg <- treePathToPackage store path
            let onRemoveSourceDependency  = do
                    workspaceTryQuiet $ flip runPackage pkg (sandboxDeleteSource (ipdPackageDir p))
                onAddSourceDependencyToWs = workspaceTryQuiet . void $
                    workspaceAddPackage' (ipdCabalFile p)

            return [ [ ("Add Package To Workspace", onAddSourceDependencyToWs >> refreshWorkspacePane)
                     ]
                   , [ ("Delete Source Dependency", onRemoveSourceDependency >> refreshWorkspacePane)
                     ]
                   ]
        _ -> return []


-- | Searches the source folders to determine what the corresponding
--   module path is
dirToModulePath :: FilePath -> PackageM (Maybe [Text])
dirToModulePath fp = do
    pkgDir <- ipdPackageDir <$> ask
    srcDirs <- map (pkgDir <>) . ipdSrcDirs <$> ask
    return $ do
        srcDir <- find (`isPrefixOf` fp) srcDirs
        let suffix = if srcDir == fp then "" else makeRelative srcDir fp
        let dirs   = map (T.pack . capitalize) (splitDirectories suffix)
        return dirs
    where
        capitalize (x:xs) = toUpper x : xs
        capitalize [] = []


-- * Utility functions for operating on 'TreeStore'



leaf :: a -> Tree a
leaf x = Node x []

treeStoreRemoveChildren :: TreeStore a -> TreePath -> IO ()
treeStoreRemoveChildren store path = do
    Node record children <- treeStoreGetTree store path
    forM_ (zip [0..] children) $ \_ -> do
        treeStoreRemove store (path ++ [0]) -- this works because mutation ...

data FindResult = WhereExpected TreeIter | Found TreeIter | NotFound

-- | Tries to find the given value in the 'TreeStore'. Only looks at the given 'TreeIter' and its
-- sibling nodes to the right.
-- Returns @WhereExpected iter@ if the records is found at the provided 'TreeIter'
-- Returns @Found iter@ if the record is found at a sibling iter
-- Returns @NotFound@ otherwise
searchToRight :: (a -> a -> Bool) -> a -> TreeStore a -> Maybe TreeIter -> IO FindResult
searchToRight compare _ _ Nothing = return NotFound
searchToRight compare a store (Just iter) = do
    row <- treeModelGetRow store iter
    if compare row a
        then return $ WhereExpected iter
        else treeModelIterNext store iter >>= find'
  where
    find' :: Maybe TreeIter -> IO FindResult
    find' Nothing = return NotFound
    find' (Just iter) = do
        row <- treeModelGetRow store iter
        if compare row a
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
