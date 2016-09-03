{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Maybe
       (fromJust, fromMaybe, maybeToList, listToMaybe, isJust)
import Control.Monad (forM, void, when)
import Data.Foldable (forM_)
import Data.Typeable (Typeable)
import IDE.Core.State
       (onIDE, catchIDE, window, getIDE, MessageLevel(..), ipdPackageId,
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
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils
       (showErrorDialog, showInputDialog, treeViewContextMenu', __,
        showDialogOptions, treeViewToggleRow)
import Control.Exception (SomeException(..), catch)
import Data.Text (Text)
import qualified Data.Text as T
       (isPrefixOf, words, isSuffixOf, unpack, pack)
import Data.Monoid ((<>))
import IDE.Core.Types
       (ipdLib, WorkspaceAction, Workspace(..), wsAllPackages, WorkspaceM,
        runPackage, runWorkspace, PackageAction, PackageM, IDEPackage(..),
        IDE(..), Prefs(..), MonadIDE(..), ipdPackageDir)
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
import IDE.Pane.Modules (addModule)
import IDE.Pane.PackageEditor (packageEditText)
import IDE.Package (packageTest, packageRun, packageClean,packageBench)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.GI.Gtk.ModelView.ForestStore
       (forestStoreGetTree, forestStoreGetValue, ForestStore(..),
        forestStoreRemove, forestStoreInsert, forestStoreSetValue,
        forestStoreClear, forestStoreNew)
import GI.Gtk.Structs.TreeIter (treeIterCopy, TreeIter(..))
import Data.GI.Gtk.ModelView.TreeModel
       (treeModelIterNext, treeModelIterNthChild, treeModelGetIter,
        treeModelGetPath)
import GI.Gtk.Structs.TreePath
       (TreePath(..))
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew, ScrolledWindow(..))
import GI.Gtk.Objects.TreeView
       (treeViewRowExpanded, onTreeViewRowActivated,
        onTreeViewRowExpanded, treeViewGetSelection,
        treeViewSetHeadersVisible, treeViewAppendColumn, treeViewSetModel,
        treeViewNew, TreeView(..))
import GI.Gtk.Objects.Widget (afterWidgetFocusInEvent, toWidget)
import GI.Gtk.Objects.TreeViewColumn
       (treeViewColumnSetReorderable, treeViewColumnSetResizable,
        treeViewColumnSetSizing, treeViewColumnNew)
import GI.Gtk.Enums
       (MessageType(..), PolicyType(..), ShadowType(..),
        TreeViewColumnSizing(..))
import GI.Gtk.Objects.CellRendererPixbuf
       (setCellRendererPixbufStockId, cellRendererPixbufNew)
import GI.Gtk.Interfaces.CellLayout (cellLayoutPackStart)
import Data.GI.Base (set)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunc', cellLayoutSetDataFunction)
import GI.Gtk.Objects.CellRendererText
       (setCellRendererTextMarkup, cellRendererTextNew)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Container (containerAdd)
import Data.GI.Gtk.ModelView.CustomStore
       (customStoreGetRow)
import Data.Int (Int32)
import Data.GI.Gtk.ModelView.Types
       (treePathGetIndices', treePathNewFromIndices')


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

    return $ case record of
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


-- | The icon to show for a record
toIcon :: WorkspaceRecord -> Text
toIcon record = case record of
    FileRecord path
        | takeExtension path == ".hs"    -> "ide_source"
        | takeExtension path == ".cabal" -> "ide_cabal_file"
    DirRecord p isSrc
        | isSrc     -> "ide_source_folder"
        | otherwise -> "ide_folder"
    PackageRecord _ -> "ide_package"
    ComponentsRecord -> "ide_component"
    AddSourcesRecord -> "ide_source_dependency"
    AddSourceRecord _ -> "ide_package"
    _ -> ""


-- | Gets the package to which a node in the tree belongs
iterToPackage :: ForestStore WorkspaceRecord -> TreeIter -> IDEM (Maybe IDEPackage)
iterToPackage store iter = do
    path <- treeModelGetPath store iter
    treePathToPackage store path

-- | Gets the package to which a node in the tree belongs
treePathToPackage :: ForestStore WorkspaceRecord -> TreePath -> IDEM (Maybe IDEPackage)
treePathToPackage store p = treePathGetIndices' p >>= treePathToPackage' store

treePathToPackage' :: ForestStore WorkspaceRecord -> [Int32] -> IDEM (Maybe IDEPackage)
treePathToPackage' store (n:_) = do
    record <- forestStoreGetValue store =<< treePathNewFromIndices' [n]
    case record of
        (PackageRecord pkg) -> return (Just pkg)
        _                     -> do
            liftIO $ debugM "leksah" "treePathToPackage: Unexpected entry at root forest"
            return Nothing
treePathToPackage' _ _ = do
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
,   recordStore     ::   ForestStore WorkspaceRecord
} deriving Typeable


-- | The additional state used when recovering the pane
--   (none)
data WorkspaceState = WorkspaceState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane WorkspacePane IDEM where
    primPaneName _  =   __ "Workspace"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . scrolledView
    paneId b        =   "*Workspace"

instance RecoverablePane WorkspacePane WorkspaceState IDEM where
    saveState p     =   return (Just WorkspaceState)
    recoverState pp WorkspaceState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = do
        ideR <- ask
        recordStore <-  forestStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView (Just recordStore)

        col1        <- treeViewColumnNew
        treeViewColumnSetSizing col1 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        treeViewAppendColumn treeView col1

        prefs <- readIDE prefs
        when (showWorkspaceIcons prefs) $ do
            renderer2    <- cellRendererPixbufNew
            cellLayoutPackStart col1 renderer2 False
            setCellRendererPixbufStockId renderer2 ""
            cellLayoutSetDataFunction col1 renderer2 recordStore
                $ setCellRendererPixbufStockId renderer2 . toIcon

        renderer1    <- cellRendererTextNew
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetDataFunc' col1 renderer1 recordStore $ \iter -> do
            record <- customStoreGetRow recordStore iter
            mbPkg  <- flip reflectIDE ideR $ iterToPackage recordStore iter
            forM_ mbPkg $ \pkg -> do
                -- The cellrenderer is stateful, so it knows which cell this markup will be for (the cell at iter)
                markup <- flip reflectIDE ideR $ toMarkup record pkg
                forM_ mbPkg $ \pkg -> setCellRendererTextMarkup renderer1 markup

        -- treeViewSetActiveOnSingleClick treeView True
        treeViewSetHeadersVisible treeView False
        sel <- treeViewGetSelection treeView
        -- treeSelectionSetMode sel SelectionModeSingle

        scrolledView <- scrolledWindowNew noAdjustment noAdjustment
        scrolledWindowSetShadowType scrolledView ShadowTypeIn
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyTypeAutomatic PolicyTypeAutomatic

        let wsPane = WorkspacePane {..}

        cid1 <- onIDE afterWidgetFocusInEvent treeView $ do
            liftIDE $ makeActive wsPane
            return True

        onTreeViewRowExpanded treeView $ \iter path -> do
            record <- forestStoreGetValue recordStore path
            mbPkg  <- flip reflectIDE ideR $ iterToPackage recordStore iter
            forM_ mbPkg $ \pkg -> do
                flip reflectIDE ideR $ do
                    workspaceTryQuiet $ do
                        runPackage (refreshPackageTreeFrom recordStore treeView path) pkg

        onTreeViewRowActivated treeView $ \path col -> do
            record <- forestStoreGetValue recordStore path
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

        cids2 <- treeViewContextMenu' treeView recordStore contextMenuItems
        refresh wsPane

        return (Just wsPane, cid1:cids2)

-- | Get the Workspace pane
getWorkspacePane :: IDEM WorkspacePane
getWorkspacePane = forceGetPane (Right "*Workspace")


-- | Show the Workspace pane
showWorkspacePane :: IDEAction
showWorkspacePane = do
    l <- getWorkspacePane
    displayPane l False


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
        setChildren Nothing store view [] (map PackageRecord packages)


-- | Mutates the 'ForestStore' with the given TreePath as root to attach new
-- entries to. Walks the directory tree recursively when refreshing directories.
refreshPackageTreeFrom :: ForestStore WorkspaceRecord -> TreeView -> TreePath -> PackageAction
refreshPackageTreeFrom store view path = do
    record     <- liftIO $ forestStoreGetValue store path
    Just pkg   <- liftIDE $ treePathToPackage store path
    expandable <- liftIDE $ canExpand record pkg

    kids     <- children record
    path' <- treePathGetIndices' path
    lift $ setChildren (Just pkg) store view path' kids

-- | Returns the children of the 'WorkspaceRecord'.
children :: WorkspaceRecord -> PackageM [WorkspaceRecord]
children record = case record of
    DirRecord dir _     -> dirRecords dir
    ComponentsRecord    -> componentsRecords
    AddSourcesRecord    -> addSourcesRecords
    AddSourceRecord pkg -> return []
    PackageRecord pkg   -> do
        p <- ask
        return [ ComponentsRecord
               , AddSourcesRecord
               , DirRecord (ipdPackageDir p) False]
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


-- | Recursively sets the children of the given 'TreePath' to the provided tree of 'WorkspaceRecord's. If a record
-- is already present, it is kept in the same (expanded) state.
-- If a the parent record is not expanded just makes sure at least one of
-- the chldren is added.
setChildren :: Maybe IDEPackage -> ForestStore WorkspaceRecord -> TreeView -> [Int32] -> [WorkspaceRecord] -> WorkspaceAction
setChildren _ store _ [] [] = liftIO $ forestStoreClear store
setChildren mbPkg store view parentPath kids = do
    -- We only need to get all the children right when they are visible
    expanded <- if null parentPath
                    then return True
                    else liftIO $ treeViewRowExpanded view =<< treePathNewFromIndices' parentPath

    let kidsToAdd = (if expanded
                            then id
                            else take 1) kids

    forM_ (zip [0..] kidsToAdd) $ \(n, record) -> do
      liftIO $ do
        mbChildIter <- (treeModelGetIter store =<< treePathNewFromIndices' parentPath) >>= \case
            Just parentIter ->
                treeModelIterNthChild store (Just parentIter) n >>= \case
                    (True, childIter) -> return (Just childIter)
                    (False, _)        -> return Nothing
            Nothing         -> return Nothing
        let compare rec1 rec2 = case (rec1, rec2) of
                (PackageRecord p1, PackageRecord p2) -> ipdCabalFile p1 == ipdCabalFile p2
                _ -> rec1 == rec2
        findResult <- searchToRight compare record store mbChildIter
        case (mbChildIter, findResult) of
            (_, WhereExpected iter) -> do -- it's already there
                path <- treeModelGetPath store iter
                forestStoreSetValue store path record
            (Just iter, Found _) -> do -- it's already there at a later sibling
                path <- treeModelGetPath store iter
                removeUntil record store path
            _ -> do
                parentPath' <- treePathNewFromIndices' parentPath
                forestStoreInsert store parentPath' (fromIntegral n) record

      let pkg = case record of
                        PackageRecord p -> p
                        _               -> fromJust mbPkg
      -- Only update the grand kids if they are visible
      when expanded $ do
          grandKids <- (`runPackage` pkg) $ children record
          setChildren (Just pkg) store view (parentPath ++ [n]) grandKids

    liftIO $ if null kids
        then forestStoreRemoveChildren store parentPath
        else when expanded . void $ removeRemaining store =<< treePathNewFromIndices' (parentPath++[fromIntegral $ length kids])


-- * Context menu

contextMenuItems :: WorkspaceRecord -> TreePath -> ForestStore WorkspaceRecord -> IDEM [[(Text, IDEAction)]]
contextMenuItems record path store = do
    case record of
        (FileRecord fp) -> do
            let onDeleteFile = flip catchIDE (\(e :: SomeException) -> print e) $ reifyIDE $ \ideRef -> do
                    showDialogOptions
                        ("Are you sure you want to delete " <> T.pack (takeFileName fp) <> "?")
                        MessageTypeQuestion
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
                        MessageTypeQuestion
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
                     , ("Benchmark", workspaceTryQuiet $ runPackage packageBench p)
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
            return []


        AddSourceRecord p -> do
            Just pkg <- treePathToPackage store path
            let onAddSourceDependencyToWs = workspaceTryQuiet . void $
                    workspaceAddPackage' (ipdCabalFile p)

            return [ [ ("Add Package To Workspace", onAddSourceDependencyToWs >> refreshWorkspacePane)
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


-- * Utility functions for operating on 'ForestStore'



leaf :: a -> Tree a
leaf x = Node x []

forestStoreRemoveChildren :: ForestStore a -> [Int32] -> IO ()
forestStoreRemoveChildren store path = do
    Node record children <- forestStoreGetTree store =<< treePathNewFromIndices' path
    forM_ (zip [0..] children) $ \_ ->
        forestStoreRemove store =<< treePathNewFromIndices' (path ++ [0]) -- this works because mutation ...

data FindResult = WhereExpected TreeIter | Found TreeIter | NotFound

-- | Tries to find the given value in the 'ForestStore'. Only looks at the given 'TreeIter' and its
-- sibling nodes to the right.
-- Returns @WhereExpected iter@ if the records is found at the provided 'TreeIter'
-- Returns @Found iter@ if the record is found at a sibling iter
-- Returns @NotFound@ otherwise
searchToRight :: (a -> a -> Bool) -> a -> ForestStore a -> Maybe TreeIter -> IO FindResult
searchToRight compare _ _ Nothing = return NotFound
searchToRight compare a store (Just iter) = do
    row <- customStoreGetRow store iter
    if compare row a
        then return $ WhereExpected iter
        else do
            next <- treeIterCopy iter
            treeModelIterNext store next >>= find' next
  where
    find' :: TreeIter -> Bool -> IO FindResult
    find' _ False = return NotFound
    find' iter True = do
        row <- customStoreGetRow store iter
        if compare row a
            then return $ Found iter
            else do
                next <- treeIterCopy iter
                treeModelIterNext store iter >>= find' next


-- | Starting at the node at the given 'TreePath', removes all sibling nodes to the right
--   until the given value is found.
removeUntil :: Eq a => a -> ForestStore a -> TreePath -> IO ()
removeUntil a store path = do
    row <- forestStoreGetValue store path
    when (row /= a) $ do
        found <- forestStoreRemove store path
        when found $ removeUntil a store path


-- | Starting at the node at the given 'TreePath', removes all sibling nodes to the right
removeRemaining :: ForestStore a -> TreePath -> IO ()
removeRemaining store path = do
    found <- forestStoreRemove store path
    when found $ removeRemaining store path
