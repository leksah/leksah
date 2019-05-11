{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.PackageEditor
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info@leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for editing of cabal packages and build infos
--
-----------------------------------------------------------------------------------


module IDE.Pane.PackageEditor (
    packageNew'
,   packageClone
,   packageEdit
,   projectEditText
,   packageEditText
,   choosePackageDir
,   choosePackageFile

,   hasConfigs
,   standardSetup
) where

import Prelude ()
import Prelude.Compat

import Control.Event (EventSource(..))
import Control.Exception (SomeException(..))
import Control.Applicative (Applicative, (<*>), (<$>))
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)

import Data.Char (isDigit, isAlphaNum)
import Data.Conduit (ConduitT)
import qualified Data.Conduit as C (ZipSink(..), getZipSink)
import qualified Data.Conduit.List as CL (fold)
import Data.Default (Default(..))
import Data.List (isPrefixOf, sort, nub, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe
       (isJust, fromMaybe, mapMaybe, fromJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
       (replace, span, splitAt, isPrefixOf, length, toLower, lines,
        unlines, pack, unpack, null)
import qualified Data.Text.IO as T (writeFile, readFile)
import Data.Typeable (Typeable)
import Data.Void (Void)

import Distribution.Compiler (CompilerFlavor(..))
import Distribution.License (License(..))
import Distribution.ModuleName (ModuleName)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse (readGenericPackageDescription)
#endif
import Distribution.Simple (Extension(..), VersionRange, anyVersion)
import Distribution.Text (simpleParse, display)
import Distribution.Types.ExecutableScope (ExecutableScope(..))
import Distribution.Types.ForeignLib (ForeignLib(..))
import Distribution.Types.UnqualComponentName
       (UnqualComponentName, mkUnqualComponentName,
        unUnqualComponentName)
import Distribution.Verbosity
import Distribution.Version
       (versionNumbers, mkVersion, orLaterVersion)

import Language.Haskell.Extension (Language(..))

import System.Exit (ExitCode(..))
import System.FilePath
       (takeBaseName, takeDirectory, takeFileName, (</>))
import System.Directory
       (removeFile, renameDirectory, doesFileExist, getDirectoryContents,
        createDirectory, removeDirectoryRecursive, doesDirectoryExist,
        createDirectoryIfMissing)

import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)

import Data.GI.Base (unsafeCastTo, new')
import GI.Gtk.Enums
       (Align(..), ShadowType(..), WindowPosition(..), ButtonsType(..),
        MessageType(..), ResponseType(..), FileChooserAction(..),
        Orientation(..))
import GI.Gtk.Objects.Box (boxNew, Box(..))
import GI.Gtk.Objects.Button (onButtonClicked, buttonNewWithLabel)
import GI.Gtk.Objects.ButtonBox (buttonBoxNew)
import GI.Gtk.Objects.CellRendererText (setCellRendererTextText)
import GI.Gtk.Objects.Dialog
       (Dialog(..), constructDialogUseHeaderBar, dialogGetContentArea)
import GI.Gtk.Objects.Label (labelSetMarkup, labelNew)
import GI.Gtk.Objects.MessageDialog
       (setMessageDialogText, constructMessageDialogButtons, setMessageDialogMessageType,
        MessageDialog(..))
import GI.Gtk.Objects.Notebook (Notebook(..))
import GI.Gtk.Objects.Window
       (windowSetTransientFor, setWindowWindowPosition, setWindowTitle,
        setWindowTransientFor, Window(..))
import GI.Gtk.Objects.Widget
       (widgetSetSensitive, toWidget, widgetDestroy, widgetShowAll,
        widgetGrabDefault, setWidgetCanDefault)

import Graphics.UI.Editor.Basics
       (Notifier, Editor, GUIEventSelector(..), GUIEvent(..))
import Graphics.UI.Editor.Composite
       (versionEditor, versionRangeEditor,
        dependenciesEditor, textsEditor, filesEditor, tupel3Editor,
        eitherOrEditor, maybeEditor, pairEditor, ColumnDescr(..),
        multisetEditor)
import Graphics.UI.Editor.MakeEditor
       (FieldDescription(..), extract,
        flattenFieldDescription, buildEditor, mkField)
import Graphics.UI.Editor.Parameters
       (paraMargin, paraVAlign, paraHAlign, boxPackEnd',
        dialogSetDefaultResponse', dialogRun', Packing(..), boxPackStart',
        dialogAddButton', Parameter(..), paraPack,
        paraOrientation, paraMinSize, paraShadow, paraSynopsis, (<<<-),
        emptyParams, paraName)
import Graphics.UI.Editor.Simple
       (stringEditor, comboEntryEditor, staticListMultiEditor,
        intEditor, boolEditor, fileEditor, comboSelectionEditor,
        multilineStringEditor, textEditor)

import IDE.Core.State
       (IDEM, PackageAction, ProjectAction, Project, IDEAction, liftIDE,
        IDEPackage(..), ipdPackageDir, ideMessage, MessageLevel(..),
        catchIDE, pjFile, reifyIDE, sysMessage, reflectIDE, throwIDE,
        pjKey, pjFileOrDir, ProjectKey, pjDir)
import IDE.Gtk.State
       (Pane(..), RecoverablePane(..), PanePath, Connections,
        getWindows, getMainWindow, getBestPathForId, bringPaneToFront,
        getNotebook, buildThisPane, markLabel)
import IDE.Metainfo.Provider (getAllPackageIds)
import IDE.Pane.SourceBuffer (fileOpenThis)
import IDE.Utils.CabalUtils (writeGenericPackageDescription')
import IDE.Utils.ExternalTool (runExternalTool')
import IDE.Utils.FileUtils
       (isEmptyDirectory, cabalFileName, allModules)
import IDE.Utils.GUIUtils
import IDE.Utils.Tool (ToolOutput(..))

#if !MIN_VERSION_Cabal(2,4,0)
allBuildDepends :: PackageDescription -> [Dependency]
allBuildDepends = buildDepends
#endif

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

-- | Get the last item
sinkLast :: ConduitT a o IDEM (Maybe a)
sinkLast = CL.fold (\_ a -> Just a) Nothing

--------------------------------------------------------------------------
-- Handling of Generic Package Descriptions

toGenericPackageDescription :: PackageDescription -> GenericPackageDescription
toGenericPackageDescription pd =
    GenericPackageDescription
        { packageDescription = pd
            { library = Nothing
            , executables = []
            , testSuites = []
            , benchmarks = []
#if !MIN_VERSION_Cabal(2,4,0)
            , buildDepends = []
#endif
            }
        , genPackageFlags = []
        , condLibrary = case library pd of
                            Nothing -> Nothing
                            Just lib -> Just (buildCondTreeLibrary lib)
        , condExecutables  = map buildCondTreeExe (executables pd)
        , condTestSuites   = map buildCondTreeTest (testSuites pd)
        , condBenchmarks   = map buildCondTreeBenchmark (benchmarks pd)
        , condForeignLibs  = map buildCondTreeForeignLib (foreignLibs pd)
        , condSubLibraries = concatMap buildCondTreeSubLibraries (subLibraries pd)
        }
  where
    buildCondTreeLibrary lib =
        CondNode {
            condTreeData = lib { libBuildInfo = (libBuildInfo lib) { targetBuildDepends = allBuildDepends pd } },
            condTreeConstraints = [],
            condTreeComponents = []}
    buildCondTreeExe exe =
        (exeName exe, CondNode {
            condTreeData = exe {
#if MIN_VERSION_Cabal(2,0,0) && !MIN_VERSION_Cabal(2,1,0)
                -- Without this Cabal 2.0.0.2 ppCondExecutables is broken (it is missing some parens).
                exeScope = ExecutablePublic,
#endif
                buildInfo = (buildInfo exe) { targetBuildDepends = allBuildDepends pd } },
            condTreeConstraints = [],
            condTreeComponents = []})
    buildCondTreeTest test =
        (testName test, CondNode {
            condTreeData = test { testBuildInfo = (testBuildInfo test) { targetBuildDepends = allBuildDepends pd } },
            condTreeConstraints = [],
            condTreeComponents = []})
    buildCondTreeBenchmark bm =
        (benchmarkName bm, CondNode {
            condTreeData = bm { benchmarkBuildInfo = (benchmarkBuildInfo bm) { targetBuildDepends = allBuildDepends pd } },
            condTreeConstraints = [],
            condTreeComponents = []})
    buildCondTreeForeignLib fl =
        (foreignLibName fl, CondNode {
            condTreeData = fl { foreignLibBuildInfo = (foreignLibBuildInfo fl) { targetBuildDepends = allBuildDepends pd } },
            condTreeConstraints = [],
            condTreeComponents = []})
    buildCondTreeSubLibraries Library{libName = Nothing} = []
    buildCondTreeSubLibraries sl@Library{libName = Just ln} = [
        (ln, CondNode {
            condTreeData = sl { libBuildInfo = (libBuildInfo sl) { targetBuildDepends = allBuildDepends pd } },
            condTreeConstraints = [],
            condTreeComponents = []})]

-- ---------------------------------------------------------------------
-- The exported stuff goes here
--

choosePackageDir :: Window -> Maybe FilePath -> IO (Maybe FilePath)
choosePackageDir window
  = chooseDir window (__ "Select root folder for package")

choosePackageFile :: Window -> Maybe FilePath -> IO (Maybe FilePath)
choosePackageFile window mbfp
  = chooseFile window (__ "Select cabal package file (.cabal)") mbfp [("Cabal Package Files", ["*.cabal"])]

packageEdit :: PackageAction
packageEdit = do
    idePackage <- ask
    liftIDE $ do
        let dirName = ipdPackageDir idePackage
        modules <- liftIO $ allModules dirName
        package <- liftIO $ readGenericPackageDescription normal (ipdCabalFile idePackage)
        if hasConfigs package
            then do
                liftIDE $ ideMessage High
                    (__ "Cabal file with configurations can't be edited with the current version of the editor")
                liftIDE $ fileOpenThis $ ipdCabalFile idePackage
                return ()
            else do
                let flat = flattenPackageDescription package
                if hasUnknownTestTypes flat || hasUnknownBenchmarkTypes flat
                    then do
                        liftIDE $ ideMessage High
                            (__ "Cabal file with tests or benchmarks of this type can't be edited with the current version of the editor")
                        liftIDE $ fileOpenThis $ ipdCabalFile idePackage
                        return ()
                    else do
                        liftIDE $ editPackage flat dirName  modules (\ _ -> return ())
                        return ()
      `catchIDE`
        (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

projectEditText :: ProjectAction
projectEditText = do
    project <- ask
    case pjFile $ pjKey project of
        Nothing -> ideMessage High "Unable to edit custom project"
        Just f -> liftIDE $ fileOpenThis f
    return ()

packageEditText :: PackageAction
packageEditText = do
    idePackage <- ask
    liftIDE $ fileOpenThis $ ipdCabalFile idePackage
    return ()

hasConfigs :: GenericPackageDescription -> Bool
hasConfigs gpd =
    let libConds = case condLibrary gpd of
                        Nothing -> False
                        Just condTree -> not (null (condTreeComponents condTree))
        exeConds = foldr (\ (_,condTree) hasConfigs' ->
                                (hasConfigs' || not (null (condTreeComponents condTree))))
                        False (condExecutables gpd)
        testConds = foldr (\ (_,condTree) hasConfigs' ->
                                (hasConfigs' || not (null (condTreeComponents condTree))))
                        False (condTestSuites gpd)
    in libConds || exeConds || testConds

hasUnknownTestTypes :: PackageDescription -> Bool
hasUnknownTestTypes pd =
    any unknown $ testSuites pd
  where
    unknown TestSuite {testInterface = TestSuiteExeV10 _ _} = False
    unknown _ = True

hasUnknownBenchmarkTypes :: PackageDescription -> Bool
hasUnknownBenchmarkTypes pd =
    any unknown $ benchmarks pd
  where
    unknown Benchmark {benchmarkInterface = BenchmarkExeV10 _ _} = False
    unknown _ = True

data NewPackage = NewPackage {
    newPackageName :: Text,
    newPackageParentDir :: FilePath,
    templatePackage :: Maybe Text,
    newPackageProject :: Maybe ProjectKey }

packageFields :: FilePath -> [Project] -> FieldDescription NewPackage
packageFields workspaceDir projects = VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "New package name")
                    $ emptyParams)
            newPackageName
            (\ a b -> b{newPackageName = a})
            (textEditor (const True) True),
        mkField
            (paraName <<<- ParaName (__ "Parent directory")
                $ paraMinSize <<<- ParaMinSize (-1, 120)
                    $ emptyParams)
            newPackageParentDir
            (\ a b -> b{newPackageParentDir = a})
            (fileEditor (Just workspaceDir) FileChooserActionSelectFolder "Select"),
        mkField
            (paraName <<<- ParaName (__ "Copy existing package")
                    $ emptyParams)
            templatePackage
            (\ a b -> b{templatePackage = a})
            (maybeEditor "" (comboEntryEditor examplePackages, emptyParams) True ""),
        mkField
            (paraName <<<- ParaName (__ "Add to existing project")
                    $ emptyParams)
            newPackageProject
            (\ a b -> b{newPackageProject = a})
            (comboSelectionEditor (Nothing : map (Just . pjKey) projects) (maybe "" (T.pack . pjFileOrDir)))]

examplePackages :: [Text]
examplePackages = [ "hello"
                  , "gtk2hs-hello"
                  , "ghcjs-dom-hello"
                  , "jsaddle-hello"]

newPackageDialog :: (Applicative m, MonadIO m) => Window -> FilePath -> [Project] -> m (Maybe NewPackage)
newPackageDialog parent workspaceDir projects = do
    let defaultParentDir = maybe workspaceDir (pjDir . pjKey) (listToMaybe projects)
    dia                <- new' Dialog [constructDialogUseHeaderBar 1]
    setWindowTransientFor dia parent
    setWindowTitle dia $ __ "Create New Package"
    upper              <- dialogGetContentArea dia >>= liftIO . unsafeCastTo Box
    (widget,_inj,ext,_) <- liftIO $ buildEditor (packageFields defaultParentDir projects)
                                        (NewPackage "" defaultParentDir Nothing (pjKey <$> listToMaybe projects))
    okButton <- dialogAddButton' dia (__"Create Package") ResponseTypeOk
    _ <- dialogSetDefaultResponse' dia ResponseTypeOk
    _ <- dialogAddButton' dia (__"Cancel") ResponseTypeCancel
    boxPackStart' upper widget PackGrow 7
    setWidgetCanDefault okButton True
    widgetGrabDefault okButton
    widgetShowAll dia
    resp  <- dialogRun' dia
    value <- liftIO $ ext (NewPackage "" defaultParentDir Nothing (pjKey <$> listToMaybe projects))
    widgetDestroy dia
    --find
    case (resp, value) of
        (ResponseTypeOk, Just p)
            | validPackageName . T.unpack $ newPackageName p -> return value
            | otherwise -> do
                liftIO $ showErrorDialog (Just parent) "Invalid package name."
                newPackageDialog parent defaultParentDir projects
        _              -> return Nothing

validPackageName :: String -> Bool
validPackageName = all valid . splitOn "-"
    where
       valid "" = False
       valid w  = all isAlphaNum w && not (all isDigit w)

packageNew' :: FilePath -> [Project] -> ConduitT ToolOutput Void IDEM () -> (Bool -> Maybe Project -> FilePath -> IDEAction) -> IDEAction
packageNew' workspaceDir projects log' activateAction = do
    windows <- getWindows
    mbNewPackage <- newPackageDialog (head windows) workspaceDir projects
    case mbNewPackage of
        Nothing -> return ()
        Just NewPackage{..} | isNothing templatePackage -> do
            let dirName = newPackageParentDir </> T.unpack newPackageName
                mbProject = listToMaybe $ filter ((== newPackageProject) . Just . pjKey) projects
            mbCabalFile <-  liftIO $ cabalFileName dirName
            window <- getMainWindow
            case mbCabalFile of
                Just cfn -> do
                    md <- new' MessageDialog [
                        constructDialogUseHeaderBar 0,
                        constructMessageDialogButtons ButtonsTypeCancel]
                    setMessageDialogMessageType md MessageTypeQuestion
                    setMessageDialogText md $ T.pack (printf (__
                          "There is already file %s in this directory. Would you like to add this package to the project?")
                          (takeFileName cfn))
                    windowSetTransientFor md (Just window)
                    _ <- dialogAddButton' md (__ "_Add Package") (AnotherResponseType 1)
                    dialogSetDefaultResponse' md (AnotherResponseType 1)
                    setWindowWindowPosition md WindowPositionCenterOnParent
                    rid <- dialogRun' md
                    widgetDestroy md
                    when (rid == AnotherResponseType 1) $
                        activateAction False mbProject cfn
                Nothing -> do
                    liftIO $ createDirectoryIfMissing True dirName
                    isEmptyDir <- liftIO $ isEmptyDirectory dirName
                    make <- if isEmptyDir
                        then return True
                        else do
                            md <- new' MessageDialog [
                                    constructDialogUseHeaderBar 0,
                                    constructMessageDialogButtons ButtonsTypeCancel]
                            setMessageDialogMessageType md MessageTypeQuestion
                            setMessageDialogText md . T.pack $ printf (__
                                      "The path you have choosen %s is not an empty directory. Are you sure you want to make a new package here?")
                                      dirName
                            windowSetTransientFor md (Just window)
                            _ <- dialogAddButton' md (__ "_Make Package Here") (AnotherResponseType 1)
                            dialogSetDefaultResponse' md (AnotherResponseType 1)
                            setWindowWindowPosition md WindowPositionCenterOnParent
                            rid <- dialogRun' md
                            widgetDestroy md
                            return $ rid == AnotherResponseType 1
                    when make $ do
                        modules <- liftIO $ allModules dirName
                        let Just initialVersion = simpleParse "0.0.1"
                        editPackage emptyPackageDescription {
                            package   = PackageIdentifier (mkPackageName $ T.unpack newPackageName)
                                                          initialVersion
#if MIN_VERSION_Cabal(2,2,0)
                          , buildTypeRaw = Just Simple
#else
                          , buildType = Just Simple
#endif
                          , specVersionRaw = Right (orLaterVersion (mkVersion [1,12]))
#if MIN_VERSION_Cabal(2,2,0)
                          , licenseRaw = Right AllRightsReserved
#else
                          , license = AllRightsReserved
#endif
#if !MIN_VERSION_Cabal(2,4,0)
                          , buildDepends = [
                                Dependency (mkPackageName "base") anyVersion
                              , Dependency (mkPackageName "QuickCheck") anyVersion
                              , Dependency (mkPackageName "doctest") anyVersion]
#endif
                          , executables = [emptyExecutable {
                                exeName    = mkUnqualComponentName $ T.unpack newPackageName
                              , modulePath = "Main.hs"
                              , buildInfo  = emptyBuildInfo {
                                    hsSourceDirs       = ["src"]
                                  , targetBuildDepends = [Dependency (mkPackageName "base") anyVersion]
                                  , options            = [(GHC, ["-ferror-spans"])]
                                  , defaultLanguage    = Just Haskell2010}}]
                          , testSuites = [emptyTestSuite {
                                    testName = mkUnqualComponentName $ "test-" ++ T.unpack newPackageName
                                  , testInterface = TestSuiteExeV10 (mkVersion [1,0]) "Main.hs"
                                  , testBuildInfo = emptyBuildInfo {
                                        hsSourceDirs    = ["test"]
                                      , targetBuildDepends = [
                                            Dependency (mkPackageName "base") anyVersion
                                          , Dependency (mkPackageName "QuickCheck") anyVersion
                                          , Dependency (mkPackageName "doctest") anyVersion]
                                      , options            = [(GHC, ["-ferror-spans"])]
                                      , defaultLanguage = Just Haskell2010}}]
                          , benchmarks =  []
                          } dirName modules (activateAction True mbProject)
        Just NewPackage{..} -> do
            let mbProject = listToMaybe $ filter ((== newPackageProject) . Just . pjKey) projects
            cabalUnpack newPackageParentDir (fromJust templatePackage) False (Just newPackageName) log' (activateAction False mbProject)

standardSetup :: Text
standardSetup = "#!/usr/bin/runhaskell \n"
                    <> "> module Main where\n"
                    <> "> import Distribution.Simple\n"
                    <> "> main :: IO ()\n"
                    <> "> main = defaultMain\n\n"

data ClonePackageSourceRepo = ClonePackageSourceRepo {
    packageToClone :: Text,
    cloneParentDir :: FilePath}

cloneFields :: [PackageId] -> FilePath -> FieldDescription ClonePackageSourceRepo
cloneFields packages workspaceDir = VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Existing package to copy source repository")
                    $ emptyParams)
            packageToClone
            (\ a b -> b{packageToClone = a})
            (comboEntryEditor ((sort . nub) (map (T.pack . display . pkgName) packages))),
        mkField
            (paraName <<<- ParaName (__ "Parent directory")
                $ paraMinSize <<<- ParaMinSize (-1, 120)
                    $ emptyParams)
            cloneParentDir
            (\ a b -> b{cloneParentDir = a})
            (fileEditor (Just workspaceDir) FileChooserActionSelectFolder "Select")]

clonePackageSourceDialog :: Window -> FilePath -> IDEM (Maybe ClonePackageSourceRepo)
clonePackageSourceDialog parent workspaceDir = do
    packages <- getAllPackageIds
    dia      <- new' Dialog [constructDialogUseHeaderBar 1]
    setWindowTransientFor dia parent
    setWindowTitle dia $ __ "Copy Installed Package"
    upper              <- dialogGetContentArea dia >>= liftIO . unsafeCastTo Box
    (widget,_inj,ext,_) <- liftIO $ buildEditor (cloneFields packages workspaceDir)
                                        (ClonePackageSourceRepo "" workspaceDir)
    okButton <- dialogAddButton' dia (__"Copy Package") ResponseTypeOk
    dialogSetDefaultResponse' dia ResponseTypeOk
    _ <- dialogAddButton' dia (__"Cancel") ResponseTypeCancel
    boxPackStart' upper widget PackGrow 7
    setWidgetCanDefault okButton True
    widgetGrabDefault okButton
    widgetShowAll dia
    resp  <- dialogRun' dia
    value <- liftIO $ ext (ClonePackageSourceRepo "" workspaceDir)
    widgetDestroy dia
    --find
    case resp of
        ResponseTypeOk    -> return value
        _             -> return Nothing

packageClone :: FilePath -> ConduitT ToolOutput Void IDEM () -> (FilePath -> IDEAction) -> IDEAction
packageClone workspaceDir log' activateAction = flip catchIDE (\(e :: SomeException) -> ideMessage High . T.pack $ show e) $ do
    windows  <- getWindows
    mbResult <- clonePackageSourceDialog (head windows) workspaceDir
    case mbResult of
        Nothing -> return ()
        Just ClonePackageSourceRepo{..} -> cabalUnpack cloneParentDir packageToClone True Nothing log' activateAction

cabalUnpack :: FilePath -> Text -> Bool -> Maybe Text -> ConduitT ToolOutput Void IDEM () -> (FilePath -> IDEAction) -> IDEAction
cabalUnpack parentDir packageToUnpack sourceRepo mbNewName log' activateAction = do
    let tempDir = parentDir </> (T.unpack packageToUnpack ++ ".leksah.temp")
    liftIO $ do
        oldDirExists <- doesDirectoryExist tempDir
        when oldDirExists $ removeDirectoryRecursive tempDir
        createDirectory tempDir
    runExternalTool' (__ "Unpacking") "cabal" (["unpack"]
              ++ ["--source-repository" | sourceRepo]
              ++ ["--destdir=" <> T.pack tempDir, packageToUnpack]) tempDir Nothing $ do
        mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> C.ZipSink log'
        case mbLastOutput of
            Just (ToolExit ExitSuccess) -> do
                contents <- liftIO $ getDirectoryContents tempDir
                case filter (not . isPrefixOf ".") contents of
                    [] -> do
                        liftIO $ removeDirectoryRecursive tempDir
                        lift $ ideMessage High $ "Nothing found in " <> T.pack tempDir <> " after doing a cabal unpack."
                    [repoName] -> do
                        let destDir = parentDir </> maybe repoName T.unpack mbNewName
                        exists <- liftIO $ (||) <$> doesDirectoryExist destDir <*> doesFileExist destDir
                        if exists
                            then lift $ ideMessage High $ T.pack destDir <> " already exists"
                            else do
                                liftIO $ renameDirectory (tempDir </> repoName) destDir
                                mbCabalFile <- liftIO $ cabalFileName destDir
                                lift $ case (mbCabalFile, mbNewName) of
                                    (Just cfn, Just newName) -> do
                                        let newCfn = takeDirectory cfn </> T.unpack newName ++ ".cabal"
                                        when (cfn /= newCfn) . liftIO $ do
                                            s <- T.readFile cfn
                                            T.writeFile newCfn $ renameCabalFile (T.pack $ takeBaseName cfn) newName s
                                            removeFile cfn
                                        activateAction newCfn
                                    (Just cfn, _) -> activateAction cfn
                                    _  -> ideMessage High $ "Unpacked source reposity to " <> T.pack destDir <> " but it does not contain a .cabal file in the root directory."
                        liftIO $ removeDirectoryRecursive tempDir
                    _ -> do
                        liftIO $ removeDirectoryRecursive tempDir
                        lift $ ideMessage High $ "More than one subdirectory found in " <> T.pack tempDir <> " after doing a cabal unpack."

            _ -> do
                liftIO $ removeDirectoryRecursive tempDir
                lift $ ideMessage High $ "Failed to unpack source reposity to " <> T.pack tempDir

renameCabalFile :: Text -> Text -> Text -> Text
renameCabalFile oldName newName = T.unlines . map renameLine . T.lines
    where
        prefixes = ["name:", "executable ", "test-suite "]
        prefixesWithLength :: [(Text, Int)]
        prefixesWithLength = zip prefixes $ map T.length prefixes
        renameLine :: Text -> Text
        renameLine line =
            case mapMaybe (rename (line, T.toLower line)) prefixesWithLength of
                l:_ -> l
                []  -> line
        rename :: (Text, Text) -> (Text, Int) -> Maybe Text
        rename (line, lcLine) (lcPrefix, pLen) | lcPrefix `T.isPrefixOf` lcLine =
            let (prefix, rest) = T.splitAt pLen line
                (spaces, value) = T.span (==' ') rest in
            Just $ prefix <> spaces <> T.replace oldName newName value
        rename _ _ = Nothing

--  ---------------------------------------------------------------------
--  | We do some twist for handling build infos seperately to edit them in one editor together
--  with the other stuff. This type show what we really edit here
--

data PackageDescriptionEd = PDE {
    pd           :: PackageDescription,
    exes         :: [Executable'],
    tests        :: [Test'],
    bms          :: [Benchmark'],
    mbLib        :: Maybe Library',
    bis          :: [BuildInfo]}
        deriving Eq

fromEditor :: PackageDescriptionEd -> PackageDescription
fromEditor (PDE pd exes'
        tests'
        benchmarks'
        mbLib' buildInfos) =
    let     exes = map (\ (Executable' s fb scope bii) -> if bii + 1 > length buildInfos
                                        then Executable (mkUnqualComponentName $ T.unpack s) fb scope (buildInfos !! (length buildInfos - 1))
                                        else Executable (mkUnqualComponentName $ T.unpack s) fb scope (buildInfos !! bii)) exes'
            tests = map (\ (Test' s fb bii) -> if bii + 1 > length buildInfos
                                        then TestSuite (mkUnqualComponentName $ T.unpack s) fb (buildInfos !! (length buildInfos - 1))
                                        else TestSuite (mkUnqualComponentName $ T.unpack s) fb (buildInfos !! bii)) tests'
            bms = map (\ (Benchmark' s fb bii) -> if bii + 1 > length buildInfos
                                        then Benchmark (mkUnqualComponentName $ T.unpack s) fb (buildInfos !! (length buildInfos - 1))
                                        else Benchmark (mkUnqualComponentName $ T.unpack s) fb (buildInfos !! bii)) benchmarks'
            mbLib = case mbLib' of
                    Nothing -> Nothing
                    Just (Library' ln mn rmn s b bii) -> if bii + 1 > length buildInfos
                                        then Just (Library (mkUnqualComponentName . T.unpack <$> ln) mn rmn s b (buildInfos !! (length buildInfos - 1)))
                                        else Just (Library (mkUnqualComponentName . T.unpack <$> ln) mn rmn s b (buildInfos !! bii))
    in pd {
        library = mbLib
      , executables = exes
      , testSuites = tests
      , benchmarks = bms
      }

toEditor :: PackageDescription -> PackageDescriptionEd
toEditor pd =
    let     (exes,exeBis) = unzip $ map (\(Executable s fb scope bi, i) -> (Executable' (T.pack $ unUnqualComponentName s) fb scope i, bi))
                            (zip (executables pd) [0..])
            (tests,testBis) = unzip $ map (\(TestSuite s fb bi, i) -> (Test' (T.pack $ unUnqualComponentName s) fb i, bi))
                            (zip (testSuites pd) [length exeBis..])
            (bms,benchmarkBis) = unzip $ map (\(Benchmark s fb bi, i) -> (Benchmark' (T.pack $ unUnqualComponentName s) fb i, bi))
                            (zip (benchmarks pd) [length testBis..])
            bis = exeBis ++ testBis ++ benchmarkBis
            (mbLib,bis2) = case library pd of
                    Nothing                -> (Nothing,bis)
                    Just (Library ln mn rmn s b bi) -> (Just (Library' (T.pack . unUnqualComponentName <$> ln) (sort mn) rmn s b (length bis)), bis ++ [bi])
            bis3 = if null bis2
                        then [emptyBuildInfo]
                        else bis2
    in PDE (pd {library = Nothing , executables = []})
        exes
        tests
        bms
        mbLib
        bis3

-- ---------------------------------------------------------------------
-- The pane stuff
--

data PackagePane             =   PackagePane {
    packageBox              ::   Box,
    packageNotifer          ::   Notifier
} deriving Typeable


data PackageState = PackageState
    deriving (Read, Show, Typeable)

instance Pane PackagePane IDEM
    where
    primPaneName _  =   __ "Package"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . packageBox
    paneId _        =   "*Package"

instance RecoverablePane PackagePane PackageState IDEM where
    saveState _     =   return Nothing
    recoverState _pp _st  =  return Nothing
    buildPane _panePath _notebook _builder = return Nothing
    builder _pp _nb _w =    return (Nothing,[])

editPackage :: PackageDescription -> FilePath -> [ModuleName] -> (FilePath -> IDEAction) -> IDEAction
editPackage packageD packagePath modules afterSaveAction = do
    mbPane :: Maybe PackagePane <-  getPane
    case mbPane of
        Nothing -> do
            pp  <- getBestPathForId "*Package"
            nb  <- getNotebook pp
            packageInfos <- getAllPackageIds
            let packageEd = toEditor packageD
            initPackage packagePath packageEd
                (packageDD
                    packageInfos
                    (takeDirectory packagePath)
                    modules
                    (length (bis packageEd))
                    (concatMap (buildInfoD (Just (takeDirectory packagePath)) modules)
                        [0..length (bis packageEd) - 1]))
                pp nb modules afterSaveAction packageEd
        Just p -> bringPaneToFront p

initPackage :: FilePath
    -> PackageDescriptionEd
    -> FieldDescription PackageDescriptionEd
    -> PanePath
    -> Notebook
    -> [ModuleName]
    -> (FilePath -> IDEAction)
    -> PackageDescriptionEd
    -> IDEM ()
initPackage packageDir packageD packageDescr panePath nb modules afterSaveAction origPackageD = do
    let fields =  flattenFieldDescription packageDescr
    packageInfos <- getAllPackageIds
    mbP <- buildThisPane panePath nb
        (builder' packageDir packageD packageDescr afterSaveAction
            modules packageInfos fields origPackageD)
    case mbP of
        Nothing -> return ()
        Just PackagePane{packageNotifer = pn} ->
            void . liftIO $ triggerEvent pn GUIEvent{selector = MayHaveChanged, eventText = "",
                                              gtkReturn = True}

builder' :: FilePath ->
    PackageDescriptionEd ->
    FieldDescription PackageDescriptionEd ->
    (FilePath -> IDEAction) ->
    [ModuleName] ->
    [PackageId] ->
    [FieldDescription PackageDescriptionEd] ->
    PackageDescriptionEd ->
    PanePath ->
    Notebook ->
    Window ->
    IDEM (Maybe PackagePane,Connections)
builder' packageDir packageD packageDescr afterSaveAction modules packageInfos _fields
    origPackageD panePath nb window  = reifyIDE $ \ ideR -> do
    vb      <-  boxNew OrientationVertical 0
    bb      <-  buttonBoxNew OrientationHorizontal
    save    <- buttonNewWithLabel "Save"
    widgetSetSensitive save False
    closeB  <- buttonNewWithLabel "Close"
    addB    <- buttonNewWithLabel (__ "Add Build Info")
    removeB <- buttonNewWithLabel (__ "Remove Build Info")
    label   <-  labelNew (Nothing :: Maybe Text)
    boxPackStart' bb addB PackNatural 0
    boxPackStart' bb removeB PackNatural 0
    boxPackEnd' bb closeB PackNatural 0
    boxPackEnd' bb save PackNatural 0
    (widget, _setInj, getExt, notifier)  <-  buildEditor packageDescr packageD
    let packagePane = PackagePane vb notifier
    boxPackStart' vb widget PackGrow 7
    boxPackStart' vb label PackNatural 0
    boxPackEnd' vb bb PackNatural 7

    -- let fieldNames = map (fromMaybe (__ "Unnamed") . getParameterPrim paraName . parameters) fields
    _ <- onButtonClicked addB $ do
        mbNewPackage' <- extract packageD [getExt]
        case mbNewPackage' of
            Nothing -> sysMessage Normal (__ "Content doesn't validate")
            Just pde -> reflectIDE (do
                    _ <- closePane packagePane
                    initPackage packageDir pde {bis = bis pde ++ [head (bis pde)]}
                        (packageDD
                            packageInfos
                            packageDir
                            modules
                            (length (bis pde) + 1)
                            (concatMap (buildInfoD (Just packageDir) modules)
                                [0..length (bis pde)]))
                        panePath nb modules afterSaveAction origPackageD) ideR
    _ <- onButtonClicked removeB $ do
        mbNewPackage' <- extract packageD [getExt]
        case mbNewPackage' of
            Nothing -> sysMessage Normal (__ "Content doesn't validate")
            Just pde | length (bis pde) == 1  -> sysMessage Normal (__ "Just one Build Info")
                     | otherwise -> reflectIDE (do
                        _ <- closePane packagePane
                        initPackage packageDir pde{bis = take (length (bis pde) - 1) (bis pde)}
                            (packageDD
                                packageInfos
                                packageDir
                                modules
                                (length (bis pde) - 1)
                                (concatMap (buildInfoD (Just packageDir) modules)
                                    [0..length (bis pde) - 2]))
                            panePath nb modules afterSaveAction origPackageD) ideR
    _ <- onButtonClicked closeB $ do
        mbP <- extract packageD [getExt]
        let hasChanged = case mbP of
                                Nothing -> False
                                Just p -> p /= origPackageD
        if not hasChanged
            then reflectIDE (void (closePane packagePane)) ideR
            else do
                md <- new' MessageDialog [
                        constructDialogUseHeaderBar 0,
                        constructMessageDialogButtons ButtonsTypeYesNo]
                setMessageDialogMessageType md MessageTypeQuestion
                setMessageDialogText md $ __ "Unsaved changes. Close anyway?"
                windowSetTransientFor md (Just window)
                setWindowWindowPosition md WindowPositionCenterOnParent
                resp <- dialogRun' md
                widgetDestroy md
                case resp of
                    ResponseTypeYes ->   reflectIDE (void (closePane packagePane)) ideR
                    _  ->   return ()
    _ <- onButtonClicked save $ do
        mbNewPackage' <- extract packageD [getExt]
        case mbNewPackage' of
            Nothing -> return ()
            Just newPackage' -> let newPackage = fromEditor newPackage' in do
                let packagePath = packageDir </> (display . pkgName . package . pd) newPackage'
                                                ++ ".cabal"
                writeGenericPackageDescription' packagePath (toGenericPackageDescription newPackage)
                reflectIDE (do
                    afterSaveAction packagePath
                    _ <- closePane packagePane
                    return ()) ideR
    _ <- registerEvent notifier MayHaveChanged (\ e -> do
        mbP <- extract packageD [getExt]
        let hasChanged = case mbP of
                                Nothing -> False
                                Just p -> p /= origPackageD
        when (isJust mbP) $ labelSetMarkup label ""
        w <- reflectIDE (getTopWidget packagePane) ideR
        markLabel nb w hasChanged
        widgetSetSensitive save hasChanged
        return (e{gtkReturn=False}))
    _ <- registerEvent notifier ValidationError (\e -> do
        labelSetMarkup label $ "<span foreground=\"red\" size=\"x-large\">The following fields have invalid values: "
            <> eventText e <> "</span>"
        return e)
    return (Just packagePane,[])

-- ---------------------------------------------------------------------
-- The description with some tricks
--

packageDD :: [PackageIdentifier]
    -> FilePath
    -> [ModuleName]
    -> Int
    -> [(Text, FieldDescription PackageDescriptionEd)]
    -> FieldDescription PackageDescriptionEd
packageDD packages fp modules numBuildInfos extras = NFD ([
    (__ "Package", VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Synopsis")
           $ paraSynopsis <<<- ParaSynopsis (__ "A one-line summary of this package")
           $ emptyParams)
            (synopsis . pd)
            (\ a b -> b{pd = (pd b){synopsis = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName (__ "Package Identifier") $ emptyParams)
            (package . pd)
            (\ a b -> b{pd = (pd b){package = a}})
            packageEditor
    ,   mkField
            (paraName <<<- ParaName (__ "Description")
                $ paraSynopsis <<<- ParaSynopsis (__ "A more verbose description of this package")
                    $ paraShadow <<<- ParaShadow ShadowTypeOut
                        $ paraMinSize <<<- ParaMinSize (-1,210)
                            $ emptyParams)
            (T.pack . description . pd)
            (\ a b -> b{pd = (pd b){description = T.unpack $ if T.null a then " " else a}})
            multilineStringEditor
    ,   mkField
            (paraName <<<- ParaName (__ "Homepage") $ emptyParams)
            (homepage . pd)
            (\ a b -> b{pd = (pd b){homepage = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName (__ "Package URL") $ emptyParams)
            (pkgUrl . pd)
            (\ a b -> b{pd = (pd b){pkgUrl = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName (__ "Category") $ emptyParams)
            (category . pd)
            (\ a b -> b{pd = (pd b){category = a}})
            (stringEditor (const True) True)
    ]),
    (__ "Description", VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Stability") $ emptyParams)
            (stability . pd)
            (\ a b -> b{pd = (pd b){stability = a}})
            (stringEditor (const True) True)
            -- TODO Fix this up to work with current Cabal
--    ,   mkField
--            (paraName <<<- ParaName (__ "License") $ emptyParams)
--            (license . pd)
--            (\ a b -> b{pd = (pd b){license = a}})
--            (comboSelectionEditor [GPL, LGPL, BSD3, BSD4, PublicDomain, AllRightsReserved, OtherLicense] show)
    ,   mkField
            (paraName <<<- ParaName (__ "License Files")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "A list of license files.")
                    $ paraOrientation <<<- ParaOrientation OrientationVertical
                        $ paraMinSize <<<- ParaMinSize (-1,250)
                            $ emptyParams)
            (licenseFiles . pd)
            (\ a b -> b{pd = (pd b){licenseFiles = a}})
            (filesEditor (Just fp) FileChooserActionOpen (__ "Select File"))
    ,   mkField
            (paraName <<<- ParaName (__ "Copyright") $ emptyParams)
            (copyright . pd)
            (\ a b -> b{pd = (pd b){copyright = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName (__ "Author") $ emptyParams)
            (author . pd)
            (\ a b -> b{pd = (pd b){author = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName (__ "Maintainer") $ emptyParams)
            (maintainer . pd)
            (\ a b -> b{pd = (pd b){maintainer = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName (__ "Bug Reports") $ emptyParams)
            (bugReports . pd)
            (\ a b -> b{pd = (pd b){bugReports = a}})
            (stringEditor (const True) True)
    ]),
--    ("Repositories", VFD emptyParams [
--        mkField
--            (paraName <<<- ParaName "Source Repositories" $ emptyParams)
--            (sourceRepos . pd)
--            (\ a b -> b{pd = (pd b){sourceRepos = a}})
--            reposEditor]),
    (__ "Dependencies  ", VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Build Dependencies")
                $ paraSynopsis <<<- ParaSynopsis (__ "Does this package depends on other packages?")
                    $ paraOrientation <<<- ParaOrientation OrientationVertical
                        $ paraMinSize <<<- ParaMinSize (-1,250)
                            $ emptyParams)
#if MIN_VERSION_Cabal(2,4,0)
            (nub . allBuildDepends . pd)
            (\ a b -> b) -- TODO
#else
            (nub . buildDepends . pd)
            (\ a b -> b{pd = (pd b){buildDepends = a}})
#endif
            (dependenciesEditor packages)
    ]),
    (__ "Meta Dep.", VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Cabal version")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "Does this package depends on a specific version of Cabal?")
                    $ paraShadow <<<- ParaShadow ShadowTypeIn $ emptyParams)
            (specVersion . pd)
            (\ a b -> b{pd = (pd b){specVersionRaw = Left a}})
            versionEditor
    ,   mkField
            (paraName <<<- ParaName (__ "Tested with compiler")
                $ paraShadow <<<- ParaShadow ShadowTypeIn
                    $ paraOrientation <<<- ParaOrientation OrientationVertical
                        $ paraMinSize <<<- ParaMinSize (-1,150)
                            $ emptyParams)
            (\a -> case (testedWith . pd) a of
                []          -> []--(GHC,anyVersion)]
                l           -> l)
            (\ a b -> b{pd = (pd b){testedWith = a}})
            testedWithEditor
    ]),
    (__ "Data Files", VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Data Files")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "A list of files to be installed for run-time use by the package.")
                    $ paraOrientation <<<- ParaOrientation OrientationVertical
                        $ paraMinSize <<<- ParaMinSize (-1,250)
                            $ emptyParams)
            (dataFiles . pd)
            (\ a b -> b{pd = (pd b){dataFiles = a}})
            (filesEditor (Just fp) FileChooserActionOpen (__ "Select File"))
    ,   mkField
            (paraName <<<- ParaName (__ "Data directory") $ emptyParams)
            (dataDir . pd)
            (\ a b -> b{pd = (pd b){dataDir = a}})
            (fileEditor (Just fp) FileChooserActionSelectFolder (__ "Select file"))
    ]),
    (__ "Extra Files", VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Extra Source Files")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "A list of additional files to be included in source distributions.")
                    $ paraOrientation <<<- ParaOrientation OrientationVertical
                        $ paraMinSize <<<- ParaMinSize (-1,120)
                            $ emptyParams)
            (extraSrcFiles . pd)
            (\ a b -> b{pd = (pd b){extraSrcFiles = a}})
            (filesEditor (Just fp) FileChooserActionOpen (__ "Select File"))
    ,   mkField
            (paraName <<<-  ParaName (__ "Extra Tmp Files")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "A list of additional files or directories to be removed by setup clean.")
                    $ paraOrientation <<<- ParaOrientation OrientationVertical
                        $ paraMinSize <<<- ParaMinSize (-1,120)
                            $ emptyParams)
            (extraTmpFiles . pd)
            (\ a b -> b{pd = (pd b){extraTmpFiles = a}})
            (filesEditor (Just fp) FileChooserActionOpen (__ "Select File"))
    ]),
    (__ "Other",VFD emptyParams  [
        mkField
            (paraName <<<- ParaName (__ "Build Type")
                $ paraSynopsis <<<- ParaSynopsis
                (__ "Describe executable programs contained in the package")
                        $ paraShadow <<<- ParaShadow ShadowTypeIn
                            $ paraOrientation <<<- ParaOrientation OrientationVertical
                                $ emptyParams)
            (buildType . pd)
            (\ a b -> b{pd = (pd b){
#if MIN_VERSION_Cabal(2,2,0)
              buildTypeRaw = Just a
#else
              buildType = a
#endif
              }})
#if MIN_VERSION_Cabal(2,2,0)
            buildTypeEditor
#else
            (maybeEditor def (buildTypeEditor, emptyParams) True (__ "Specify?"))
#endif
    ,   mkField
            (paraName <<<- ParaName (__ "Custom Fields")
                $ paraShadow <<<- ParaShadow ShadowTypeIn
                    $ paraMinSize <<<- ParaMinSize (-1,150)
                        $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
            (customFieldsPD . pd)
            (\ a b -> b{pd = (pd b){customFieldsPD = a}})
            (multisetEditor def
                (ColumnDescr True [(__ "Name",\cell (n,_) -> setCellRendererTextText cell $ T.pack n)
                                   ,(__ "Value",\cell (_,v) -> setCellRendererTextText cell $ T.pack v)])
                (pairEditor (stringxEditor (const True), emptyParams)
                   (stringEditor (const True) True, emptyParams),
                 emptyParams)
            Nothing
            Nothing)
    ]),
    (__ "Executables",VFD emptyParams  [
        mkField
            (paraName <<<- ParaName (__ "Executables")
                $ paraSynopsis <<<- ParaSynopsis
                (__ "Describe executable programs contained in the package")
                    $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
            exes
            (\ a b -> b{exes = a})
            (executablesEditor (Just fp) modules numBuildInfos)
    ]),
    (__ "Tests",VFD emptyParams  [
        mkField
            (paraName <<<- ParaName (__ "Tests")
                $ paraSynopsis <<<- ParaSynopsis
                (__ "Describe tests contained in the package")
                    $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
            tests
            (\ a b -> b{tests = a})
            (testsEditor (Just fp) modules numBuildInfos)
    ]),
    (__ "Benchmarks",VFD emptyParams  [
        mkField
            (paraName <<<- ParaName (__ "Benchmarks")
                $ paraSynopsis <<<- ParaSynopsis
                (__ "Describe tests contained in the package")
                    $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
            bms
            (\ a b -> b{bms = a})
            (benchmarksEditor (Just fp) modules numBuildInfos)
    ]),
    (__ "Library", VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Library")
           $ paraSynopsis <<<- ParaSynopsis
             (__ "If the package contains a library, specify the exported modules here")
           $ paraOrientation <<<- ParaOrientation OrientationVertical
           $ paraShadow <<<- ParaShadow ShadowTypeIn $ emptyParams)
            mbLib
            (\ a b -> b{mbLib = a})
            (maybeEditor def (libraryEditor (Just fp) modules numBuildInfos,
                paraName <<<- ParaName (__ "Specify exported modules")
                $ emptyParams) True
                (__ "Does this package contain a library?"))
    ])
    ] ++ extras)

update :: [BuildInfo] -> Int -> (BuildInfo -> BuildInfo)  -> [BuildInfo]
update bis index func =
    map (\(bi,ind) -> if ind == index
                        then func bi
                        else bi)
        (zip bis [0..length bis - 1])

buildInfoD :: Maybe FilePath -> [ModuleName] -> Int -> [(Text,FieldDescription PackageDescriptionEd)]
buildInfoD fp modules i = [
    (T.pack $ printf (__ "%s Build Info") (show (i + 1)), VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Component is buildable here") $ emptyParams)
            (buildable . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{buildable = a})})
            boolEditor
    ,   mkField
            (paraName  <<<- ParaName
                (__ "Where to look for the source hierarchy")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "Root directories for the source hierarchy.")
                    $ paraShadow  <<<- ParaShadow ShadowTypeIn
                        $ paraOrientation  <<<- ParaOrientation OrientationVertical
                            $ paraMinSize <<<- ParaMinSize (-1,150)
                                $ emptyParams)
            (hsSourceDirs . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{hsSourceDirs = a})})
            (filesEditor fp FileChooserActionSelectFolder (__ "Select folder"))
    ,   mkField
            (paraName <<<- ParaName (__ "Non-exposed or non-main modules")
            $ paraSynopsis <<<- ParaSynopsis
                                       (__ "A list of modules used by the component but not exposed to users.")
                $ paraShadow <<<- ParaShadow ShadowTypeIn
                    $ paraOrientation <<<- ParaOrientation OrientationVertical
                        $ paraMinSize <<<- ParaMinSize (-1,300)
                            $ paraPack <<<- ParaPack PackGrow
                                $ emptyParams)
            (map (T.pack . display) . otherModules . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi ->
                bi{otherModules = map (fromMaybe (error "no parse for moduile name") . simpleParse . T.unpack) a})})
            (modulesEditor modules)
    ]),
    (T.pack $ printf (__ "%s Compiler ") (show (i + 1)), VFD emptyParams [
        mkField
            (paraName  <<<- ParaName (__ "Options for haskell compilers")
            $ paraOrientation <<<- ParaOrientation OrientationVertical
            $ paraShadow <<<- ParaShadow ShadowTypeIn
            $ paraPack <<<- ParaPack PackGrow $ emptyParams)
            (options . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{options = a})})
            (multisetEditor def
                (ColumnDescr True [( __ "Compiler Flavor",\cell (cv,_) -> setCellRendererTextText cell . T.pack $ show cv)
                                   ,(__ "Options",\cell (_,op) -> setCellRendererTextText cell . T.pack $ concatMap (\s -> ' ' : s) op)])
                (pairEditor
                    (compilerFlavorEditor,emptyParams)
                    (optsEditor,emptyParams),
                        paraOrientation <<<- ParaOrientation OrientationVertical
                            $ paraShadow  <<<- ParaShadow ShadowTypeIn $ emptyParams)
                Nothing
                Nothing)
     ,  mkField
            (paraName <<<- ParaName (__ "Additional options for GHC when built with profiling")
           $ emptyParams)
            (profOptions . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{profOptions = a})})
            compilerOptsEditor
     ,  mkField
            (paraName <<<- ParaName (__ "Additional options for GHC when the package is built as shared library")
           $ emptyParams)
            (sharedOptions . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{sharedOptions = a})})
            compilerOptsEditor
    ]),
    (T.pack $ printf (__ "%s Extensions ") (show (i + 1)), VFD emptyParams [
        mkField
            (paraName  <<<- ParaName (__ "Extensions")
                $ paraSynopsis  <<<- ParaSynopsis
                    (__ "A list of Haskell extensions used by every module.")
                         $ paraMinSize <<<- ParaMinSize (-1,400)
                            $ paraPack <<<- ParaPack PackGrow
                                $ emptyParams)
            (oldExtensions . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{oldExtensions = a})})
            extensionsEditor
    ]),
--    (T.pack $ printf (__ "%s Build Tools ") (show (i + 1)), VFD emptyParams [
--        mkField
--            (paraName <<<- ParaName (__ "Tools needed for a build")
--                $ paraOrientation <<<- ParaOrientation OrientationVertical
--                    $ paraMinSize <<<- ParaMinSize (-1,120)
--                        $ emptyParams)
--            (map Dependency buildTools . (!! i) . bis)
--            (\ a b -> b{bis = update (bis b) i (\bi -> bi{buildTools = a})})
--            (dependenciesEditor [])
--    ]),
--    (T.pack $ printf (__ "%s Pkg Config ") (show (i + 1)), VFD emptyParams [
--        mkField
--            (paraName <<<- ParaName (__ "A list of pkg-config packages, needed to build this package")
--                $ paraOrientation <<<- ParaOrientation OrientationVertical
--                    $ paraMinSize <<<- ParaMinSize (-1,120)
--                        $ emptyParams)
--            (pkgconfigDepends . (!! i) . bis)
--            (\ a b -> b{bis = update (bis b) i (\bi -> bi{pkgconfigDepends = a})})
--            (dependenciesEditor [])
--    ]),
    (T.pack $ printf (__ "%s Opts C -1-") (show (i + 1)), VFD emptyParams [
         mkField
            (paraName <<<- ParaName (__ "Options for C compiler")
                $ paraOrientation <<<- ParaOrientation OrientationVertical
                    $ emptyParams)
            (ccOptions . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{ccOptions = a})})
            optsEditor
    ,    mkField
            (paraName <<<- ParaName (__ "Options for linker")
                $ paraOrientation <<<- ParaOrientation OrientationVertical
                    $ emptyParams)
            (ldOptions . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{ldOptions = a})})
            optsEditor
    ,    mkField
            (paraName <<<- ParaName (__ "A list of header files to use when compiling")
                $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
            (map T.pack . includes . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{includes = map T.unpack a})})
            (textsEditor (const True) True)
     ,   mkField
            (paraName <<<- ParaName (__ "A list of header files to install")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
            (installIncludes . (!! i) . bis)
             (\ a b -> b{bis = update (bis b) i (\bi -> bi{installIncludes = a})})
           (filesEditor fp FileChooserActionOpen (__ "Select File"))
    ]),
    (T.pack $ printf (__ "%s Opts C -2-") (show (i + 1)), VFD emptyParams [
         mkField
            (paraName <<<- ParaName (__ "A list of directories to search for header files")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
            (includeDirs . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{includeDirs = a})})
            (filesEditor fp FileChooserActionSelectFolder (__ "Select Folder"))
     ,   mkField
            (paraName <<<- ParaName
                (__ "A list of C source files to be compiled,linked with the Haskell files.")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
            (cSources . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{cSources = a})})
            (filesEditor fp FileChooserActionOpen (__ "Select file"))
    ]),
    (T.pack $ printf (__ "%s Opts Libs ") (show (i + 1)), VFD emptyParams [
         mkField
            (paraName <<<- ParaName (__ "A list of extra libraries to link with")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
            (map T.pack . extraLibs . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{extraLibs = map T.unpack a})})
            (textsEditor (const True) True)
     ,   mkField
            (paraName <<<- ParaName (__ "A list of directories to search for libraries.")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
            (extraLibDirs . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{extraLibDirs = a})})
            (filesEditor fp FileChooserActionSelectFolder (__ "Select Folder"))
   ]),
    (T.pack $ printf (__ "%s Other") (show (i + 1)), VFD emptyParams [
         mkField
            (paraName <<<- ParaName (__ "Options for C preprocessor")
                $ paraOrientation <<<- ParaOrientation OrientationVertical
                    $ emptyParams)
            (cppOptions . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{cppOptions = a})})
            optsEditor
    ,   mkField
            (paraName <<<- ParaName (__ "Support frameworks for Mac OS X")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
            (map T.pack . frameworks . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{frameworks = map T.unpack a})})
            (textsEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName (__ "Custom fields build info")
                $ paraShadow <<<- ParaShadow ShadowTypeIn
                    $ paraMinSize <<<- ParaMinSize (-1,150)
                        $ paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
             (customFieldsBI . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{customFieldsBI = a})})
            (multisetEditor def
                (ColumnDescr True [(__ "Name",\cell (n,_) -> setCellRendererTextText cell $ T.pack n)
                                   ,(__ "Value",\cell (_,v) -> setCellRendererTextText cell $ T.pack v)])
                (pairEditor
                    (stringxEditor (const True),emptyParams)
                    (stringEditor (const True) True,emptyParams),emptyParams)
            Nothing
            Nothing)
            ])]

stringxEditor :: (String -> Bool) -> Editor String
stringxEditor val para noti = do
    (wid,inj,ext) <- stringEditor val True para noti
    let
        xinj "" = inj ""
        xinj ('x':'-':rest) = inj rest
        xinj _ = throwIDE "PackageEditor>>stringxEditor: field without leading x-"
        xext = do
            res <- ext
            case res of
                Nothing -> return Nothing
                Just str -> return (Just ("x-" ++ str))
    return (wid,xinj,xext)

optsEditor :: Editor [String]
optsEditor para noti = do
    (wid,inj,ext) <- stringEditor (const True) True para noti
    let
        oinj = inj . unwords
        oext = do
            res <- ext
            case res of
                Nothing -> return Nothing
                Just str -> return (Just (words str))
    return (wid,oinj,oext)

compilerOptRecordEditor :: Editor (CompilerFlavor, [String])
compilerOptRecordEditor para =
    pairEditor
        (compilerFlavorEditor
            , paraName <<<- ParaName "Compiler" $ emptyParams)
        (optsEditor,paraName <<<- ParaName "Options" $ emptyParams)
        (paraOrientation <<<- ParaOrientation OrientationVertical $ para)

compilerOptsEditor :: Editor [(CompilerFlavor, [String])]
compilerOptsEditor p =
    multisetEditor def
        (ColumnDescr True [("Compiler",\cell (compiler, _) -> setCellRendererTextText cell . T.pack $ show compiler)
                           ,("Options",\cell (_, opts    ) -> setCellRendererTextText cell . T.pack $ unwords opts)])
        (compilerOptRecordEditor,
            paraHAlign <<<- ParaHAlign AlignFill
                $ paraVAlign <<<- ParaVAlign AlignCenter
                   $ emptyParams)
        (Just (sortBy (\ (f1, _) (f2, _) -> compare f1 f2)))
        (Just (\ (f1, _) (f2, _) -> f1 == f2))
        (paraShadow <<<- ParaShadow ShadowTypeIn
            $ paraHAlign <<<- ParaHAlign AlignFill
                $ paraVAlign <<<- ParaVAlign AlignCenter
                    $ paraOrientation  <<<-  ParaOrientation OrientationVertical
                        $ paraPack <<<- ParaPack PackGrow
                            $ p)

packageEditor :: Editor PackageIdentifier
packageEditor para noti = do
    (wid,inj,ext) <- pairEditor
        (stringEditor (not . null) True, paraName <<<- ParaName (__ "Name") $ emptyParams)
        (versionEditor, paraName <<<- ParaName (__ "Version") $ emptyParams)
        (paraOrientation <<<- ParaOrientation OrientationHorizontal
            $ paraShadow <<<- ParaShadow ShadowTypeIn
                $ para) noti
    let pinj (PackageIdentifier n v) = inj (unPackageName n,v)
    let pext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just (n,v) -> return $
                    if null n
                        then Nothing
                        else Just $ PackageIdentifier (mkPackageName n) v
    return (wid,pinj,pext)

testedWithEditor :: Editor [(CompilerFlavor, VersionRange)]
testedWithEditor =
    multisetEditor def
       (ColumnDescr True [(__ "Compiler Flavor",\cell (cv,_) -> setCellRendererTextText cell . T.pack $ show cv)
                           ,(__ "Version Range",\cell (_,vr) -> setCellRendererTextText cell . T.pack $ display vr)])
       (pairEditor
          (compilerFlavorEditor,
           paraShadow <<<- ParaShadow ShadowTypeNone $ emptyParams)
          (versionRangeEditor,
           paraShadow <<<- ParaShadow ShadowTypeNone $ emptyParams),
        paraOrientation <<<- ParaOrientation OrientationVertical $ emptyParams)
       Nothing
       (Just (==))

compilerFlavorEditor :: Editor CompilerFlavor
compilerFlavorEditor para noti = do
    (wid,inj,ext) <- eitherOrEditor def ""
        (comboSelectionEditor flavors (T.pack . show), paraName <<<- ParaName (__ "Select compiler") $ emptyParams)
        (textEditor (not . T.null) True, paraName <<<- ParaName (__ "Specify compiler") $ emptyParams)
        (__ "Other")
        (paraName <<<- ParaName (__ "Select") $ para)
        noti
    let cfinj comp  = case comp of
                        (OtherCompiler str) -> inj (Right $ T.pack str)
                        other               -> inj (Left other)
    let cfext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just (Right s) -> return (Just . OtherCompiler $ T.unpack s)
                Just (Left other) -> return (Just other)
    return (wid,cfinj,cfext)
        where
        flavors = [GHC, NHC, Hugs, HBC, Helium, JHC]

buildTypeEditor :: Editor BuildType
buildTypeEditor para noti = do
    (wid,inj,ext) <- -- eitherOrEditor def ""
        comboSelectionEditor flavors (T.pack . show) --, paraName <<<- ParaName (__ "Select") $ emptyParams)
        (paraName <<<- ParaName (__ "Select") $ para)
        noti
    return (wid,inj,ext)
        where
        flavors = [Simple, Configure, Make, Custom]

extensionsEditor :: Editor [Extension]
extensionsEditor = staticListMultiEditor extensionsL (T.pack . show)


extensionsL :: [Extension]
extensionsL = map EnableExtension [minBound..maxBound]

{--
reposEditor :: Editor [SourceRepo]
reposEditor p noti =
    multisetEditor
        (ColumnDescr False [("",\cell repo -> setCellRendererTextText cell display repo)])
        (repoEditor,
            paraOuterAlignment <<<- ParaInnerAlignment (0.0, 0.5, 1.0, 1.0)
                $ paraInnerAlignment <<<- ParaOuterAlignment (0.0, 0.5, 1.0, 1.0)
                   $ emptyParams)
        Nothing
        Nothing
        (paraShadow <<<- ParaShadow ShadowTypeIn $
            paraOuterAlignment <<<- ParaInnerAlignment (0.0, 0.5, 1.0, 1.0)
                $ paraInnerAlignment <<<- ParaOuterAlignment (0.0, 0.5, 1.0, 1.0)
                    $ paraOrientation  <<<-  ParaOrientation OrientationVertical
                        $ paraPack <<<- ParaPack PackGrow
                            $ p)
        noti

instance Text SourceRepo where
  disp (SourceRepo repoKind repoType repoLocation repoModule repoBranch repoTag repoSubdir)
    = disp repoKind
        <+> case repoType of
                Nothing -> empty
                Just repoT -> disp repoT
        <+> case repoLocation of
                Nothing -> empty
                Just repoL -> text repoL

repoEditor :: Editor SourceRepo
repoEditor paras noti = do
    (widg,inj,ext) <- tupel7Editor
                            (repoKindEditor,noBorder)
                            (maybeEditor (repoTypeEditor,noBorder) True "Specify a type", emptyParams)
                            (maybeEditor (textEditor (const True) True,noBorder) True "Specify a location", emptyParams)
                            (maybeEditor (textEditor (const True) True,noBorder) True "Specify a module", emptyParams)
                            (maybeEditor (textEditor (const True) True,noBorder) True "Specify a branch", emptyParams)
                            (maybeEditor (textEditor (const True) True,noBorder) True "Specify a tag", emptyParams)
                            (maybeEditor (textEditor (const True) True,noBorder) True "Specify a subdir", emptyParams)
                            (paraOrientation  <<<- ParaOrientation OrientationVertical $ noBorder)
                            noti
    return (widg,
        (\ r -> inj (repoKind r,repoType r,repoLocation r,repoModule r,repoBranch r,repoTag r,repoSubdir r)),
        (do
            mb <- ext
            case mb of
                Nothing        -> return Nothing
                Just (a,b,c,d,e,f,g) -> return (Just (SourceRepo a b c d e f g))))
    where noBorder  =  paraOuterAlignment <<<- ParaOuterAlignment  (0.0, 0.0, 0.0, 0.0)
                            $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                                $ paraInnerAlignment <<<- ParaInnerAlignment  (0.0, 0.0, 0.0, 0.0)
                                    $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                                        $ emptyParams

repoKindEditor :: Editor RepoKind
repoKindEditor paras noti = do
    (widg,inj,ext) <- pairEditor
                        (comboSelectionEditor selectionList show, emptyParams)
                        (textEditor (const True) True,emptyParams)
                        paras
                        noti
    return (widg,
            (\kind -> case kind of
                        RepoKindUnknown str  -> inj (RepoKindUnknown "",str)
                        other                -> inj (other,"")),
            (do
                mbRes <- ext
                case mbRes of
                    Nothing                       -> return Nothing
                    Just (RepoKindUnknown "",str) -> return (Just (RepoKindUnknown str))
                    Just (other,_)                -> return (Just other)))
    where selectionList =  [RepoHead, RepoThis, RepoKindUnknown ""]

repoTypeEditor :: Editor RepoType
repoTypeEditor paras noti = do
    (widg,inj,ext) <- pairEditor
                        (comboSelectionEditor selectionList show, emptyParams)
                        (textEditor (const True) True,emptyParams)
                        paras
                        noti
    return (widg,
            (\kind -> case kind of
                        OtherRepoType str    -> inj (OtherRepoType "",str)
                        other                -> inj (other,"")),
            (do
                mbRes <- ext
                case mbRes of
                    Nothing                       -> return Nothing
                    Just (OtherRepoType "",str)   -> return (Just (OtherRepoType str))
                    Just (other,_)                -> return (Just other)))
    where selectionList =  [Darcs, Git, SVN, CVS, Mercurial, GnuArch, Bazaar, Monotone, OtherRepoType ""]
--}

-- ------------------------------------------------------------
-- * BuildInfos
-- ------------------------------------------------------------

data Library' = Library'{
    libName'           :: Maybe Text
,   exposedModules'    :: [ModuleName]
,   reexportedModules' :: [ModuleReexport]
,   signatures'        :: [ModuleName]
,   libExposed'        :: Bool
,   libBuildInfoIdx    :: Int}
    deriving (Show, Eq)

data Executable' = Executable'{
    exeName'        :: Text
,   modulePath'     :: FilePath
,   exeScope'       :: ExecutableScope
,   exeBuildInfoIdx    :: Int}
    deriving (Show, Eq)

data Test' = Test'{
    testName'        :: Text
,   testInterface'   :: TestSuiteInterface
,   testBuildInfoIdx :: Int}
    deriving (Show, Eq)

data Benchmark' = Benchmark'{
    benchmarkName'        :: Text
,   benchmarkInterface'   :: BenchmarkInterface
,   benchmarkBuildInfoIdx :: Int}
    deriving (Show, Eq)

instance Default Library'
    where def =  Library' Nothing [] [] [] True def

instance Default Executable'
    where def = Executable' "" def mempty def

instance Default Test'
    where def = Test' "" (TestSuiteExeV10 (mkVersion [1,0]) def) def

instance Default Benchmark'
    where def = Benchmark' "" (BenchmarkExeV10 (mkVersion [1,0]) def) def

libraryEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Library'
libraryEditor _fp modules numBuildInfos para noti = do
    (wid,inj,ext) <-
        pairEditor
            (tupel3Editor
                (boolEditor,
                    paraName <<<- ParaName (__ "Exposed")
                    $ paraSynopsis <<<- ParaSynopsis (__ "Is the lib to be exposed by default?")
                    $ emptyParams)
                (maybeEditor "" (textEditor (const True) True, paraName <<<- ParaName (__ "Name") $ emptyParams) True "Named?",
                    paraSynopsis <<<- ParaSynopsis (__ "Is this a namesd library?")
                    $ emptyParams)
                (modulesEditor (sort modules),
                    paraName <<<- ParaName (__ "Exposed Modules")
                    $ paraMinSize <<<- ParaMinSize (-1,300)
                    $ para),
                paraOrientation <<<- ParaOrientation OrientationVertical
                $ emptyParams)
            (tupel3Editor
                (buildInfoEditorP numBuildInfos, paraName <<<- ParaName (__ "Build Info")
                    $ paraPack <<<- ParaPack PackNatural
                    $ para)
                (modulesEditor (sort modules),
                    paraName <<<- ParaName (__ "Reexported Modules")
                    $ paraMinSize <<<- ParaMinSize (-1,300)
                    $ para)
                (modulesEditor (sort modules),
                    paraName <<<- ParaName (__ "Signatures Needing Implementations")
                    $ paraMinSize <<<- ParaMinSize (-1,300)
                    $ para),
                paraOrientation <<<- ParaOrientation OrientationVertical
                $ emptyParams)
            (paraOrientation <<<- ParaOrientation OrientationVertical
            $ emptyParams)
            noti
    let pinj (Library' ln em rmn s exp' bi) = inj (
              (exp', ln, map (T.pack . display) em)
            , (bi, map (T.pack . display) rmn, map (T.pack . display) s)
            )
        parseModuleNames = map (fromMaybe (error "no parse for moduile name") . simpleParse . T.unpack)
        parseRexportedModules = map (fromMaybe (error "no parse for moduile name") . simpleParse . T.unpack)
        pext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just ((exp',ln,em), (bi, rmn, s)) -> return (Just $ Library'
                    ln
                    (parseModuleNames em)
                    (parseRexportedModules rmn)
                    (parseModuleNames s) exp' bi)
    return (wid,pinj,pext)

modulesEditor :: [ModuleName] -> Editor [Text]
modulesEditor modules   =   staticListMultiEditor (map (T.pack . display) modules) id

executablesEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor [Executable']
executablesEditor fp modules countBuildInfo p =
    multisetEditor def
        (ColumnDescr True [(__ "Executable Name",\cell Executable' {exeName' = exeName} -> setCellRendererTextText cell exeName)
                           ,(__ "Module Path",\cell Executable' {modulePath' = mp} -> setCellRendererTextText cell $ T.pack mp)
                           ,(__ "Build info index",\cell Executable' {exeBuildInfoIdx = bii} -> setCellRendererTextText cell . T.pack $ show (bii + 1))])
        (executableEditor fp modules countBuildInfo,emptyParams)
        Nothing
        Nothing
        (paraShadow  <<<- ParaShadow ShadowTypeIn
            $ paraMinSize <<<- ParaMinSize (-1,200) $ p)

executableEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Executable'
executableEditor _fp _modules countBuildInfo para noti = do
    (wid,inj,ext) <-
        pairEditor
            ( tupel3Editor
                (textEditor (not . T.null) True,
                    paraName <<<- ParaName (__ "Executable Name")
                    $ emptyParams)
                (stringEditor (not . null) True,
                    paraOrientation <<<- ParaOrientation OrientationVertical
                    $ paraName <<<- ParaName (__ "File with main function")
                    $ emptyParams)
                (buildInfoEditorP countBuildInfo, paraName <<<- ParaName (__ "Build Info")
                    $ paraHAlign <<<- ParaHAlign AlignStart
                        $ paraVAlign <<<- ParaVAlign AlignStart
                            $ paraMargin <<<- ParaMargin (0, 0, 0, 0)
                                $ emptyParams)
            , paraOrientation <<<- ParaOrientation OrientationVertical
                $ emptyParams)
            (comboSelectionEditor
                [ ExecutablePublic
                , ExecutablePrivate] (\case
#if !MIN_VERSION_Cabal(2,2,0)
                    ExecutableScopeUnknown -> "Scope Unknown"
#endif
                    ExecutablePublic       -> "Public"
                    ExecutablePrivate      -> "Private"),
                paraName <<<- ParaName (__ "Executable Name")
                    $ emptyParams)
        (paraOrientation  <<<- ParaOrientation OrientationVertical $ para)
        noti
    let pinj (Executable' s f scope bi) = inj ((s,f,bi), scope)
    let pext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just ((s,f,bi), scope) -> return (Just $Executable' s f scope bi)
    return (wid,pinj,pext)

testsEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor [Test']
testsEditor fp modules countBuildInfo p =
    multisetEditor def
        (ColumnDescr True [(__ "Test Name",\cell (Test' testName _ _) -> setCellRendererTextText cell testName)
                           ,(__ "Interface",\cell (Test'  _ i _) -> setCellRendererTextText cell . T.pack $ interfaceName i)
                           ,(__ "Build info index",\cell (Test'  _ _ bii) -> setCellRendererTextText cell . T.pack $ show (bii + 1))])
        (testEditor fp modules countBuildInfo,emptyParams)
        Nothing
        Nothing
        (paraShadow  <<<- ParaShadow ShadowTypeIn
            $ paraMinSize <<<- ParaMinSize (-1,200) $ p)
  where
    interfaceName (TestSuiteExeV10 _ f) = f
    interfaceName i = show i

testEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Test'
testEditor _fp _modules countBuildInfo para noti = do
    (wid,inj,ext) <- tupel3Editor
        (textEditor (not . T.null) True,
            paraName <<<- ParaName (__ "Test Name")
            $ emptyParams)
        (stringEditor (not . null) True,
            paraOrientation <<<- ParaOrientation OrientationVertical
            $ paraName <<<- ParaName (__ "File with main function")
            $ emptyParams)
        (buildInfoEditorP countBuildInfo, paraName <<<- ParaName (__ "Build Info")
            $ paraHAlign <<<- ParaHAlign AlignStart
                $ paraVAlign <<<- ParaVAlign AlignStart
                    $ paraMargin <<<- ParaMargin (0, 0, 0, 0)
                        $ emptyParams)
        (paraOrientation  <<<- ParaOrientation OrientationVertical $ para)
        noti
    let pinj (Test' s (TestSuiteExeV10 v f) bi) | versionNumbers v == [1,0] = inj (s,f,bi)
        pinj _ = error "Unexpected Test Interface"
    let pext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just (s,f,bi) -> return (Just $Test' s (TestSuiteExeV10 (mkVersion [1,0]) f) bi)
    return (wid,pinj,pext)

benchmarksEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor [Benchmark']
benchmarksEditor fp modules countBuildInfo p =
    multisetEditor def
        (ColumnDescr True [(__ "Benchmark Name",\cell (Benchmark' benchmarkName _ _) -> setCellRendererTextText cell benchmarkName)
                           ,(__ "Interface",\cell (Benchmark'  _ i _) -> setCellRendererTextText cell . T.pack $ interfaceName i)
                           ,(__ "Build info index",\cell (Benchmark'  _ _ bii) -> setCellRendererTextText cell . T.pack $ show (bii + 1))])
        (benchmarkEditor fp modules countBuildInfo,emptyParams)
        Nothing
        Nothing
        (paraShadow  <<<- ParaShadow ShadowTypeIn
            $ paraMinSize <<<- ParaMinSize (-1,200) $ p)
  where
    interfaceName (BenchmarkExeV10 _ f) = f
    interfaceName i = show i

benchmarkEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Benchmark'
benchmarkEditor _fp _modules countBuildInfo para noti = do
    (wid,inj,ext) <- tupel3Editor
        (textEditor (not . T.null) True,
            paraName <<<- ParaName (__ "Benchmark Name")
            $ emptyParams)
        (stringEditor (not . null) True,
            paraOrientation <<<- ParaOrientation OrientationVertical
            $ paraName <<<- ParaName (__ "File with main function")
            $ emptyParams)
        (buildInfoEditorP countBuildInfo, paraName <<<- ParaName (__ "Build Info")
            $ paraHAlign <<<- ParaHAlign AlignStart
                $ paraVAlign <<<- ParaVAlign AlignStart
                    $ paraMargin <<<- ParaMargin (0, 0, 0, 0)
                        $ emptyParams)
        (paraOrientation  <<<- ParaOrientation OrientationVertical $ para)
        noti
    let pinj (Benchmark' s (BenchmarkExeV10 v f) bi) | versionNumbers v == [1,0] = inj (s,f,bi)
        pinj _ = error "Unexpected Benchmark Interface"
    let pext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just (s,f,bi) -> return (Just $Benchmark' s (BenchmarkExeV10 (mkVersion [1,0]) f) bi)
    return (wid,pinj,pext)

buildInfoEditorP :: Int -> Editor Int
buildInfoEditorP numberOfBuildInfos para noti = do
    (wid,inj,ext) <- intEditor (1.0,fromIntegral numberOfBuildInfos,1.0)
        (paraName <<<- ParaName (__ "Build Info") $para) noti
    let pinj i = inj (i + 1)
    let pext = do
            mbV <- ext
            case mbV of
                Nothing -> return Nothing
                Just i  -> return (Just (i - 1))
    return (wid,pinj,pext)

-- ------------------------------------------------------------
-- * (Boring) default values
-- ------------------------------------------------------------


instance Default CompilerFlavor
    where def =  GHC

instance Default BuildInfo
    where def =  emptyBuildInfo

instance Default Library
    where def =  Library Nothing [] [] [] True def

instance Default ExecutableScope
    where def = mempty

instance Default UnqualComponentName
    where def = mkUnqualComponentName ""

instance Default Executable
    where def = Executable def def def def

instance Default RepoType
    where def = Darcs

instance Default RepoKind
    where def = RepoThis

instance Default SourceRepo
    where def =  SourceRepo def def def def def
                                    def def

instance Default BuildType
    where def = Simple
