{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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
,   packageEditText
,   choosePackageDir
,   choosePackageFile

,   hasConfigs
,   standardSetup
) where

import Graphics.UI.Gtk
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Verbosity
import System.FilePath
import Data.Maybe
import System.Directory

import IDE.Core.State
import IDE.Utils.FileUtils
import Graphics.UI.Editor.MakeEditor
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.ModuleName(ModuleName)
import Data.Typeable (Typeable(..))
import Graphics.UI.Editor.Composite
       (versionEditor, versionRangeEditor,
        dependenciesEditor, textsEditor, filesEditor, tupel3Editor,
        eitherOrEditor, maybeEditor, pairEditor, ColumnDescr(..),
        multisetEditor)
import Distribution.Text (simpleParse, display)
import Graphics.UI.Editor.Parameters
    (paraInnerPadding,
     paraInnerAlignment,
     paraOuterPadding,
     paraOuterAlignment,
     Parameter(..),
     paraPack,
     Direction(..),
     paraDirection,
     paraMinSize,
     paraShadow,
     paraSynopsis,
     (<<<-),
     emptyParams,
     paraName,
     getParameterPrim)
import Graphics.UI.Editor.Simple
       (stringEditor, comboEntryEditor,
        staticListMultiEditor, intEditor, boolEditor, fileEditor,
        comboSelectionEditor, multilineStringEditor, textEditor)
import Graphics.UI.Editor.Basics
       (Notifier, Editor(..), GUIEventSelector(..), GUIEvent(..))
import Distribution.Compiler
    (CompilerFlavor(..))
import Distribution.Simple (knownExtensions, Extension(..), VersionRange, anyVersion)
import Default (Default(..))
import IDE.Utils.GUIUtils
import IDE.Pane.SourceBuffer (fileOpenThis)
import Control.Event (EventSource(..))

import qualified Graphics.UI.Gtk.Gdk.Events as GTK (Event(..))
import Data.List (isPrefixOf, sort, nub, sortBy)
import Data.Text (Text)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad (void, when)
import Distribution.PackageDescription.PrettyPrint
       (writeGenericPackageDescription)
import Distribution.Version (Version(..), orLaterVersion)

import Control.Applicative ((<*>), (<$>))
import IDE.Utils.Tool (ToolOutput(..))
import System.Exit (ExitCode(..))
import qualified Data.Conduit.List as CL (fold)
import qualified Data.Conduit as C (Sink, ZipSink(..), getZipSink)
import IDE.Utils.ExternalTool (runExternalTool')
import qualified System.IO.Strict as S (readFile)
import Data.Char (toLower)
import qualified Data.Text as T
       (replace, span, splitAt, isPrefixOf, length, toLower, lines,
        unlines, pack, unpack, null)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T (writeFile, readFile)
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)
import MyMissing (forceJust)
import Language.Haskell.Extension (Language(..))
import Distribution.License (License(..))
import Control.Exception (SomeException(..))
import IDE.Metainfo.Provider (getAllPackageIds)

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

-- | Get the last item
sinkLast = CL.fold (\_ a -> Just a) Nothing

--------------------------------------------------------------------------
-- Handling of Generic Package Descriptions

toGenericPackageDescription :: PackageDescription -> GenericPackageDescription
toGenericPackageDescription pd =
    GenericPackageDescription {
        packageDescription = pd{
            library = Nothing,
            executables = [],
            testSuites = [],
            benchmarks = [],
            buildDepends = []},
        genPackageFlags = [],
        condLibrary = case library pd of
                            Nothing -> Nothing
                            Just lib -> Just (buildCondTreeLibrary lib),
        condExecutables = map buildCondTreeExe (executables pd),
        condTestSuites =  map buildCondTreeTest (testSuites pd),
        condBenchmarks =  map buildCondTreeBenchmark (benchmarks pd)}
  where
    buildCondTreeLibrary lib =
        CondNode {
            condTreeData = lib { libBuildInfo = (libBuildInfo lib) { targetBuildDepends = buildDepends pd } },
            condTreeConstraints = buildDepends pd,
            condTreeComponents = []}
    buildCondTreeExe exe =
        (exeName exe, CondNode {
            condTreeData = exe { buildInfo = (buildInfo exe) { targetBuildDepends = buildDepends pd } },
            condTreeConstraints = buildDepends pd,
            condTreeComponents = []})
    buildCondTreeTest test =
        (testName test, CondNode {
            condTreeData = test { testBuildInfo = (testBuildInfo test) { targetBuildDepends = buildDepends pd } },
            condTreeConstraints = buildDepends pd,
            condTreeComponents = []})
    buildCondTreeBenchmark bm =
        (benchmarkName bm, CondNode {
            condTreeData = bm { benchmarkBuildInfo = (benchmarkBuildInfo bm) { targetBuildDepends = buildDepends pd } },
            condTreeConstraints = buildDepends pd,
            condTreeComponents = []})

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
    let dirName = ipdPackageDir idePackage
    modules <- liftIO $ allModules dirName
    package <- liftIO $ readPackageDescription normal (ipdCabalFile idePackage)
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
        exeConds = foldr (\ (_,condTree) hasConfigs ->
                                (hasConfigs || not (null (condTreeComponents condTree))))
                        False (condExecutables gpd)
        testConds = foldr (\ (_,condTree) hasConfigs ->
                                (hasConfigs || not (null (condTreeComponents condTree))))
                        False (condTestSuites gpd)
    in libConds || exeConds || testConds

hasUnknownTestTypes :: PackageDescription -> Bool
hasUnknownTestTypes pd =
    any unknown $ testSuites pd
  where
    unknown (TestSuite _ (TestSuiteExeV10 _ _) _ _) = False
    unknown _ = True

hasUnknownBenchmarkTypes :: PackageDescription -> Bool
hasUnknownBenchmarkTypes pd =
    any unknown $ benchmarks pd
  where
    unknown (Benchmark _ (BenchmarkExeV10 _ _) _ _) = False
    unknown _ = True

data NewPackage = NewPackage {
    newPackageName :: Text,
    newPackageParentDir :: FilePath,
    templatePackage :: Maybe Text}

packageFields :: FilePath -> FieldDescription NewPackage
packageFields workspaceDir = VFD emptyParams [
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
            (maybeEditor (comboEntryEditor examplePackages, emptyParams) True "")]

examplePackages = [ "hello"
                  , "gtk2hs-hello"
                  , "ghcjs-dom-hello"
                  , "jsaddle-hello"]

newPackageDialog :: Window -> FilePath -> IO (Maybe NewPackage)
newPackageDialog parent workspaceDir = do
    dia                        <-   dialogNew
    set dia [ windowTransientFor := parent
            , windowTitle := __ "Create New Package" ]
    upper                      <-   dialogGetContentArea dia
    lower                      <-   dialogGetActionArea dia
    (widget,inj,ext,_)         <-   buildEditor (packageFields workspaceDir)
                                        (NewPackage "" workspaceDir Nothing)
    okButton <- dialogAddButton dia (__"Create Package") ResponseOk
    dialogAddButton dia (__"Cancel") ResponseCancel
    boxPackStart (castToBox upper) widget PackGrow 7
    set okButton [widgetCanDefault := True]
    widgetGrabDefault okButton
    widgetShowAll dia
    resp  <- dialogRun dia
    value <- ext (NewPackage "" workspaceDir Nothing)
    widgetDestroy dia
    --find
    case resp of
        ResponseOk    -> return value
        _             -> return Nothing

packageNew' :: FilePath -> C.Sink ToolOutput IDEM () -> (Bool -> FilePath -> IDEAction) -> IDEAction
packageNew' workspaceDir log activateAction = do
    windows  <- getWindows
    mbNewPackage <- liftIO $ newPackageDialog (head windows) workspaceDir
    case mbNewPackage of
        Nothing -> return ()
        Just NewPackage{..} | isNothing templatePackage -> do
            let dirName = newPackageParentDir </> T.unpack newPackageName
            mbCabalFile <-  liftIO $ cabalFileName dirName
            window <- getMainWindow
            case mbCabalFile of
                Just cfn -> do
                    add <- liftIO $ do
                        md <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel
                             (T.pack $ printf (__
                              "There is already file %s in this directory. Would you like to add this package to the workspace?")
                              (takeFileName cfn))
                        dialogAddButton md (__ "_Add Package") (ResponseUser 1)
                        dialogSetDefaultResponse md (ResponseUser 1)
                        set md [ windowWindowPosition := WinPosCenterOnParent ]
                        rid <- dialogRun md
                        widgetDestroy md
                        return $ rid == ResponseUser 1
                    when add $ activateAction False cfn
                Nothing -> do
                    liftIO $ createDirectoryIfMissing True dirName
                    isEmptyDir <- liftIO $ isEmptyDirectory dirName
                    make <- if isEmptyDir
                        then return True
                        else liftIO $ do
                            md <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel
                                 (T.pack $ printf (__
                                  "The path you have choosen %s is not an empty directory. Are you sure you want to make a new package here?")
                                  dirName)
                            dialogAddButton md (__ "_Make Package Here") (ResponseUser 1)
                            dialogSetDefaultResponse md (ResponseUser 1)
                            set md [ windowWindowPosition := WinPosCenterOnParent ]
                            rid <- dialogRun md
                            widgetDestroy md
                            return $ rid == ResponseUser 1
                    when make $ do
                        modules <- liftIO $ allModules dirName
                        let Just initialVersion = simpleParse "0.0.1"
                        editPackage emptyPackageDescription {
                            package   = PackageIdentifier (PackageName $ T.unpack newPackageName)
                                                          initialVersion
                          , buildType = Just Simple
                          , specVersionRaw = Right (orLaterVersion (Version [1,12] []))
                          , license = AllRightsReserved
                          , buildDepends = [
                                Dependency (PackageName "base") anyVersion
                              , Dependency (PackageName "QuickCheck") anyVersion
                              , Dependency (PackageName "doctest") anyVersion]
                          , executables = [emptyExecutable {
                                exeName    = T.unpack newPackageName
                              , modulePath = "Main.hs"
                              , buildInfo  = emptyBuildInfo {
                                    hsSourceDirs       = ["src"]
                                  , targetBuildDepends = [Dependency (PackageName "base") anyVersion]
                                  , options            = [(GHC, ["-ferror-spans"])]
                                  , defaultLanguage    = Just Haskell2010}}]
                          , testSuites = [emptyTestSuite {
                                    testName = "test-" ++ T.unpack newPackageName
                                  , testInterface = TestSuiteExeV10 (Version [1,0] []) "Main.hs"
                                  , testBuildInfo = emptyBuildInfo {
                                        hsSourceDirs    = ["test"]
                                      , targetBuildDepends = [
                                            Dependency (PackageName "base") anyVersion
                                          , Dependency (PackageName "QuickCheck") anyVersion
                                          , Dependency (PackageName "doctest") anyVersion]
                                      , options            = [(GHC, ["-ferror-spans"])]
                                      , defaultLanguage = Just Haskell2010}}]
                          , benchmarks =  []
                          } dirName modules (activateAction True)
                    return ()
        Just NewPackage{..} -> cabalUnpack newPackageParentDir (fromJust templatePackage) False (Just newPackageName) log (activateAction False)

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
  liftIO $ do
    dia                        <-   dialogNew
    set dia [ windowTransientFor := parent
            , windowTitle := __ "Copy Installed Package" ]
    upper                      <-   dialogGetContentArea dia
    lower                      <-   dialogGetActionArea dia
    (widget,inj,ext,_)         <-   buildEditor (cloneFields packages workspaceDir)
                                        (ClonePackageSourceRepo "" workspaceDir)
    okButton <- dialogAddButton dia (__"Copy Package") ResponseOk
    dialogAddButton dia (__"Cancel") ResponseCancel
    boxPackStart (castToBox upper) widget PackGrow 7
    set okButton [widgetCanDefault := True]
    widgetGrabDefault okButton
    widgetShowAll dia
    resp  <- dialogRun dia
    value <- ext (ClonePackageSourceRepo "" workspaceDir)
    widgetDestroy dia
    --find
    case resp of
        ResponseOk    -> return value
        _             -> return Nothing

packageClone :: FilePath -> C.Sink ToolOutput IDEM () -> (FilePath -> IDEAction) -> IDEAction
packageClone workspaceDir log activateAction = flip catchIDE (\(e :: SomeException) -> print e) $ do
    windows  <- getWindows
    mbResult <- clonePackageSourceDialog (head windows) workspaceDir
    case mbResult of
        Nothing -> return ()
        Just ClonePackageSourceRepo{..} -> cabalUnpack cloneParentDir packageToClone True Nothing log activateAction

cabalUnpack :: FilePath -> Text -> Bool -> Maybe Text -> C.Sink ToolOutput IDEM () -> (FilePath -> IDEAction) -> IDEAction
cabalUnpack parentDir packageToUnpack sourceRepo mbNewName log activateAction = do
    let tempDir = parentDir </> (T.unpack packageToUnpack ++ ".leksah.temp")
    liftIO $ do
        oldDirExists <- doesDirectoryExist tempDir
        when oldDirExists $ removeDirectoryRecursive tempDir
        createDirectory tempDir
    runExternalTool' (__ "Unpacking") "cabal" (["unpack"]
              ++ ["--source-repository" | sourceRepo]
              ++ ["--destdir=" <> T.pack tempDir, packageToUnpack]) tempDir $ do
        mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> C.ZipSink log
        case mbLastOutput of
            Just (ToolExit ExitSuccess) -> do
                contents <- liftIO $ getDirectoryContents tempDir
                case filter (not . isPrefixOf ".") contents of
                    [] -> do
                        liftIO $ removeDirectoryRecursive tempDir
                        lift $ ideMessage High $ "Nothing found in " <> T.pack tempDir <> " after doing a cabal unpack."
                    [repoName] -> do
                        let destDir = parentDir </> fromMaybe repoName (T.unpack <$> mbNewName)
                        exists <- liftIO $ (||) <$> doesDirectoryExist destDir <*> doesFileExist destDir
                        if exists
                            then lift $ ideMessage High $ T.pack destDir <> " already exists"
                            else do
                                liftIO $ renameDirectory (tempDir </> repoName) destDir
                                mbCabalFile <- liftIO $ cabalFileName destDir
                                window <- lift getMainWindow
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

comparePDE a b = do
    when (pd a /= pd b) $ putStrLn  "pd"
    when (exes a /= exes b) $ putStrLn  "exes"
    when (tests a /= tests b) $ putStrLn  "tests"
    when (mbLib a /= mbLib b) $ putStrLn  "mbLib"
    when (bis a /= bis b) $ putStrLn  "bis"

fromEditor :: PackageDescriptionEd -> PackageDescription
fromEditor (PDE pd exes'
        tests'
        benchmarks'
        mbLib' buildInfos) =
    let     exes = map (\ (Executable' s fb bii) -> if bii + 1 > length buildInfos
                                        then Executable (T.unpack s) fb (buildInfos !! (length buildInfos - 1))
                                        else Executable (T.unpack s) fb (buildInfos !! bii)) exes'
            tests = map (\ (Test' s fb bii) -> if bii + 1 > length buildInfos
                                        then TestSuite (T.unpack s) fb (buildInfos !! (length buildInfos - 1)) False
                                        else TestSuite (T.unpack s) fb (buildInfos !! bii) False) tests'
            bms = map (\ (Benchmark' s fb bii) -> if bii + 1 > length buildInfos
                                        then Benchmark (T.unpack s) fb (buildInfos !! (length buildInfos - 1)) False
                                        else Benchmark (T.unpack s) fb (buildInfos !! bii) False) benchmarks'
            mbLib = case mbLib' of
                    Nothing -> Nothing
#if MIN_VERSION_Cabal(1,22,0)
                    Just (Library' mn rmn rs es b bii) -> if bii + 1 > length buildInfos
                                        then Just (Library mn rmn rs es b (buildInfos !! (length buildInfos - 1)))
                                        else Just (Library mn rmn rs es b (buildInfos !! bii))
#else
                    Just (Library' mn b bii) -> if bii + 1 > length buildInfos
                                        then Just (Library mn b (buildInfos !! (length buildInfos - 1)))
                                        else Just (Library mn b (buildInfos !! bii))
#endif
    in pd {
        library = mbLib
      , executables = exes
      , testSuites = tests
      , benchmarks = bms
      }

toEditor :: PackageDescription -> PackageDescriptionEd
toEditor pd =
    let     (exes,exeBis) = unzip $ map (\(Executable s fb bi, i) -> (Executable' (T.pack s) fb i, bi))
                            (zip (executables pd) [0..])
            (tests,testBis) = unzip $ map (\(TestSuite s fb bi _, i) -> (Test' (T.pack s) fb i, bi))
                            (zip (testSuites pd) [length exeBis..])
            (bms,benchmarkBis) = unzip $ map (\(Benchmark s fb bi _, i) -> (Benchmark' (T.pack s) fb i, bi))
                            (zip (benchmarks pd) [length testBis..])
            bis = exeBis ++ testBis ++ benchmarkBis
            (mbLib,bis2) = case library pd of
                    Nothing                -> (Nothing,bis)
#if MIN_VERSION_Cabal(1,22,0)
                    Just (Library mn rmn rs es b bi) -> (Just (Library' (sort mn) rmn rs es b (length bis)), bis ++ [bi])
#else
                    Just (Library mn b bi) -> (Just (Library' (sort mn) b (length bis)), bis ++ [bi])
#endif
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
    packageBox              ::   VBox,
    packageNotifer          ::   Notifier
} deriving Typeable


data PackageState = PackageState
    deriving (Read, Show, Typeable)

instance Pane PackagePane IDEM
    where
    primPaneName _  =   __ "Package"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . packageBox
    paneId b        =   "*Package"

instance RecoverablePane PackagePane PackageState IDEM where
    saveState p     =   return Nothing
    recoverState pp st  =  return Nothing
    buildPane panePath notebook builder = return Nothing
    builder pp nb w =    return (Nothing,[])

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
        Just p -> liftIO $ bringPaneToFront p

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
    let initialPackagePath = packageDir </> (display . pkgName . package . pd) packageD ++ ".cabal"
    packageInfos <- getAllPackageIds
    mbP <- buildThisPane panePath nb
        (builder' packageDir packageD packageDescr afterSaveAction
            initialPackagePath modules packageInfos fields origPackageD)
    case mbP of
        Nothing -> return ()
        Just (PackagePane{packageNotifer = pn}) -> do
            liftIO $ triggerEvent pn GUIEvent{selector = MayHaveChanged, eventText = "",
                                              gtkReturn = True}
            return ()

builder' :: FilePath ->
    PackageDescriptionEd ->
    FieldDescription PackageDescriptionEd ->
    (FilePath -> IDEAction) ->
    FilePath ->
    [ModuleName] ->
    [PackageId] ->
    [FieldDescription PackageDescriptionEd] ->
    PackageDescriptionEd ->
    PanePath ->
    Notebook ->
    Window ->
    IDEM (Maybe PackagePane,Connections)
builder' packageDir packageD packageDescr afterSaveAction initialPackagePath modules packageInfos fields
    origPackageD panePath nb window  = reifyIDE $ \ ideR -> do
    vb      <-  vBoxNew False 0
    bb      <-  hButtonBoxNew
    save    <- buttonNewFromStock "gtk-save"
    widgetSetSensitive save False
    closeB  <- buttonNewFromStock "gtk-close"
    addB    <- buttonNewFromStock (__ "Add Build Info")
    removeB <- buttonNewFromStock (__ "Remove Build Info")
    label   <-  labelNew (Nothing :: Maybe Text)
    boxPackStart bb addB PackNatural 0
    boxPackStart bb removeB PackNatural 0
    boxPackEnd bb closeB PackNatural 0
    boxPackEnd bb save PackNatural 0
    (widget, setInj, getExt, notifier)  <-  buildEditor packageDescr packageD
    let packagePane = PackagePane vb notifier
    boxPackStart vb widget PackGrow 7
    boxPackStart vb label PackNatural 0
    boxPackEnd vb bb PackNatural 7

    let fieldNames = map (fromMaybe (__ "Unnamed") . getParameterPrim paraName . parameters) fields
    on addB buttonActivated $ do
        mbNewPackage' <- extract packageD [getExt]
        case mbNewPackage' of
            Nothing -> sysMessage Normal (__ "Content doesn't validate")
            Just pde -> reflectIDE (do
                    closePane packagePane
                    initPackage packageDir pde {bis = bis pde ++ [head (bis pde)]}
                        (packageDD
                            packageInfos
                            packageDir
                            modules
                            (length (bis pde) + 1)
                            (concatMap (buildInfoD (Just packageDir) modules)
                                [0..length (bis pde)]))
                        panePath nb modules afterSaveAction origPackageD) ideR
    on removeB buttonActivated $ do
        mbNewPackage' <- extract packageD [getExt]
        case mbNewPackage' of
            Nothing -> sysMessage Normal (__ "Content doesn't validate")
            Just pde | length (bis pde) == 1  -> sysMessage Normal (__ "Just one Build Info")
                     | otherwise -> reflectIDE (do
                        closePane packagePane
                        initPackage packageDir pde{bis = take (length (bis pde) - 1) (bis pde)}
                            (packageDD
                                packageInfos
                                packageDir
                                modules
                                (length (bis pde) - 1)
                                (concatMap (buildInfoD (Just packageDir) modules)
                                    [0..length (bis pde) - 2]))
                            panePath nb modules afterSaveAction origPackageD) ideR
    on closeB buttonActivated $ do
        mbP <- extract packageD [getExt]
        let hasChanged = case mbP of
                                Nothing -> False
                                Just p -> p /= origPackageD
        if not hasChanged
            then reflectIDE (void (closePane packagePane)) ideR
            else do
                md <- messageDialogNew (Just window) []
                    MessageQuestion
                    ButtonsYesNo
                    (__ "Unsaved changes. Close anyway?")
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetDestroy md
                case resp of
                    ResponseYes ->   reflectIDE (void (closePane packagePane)) ideR
                    _  ->   return ()
    on save buttonActivated $ do
        mbNewPackage' <- extract packageD [getExt]
        case mbNewPackage' of
            Nothing -> return ()
            Just newPackage' -> let newPackage = fromEditor newPackage' in do
                let packagePath = packageDir </> (display . pkgName . package . pd) newPackage'
                                                ++ ".cabal"
                writeGenericPackageDescription packagePath (toGenericPackageDescription newPackage)
                reflectIDE (do
                    afterSaveAction packagePath
                    closePane packagePane
                    return ()) ideR
    registerEvent notifier MayHaveChanged (\ e -> do
        mbP <- extract packageD [getExt]
        let hasChanged = case mbP of
                                Nothing -> False
                                Just p -> p /= origPackageD
        when (isJust mbP) $ labelSetMarkup label ("" :: Text)
        when (isJust mbP) $ comparePDE (fromJust mbP) packageD
        markLabel nb (getTopWidget packagePane) hasChanged
        widgetSetSensitive save hasChanged
        return (e{gtkReturn=False}))
    registerEvent notifier ValidationError (\e -> do
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
                    $ paraShadow <<<- ParaShadow ShadowOut
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
#if MIN_VERSION_Cabal(1,22,0)
            (paraName <<<- ParaName (__ "License Files")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "A list of license files.")
                    $ paraDirection <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,250)
                            $ emptyParams)
            (licenseFiles . pd)
            (\ a b -> b{pd = (pd b){licenseFiles = a}})
            (filesEditor (Just fp) FileChooserActionOpen (__ "Select File"))
#else
            (paraName <<<- ParaName (__ "License File") $ emptyParams)
            (licenseFile . pd)
            (\ a b -> b{pd = (pd b){licenseFile = a}})
            (fileEditor (Just fp) FileChooserActionOpen (__ "Select file"))
#endif
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
                    $ paraDirection <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,250)
                            $ emptyParams)
            (nub . buildDepends . pd)
            (\ a b -> b{pd = (pd b){buildDepends = a}})
            (dependenciesEditor packages)
    ]),
    (__ "Meta Dep.", VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Cabal version")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "Does this package depends on a specific version of Cabal?")
                    $ paraShadow <<<- ParaShadow ShadowIn $ emptyParams)
            (specVersion . pd)
            (\ a b -> b{pd = (pd b){specVersionRaw = Left a}})
            versionEditor
    ,   mkField
            (paraName <<<- ParaName (__ "Tested with compiler")
                $ paraShadow <<<- ParaShadow ShadowIn
                    $ paraDirection <<<- ParaDirection Vertical
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
                    $ paraDirection <<<- ParaDirection Vertical
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
                    $ paraDirection <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,120)
                            $ emptyParams)
            (extraSrcFiles . pd)
            (\ a b -> b{pd = (pd b){extraSrcFiles = a}})
            (filesEditor (Just fp) FileChooserActionOpen (__ "Select File"))
    ,   mkField
            (paraName <<<-  ParaName (__ "Extra Tmp Files")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "A list of additional files or directories to be removed by setup clean.")
                    $ paraDirection <<<- ParaDirection Vertical
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
                        $ paraShadow <<<- ParaShadow ShadowIn
                            $ paraDirection <<<- ParaDirection Vertical
                                $ emptyParams)
            (buildType . pd)
            (\ a b -> b{pd = (pd b){buildType = a}})
            (maybeEditor (buildTypeEditor, emptyParams) True (__ "Specify?"))
    ,   mkField
            (paraName <<<- ParaName (__ "Custom Fields")
                $ paraShadow <<<- ParaShadow ShadowIn
                    $ paraMinSize <<<- ParaMinSize (-1,150)
                        $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (customFieldsPD . pd)
            (\ a b -> b{pd = (pd b){customFieldsPD = a}})
            (multisetEditor
                (ColumnDescr True [(__ "Name",\(n,_) -> [cellText := T.pack n])
                                   ,(__ "Value",\(_,v) -> [cellText := T.pack v])])
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
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            exes
            (\ a b -> b{exes = a})
            (executablesEditor (Just fp) modules numBuildInfos)
    ]),
    (__ "Tests",VFD emptyParams  [
        mkField
            (paraName <<<- ParaName (__ "Tests")
                $ paraSynopsis <<<- ParaSynopsis
                (__ "Describe tests contained in the package")
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            tests
            (\ a b -> b{tests = a})
            (testsEditor (Just fp) modules numBuildInfos)
    ]),
    (__ "Benchmarks",VFD emptyParams  [
        mkField
            (paraName <<<- ParaName (__ "Benchmarks")
                $ paraSynopsis <<<- ParaSynopsis
                (__ "Describe tests contained in the package")
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            bms
            (\ a b -> b{bms = a})
            (benchmarksEditor (Just fp) modules numBuildInfos)
    ]),
    (__ "Library", VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Library")
           $ paraSynopsis <<<- ParaSynopsis
             (__ "If the package contains a library, specify the exported modules here")
           $ paraDirection <<<- ParaDirection Vertical
           $ paraShadow <<<- ParaShadow ShadowIn $ emptyParams)
            mbLib
            (\ a b -> b{mbLib = a})
            (maybeEditor (libraryEditor (Just fp) modules numBuildInfos,
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
                    $ paraShadow  <<<- ParaShadow ShadowIn
                        $ paraDirection  <<<- ParaDirection Vertical
                            $ paraMinSize <<<- ParaMinSize (-1,150)
                                $ emptyParams)
            (hsSourceDirs . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{hsSourceDirs = a})})
            (filesEditor fp FileChooserActionSelectFolder (__ "Select folder"))
    ,   mkField
            (paraName <<<- ParaName (__ "Non-exposed or non-main modules")
            $ paraSynopsis <<<- ParaSynopsis
                                       (__ "A list of modules used by the component but not exposed to users.")
                $ paraShadow <<<- ParaShadow ShadowIn
                    $ paraDirection <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,300)
                            $ paraPack <<<- ParaPack PackGrow
                                $ emptyParams)
            (map (T.pack . display) . otherModules . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi ->
                bi{otherModules = map (\i -> forceJust (simpleParse $ T.unpack i)
                "   PackageEditor >> buildInfoD: no parse for moduile name" ) a})})
            (modulesEditor modules)
    ]),
    (T.pack $ printf (__ "%s Compiler ") (show (i + 1)), VFD emptyParams [
        mkField
            (paraName  <<<- ParaName (__ "Options for haskell compilers")
            $ paraDirection <<<- ParaDirection Vertical
            $ paraShadow <<<- ParaShadow ShadowIn
            $ paraPack <<<- ParaPack PackGrow $ emptyParams)
            (options . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{options = a})})
            (multisetEditor
                (ColumnDescr True [( __ "Compiler Flavor",\(cv,_) -> [cellText := T.pack $ show cv])
                                   ,(__ "Options",\(_,op) -> [cellText := T.pack $ concatMap (\s -> ' ' : s) op])])
                (pairEditor
                    (compilerFlavorEditor,emptyParams)
                    (optsEditor,emptyParams),
                        paraDirection <<<- ParaDirection Vertical
                            $ paraShadow  <<<- ParaShadow ShadowIn $ emptyParams)
                Nothing
                Nothing)
#if MIN_VERSION_Cabal(1,22,0)
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
#else
     ,  mkField
            (paraName <<<- ParaName (__ "Additional options for GHC when built with profiling")
           $ emptyParams)
            (ghcProfOptions . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{ghcProfOptions = a})})
            optsEditor
     ,  mkField
            (paraName <<<- ParaName (__ "Additional options for GHC when the package is built as shared library")
           $ emptyParams)
            (ghcSharedOptions . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{ghcSharedOptions = a})})
            optsEditor
#endif
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
    (T.pack $ printf (__ "%s Build Tools ") (show (i + 1)), VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "Tools needed for a build")
                $ paraDirection <<<- ParaDirection Vertical
                    $ paraMinSize <<<- ParaMinSize (-1,120)
                        $ emptyParams)
            (buildTools . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{buildTools = a})})
            (dependenciesEditor [])
    ]),
    (T.pack $ printf (__ "%s Pkg Config ") (show (i + 1)), VFD emptyParams [
        mkField
            (paraName <<<- ParaName (__ "A list of pkg-config packages, needed to build this package")
                $ paraDirection <<<- ParaDirection Vertical
                    $ paraMinSize <<<- ParaMinSize (-1,120)
                        $ emptyParams)
            (pkgconfigDepends . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{pkgconfigDepends = a})})
            (dependenciesEditor [])
    ]),
    (T.pack $ printf (__ "%s Opts C -1-") (show (i + 1)), VFD emptyParams [
         mkField
            (paraName <<<- ParaName (__ "Options for C compiler")
                $ paraDirection <<<- ParaDirection Vertical
                    $ emptyParams)
            (ccOptions . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{ccOptions = a})})
            optsEditor
    ,    mkField
            (paraName <<<- ParaName (__ "Options for linker")
                $ paraDirection <<<- ParaDirection Vertical
                    $ emptyParams)
            (ldOptions . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{ldOptions = a})})
            optsEditor
    ,    mkField
            (paraName <<<- ParaName (__ "A list of header files to use when compiling")
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (map T.pack . includes . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{includes = map T.unpack a})})
            (textsEditor (const True) True)
     ,   mkField
            (paraName <<<- ParaName (__ "A list of header files to install")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (installIncludes . (!! i) . bis)
             (\ a b -> b{bis = update (bis b) i (\bi -> bi{installIncludes = a})})
           (filesEditor fp FileChooserActionOpen (__ "Select File"))
    ]),
    (T.pack $ printf (__ "%s Opts C -2-") (show (i + 1)), VFD emptyParams [
         mkField
            (paraName <<<- ParaName (__ "A list of directories to search for header files")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (includeDirs . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{includeDirs = a})})
            (filesEditor fp FileChooserActionSelectFolder (__ "Select Folder"))
     ,   mkField
            (paraName <<<- ParaName
                (__ "A list of C source files to be compiled,linked with the Haskell files.")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (cSources . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{cSources = a})})
            (filesEditor fp FileChooserActionOpen (__ "Select file"))
    ]),
    (T.pack $ printf (__ "%s Opts Libs ") (show (i + 1)), VFD emptyParams [
         mkField
            (paraName <<<- ParaName (__ "A list of extra libraries to link with")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (map T.pack . extraLibs . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{extraLibs = map T.unpack a})})
            (textsEditor (const True) True)
     ,   mkField
            (paraName <<<- ParaName (__ "A list of directories to search for libraries.")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (extraLibDirs . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{extraLibDirs = a})})
            (filesEditor fp FileChooserActionSelectFolder (__ "Select Folder"))
   ]),
    (T.pack $ printf (__ "%s Other") (show (i + 1)), VFD emptyParams [
         mkField
            (paraName <<<- ParaName (__ "Options for C preprocessor")
                $ paraDirection <<<- ParaDirection Vertical
                    $ emptyParams)
            (cppOptions . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{cppOptions = a})})
            optsEditor
    ,   mkField
            (paraName <<<- ParaName (__ "Support frameworks for Mac OS X")
                $ paraMinSize <<<- ParaMinSize (-1,150)
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (map T.pack . frameworks . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{frameworks = map T.unpack a})})
            (textsEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName (__ "Custom fields build info")
                $ paraShadow <<<- ParaShadow ShadowIn
                    $ paraMinSize <<<- ParaMinSize (-1,150)
                        $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
             (customFieldsBI . (!! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{customFieldsBI = a})})
            (multisetEditor
                (ColumnDescr True [(__ "Name",\(n,_) -> [cellText := T.pack n])
                                   ,(__ "Value",\(_,v) -> [cellText := T.pack v])])
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
        xinj ("") = inj ""
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
        (paraDirection <<<- ParaDirection Vertical $ para)

compilerOptsEditor :: Editor [(CompilerFlavor, [String])]
compilerOptsEditor p =
    multisetEditor
        (ColumnDescr True [("Compiler",\(compiler, _) -> [cellText := T.pack $ show compiler])
                           ,("Options",\(_, opts    ) -> [cellText := T.pack $ unwords opts])])
        (compilerOptRecordEditor,
            paraOuterAlignment <<<- ParaInnerAlignment (0.0, 0.5, 1.0, 1.0)
                $ paraInnerAlignment <<<- ParaOuterAlignment (0.0, 0.5, 1.0, 1.0)
                   $ emptyParams)
        (Just (sortBy (\ (f1, _) (f2, _) -> compare f1 f2)))
        (Just (\ (f1, _) (f2, _) -> f1 == f2))
        (paraShadow <<<- ParaShadow ShadowIn
            $ paraOuterAlignment <<<- ParaInnerAlignment (0.0, 0.5, 1.0, 1.0)
                $ paraInnerAlignment <<<- ParaOuterAlignment (0.0, 0.5, 1.0, 1.0)
                    $ paraDirection  <<<-  ParaDirection Vertical
                        $ paraPack <<<- ParaPack PackGrow
                            $ p)

packageEditor :: Editor PackageIdentifier
packageEditor para noti = do
    (wid,inj,ext) <- pairEditor
        (stringEditor (not . null) True, paraName <<<- ParaName (__ "Name") $ emptyParams)
        (versionEditor, paraName <<<- ParaName (__ "Version") $ emptyParams)
        (paraDirection <<<- ParaDirection Horizontal
            $ paraShadow <<<- ParaShadow ShadowIn
                $ para) noti
    let pinj (PackageIdentifier (PackageName n) v) = inj (n,v)
    let pext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just (n,v) -> return $
                    if null n
                        then Nothing
                        else Just $ PackageIdentifier (PackageName n) v
    return (wid,pinj,pext)

testedWithEditor :: Editor [(CompilerFlavor, VersionRange)]
testedWithEditor =
    multisetEditor
       (ColumnDescr True [(__ "Compiler Flavor",\(cv,_) -> [cellText := T.pack $ show cv])
                           ,(__ "Version Range",\(_,vr) -> [cellText := T.pack $ display vr])])
       (pairEditor
          (compilerFlavorEditor,
           paraShadow <<<- ParaShadow ShadowNone $ emptyParams)
          (versionRangeEditor,
           paraShadow <<<- ParaShadow ShadowNone $ emptyParams),
        paraDirection <<<- ParaDirection Vertical $ emptyParams)
       Nothing
       (Just (==))

compilerFlavorEditor :: Editor CompilerFlavor
compilerFlavorEditor para noti = do
    (wid,inj,ext) <- eitherOrEditor
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
    (wid,inj,ext) <- eitherOrEditor
        (comboSelectionEditor flavors (T.pack . show), paraName <<<- ParaName (__ "Select") $ emptyParams)
        (textEditor (const True) True, paraName <<<- ParaName (__ "Unknown") $ emptyParams)
        (__ "Unknown")
        (paraName <<<- ParaName (__ "Select") $ para)
        noti
    let cfinj comp  = case comp of
                        (UnknownBuildType str) -> inj (Right $ T.pack str)
                        other               -> inj (Left other)
    let cfext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just (Right s) -> return (Just . UnknownBuildType $ T.unpack s)
                Just (Left other) -> return (Just other)
    return (wid,cfinj,cfext)
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
        (ColumnDescr False [("",\repo -> [cellText := display repo])])
        (repoEditor,
            paraOuterAlignment <<<- ParaInnerAlignment (0.0, 0.5, 1.0, 1.0)
                $ paraInnerAlignment <<<- ParaOuterAlignment (0.0, 0.5, 1.0, 1.0)
                   $ emptyParams)
        Nothing
        Nothing
        (paraShadow <<<- ParaShadow ShadowIn $
            paraOuterAlignment <<<- ParaInnerAlignment (0.0, 0.5, 1.0, 1.0)
                $ paraInnerAlignment <<<- ParaOuterAlignment (0.0, 0.5, 1.0, 1.0)
                    $ paraDirection  <<<-  ParaDirection Vertical
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
                            (paraDirection  <<<- ParaDirection Vertical $ noBorder)
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
    exposedModules'    :: [ModuleName]
#if MIN_VERSION_Cabal(1,22,0)
,   reexportedModules' :: [ModuleReexport]
,   requiredSignatures':: [ModuleName]
,   exposedSignatures' :: [ModuleName]
#endif
,   libExposed'        :: Bool
,   libBuildInfoIdx    :: Int}
    deriving (Show, Eq)

data Executable' = Executable'{
    exeName'        :: Text
,   modulePath'     :: FilePath
,   buildInfoIdx    :: Int}
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
#if MIN_VERSION_Cabal(1,22,0)
    where getDefault =  Library' [] [] [] [] getDefault getDefault
#else
    where getDefault =  Library' [] getDefault getDefault
#endif

instance Default Executable'
    where getDefault = Executable' getDefault getDefault getDefault

instance Default Test'
    where getDefault = Test' getDefault (TestSuiteExeV10 (Version [1,0] []) getDefault) getDefault

instance Default Benchmark'
    where getDefault = Benchmark' getDefault (BenchmarkExeV10 (Version [1,0] []) getDefault) getDefault

#if MIN_VERSION_Cabal(1,22,0)
libraryEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Library'
libraryEditor fp modules numBuildInfos para noti = do
    (wid,inj,ext) <-
        pairEditor
            (tupel3Editor
                (boolEditor,
                paraName <<<- ParaName (__ "Exposed")
                $ paraSynopsis <<<- ParaSynopsis (__ "Is the lib to be exposed by default?")
                $ emptyParams)
                (modulesEditor (sort modules),
                paraName <<<- ParaName (__ "Exposed Modules")
                $ paraMinSize <<<- ParaMinSize (-1,300)
                $ para)
                (buildInfoEditorP numBuildInfos, paraName <<<- ParaName (__ "Build Info")
                $ paraPack <<<- ParaPack PackNatural
                $ para),
                paraDirection <<<- ParaDirection Vertical
                $ emptyParams)
            (tupel3Editor
                (modulesEditor (sort modules),
                paraName <<<- ParaName (__ "Reexported Modules")
                $ paraMinSize <<<- ParaMinSize (-1,300)
                $ para)
                (modulesEditor (sort modules),
                paraName <<<- ParaName (__ "Required Signatures")
                $ paraMinSize <<<- ParaMinSize (-1,300)
                $ para)
                (modulesEditor (sort modules),
                paraName <<<- ParaName (__ "Exposed Signatures")
                $ paraMinSize <<<- ParaMinSize (-1,300)
                $ para),
                paraDirection <<<- ParaDirection Vertical
                $ emptyParams)
            (paraDirection <<<- ParaDirection Vertical
            $ emptyParams)
            noti
    let pinj (Library' em rmn rs es exp bi) = inj ((exp, map (T.pack . display) em,bi), (map (T.pack . display) rmn, map (T.pack . display) rs, map (T.pack . display) es))
        parseModuleNames = map (\s -> forceJust (simpleParse $ T.unpack s)
                    "SpecialEditor >> libraryEditor: no parse for moduile name")
        parseRexportedModules = map (\s -> forceJust (simpleParse $ T.unpack s)
                    "SpecialEditor >> libraryEditor: no parse for moduile name")
        pext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just ((exp,em,bi), (rmn, rs, es)) -> return (Just $ Library'
                    (parseModuleNames em)
                    (parseRexportedModules rmn)
                    (parseModuleNames rs)
                    (parseModuleNames es) exp bi)
    return (wid,pinj,pext)
#else
libraryEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Library'
libraryEditor fp modules numBuildInfos para noti = do
    (wid,inj,ext) <-
        tupel3Editor
            (boolEditor,
            paraName <<<- ParaName (__ "Exposed")
            $ paraSynopsis <<<- ParaSynopsis (__ "Is the lib to be exposed by default?")
            $ emptyParams)
            (modulesEditor (sort modules),
            paraName <<<- ParaName (__ "Exposed Modules")
            $ paraMinSize <<<- ParaMinSize (-1,300)
            $ para)
            (buildInfoEditorP numBuildInfos, paraName <<<- ParaName (__ "Build Info")
            $ paraPack <<<- ParaPack PackNatural
            $ para)
            (paraDirection <<<- ParaDirection Vertical
            $ emptyParams)
            noti
    let pinj (Library' em exp bi) = inj (exp, map (T.pack . display) em,bi)
    let pext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just (exp,em,bi) -> return (Just $ Library' (map (\s -> forceJust (simpleParse $ T.unpack s)
                    "SpecialEditor >> libraryEditor: no parse for moduile name") em) exp bi)
    return (wid,pinj,pext)
#endif

modulesEditor :: [ModuleName] -> Editor [Text]
modulesEditor modules   =   staticListMultiEditor (map (T.pack . display) modules) id

executablesEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor [Executable']
executablesEditor fp modules countBuildInfo p =
    multisetEditor
        (ColumnDescr True [(__ "Executable Name",\(Executable' exeName _ _) -> [cellText := exeName])
                           ,(__ "Module Path",\(Executable'  _ mp _) -> [cellText := T.pack mp])
                           ,(__ "Build info index",\(Executable'  _ _ bii) -> [cellText := T.pack $ show (bii + 1)])])
        (executableEditor fp modules countBuildInfo,emptyParams)
        Nothing
        Nothing
        (paraShadow  <<<- ParaShadow ShadowIn
            $ paraMinSize <<<- ParaMinSize (-1,200) $ p)

executableEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Executable'
executableEditor fp modules countBuildInfo para noti = do
    (wid,inj,ext) <- tupel3Editor
        (textEditor (not . T.null) True,
            paraName <<<- ParaName (__ "Executable Name")
            $ emptyParams)
        (stringEditor (not . null) True,
            paraDirection <<<- ParaDirection Vertical
            $ paraName <<<- ParaName (__ "File with main function")
            $ emptyParams)
        (buildInfoEditorP countBuildInfo, paraName <<<- ParaName (__ "Build Info")
            $ paraOuterAlignment <<<- ParaOuterAlignment  (0.0, 0.0, 0.0, 0.0)
                $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                    $ paraInnerAlignment <<<- ParaInnerAlignment  (0.0, 0.0, 0.0, 0.0)
                        $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                            $ emptyParams)
        (paraDirection  <<<- ParaDirection Vertical $ para)
        noti
    let pinj (Executable' s f bi) = inj (s,f,bi)
    let pext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just (s,f,bi) -> return (Just $Executable' s f bi)
    return (wid,pinj,pext)

testsEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor [Test']
testsEditor fp modules countBuildInfo p =
    multisetEditor
        (ColumnDescr True [(__ "Test Name",\(Test' testName _ _) -> [cellText := testName])
                           ,(__ "Interface",\(Test'  _ i _) -> [cellText := T.pack $ interfaceName i])
                           ,(__ "Build info index",\(Test'  _ _ bii) -> [cellText := T.pack $ show (bii + 1)])])
        (testEditor fp modules countBuildInfo,emptyParams)
        Nothing
        Nothing
        (paraShadow  <<<- ParaShadow ShadowIn
            $ paraMinSize <<<- ParaMinSize (-1,200) $ p)
  where
    interfaceName (TestSuiteExeV10 _ f) = f
    interfaceName i = show i

testEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Test'
testEditor fp modules countBuildInfo para noti = do
    (wid,inj,ext) <- tupel3Editor
        (textEditor (not . T.null) True,
            paraName <<<- ParaName (__ "Test Name")
            $ emptyParams)
        (stringEditor (not . null) True,
            paraDirection <<<- ParaDirection Vertical
            $ paraName <<<- ParaName (__ "File with main function")
            $ emptyParams)
        (buildInfoEditorP countBuildInfo, paraName <<<- ParaName (__ "Build Info")
            $ paraOuterAlignment <<<- ParaOuterAlignment  (0.0, 0.0, 0.0, 0.0)
                $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                    $ paraInnerAlignment <<<- ParaInnerAlignment  (0.0, 0.0, 0.0, 0.0)
                        $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                            $ emptyParams)
        (paraDirection  <<<- ParaDirection Vertical $ para)
        noti
    let pinj (Test' s (TestSuiteExeV10 (Version [1,0] []) f) bi) = inj (s,f,bi)
        pinj _ = error "Unexpected Test Interface"
    let pext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just (s,f,bi) -> return (Just $Test' s (TestSuiteExeV10 (Version [1,0] []) f) bi)
    return (wid,pinj,pext)

benchmarksEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor [Benchmark']
benchmarksEditor fp modules countBuildInfo p =
    multisetEditor
        (ColumnDescr True [(__ "Benchmark Name",\(Benchmark' benchmarkName _ _) -> [cellText := benchmarkName])
                           ,(__ "Interface",\(Benchmark'  _ i _) -> [cellText := T.pack $ interfaceName i])
                           ,(__ "Build info index",\(Benchmark'  _ _ bii) -> [cellText := T.pack $ show (bii + 1)])])
        (benchmarkEditor fp modules countBuildInfo,emptyParams)
        Nothing
        Nothing
        (paraShadow  <<<- ParaShadow ShadowIn
            $ paraMinSize <<<- ParaMinSize (-1,200) $ p)
  where
    interfaceName (BenchmarkExeV10 _ f) = f
    interfaceName i = show i

benchmarkEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Benchmark'
benchmarkEditor fp modules countBuildInfo para noti = do
    (wid,inj,ext) <- tupel3Editor
        (textEditor (not . T.null) True,
            paraName <<<- ParaName (__ "Benchmark Name")
            $ emptyParams)
        (stringEditor (not . null) True,
            paraDirection <<<- ParaDirection Vertical
            $ paraName <<<- ParaName (__ "File with main function")
            $ emptyParams)
        (buildInfoEditorP countBuildInfo, paraName <<<- ParaName (__ "Build Info")
            $ paraOuterAlignment <<<- ParaOuterAlignment  (0.0, 0.0, 0.0, 0.0)
                $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                    $ paraInnerAlignment <<<- ParaInnerAlignment  (0.0, 0.0, 0.0, 0.0)
                        $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                            $ emptyParams)
        (paraDirection  <<<- ParaDirection Vertical $ para)
        noti
    let pinj (Benchmark' s (BenchmarkExeV10 (Version [1,0] []) f) bi) = inj (s,f,bi)
        pinj _ = error "Unexpected Benchmark Interface"
    let pext = do
            mbp <- ext
            case mbp of
                Nothing -> return Nothing
                Just (s,f,bi) -> return (Just $Benchmark' s (BenchmarkExeV10 (Version [1,0] []) f) bi)
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
    where getDefault =  GHC

instance Default BuildInfo
    where getDefault =  emptyBuildInfo

instance Default Library
    where getDefault =  Library []
#if MIN_VERSION_Cabal(1,22,0)
                            [] [] []
#endif
                            getDefault getDefault

instance Default Executable
    where getDefault = Executable getDefault getDefault getDefault

instance Default RepoType
    where getDefault = Darcs

instance Default RepoKind
    where getDefault = RepoThis

instance Default SourceRepo
    where getDefault =  SourceRepo getDefault getDefault getDefault getDefault getDefault
                                    getDefault getDefault

instance Default BuildType
    where getDefault = Simple


