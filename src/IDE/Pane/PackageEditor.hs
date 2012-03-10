{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable,
             MultiParamTypeClasses, TypeSynonymInstances #-}
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
,   packageEdit
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
    (versionEditor,
     versionRangeEditor,
     dependenciesEditor,
     stringsEditor,
     filesEditor,
     tupel3Editor,
     eitherOrEditor,
     maybeEditor,
     pairEditor,
     ColumnDescr(..),
     multisetEditor)
import Distribution.Text (simpleParse, display)
import MyMissing
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
    (staticListMultiEditor,
     intEditor,
     boolEditor,
     fileEditor,
     comboSelectionEditor,
     multilineStringEditor,
     stringEditor)
import Graphics.UI.Editor.Basics
       (Notifier, Editor(..), GUIEventSelector(..), GUIEvent(..))
import Distribution.Compiler
    (CompilerFlavor(..))
#if !MIN_VERSION_Cabal(1,11,0)
import Distribution.Simple (knownExtensions)
#endif
import Distribution.Simple (Extension(..), VersionRange, anyVersion)
import Default (Default(..))
import IDE.Utils.GUIUtils
import IDE.Pane.SourceBuffer (fileOpenThis)
import Control.Event (EventSource(..))
#if !MIN_VERSION_Cabal(1,8,0)
import Distribution.License
#endif

import qualified Graphics.UI.Gtk.Gdk.Events as GTK (Event(..))
import Data.List (sort,nub)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad (when)
#if MIN_VERSION_Cabal(1,10,0)
import Distribution.PackageDescription.PrettyPrintCopied
       (writeGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse
       (writePackageDescription)
#endif
import Distribution.Version (Version(..), orLaterVersion)

--------------------------------------------------------------------------
-- Handling of Generic Package Descriptions

#if MIN_VERSION_Cabal(1,10,0)
toGenericPackageDescription :: PackageDescription -> GenericPackageDescription
toGenericPackageDescription pd =
    GenericPackageDescription {
        packageDescription = pd{
            library = Nothing,
            executables = [],
            testSuites = [],
            buildDepends = []},
        genPackageFlags = [],
        condLibrary = case library pd of
                            Nothing -> Nothing
                            Just lib -> Just (buildCondTreeLibrary lib),
        condExecutables = map buildCondTreeExe (executables pd),
        condTestSuites =  map buildCondTreeTest (testSuites pd)}
  where
    buildCondTreeLibrary lib =
        CondNode {
            condTreeData = lib,
            condTreeConstraints = buildDepends pd,
            condTreeComponents = []}
    buildCondTreeExe exe =
        (exeName exe, CondNode {
            condTreeData = exe,
            condTreeConstraints = buildDepends pd,
            condTreeComponents = []})
    buildCondTreeTest test =
        (testName test, CondNode {
            condTreeData = test,
            condTreeConstraints = buildDepends pd,
            condTreeComponents = []})
#endif

-- ---------------------------------------------------------------------
-- The exported stuff goes here
--

choosePackageDir :: Window -> Maybe FilePath -> IO (Maybe FilePath)
choosePackageDir window mbDir = chooseDir window "Select root folder for package" mbDir

choosePackageFile :: Window -> Maybe FilePath -> IO (Maybe FilePath)
choosePackageFile window mbDir = chooseFile window "Select cabal package file (.cabal)" mbDir

packageEdit :: PackageAction
packageEdit = do
    idePackage <- ask
    let dirName = dropFileName (ipdCabalFile idePackage)
    modules <- liftIO $ allModules dirName
    package <- liftIO $ readPackageDescription normal (ipdCabalFile idePackage)
    if hasConfigs package
        then do
            lift $ ideMessage High ("Cabal file with configurations can't be edited with the "
                ++ "current version of the editor")
            lift $ fileOpenThis $ ipdCabalFile idePackage
            return ()
        else do
            let flat = flattenPackageDescription package
#if MIN_VERSION_Cabal(1,10,0)
            if hasUnknownTestTypes flat
                then do
                    lift $ ideMessage High ("Cabal file with tests of this type can't be edited with the "
                        ++ "current version of the editor")
                    lift $ fileOpenThis $ ipdCabalFile idePackage
                    return ()
                else do
                    lift $ editPackage flat dirName  modules (\ _ -> return ())
                    return ()
#else
            lift $ editPackage flat dirName  modules (\ _ -> return ())
            return ()
#endif

hasConfigs :: GenericPackageDescription -> Bool
hasConfigs gpd =
    let libConds = case condLibrary gpd of
                        Nothing -> False
                        Just condTree -> not (null (condTreeComponents condTree))
        exeConds = foldr (\ (_,condTree) hasConfigs ->
                                if hasConfigs
                                    then True
                                    else not (null (condTreeComponents condTree)))
                        False (condExecutables gpd)
#if MIN_VERSION_Cabal(1,10,0)
        testConds = foldr (\ (_,condTree) hasConfigs ->
                                if hasConfigs
                                    then True
                                    else not (null (condTreeComponents condTree)))
                        False (condTestSuites gpd)
    in libConds || exeConds || testConds
#else
    in libConds || exeConds
#endif

#if MIN_VERSION_Cabal(1,10,0)
hasUnknownTestTypes :: PackageDescription -> Bool
hasUnknownTestTypes pd =
    not . null . filter unknown $ testSuites pd
  where
    unknown (TestSuite _ (TestSuiteExeV10 _ _) _ _) = False
    unknown _ = True
#endif

packageNew' :: Maybe FilePath -> (Bool -> FilePath -> IDEAction) -> IDEAction
packageNew' mbDir activateAction = do
    windows  <- getWindows
    mbDirName <- liftIO $ choosePackageDir (head windows) mbDir
    case mbDirName of
        Nothing -> return ()
        Just dirName -> do
            mbCabalFile <-  liftIO $ cabalFileName dirName
            window <- getMainWindow
            case mbCabalFile of
                Just cfn -> do
                    add <- liftIO $ do
                        md <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel
                            $ "There is already file " ++ takeFileName cfn ++ " in this directory. "
                            ++ "Would you like to add this package to the workspace?"
                        dialogAddButton md "_Add Package" (ResponseUser 1)
                        dialogSetDefaultResponse md (ResponseUser 1)
                        set md [ windowWindowPosition := WinPosCenterOnParent ]
                        rid <- dialogRun md
                        widgetDestroy md
                        return $ rid == ResponseUser 1
                    when add $ activateAction False cfn
                Nothing -> do
                    isEmptyDir <- liftIO $ isEmptyDirectory dirName
                    make <- if isEmptyDir
                        then return True
                        else liftIO $ do
                            md <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel
                                $ "The path you have choosen " ++ dirName ++ " is not an empty directory. "
                                ++ "Are you sure you want to make a new package here?"
                            dialogAddButton md "_Make Package Here" (ResponseUser 1)
                            dialogSetDefaultResponse md (ResponseUser 1)
                            set md [ windowWindowPosition := WinPosCenterOnParent ]
                            rid <- dialogRun md
                            widgetDestroy md
                            return $ rid == ResponseUser 1
                    when make $ do
                        modules <- liftIO $ do
                            b1 <- doesFileExist (dirName </> "Setup.hs")
                            b2 <- doesFileExist (dirName </> "Setup.lhs")
                            if  not (b1 || b2)
                                then do
                                    sysMessage Normal "Setup.(l)hs does not exist. Writing Standard"
                                    writeFile (dirName </> "Setup.lhs") standardSetup
                                else sysMessage Normal "Setup.(l)hs already exist"
                            allModules dirName
                        let Just initialVersion = simpleParse "0.0.1"
                        editPackage emptyPackageDescription{
                            package   = PackageIdentifier (PackageName $ takeBaseName dirName)
                                                          initialVersion,
                            buildType = Just Simple,
#if MIN_VERSION_Cabal(1,10,0)
                            specVersionRaw = Right (orLaterVersion (Version [1,2] [])),
#endif
                            buildDepends = [
                                Dependency (PackageName "base") anyVersion
                              , Dependency (PackageName "QuickCheck") anyVersion],
                            executables = [emptyExecutable {
                                exeName    = (takeBaseName dirName)
                              , modulePath = "Main.hs"
                              , buildInfo  = emptyBuildInfo {
                                    hsSourceDirs = ["src"]}}]
#if MIN_VERSION_Cabal(1,10,0)
                              , testSuites = [emptyTestSuite {
                                    testName = "test-" ++ takeBaseName dirName
                                  , testInterface = (TestSuiteExeV10 (Version [1,0] []) "Main.hs")
                                  , testBuildInfo = emptyBuildInfo {
                                        hsSourceDirs = ["src"]
                                      , cppOptions = ["-DMAIN_FUNCTION=testMain"]}}]
#endif
                            } dirName modules (activateAction True)
                    return ()

standardSetup = "#!/usr/bin/runhaskell \n"
                    ++ "> module Main where\n"
                    ++ "> import Distribution.Simple\n"
                    ++ "> main :: IO ()\n"
                    ++ "> main = defaultMain\n\n"

--  ---------------------------------------------------------------------
--  | We do some twist for handling build infos seperately to edit them in one editor together
--  with the other stuff. This type show what we really edit here
--

data PackageDescriptionEd = PDE {
    pd           :: PackageDescription,
    exes         :: [Executable'],
#if MIN_VERSION_Cabal(1,10,0)
    tests        :: [Test'],
#endif
    mbLib        :: Maybe Library',
    bis          :: [BuildInfo]}
        deriving Eq

comparePDE a b = do
    when (pd a /= pd b) $ putStrLn  "pd"
    when (exes a /= exes b) $ putStrLn  "exes"
#if MIN_VERSION_Cabal(1,10,0)
    when (tests a /= tests b) $ putStrLn  "tests"
#endif
    when (mbLib a /= mbLib b) $ putStrLn  "mbLib"
    when (bis a /= bis b) $ putStrLn  "bis"

fromEditor :: PackageDescriptionEd -> PackageDescription
fromEditor (PDE pd exes'
#if MIN_VERSION_Cabal(1,10,0)
        tests'
#endif
        mbLib' buildInfos) =
    let     exes = map (\ (Executable' s fb bii) -> if bii + 1 > length buildInfos
                                        then Executable s fb (buildInfos !! (length buildInfos - 1))
                                        else Executable s fb (buildInfos !! bii)) exes'
#if MIN_VERSION_Cabal(1,10,0)
            tests = map (\ (Test' s fb bii) -> if bii + 1 > length buildInfos
                                        then TestSuite s fb (buildInfos !! (length buildInfos - 1)) False
                                        else TestSuite s fb (buildInfos !! bii) False) tests'
#endif
            mbLib = case mbLib' of
                    Nothing -> Nothing
                    Just (Library' mn b bii) -> if bii + 1 > length buildInfos
                                        then Just (Library mn b (buildInfos !! (length buildInfos - 1)))
                                        else Just (Library mn b (buildInfos !! bii))
    in pd {
        library = mbLib
      , executables = exes
#if MIN_VERSION_Cabal(1,10,0)
      , testSuites = tests
#endif
      }

toEditor :: PackageDescription -> PackageDescriptionEd
toEditor pd =
    let     (exes,exeBis) = unzip $ map (\((Executable s fb bi), i) -> ((Executable' s fb i), bi))
                            (zip (executables pd) [0..])
#if MIN_VERSION_Cabal(1,10,0)
            (tests,testBis) = unzip $ map (\((TestSuite s fb bi _), i) -> ((Test' s fb i), bi))
                            (zip (testSuites pd) [length exeBis..])
            bis = exeBis++testBis
#else
            bis = exeBis
#endif
            (mbLib,bis2) = case library pd of
                    Nothing                -> (Nothing,bis)
                    Just (Library mn b bi) -> (Just (Library' (sort mn) b (length bis)), bis ++ [bi])
            bis3 = if null bis2
                        then [emptyBuildInfo]
                        else bis2
    in PDE (pd {library = Nothing , executables = []})
        exes
#if MIN_VERSION_Cabal(1,10,0)
        tests
#endif
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
    primPaneName _  =   "Package"
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
            packageInfos <- liftIO $ getInstalledPackageIds
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
    packageInfos <- liftIO $ getInstalledPackageIds
    mbP <- buildThisPane panePath nb
        (builder' packageDir packageD packageDescr afterSaveAction
            initialPackagePath modules packageInfos fields origPackageD)
    case mbP of
        Nothing -> return ()
        Just (PackagePane{packageNotifer = pn}) -> do
            liftIO $ triggerEvent pn (GUIEvent {
                    selector = MayHaveChanged,
                    gtkEvent = GTK.Event True,
                    eventText = "",
                    gtkReturn = True})
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
    addB    <- buttonNewFromStock "Add Build Info"
    removeB <- buttonNewFromStock "Remove Build Info"
    label   <-  labelNew Nothing
    boxPackStart bb addB PackNatural 0
    boxPackStart bb removeB PackNatural 0
    boxPackEnd bb closeB PackNatural 0
    boxPackEnd bb save PackNatural 0
    (widget, setInj, getExt, notifier)  <-  buildEditor packageDescr packageD
    let packagePane = PackagePane vb notifier
    boxPackStart vb widget PackGrow 7
    boxPackStart vb label PackNatural 0
    boxPackEnd vb bb PackNatural 7

    let fieldNames = map (\fd -> case getParameterPrim paraName (parameters fd) of
                                    Just s -> s
                                    Nothing -> "Unnamed") fields
    addB `onClicked` (do
        mbNewPackage' <- extract packageD [getExt]
        case mbNewPackage' of
            Nothing -> sysMessage Normal "Content doesn't validate"
            Just pde -> reflectIDE (do
                    closePane packagePane
                    initPackage packageDir pde {bis = bis pde ++ [bis pde !! 0]}
                        (packageDD
                            (packageInfos)
                            packageDir
                            modules
                            (length (bis pde) + 1)
                            (concatMap (buildInfoD (Just packageDir) modules)
                                [0..length (bis pde)]))
                        panePath nb modules afterSaveAction origPackageD) ideR)
    removeB `onClicked` (do
        mbNewPackage' <- extract packageD [getExt]
        case mbNewPackage' of
            Nothing -> sysMessage Normal "Content doesn't validate"
            Just pde | length (bis pde) == 1  -> sysMessage Normal "Just one Build Info"
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
                            panePath nb modules afterSaveAction origPackageD) ideR)
    closeB `onClicked` (do
        mbP <- extract packageD [getExt]
        let hasChanged = case mbP of
                                Nothing -> False
                                Just p -> p /= origPackageD
        if not hasChanged
            then reflectIDE (closePane packagePane >> return ()) ideR
            else do
                md <- messageDialogNew (Just window) []
                    MessageQuestion
                    ButtonsYesNo
                    "Unsaved changes. Close anyway?"
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetDestroy md
                case resp of
                    ResponseYes ->   do
                        reflectIDE (closePane packagePane >> return ()) ideR
                    _  ->   return ())
    save `onClicked` (do
        mbNewPackage' <- extract packageD [getExt]
        case mbNewPackage' of
            Nothing -> return ()
            Just newPackage' -> let newPackage = fromEditor newPackage' in do
                let packagePath = packageDir </> (display . pkgName . package . pd) newPackage'
                                                ++ ".cabal"
#if MIN_VERSION_Cabal(1,10,0)
                writeGenericPackageDescription packagePath (toGenericPackageDescription newPackage)
#else
                writePackageDescription packagePath newPackage
#endif
                reflectIDE (do
                    afterSaveAction packagePath
                    closePane packagePane
                    return ()) ideR)
    registerEvent notifier MayHaveChanged (\ e -> do
        mbP <- extract packageD [getExt]
        let hasChanged = case mbP of
                                Nothing -> False
                                Just p -> p /= origPackageD
        when (isJust mbP) $ labelSetMarkup label ""
        when (isJust mbP) $ comparePDE (fromJust mbP) packageD
        markLabel nb (getTopWidget packagePane) hasChanged
        widgetSetSensitive save hasChanged
        return (e{gtkReturn=False}))
    registerEvent notifier ValidationError (\e -> do
        labelSetMarkup label $ "<span foreground=\"red\" size=\"x-large\">The following fields have invalid values: "
            ++ eventText e ++ "</span>"
        return e)
    return (Just packagePane,[])

-- ---------------------------------------------------------------------
-- The description with some tricks
--

packageDD :: [PackageIdentifier]
    -> FilePath
    -> [ModuleName]
    -> Int
    -> [(String, FieldDescription PackageDescriptionEd)]
    -> FieldDescription PackageDescriptionEd
packageDD packages fp modules numBuildInfos extras = NFD ([
    ("Package", VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Synopsis"
           $ paraSynopsis <<<- ParaSynopsis "A one-line summary of this package"
           $ emptyParams)
            (synopsis . pd)
            (\ a b -> b{pd = (pd b){synopsis = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName "Package Identifier" $ emptyParams)
            (package . pd)
            (\ a b -> b{pd = (pd b){package = a}})
            packageEditor
    ,   mkField
            (paraName <<<- ParaName "Description"
                $ paraSynopsis <<<- ParaSynopsis "A more verbose description of this package"
                    $ paraShadow <<<- ParaShadow ShadowOut
                        $ paraMinSize <<<- ParaMinSize (-1,210)
                            $ emptyParams)
            (description . pd)
            (\ a b -> b{pd = (pd b){description = if null a then " \n\n\n\n\n" else a}})
            multilineStringEditor
    ,   mkField
            (paraName <<<- ParaName "Homepage" $ emptyParams)
            (homepage . pd)
            (\ a b -> b{pd = (pd b){homepage = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName "Package URL" $ emptyParams)
            (pkgUrl . pd)
            (\ a b -> b{pd = (pd b){pkgUrl = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName "Category" $ emptyParams)
            (category . pd)
            (\ a b -> b{pd = (pd b){category = a}})
            (stringEditor (const True) True)
    ]),
    ("Description", VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Stability" $ emptyParams)
            (stability . pd)
            (\ a b -> b{pd = (pd b){stability = a}})
            (stringEditor (const True) True)
#if MIN_VERSION_Cabal(1,8,0)
            -- TODO
#else
    ,   mkField
            (paraName <<<- ParaName "License" $ emptyParams)
            (license . pd)
            (\ a b -> b{pd = (pd b){license = a}})
            (comboSelectionEditor [GPL, LGPL, BSD3, BSD4, PublicDomain, AllRightsReserved, OtherLicense] show)
#endif
    ,   mkField
            (paraName <<<- ParaName "License File" $ emptyParams)
            (licenseFile . pd)
            (\ a b -> b{pd = (pd b){licenseFile = a}})
            (fileEditor (Just fp) FileChooserActionOpen "Select file")
    ,   mkField
            (paraName <<<- ParaName "Copyright" $ emptyParams)
            (copyright . pd)
            (\ a b -> b{pd = (pd b){copyright = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName "Author" $ emptyParams)
            (author . pd)
            (\ a b -> b{pd = (pd b){author = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName "Maintainer" $ emptyParams)
            (maintainer . pd)
            (\ a b -> b{pd = (pd b){maintainer = a}})
            (stringEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName "Bug Reports" $ emptyParams)
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
    ("Dependencies  ", VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Build Dependencies"
                $ paraSynopsis <<<- ParaSynopsis "Does this package depends on other packages?"
                    $ paraDirection <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,250)
                            $ emptyParams)
            (nub . buildDepends . pd)
            (\ a b -> b{pd = (pd b){buildDepends = a}})
            (dependenciesEditor packages)
    ]),
    ("Meta Dep.", VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Cabal version"
                $ paraSynopsis <<<- ParaSynopsis
                    "Does this package depends on a specific version of Cabal?"
                    $ paraShadow <<<- ParaShadow ShadowIn $ emptyParams)
            (descCabalVersion . pd)
#if MIN_VERSION_Cabal(1,10,0)
            (\ a b -> b{pd = (pd b){specVersionRaw = Right a}})
#else
            (\ a b -> b{pd = (pd b){descCabalVersion = a}})
#endif
            versionRangeEditor
    ,   mkField
            (paraName <<<- ParaName "Tested with compiler"
                $ paraShadow <<<- ParaShadow ShadowIn
                    $ paraDirection <<<- ParaDirection Vertical
                        $ emptyParams)
            (\a -> case (testedWith . pd) a of
                []          -> []--(GHC,anyVersion)]
                l           -> l)
            (\ a b -> b{pd = (pd b){testedWith = a}})
            testedWithEditor
    ]),
    ("Data Files", VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Data Files"
                $ paraSynopsis <<<- ParaSynopsis
                    "A list of files to be installed for run-time use by the package."
                    $ paraDirection <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,250)
                            $ emptyParams)
            (dataFiles . pd)
            (\ a b -> b{pd = (pd b){dataFiles = a}})
            (filesEditor (Just fp) FileChooserActionOpen "Select File")
    ,   mkField
            (paraName <<<- ParaName "Data directory" $ emptyParams)
            (dataDir . pd)
            (\ a b -> b{pd = (pd b){dataDir = a}})
            (fileEditor (Just fp) FileChooserActionSelectFolder "Select file")
    ]),
    ("Extra Files", VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Extra Source Files"
                $ paraSynopsis <<<- ParaSynopsis
                    "A list of additional files to be included in source distributions."
                    $ paraDirection <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,120)
                            $ emptyParams)
            (extraSrcFiles . pd)
            (\ a b -> b{pd = (pd b){extraSrcFiles = a}})
            (filesEditor (Just fp) FileChooserActionOpen "Select File")
    ,   mkField
            (paraName <<<-  ParaName "Extra Tmp Files"
                $ paraSynopsis <<<- ParaSynopsis
                    "A list of additional files or directories to be removed by setup clean."
                    $ paraDirection <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,120)
                            $ emptyParams)
            (extraTmpFiles . pd)
            (\ a b -> b{pd = (pd b){extraTmpFiles = a}})
            (filesEditor (Just fp) FileChooserActionOpen "Select File")
    ]),
    ("Other",VFD emptyParams  [
        mkField
            (paraName <<<- ParaName "Build Type"
                $ paraSynopsis <<<- ParaSynopsis
                "Describe executable programs contained in the package"
                        $ paraShadow <<<- ParaShadow ShadowIn
                            $ paraDirection <<<- ParaDirection Vertical
                                $ emptyParams)
            (buildType . pd)
            (\ a b -> b{pd = (pd b){buildType = a}})
            (maybeEditor (buildTypeEditor, emptyParams) True "Specify?")
    ,   mkField
            (paraName <<<- ParaName "Custom Fields"
                $ paraShadow <<<- ParaShadow ShadowIn
                     $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (customFieldsPD . pd)
            (\ a b -> b{pd = (pd b){customFieldsPD = a}})
            (multisetEditor
                (ColumnDescr True [("Name",\(n,_) -> [cellText := n])
                                   ,("Value",\(_,v) -> [cellText := v])])
                ((pairEditor
                    (stringxEditor (const True),emptyParams)
                    (stringEditor (const True) True,emptyParams)),emptyParams)
            Nothing
            Nothing)
    ]),
    ("Executables",VFD emptyParams  [
        mkField
            (paraName <<<- ParaName "Executables"
                $ paraSynopsis <<<- ParaSynopsis
                "Describe executable programs contained in the package"
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            exes
            (\ a b -> b{exes = a})
            (executablesEditor (Just fp) modules numBuildInfos)
    ]),
#if MIN_VERSION_Cabal(1,10,0)
    ("Tests",VFD emptyParams  [
        mkField
            (paraName <<<- ParaName "Tests"
                $ paraSynopsis <<<- ParaSynopsis
                "Describe tests contained in the package"
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            tests
            (\ a b -> b{tests = a})
            (testsEditor (Just fp) modules numBuildInfos)
    ]),
#endif
    ("Library", VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Library"
           $ paraSynopsis <<<- ParaSynopsis
             "If the package contains a library, specify the exported modules here"
           $ paraDirection <<<- ParaDirection Vertical
           $ paraShadow <<<- ParaShadow ShadowIn $ emptyParams)
            mbLib
            (\ a b -> b{mbLib = a})
            (maybeEditor (libraryEditor (Just fp) modules numBuildInfos,
                paraName <<<- ParaName "Specify exported modules"
                $ emptyParams) True
                "Does this package contain a library?")
    ])
    ] ++ extras)

update :: [BuildInfo] -> Int -> (BuildInfo -> BuildInfo)  -> [BuildInfo]
update bis index func =
    map (\(bi,ind) -> if ind == index
                        then func bi
                        else bi)
        (zip bis [0..length bis - 1])

buildInfoD :: Maybe FilePath -> [ModuleName] -> Int -> [(String,FieldDescription PackageDescriptionEd)]
buildInfoD fp modules i = [
    (show (i + 1) ++ " Build Info", VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Component is buildable here" $ emptyParams)
            (buildable . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{buildable = a})})
            boolEditor
    ,   mkField
            (paraName  <<<- ParaName
                "Where to look for the source hierarchy"
                $ paraSynopsis <<<- ParaSynopsis
                    "Root directories for the source hierarchy."
                    $ paraShadow  <<<- ParaShadow ShadowIn
                        $ paraDirection  <<<- ParaDirection Vertical
                            $ emptyParams)
            (hsSourceDirs . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{hsSourceDirs = a})})
            (filesEditor fp FileChooserActionSelectFolder "Select folder")
    ,   mkField
            (paraName <<<- ParaName "Non-exposed or non-main modules"
            $ paraSynopsis <<<- ParaSynopsis ("A list of modules used by the component but "
                                             ++ "not exposed to users.")
                $ paraShadow <<<- ParaShadow ShadowIn
                    $ paraDirection <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,300)
                            $ paraPack <<<- ParaPack PackGrow
                                $ emptyParams)
            (map display. otherModules . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi ->
                bi{otherModules = (map (\i -> forceJust (simpleParse i)
                "   PackageEditor >> buildInfoD: no parse for moduile name" ) a)})})
            (modulesEditor modules)
    ]),
    (show (i + 1) ++ " Compiler ", VFD emptyParams [
        mkField
            (paraName  <<<- ParaName "Options for haskell compilers"
            $ paraDirection <<<- ParaDirection Vertical
            $ paraShadow <<<- ParaShadow ShadowIn
            $ paraPack <<<- ParaPack PackGrow $ emptyParams)
            (options . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{options = a})})
            (multisetEditor
                (ColumnDescr True [("Compiler Flavor",\(cv,_) -> [cellText := show cv])
                                   ,("Options",\(_,op) -> [cellText := concatMap (\s -> ' ' : s) op])])
                ((pairEditor
                    (compilerFlavorEditor,emptyParams)
                    (optsEditor,emptyParams)),
                        (paraDirection <<<- ParaDirection Vertical
                            $ paraShadow  <<<- ParaShadow ShadowIn $ emptyParams))
                Nothing
                Nothing)
     ,  mkField
            (paraName <<<- ParaName "Additional options for GHC when built with profiling"
           $ emptyParams)
            (ghcProfOptions . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{ghcProfOptions = a})})
            optsEditor
     ,  mkField
            (paraName <<<- ParaName "Additional options for GHC when the package is built as shared library"
           $ emptyParams)
            (ghcSharedOptions . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{ghcSharedOptions = a})})
            optsEditor
    ]),
    (show (i + 1) ++ " Extensions ", VFD emptyParams [
        mkField
            (paraName  <<<- ParaName "Extensions"
                $ paraSynopsis  <<<- ParaSynopsis
                    "A list of Haskell extensions used by every module."
                         $ paraMinSize <<<- ParaMinSize (-1,400)
                            $ paraPack <<<- ParaPack PackGrow
                                $ emptyParams)
#if MIN_VERSION_Cabal(1,10,0)
            (oldExtensions . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{oldExtensions = a})})
#else
            (extensions . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{extensions = a})})
#endif
            extensionsEditor
    ]),
    (show (i + 1) ++ " Build Tools ", VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Tools needed for a build"
                $ paraDirection <<<- ParaDirection Vertical
                    $ paraMinSize <<<- ParaMinSize (-1,120)
                        $ emptyParams)
            (buildTools . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{buildTools = a})})
            (dependenciesEditor [])
    ]),
    (show (i + 1) ++ " Pkg Config ", VFD emptyParams [
        mkField
            (paraName <<<- ParaName "A list of pkg-config packages, needed to build this package"
                $ paraDirection <<<- ParaDirection Vertical
                    $ paraMinSize <<<- ParaMinSize (-1,120)
                        $ emptyParams)
            (pkgconfigDepends . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{pkgconfigDepends = a})})
            (dependenciesEditor [])
    ]),
    (show (i + 1) ++ " Opts C -1-", VFD emptyParams [
         mkField
            (paraName <<<- ParaName "Options for C compiler"
                $ paraDirection <<<- ParaDirection Vertical
                    $ emptyParams)
            (ccOptions . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{ccOptions = a})})
            optsEditor
    ,    mkField
            (paraName <<<- ParaName "Options for linker"
                $ paraDirection <<<- ParaDirection Vertical
                    $ emptyParams)
            (ldOptions . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{ldOptions = a})})
            optsEditor
    ,    mkField
            (paraName <<<- ParaName "A list of header files to use when compiling"
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (includes . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{includes = a})})
            (stringsEditor (const True) True)
     ,   mkField
            (paraName <<<- ParaName "A list of header files to install"
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (installIncludes . (\a -> a !! i) . bis)
             (\ a b -> b{bis = update (bis b) i (\bi -> bi{installIncludes = a})})
           (filesEditor fp FileChooserActionOpen "Select File")
    ]),
    (show (i + 1) ++ " Opts C -2-", VFD emptyParams [
         mkField
            (paraName <<<- ParaName "A list of directories to search for header files"
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (includeDirs . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{includeDirs = a})})
            (filesEditor fp FileChooserActionSelectFolder "Select Folder")
     ,   mkField
            (paraName <<<- ParaName
                "A list of C source files to be compiled,linked with the Haskell files."
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (cSources . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{cSources = a})})
            (filesEditor fp FileChooserActionOpen "Select file")
    ]),
    (show (i + 1) ++ " Opts Libs ", VFD emptyParams [
         mkField
            (paraName <<<- ParaName "A list of extra libraries to link with"
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (extraLibs . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{extraLibs = a})})
            (stringsEditor (const True) True)
     ,   mkField
            (paraName <<<- ParaName "A list of directories to search for libraries."
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (extraLibDirs . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{extraLibDirs = a})})
            (filesEditor fp FileChooserActionSelectFolder "Select Folder")
   ]),
    (show (i + 1) ++ " Other", VFD emptyParams [
         mkField
            (paraName <<<- ParaName "Options for C preprocessor"
                $ paraDirection <<<- ParaDirection Vertical
                    $ emptyParams)
            (cppOptions . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{cppOptions = a})})
            optsEditor
    ,   mkField
            (paraName <<<- ParaName "Support frameworks for Mac OS X"
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (frameworks . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{frameworks = a})})
            (stringsEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName "Custom fields build info"
                $ paraShadow <<<- ParaShadow ShadowIn
                     $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
             (customFieldsBI . (\a -> a !! i) . bis)
            (\ a b -> b{bis = update (bis b) i (\bi -> bi{customFieldsBI = a})})
            (multisetEditor
                (ColumnDescr True [("Name",\(n,_) -> [cellText := n])
                                   ,("Value",\(_,v) -> [cellText := v])])
                ((pairEditor
                    (stringxEditor (const True),emptyParams)
                    (stringEditor (const True) True,emptyParams)),emptyParams)
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

packageEditor :: Editor PackageIdentifier
packageEditor para noti = do
    (wid,inj,ext) <- pairEditor
        (stringEditor (\s -> not (null s)) True, paraName <<<- ParaName "Name" $ emptyParams)
        (versionEditor, paraName <<<- ParaName "Version" $ emptyParams)
        (paraDirection <<<- ParaDirection Horizontal
            $ paraShadow <<<- ParaShadow ShadowIn
                $ para) noti
    let pinj (PackageIdentifier (PackageName n) v) = inj (n,v)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (n,v) -> do
                if null n
                    then return Nothing
                    else return (Just $PackageIdentifier (PackageName n) v)
    return (wid,pinj,pext)

testedWithEditor :: Editor [(CompilerFlavor, VersionRange)]
testedWithEditor para = do
    multisetEditor
       (ColumnDescr True [("Compiler Flavor",\(cv,_) -> [cellText := show cv])
                           ,("Version Range",\(_,vr) -> [cellText := display vr])])
       (pairEditor
            (compilerFlavorEditor, paraShadow <<<- (ParaShadow ShadowNone) $ emptyParams)
            (versionRangeEditor, paraShadow <<<- (ParaShadow ShadowNone) $ emptyParams),
            (paraDirection <<<- (ParaDirection Vertical) $ emptyParams))
       Nothing
       (Just (==))
       para

compilerFlavorEditor :: Editor CompilerFlavor
compilerFlavorEditor para noti = do
    (wid,inj,ext) <- eitherOrEditor
        (comboSelectionEditor flavors show, paraName <<<- (ParaName "Select compiler") $ emptyParams)
        (stringEditor (\s -> not (null s)) True, paraName <<<- (ParaName "Specify compiler") $ emptyParams)
        "Other"
        (paraName <<<- ParaName "Select" $ para)
        noti
    let cfinj comp  = case comp of
                        (OtherCompiler str) -> inj (Right str)
                        other               -> inj (Left other)
    let cfext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (Right s) -> return (Just $OtherCompiler s)
            Just (Left other) -> return (Just other)
    return (wid,cfinj,cfext)
        where
        flavors = [GHC, NHC, Hugs, HBC, Helium, JHC]

buildTypeEditor :: Editor BuildType
buildTypeEditor para noti = do
    (wid,inj,ext) <- eitherOrEditor
        (comboSelectionEditor flavors show, paraName <<<- (ParaName "Select") $ emptyParams)
        (stringEditor (const True) True, paraName <<<- (ParaName "Unknown") $ emptyParams)
        "Unknown"
        (paraName <<<- ParaName "Select" $ para)
        noti
    let cfinj comp  = case comp of
                        (UnknownBuildType str) -> inj (Right str)
                        other               -> inj (Left other)
    let cfext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (Right s) -> return (Just $ UnknownBuildType s)
            Just (Left other) -> return (Just other)
    return (wid,cfinj,cfext)
        where
        flavors = [Simple, Configure, Make, Custom]

extensionsEditor :: Editor [Extension]
extensionsEditor = staticListMultiEditor extensionsL show


extensionsL :: [Extension]
#if MIN_VERSION_Cabal(1,11,0)
extensionsL = map EnableExtension [minBound..maxBound]
#else
extensionsL = knownExtensions
#endif

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
                            (maybeEditor (stringEditor (const True) True,noBorder) True "Specify a location", emptyParams)
                            (maybeEditor (stringEditor (const True) True,noBorder) True "Specify a module", emptyParams)
                            (maybeEditor (stringEditor (const True) True,noBorder) True "Specify a branch", emptyParams)
                            (maybeEditor (stringEditor (const True) True,noBorder) True "Specify a tag", emptyParams)
                            (maybeEditor (stringEditor (const True) True,noBorder) True "Specify a subdir", emptyParams)
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
                        (stringEditor (const True) True,emptyParams)
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
                        (stringEditor (const True) True,emptyParams)
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
    exposedModules' :: [ModuleName]
,   libExposed'     :: Bool
,   libBuildInfoIdx :: Int}
    deriving (Show, Eq)

data Executable' = Executable'{
    exeName'        :: String
,   modulePath'     :: FilePath
,   buildInfoIdx    :: Int}
    deriving (Show, Eq)

#if MIN_VERSION_Cabal(1,10,0)
data Test' = Test'{
    testName'        :: String
,   testInterface'   :: TestSuiteInterface
,   testBuildInfoIdx :: Int}
    deriving (Show, Eq)
#endif

instance Default Library'
    where getDefault =  Library' [] getDefault getDefault

instance Default Executable'
    where getDefault = Executable' getDefault getDefault getDefault

#if MIN_VERSION_Cabal(1,10,0)
instance Default Test'
    where getDefault = Test' getDefault (TestSuiteExeV10 (Version [1,0] []) getDefault) getDefault
#endif

libraryEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Library'
libraryEditor fp modules numBuildInfos para noti = do
    (wid,inj,ext) <-
        tupel3Editor
            (boolEditor,
            paraName <<<- ParaName "Exposed"
            $ paraSynopsis <<<- ParaSynopsis "Is the lib to be exposed by default?"
            $ emptyParams)
            (modulesEditor (sort modules),
            paraName <<<- ParaName "Exposed Modules"
            $ paraMinSize <<<- ParaMinSize (-1,300)
            $ para)
            (buildInfoEditorP numBuildInfos, paraName <<<- ParaName "Build Info"
            $ paraPack <<<- ParaPack PackNatural
            $ para)
            (paraDirection <<<- ParaDirection Vertical
            $ emptyParams)
            noti
    let pinj (Library' em exp bi) = inj (exp, map display em,bi)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (exp,em,bi) -> return (Just $ Library' (map (\s -> forceJust (simpleParse s)
                "SpecialEditor >> libraryEditor: no parse for moduile name") em) exp bi)
    return (wid,pinj,pext)

--moduleEditor :: [ModuleName] -> Editor String
--moduleEditor modules    =   comboSelectionEditor (map display modules)

modulesEditor :: [ModuleName] -> Editor [String]
modulesEditor modules   =   staticListMultiEditor (map display modules) id

executablesEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor [Executable']
executablesEditor fp modules countBuildInfo p =
    multisetEditor
        (ColumnDescr True [("Executable Name",\(Executable' exeName _ _) -> [cellText := exeName])
                           ,("Module Path",\(Executable'  _ mp _) -> [cellText := mp])

                           ,("Build info index",\(Executable'  _ _ bii) -> [cellText := show (bii + 1)])])
        (executableEditor fp modules countBuildInfo,emptyParams)
        Nothing
        Nothing
        (paraShadow  <<<- ParaShadow ShadowIn $ p)

executableEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Executable'
executableEditor fp modules countBuildInfo para noti = do
    (wid,inj,ext) <- tupel3Editor
        (stringEditor (\s -> not (null s)) True,
            paraName <<<- ParaName "Executable Name"
            $ emptyParams)
        (stringEditor (\s -> not (null s)) True,
            paraDirection <<<- ParaDirection Vertical
            $ paraName <<<- ParaName "File with main function"
            $ emptyParams)
        (buildInfoEditorP countBuildInfo, paraName <<<- ParaName "Build Info"
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

#if MIN_VERSION_Cabal(1,10,0)
testsEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor [Test']
testsEditor fp modules countBuildInfo p =
    multisetEditor
        (ColumnDescr True [("Test Name",\(Test' testName _ _) -> [cellText := testName])
                           ,("Interface",\(Test'  _ i _) -> [cellText := interfaceName i])

                           ,("Build info index",\(Test'  _ _ bii) -> [cellText := show (bii + 1)])])
        (testEditor fp modules countBuildInfo,emptyParams)
        Nothing
        Nothing
        (paraShadow  <<<- ParaShadow ShadowIn $ p)
  where
    interfaceName (TestSuiteExeV10 _ f) = f
    interfaceName i = show i

testEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Test'
testEditor fp modules countBuildInfo para noti = do
    (wid,inj,ext) <- tupel3Editor
        (stringEditor (\s -> not (null s)) True,
            paraName <<<- ParaName "Test Name"
            $ emptyParams)
        (stringEditor (\s -> not (null s)) True,
            paraDirection <<<- ParaDirection Vertical
            $ paraName <<<- ParaName "File with main function"
            $ emptyParams)
        (buildInfoEditorP countBuildInfo, paraName <<<- ParaName "Build Info"
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
#endif

buildInfoEditorP :: Int -> Editor Int
buildInfoEditorP numberOfBuildInfos para noti = do
    (wid,inj,ext) <- intEditor (1.0,fromIntegral numberOfBuildInfos,1.0)
        (paraName <<<- ParaName "Build Info" $para) noti
    let pinj i = inj (i + 1)
    let pext =   do
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
    where getDefault =  Library [] getDefault getDefault

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


