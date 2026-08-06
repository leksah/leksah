-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Building, testing and running projects.
--
-- A fresh, flat build engine: a build runs its steps for one target
-- sequentially (build → docs? → tests? → benchmarks?), stopping at the first
-- failure.  This replaces the old @IDE.Build@ make-chain machinery, which
-- resolved a SINGLE-package project to an empty chain and so silently did
-- nothing.
--
-- The per-tool command assembly (cabal\/ffcabal\/stack\/cargo\/make) is
-- carried from the old @IDE.Package@ (recent work; see docs/relicensing.md),
-- with the build-target list fixed: only a package that really has a main
-- library contributes a @lib:\<pkg\>@ target.
module IDE.Project.Build
  ( -- * Entry points
    buildTarget
  , buildActiveTarget
  , runBackgroundBuild
  , buildCustomProject
  , packageClean
  , packageTest
  , packageBench
  , packageRun
  , packageRunJavaScript
    -- * Pieces the commands layer reuses
  , interruptAndRun
  , customBuildCommand
  ) where

import Control.Concurrent (putMVar, takeMVar)
import Control.Exception (SomeException)
import Control.Monad (forM, forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^.))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Conduit as C
import Data.Conduit (ConduitT, Void)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
       (doesDirectoryExist, doesFileExist, findExecutable,
        removeDirectoryRecursive)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>), equalFilePath, takeFileName)
import System.Log.Logger (debugM)

import Distribution.Compiler (CompilerFlavor(..))
import Distribution.PackageDescription
       (Executable(..), PackageDescription(..), exeName)
import Distribution.PackageDescription.Configuration
       (flattenPackageDescription)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)

import IDE.Core.State
import IDE.Core.Types (Log(..))
import IDE.Diagnostics
       (logOutput, logOutputForBuild, logOutputForCargoBuild)
import IDE.Gtk.State (delayedBy, postAsyncIDE, postSyncIDE)
import IDE.Pane.Log
       (addLogLaunchData, buildLogLaunchByName, getDefaultLogLaunch,
        showDefaultLogLaunch', showLog)
import IDE.Pane.SourceBuffer (removeTestLogRefs)
import IDE.Pane.WebKit.Output
       (loadOutputHtmlFile, loadOutputUri, showOutputPane)
import IDE.Project.Commands (normalVerbosity, packageFromGPD, readGPD)
import IDE.Project.Nix (projectFileArguments, withToolCommand)
import IDE.Project.Run (addFFCabalTmuxEnv)
import IDE.Utils.ExternalTool
       (interruptBuild, isRunning, runExternalTool, runExternalTool',
        sinkLast)
import IDE.Utils.Files (cabalBuildDir, cabalProjectBuildDir)
import IDE.Utils.Process (ProcessHandle, ToolOutput(..))
import IDE.Utils.Project
       (CustomProject(..), ProjectKey(..), pjCustomDir, pjCustomGhcBuild,
        pjIsCabal, pjIsStack)
import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.RestartRequest (requestRestart)


-- ---------------------------------------------------------------------
-- Entry points
-- ---------------------------------------------------------------------

-- | Build the active target: the active package if there is one, otherwise
-- the active project's own build command (a Rust crate added via \"Open
-- Folder\" builds with @cargo build@).
buildActiveTarget :: IDEAction
buildActiveTarget = readIDE activePack >>= \case
    Just _  -> withActiveTarget buildTarget
    -- No active package: build the active project's packages if it has any
    -- (activating a PROJECT row is a normal thing to do), else fall back to
    -- the project's own build command (a Rust crate, a Makefile, …).
    Nothing -> withActiveProject $ \project ->
        case rootPackage project of
            Nothing   -> buildCustomProject project
            Just pkg  -> buildTarget project pkg
  where
    -- The project's "own" package: the one rooted at the project directory
    -- (leksah's project also carries leksah-classic and its GTK stack, so
    -- building every package would be both slow and surprising).
    rootPackage project =
        let pkgs = pjPackages project
            root = pjDir (pjKey project)
        in case filter ((`equalFilePath` root) . ipdPackageDir) pkgs of
            (p:_) -> Just p
            []    -> listToMaybe pkgs

-- | Run an action on the active (project, package) pair.
withActiveTarget :: (Project -> Package -> IDEAction) -> IDEAction
withActiveTarget f = do
    mbProject <- readIDE activeProject
    mbPack <- readIDE activePack
    case (mbProject, mbPack) of
        (Just project, Just package) -> f project package
        (Just project, Nothing) -> case pjPackages project of
            (p:_) -> f project p
            []    -> ideMessage Normal (__ "No package in the active project")
        _ -> ideMessage Normal (__ "No active project")

withActiveProject :: (Project -> IDEAction) -> IDEAction
withActiveProject f = readIDE activeProject >>= \case
    Just project -> f project
    Nothing      -> ideMessage Normal (__ "No active project")

-- | The foreground Build command: save-and-interrupt, then run the build
-- steps for one package.  When the built package is leksah itself (in a
-- development checkout) a successful build asks the launcher to restart —
-- the self-hosting loop.
buildTarget :: Project -> Package -> IDEAction
buildTarget project package = interruptAndRun $ do
    liftIO $ debugM "leksah" "buildTarget"
    showDefaultLogLaunch'
    prefs' <- readIDE prefs
    runSteps False project [package] $ do
        -- Success: self-restart when we just rebuilt leksah itself.
        when (ipdPackageName package == "leksah" && native prefs') $
            readIDE developLeksah >>= \case
                False -> return ()
                True  -> liftIO requestRestart

-- | The background build: rebuild the packages whose files changed (the
-- watcher's external-modification set), quietly.
runBackgroundBuild :: IDEAction
runBackgroundBuild = (do
    extModsMVar <- readIDE externalModified
    extMods <- liftIO $ do
        x <- takeMVar extModsMVar
        putMVar extModsMVar mempty
        return (S.toList x)
    readIDE workspace >>= \case
        Nothing -> return ()
        Just ws -> do
            let affected =
                    [ (project, pkgs)
                    | project <- ws ^. wsProjects
                    , let pkgs = filter (\p -> any (`belongsToPackage` p) extMods)
                                        (pjPackages project)
                    , not (null pkgs) ]
            forM_ affected $ \(project, pkgs) ->
                runSteps True project pkgs (return ()))
  `catchIDE` (\(e :: SomeException) -> sysMessage Normal (T.pack $ show e))

-- | Run the configured build steps for one target, stopping at the first
-- failure; @onSuccess@ runs when they all pass.
runSteps :: Bool -> Project -> [Package] -> IDEAction -> IDEAction
runSteps background project packages onSuccess = do
    prefs' <- readIDE prefs
    let steps =
            [ buildStep ]
            <> [ docsStep  | makeDocs prefs' ]
            <> [ testStep  | runUnitTests prefs' ]
            <> [ benchStep | runBenchmarks prefs' ]
    chain steps
  where
    chain [] = onSuccess
    chain (step:rest) = step background project packages $ \ok -> when ok (chain rest)
    buildStep = buildPackages
    docsStep  = docsPackages
    testStep  = testPackages
    benchStep = benchPackages

-- ---------------------------------------------------------------------
-- The build step
-- ---------------------------------------------------------------------

-- | Compile the packages with each enabled compiler (GHC natively, and the
-- JavaScript backend when that preference is on for a cabal project).
buildPackages :: Bool -> Project -> [Package] -> (Bool -> IDEAction) -> IDEAction
buildPackages background project packages continuation = do
    prefs' <- readIDE prefs
    let jump = jumpToWarnings prefs'
        compilers = [GHC | native prefs' || (javaScript prefs' && pjIsCabal (pjKey project))]
    go jump compilers
  where
    go _ [] = continuation True
    go jump (compiler:rest) =
        runCabalBuild compiler background jump project packages $ \ok ->
            if ok
                then do
                    readIDE autoURI >>= \case
                        Just uri -> postSyncIDE . loadOutputUri $ T.unpack uri
                        Nothing  -> return ()
                    go jump rest
                else continuation False

-- | Assemble and run the build command for a project's packages.
runCabalBuild :: CompilerFlavor -> Bool -> Bool -> Project -> [Package]
              -> (Bool -> IDEAction) -> IDEAction
runCabalBuild _compiler background jump project packages continuation = do
    let dir = pjDir $ pjKey project
    activeComponent' <- catMaybes <$> mapM (activeComponentTarget project) packages
    pjFileArgs <- projectFileArguments project dir
    prefs'' <- readIDE prefs
    let wantTests = runUnitTests prefs''
        wantBench = runBenchmarks prefs''
    targets <- fmap concat . forM packages $ \package -> do
        -- Local packages re-read the .cabal so the target list sees edits
        -- made since the project loaded; a remote re-read would cost one ssh
        -- round trip per package per build, so those use the loaded names.
        comps <- if isRemotePath (ipdCabalFile package)
                    then return (pkgComponents package)
                    else fromMaybe (pkgComponents package) <$> freshComponents package
        let pkgName' = ipdPackageName package
            named k pre = [ pkgName' <> pre <> cName c | c <- comps, cKind c == k ]
        return $ case pjKey project of
            StackTool {} ->
                   (if wantTests && any ((== CkTest) . cKind) comps
                        then ["--test", "--no-run-tests"] else [])
                <> (if wantBench && any ((== CkBenchmark) . cKind) comps
                        then ["--bench", "--no-run-benchmarks"] else [])
            -- Only the components the user actually wants built: the
            -- libraries and executables always, tests/benchmarks when their
            -- preference is on (asking a tool to build a test component the
            -- project doesn't expose is an error, not a no-op).
            CabalTool {} ->
                   named CkLibrary    ":lib:"
                <> named CkSubLibrary ":lib:"
                <> named CkExecutable ":exe:"
                <> (if wantTests then named CkTest ":test:" else [])
                <> (if wantBench then named CkBenchmark ":benchmark:" else [])
            _ -> []
    -- Native GHC builds of cabal projects go through ffcabal WHEN GHCI MODE IS
    -- ON: it type-checks each local component in a cached tmux repl first — the
    -- first error surfaces in seconds — then builds in parallel.  Background
    -- builds use its --repl-only (checks only).  With ghci mode off the user
    -- has opted out of repls, so ALL builds use regular cabal.  Remote projects
    -- always take the plain-cabal arm (ffcabal's repls live in the LOCAL tmux).
    ghciMode <- debug <$> readIDE prefs
    mbFFCabal <- if ghciMode && not (isRemotePath dir)
                    then liftIO (findExecutable "ffcabal")
                    else return Nothing
    let builddir = ["--builddir=" <> T.pack (cabalBuildDir Nothing)]
        cabalCmd = case mbFFCabal of
            Just _  -> ("ffcabal", ["build"] <> pjFileArgs <> builddir
                          <> ["--repl-only" | background] <> activeComponent' <> targets)
            Nothing -> ("cabal", ["build"] <> pjFileArgs <> builddir
                          <> activeComponent' <> targets)
        mbCmdAndArgs = case pjKey project of
            StackTool {} -> Just ("stack", ["build"] <> pjFileArgs <> activeComponent' <> targets)
            CabalTool {} -> Just cabalCmd
            CustomTool p -> (\(c, as) -> (T.pack c, as)) <$> pjCustomGhcBuild p
            NixTool _    -> Nothing
            -- A Makefile project builds with make (through the nix env when
            -- the project has one, like the other tools).
            MakeTool {}  -> Just ("make", [])
    withToolCommand project "ghc" mbCmdAndArgs $ \(cmd, args', nixEnv') -> do
        let mbEnv = M.toList <$> nixEnv'
        -- ffcabal drives tmux repls: pin them to leksah's own tmux server so
        -- the workspace repl buttons / terminal tabs can reach the windows.
        mbEnv' <- if isJust mbFFCabal
            then Just . addFFCabalTmuxEnv <$> maybe (liftIO getEnvironment) return mbEnv
            else return mbEnv
        runExternalTool' (__ "Building") (T.unpack cmd) args' dir mbEnv' $ do
            (mbLastOutput, _) <- C.getZipSink $ (,)
                <$> C.ZipSink sinkLast
                <*> C.ZipSink (logOutputForBuild project (LogProject dir) background jump)
            lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess))
  `catchIDE` (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

-- | Re-read a local package's components from its @.cabal@ (fresh test and
-- benchmark names for the build targets).
freshComponents :: Package -> IDEM (Maybe [Component])
freshComponents package = catchIDE
    (Just . pkgComponents . packageFromGPD' <$>
        liftIO (readGPD normalVerbosity (ipdCabalFile package)))
    (\(_ :: SomeException) -> return Nothing)
  where packageFromGPD' = packageFromGPD (ipdCabalFile package)

-- | The @pkg:component@ target for the ACTIVE component, when this package is
-- the active one — so a chosen component builds alone.
activeComponentTarget :: Project -> Package -> IDEM (Maybe Text)
activeComponentTarget project package = do
    isActiveProject <- maybe False ((== pjKey project) . pjKey) <$> readIDE activeProject
    isActivePackage <- (isActiveProject &&)
        . maybe False ((== ipdCabalFile package) . ipdCabalFile) <$> readIDE activePack
    if isActivePackage
        then fmap ((ipdPackageName package <> ":") <>) <$> readIDE activeComponent
        else return Nothing

-- ---------------------------------------------------------------------
-- Docs / tests / benchmarks
-- ---------------------------------------------------------------------

-- | Haddock the packages (the @makeDocs@ preference's build step).
docsPackages :: Bool -> Project -> [Package] -> (Bool -> IDEAction) -> IDEAction
docsPackages background project packages continuation = do
    jump <- jumpToWarnings <$> readIDE prefs
    let dir = pjDir $ pjKey project
        mbCmdAndArgs = case pjKey project of
            StackTool {} -> Just ("stack", ["haddock", "--no-haddock-deps"]
                                            <> map ipdPackageName packages)
            CabalTool {} -> Just ("cabal", ["haddock"] <> map ipdPackageName packages)
            _ -> Nothing
    case mbCmdAndArgs of
        Nothing -> continuation True
        Just _ -> withToolCommand project "ghc" mbCmdAndArgs $ \(cmd, args, nixEnv') ->
            runExternalTool' (__ "Documenting") (T.unpack cmd) args dir (M.toList <$> nixEnv') $ do
                mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> C.ZipSink
                    (logOutputForBuild project (LogProject dir) background jump)
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess))
  `catchIDE` (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

-- | Run each package's test suites (the @runUnitTests@ build step, and the
-- workspace tree's Test command).
testPackages :: Bool -> Project -> [Package] -> (Bool -> IDEAction) -> IDEAction
testPackages = componentsStep CkTest "test" (Just . LogCabal . ipdCabalFile)

-- | Run each package's benchmarks.
benchPackages :: Bool -> Project -> [Package] -> (Bool -> IDEAction) -> IDEAction
benchPackages = componentsStep CkBenchmark "bench" (Just . LogCabal . ipdCabalFile)

-- | Run every component of a kind, package by package, stopping at the first
-- failure.  Test runs clear the package's stale test diagnostics first.
componentsStep
    :: ComponentKind -> Text -> (Package -> Maybe Log)
    -> Bool -> Project -> [Package] -> (Bool -> IDEAction) -> IDEAction
componentsStep kind sub logFor background project packages continuation =
    goPackages packages
  where
    goPackages [] = continuation True
    goPackages (package:rest) = do
        when (kind == CkTest) $
            mapM_ removeTestLogRefs (logFor package)
        let comps = [ cName c | c <- pkgComponents package, cKind c == kind ]
        goComponents package comps (goPackages rest)
    goComponents _ [] k = k
    goComponents package (c:cs) k =
        runComponent sub background project package c $ \ok ->
            when ok $ goComponents package cs k

-- | Run one component with the project's tool (@cabal test pkg:test:foo@ etc).
runComponent :: Text -> Bool -> Project -> Package -> Text -> (Bool -> IDEAction) -> IDEAction
runComponent sub background project package component continuation = do
    jump <- jumpToWarnings <$> readIDE prefs
    let dir = ipdPackageDir package
        target = ipdPackageName package <> ":" <> component
    showDefaultLogLaunch'
    pjFileArgs <- projectFileArguments project dir
    let mbCmdAndArgs = case pjKey project of
            StackTool {} -> Just ("stack", [sub] <> pjFileArgs <> [target])
            CabalTool {} -> Just ("cabal", [sub] <> pjFileArgs <> [target])
            _            -> Nothing
    case mbCmdAndArgs of
        Nothing -> continuation True
        Just _ -> withToolCommand project "ghc" mbCmdAndArgs $ \(cmd, args', nixEnv') ->
            runExternalTool' (__ "Run " <> component) (T.unpack cmd) args' dir (M.toList <$> nixEnv') $ do
                (mbLastOutput, _) <- C.getZipSink $ (,)
                    <$> C.ZipSink sinkLast
                    <*> C.ZipSink (logOutputForBuild project (LogCabal (ipdCabalFile package))
                                                     background jump)
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess))
  `catchIDE` (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

-- | The workspace tree's Test / Benchmark commands (foreground, one package).
packageTest, packageBench :: Project -> Package -> IDEAction
packageTest project package = interruptAndRun $
    testPackages False project [package] (const (return ()))
packageBench project package = interruptAndRun $
    benchPackages False project [package] (const (return ()))

-- ---------------------------------------------------------------------
-- Clean
-- ---------------------------------------------------------------------

-- | Remove a package's build products: for cabal, its directory under both
-- build dirs (native and the JavaScript backend's); stack has its own clean.
packageClean :: Project -> Package -> IDEAction
packageClean project package = interruptAndRun $ do
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'
    let dir = pjDir $ pjKey project
    case pjKey project of
        CabalTool _ -> do
            cleanCabal (cabalBuildDir Nothing)
            cleanCabal (cabalBuildDir (Just "js-unknown-ghcjs"))
        StackTool _ ->
            runExternalTool' (__ "Cleaning") "stack" ["clean"] dir Nothing $
                void . C.getZipSink $ const <$> C.ZipSink sinkLast
                                            <*> C.ZipSink (logOutput logLaunch)
        _ -> return ()
  where
    cleanCabal buildDir = do
        (buildDir', _, _) <- liftIO $ cabalProjectBuildDir (pjDir $ pjKey project) buildDir
        let packageBuildDir = buildDir' </> T.unpack (packageIdentifierToString (ipdPackageId package))
        liftIO $ doesDirectoryExist packageBuildDir >>= \case
            True  -> removeDirectoryRecursive packageBuildDir
            False -> return ()

-- ---------------------------------------------------------------------
-- Plain-directory (cargo etc.) projects
-- ---------------------------------------------------------------------

-- | Guess a build command for a plain-directory ('CustomTool') project that
-- has no explicit build command configured, from marker files in its root.
-- A Rust crate (@Cargo.toml@) builds with @cargo build@.  'Nothing' means we
-- don't know how to build the directory.  An explicitly configured
-- 'pjCustomGhcBuild' always wins.
customBuildCommand :: Project -> IO (Maybe (Text, [Text]))
customBuildCommand project = case pjKey project of
    CustomTool p
        | Just (cmd, args) <- pjCustomGhcBuild p -> return (Just (T.pack cmd, args))
        | otherwise -> do
            let dir = pjCustomDir p
            cargo <- doesFileExist (dir </> "Cargo.toml")
            return $ if cargo then Just ("cargo", ["build"]) else Nothing
    _ -> return Nothing

-- | Build a plain-directory project that carries no Haskell packages — e.g. a
-- Rust crate added via \"Open Folder\".  Runs the guessed command through the
-- project's nix env (so @cargo@ comes from the flake dev shell) like the other
-- tools; cargo output goes through the rustc diagnostic parser.
buildCustomProject :: Project -> IDEAction
buildCustomProject project = interruptAndRun $ do
    let dir = pjDir $ pjKey project
    liftIO (customBuildCommand project) >>= \case
        Nothing -> ideMessage Normal $ __ "Don't know how to build " <> T.pack dir
        Just (cmd, args) -> (do
            showDefaultLogLaunch'
            -- cargo/rustc need their own diagnostic parser; the GHC one
            -- mis-tags rustup/progress output and inflates the count.
            let logParser =
                    if takeFileName (T.unpack cmd) == "cargo"
                        then void $ logOutputForCargoBuild project (LogProject dir) False
                        else void $ logOutputForBuild project (LogProject dir) False False
            withToolCommand project "ghc" (Just (cmd, args)) $ \(cmd', args', nixEnv') ->
                runExternalTool' (__ "Building") (T.unpack cmd') args' dir (M.toList <$> nixEnv') $
                    void . C.getZipSink $ const <$> C.ZipSink sinkLast
                                                <*> C.ZipSink logParser)
          `catchIDE` (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

-- ---------------------------------------------------------------------
-- Running executables
-- ---------------------------------------------------------------------

-- | @\<pkg\>_datadir@ for every package of the project, so a run finds its
-- data files without installing.
packagesEnv :: [Package] -> [(String, String)] -> [(String, String)]
packagesEnv packages env =
    map (\p -> (T.unpack (ipdPackageName p) <> "_datadir", ipdPackageDir p)) packages <> env

-- | The executable to run: the active one if a component is selected, else
-- the package's first.
exeToRun :: Maybe Text -> [Executable] -> [Executable]
exeToRun Nothing (exe:_) = [exe]
exeToRun Nothing _ = []
exeToRun (Just selected) exes =
    take 1 $ filter (\Executable{exeName = n} ->
        selected == "exe:" <> T.pack (unUnqualComponentName n)) exes

-- | Run the package's (active) executable, streaming its output to a log
-- launch named after it.
packageRun :: Project -> Package -> IDEAction
packageRun project package = interruptAndRun $ (do
    pd <- liftIO $ flattenPackageDescription <$> readGPD normalVerbosity (ipdCabalFile package)
    mbComponent <- readIDE activeComponent
    let exe = exeToRun mbComponent (executables pd)
        logName' = fromMaybe (ipdPackageName package) . listToMaybe $
                       map (T.pack . unUnqualComponentName . exeName) exe
    (logLaunch, logName) <- buildLogLaunchByName logName'
    showLog
    let dir = ipdPackageDir package
    case pjKey project of
        StackTool {} -> runTool' (addLogLaunchData logName logLaunch)
            (__ "Running " <> logName) "stack"
            (["exec"] <> map (T.pack . unUnqualComponentName . exeName) exe <> ["--"])
            dir Nothing (logOutput logLaunch)
        CabalTool {} -> do
            (buildDir, cDir, _) <- liftIO $
                cabalProjectBuildDir (pjDir $ pjKey project) (cabalBuildDir Nothing)
            env <- packagesEnv (pjPackages project) <$> liftIO getEnvironment
            case exe <> executables pd of
                [] -> return ()
                (Executable {exeName = name} : _) -> do
                    let path' c = buildDir
                                    </> T.unpack (packageIdentifierToString (ipdPackageId package))
                                    </> c </> unUnqualComponentName name </> unUnqualComponentName name
                    path <- liftIO $ doesFileExist (path' "build") >>= \case
                        True  -> return $ path' "build"
                        False -> return . path' $ cDir "x" (unUnqualComponentName name)
                    runTool' (addLogLaunchData logName logLaunch)
                             (__ "Running " <> logName) path [] dir (Just env)
                             (logOutput logLaunch)
        _ -> ideMessage High (__ "Unable to run a package in this project type"))
  `catchIDE` (\(e :: SomeException) -> ideMessage High (T.pack $ show e))

-- | Build with the JavaScript backend, then show the built @jsexe@ page in
-- the output pane.
packageRunJavaScript :: Project -> Package -> IDEAction
packageRunJavaScript project package
    | pjIsStack (pjKey project) = ideMessage Normal
        (__ "Leksah does not know how to run stack.yaml projects built with GHCJS.  Please use a cabal.project file instead.")
    | otherwise = interruptAndRun $
        buildPackages False project [package] $ \ok -> when ok $ (do
            pd <- liftIO $ flattenPackageDescription <$> readGPD normalVerbosity (ipdCabalFile package)
            mbComponent <- readIDE activeComponent
            let exe = exeToRun mbComponent (executables pd)
            case exe <> executables pd of
                (Executable {exeName = name} : _) -> do
                    (buildDir, cDir, _) <- liftIO $ cabalProjectBuildDir
                        (pjDir $ pjKey project) (cabalBuildDir (Just "js-unknown-ghcjs"))
                    let path' c = buildDir
                                </> T.unpack (packageIdentifierToString (ipdPackageId package))
                                </> c </> unUnqualComponentName name
                                </> unUnqualComponentName name <.> "jsexe" </> "index.html"
                    path <- liftIO $ doesFileExist (path' "build") >>= \case
                        True  -> return $ path' "build"
                        False -> return . path' $ cDir "x" (unUnqualComponentName name)
                    postAsyncIDE $ do
                        loadOutputHtmlFile path
                        showOutputPane
                _ -> return ())
          `catchIDE` (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

-- | Run an external tool with no run-guard (the package commands' wrapper).
runTool' :: (ProcessHandle -> IDEAction) -> Text -> FilePath -> [Text] -> FilePath
         -> Maybe [(String, String)] -> ConduitT ToolOutput Void IDEM () -> IDEAction
runTool' = runExternalTool (return True)

-- ---------------------------------------------------------------------
-- Interrupt-then-run
-- ---------------------------------------------------------------------

-- | Interrupt a running build before starting another (retrying while one is
-- still shutting down).  The old save-all step is gone: the web UI saves
-- through its own editor path before asking for a build.
interruptAndRun :: (MonadUnliftIO m, MonadIDE m) => m () -> m ()
interruptAndRun action = do
    alreadyRunning <- isRunning
    if alreadyRunning
        then do
            liftIO $ debugM "leksah" "interruptAndRun: interrupting"
            interruptBuild
            void $ delayedBy 200000 (interruptAndRun action)
        else action
