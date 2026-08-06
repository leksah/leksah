-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Enumerating a project: turn a 'ProjectKey' into the 'Project' the IDE
-- shows and builds — the project file's package list, and each package's
-- identity, components and source dirs.
--
-- The @.cabal@ reading (and the batched remote-snapshot path) is carried from
-- the old @IDE.Package@; the package-list parsing and the
-- description-to-'Package' extraction are fresh (and fix a long-standing
-- bug: a package with only SUBlibraries no longer claims a main library,
-- which made cabal reject the generated @lib:<pkg>@ target).
module IDE.Project.Commands
  ( resolveProject
  , packageFromFile
  , packageFromGPD
  , readGPD
  , gpdFromBytes
  , normalVerbosity
  , cabalProjectPackages
  , stackYamlPackages
  ) where

import Control.Exception (SomeException)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (nub, nubBy)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Data.Text.Encoding (decodeUtf8)
import System.Directory (canonicalizePath)
import System.FilePath ((</>), dropFileName, takeExtension)
import qualified System.FilePath.Glob as Glob (compile, globDir)

import Distribution.Package (pkgName, unPackageName)
import Distribution.PackageDescription
       (Benchmark(..), Executable(..), Library(..), PackageDescription(..),
        TestSuite(..), benchmarkName, exeName, hsSourceDirs, libName,
        testName)
import Distribution.PackageDescription.Configuration
       (flattenPackageDescription)
import Distribution.PackageDescription.Parsec
       (parseGenericPackageDescriptionMaybe)
import Distribution.Types.GenericPackageDescription
       (GenericPackageDescription)
import Distribution.Types.LibraryName (libraryNameString)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Verbosity (Verbosity, normal)
#if MIN_VERSION_Cabal(3,8,0)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Utils.Path (getSymbolicPath)
#else
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#endif
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (makeSymbolicPath)
#endif
#if MIN_VERSION_Cabal(3,17,0)
import Distribution.Verbosity (mkVerbosity, defaultVerbosityHandles)
#endif

import IDE.Core.State
       (IDEM, MessageLevel(..), __, catchIDE, ideMessage)
import IDE.Project
       (Component(..), ComponentKind(..), Package(..), Project(..),
        mkPackageMap)
import IDE.Utils.Files (myCanonicalizePath)
import IDE.Utils.Project
       (CabalProject(..), ProjectKey(..), StackProject(..), pjDir,
        pjFileOrDir)
import IDE.Utils.RemotePath
       (isRemotePath, parseRemotePath, renderRemotePath)
import IDE.Web.FS (fsListFilesRecursive, fsReadFile)
#if !defined(ghcjs_HOST_OS)
import IDE.Utils.RemoteExec (SnapshotEntry(..), remoteCabalSnapshot)
#endif

-- Cabal 3.17 (stable-haskell fork) replaced Verbosity's constructors with a
-- handles-carrying value.
normalVerbosity :: Verbosity
#if MIN_VERSION_Cabal(3,17,0)
normalVerbosity = mkVerbosity defaultVerbosityHandles normal
#else
normalVerbosity = normal
#endif

-- | Parse a .cabal file from bytes that came through the FS seam (the
-- browser demo's mock tree, a remote host, or a project snapshot).
gpdFromBytes :: FilePath -> ByteString -> IO GenericPackageDescription
gpdFromBytes f bs = case parseGenericPackageDescriptionMaybe bs of
    Just gpd -> return gpd
    Nothing  -> ioError (userError ("Failed to parse " <> f))

-- Cabal 3.14 moved the cabal-file argument to a SymbolicPath and added a
-- working-directory argument; older Cabal takes a plain FilePath.
readGPD :: Verbosity -> FilePath -> IO GenericPackageDescription
#if defined(ghcjs_HOST_OS)
-- Browser demo: the .cabal file lives in the page-seeded mock tree
-- (IDE.Web.FS), so parse it from bytes instead of opening a real file.
readGPD _ f = gpdFromBytes f =<< fsReadFile f
#else
readGPD v f
    -- Remote .cabal files come through the FS seam as bytes.
    | isRemotePath f = gpdFromBytes f =<< fsReadFile f
    | otherwise =
#if MIN_VERSION_Cabal(3,14,0)
        readGenericPackageDescription v Nothing (makeSymbolicPath f)
#else
        readGenericPackageDescription v f
#endif
#endif

-- | The 'Package' a parsed description describes: its identity, its typed
-- components and its source dirs.
--
-- The main library is a component only when the description really has one
-- (@library@ stanza); named sublibraries are separate 'CkSubLibrary'
-- components.  The old model conflated the two through @hasLibs@, so a
-- sublibrary-only package advertised a @lib:\<pkg\>@ target that cabal
-- rejects.
packageFromGPD :: FilePath -> GenericPackageDescription -> Package
packageFromGPD file gpd =
    let pd = flattenPackageDescription gpd
        libComponent = case library pd of
            Just _  -> [Component CkLibrary (T.pack (unPackageName (pkgName (package pd))))]
            Nothing -> []
        subLibs =
            [ Component CkSubLibrary (T.pack (unUnqualComponentName n))
            | Just n <- libraryNameString . libName <$> subLibraries pd ]
        exes   = [ Component CkExecutable (compName (exeName e)) | e <- executables pd ]
        tests  = [ Component CkTest (compName (testName t)) | t <- testSuites pd ]
        benchs = [ Component CkBenchmark (compName (benchmarkName b)) | b <- benchmarks pd ]
        srcDirs = case nub (concatMap sourceDirs (allBuildInfos pd)) of
            [] -> [".", "src"]
            l  -> l
    in Package
        { pkgId         = package pd
        , pkgFile       = file
        , pkgComponents = libComponent <> subLibs <> exes <> tests <> benchs
        , pkgSrcDirs    = srcDirs
        }
  where
    compName = T.pack . unUnqualComponentName
#if MIN_VERSION_Cabal(3,8,0)
    sourceDirs = map getSymbolicPath . hsSourceDirs
#else
    sourceDirs = hsSourceDirs
#endif
    -- Every build info, including unbuildable components (their source dirs
    -- still hold files the user edits).
    allBuildInfos pd =
           [ libBuildInfo lib       | Just lib <- [library pd] ]
        <> [ libBuildInfo lib       | lib <- subLibraries pd ]
        <> [ buildInfo exe          | exe <- executables pd ]
        <> [ testBuildInfo tst      | tst <- testSuites pd ]
        <> [ benchmarkBuildInfo bch | bch <- benchmarks pd ]

-- | Read and describe one package manifest.  A parse failure is reported and
-- yields 'Nothing' (the project loads without it).
packageFromFile :: FilePath -> IDEM (Maybe Package)
packageFromFile cabalFile =
    catchIDE (Just . packageFromGPD cabalFile <$> liftIO (readGPD normalVerbosity cabalFile))
        (\(e :: SomeException) -> do
            ideMessage Normal (__ "Can't read package " <> T.pack (show e))
            return Nothing)

-- | The package directories a @cabal.project@ lists (@packages:@ and
-- @optional-packages:@ stanzas).  A @.cabal@ entry contributes its directory.
cabalProjectPackages :: Text -> [FilePath]
cabalProjectPackages txt =
    concatMap entriesOf ["packages:", "optional-packages:"]
  where
    entriesOf field = map asDir (fieldValues field (stripComments "--" txt))
    asDir f | takeExtension f == ".cabal" =
                let d = dropFileName f in if null d then "./" else d
            | otherwise = f

-- | The package directories a @stack.yaml@ lists (its @packages:@ sequence;
-- @location:@ sub-keys and quoting handled).  No packages means the project
-- dir itself, as stack defines.
stackYamlPackages :: Text -> [FilePath]
stackYamlPackages txt =
    case mapMaybe entry (fieldValues "packages:" (stripComments "#" txt)) of
        [] -> ["."]
        ps -> ps
  where
    entry = fmap unquote . Just . dropLocation
    dropLocation s = fromMaybe s (stripPrefix' "location: " s)
    stripPrefix' p s = if take (length p) s == p then Just (drop (length p) s) else Nothing
    unquote ('\'':rest) | not (null rest), last rest == '\'' = init rest
    unquote ('"':rest)  | not (null rest), last rest == '"'  = init rest
    unquote x = x

-- | The values under an indented list field: everything indented below the
-- field name, with @-@ bullets and inline values accepted.  Shared by the
-- cabal.project and stack.yaml readers (both are indentation-based).
fieldValues :: Text -> [Text] -> [FilePath]
fieldValues field ls = case break (field `T.isPrefixOf`) ls of
    (_, []) -> []
    (_, hd:rest) ->
        let inline = T.words (T.drop (T.length field) hd)
            indented = takeWhile (\l -> T.null (T.strip l) || indentedLine l) rest
        in map T.unpack (filter (not . T.null) (inline <> concatMap bullets indented))
  where
    indentedLine l = " " `T.isPrefixOf` l || "\t" `T.isPrefixOf` l
    bullets l = T.words (fromMaybe s (T.stripPrefix "- " s)) where s = T.strip l

-- | Drop comments (from the given lead-in to end of line) and blank lines.
stripComments :: Text -> Text -> [Text]
stripComments lead =
    filter (not . T.null . T.strip) . map (fst . T.breakOn lead) . T.lines

-- | The project a key describes: its packages read from the project file's
-- package list.  Project types with no Haskell packages (a plain directory, a
-- flake, a Makefile) resolve to a package-less project — their tree content
-- comes from directory introspection instead.
resolveProject :: ProjectKey -> IDEM (Maybe Project)
resolveProject key = (do
#if defined(ghcjs_HOST_OS)
    -- Browser demo: no Glob over a real file system — take every .cabal file
    -- under the project directory from the page-seeded mock tree (it is tiny,
    -- so the packages: globs are not interpreted).  Paths are absolute.
    cabalFiles <- liftIO $ filter ((== ".cabal") . takeExtension)
                      <$> fsListFilesRecursive (pjDir key)
    packages <- fmap catMaybes . mapM packageFromFile $ nub cabalFiles
#else
    packages <- case parseRemotePath (pjFileOrDir key) of
      Just (host, _) -> remotePackages host
      Nothing        -> localPackages
#endif
    return . Just $ Project { pjKey = key, pjPackageMap = mkPackageMap packages })
  `catchIDE`
     (\(e :: SomeException) -> do
        ideMessage Normal . T.pack $ show e
        return Nothing)
  where
#if !defined(ghcjs_HOST_OS)
    -- Local: glob the listed package dirs for .cabal files.
    localPackages = do
        dirs <- liftIO $ case key of
            CabalTool (CabalProject f) -> cabalProjectPackages <$> T.readFile f
            StackTool (StackProject f) -> stackYamlPackages <$> T.readFile f
            -- A flake/Makefile/plain-directory project has no cabal package
            -- list; its tree shows files (and flake outputs) instead.
            _ -> return []
        let patterns = map (Glob.compile . (</> "*.cabal")) dirs
            dir = pjDir key
        cabalFiles <- liftIO $ mapM canonicalizePath
            =<< map (dir </>) . concat <$> Glob.globDir patterns dir
        fmap catMaybes . mapM packageFromFile $ nub cabalFiles

    -- Remote: 2 round trips — read the project file, then ONE batched
    -- snapshot streaming back every matched .cabal as bytes; the packages
    -- parse locally from those bytes with no further remote reads.
    remotePackages host = do
        let rdir = maybe (pjDir key) snd (parseRemotePath (pjDir key))
        pkgDirs <- case key of
            CabalTool (CabalProject f) ->
                cabalProjectPackages . decodeUtf8 <$> liftIO (fsReadFile f)
            StackTool (StackProject f) ->
                stackYamlPackages . decodeUtf8 <$> liftIO (fsReadFile f)
            _ -> return []
        if null pkgDirs then return [] else do
            entries <- nubBy ((==) `on` seLocalPath)
                <$> liftIO (remoteCabalSnapshot host rdir pkgDirs)
            fmap catMaybes . forM [ e | e <- entries
                                  , takeExtension (seLocalPath e) == ".cabal" ] $ \e -> do
                let cabalFile = renderRemotePath host (seLocalPath e)
                catchIDE (Just . packageFromGPD cabalFile
                              <$> liftIO (gpdFromBytes cabalFile (seBytes e)))
                    (\(e' :: SomeException) -> do
                        ideMessage Normal (__ "Can't read package " <> T.pack (show e'))
                        return Nothing)
#endif
