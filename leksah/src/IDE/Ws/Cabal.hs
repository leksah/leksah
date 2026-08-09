{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: Apache-2.0

-- | The cabal project type.
--
-- Root marker: a @cabal.project@ file (recorded as 'pkFile'), or a lone
-- @*.cabal@ file.  Packages come from the @packages:@ (and
-- @optional-packages:@) fields of @cabal.project@ — entries may be plain
-- dirs (@.@, @pkg\/@), globs with @*@ in one path segment (@vendor\/*\/@),
-- or direct @foo.cabal@ files.  Each package's @.cabal@ is parsed with the
-- Cabal library.
module IDE.Ws.Cabal
  ( cabalProjectType
  , readCabalPackage
  , findCabalFileIn
  , packageTarget
  , componentTarget
  ) where

import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.List (isSuffixOf, nub, sort, tails)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.FilePath
  ( (</>), isAbsolute, normalise, splitDirectories, takeDirectory )

import qualified Distribution.Package as C
import qualified Distribution.PackageDescription as C
import Distribution.PackageDescription.Parsec
  ( parseGenericPackageDescriptionMaybe )
import Distribution.Pretty (prettyShow)
import qualified Distribution.Types.UnqualComponentName as C
import Distribution.Utils.Path (getSymbolicPath)

import IDE.Ws.Types

-- | The @cabal@ project type.
cabalProjectType :: ProjectType
cabalProjectType = ProjectType
  { ptId = "cabal"
  , ptDetect = detect
  , ptEnumerate = enumerate
  , ptCommand = command
  }

-- Detection ------------------------------------------------------------

detect :: Effects -> FilePath -> IO (Maybe ProjectKey)
detect eff dir = do
  let projFile = dir </> "cabal.project"
  hasProj <- eDoesExist eff projFile
  if hasProj
    then pure (Just (ProjectKey "cabal" dir (Just projFile)))
    else do
      names <- eListDir eff dir
      case filter (".cabal" `isSuffixOf`) names of
        [_] -> pure (Just (ProjectKey "cabal" dir Nothing))
        _   -> pure Nothing

-- Enumeration ----------------------------------------------------------

enumerate :: Effects -> ProjectKey -> IO (Either Text Project)
enumerate eff key = do
  cabalFiles <- case pkFile key of
    Just projFile -> do
      mbs <- eReadFile eff projFile
      case mbs of
        Nothing -> pure (Left ("cannot read " <> T.pack projFile))
        Just bs -> do
          let entries = projectPackageEntries (decodeUtf8Lenient bs)
          files <- concat <$> mapM (resolveEntry eff (pkRoot key)) entries
          pure (Right (nub files))
    Nothing -> do
      mb <- findCabalFileIn eff (pkRoot key)
      pure (Right (maybeToList mb))
  case cabalFiles of
    Left err -> pure (Left err)
    Right files -> do
      results <- mapM (readCabalPackage eff) files
      pure $ case sequence results of
        Left err   -> Left err
        Right pkgs -> Right (Project key pkgs)

-- | Extract the entries of the @packages:@ and @optional-packages:@ fields
-- from @cabal.project@ text.  Only top-level (column-0) fields are
-- considered; continuation lines are the indented lines that follow.
-- @--@ comments are stripped; entries are separated by whitespace or commas.
projectPackageEntries :: Text -> [String]
projectPackageEntries txt = go (map stripComment (T.lines txt))
 where
  go [] = []
  go (l:ls)
    | Just rest <- fieldValue "packages:" l = takeField rest ls
    | Just rest <- fieldValue "optional-packages:" l = takeField rest ls
    | otherwise = go ls
  takeField rest ls =
    let (contLines, more) = span continuation ls
        body = T.unwords (rest : map T.strip contLines)
    in splitEntries body ++ go more
  continuation l = T.null l || isSpace (T.head l)
  fieldValue name l
    | T.toLower name `T.isPrefixOf` T.toLower l
    = Just (T.strip (T.drop (T.length name) l))
    | otherwise = Nothing
  splitEntries = map T.unpack . filter (not . T.null)
               . T.split (\c -> isSpace c || c == ',')
  stripComment l = case T.breakOn "--" l of
    (before, rest)
      | T.null rest -> l
      | T.null before || isSpace (T.last before) -> T.stripEnd before
      | otherwise ->
          before <> T.take 2 rest
                 <> stripComment (T.drop 2 rest)

-- | Resolve one @packages:@ entry to zero or more absolute @.cabal@ paths.
-- Globs (@*@ within a path segment) are expanded via 'eListDir'; an entry
-- naming a directory contributes the @.cabal@ file found inside it; entries
-- that resolve to nothing are skipped.
resolveEntry :: Effects -> FilePath -> String -> IO [FilePath]
resolveEntry eff root entry0 = do
  let entry = unquote entry0
      (base, segs)
        | isAbsolute entry = ("/", drop 1 (splitDirectories entry))
        | otherwise        = (root, splitDirectories entry)
  paths <- expand [base] segs
  concat <$> mapM toCabalFile paths
 where
  unquote s = case s of
    ('"':rest) | not (null rest), last rest == '"' -> init rest
    _ -> s
  expand paths [] = pure paths
  expand paths (seg:rest)
    | seg == "."  = expand paths rest
    | '*' `elem` seg = do
        next <- concat <$> mapM (matchIn seg) paths
        expand next rest
    | otherwise = expand (map (</> seg) paths) rest
  matchIn pat dir = do
    names <- eListDir eff dir
    pure [dir </> n | n <- sort names, globMatch pat n]
  toCabalFile path
    | ".cabal" `isSuffixOf` path = do
        ok <- eDoesExist eff path
        pure [normalise path | ok]
    | otherwise = maybeToList <$> findCabalFileIn eff path

-- | Find the (alphabetically first) @*.cabal@ file in a directory.
findCabalFileIn :: Effects -> FilePath -> IO (Maybe FilePath)
findCabalFileIn eff dir = do
  names <- eListDir eff dir
  case sort (filter (".cabal" `isSuffixOf`) names) of
    (n:_) -> pure (Just (normalise (dir </> n)))
    []    -> pure Nothing

-- | Match a glob pattern (@*@ = any sequence, within one segment) against
-- a file name.
globMatch :: String -> String -> Bool
globMatch [] [] = True
globMatch ('*':ps) s = any (globMatch ps) (tails s)
globMatch (p:ps) (c:cs) = p == c && globMatch ps cs
globMatch _ _ = False

-- .cabal parsing --------------------------------------------------------

-- | Read and parse a @.cabal@ file (through 'Effects') into a 'Package'.
-- Component structure is taken from the unconditional part of each
-- component's condition tree.  Also used by the stack project type.
readCabalPackage :: Effects -> FilePath -> IO (Either Text Package)
readCabalPackage eff cabalFile = do
  mbs <- eReadFile eff cabalFile
  pure $ case mbs of
    Nothing -> Left ("cannot read " <> T.pack cabalFile)
    Just bs -> case parseGenericPackageDescriptionMaybe bs of
      Nothing  -> Left ("cannot parse " <> T.pack cabalFile)
      Just gpd -> Right (packageFromGpd cabalFile gpd)

packageFromGpd :: FilePath -> C.GenericPackageDescription -> Package
packageFromGpd cabalFile gpd = Package
  { pkgName = name
  , pkgVersion = T.pack (prettyShow (C.pkgVersion pid))
  , pkgManifest = cabalFile
  , pkgDir = takeDirectory cabalFile
  , pkgComponents = comps
  , pkgSrcDirs = nub (concat srcDirss)
  }
 where
  pd = C.packageDescription gpd
  pid = C.package pd
  name = T.pack (C.unPackageName (C.pkgName pid))
  (comps, srcDirss) = unzip $
       [ (Component KLib name, srcDirs (C.libBuildInfo lib))
       | lib <- maybeToList (C.condTreeData <$> C.condLibrary gpd) ]
    ++ [ (Component KLib (ucn n), srcDirs (C.libBuildInfo (C.condTreeData t)))
       | (n, t) <- C.condSubLibraries gpd ]
    ++ [ (Component KExe (ucn n), srcDirs (C.buildInfo (C.condTreeData t)))
       | (n, t) <- C.condExecutables gpd ]
    ++ [ (Component KTest (ucn n), srcDirs (C.testBuildInfo (C.condTreeData t)))
       | (n, t) <- C.condTestSuites gpd ]
    ++ [ (Component KBench (ucn n),
          srcDirs (C.benchmarkBuildInfo (C.condTreeData t)))
       | (n, t) <- C.condBenchmarks gpd ]
  ucn = T.pack . C.unUnqualComponentName
  -- no hs-source-dirs means the package dir itself
  srcDirs bi = case map getSymbolicPath (C.hsSourceDirs bi) of
    [] -> ["."]
    ds -> ds

-- Commands --------------------------------------------------------------

-- | The cabal target for a whole package: @pkg:NAME@.
packageTarget :: Package -> Text
packageTarget p = "pkg:" <> pkgName p

-- | The cabal target for a component: @NAME:exe:NAME@ style
-- (@lib@\/@exe@\/@test@\/@bench@; a 'KOther' component renders without a
-- kind, @NAME:CNAME@).  Exported for UI display; also valid for stack.
componentTarget :: Package -> Component -> Text
componentTarget p c = case cKind c of
  KOther -> pkgName p <> ":" <> cName c
  k      -> pkgName p <> ":" <> kindId k <> ":" <> cName c

command :: Verb -> Scope -> Maybe ToolCmd
command verb scope = case verb of
  VClean -> Just (cabal ["clean"])
  VBuild -> Just (cabal ["build", target])
  VRepl  -> Just (cabal ["repl", target])
  VRun -> case scope of
    ScopeProject -> Nothing
    ScopePackage _ -> Just (cabal ["run", target])
    ScopeComponent _ c
      | cKind c == KExe -> Just (cabal ["run", target])
      | otherwise -> Nothing
  VTest -> case scope of
    ScopeComponent _ c
      | cKind c /= KTest -> Nothing
    _ -> Just (cabal ["test", target])
  VBench -> case scope of
    ScopeComponent _ c
      | cKind c /= KBench -> Nothing
    _ -> Just (cabal ["bench", target])
 where
  cabal args = ToolCmd "cabal" args "."
  target = case scope of
    ScopeProject -> "all"
    ScopePackage p -> packageTarget p
    ScopeComponent p c -> componentTarget p c

-- Local helpers ----------------------------------------------------------

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With TEE.lenientDecode
