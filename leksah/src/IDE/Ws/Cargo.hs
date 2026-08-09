{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: Apache-2.0

-- | The cargo project type.
--
-- Root marker: @Cargo.toml@ ('pkFile').  Enumeration runs
-- @cargo metadata --format-version 1 --no-deps@ through 'eRunTool' and
-- decodes the JSON: each workspace member becomes a 'Package'; target
-- kinds map @lib -> KLib@, @bin -> KExe@, @test -> KTest@,
-- @bench -> KBench@, anything else 'KOther'; source dirs are the parent
-- directories of the targets' @src_path@s.  When @cargo metadata@ is
-- unavailable (tool missing or failing) it falls back to a single package
-- named after the root directory with one 'KOther' component.
module IDE.Ws.Cargo
  ( cargoProjectType
  ) where

import Data.Aeson
  ( FromJSON(..), eitherDecodeStrict', withObject, (.:), (.:?), (.!=) )
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath
  ( (</>), makeRelative, normalise, takeDirectory, takeFileName )

import IDE.Ws.Types

-- | The @cargo@ project type.
cargoProjectType :: ProjectType
cargoProjectType = ProjectType
  { ptId = "cargo"
  , ptDetect = detect
  , ptEnumerate = enumerate
  , ptCommand = command
  }

detect :: Effects -> FilePath -> IO (Maybe ProjectKey)
detect eff dir = do
  let toml = dir </> "Cargo.toml"
  has <- eDoesExist eff toml
  pure $ if has then Just (ProjectKey "cargo" dir (Just toml)) else Nothing

-- cargo metadata JSON ----------------------------------------------------

newtype CargoMeta = CargoMeta { cmPackages :: [CargoPkg] }

instance FromJSON CargoMeta where
  parseJSON = withObject "cargo metadata" $ \o ->
    CargoMeta <$> o .: "packages"

data CargoPkg = CargoPkg
  { cpName :: Text
  , cpVersion :: Text
  , cpManifest :: FilePath
  , cpTargets :: [CargoTarget]
  }

instance FromJSON CargoPkg where
  parseJSON = withObject "package" $ \o -> CargoPkg
    <$> o .: "name"
    <*> o .: "version"
    <*> o .: "manifest_path"
    <*> o .:? "targets" .!= []

data CargoTarget = CargoTarget
  { ctKinds :: [Text]
  , ctName :: Text
  , ctSrcPath :: Maybe FilePath
  }

instance FromJSON CargoTarget where
  parseJSON = withObject "target" $ \o -> CargoTarget
    <$> o .:? "kind" .!= []
    <*> o .: "name"
    <*> o .:? "src_path"

enumerate :: Effects -> ProjectKey -> IO (Either Text Project)
enumerate eff key = do
  mout <- eRunTool eff (pkRoot key) "cargo"
    ["metadata", "--format-version", "1", "--no-deps"]
  case mout of
    Nothing -> Right . Project key . (:[]) <$> fallbackPackage eff key
    Just out -> pure $ case eitherDecodeStrict' out of
      Left err -> Left ("cargo metadata: " <> T.pack err)
      Right meta ->
        Right (Project key (map packageFromCargo (cmPackages meta)))

packageFromCargo :: CargoPkg -> Package
packageFromCargo cp = Package
  { pkgName = cpName cp
  , pkgVersion = cpVersion cp
  , pkgManifest = manifest
  , pkgDir = dir
  , pkgComponents = map targetComponent (cpTargets cp)
  , pkgSrcDirs = nub
      [ makeRelative dir (takeDirectory sp)
      | t <- cpTargets cp, Just sp <- [ctSrcPath t] ]
  }
 where
  manifest = normalise (cpManifest cp)
  dir = takeDirectory manifest
  targetComponent t = Component (kindOf (ctKinds t)) (ctName t)
  kindOf ks
    | "lib"   `elem` ks = KLib
    | "bin"   `elem` ks = KExe
    | "test"  `elem` ks = KTest
    | "bench" `elem` ks = KBench
    | otherwise         = KOther

-- | Used when @cargo metadata@ is unavailable: a single package named
-- after the root directory, one 'KOther' component, @src@ as the source
-- dir when it exists.
fallbackPackage :: Effects -> ProjectKey -> IO Package
fallbackPackage eff key = do
  let name = T.pack (takeFileName (pkRoot key))
  hasSrc <- eDoesExist eff (pkRoot key </> "src")
  pure Package
    { pkgName = name
    , pkgVersion = ""
    , pkgManifest = pkRoot key </> "Cargo.toml"
    , pkgDir = pkRoot key
    , pkgComponents = [Component KOther name]
    , pkgSrcDirs = ["src" | hasSrc]
    }

command :: Verb -> Scope -> Maybe ToolCmd
command verb scope = case verb of
  VRepl  -> Nothing
  VClean -> Just (cargo ["clean"])
  VBuild -> Just (cargo ("build" : selection))
  VTest -> case scope of
    ScopeComponent p c
      | cKind c == KTest -> Just (cargo ["test", "-p", pkgName p
                                        , "--test", cName c])
      | otherwise -> Nothing
    _ -> Just (cargo ("test" : selection))
  VBench -> case scope of
    ScopeComponent p c
      | cKind c == KBench -> Just (cargo ["bench", "-p", pkgName p
                                         , "--bench", cName c])
      | otherwise -> Nothing
    _ -> Just (cargo ("bench" : selection))
  VRun -> case scope of
    ScopeComponent p c
      | cKind c == KExe -> Just (cargo ["run", "-p", pkgName p
                                       , "--bin", cName c])
      | otherwise -> Nothing
    ScopePackage p -> Just (cargo ["run", "-p", pkgName p])
    ScopeProject -> Nothing
 where
  cargo args = ToolCmd "cargo" args "."
  selection = case scope of
    ScopeProject -> ["--workspace"]
    ScopePackage p -> ["-p", pkgName p]
    ScopeComponent p c -> ["-p", pkgName p] ++ componentFlag c
  componentFlag c = case cKind c of
    KLib   -> ["--lib"]
    KExe   -> ["--bin", cName c]
    KTest  -> ["--test", cName c]
    KBench -> ["--bench", cName c]
    KOther -> []
