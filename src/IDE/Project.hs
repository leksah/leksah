-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The project model: a 'Workspace' holds 'Project's (identified by their
-- 'ProjectKey' — cabal\/stack\/custom-dir\/nix-flake\/make), each holding the
-- 'Package's its project file names, each with typed 'Component's.
--
-- A fresh, minimal replacement for the old 22-field @IDEPackage@: the record
-- carries exactly what the IDE reads (identity, label, components, source
-- dirs).  Compatibility accessors keep the old @ipd*@ names working — the
-- component name lists derive from 'pkgComponents'.
module IDE.Project
  ( -- * Components
    ComponentKind(..)
  , Component(..)
  , componentTarget
    -- * Packages
  , Package(..)
  , pkgDir
  , IDEPackage
  , ipdCabalFile
  , ipdPackageId
  , ipdPackageDir
  , ipdPackageName
  , ipdLib
  , ipdSubLibraries
  , ipdExes
  , ipdTests
  , ipdBenchmarks
  , ipdSrcDirs
  , mkPackageMap
    -- * Projects
  , Project(..)
  , pjPackages
  , pjLookupPackage
    -- * Per-project settings
  , ProjectSettings(..)
  , defaultProjectSettings
    -- * The workspace
  , Workspace(..)
  , wsFile
  , wsProjects
  , wsProjectSettings
  , wsActiveProjectKey
  , wsActivePackFile
  , wsActiveComponent
  , emptyWorkspace
  , wsProjectKeys
  , wsLookupProject
  , wsActiveProject
  , wsActivePackage
  , wsPackages
  , wsProjectAndPackages
  , wsAllPackages
  , wsSettingsFor
  ) where

import Control.Lens (Getter, makeLenses, to, view, (^.))
import Control.Monad ((>=>))
import Data.Aeson
       (FromJSON(..), ToJSON(..), defaultOptions, genericParseJSON,
        genericToEncoding, genericToJSON, omitNothingFields)
import Data.Function (on)
import Data.List (find, nubBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath (dropFileName)

import Distribution.Package (PackageIdentifier(..), unPackageName)

import IDE.Utils.Project (ProjectKey(..), pjDir)

-- ---------------------------------------------------------------------
-- Components
-- ---------------------------------------------------------------------

-- | What kind of buildable thing a component is.  'CkLibrary' is the
-- package's main (unnamed) library; named sublibraries are 'CkSubLibrary'.
data ComponentKind
    = CkLibrary
    | CkSubLibrary
    | CkExecutable
    | CkTest
    | CkBenchmark
    deriving (Eq, Ord, Show, Generic)

data Component = Component
    { cKind :: ComponentKind
    , cName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | The component as a build-target string (@lib:foo@, @exe:bar@, …) — the
-- vocabulary cabal\/stack and the workspace tree share.
componentTarget :: Component -> Text
componentTarget (Component k n) = prefix <> n
  where
    prefix = case k of
        CkLibrary    -> "lib:"
        CkSubLibrary -> "lib:"
        CkExecutable -> "exe:"
        CkTest       -> "test:"
        CkBenchmark  -> "bench:"

-- ---------------------------------------------------------------------
-- Packages
-- ---------------------------------------------------------------------

-- | One package of a project.  Identity is the manifest path ('pkgFile');
-- everything else is what the IDE presents or builds.
data Package = Package
    { pkgId         :: PackageIdentifier
    , pkgFile       :: FilePath      -- ^ the @.cabal@ (or other manifest) path
    , pkgComponents :: [Component]
    , pkgSrcDirs    :: [FilePath]    -- ^ relative source dirs (file tree seeds)
    } deriving (Eq, Show)

-- | The directory of the manifest.
pkgDir :: Package -> FilePath
pkgDir = dropFileName . pkgFile

-- Compatibility layer: the old accessor names, over the new record. -----

type IDEPackage = Package

ipdCabalFile :: Package -> FilePath
ipdCabalFile = pkgFile

ipdPackageId :: Package -> PackageIdentifier
ipdPackageId = pkgId

ipdPackageDir :: Package -> FilePath
ipdPackageDir = pkgDir

ipdPackageName :: Package -> Text
ipdPackageName = T.pack . unPackageName . pkgName . pkgId

-- | The main library's presented name, when the package has one.
ipdLib :: Package -> Maybe Text
ipdLib p = if any ((== CkLibrary) . cKind) (pkgComponents p)
              then Just (ipdPackageName p)
              else Nothing

componentNames :: ComponentKind -> Package -> [Text]
componentNames k = map cName . filter ((== k) . cKind) . pkgComponents

ipdSubLibraries, ipdExes, ipdTests, ipdBenchmarks :: Package -> [Text]
ipdSubLibraries = componentNames CkSubLibrary
ipdExes         = componentNames CkExecutable
ipdTests        = componentNames CkTest
ipdBenchmarks   = componentNames CkBenchmark

ipdSrcDirs :: Package -> [FilePath]
ipdSrcDirs = pkgSrcDirs

mkPackageMap :: [Package] -> Map FilePath Package
mkPackageMap = M.fromList . map (\p -> (pkgFile p, p))

-- ---------------------------------------------------------------------
-- Projects
-- ---------------------------------------------------------------------

data Project = Project
    { pjKey        :: ProjectKey
    , pjPackageMap :: Map FilePath Package
    } deriving (Eq, Show)

pjPackages :: Project -> [Package]
pjPackages = M.elems . pjPackageMap

pjLookupPackage :: FilePath -> Project -> Maybe Package
pjLookupPackage f = M.lookup f . pjPackageMap

-- ---------------------------------------------------------------------
-- Per-project settings
-- ---------------------------------------------------------------------

-- | Per-project user settings persisted in the workspace file (they must
-- survive 'Project' being rebuilt from disk, so they live beside the
-- project list keyed by 'ProjectKey', not inside 'Project').
data ProjectSettings = ProjectSettings {
    -- | Shell fragment prefixed to tool commands run for this project on
    -- its remote host (e.g. @nix develop -c@).  Spliced verbatim into the
    -- remote command line — it may carry flags and shell syntax.
    psCmdPrefix :: Maybe Text
} deriving (Show, Eq, Generic)

defaultProjectSettings :: ProjectSettings
defaultProjectSettings = ProjectSettings {
    psCmdPrefix = Nothing
}

instance ToJSON ProjectSettings where
    toJSON = genericToJSON (defaultOptions { omitNothingFields = True })
    toEncoding = genericToEncoding (defaultOptions { omitNothingFields = True })
instance FromJSON ProjectSettings where
    parseJSON = genericParseJSON (defaultOptions { omitNothingFields = True })

-- ---------------------------------------------------------------------
-- The workspace
-- ---------------------------------------------------------------------

data Workspace = Workspace {
    _wsFile              ::   FilePath
,   _wsProjects          ::   [Project]
,   _wsProjectSettings   ::   Map ProjectKey ProjectSettings
,   _wsActiveProjectKey  ::   Maybe ProjectKey
,   _wsActivePackFile    ::   Maybe FilePath
,   _wsActiveComponent   ::   Maybe Text
} deriving Show

makeLenses ''Workspace

emptyWorkspace :: FilePath -> Workspace
emptyWorkspace fp = Workspace fp [] mempty Nothing Nothing Nothing

wsProjectKeys :: Getter Workspace [ProjectKey]
wsProjectKeys = wsProjects . to (map pjKey)

wsLookupProject :: ProjectKey -> Workspace -> Maybe Project
wsLookupProject f = find ((==f) . pjKey) . _wsProjects

_wsActiveProject :: Workspace -> Maybe Project
_wsActiveProject w = (w ^. wsActiveProjectKey) >>= (`wsLookupProject` w)

wsActiveProject :: Getter Workspace (Maybe Project)
wsActiveProject = to _wsActiveProject

_wsActivePackage :: Workspace -> Maybe Package
_wsActivePackage w = do
    project <- _wsActiveProject w
    _wsActivePackFile w >>= (`pjLookupPackage` project)

wsActivePackage :: Getter Workspace (Maybe Package)
wsActivePackage = to _wsActivePackage

wsPackages :: Getter Workspace [Package]
wsPackages = to (_wsProjects >=> pjPackages)

_wsProjectAndPackages :: Workspace -> [(Project, Package)]
_wsProjectAndPackages = _wsProjects >=> (\project -> (project,) <$> pjPackages project)

wsProjectAndPackages :: Getter Workspace [(Project, Package)]
wsProjectAndPackages = to _wsProjectAndPackages

_wsAllPackages :: Workspace -> [Package]
_wsAllPackages w = nubBy ((==) `on` pkgFile) $ w ^. wsPackages

wsAllPackages :: Getter Workspace [Package]
wsAllPackages = to _wsAllPackages

-- | The (total, defaulting) per-project settings for a project key.
wsSettingsFor :: ProjectKey -> Workspace -> ProjectSettings
wsSettingsFor pk = fromMaybe defaultProjectSettings . M.lookup pk . _wsProjectSettings
