{-# LANGUAGE DeriveGeneric #-}
module IDE.Utils.Project where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import System.FilePath (dropFileName, takeExtension, takeFileName)

newtype CabalProject = CabalProject
  { pjCabalFile :: FilePath
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON CabalProject
instance FromJSON CabalProject

newtype StackProject = StackProject
  { pjStackFile :: FilePath
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON StackProject
instance FromJSON StackProject

-- | A Nix flake project: its project file is the @flake.nix@.  The workspace
-- tree shows the flake's outputs (evaluated via @builtins.getFlake@) and the
-- project's files; see "IDE.Web.Widget.Flake".
newtype NixProject = NixProject
  { pjFlakeFile :: FilePath
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON NixProject
instance FromJSON NixProject

-- | A Makefile project: its project file is the @Makefile@.  Like a cabal
-- project it may also carry a @flake.nix@ (Flake node in the workspace tree)
-- and a git checkout (branch node); builds run @make@.
newtype MakeProject = MakeProject
  { pjMakeFile :: FilePath
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON MakeProject
instance FromJSON MakeProject

data CustomProject = CustomProject
  { pjCustomDir        :: FilePath
  , pjCustomNixShell   :: [Text]
  , pjCustomGhcBuild   :: Maybe (FilePath, [Text])
  , pjCustomGhcjsBuild :: Maybe (FilePath, [Text])
  , pjCustomDoc        :: Maybe (FilePath, [Text])
  , pjCustomRepl       :: Maybe (FilePath, [Text])
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON CustomProject
instance FromJSON CustomProject

data ProjectKey =
    CabalTool CabalProject
  | StackTool StackProject
  | CustomTool CustomProject
  | NixTool NixProject
  | MakeTool MakeProject
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON ProjectKey
instance FromJSON ProjectKey

pjDir :: ProjectKey -> FilePath
pjDir (CabalTool p)  = dropFileName (pjCabalFile p)
pjDir (StackTool p)  = dropFileName (pjStackFile p)
pjDir (CustomTool p) = pjCustomDir p
pjDir (NixTool p)    = dropFileName (pjFlakeFile p)
pjDir (MakeTool p)   = dropFileName (pjMakeFile p)

pjFile :: ProjectKey -> Maybe FilePath
pjFile (CabalTool p)  = Just $ pjCabalFile p
pjFile (StackTool p)  = Just $ pjStackFile p
pjFile (NixTool p)    = Just $ pjFlakeFile p
pjFile (MakeTool p)   = Just $ pjMakeFile p
pjFile _ = Nothing

pjFileOrDir :: ProjectKey -> FilePath
pjFileOrDir (CabalTool p)  = pjCabalFile p
pjFileOrDir (StackTool p)  = pjStackFile p
pjFileOrDir (CustomTool p) = pjCustomDir p
pjFileOrDir (NixTool p)    = pjFlakeFile p
pjFileOrDir (MakeTool p)   = pjMakeFile p

pjIsCabal :: ProjectKey -> Bool
pjIsCabal (CabalTool _) = True
pjIsCabal _ = False

pjIsStack :: ProjectKey -> Bool
pjIsStack (StackTool _) = True
pjIsStack _ = False

pjIsNix :: ProjectKey -> Bool
pjIsNix (NixTool _) = True
pjIsNix _ = False

filePathToProjectKey :: FilePath -> Maybe ProjectKey
filePathToProjectKey filePath =
  case takeFileName filePath of
    "flake.nix"   -> Just (NixTool (NixProject filePath))
    "Makefile"    -> Just (MakeTool (MakeProject filePath))
    "makefile"    -> Just (MakeTool (MakeProject filePath))
    "GNUmakefile" -> Just (MakeTool (MakeProject filePath))
    _ -> case takeExtension filePath of
      ".project" -> Just (CabalTool (CabalProject filePath))
      ".yaml" -> Just (StackTool (StackProject filePath))
      _ -> Nothing

