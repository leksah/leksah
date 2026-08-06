{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: Apache-2.0

-- | The nix-flake project type.
--
-- Root marker: @flake.nix@ ('pkFile').  Enumeration yields a single
-- package named after the root directory; flake outputs are not
-- enumerated as components (empty list).  Commands: @nix build@,
-- @nix run@; no clean.
module IDE.Ws.NixFlake
  ( nixFlakeProjectType
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>), takeFileName)

import IDE.Ws.Types

-- | The @nix-flake@ project type.
nixFlakeProjectType :: ProjectType
nixFlakeProjectType = ProjectType
  { ptId = "nix-flake"
  , ptDetect = detect
  , ptEnumerate = enumerate
  , ptCommand = command
  }

detect :: Effects -> FilePath -> IO (Maybe ProjectKey)
detect eff dir = do
  let flake = dir </> "flake.nix"
  has <- eDoesExist eff flake
  pure $
    if has then Just (ProjectKey "nix-flake" dir (Just flake)) else Nothing

enumerate :: Effects -> ProjectKey -> IO (Either Text Project)
enumerate _ key = pure . Right $ Project key
  [ Package
      { pkgName = T.pack (takeFileName (pkRoot key))
      , pkgVersion = ""
      , pkgManifest = pkRoot key </> "flake.nix"
      , pkgDir = pkRoot key
      , pkgComponents = []
      , pkgSrcDirs = []
      }
  ]

command :: Verb -> Scope -> Maybe ToolCmd
command verb _ = case verb of
  VBuild -> Just (ToolCmd "nix" ["build"] ".")
  VRun   -> Just (ToolCmd "nix" ["run"] ".")
  _      -> Nothing
