{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: Apache-2.0

-- | The make project type.
--
-- Root marker: a @Makefile@ (also accepts the classic @makefile@ and
-- @GNUmakefile@ spellings; 'pkFile' records the one found).  Enumeration
-- yields a single component-less package named after the root directory.
-- Commands: @make@, @make test@, @make clean@.
module IDE.Ws.Make
  ( makeProjectType
  ) where

import Control.Monad (filterM)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>), takeFileName)

import IDE.Ws.Types

-- | The @make@ project type.
makeProjectType :: ProjectType
makeProjectType = ProjectType
  { ptId = "make"
  , ptDetect = detect
  , ptEnumerate = enumerate
  , ptCommand = command
  }

detect :: Effects -> FilePath -> IO (Maybe ProjectKey)
detect eff dir = do
  found <- filterM (eDoesExist eff . (dir </>))
    ["Makefile", "makefile", "GNUmakefile"]
  pure $ case found of
    (f:_) -> Just (ProjectKey "make" dir (Just (dir </> f)))
    []    -> Nothing

enumerate :: Effects -> ProjectKey -> IO (Either Text Project)
enumerate _ key = pure . Right $ Project key
  [ Package
      { pkgName = T.pack (takeFileName (pkRoot key))
      , pkgVersion = ""
      , pkgManifest =
          maybe (pkRoot key </> "Makefile") id (pkFile key)
      , pkgDir = pkRoot key
      , pkgComponents = []
      , pkgSrcDirs = []
      }
  ]

command :: Verb -> Scope -> Maybe ToolCmd
command verb _ = case verb of
  VBuild -> Just (ToolCmd "make" [] ".")
  VTest  -> Just (ToolCmd "make" ["test"] ".")
  VClean -> Just (ToolCmd "make" ["clean"] ".")
  _      -> Nothing
