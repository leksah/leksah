{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: Apache-2.0

-- | The plain-directory project type: the catch-all that claims any
-- directory (lowest priority in the registry).  Enumeration yields a
-- single component-less package named after the directory; there are no
-- commands.
module IDE.Ws.Dir
  ( dirProjectType
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeFileName)

import IDE.Ws.Types

-- | The @dir@ project type.
dirProjectType :: ProjectType
dirProjectType = ProjectType
  { ptId = "dir"
  , ptDetect = detect
  , ptEnumerate = enumerate
  , ptCommand = \_ _ -> Nothing
  }

-- The catch-all claims any DIRECTORY.  It must not claim a plain file:
-- a project rooted at a file breaks every consumer that lists the root.
detect :: Effects -> FilePath -> IO (Maybe ProjectKey)
detect eff dir = do
  has <- eIsDir eff dir
  pure $ if has then Just (ProjectKey "dir" dir Nothing) else Nothing

enumerate :: Effects -> ProjectKey -> IO (Either Text Project)
enumerate _ key = pure . Right $ Project key
  [ Package
      { pkgName = T.pack (takeFileName (pkRoot key))
      , pkgVersion = ""
      , pkgManifest = pkRoot key
      , pkgDir = pkRoot key
      , pkgComponents = []
      , pkgSrcDirs = []
      }
  ]
