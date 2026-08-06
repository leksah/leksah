{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: Apache-2.0

-- | The registry of project types, in detection-priority order, and the
-- entry points that dispatch over it.
module IDE.Ws.Registry
  ( projectTypes
  , detectProject
  , typeById
  , enumerateProject
  ) where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

import IDE.Ws.Cabal (cabalProjectType)
import IDE.Ws.Cargo (cargoProjectType)
import IDE.Ws.Dir (dirProjectType)
import IDE.Ws.Make (makeProjectType)
import IDE.Ws.NixFlake (nixFlakeProjectType)
import IDE.Ws.Stack (stackProjectType)
import IDE.Ws.Types

-- | All project types, in priority order: the first one whose 'ptDetect'
-- claims a directory wins.
projectTypes :: [ProjectType]
projectTypes =
  [ cabalProjectType
  , stackProjectType
  , cargoProjectType
  , nixFlakeProjectType
  , makeProjectType
  , dirProjectType
  ]

-- | Try each project type against a directory; first claim wins.
detectProject :: Effects -> FilePath -> IO (Maybe ProjectKey)
detectProject eff dir = go projectTypes
 where
  go [] = pure Nothing
  go (pt:pts) = do
    m <- ptDetect pt eff dir
    case m of
      Just key -> pure (Just key)
      Nothing  -> go pts

-- | Look a project type up by its 'ptId'.
typeById :: Text -> Maybe ProjectType
typeById tid = find ((== tid) . ptId) projectTypes

-- | Enumerate a project by dispatching on its key's type id.
enumerateProject :: Effects -> ProjectKey -> IO (Either Text Project)
enumerateProject eff key = case typeById (pkType key) of
  Nothing -> pure (Left ("unknown project type: " <> pkType key))
  Just pt -> ptEnumerate pt eff key
