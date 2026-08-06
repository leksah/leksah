{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: Apache-2.0

-- | Reading and writing the workspace file, @\<name\>.leksah.json@.
--
-- The JSON layout (keys written sorted; unknown keys are ignored on read,
-- missing optional keys default):
--
-- > { "version": 1
-- > , "name": "my workspace"
-- > , "projects":
-- >     [ { "type": "cabal"
-- >       , "root": "proj"                 -- relative to the ws file's dir
-- >       , "file": "proj/cabal.project"   -- optional
-- >       , "commandOverrides":            -- optional, absent = none
-- >           { "build": { "program": "just", "args": ["build"] } }
-- >       } ]
-- > , "active":                            -- optional
-- >     { "project": "proj"                -- the project's root
-- >     , "package": "proj/foo.cabal"      -- optional, manifest path
-- >     , "component": { "kind": "exe", "name": "foo" }  -- optional
-- >     } }
--
-- All paths are stored relative to the workspace file's directory with
-- POSIX separators; reading resolves them back to absolute paths, so an
-- in-memory 'Workspace' always carries absolute paths.
module IDE.Ws.File
  ( Workspace(..)
  , WsProject(..)
  , WsActive(..)
  , ToolOverride(..)
  , readWorkspaceFile
  , writeWorkspaceFile
  , applyOverrides
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty')
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath
  ( (</>), isAbsolute, isPathSeparator, joinPath, normalise
  , splitDirectories, takeDirectory )

import IDE.Ws.Types

-- | A command override stored in the workspace file: replaces the
-- program and arguments a project type would use for a 'Verb'.
data ToolOverride = ToolOverride
  { toProgram :: Text
  , toArgs    :: [Text]
  } deriving (Eq, Show)

-- | One project entry of a workspace.  Paths are absolute in memory.
data WsProject = WsProject
  { wpType      :: Text
  , wpRoot      :: FilePath
  , wpFile      :: Maybe FilePath
  , wpOverrides :: Map Verb ToolOverride
  , wpCmdPrefix :: Maybe Text
    -- ^ words prepended to every command run for this project (e.g. an
    -- @ssh host@ prefix for a remote root, or a @nix develop -c@ wrapper)
  } deriving (Eq, Show)

-- | What is currently \"active\" in the UI: a project (by its root), and
-- optionally a package (by its manifest) and component within it.
data WsActive = WsActive
  { waProject   :: FilePath
  , waPackage   :: Maybe FilePath
  , waComponent :: Maybe Component
  } deriving (Eq, Show)

-- | The workspace: a name plus its projects.
data Workspace = Workspace
  { wsName     :: Text
  , wsProjects :: [WsProject]
  , wsActive   :: Maybe WsActive
  } deriving (Eq, Show)

-- Reading -----------------------------------------------------------------

-- | Read a workspace file; relative paths are resolved against the file's
-- directory.
readWorkspaceFile :: FilePath -> IO (Either Text Workspace)
readWorkspaceFile path = do
  ebs <- try (B.readFile path) :: IO (Either SomeException B.ByteString)
  pure $ case ebs of
    Left e -> Left (T.pack (show e))
    Right bs -> case eitherDecodeStrict' bs of
      Left err -> Left (T.pack err)
      Right v -> case parseEither (parseWorkspace base) v of
        Left err -> Left (T.pack err)
        Right ws -> Right ws
 where
  base = takeDirectory path

parseWorkspace :: FilePath -> Value -> Parser Workspace
parseWorkspace base = withObject "workspace" $ \o -> Workspace
  <$> o .:? "name" .!= ""
  <*> (o .:? "projects" .!= [] >>= mapM (parseProject base))
  <*> (o .:? "active" >>= traverse (parseActive base))

parseProject :: FilePath -> Value -> Parser WsProject
parseProject base = withObject "project" $ \o -> WsProject
  <$> o .: "type"
  <*> (resolve base <$> o .: "root")
  <*> (fmap (resolve base) <$> o .:? "file")
  <*> (o .:? "commandOverrides" .!= KM.empty >>= parseOverrides)
  <*> o .:? "commandPrefix"
 where
  parseOverrides km = Map.fromList . catMaybes
    <$> mapM parseOne (KM.toList km)
  parseOne (k, v) = case verbFromText (Key.toText k) of
    Nothing -> pure Nothing        -- unknown verb: ignore
    Just verb -> do
      ov <- withObject "override"
        (\o -> ToolOverride <$> o .: "program" <*> o .:? "args" .!= []) v
      pure (Just (verb, ov))

parseActive :: FilePath -> Value -> Parser WsActive
parseActive base = withObject "active" $ \o -> WsActive
  <$> (resolve base <$> o .: "project")
  <*> (fmap (resolve base) <$> o .:? "package")
  <*> (o .:? "component" >>= traverse parseComponent)
 where
  parseComponent = withObject "component" $ \o -> Component
    <$> (kindFromText <$> o .: "kind")
    <*> o .: "name"

-- Writing -----------------------------------------------------------------

-- | Write a workspace file (pretty, keys sorted); absolute paths are
-- stored relative to the file's directory with POSIX separators.
writeWorkspaceFile :: FilePath -> Workspace -> IO ()
writeWorkspaceFile path ws =
  BL.writeFile path
    (encodePretty' defConfig { confCompare = compare }
      (workspaceValue (takeDirectory path) ws) <> "\n")

workspaceValue :: FilePath -> Workspace -> Value
workspaceValue base ws = obj $
  [ ("version", Number 1)
  , ("name", String (wsName ws))
  , ("projects", toJSON (map (projectValue base) (wsProjects ws)))
  ] ++
  [ ("active", activeValue base a) | Just a <- [wsActive ws] ]

projectValue :: FilePath -> WsProject -> Value
projectValue base p = obj $
  [ ("type", String (wpType p))
  , ("root", relPath base (wpRoot p))
  ] ++
  [ ("file", relPath base f) | Just f <- [wpFile p] ] ++
  [ ("commandPrefix", String pre) | Just pre <- [wpCmdPrefix p] ] ++
  [ ("commandOverrides", overridesValue (wpOverrides p))
  | not (Map.null (wpOverrides p)) ]
 where
  overridesValue m = obj
    [ ( Key.fromText (verbId v)
      , obj [ ("program", String (toProgram o))
            , ("args", toJSON (toArgs o)) ] )
    | (v, o) <- Map.toList m ]

activeValue :: FilePath -> WsActive -> Value
activeValue base a = obj $
  [ ("project", relPath base (waProject a)) ] ++
  [ ("package", relPath base p) | Just p <- [waPackage a] ] ++
  [ ( "component"
    , obj [ ("kind", String (kindId (cKind c)))
          , ("name", String (cName c)) ] )
  | Just c <- [waComponent a] ]

obj :: [(Key.Key, Value)] -> Value
obj = Object . KM.fromList

-- Overrides ----------------------------------------------------------------

-- | Apply a project's command override for a verb to the command the
-- project type produced: the override replaces program and args but keeps
-- the directory.  When the project type had no command at all, an override
-- still yields one, running at the project root (@\".\"@).
applyOverrides :: WsProject -> Verb -> Maybe ToolCmd -> Maybe ToolCmd
applyOverrides p verb base = case Map.lookup verb (wpOverrides p) of
  Nothing -> base
  Just o -> Just $ case base of
    Just tc -> tc { tcProgram = toProgram o, tcArgs = toArgs o }
    Nothing -> ToolCmd (toProgram o) (toArgs o) "."

-- Path helpers ---------------------------------------------------------------

verbFromText :: Text -> Maybe Verb
verbFromText t = lookup t [(verbId v, v) | v <- [minBound .. maxBound]]

kindFromText :: Text -> ComponentKind
kindFromText t = maybe KOther id
  (lookup t [(kindId k, k) | k <- [minBound .. maxBound]])

-- | Render an absolute path relative to @base@, POSIX separators.
-- An already-relative path is kept (POSIX-ised) as is.
relPath :: FilePath -> FilePath -> Value
relPath base p = String (T.pack (toPosix (makeRel base p)))
 where
  toPosix = map (\c -> if isPathSeparator c then '/' else c)

makeRel :: FilePath -> FilePath -> FilePath
makeRel base p
  | not (isAbsolute p) = p
  | otherwise =
      let bs = splitDirectories (collapse base)
          ps = splitDirectories (collapse p)
          (bs', ps') = dropCommon bs ps
      in case replicate (length bs') ".." ++ ps' of
           [] -> "."
           segs -> intercalate "/" segs
 where
  dropCommon (x:xs) (y:ys) | x == y = dropCommon xs ys
  dropCommon xs ys = (xs, ys)

-- | Resolve a stored (usually relative, POSIX) path against @base@.
resolve :: FilePath -> FilePath -> FilePath
resolve base p
  | isAbsolute p = collapse p
  | otherwise    = collapse (base </> p)

-- | Normalise a path, collapsing @.@ and @..@ segments textually.
collapse :: FilePath -> FilePath
collapse = joinPath . go [] . splitDirectories . normalise
 where
  go acc [] = reverse acc
  go acc (".":rest) = go acc rest
  go (a:acc) ("..":rest)
    | a /= "/" && a /= ".." = go acc rest
  go acc (x:rest) = go (x:acc) rest
