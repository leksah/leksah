-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | File-system utilities and the IDE's config-dir conventions.  A fresh
-- replacement for the subset of leksah-server's @IDE.Utils.FileUtils@ (and
-- its filename constants) the web UI still uses.
module IDE.Utils.Files
  ( -- * Config dir (@~/.leksah-VERSION/@)
    leksahVersion
  , configDirName
  , getConfigDir
  , getConfigFilePathForSave
  , getConfigFilePathForLoad
  , mbGetConfigFilePathForLoad
    -- * Filename conventions
  , leksahTemplateFileExtension
    -- * Path helpers
  , myCanonicalizePath
  , isSubPath
    -- * The cached nix project environments (@~/.leksah-VERSION/nix.cache@)
  , loadNixCache
  , saveNixCache
  , loadNixEnv
  , nixShellFile
    -- * cabal dist-newstyle layout
  , cabalBuildDir
  , cabalProjectBuildDir
  , findCabalProjectRoot
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:?), FromJSON(..), withObject)
import qualified Data.Aeson as A
import Data.Bifunctor (second)
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
       (canonicalizePath, createDirectoryIfMissing,
        doesFileExist, doesPathExist, getHomeDirectory)
import System.FilePath ((</>), isDrive, normalise, splitPath, takeDirectory)
import Text.Read (readMaybe)

import IDE.Utils.Process (ToolOutput(..))
import IDE.Utils.Project (ProjectKey, pjDir)

-- | The version part of the config dir name.  Bumped when the on-disk
-- layout changes incompatibly.
leksahVersion :: String
leksahVersion = "0.17"

configDirName :: String
configDirName = ".leksah-" <> leksahVersion

-- | @~/.leksah-VERSION/@, created if missing.
getConfigDir :: IO FilePath
getConfigDir = do
    home <- getHomeDirectory
    let d = home </> configDirName
    createDirectoryIfMissing False d
    return d

-- | Where to write a config file: always the config dir.
getConfigFilePathForSave :: FilePath -> IO FilePath
getConfigFilePathForSave fn = (</> fn) <$> getConfigDir

-- | Where to read a config file from: the explicitly given directory if any,
-- else the user's config dir, falling back to the shipped default under
-- @DATADIR/data/@.  Errors out when it is nowhere (the shipped defaults are
-- part of the install).
getConfigFilePathForLoad :: FilePath -> Maybe FilePath -> FilePath -> IO FilePath
getConfigFilePathForLoad fn mbDir dataDir =
    mbGetConfigFilePathForLoad fn mbDir dataDir >>= \case
        Just fp -> return fp
        Nothing -> error $ "Config file not found: " <> fn

-- | 'getConfigFilePathForLoad' returning 'Nothing' instead of erroring.
mbGetConfigFilePathForLoad :: FilePath -> Maybe FilePath -> FilePath -> IO (Maybe FilePath)
mbGetConfigFilePathForLoad fn mbDir dataDir = do
    home <- getHomeDirectory
    let candidates = maybe [] (\d -> [d </> fn]) mbDir
            <> [home </> configDirName </> fn, dataDir </> "data" </> fn]
    firstExisting candidates
  where
    firstExisting [] = return Nothing
    firstExisting (c:cs) = doesFileExist c >>= \case
        True  -> return (Just c)
        False -> firstExisting cs

leksahTemplateFileExtension :: FilePath
leksahTemplateFileExtension = ".lksht"

-- | 'canonicalizePath' that leaves paths that do not (yet) exist untouched
-- instead of throwing.
myCanonicalizePath :: FilePath -> IO FilePath
myCanonicalizePath fp = doesPathExist fp >>= \case
    True  -> canonicalizePath fp
    False -> return fp

-- | Is the second path inside the first (by path components, no IO)?
isSubPath :: FilePath -> FilePath -> Bool
isSubPath parent child =
    splitPath (normalise parent) `isPrefixOf` splitPath (normalise child)

-- | The nix environment cache: for each (project dir, compiler) the captured
-- environment of its nix shell.  Stored with 'show'/'readMaybe' — the format
-- predates this module and existing caches must keep loading.
nixCacheFile :: IO FilePath
nixCacheFile = (</> "nix.cache") <$> getConfigDir

loadNixCache :: MonadIO m => m (Map (FilePath, Text) (Map String String))
loadNixCache = liftIO $ do
    fp <- nixCacheFile
    doesFileExist fp >>= \case
        True  -> fromMaybe mempty . readMaybe . T.unpack <$> T.readFile fp
        False -> return mempty

-- | Capture a @env@-style dump (NAME=value lines) from a nix shell into the
-- cache and return it.  Skips shell-special and build-temp variables, which
-- point at directories that no longer exist when the cached env is reused.
saveNixCache :: MonadIO m => ProjectKey -> Text -> [ToolOutput] -> m (Map String String)
saveNixCache project compiler out = liftIO $ do
    let keep name = not (null name)
            && all (\c -> isAlphaNum c || c == '_') name
            && name `notElem` ["POSIXLY_CORRECT", "SHELLOPTS", "BASHOPTS", "TMP", "TMPDIR"]
        unquote ("IFS", _) = ("IFS", " \t\n")
        unquote (n, '\'':rest) =
            (n, maybe rest T.unpack . T.stripSuffix "'" $ T.pack rest)
        unquote x = x
        newEnv = M.fromList
            [ unquote . second (drop 1) $ span (/= '=') (T.unpack line)
            | ToolOutput line <- out
            , keep (takeWhile (/= '=') (T.unpack line)) ]
    fp <- nixCacheFile
    cache <- M.insert (pjDir project, compiler) newEnv <$> loadNixCache
    T.writeFile fp . T.pack $ show cache
    return newEnv

-- | The cached nix environment for a project and compiler, if captured.
loadNixEnv :: MonadIO m => ProjectKey -> Text -> m (Maybe (Map String String))
loadNixEnv project compiler = M.lookup (pjDir project, compiler) <$> loadNixCache

-- | The project's nix shell entry file (@shell.nix@ preferred over
-- @default.nix@), if it has one.
nixShellFile :: MonadIO m => ProjectKey -> m (Maybe FilePath)
nixShellFile project = liftIO $ do
    let dir = pjDir project
    doesFileExist (dir </> "shell.nix") >>= \case
        True  -> return $ Just (dir </> "shell.nix")
        False -> doesFileExist (dir </> "default.nix") >>= \case
            True  -> return $ Just (dir </> "default.nix")
            False -> return Nothing

-- | The build dir cabal uses (always the default: leksah's own in-IDE builds
-- must agree with shell builds — see the incremental-build invariant).
cabalBuildDir :: Maybe String -> FilePath
cabalBuildDir _ = "dist-newstyle"

-- | Just enough of @plan.json@ to locate built components.
data Plan = Plan
    { planCabalVersion :: Maybe String
    , planCompilerId   :: Maybe String
    , planOS           :: Maybe String
    , planArch         :: Maybe String
    }

instance FromJSON Plan where
    parseJSON = withObject "plan.json" $ \o -> Plan
        <$> o .:? "cabal-version"
        <*> o .:? "compiler-id"
        <*> o .:? "os"
        <*> o .:? "arch"

-- | Where a cabal project's build products live, and how a component's
-- build subdir is laid out inside its package dir: returns
-- @(buildRoot, \\ctype component -> relative build path, cabal version)@.
-- Layout follows the project's @plan.json@; without one, the pre-nix-style
-- flat @dist-newstyle\/build@ is assumed.
cabalProjectBuildDir :: FilePath -> FilePath -> IO (FilePath, FilePath -> FilePath -> FilePath, Maybe FilePath)
cabalProjectBuildDir projectRoot buildDir = do
    let distNewstyle = projectRoot </> buildDir
        planFile = distNewstyle </> "cache" </> "plan.json"
        fallback = (distNewstyle </> "build", \_ _ -> "build", Nothing)
    (doesFileExist planFile >>= \case
        False -> return fallback
        True -> A.decodeFileStrict planFile >>= \case
            Just Plan { planCabalVersion = v
                      , planCompilerId = Just compilerId
                      , planOS = Just os
                      , planArch = Just arch }
                | maybe False ("1.24." `isPrefixOf`) v -> return fallback
                | maybe False ("2.0." `isPrefixOf`) v ->
                    return ( distNewstyle </> "build" </> arch <> "-" <> os </> compilerId
                           , \_ component -> "c" </> component </> "build", v )
                | otherwise ->
                    return ( distNewstyle </> "build" </> arch <> "-" <> os </> compilerId
                           , \ctype component -> ctype </> component </> "build", v )
            _ -> return fallback)
        `catch` \(_ :: SomeException) -> return fallback

-- | The cabal project root for a directory: the nearest ancestor holding a
-- @cabal.project@, stopping (and falling back to the directory itself) at
-- the user's home or the filesystem root.
findCabalProjectRoot :: FilePath -> IO FilePath
findCabalProjectRoot curdir = do
    home <- getHomeDirectory
    let probe dir
          | isDrive dir || dir == home = return curdir
          | otherwise = doesFileExist (dir </> "cabal.project") >>= \case
              True  -> return dir
              False -> probe (takeDirectory dir)
    probe curdir
