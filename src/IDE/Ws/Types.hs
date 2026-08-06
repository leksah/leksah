{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: Apache-2.0

-- | Core types of the leksah project model.
--
-- A 'Project' lives at a root directory, was claimed by one 'ProjectType'
-- (cabal, stack, cargo, nix-flake, make or plain dir), and enumerates to a
-- list of 'Package's with typed 'Component's.  All file-system and process
-- access needed for detection and enumeration goes through an 'Effects'
-- record, so the model is pure-testable and can later be run against a
-- remote (e.g. ssh) file system.
module IDE.Ws.Types
  ( ProjectKey(..)
  , ComponentKind(..)
  , Component(..)
  , Package(..)
  , Project(..)
  , Verb(..)
  , Scope(..)
  , ToolCmd(..)
  , Effects(..)
  , ProjectType(..)
  , defaultEffects
  , verbId
  , kindId
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try, evaluate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesPathExist, listDirectory)
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.Process
  ( CreateProcess(..), StdStream(..), proc, createProcess, waitForProcess )

-- | Identifies a project in a workspace: its type id (e.g. @\"cabal\"@),
-- its root directory, and — when the project is described by a specific
-- file (@cabal.project@, @stack.yaml@, …) — that file.
data ProjectKey = ProjectKey
  { pkType :: Text
  , pkRoot :: FilePath
  , pkFile :: Maybe FilePath
  } deriving (Eq, Ord, Show)

-- | The kind of a buildable component inside a package.
data ComponentKind = KLib | KExe | KTest | KBench | KOther
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A named component of a 'Package'.  For a main library the name is the
-- package name; sublibraries carry their own names.
data Component = Component
  { cKind :: ComponentKind
  , cName :: Text
  } deriving (Eq, Ord, Show)

-- | A package found while enumerating a project.
data Package = Package
  { pkgName       :: Text
  , pkgVersion    :: Text
  , pkgManifest   :: FilePath   -- ^ the package's manifest file (absolute)
  , pkgDir        :: FilePath   -- ^ the directory containing the manifest
  , pkgComponents :: [Component]
  , pkgSrcDirs    :: [FilePath] -- ^ source dirs, relative to 'pkgDir'
  } deriving (Eq, Show)

-- | A project: its key plus the packages enumeration found.
data Project = Project
  { prKey      :: ProjectKey
  , prPackages :: [Package]
  } deriving (Eq, Show)

-- | The user-level actions a project type can translate to shell commands.
data Verb = VBuild | VRun | VTest | VBench | VClean | VRepl
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | What a 'Verb' should apply to.
data Scope
  = ScopeProject
  | ScopePackage Package
  | ScopeComponent Package Component
  deriving (Show)

-- | A shell command: program, arguments, and the directory to run it in.
-- 'tcDir' is interpreted relative to the project root (so @\".\"@ means the
-- root itself), keeping 'ProjectType' values context free.
data ToolCmd = ToolCmd
  { tcProgram :: Text
  , tcArgs    :: [Text]
  , tcDir     :: FilePath
  } deriving (Eq, Show)

-- | All file-system and process access used by detection/enumeration.
data Effects = Effects
  { eReadFile  :: FilePath -> IO (Maybe ByteString)
    -- ^ file contents, or 'Nothing' if unreadable
  , eListDir   :: FilePath -> IO [FilePath]
    -- ^ entry /names/ (not paths); @[]@ if not a listable directory
  , eDoesExist :: FilePath -> IO Bool
  , eRunTool   :: FilePath -> Text -> [Text] -> IO (Maybe ByteString)
    -- ^ @eRunTool cwd program args@: stdout iff the tool exits 0
  }

-- | One project flavour: how to claim a directory, how to enumerate its
-- packages, and how to render commands for it.
data ProjectType = ProjectType
  { ptId        :: Text
  , ptDetect    :: Effects -> FilePath -> IO (Maybe ProjectKey)
    -- ^ claim a dir (setting 'pkFile' when a specific file describes it)
  , ptEnumerate :: Effects -> ProjectKey -> IO (Either Text Project)
  , ptCommand   :: Verb -> Scope -> Maybe ToolCmd
  }

-- | The canonical short name of a 'Verb' (used e.g. as JSON keys).
verbId :: Verb -> Text
verbId VBuild = "build"
verbId VRun   = "run"
verbId VTest  = "test"
verbId VBench = "bench"
verbId VClean = "clean"
verbId VRepl  = "repl"

-- | The canonical short name of a 'ComponentKind'.
kindId :: ComponentKind -> Text
kindId KLib   = "lib"
kindId KExe   = "exe"
kindId KTest  = "test"
kindId KBench = "bench"
kindId KOther = "other"

-- | The real-IO implementation of 'Effects'.  Every operation catches
-- exceptions (missing files, unlaunchable tools, …) and returns the
-- \"absent\" value instead of throwing.
defaultEffects :: Effects
defaultEffects = Effects
  { eReadFile = \fp -> hush <$> try (B.readFile fp)
  , eListDir = \fp -> either (const []) id
      <$> (try (listDirectory fp) :: IO (Either SomeException [FilePath]))
  , eDoesExist = \fp -> either (const False) id
      <$> (try (doesPathExist fp) :: IO (Either SomeException Bool))
  , eRunTool = \cwdDir prog args -> hush <$> try (runTool cwdDir prog args)
  }
 where
  hush :: Either SomeException a -> Maybe a
  hush = either (const Nothing) Just

  runTool :: FilePath -> Text -> [Text] -> IO ByteString
  runTool cwdDir prog args = do
    (_, Just outH, Just errH, ph) <- createProcess
      (proc (T.unpack prog) (map T.unpack args))
        { cwd = Just cwdDir
        , std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    errDone <- newEmptyMVar
    _ <- forkIO $ do
      _ <- (try (B.hGetContents errH >>= evaluate)
              :: IO (Either SomeException ByteString))
      putMVar errDone ()
    out <- B.hGetContents outH >>= evaluate
    takeMVar errDone
    hClose outH
    hClose errH
    ec <- waitForProcess ph
    case ec of
      ExitSuccess -> pure out
      ExitFailure c -> ioError (userError ("exit " <> show c))
