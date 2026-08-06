-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The IDE's state and the monad it runs in.
--
-- One 'IDE' record holds everything the whole application shares — the
-- preferences, the open workspace, the diagnostics, the per-OS-window UI
-- state — behind an 'MVar' ('IDERef') that every thread reads and mutates
-- through 'IDEM'.  The domain models it composes live in their own modules
-- ('IDE.Settings', 'IDE.Project', 'IDE.Web.Model',
-- 'IDE.Diagnostics.Model'); this module re-exports them so a consumer needs
-- one import.
module IDE.Core.Types (
    IDE(..)
,   IDEGtk
,   activeProject
,   activePack
,   activeComponent
,   activeProjectLogRefs
,   nixEnv
,   IDEState(..)
,   IDERef
,   IDEM
,   IDEEventM
,   IDEAction
,   MonadIDE
,   liftIDE
,   (?>>=)

,   WorkspaceM
,   WorkspaceAction
,   runWorkspace

,   ProjectM
,   ProjectAction
,   runProject

,   PackageM
,   PackageAction
,   runPackage


,   module IDE.Project
,   ProjectKey(..)
,   CabalProject(..)
,   StackProject(..)
,   CustomProject(..)
,   NixProject(..)
,   MakeProject(..)
,   pjDir
,   pjFile
,   pjFileOrDir
,   pjIsCabal
,   pjIsStack
,   pjIsNix
,   filePathToProjectKey

,   ActionDescr(..)
,   ActionString
,   KeyString

,   module IDE.Settings
,   module IDE.Web.Model

,   module IDE.Diagnostics.Model

,   SearchHint(..)
,   KeymapI(..)
#if defined(ghcjs_HOST_OS)
    -- Stand-ins for packages that don't build on the JS backend (see their
    -- definitions below); natively the real ones come from fsnotify.
,   WatchManager(..)
,   StopListening
#endif


,   LogLaunchData(..)
,   SearchMode(..)

-- IDE
,   ideGtk
,   exitCode
,   prefs
,   workspace
,   bufferProjCache
,   allLogRefs
,   currentEBC
,   currentHist
,   currentState
,   recentFiles
,   recentWorkspaces
,   runningTool
,   hlintQueue
,   logLaunches
,   autoCommand
,   autoURI
,   triggerBuild
,   fsnotify
,   watchers
,   developLeksah
,   nixCache
,   externalModified
,   jsContexts
,   logLineMap
,   webWindows
,   leksahWindows
,   nextLeksahWin
,   hiddenWindows
,   activeWindow
,   nextWindowId
,   flipMirror
,   flipMru
,   paneAISession
,   ideVersion

,   __
) where

import Prelude ()
import Prelude.Compat
import Distribution.Package
       (unPackageName, PackageIdentifier(..), Dependency(..))
import Distribution.PackageDescription (BuildInfo)
import Data.Map (Map)
import Data.Set (Set)
import Data.List (find, nubBy, isPrefixOf)
import Data.Maybe (fromMaybe)
import IDE.Utils.RemotePath
       (isRemotePath, parseRemotePath, remoteMakeRelative, renderRemotePath)
import Control.Concurrent (MVar)
import Distribution.ModuleName (ModuleName)
import Distribution.Simple (Extension(..))
import IDE.Utils.Process (ProcessHandle)
import Data.IORef (IORef)
import System.FilePath
       (dropFileName, (</>), isAbsolute, makeRelative, equalFilePath,
        addTrailingPathSeparator)
import IDE.Core.Location
import IDE.Diagnostics.Model
import IDE.Web.Model
import IDE.Settings
import IDE.Project
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT(..))

import qualified Data.Map as Map (Map)
import Control.Monad.Reader.Class (MonadReader(..))
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Function (on)
import Control.Concurrent.STM.TVar (TVar)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (filter)
import Control.Monad ((>=>))
#if !defined(ghcjs_HOST_OS)
import System.FSNotify (StopListening, WatchManager)
#endif
import qualified Data.Map as M (fromList, lookup, elems)
import System.Exit (ExitCode)
import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)
import Data.Aeson.Types
       (genericParseJSON, genericToEncoding, genericToJSON,
        defaultOptions, fieldLabelModifier, omitNothingFields, Options)
import Language.Javascript.JSaddle (JSContextRef)
import Control.Lens (makeLenses, (^.), Getter, to, view)
import IDE.Gtk.Types
       (IDEState(..), IDEGtk, Color(..), PanePath,
       LogLaunchData(..),
       ActionString, KeyString, ActionDescr(..))

#ifdef LOCALIZATION

import Text.I18N.GetText
import System.IO.Unsafe (unsafePerformIO)

#endif

import IDE.Utils.Project
       (ProjectKey(..), pjCabalFile, pjStackFile, pjCustomDir, pjDir, pjFlakeFile,
        CabalProject(..), StackProject(..), CustomProject(..), NixProject(..),
        MakeProject(..),
        pjIsCabal, pjIsStack, pjIsNix, pjFileOrDir, pjFile, filePathToProjectKey)
import Distribution.Pretty (prettyShow)

-- ---------------------------------------------------------------------
-- The state
-- ---------------------------------------------------------------------

-- | Everything the application shares.  Fields are grouped by what owns
-- them: the front-end handles, the user's configuration and workspace, the
-- diagnostics, the running tool, the file watchers, and the web UI's window
-- state.  Lenses (below) are how it is read and written — never the fields
-- directly, so a change bumps '_ideVersion' and the windows notice.
data IDE = IDE {
    _ideGtk              :: Maybe (IDEGtk IDEM IDERef)
,   _exitCode            :: IORef ExitCode
,   _prefs               :: Prefs                   -- ^ configuration preferences
,   _workspace           :: Maybe Workspace         -- ^ may be a workspace (set of packages)
,   _bufferProjCache     :: Map FilePath [(Project, IDEPackage)] -- ^ cache the associated packages for a file
,   _allLogRefs          :: Seq LogRef
,   _currentEBC          :: (Maybe LogRef, Maybe LogRef, Maybe LogRef)
,   _currentHist         :: Int
,   _currentState        :: IDEState
,   _recentFiles         :: [FilePath]
,   _recentWorkspaces    :: [FilePath]
,   _runningTool         :: Maybe (ProcessHandle, IO ())
,   _hlintQueue          :: Maybe (TVar [Either FilePath FilePath])
,   _logLaunches         :: Map.Map Text LogLaunchData
,   _autoCommand         :: Maybe ((ProjectKey, FilePath), IDEAction)
,   _autoURI             :: Maybe Text
,   _triggerBuild        :: MVar ()
,   _fsnotify            :: WatchManager
,   _watchers            :: MVar (Map ProjectKey StopListening, Map FilePath StopListening)
,   _developLeksah       :: Bool -- If True leksah will exit when the `leksah` package is rebuilt
,   _nixCache            :: Map (FilePath, Text) (Map String String)
,   _externalModified    :: MVar (Set FilePath)
,   _jsContexts          :: [JSContextRef]
,   _logLineMap          :: Map Int (Text, LogTag)
,   _webWindows          :: Map WindowId WebWindow -- ^ per-OS-window state (multi-window web UI)
,   _leksahWindows       :: Map.Map Text LeksahWindow
                                                    -- ^ leksah window id -> that tab's native split
                                                    --   layout (tmux-window panes + native views,
                                                    --   per-pane font size).  Runtime truth; mirrored
                                                    --   to @leksah_layout on the backing tmux session
                                                    --   (see IDE.Web.SplitLayout), or to the web
                                                    --   session file when sessionless
,   _nextLeksahWin       :: Int                     -- ^ monotonic minter for 'LeksahWindow' ids
                                                    --   (@lw-N@); seeded at startup past every
                                                    --   persisted id, never reused
,   _hiddenWindows       :: Set (Text, Int)        -- ^ (tmux session id, window index) hidden from the
                                                    --   tab rows + flipper by the ⌘W menu's "Hide Window"
                                                    --   / "Move Pane to Hidden Window" — still alive in
                                                    --   tmux + shown in the Terminals tree; un-hidden when
                                                    --   the pane is next focused (tree/claude/editor)
,   _activeWindow        :: Maybe WindowId         -- ^ the frontmost OS window (native becomeKey)
,   _nextWindowId        :: Int                    -- ^ monotonic 'WindowId' minter
,   _flipMirror          :: Maybe (Int, [(Text, Int, Text)], Int)
                                                    -- ^ shared flipper-mirror state so every OS window
                                                    --   can draw the open flipper: @(ownerWindowId,
                                                    --   [(label, itemOwnerWinId)], selectedIndex)@;
                                                    --   'Nothing' = no flipper open.  Each window renders
                                                    --   its own mirror from this (via the MVar poll) —
                                                    --   never a cross-window JS broadcast (that deadlocks
                                                    --   the jsaddle-wkwebview main-thread bridge).
,   _flipMru             :: [FlipItem]              -- ^ THE flip MRU, shared by every OS window (single
                                                    --   source of truth for flipper order).  Bumped via
                                                    --   'modifyIDE_' on focus/click/open/flip-commit and
                                                    --   when a window becomes key; each window reads it
                                                    --   through its polled 'ideD'.
,   _paneAISession       :: Map AIPaneRef Text      -- ^ each pane's default AI session, as a Claude
                                                    --   session id (the only durable handle — pids and
                                                    --   tmux panes come and go).  Only EXPLICIT bindings
                                                    --   live here; the rest is derived on demand (a
                                                    --   Claude pane targets itself, a file's pane its
                                                    --   project's most recent session — see
                                                    --   'IDE.Web.Main.paneDefaultSession').  Entries are
                                                    --   dropped when the PANE goes, never when the
                                                    --   session exits: a closed default is resumed, so
                                                    --   it has to survive.  Persisted as @wsPaneAI@.
,   _ideVersion          :: Int                    -- ^ bumped on every 'modifyIDEM'; lets each web-UI
                                                    --   window poll the shared MVar and refresh its
                                                    --   'ideD' when the cross-window trigger fan-out
                                                    --   drops a fire to a background window
} -- deriving Show

-- ---------------------------------------------------------------------
-- The monad
-- ---------------------------------------------------------------------

-- | The shared state, plus the notification callback a mutation runs (that
-- is what makes every window redraw).
type IDERef = MVar (IDE -> IO (), IDE)

-- | Anything that can touch the IDE runs here.
type IDEM = ReaderT IDERef IO

-- | An 'IDEM' that returns nothing — most of the IDE's actions.
type IDEAction = IDEM ()


-- | A monad an 'IDEM' action can be run in: the tower below, and the
-- front ends' event monads.  'liftIDE' is the only thing it promises.
class (Applicative m, Monad m, MonadIO m) => MonadIDE m where
    liftIDE :: IDEM a -> m a

instance MonadIDE IDEM where
    liftIDE = id

instance MonadIDE WorkspaceM where
    liftIDE = lift

-- | Run the continuation only when the action produced something.
(?>>=) :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
a ?>>= b = a >>= \case
    Just v  -> b v
    Nothing -> return ()

-- | A widget-event handler that can also reach the IDE (the front ends'
-- @onIDE@ wraps a raw event handler in this).
type IDEEventM t = ReaderT IDERef (ReaderT t IO)

instance MonadIDE (IDEEventM t) where
    liftIDE f = do
        ideR <- ask
        liftIO $ runReaderT f ideR

-- ---------------------------------------------------------------------
-- Ambient context: an action that needs an open workspace, a project
-- within it, or one of that project's packages, reads it from the
-- environment instead of taking it as an argument.
-- ---------------------------------------------------------------------

type WorkspaceM = ReaderT Workspace IDEM
type WorkspaceAction = WorkspaceM ()

runWorkspace :: WorkspaceM a -> Workspace -> IDEM a
runWorkspace = runReaderT

type ProjectM = ReaderT Project WorkspaceM
type ProjectAction = ProjectM ()

instance MonadIDE ProjectM where
    liftIDE = lift . lift

runProject :: ProjectM a -> Project -> WorkspaceM a
runProject = runReaderT

type PackageM = ReaderT Package ProjectM
type PackageAction = PackageM ()

instance MonadIDE PackageM where
    liftIDE = lift . lift . lift

runPackage :: PackageM a -> Package -> ProjectM a
runPackage = runReaderT

-- ---------------------------------------------------------------------
-- Small shared vocabulary
-- ---------------------------------------------------------------------

-- | Which way (and from what) a find should search next.
data SearchHint = Forward | Backward | Insert | Delete | Initial
    deriving (Eq)

#if defined(ghcjs_HOST_OS)
-- | Stand-ins for fsnotify's types: fsnotify (via unix-compat) doesn't build
-- on the JS backend, and there is no file watching in a browser anyway.  The
-- '_fsnotify' / '_watchers' fields still exist; the JS branch of
-- 'IDE.Project.WorkspaceFile' registers only no-op watchers.
data WatchManager = NoWatchManager
type StopListening = IO ()
#endif

-- | A classic-era keymap: per action, the chord (or two-chord sequence)
-- bound to it and an optional display override.  The web UI's own
-- keybindings live in "IDE.Web.Keybindings".
newtype KeymapI = KM
    (Map ActionString [(Maybe (Either KeyString (KeyString, KeyString)), Maybe Text)])

-- | How a find matches: literally, as a prefix, or as a regex — each
-- either case-sensitively or not.
data SearchMode
    = Exact  { caseSense :: Bool }
    | Prefix { caseSense :: Bool }
    | Regex  { caseSense :: Bool }
    deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON SearchMode
instance FromJSON SearchMode


makeLenses ''IDE

-- ---------------------------------------------------------------------
-- Derived views of the state
-- ---------------------------------------------------------------------

-- | The active project, package and component: what the workspace says,
-- resolved through the open workspace (nothing without one).
activeProject :: Getter IDE (Maybe Project)
activeProject = workspace . to (>>= view wsActiveProject)

activePack :: Getter IDE (Maybe Package)
activePack = workspace . to (>>= view wsActivePackage)

activeComponent :: Getter IDE (Maybe Text)
activeComponent = workspace . to (>>= view wsActiveComponent)

-- | Log refs (errors/warnings/hints) that belong to the active project: those
-- whose root path is the active project's directory, or a package directory
-- under it.  With no active project, all refs.  Scopes the Errors pane and the
-- status-bar counts to the project you are working on, so another workspace
-- project's diagnostics — e.g. leksah's own Haskell LSP errors while you build
-- a Rust crate — don't pollute the count.
activeProjectLogRefs :: IDE -> Seq LogRef
activeProjectLogRefs ide = case ide ^. activeProject of
    Nothing   -> ide ^. allLogRefs
    Just proj -> Seq.filter (underRoot (pjDir (pjKey proj)) . logRefRootPath)
                            (ide ^. allLogRefs)
  where
    underRoot dir p =
        equalFilePath dir p || addTrailingPathSeparator dir `isPrefixOf` p

-- | A project's captured nix-shell environment for a compiler, if one has
-- been captured (see "IDE.Project.Nix").
nixEnv :: ProjectKey -> Text -> IDE -> Maybe (Map String String)
nixEnv project compiler ide = M.lookup (pjDir project, compiler) $ ide ^. nixCache

-- | Mark a user-visible string for translation.  Real gettext lookup when
-- built with the @LOCALIZATION@ flag; the identity otherwise.
__ :: Text -> Text
#ifdef LOCALIZATION
__ = T.pack . unsafePerformIO . getText . T.unpack
#else
__ = id
#endif

