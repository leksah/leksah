{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.Data
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | The core state of ide. This module is imported from every other module,
-- | and all data structures of the state are declared here, to avoid circular
-- | module dependencies.
--
-------------------------------------------------------------------------------

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
,   TallVisibility(..)
,   TabKey(..)
,   WindowId(..)
,   WebWindow(..)
,   wwWide0
,   wwActive
,   wwTall
,   wwWide1
,   wwFrame

,   LogRefType(..)
,   Log(..)
,   logRootPath
,   LogRef(..)
,   logRefRootPath
,   logRefFilePath
,   logRefFullFilePath
,   isError
,   isBreakpoint
,   displaySrcSpan
,   colorHexString

,   SearchHint(..)
,   KeymapI(..)
#if defined(ghcjs_HOST_OS) || defined(LEKSAH_NO_HLINT)
    -- Idea stand-in (see below); natively the real one comes from hlint,
    -- unless the no-hlint flag drops it (leksah.sh --ghci).
,   Idea(..)
#endif
#if defined(ghcjs_HOST_OS)
    -- Stand-ins for packages that don't build on the JS backend (see their
    -- definitions below); natively the real ones come from fsnotify.
,   WatchManager(..)
,   StopListening
#endif


,   LogLaunchData(..)
,   LogTag(..)
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
,   LeafId(..)
,   SplitOrientation(..)
,   SplitTree(..)
,   PaneKind(..)
,   PaneContent(..)
,   LeksahWindow(..)
,   hiddenWindows
,   activeWindow
,   nextWindowId
,   flipMirror
,   FlipItem(..)
,   flipMru
,   AIPaneRef(..)
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
import Numeric (showHex)
import System.FilePath
       (dropFileName, (</>), isAbsolute, makeRelative, equalFilePath,
        addTrailingPathSeparator)
import IDE.Core.Location
import IDE.Settings
import IDE.Project
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT(..))

import qualified Data.Map as Map (Map)
import Control.Monad.Reader.Class (MonadReader(..))
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
#if !defined(ghcjs_HOST_OS) && !defined(LEKSAH_NO_HLINT)
import Language.Haskell.HLint (Idea(..))
#endif
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
-- IDE State
--

--
-- | The IDE state
--

data IDE            =  IDE {
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

--
-- | A mutable reference to the IDE state
--
type IDERef = MVar (IDE -> IO (), IDE)

--
-- | The IDE Monad
--
type IDEM = ReaderT IDERef IO

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
--   which does not return a value
--
type IDEAction = IDEM ()


class (Applicative m, Monad m, MonadIO m) => MonadIDE m where
    liftIDE :: IDEM a -> m a

instance MonadIDE IDEM where
    liftIDE = id

instance MonadIDE WorkspaceM where
    liftIDE = lift

(?>>=) :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
a ?>>= b = do
    mA <- a
    case mA of
        Just v -> b v
        Nothing -> return ()

-- ---------------------------------------------------------------------
-- Monad for Gtk events (use onIDE instead of on)
--
type IDEEventM t = ReaderT IDERef (ReaderT t IO)

instance MonadIDE (IDEEventM t) where
    liftIDE f = do
        ideR <- ask
        liftIO $ runReaderT f ideR

-- ---------------------------------------------------------------------
-- Monad for functions that need an open workspace
--
type WorkspaceM = ReaderT Workspace IDEM
type WorkspaceAction = WorkspaceM ()

runWorkspace :: WorkspaceM a -> Workspace -> IDEM a
runWorkspace = runReaderT

-- ---------------------------------------------------------------------
-- Monad for functions that need an active package
--
type ProjectM = ReaderT Project WorkspaceM
type ProjectAction = ProjectM ()

instance MonadIDE ProjectM where
    liftIDE = lift . lift

runProject :: ProjectM a -> Project -> WorkspaceM a
runProject = runReaderT

-- ---------------------------------------------------------------------
-- Monad for functions that need an active package
--
type PackageM = ReaderT IDEPackage ProjectM
type PackageAction = PackageM ()

instance MonadIDE PackageM where
    liftIDE = lift . lift . lift

runPackage :: PackageM a -> IDEPackage -> ProjectM a
runPackage = runReaderT

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Events which can be signalled and handled
--


-- ---------------------------------------------------------------------
-- Project
--
--newtype CabalProject = CabalProject
--  { pjCabalFile :: FilePath
--  } deriving (Show, Eq)
--newtype StackProject = StackProject
--  { pjStackFile :: FilePath
--  } deriving (Show, Eq)
--data CustomProject = CustomProject
--  { pjCustomDir        :: FilePath
--  , pjCustomNixShell   :: [Text]
--  , pjCustomGhcBuild   :: [Text]
--  , pjCustomGhcjsBuild :: [Text]
--  , pjCustomRepl       :: [Text]
--  } deriving (Show, Eq)
--
--data ProjectKey =
--    CabalTool CabalProject
--  | StackTool StackProject
--  | CustomTool CustomProject
--  deriving (Show, Eq)

-- | Visibility of the side ("tall") pane, cycled by the toolbar button.
data TallVisibility = TallShow | TallAutoHide | TallHide
    deriving (Eq, Show, Read, Enum, Bounded, Generic)

-- | Identifies one open tab/pane in the web UI.  Lives here (rather than in
-- @IDE.Web.Events@, which re-exports it) because 'WebWindow' below references
-- it and @IDE.Core@ must not depend on @IDE.Web@.
data TabKey
  = WorkspaceKey
  | ErrorsKey
  | LogKey
  | GrepKey
  -- | The side-pane tree of Claude Code agents (by who forked whom), each with
  -- the title and description it gives itself.  Its position here sets its place
  -- in the side bar's tab strip (those buttons render in constructor order).
  | AgentsKey
  | TerminalsKey
  -- | RETIRED (read-only migration alias): pre-v6 web sessions keyed terminal
  -- tabs by tmux session id.  Nothing writes it; reads map it to the
  -- session's 'LeksahWinKey' tabs.  Delete once v5 sessions have cycled.
  | TerminalKey Text
  -- | A leksah window: one tab holding a native split tree of leksah panes
  -- (tmux windows and/or native views).  The Text is the 'LeksahWindow' id
  -- (key into 'leksahWindows').
  | LeksahWinKey Text
  | MetadataKey
  | ChangesKey
  | PreferencesKey
  -- | The keyboard-shortcut cheat sheet (read-only, ⌘D-convertible).
  | ShortcutsKey
  -- | An embedded web browser pane.  The Int is its persistent id (minted by
  -- 'IDE.Web.Widget.Browser.nextBrowserId', never reused); the pane's URL
  -- state lives in that module's registry, not in the key.
  | BrowserKey Int
  | EditorKey FilePath
  -- | A git log viewer for a branch: the repo dir and the branch/ref to log.
  | GitLogKey FilePath Text
  -- | The agent-change review pane for a checkout (Claude worktree flow):
  -- diff vs the review base, comment-to-session, merge/PR/archive actions.
  | ReviewKey FilePath
  -- | The Claude task queue (queued prompts → worktree sessions).
  | TasksKey
  -- | A session's plan-review pane: the session's dir and its transcript
  -- path (where the plan markdown is read from).
  | PlanKey FilePath Text
  -- | Side-by-side comparison of the N worktrees running the same queued
  -- prompt (compare-N-approaches): the project dir and the shared prompt.
  | CompareKey FilePath Text
    deriving (Ord, Eq, Show, Generic)

-- | Identifies one native OS window in the multi-window web UI.  Minted
-- monotonically ('nextWindowId'); a freed id is never reused.
newtype WindowId = WindowId Int deriving (Eq, Ord, Show, Generic)

-- | Per-OS-window state that must be visible across windows (it lives
-- window-keyed in the shared 'IDE' MVar so a mutation in one window's reflex
-- network is observed by every other window and by session persistence).  The
-- shared side pane / bottom bar /content/ is NOT here — only what is genuinely
-- per-window: the wide0 (editor/terminal) tabs this window owns, its visible
-- wide0 tab, its side/bottom pane visibility, and its native frame.
data WebWindow = WebWindow
  { _wwWide0  :: [TabKey]          -- ^ wide0 tabs owned by this window, MRU/flip order
  , _wwActive :: Maybe TabKey      -- ^ the visible wide0 tab in this window
  , _wwTall   :: TallVisibility    -- ^ per-window side-pane visibility
  , _wwWide1  :: TallVisibility    -- ^ per-window bottom-bar visibility
  , _wwFrame  :: Maybe Text        -- ^ native window frame "x,y,w,h" (filled by the native side)
  } deriving (Eq, Show)

-- | A flipper (Ctrl-Tab) target: an ordinary tab, an individual tmux pane
-- @(session id, window, pane)@ — so the flipper cycles panes, not whole
-- terminals — or a native VIEW pane (editor\/git-log\/browser leaf) inside a
-- leksah window, as @(lw id, leaf id)@.  The session id is tmux's stable
-- @#{session_id}@.  Lives here (rather than in @IDE.Web.Events@, which
-- re-exports it) because the shared flip MRU ('flipMru') references it and
-- @IDE.Core@ must not depend on @IDE.Web@.
data FlipItem = FlipTab TabKey | FlipPane Text Int Int | FlipView Text Int
  deriving (Eq, Ord, Show, Generic)

-- | Identifies one pane for the purpose of remembering its default AI session
-- ('paneAISession').  Deliberately NOT 'FlipItem': that carries tmux window\/
-- pane *indexes*, which shift when a neighbour closes — harmless for an MRU
-- list, but it would silently re-aim a send at the wrong pane.  Each
-- constructor holds an identifier that is stable for the pane's whole life:
--
--   * 'PRTmux' — tmux's @#{pane_id}@ (@%7@), stable for the tmux server's
--     lifetime (which outlives leksah), and it travels with the pane through
--     move-pane\/join-pane.
--   * 'PRLeaf' — a native VIEW leaf as @(leksah window id, 'LeafId')@; leaf
--     ids are minted monotonically per window and never reused.
--   * 'PRTab'  — a plain wide0 tab (an editor opened as its own tab rather
--     than as a split leaf).  This is where AI ▸ Send Selection usually fires
--     from, which is why the association can't live on 'PaneContent'.
data AIPaneRef = PRTmux Text | PRLeaf Text Int | PRTab TabKey
  deriving (Eq, Ord, Show, Generic)

-- | Stable id of one pane within a leksah window's native split layout.
-- Minted monotonically per window ('lwNext') and never reused, so reflex
-- keyed widgets can never confuse two panes.  Lives here (like 'TabKey')
-- because the 'IDE' record references 'LeksahWindow' and @IDE.Core@ must not
-- depend on @IDE.Web@.
newtype LeafId = LeafId Int deriving (Eq, Ord, Show, Generic)

-- | Orientation of a native split: 'SplitH' lays children out side by side
-- (a horizontal row), 'SplitV' stacks them top to bottom.
data SplitOrientation = SplitH | SplitV deriving (Eq, Show, Generic)

-- | The geometry of a leksah window's native split layout: an n-ary tree
-- (like tmux's own layout cells) whose leaves are identified by 'LeafId' —
-- contents live separately in 'lwPanes'.  Each child carries its fraction of
-- the parent; fractions sum to 1 and are renormalised on every edit.
data SplitTree
  = SplitLeaf LeafId
  | SplitNode SplitOrientation [(Double, SplitTree)]
  deriving (Eq, Show, Generic)

-- | What one leksah pane shows: a whole tmux window (tmux keeps doing
-- everything it can express — its own panes, splits and resizes render
-- inside the leaf), or a native leksah view for the things tmux can't host
-- (an editor, a git log).
data PaneKind
  = PaneTmux Text                -- ^ tmux window ID (@\@7@ — stable for the
                                 --   tmux server's lifetime, unlike indexes)
  | PaneView TabKey              -- ^ 'EditorKey' / 'GitLogKey' / 'BrowserKey'
                                 --   only (enforced by the layout codec)
  deriving (Eq, Show, Generic)

-- | A pane's content plus its font size.  @Nothing@ = follow the global
-- @monospaceFontSize@ pref (forever — not a snapshot).  Because font size is
-- per leksah pane and a tmux window lives in exactly one pane, two tmux
-- panes with different font sizes can never share a tmux window —
-- structurally.
data PaneContent = PaneContent
  { pcKind     :: PaneKind
  , pcFontSize :: Maybe Int
  } deriving (Eq, Show, Generic)

-- | One leksah window: a wide0 TAB holding a native split tree of leksah
-- panes.  NOT a tmux session (a session may back several leksah windows),
-- not a tmux window (that's one possible pane content), and not an OS
-- window (an OS window holds many tabs).  Runtime truth lives in
-- 'leksahWindows' (shared across OS windows via the IDE MVar); session-backed
-- windows are persisted onto their tmux session as the @\@leksah_layout@
-- option (base64 JSON, see @IDE.Web.SplitLayout@) so they survive leksah
-- restarts; sessionless (pure native) ones persist in the web session file.
data LeksahWindow = LeksahWindow
  { lwSession :: Maybe Text      -- ^ backing tmux session id (@$5@); every
                                 --   'PaneTmux' window belongs to it.
                                 --   @Nothing@ = sessionless (views only)
  , lwTree    :: SplitTree
  , lwPanes   :: Map.Map LeafId PaneContent
  , lwFocused :: Maybe LeafId    -- ^ target of ⌘+/⌘−/split commands
  , lwZoomed  :: Maybe LeafId    -- ^ zoomed pane fills the tab
  , lwNext    :: Int             -- ^ 'LeafId' minter (monotonic, never reused)
  } deriving (Eq, Show, Generic)

data SearchHint = Forward | Backward | Insert | Delete | Initial
    deriving (Eq)

-- Version-Control-System Configuration

--
-- | Other types
--

-- Order determines priority of the icons in the gutter
data LogRefType = ContextRef | BreakpointRef | ErrorRef | TestFailureRef | WarningRef | LintRef
    deriving (Eq, Ord, Show, Enum, Bounded)

data Log =
    LogProject {logBasePath :: FilePath}
  | LogCabal {logCabalFile :: FilePath}
  | LogNix {logNixFile :: FilePath, logNixAttribute :: Text}
  deriving(Eq, Show)

logRootPath :: Log -> FilePath
logRootPath LogProject{..} = logBasePath
logRootPath LogCabal{..} = dropFileName logCabalFile
logRootPath LogNix{..} = dropFileName logNixFile

#if defined(ghcjs_HOST_OS) || defined(LEKSAH_NO_HLINT)
-- | Stand-in for hlint's 'Language.Haskell.HLint.Idea': hlint (via
-- ghc-lib-parser, whose RTS-internals hsc doesn't compile) is unavailable on
-- the JS backend, and is dropped by the no-hlint flag (leksah.sh --ghci, where
-- the RTS linker can't load ghc-lib-parser's static archive).  'LogRef' stores
-- one and 'IDE.Core.State.canResolve' reads 'ideaHint' / 'ideaTo'; nothing
-- more of the real record is used here.
data Idea = Idea { ideaHint :: String, ideaTo :: Maybe String }
    deriving (Eq, Show)
#endif

#if defined(ghcjs_HOST_OS)
-- | Stand-ins for fsnotify's types: fsnotify (via unix-compat) doesn't build
-- on the JS backend, and there is no file watching in a browser anyway.  The
-- '_fsnotify' / '_watchers' fields still exist; the JS branch of
-- 'IDE.Workspaces.Writer' registers only no-op watchers.
data WatchManager = NoWatchManager
type StopListening = IO ()
#endif

-- | Represents a message about a part of the source code
data LogRef = LogRef {
    logRefSrcSpan       ::   SrcSpan
,   logRefLog           ::   Log
,   refDescription      ::   Text
,   logRefIdea          ::   Maybe (Text, Idea)
,   logLines            ::   Maybe (Int, Int)
,   logRefType          ::   LogRefType
} deriving(Eq)

instance Show LogRef where
    show lr = T.unpack (refDescription lr) ++ displaySrcSpan (logRefSrcSpan lr)

displaySrcSpan :: SrcSpan -> String
displaySrcSpan s = srcSpanFilename s ++ ":" ++
    if srcSpanStartLine s == srcSpanEndLine s
        then show (srcSpanStartLine s) ++ ":" ++
            if srcSpanStartColumn s == srcSpanEndColumn s
                then show (srcSpanStartColumn s)
                else show (srcSpanStartColumn s) ++ "-" ++ show (srcSpanEndColumn s)
        else show (srcSpanStartLine s) ++ ":" ++
            show (srcSpanStartColumn s) ++ "-" ++ show (srcSpanEndColumn s)

-- | The root folder of the package the message references
logRefRootPath :: LogRef -> FilePath
logRefRootPath = logRootPath . logRefLog

-- | The file path the message references, relative to the root path
logRefFilePath :: LogRef -> FilePath
logRefFilePath lr = let
    f = srcSpanFilename $ logRefSrcSpan lr
    in if isRemotePath f
            -- Stored ssh:// span (an out-of-root remote file): show it
            -- relative to the (remote) root when possible.
            then remoteMakeRelative (logRefRootPath lr) f
       else if isAbsolute f -- can happen, at least when building with stack a source file that is present in several components (ie library and test)
            then makeRelative (logRefRootPath lr) f
            else f

-- | The absolute file path the message references
logRefFullFilePath :: LogRef -- ^ The log ref
    -> FilePath -- ^ the result
logRefFullFilePath lr = let
    f = srcSpanFilename $ logRefSrcSpan lr
    root = logRefRootPath lr
    in if isRemotePath f
            then f
       else if isAbsolute f
            -- An absolute (host-local) filename under a remote root came
            -- from the remote compiler — re-attach the host.
            then maybe f (\(host, _) -> renderRemotePath host f) (parseRemotePath root)
            else root </> f

isError :: LogRef -> Bool
isError = (== ErrorRef) . logRefType

isBreakpoint :: LogRef -> Bool
isBreakpoint = (== BreakpointRef) . logRefType

--isContext :: LogRef -> Bool
--isContext = (== ContextRef) . logRefType

-- This should probably be in Gtk2Hs allong with a suitable parser
colorHexString :: Color -> String
colorHexString (Color r g b) = '#' : pad (showHex r "")
                                  ++ pad (showHex g "")
                                  ++ pad (showHex b "")
    where pad s = replicate (4 - length s) '0' ++ s


newtype KeymapI         =   KM  (Map ActionString
                                [(Maybe (Either KeyString (KeyString,KeyString)), Maybe Text)])

data LogTag = LogTag | ErrorTag | FrameTag | InputTag | InfoTag deriving(Eq, Ord, Show)

data SearchMode = Exact {caseSense :: Bool} | Prefix {caseSense :: Bool}
                | Regex {caseSense :: Bool}
    deriving (Eq,Ord,Read,Show,Generic)

instance ToJSON SearchMode
instance FromJSON SearchMode


makeLenses ''IDE
makeLenses ''WebWindow

activeProject :: Getter IDE (Maybe Project)
activeProject = workspace . to (>>= view wsActiveProject)

activePack :: Getter IDE (Maybe IDEPackage)
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

nixEnv :: ProjectKey -> Text -> IDE -> Maybe (Map String String)
nixEnv project compiler ide = M.lookup (pjDir project, compiler) $ ide ^. nixCache

#ifdef LOCALIZATION

-- | For i18n using hgettext
__ :: Text -> Text
__ = T.pack . unsafePerformIO . getText . T.unpack


#else

-- | For i18n support. Not included in this build.
__ :: Text -> Text
__ = id

#endif

