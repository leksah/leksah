{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
module IDE.Web.Events (module IDE.Web.Events, TabKey(..), FlipItem(..)) where

import Control.Lens (makePrisms)

import Data.Dependent.Map (DMap)
import Data.Functor.Identity (Identity(..))
import Data.GADT.Compare.TH (DeriveGEQ(..), DeriveGCompare(..))
import Data.Map (Map)
import Data.Text (Text)

import IDE.Config (Config)
import IDE.Problems.Types (Loc, Problem)
import IDE.Web.Command (Command(..))
import IDE.Web.Model (TabKey(..), FlipItem(..))
import IDE.Ws.Types (ProjectKey)

data FileEvent
  = OpenFile Bool FilePath
  | DeleteFile FilePath
  deriving (Eq, Ord, Show)

makePrisms ''FileEvent

-- Workspace Tree Events
type FileEvents    = Map FilePath FileEvent

data PackageEvent
  = PackageCommand Command
  | PackageFileEvents FileEvents

makePrisms ''PackageEvent

-- | Keyed by the package's manifest path ('IDE.Ws.Types.pkgManifest') — the
-- stable identity of a 'Package' in the new workspace model.
type PackageEvents = Map FilePath PackageEvent

data ProjectEvent
  = ProjectCommand Command
  | ProjectPackageEvents PackageEvents
  | ProjectFileEvents FileEvents

makePrisms ''ProjectEvent

type ProjectEvents = Map (Int, ProjectKey) ProjectEvent

newtype KeymapEvents =
  KeymapCommand Command

makePrisms ''KeymapEvents

type EditorEvents = ()
newtype ErrorsEvents =
  ErrorsGoto Problem deriving (Eq, Show)

makePrisms ''ErrorsEvents

-- | Find-bar commands for list/tree panes (the editor drives CodeMirror
-- directly).  'FindUpdate' carries the query text and the flag bitmask
-- (1=case, 2=word, 4=regexp); 'FindStep' moves to the next/previous match;
-- 'FindGrep' (the Grep button) carries the query+flags to grep the workspace.
-- 'FindHide' (Escape in the bar) asks for the bar to close and the keyboard
-- to go back to the active pane.
data FindbarEvents
  = FindUpdate Text Int
  | FindStep Bool
  | FindGrep Text Int
  | FindHide
-- | The Grep pane: clicking a result navigates to that file + line.
newtype GrepEvents = GrepGoto Loc
type LogEvents = ()

makePrisms ''GrepEvents

-- | A single terminal reports its (OSC-set) window title so the Terminals list
-- can label it, and (on a Ctrl+click of a project-file path in its output) asks
-- to open that file at a location.
-- | A terminal's window title changed, a link was clicked (go to source), the
-- shell rang the bell (Claude Code's "needs input / done" signal — caught here
-- for the *viewed* window, which tmux's alert-bell hook skips), or the attached
-- tmux client exited (the session ended, e.g. the last window's shell exited) —
-- the tab should close rather than linger showing "[exited]".
data TerminalEvents
  = TerminalTitle Text | TerminalGoto Loc | TerminalBell | TerminalExited
  -- | A control-mode tab saw a window created/closed: the Terminals tree and
  -- tab row should refresh now, not on the next 2s/10s poll.
  | TerminalTreeChanged
  -- | The leksah user gave a specific pane (by tmux @#{pane_id}@, e.g. @%5@)
  -- focus *from within leksah* — a ⌘-number split select, or (via the global
  -- mousedown listener) a pane click.  This is the leksah-owned pane-recency
  -- signal that floats the pane to the flipper MRU front; deliberately NOT the
  -- control-mode @%window-pane-changed@ broadcast, which also follows other
  -- clients attached to the same tmux session.
  | TerminalPaneFocused Text
  -- | The session's *current* window was just closed (its last pane exited), so
  -- tmux is about to pick a replacement by its own rule — the IDE overrides that
  -- to activate the second entry in the tab-button list (the ⌘1 button) instead.
  | TerminalActiveWinClosed
  -- | The user went to a terminal's Retry/error page: the IDE floats this
  -- terminal to the flipper MRU front so it's easy to flip back to.  The flag
  -- is 'True' for a deliberate navigation (selecting a tab already showing the
  -- page — always float it) and 'False' for the page merely appearing on a
  -- dropped connection (float only if it's the visible tab, so a background
  -- drop doesn't reorder tabs under the user).
  | TerminalConnErrShown Bool
  -- | A Retry attempt actually reconnected (real session data arrived, not just
  -- a doomed ssh that will exit): if this terminal is still the active tab, the
  -- IDE brings its OS window forward.
  | TerminalReconnected

makePrisms ''TerminalEvents

-- | The Terminals tree pane: create a new terminal; select an existing session
-- (by tmux session id, e.g. @$3@) to bring it up in the editor area; close one
-- (kill its tmux session); or, drilling into the tmux hierarchy, select a window
-- (session id, window index) or a pane (session id, window index, pane index) —
-- which switches tmux to it and brings the owning session's terminal up.  The
-- session id is tmux's stable @#{session_id}@, which survives renames.
data TerminalsEvents
  = NewTerminal
  | SelectTerminal Text
  | CloseTerminal Text
  | SelectTerminalWindow Text Int
  | SelectTerminalPane Text Int Int
    -- remote hosts (Terminals-tree host nodes; sessions render in CC tabs
    -- keyed @ssh://host#session@)
  | NewRemoteTerminal Text                      -- ^ host
  | SelectRemoteHost Text                        -- ^ host: bring up its one per-server connection
    -- Remote selections carry the session id AND its current name: an open
    -- tab may be keyed by either (cc-connect HOST#NAME vs the tree's ids),
    -- so the handler matches both before opening a new tab.
  | SelectRemoteTerminal Text Text Text              -- ^ host, session id, name
  | SelectRemoteTerminalWindow Text Text Text Int    -- ^ host, id, name, window
  | SelectRemoteTerminalPane Text Text Text Int Int  -- ^ host, id, name, window, pane
    -- Remote management, the analogues of the local session/window/pane
    -- controls; run over ssh in 'IDE.Web.Main', which then re-polls the host.
  | CloseRemoteTerminal Text Text Text               -- ^ host, id, name (kill-session + drop tab)
  | NewRemoteTerminalWindow Text Text                -- ^ host, session id
  | KillRemoteTerminalWindow Text Text Int           -- ^ host, id, window
  | RenameRemoteTerminalSession Text Text Text       -- ^ host, id, new name
  | RenameRemoteTerminalWindow Text Text Int Text    -- ^ host, id, window, new name
  | ZoomRemoteTerminalPane Text Text Int Int         -- ^ host, id, window, pane
  | BreakRemoteTerminalPane Text Text Int Int        -- ^ host, id, window, pane
  | KillRemoteTerminalPane Text Text Int Int         -- ^ host, id, window, pane

makePrisms ''TerminalsEvents

-- | Navigate to a source location (file + span) chosen in the metadata tree.
newtype MetadataEvents = MetadataGoto Loc

makePrisms ''MetadataEvents

-- | Open a changed file picked in the Changes pane.
newtype ChangesEvents = ChangesOpen FilePath

makePrisms ''ChangesEvents

-- | The Preferences pane edits the user 'Config'; each change is the update to
-- apply (the reflex layer applies it to the current config and persists via
-- @saveConfig@).
newtype PreferencesEvents = PrefsUpdate (Config -> Config)

makePrisms ''PreferencesEvents

-- | The git log viewer's diff is shown in-pane, so it reports nothing outward.
type GitLogEvents = ()
-- | The review pane acts through IO (send-to-session, git) — nothing outward.
type ReviewEvents = ()
-- | The task-queue pane acts through IO (queue ops) — nothing outward.
type TasksEvents = ()
-- | The plan-review pane acts through IO (send-keys) — nothing outward.
type PlanEvents = ()
-- | The Agents pane acts through IO (show/resume a session, send it the refresh
-- prompt, forget an exited one) — nothing outward.
type AgentsEvents = ()
type CompareEvents = ()
-- | The Shortcuts cheat-sheet pane is read-only, so it reports nothing outward.
type ShortcutsEvents = ()
-- | The browser pane keeps its navigation state to itself.
type BrowserEvents = ()
type StatusbarEvents = ()
newtype MenubarEvents =
  MenubarCommand Command

makePrisms ''MenubarEvents

newtype ToolbarEvents =
  ToolbarCommand Command

makePrisms ''ToolbarEvents

-- ('FlipItem' — a flipper target — now lives in "IDE.Web.Model", re-exported
-- above, because the shared flip MRU in the UI model references it.)

data TabEvents e where
  EditorTab    :: TabEvents EditorEvents
  ErrorsTab    :: TabEvents ErrorsEvents
  LogTab       :: TabEvents LogEvents
  GrepTab      :: TabEvents GrepEvents
  TerminalTab  :: TabEvents TerminalEvents
  TerminalsTab :: TabEvents TerminalsEvents
  MetadataTab  :: TabEvents MetadataEvents
  AgentsTab    :: TabEvents AgentsEvents
  ChangesTab   :: TabEvents ChangesEvents
  PreferencesTab :: TabEvents PreferencesEvents
  ShortcutsTab :: TabEvents ShortcutsEvents
  BrowserTab   :: TabEvents BrowserEvents
  WorkspaceTab :: TabEvents ProjectEvents
  GitLogTab    :: TabEvents GitLogEvents
  ReviewTab    :: TabEvents ReviewEvents
  TasksTab     :: TabEvents TasksEvents
  PlanTab      :: TabEvents PlanEvents
  CompareTab   :: TabEvents CompareEvents

deriveGEq      ''TabEvents
deriveGCompare ''TabEvents

data IDEWidget e where
  FindbarWidget   :: IDEWidget FindbarEvents
  MenubarWidget   :: IDEWidget MenubarEvents
  StatusbarWidget :: IDEWidget StatusbarEvents
  ToolbarWidget   :: IDEWidget ToolbarEvents
  TabWidget       :: IDEWidget (Map TabKey (DMap TabEvents Identity))
  KeymapWidget    :: IDEWidget KeymapEvents

deriveGEq      ''IDEWidget
deriveGCompare ''IDEWidget

