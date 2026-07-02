{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
module IDE.Web.Events where

import Control.Lens (makePrisms)

import Data.Dependent.Map (DMap)
import Data.Functor.Identity (Identity(..))
import Data.GADT.Compare.TH (DeriveGEQ(..), DeriveGCompare(..))
import Data.Map (Map)
import Data.Text (Text)

import GHC.Generics (Generic)

import Distribution.Types.PackageId (PackageIdentifier(..))

import IDE.Core.CTypes (SrcSpan)
import IDE.Core.Types (LogRef(..), Prefs(..))
import IDE.Utils.Project (ProjectKey)
import IDE.Web.Command (Command(..))

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

type PackageEvents = Map PackageIdentifier PackageEvent

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
  ErrorsGoto LogRef deriving (Eq, Show)

makePrisms ''ErrorsEvents

-- | Find-bar commands for list/tree panes (the editor drives CodeMirror
-- directly).  'FindUpdate' carries the query text and the flag bitmask
-- (1=case, 2=word, 4=regexp); 'FindStep' moves to the next/previous match;
-- 'FindGrep' (the Grep button) carries the query+flags to grep the workspace.
data FindbarEvents
  = FindUpdate Text Int
  | FindStep Bool
  | FindGrep Text Int
-- | The Grep pane: clicking a result navigates to that file + line.
newtype GrepEvents = GrepGoto SrcSpan
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
  = TerminalTitle Text | TerminalGoto SrcSpan | TerminalBell | TerminalExited

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
    -- Remote selections carry the session id AND its current name: an open
    -- tab may be keyed by either (cc-connect HOST#NAME vs the tree's ids),
    -- so the handler matches both before opening a new tab.
  | SelectRemoteTerminal Text Text Text              -- ^ host, session id, name
  | SelectRemoteTerminalWindow Text Text Text Int    -- ^ host, id, name, window
  | SelectRemoteTerminalPane Text Text Text Int Int  -- ^ host, id, name, window, pane

makePrisms ''TerminalsEvents

-- | Navigate to a source location (file + span) chosen in the metadata tree.
newtype MetadataEvents = MetadataGoto SrcSpan

makePrisms ''MetadataEvents

-- | Open a changed file picked in the Changes pane.
newtype ChangesEvents = ChangesOpen FilePath

makePrisms ''ChangesEvents

-- | The Preferences pane edits the IDE 'Prefs'; each change is the update to
-- apply (the reflex layer runs it via @modifyIDE_ (prefs %~ f)@, which the
-- existing debounced writer then persists).
newtype PreferencesEvents = PrefsUpdate (Prefs -> Prefs)

makePrisms ''PreferencesEvents
type StatusbarEvents = ()
newtype MenubarEvents =
  MenubarCommand Command

makePrisms ''MenubarEvents

newtype ToolbarEvents =
  ToolbarCommand Command

makePrisms ''ToolbarEvents

data TabKey
  = WorkspaceKey
  | ErrorsKey
  | LogKey
  | GrepKey
  | TerminalsKey
  | TerminalKey Text
  | MetadataKey
  | ChangesKey
  | PreferencesKey
  | EditorKey FilePath
    deriving (Ord, Eq, Show, Generic)

-- | A flipper (Ctrl-Tab) target: an ordinary tab, or an individual tmux pane
-- @(session id, window, pane)@ — so the flipper cycles panes, not whole
-- terminals.  The session id is tmux's stable @#{session_id}@.
data FlipItem = FlipTab TabKey | FlipPane Text Int Int
  deriving (Eq, Ord, Show)

data TabEvents e where
  EditorTab    :: TabEvents EditorEvents
  ErrorsTab    :: TabEvents ErrorsEvents
  LogTab       :: TabEvents LogEvents
  GrepTab      :: TabEvents GrepEvents
  TerminalTab  :: TabEvents TerminalEvents
  TerminalsTab :: TabEvents TerminalsEvents
  MetadataTab  :: TabEvents MetadataEvents
  ChangesTab   :: TabEvents ChangesEvents
  PreferencesTab :: TabEvents PreferencesEvents
  WorkspaceTab :: TabEvents ProjectEvents

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

