-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The web UI's own state model: what tabs and windows there are, how a
-- window's panes are split, and which pane owns which AI session.
--
-- A leaf module (types + lenses only, no behaviour): the IDE state holds
-- these, and every @IDE.Web@ widget reads them, so it must sit below both.
module IDE.Web.Model
  ( -- * Side\/bottom bar visibility
    TallVisibility(..)
    -- * Tabs
  , TabKey(..)
    -- * OS windows
  , WindowId(..)
  , WebWindow(..)
  , wwWide0
  , wwActive
  , wwTall
  , wwWide1
  , wwFrame
    -- * The tab flipper
  , FlipItem(..)
    -- * AI sessions per pane
  , AIPaneRef(..)
    -- * Leksah windows (split trees of panes)
  , LeafId(..)
  , SplitOrientation(..)
  , SplitTree(..)
  , PaneKind(..)
  , PaneContent(..)
  , LeksahWindow(..)
  ) where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Map (Map)
import qualified Data.Map as Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

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


makeLenses ''WebWindow
