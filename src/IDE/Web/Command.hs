{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Command where

import Control.Lens (Getter, to, makePrisms, (%~), (^.), (&), ix)
import Control.Monad (unless)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (cons)
import Data.Text (Text)

import IDE.Web.AIContextRequest
       (AIAction(..), requestAIAction)
import IDE.Web.AddServerRequest (requestAddServer)
import IDE.Web.CloseRequest (requestCloseActivePane)
import IDE.Web.NewWindowRequest (requestNewWindow)
import IDE.Web.NewLwRequest (requestFontConvert, requestConsolidate)
import IDE.Web.ReplTmux (paneCountOfWindow)
import IDE.Web.RegionGrabRequest (requestRegionGrab)
import IDE.Web.TerminalInput
       (sendToActiveTerminal, tmuxCommandActiveTerminal, splitActiveTerminal)
import IDE.Web.TransparencyRequest (requestToggleTransparency)
import IDE.Web.SnapRequest (requestSnapWindow)

import qualified Data.Map as M (adjust, delete, insert, lookup)
import Data.Maybe (fromMaybe)

import IDE.App (App(..), AppAction, appNote)
import IDE.Builder (buildActiveTarget, runVerb)
import IDE.Config
       (BuildC(..), Config(..), FontC(..), TerminalC(..), UiC(..),
        currentConfig, saveConfig)
import IDE.Reactive (modifyCell, readCell)
import IDE.Web.Model
       (LeafId, LeksahWindow(..), PaneContent(..), PaneKind(..), TabKey(..),
        TallVisibility(..), WebUi, activeWindow, leksahWindows, tabFontSize,
        webWindows, wwActive, wwTall, wwWide1, wwZoom)
import IDE.Workspace
       (WorkspaceService(..), Ws, activeComponent, activePackage,
        activeProject, prDir)
import IDE.Ws.Types (Package, Project(..), Verb(..))
import IDE.Web.Claude (runClaudeCmd, ClaudeCmd(..))

-- | The active-target flavours of a command's action: run with the current
-- workspace snapshot / the active project / the active package (a build-log
-- note when nothing is active — the 'workspaceTry'\/'projectTry'\/'packageTry'
-- successors, reading the 'Ws' cell instead of an IDE monad).
type WorkspaceAction = App -> Ws -> IO ()
type ProjectAction   = App -> Project -> IO ()
type PackageAction   = App -> Project -> Package -> IO ()

data Command =
    CommandIDEAction Text Text AppAction
  | CommandIDEToggleAction Text Text AppAction (Config -> Bool)
  | CommandWorkspaceAction Text Text WorkspaceAction
  | CommandProjectAction Text Text ProjectAction
  | CommandPackageAction Text Text PackageAction
  | CommandFileOpen
  | CommandProjectOpen
  | CommandProjectOpenFolder
  | CommandProjectAddRemote
  | CommandFileSave
  | CommandFind
  | CommandShowPreferences
  | CommandShowShortcuts
  | CommandOpenBrowser
  | CommandNextError
  | CommandPreviousError
  | CommandFlipDown
  | CommandFlipUp
  | CommandFlipDone
  -- Jump to the next terminal window flagged for attention (bell first, then
  -- activity) — e.g. a teammate that rang the bell wanting input.
  | CommandFocusAlert
  -- Numbered navigation (1-based; see IDE.Web.Main): Cmd+N the Nth split of
  -- the active terminal, Opt+Cmd+N the Nth side-bar pane, Ctrl+Cmd+N the Nth
  -- bottom-bar pane.
  | CommandSelectSplit Int
  | CommandSelectSidePane Int
  | CommandSelectBottomPane Int

makePrisms ''Command

-- | Run @f@ with the active project, or note why not.
withActiveProject :: App -> ProjectAction -> IO ()
withActiveProject app f = do
  ws <- readCell (wsCell (appWorkspace app))
  case activeProject ws of
    Nothing -> appNote app "no active project"
    Just pr -> f app pr

-- | Run @f@ with the active project and package, or note why not.
withActivePackage :: App -> PackageAction -> IO ()
withActivePackage app f = do
  ws <- readCell (wsCell (appWorkspace app))
  case activeProject ws of
    Nothing -> appNote app "no active project"
    Just pr -> case activePackage ws of
      Nothing  -> appNote app "no active package"
      Just pkg -> f app pr pkg

commandAction :: Getter Command (Maybe AppAction)
commandAction = to $ \case
  (CommandIDEAction       _ _ a)   -> Just a
  (CommandIDEToggleAction _ _ a _) -> Just a
  (CommandWorkspaceAction _ _ a)   -> Just $ \app ->
      readCell (wsCell (appWorkspace app)) >>= a app
  (CommandProjectAction   _ _ a)   -> Just (`withActiveProject` a)
  (CommandPackageAction   _ _ a)   -> Just (`withActivePackage` a)
  _ -> Nothing

commandImageAndTip :: Command -> (Text, Text)
commandImageAndTip (CommandIDEAction img tip _) = (img, tip)
commandImageAndTip (CommandIDEToggleAction img tip _ _) = (img, tip)
commandImageAndTip (CommandWorkspaceAction img tip _) = (img, tip)
commandImageAndTip (CommandProjectAction img tip _) = (img, tip)
commandImageAndTip (CommandPackageAction img tip _) = (img, tip)
commandImageAndTip CommandFileOpen = ("/pics/file-open.svg", "Opens an existing file")
commandImageAndTip CommandFileSave = ("/pics/file-save.svg", "Saves the current buffer")
commandImageAndTip CommandFind = ("/pics/find.svg", "Show or hide the find bar")
commandImageAndTip CommandNextError = ("/pics/error-next.svg", "Go to the next error")
commandImageAndTip CommandPreviousError = ("/pics/error-prev.svg", "Go to the previous error")
commandImageAndTip CommandShowShortcuts = ("/pics/shortcuts.svg", "Show the keyboard shortcut cheat sheet")
commandImageAndTip CommandOpenBrowser = ("/pics/browser.svg", "Open a new web browser pane")
commandImageAndTip _ = ("", "")

-- | A toggle command's live state, read off the current 'Config' (the
-- Toolbar drives its highlight from @f \<$\> cCfg ctx@; the native menus'
-- validateMenuItem reads it via 'IDE.Config.currentConfig').
commandGetToggleState :: Command -> Maybe (Config -> Bool)
commandGetToggleState (CommandIDEToggleAction _ _ _ f) = Just f
commandGetToggleState _ = Nothing

-- | Update the live config and persist it (the toggles' shared shape).
overConfig :: (Config -> Config) -> AppAction
overConfig f app = currentConfig (appConfig app) >>= saveConfig (appConfig app) . f

overBuildC :: (BuildC -> BuildC) -> AppAction
overBuildC f = overConfig $ \c -> c { cfgBuild = f (cfgBuild c) }

commandAddModule, commandRefreshNix, commandPackageClean
  , commandPackageBuild, commandPackageRun, commandPackageRunJavascript
  , commandToggleBackgroundBuild, commandToggleNative, commandToggleJavaScript
  , commandToggleDebug, commandToggleMakeDocs, commandToggleTest
  , commandToggleRunBenchmarks, commandToggleMakeDependents
  , commandToggleShowIgnored, commandToggleShowHidden, commandToggleTallPane
  , commandToggleWide1Pane, commandToggleTmuxIntercept
  , commandFileClose :: Command
commandAddModule = CommandPackageAction
  "/pics/new-module.svg"
  "Creates a new Haskell module"
  (\_ _ _ -> return ())

commandRefreshNix = CommandProjectAction
  "/pics/nix.svg"
  "Refresh Leksah's cached nix environment variables for the active project"
  -- The nix dev-env command wrapping is dropped for now (commands run in the
  -- ambient env, like the dev loop); keep the menu/button wiring.
  (\app _ -> appNote app "nix env refresh is not reimplemented yet")

commandPackageClean = CommandPackageAction
  "/pics/clean.svg"
  "Cleans the package"
  (\app pr pkg -> runVerb (appBuilder app) (prKey pr) (Just pkg) Nothing VClean)

commandPackageBuild = CommandIDEAction
  "/pics/build.svg"
  "Builds the package"
  (buildActiveTarget . appBuilder)

commandPackageRun = CommandPackageAction
  "/pics/run.svg"
  "Runs the package"
  (\app pr pkg -> do
      ws <- readCell (wsCell (appWorkspace app))
      runVerb (appBuilder app) (prKey pr) (Just pkg) (activeComponent ws) VRun)

commandPackageRunJavascript = CommandPackageAction
  "/pics/run-js.svg"
  "Run jsexe created by GHCJS"
  -- No JavaScript verb in the new builder yet; keep the wiring, note it.
  (\app _ _ -> appNote app "run JavaScript (GHCJS) is not reimplemented yet")

commandToggleBackgroundBuild = CommandIDEToggleAction
  "/pics/background-build.svg"
  "Build in the background and report errors"
  (overBuildC $ \b -> b { bcBackground = not (bcBackground b) })
  (bcBackground . cfgBuild)

commandToggleNative = CommandIDEToggleAction
  "/pics/target-native.svg"
  "Use GHC to compile"
  (overBuildC $ \b -> b { bcNative = not (bcNative b) })
  (bcNative . cfgBuild)

commandToggleJavaScript = CommandIDEToggleAction
  "/pics/target-js.svg"
  "Use GHCJS to compile"
  (overBuildC $ \b -> b { bcJavaScript = not (bcJavaScript b) })
  (bcJavaScript . cfgBuild)

commandToggleDebug = CommandIDEToggleAction
  "/pics/debug.svg"
  "Build and run in GHCi (ffcabal repls)"
  (overBuildC $ \b -> b { bcGhci = not (bcGhci b) })
  (bcGhci . cfgBuild)

commandToggleMakeDocs = CommandIDEToggleAction
  "/pics/docs.svg"
  "Make documentation when building"
  (overBuildC $ \b -> b { bcDocs = not (bcDocs b) })
  (bcDocs . cfgBuild)

commandToggleTest = CommandIDEToggleAction
  "/pics/test.svg"
  "Run unit tests when building"
  (overBuildC $ \b -> b { bcTests = not (bcTests b) })
  (bcTests . cfgBuild)

commandToggleRunBenchmarks = CommandIDEToggleAction
  "/pics/bench.svg"
  "Run benchmarks when building"
  (overBuildC $ \b -> b { bcBenchmarks = not (bcBenchmarks b) })
  (bcBenchmarks . cfgBuild)

commandToggleMakeDependents = CommandIDEToggleAction
  "/pics/dependents.svg"
  "Make dependent packages"
  (overBuildC $ \b -> b { bcMakeMode = not (bcMakeMode b) })
  (bcMakeMode . cfgBuild)

commandToggleShowIgnored = CommandIDEToggleAction
  "/pics/show-ignored.svg"
  "Show files ignored by git in the workspace file trees"
  (overConfig $ \c -> c { cfgUi = (cfgUi c)
      { uiShowIgnoredFiles = not (uiShowIgnoredFiles (cfgUi c)) } })
  (uiShowIgnoredFiles . cfgUi)

commandToggleShowHidden = CommandIDEToggleAction
  "/pics/show-hidden.svg"
  "Show hidden (dot-) files in the workspace file trees"
  (overConfig $ \c -> c { cfgUi = (cfgUi c)
      { uiShowHiddenFiles = not (uiShowHiddenFiles (cfgUi c)) } })
  (uiShowHiddenFiles . cfgUi)

-- | Toggle intercepting the tmux @C-b@ prefix in terminals: @C-b w@ activates
-- the Terminals pane and other prefix keys run the equivalent tmux command
-- (so they work in control-mode tabs, where a raw @C-b@ chord otherwise just
-- types @^B@).  The interception state machine lives in JS
-- (@window.LeksahTmux@); this flips the setting that each window mirrors into it.
commandToggleTmuxIntercept = CommandIDEToggleAction
  ""  -- menu-only: no toolbar icon
  "Intercept the tmux Ctrl+B prefix in terminals (C-b w shows the Tmux pane)"
  (overConfig $ \c -> c { cfgTerminal = (cfgTerminal c)
      { tcTmuxPrefix = not (tcTmuxPrefix (cfgTerminal c)) } })
  (tcTmuxPrefix . cfgTerminal)

-- | Cycle the side ("tall") pane: show -> auto-hide -> hide -> show.  Rendered
-- by a dedicated toolbar button that shows the current state (see Toolbar).
commandToggleTallPane = CommandIDEAction
  "/pics/sidebar.svg"
  "Side pane: show / auto-hide / hide"
  -- Per-window: cycle the visibility of the frontmost OS window's side pane.
  (\app -> modifyCell (appUi app) $ \u -> case u ^. activeWindow of
     Just aw -> u & webWindows . ix aw . wwTall %~ cycleTall
     Nothing -> u)

-- | Cycle the bottom pane (the errors/log/grep/changes area, grid area wide1):
-- show -> auto-hide -> hide -> show.  Like 'commandToggleTallPane' but for the
-- bottom row instead of the side column.
commandToggleWide1Pane = CommandIDEAction
  "/pics/bottombar.svg"
  "Bottom pane: show / auto-hide / hide"
  -- Per-window: cycle the visibility of the frontmost OS window's bottom pane.
  (\app -> modifyCell (appUi app) $ \u -> case u ^. activeWindow of
     Just aw -> u & webWindows . ix aw . wwWide1 %~ cycleTall
     Nothing -> u)

-- | Next side-pane visibility in the cycle.
cycleTall :: TallVisibility -> TallVisibility
cycleTall v = if v == maxBound then minBound else succ v

-- | View ▸ Zoom In / Zoom Out / Actual Size (⌘+/⌘−/⌘0): the whole OS window's
-- page zoom, browser-style — layout as well as text (see '_wwZoom').
--
-- Deliberately much simpler than 'paneFontAdjust' below: zoom is a property of
-- the OS WINDOW, so 'activeWindow' is the entire target lookup.  There is no
-- focused-leaf resolution, no tmux font-convert detour and no consolidate,
-- because nothing about a window's zoom can split or merge a tmux window.  On
-- macOS the single menubar acts on the key window, which is exactly what
-- '_activeWindow' tracks.
commandZoomIn, commandZoomOut, commandZoomReset :: Command
commandZoomIn = CommandIDEAction
  "" "Zoom this window in"      (zoomAdjust (stepZoom 1))
commandZoomOut = CommandIDEAction
  "" "Zoom this window out"     (zoomAdjust (stepZoom (-1)))
commandZoomReset = CommandIDEAction
  "" "Reset this window's zoom" (zoomAdjust (const 100))

zoomAdjust :: (Int -> Int) -> AppAction
zoomAdjust f app = modifyCell (appUi app) $ \u -> case u ^. activeWindow of
  Just aw -> u & webWindows . ix aw . wwZoom %~ (clampZoom . f)
  Nothing -> u

-- | The zoom rungs, in percent — the familiar browser ladder.  ⌘0 goes to 100
-- directly rather than walking back down it.  Kept non-empty by construction;
-- 'zoomMin'\/'zoomMax' are the ends (spelled out rather than @head@\/@last@,
-- which are -Wx-partial errors here).
zoomLadder :: [Int]
zoomLadder = [50, 67, 75, 80, 90, 100, 110, 125, 150, 175, 200, 250, 300]

zoomMin, zoomMax :: Int
zoomMin = 50
zoomMax = 300

-- | Step @n@ rungs from the current percent.  A value that is not ON the ladder
-- (a hand-edited session file, or a rung removed by a later edit to
-- 'zoomLadder') snaps to the nearest rung first, so the next ⌘+ always does
-- something sensible instead of nothing.  Stepping past either end stays there.
stepZoom :: Int -> Int -> Int
stepZoom n cur =
    case drop (max 0 (min (length zoomLadder - 1) (nearest + n))) zoomLadder of
      (z : _) -> z
      []      -> cur   -- unreachable: the index is clamped into the ladder
  where
    nearest = snd (minimum [ (abs (z - cur), i) | (i, z) <- zip [0 :: Int ..] zoomLadder ])

-- | Keep a zoom percentage inside the ladder's range.  Applied on the way in
-- from a session file as well as on every step.
clampZoom :: Int -> Int
clampZoom = max zoomMin . min zoomMax

-- | Close the active editor or terminal tab.  The actual close happens in the
-- reflex network (it's tab state), so this just signals a request that
-- 'IDE.Web.Main' picks up; closing a terminal this way detaches from tmux
-- rather than killing the session.
commandFileClose = CommandIDEAction
  "/pics/tango/actions/window-close.svg"
  "Close the active source file or terminal"
  (const requestCloseActivePane)

-- | File ▸ New Window (⌘N): open a fresh, empty OS window.  The library can't
-- create a native window, so the action just drops a request; the wkwebview
-- front end ('IDE.Web.MacMenu') registers the handler that mints a 'WindowId'
-- and asks the ObjC glue to create the NSWindow + WKWebView.  A no-op on
-- warp/webkitgtk (one browser tab is the whole UI).
commandNewWindow :: Command
commandNewWindow = CommandIDEAction
  ""
  "Open a new window"
  (const requestNewWindow)

-- | File ▸ Add Server… (and the Terminals-tree row): register an ssh host in
-- the remote-hosts setting.  A plain 'CommandIDEAction', so both the web
-- menubar and the native menus dispatch it generically; the action drops a
-- token on the "IDE.Web.AddServerRequest" bridge and the reflex modal in
-- 'IDE.Web.Main' does the actual work.
commandAddServer :: Command
commandAddServer = CommandIDEAction
  ""
  "Add an ssh server to the Terminals tree"
  (const requestAddServer)

-- | AI ▸ Grab Region: select a screen rectangle and drop its PNG path into the
-- terminal named by the capture-target setting.  The orchestration
-- (permission probe → crosshair or in-leksah overlay+snapshot) lives in
-- 'IDE.Web.Main'; this just drops a request.
commandGrabRegion :: Command
commandGrabRegion = CommandIDEAction
  ""
  "Grab a screen region and send its image to the terminal"
  (const (requestRegionGrab Nothing))

-- The AI ▸ Send… commands drop a token; the front end ('IDE.Web.Main') reads the
-- active editor / current error and types the reference into the AI terminal.
commandSendSelection :: Command
commandSendSelection = CommandIDEAction
  ""
  "Send the selected lines (@file#Lx-Ly) to the AI terminal"
  (const (requestAIAction SendSelection))

commandSendFileRef :: Command
commandSendFileRef = CommandIDEAction
  ""
  "Send the current file (@file) to the AI terminal"
  (const (requestAIAction SendFileRef))

commandSendError :: Command
commandSendError = CommandIDEAction
  ""
  "Send the current error (location + message) to the AI terminal"
  (const (requestAIAction SendError))

commandFocusAITerminal :: Command
commandFocusAITerminal = CommandIDEAction
  ""
  "Focus the AI terminal pane"
  (const (requestAIAction FocusAITerminal))

-- | Start a new Claude Code session in the active project's directory (toolbar
-- + AI menu).  A no-op when no project is active or @claude@ isn't on PATH
-- (runClaudeCmd just opens nothing).
commandClaudeNew :: Command
commandClaudeNew = CommandIDEAction
  "/pics/tree-claude.svg"
  "Start a Claude Code session in the active project"
  (\app -> readCell (wsCell (appWorkspace app)) >>=
      mapM_ (runClaudeCmd . ClaudeNew . prDir) . activeProject)

-- | Continue the most recent Claude Code session in the active project.
commandClaudeContinue :: Command
commandClaudeContinue = CommandIDEAction
  ""
  "Continue the most recent Claude Code session in the active project"
  (\app -> readCell (wsCell (appWorkspace app)) >>=
      mapM_ (runClaudeCmd . ClaudeContinue . prDir) . activeProject)

-- | View ▸ Bigger/Smaller/Reset Pane Font (⌥⌘=/⌥⌘−/⌥⌘0): adjust the FOCUSED pane's
-- font size in the active leksah window.  Per-pane fonts are the point of
-- the native split system: a pane shows a whole tmux window, so two tmux
-- panes with different font sizes can never share a window.  A no-op when
-- the active tab isn't a leksah window (or nothing is focused).
commandFontBigger, commandFontSmaller, commandFontReset :: Command
commandFontBigger = CommandIDEAction
  ""
  "Increase the focused split's font size"
  (paneFontAdjust (\eff -> Just (eff + 1)))

commandFontSmaller = CommandIDEAction
  ""
  "Decrease the focused split's font size"
  (paneFontAdjust (\eff -> Just (eff - 1)))

commandFontReset = CommandIDEAction
  ""
  "Reset the focused split's font size to the preference"
  (paneFontAdjust (const Nothing))

-- | Apply a font-size edit to the focused pane of the active OS window's
-- active leksah window.  @f@ maps the current EFFECTIVE size (override, else
-- the global monospace setting) to the new override; 'Nothing' = follow the
-- setting.  A tmux pane in a MULTI-pane window is isolated first (per-pane
-- fonts can't share a tmux window) via the font-convert queue — Main's
-- driver runs the minimal-path conversion and then applies @f@.
-- | What a pane-font change acts on: a focused LEAF inside the active leksah
-- window, or the active wide0 TAB itself.
data FontTarget
  = FTLeaf Text LeafId PaneContent   -- ^ leksah window id, leaf, its content
  | FTTab TabKey                     -- ^ a plain wide0 tab (editor, browser, …)

-- | Resolve the pane a font change aims at.  Mirrors 'activeAIPaneRef' (see
-- "IDE.Web.AISession"), which already encodes the rule the whole UI uses:
-- active OS window → its active wide0 tab → the focused leaf if that tab is a
-- leksah window, else the tab itself.  Focus in the side\/bottom trees is
-- deliberately ignored, so ⌥⌘= keeps acting on the pane you were editing while
-- you click about in the Workspace tree.
activeFontTarget :: WebUi -> Maybe FontTarget
activeFontTarget ui = do
  a  <- ui ^. activeWindow
  ww <- M.lookup a (ui ^. webWindows)
  k  <- ww ^. wwActive
  case k of
    LeksahWinKey n
      | Just lw <- M.lookup n (ui ^. leksahWindows)
      , Just l  <- lwFocused lw
      , Just pc <- M.lookup l (lwPanes lw) -> Just (FTLeaf n l pc)
    _ -> Just (FTTab k)

-- | Which wide0 tabs have a resizable text body.  Kept separate from
-- 'IDE.Web.Session.viewLeafAllowed' although the two sets coincide today: that
-- one answers \"may live in a split leaf\", this one \"has text you can resize\",
-- and they will diverge as more panes gain the wrapper.
fontAdjustableTab :: TabKey -> Bool
fontAdjustableTab EditorKey{}  = True
fontAdjustableTab BrowserKey{} = True
fontAdjustableTab GitLogKey{}  = True
fontAdjustableTab ReviewKey{}  = True
fontAdjustableTab _            = False

paneFontAdjust :: (Int -> Maybe Int) -> AppAction
paneFontAdjust f app = do
  ui <- readCell (appUi app)
  case activeFontTarget ui of
    Nothing -> return ()
    Just (FTLeaf n l (PaneContent kind cur)) -> do
      multi <- case kind of
        PaneTmux w -> (> 1) <$> paneCountOfWindow w
        _          -> return False
      case kind of
        PaneTmux w | multi -> requestFontConvert (n, w, f)
        _ -> do
          defSize <- fcMonoSize . cfgFont <$> currentConfig (appConfig app)
          modifyCell (appUi app) $ \u ->
            let eff = fromMaybe defSize cur
                new = fmap (max 6 . min 72) (f eff)
                setFont lw = lw { lwPanes =
                    M.adjust (\pc -> pc { pcFontSize = new }) l (lwPanes lw) }
            in u & leksahWindows %~ M.adjust setFont n
          -- The new size may match a neighbouring tmux window's — merge
          -- them (Main's consolidateLw drains this).
          requestConsolidate n
    -- A plain tab: neither the tmux isolate detour nor the consolidate applies,
    -- structurally — a wide0 tab is never a 'PaneTmux' and never shares a tmux
    -- window with a neighbour, so there is nothing to break out and nothing to
    -- merge back.  Absent from the map = follow the preference, so a reset
    -- ('f' answering 'Nothing') deletes rather than storing a default.
    Just (FTTab k)
      | fontAdjustableTab k -> do
          defSize <- fcMonoSize . cfgFont <$> currentConfig (appConfig app)
          modifyCell (appUi app) $ tabFontSize %~ \m ->
            let eff = fromMaybe defSize (M.lookup k m)
            in maybe (M.delete k m) (\n -> M.insert k n m)
                     (fmap (max 6 . min 72) (f eff))
      | otherwise -> return ()

-- | A menu command that sends the tmux prefix (@C-b@, byte 0x02) followed by
-- @keys@ to the active terminal — exactly as if the shortcut had been typed
-- there.  @keys@ are the raw bytes that follow the prefix: a printable key like
-- @\"c\"@, or an escape sequence such as @\"\\ESC[A\"@ (Up) / @\"\\ESC1\"@
-- (M-1).  A no-op when no terminal is on screen (see 'IDE.Web.TerminalInput').
tmuxKey :: ByteString -> Command
tmuxKey keys = CommandIDEAction
  ""  -- menu-only: no toolbar icon
  "Send this tmux C-b shortcut to the active terminal"
  (const (sendToActiveTerminal (BS.cons 2 keys)))

-- | A pane command for the Terminal menu that works with EITHER kind of
-- terminal tab: on a control-mode (CC) tab it runs @ccCmd@ verbatim over the
-- control channel (a chord can't work there — keystrokes are @send-keys@'d
-- straight into the pane, bypassing tmux's prefix handling); on a classic PTY
-- tab it falls back to typing the @C-b chord@ (which works even for remote
-- @ssh -t@ sessions, where we can't run commands directly).
paneCmd :: Text -> ByteString -> Command
paneCmd ccCmd chord = CommandIDEAction
  ""  -- menu-only: no toolbar icon
  "Terminal split/pane command (control channel or C-b chord)"
  (const $ do
      done <- tmuxCommandActiveTerminal ccCmd
      unless done $ sendToActiveTerminal (BS.cons 2 chord))

-- | The Terminal menu's split commands.  Unlike 'paneCmd' these build a
-- @split-window@ that reproduces a directory window's environment — the new
-- pane re-enters the project's command prefix (a repl just inherits the
-- directory); see 'IDE.Web.TerminalInput.splitActiveTerminal'.
splitCmd :: Bool -> ByteString -> Command
splitCmd horizontal chord = CommandIDEAction
  ""  -- menu-only: no toolbar icon
  "Split the terminal pane (re-entering a directory window's environment)"
  (const (splitActiveTerminal horizontal chord))

-- | A menu command that toggles whether the active terminal's active tmux pane
-- is shown as a see-through, click-through hole in the window (macOS).  The work
-- happens in the reflex layer; this just signals it (see
-- 'IDE.Web.TransparencyRequest').
toggleTransparencyCmd :: Command
toggleTransparencyCmd = CommandIDEAction
  ""  -- menu-only: no toolbar icon
  "Make the active tmux pane transparent (a click-through hole)"
  (const requestToggleTransparency)

-- | A menu command that snaps another app's window over the active tmux pane
-- (which is made transparent so the window shows through), tracking the pane;
-- toggling it again unsnaps.  Needs macOS Accessibility permission (it asks).
-- See 'IDE.Web.SnapRequest' and the native side in @main/leksah-mac-menu.m@.
snapWindowCmd :: Command
snapWindowCmd = CommandIDEAction
  ""  -- menu-only: no toolbar icon
  "Snap another app's window over the active tmux pane (macOS)"
  (const requestSnapWindow)
