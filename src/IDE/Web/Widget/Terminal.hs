{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
-- | A terminal pane for the web UI.
--
-- The front end is xterm.js (loaded as a global by 'IDE.Web.Main', the same
-- way codemirror is).  The back end is a real pseudo-terminal running the
-- user's shell, spawned locally with @posix-pty@: the web executables run on
-- the same host as the browser, so we can bridge the PTY to xterm.js over
-- jsaddle:
--
--   * xterm @onData@ (keystrokes)  -> 'writePty'  (to the shell)
--   * a reader thread 'readPty'    -> reflex event -> @term.write@ (to screen)
--
-- All JS calls stay on the reflex event loop; only the blocking PTY read runs
-- on its own thread, handing bytes back through a trigger event.
module IDE.Web.Widget.Terminal
  ( terminalCss
  , terminalWidget
  , listTerminalSessions
  , killTerminalSession
  , TmuxWindow(..)
  , TmuxPane(..)
  , isClaudePane
  , listTerminalTree
  , listRemoteTerminalTree
  , remoteTabTree
  , remoteTabHostTarget
  , createRemoteSession
  , selectRemoteTmuxWindow
  , selectRemoteTmuxPane
  , killRemoteTmuxSession
  , killRemoteTmuxWindow
  , killRemoteTmuxPane
  , newRemoteTmuxWindow
  , zoomRemoteTmuxPane
  , breakRemoteTmuxPane
  , moveRemoteTmuxPane
  , renameRemoteTmuxSession
  , renameRemoteTmuxWindow
  , reapControlClients
  , createTerminalSession
  , openFileInEditor
  , cleanupStaleTwinPanes
  , resolveEditorCmd
  , shellQuoteArg
  , replSessionName
  , ffcabalTmuxEnv
  , findReplWindow
  , selectTmuxWindowById
  , selectTmuxWindow
  , selectTmuxPane
  , killTmuxWindow
  , killTmuxPane
  , newTmuxWindow
  , zoomTmuxPane
  , breakTmuxPane
  , killTmuxPaneId
  , breakTmuxPaneId
  , windowIndexOfPane
  , paneCountOfSession
  , moveTmuxPane
  , moveTmuxWindow
  , selectTmuxWindowId
  , selectTmuxPaneId
  , panesOfWindow
  , joinTmuxPane
  , joinTmuxPaneFull
  , breakTmuxPaneTo
  , windowLayoutString
  , movePaneToPane
  , selectWindowLayout
  , swapTmuxPanes
  , renameTmuxSession
  , renameTmuxWindow
  , activePaneId
  , paneGeometry
  , sessionOfPane
  , notifyTerminalBell
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (try, catch, SomeException)
import Control.Lens ((^.))
import Control.Monad (void, forM_, when, unless, mfilter)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.List (find, intercalate, nub)
import Data.Map (Map)
import qualified Data.Map as M
       (empty, singleton, fromList, fromListWith, unionWith, toAscList, toList,
        map, lookup, findWithDefault)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
       (unpack, pack, splitOn, stripPrefix, intercalate, strip, words, lines,
        null, breakOn, drop, isPrefixOf, replace)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.Read (readMaybe)

import Clay (height, width, pct, (?), (-:), Css, none, None(..))
import qualified Clay (display)

import Language.Javascript.JSaddle
       (jsg, js, jss, js0, js1, js2, js4, fun, new, obj, valToText,
        valToNumber, valToBool, liftJSM)
import GHCJS.DOM.Types (pToJSVal)

import IDE.DebugLog (focusLog)
import IDE.Problems.Types (Loc(..), Pos(..), pointRange)
import IDE.Web.Ctx (Ctx(..))
import IDE.Web.Widget.Menu (menu)
import qualified IDE.LSP as LSP

import Reflex
       (attach, attachWith, current, ffor, getPostBuild, holdDyn, never,
        leftmost, constDyn, fmapMaybe, switchHold,
        newTriggerEvent, delay, Event)
import Reflex.Dom.Core
       (elAttr, elAttr', dyn, (=:),
        _element_raw)

import IDE.Web.Widget.ResizeObserver (resizeObserverWithAttrs)

import System.Directory
       (findExecutable, getHomeDirectory,
        createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath ((</>), takeDirectory, takeFileName)
#if defined(mingw32_HOST_OS)
import IDE.Web.ConPty
       (spawnWithPty, readPty, writePty, resizePty, threadWaitReadPty)
#elif defined(ghcjs_HOST_OS)
import IDE.Web.NoPty
       (dummyPty, writePty, resizePty)
import IDE.Web.DemoTerminals
       (demoTerminals, demoTerminalB64, demoCreateSession, demoCreatedSessions,
        demoKillSession)
#else
import System.Posix.Pty
       (spawnWithPty, readPty, writePty, resizePty, threadWaitReadPty)
import System.Posix.Signals (signalProcess, sigKILL)
#endif
import System.Process (readProcessWithExitCode, createProcess, proc)
import System.Exit (ExitCode(ExitSuccess))

import IDE.Web.Claude
       (claudeSessionsFor, csId, csTitle, claudeLiveOwners, ClaudeLive(..))
import IDE.Web.Events (TerminalEvents(..))
import IDE.Web.ReplTmux
       (tmuxSocket, tmuxCmd, replSessionName, ffcabalTmuxEnv, findReplWindow,
        findRunPane, liveRunPanes, isBackingRunKey, selectTmuxWindowById,
        getLoginShell, interactiveShellArgs, writeTmuxConf, clipboardCopyCmd)
import IDE.Web.TerminalInput (registerTerminalPty, unregisterTerminalPty)
import IDE.Web.TerminalRefresh (monitorSessionName)
import IDE.Web.SnapRequest (requestSnapPane)
import IDE.Web.Frame (MonadWidget, performEvent, performEvent_)

terminalCss :: Css
terminalCss = do
    ".terminal" ? do
        height (pct 100)
        width (pct 100)
    -- Match native terminal weight (e.g. iTerm2).  macOS defaults web text to
    -- subpixel antialiasing, which renders noticeably bolder than a native
    -- terminal; grayscale (antialiased) matches the lighter native rendering.
    ".xterm" ? ("-webkit-font-smoothing" -: "antialiased")
    -- The pane box reserves a uniform inset around the grid (terminalPanePad),
    -- and row quantisation leaves a little vertical slack; both areas show the
    -- pane's OWN background.  xterm paints its background on .xterm-viewport
    -- (inline, from its theme) which is --leksah-terminal-bg, so match the pane
    -- to that var — otherwise the inset frames the terminal in the editor-area
    -- colour instead of the terminal's.  (The var is theme-synced for
    -- light/dark; the literal is just the pre-JS fallback.)
    ".terminal-cc-pane" ? ("background" -: "var(--leksah-terminal-bg, rgb(16,16,16))")
    -- The pane box reserves a uniform inset around the grid (see
    -- 'terminalPanePad'); the whole cell grid, being quantised to whole rows,
    -- is a little shorter than the pane's content box, so centre it vertically
    -- to split that slack evenly top/bottom rather than pooling it below.
    ".terminal-cc-pane .xterm" ? do
        "display" -: "flex"
        "flex-direction" -: "column"
        "justify-content" -: "center"
    -- tmux turns on xterm's mouse mode, which makes xterm switch the cursor to
    -- the default arrow (.xterm.enable-mouse-events).  We want the usual text
    -- (I-beam) cursor over the terminal, so override it back (our stylesheet is
    -- concatenated after xterm.css, so this equal-specificity rule wins).
    ".xterm.enable-mouse-events" ? ("cursor" -: "text")
    -- ...but keep the hand cursor when hovering a clickable link (the terminal
    -- file links): xterm toggles .xterm-cursor-pointer on the same element, so
    -- re-assert it *after* the rule above or our text cursor would mask it.
    ".xterm.xterm-cursor-pointer" ? ("cursor" -: "pointer")
    ".xterm .xterm-cursor-pointer" ? ("cursor" -: "pointer")
    -- CC pane dividers: the whole tmux separator gutter is the grab strip for
    -- drag-to-resize; the visible 1px line (.divider-line) sits centered in it
    -- and brightens on hover/drag.  Geometry is inline (per-layout).
    -- Above a tunnel iframe (z-index 5): the grab strip and its line must sit
    -- on top of an adjacent pane's iframe, not under it.
    -- The connection-error / Retry view is keyboard-driven (Retry autofocused,
    -- Tab to Close, Enter/Space activate, Escape closes), so make the focused
    -- button clearly visible with an on-brand blue ring.
    ".terminal-cc-error button:focus" ? do
        "outline" -: "2px solid var(--leksah-selection)"
        "outline-offset" -: "1px"
    ".terminal-cc-divider" ? ("z-index" -: "10")
    ".terminal-cc-divider.vert" ? ("cursor" -: "col-resize")
    ".terminal-cc-divider.horiz" ? ("cursor" -: "row-resize")
    ".terminal-cc-divider .divider-line" ?
        ("background" -: "var(--leksah-border-line)")
    ".terminal-cc-divider:hover .divider-line" ?
        ("background" -: "var(--leksah-border-line-hi)")
    ".terminal-cc-divider.dragging .divider-line" ?
        ("background" -: "var(--leksah-border-line-hi)")
    -- Hovering the grab strip also glows it blue, matching the native-leaf
    -- dividers (and the side/bottom-bar resize strips) — every draggable pane
    -- boundary answers the mouse the same way.
    ".terminal-cc-divider:hover" ?
        ("background" -: "var(--leksah-hover)")
    ".terminal-cc-divider.dragging" ?
        ("background" -: "var(--leksah-hover)")
    -- Per-pane markers (rendered per pane by 'renderHlSegments'): pure
    -- geometry — they draw NOTHING.  Boundary lines are owned exclusively by
    -- the dividers/layout chrome (one line per boundary: CC gutters' mid
    -- .divider-line, native dividers' boundary line, .area-* border-left,
    -- .wide1-divider/.findbar border-top), so nothing can double and no line
    -- appears at the tab-row or screen edges.  The active pane's WHITE ring
    -- lives on the glow overlay below, not on any pane element.  Which
    -- marker is `.active` (= carries the glow's anchor) is model/CC state:
    -- 'applyPaneHighlight' toggles the class per widget, gated by
    -- `.tab-active` (the per-window 'activePane' Dynamic in Tabs.hs), so
    -- exactly one anchor exists per OS window.  `.flip-target` /
    -- `.menu-target` are the flipper hold-preview and ⌘W close-menu
    -- overrides — transient rings drawn directly on the marker.
    ".terminal-cc-hl" ? do
        "box-sizing" -: "border-box"
        "z-index" -: "5"
    ".tab-active .terminal-cc-hl.active" ?
        ("anchor-name" -: "--leksah-active-pane")
    -- Per-area overrides: each area's actives anchor their own overlay
    -- variant, because during the bottom bar's transform-only auto-hide
    -- reveal each area's content moves DIFFERENTLY and the overlays must
    -- ride the matching transform (see the wide1-auto rules in Layout.hs).
    -- The tall variant also drops border-left (screen edge).
    ".tab.area-tall.tab-active .terminal-cc-hl.active" ?
        ("anchor-name" -: "--leksah-active-pane-tall")
    ".tab.area-wide1.tab-active .terminal-cc-hl.active" ?
        ("anchor-name" -: "--leksah-active-pane-wide1")
    ".terminal-cc-hl.flip-target" ? do
        "border" -: "1px solid var(--leksah-pane-ring-active)"
        "z-index" -: "11"
    ".terminal-cc-hl.menu-target" ? do
        "border" -: "1px solid var(--leksah-pane-ring-active)"
        "z-index" -: "11"
    -- ⌘W "Hide Window": the whole terminal (all its panes) is the target.
    ".terminal-cc.menu-target" ?
        ("outline" -: "1px solid var(--leksah-pane-ring-active)")
    -- The native-split view leaves' markers (see TerminalCC's leaf
    -- renderers): one per leaf, covering the whole leaf exactly; like the
    -- tmux markers they draw nothing — `active` (from the model's
    -- lwFocused) just carries the glow's anchor.
    ".pane-chrome" ? do
        "position" -: "absolute"
        "inset" -: "0"
        "box-sizing" -: "border-box"
        "pointer-events" -: "none"
        "z-index" -: "5"
    ".tab-active .pane-chrome.active" ?
        ("anchor-name" -: "--leksah-active-pane")
    ".tab.area-tall.tab-active .pane-chrome.active" ?
        ("anchor-name" -: "--leksah-active-pane-tall")
    ".tab.area-wide1.tab-active .pane-chrome.active" ?
        ("anchor-name" -: "--leksah-active-pane-wide1")
    -- The active pane's soft glow (the element renders in Main.hs at the
    -- .leksah root): CSS anchor positioning ties it to whichever chrome
    -- element currently declares --leksah-active-pane (the rules above and
    -- the .tab.tab-active outline in Tabs.hs) — the browser tracks the
    -- anchor through layout changes, tab switches and resizes with NO JS.
    -- The fallbacks park it offscreen at zero size when no anchor exists
    -- (no active pane / the active tab hidden).  pointer-events:none and
    -- z-index 25 as the old overlay had; the glow spills over the bars,
    -- which is the point of it being one fixed top-level element.
    -- CAVEAT: anchor() resolves from LAYOUT geometry — a transform (the
    -- tall/wide1 auto-hide reveal translates pane content) does not move
    -- the glow; the ring (in-tree) stays correct, the glow catches up when
    -- the reveal settles.
    -- The overlay ALSO carries the active pane's 1px max-contrast ring (the
    -- white border lives here, never on pane elements).  It is 1px WIDER and
    -- TALLER than its anchor (border-box): the left/top borders land on the
    -- pane's own first pixel — the boundary-line position owned by the pane
    -- to the right/below of a divider — and the right/bottom borders land on
    -- the first pixel AFTER the pane, i.e. the neighbouring pane's boundary
    -- line.  Where those pixels hold a grey divider/chrome line, z-index 25
    -- paints the white over it (still one pixel-wide line); at the screen's
    -- right/bottom edges the 1px overhang is clipped away (no line drawn at
    -- screen edges).
    ".leksah-pane-glow" ? do
        "position" -: "fixed"
        "top" -: "anchor(--leksah-active-pane top, -10000px)"
        "left" -: "anchor(--leksah-active-pane left, -10000px)"
        "width" -: "calc(anchor-size(--leksah-active-pane width, 0px) + 1px)"
        "height" -: "calc(anchor-size(--leksah-active-pane height, 0px) + 1px)"
        "box-sizing" -: "border-box"
        "border" -: "1px solid var(--leksah-pane-ring-active)"
        "pointer-events" -: "none"
        "z-index" -: "25"
        "box-shadow" -: "0 0 64px var(--leksah-shadow-glow)"
    -- The tall-area variant (second overlay div; anchors declared by the
    -- side tabs' rules in Tabs.hs): side tabs sit ON the screen's left edge,
    -- where no line must be drawn — same glow and ring, minus border-left.
    ".leksah-pane-glow.glow-tall" ? do
        "top" -: "anchor(--leksah-active-pane-tall top, -10000px)"
        "left" -: "anchor(--leksah-active-pane-tall left, -10000px)"
        "width" -: "calc(anchor-size(--leksah-active-pane-tall width, 0px) + 1px)"
        "height" -: "calc(anchor-size(--leksah-active-pane-tall height, 0px) + 1px)"
        "border-left" -: "none"
    -- The wide1 (bottom bar) variant (third overlay div): the bar is a
    -- transform-parked overlay in auto-hide mode, so its overlay carries the
    -- bar's own parked/revealed transforms (Layout.hs wide1-auto rules).
    ".leksah-pane-glow.glow-wide1" ? do
        "top" -: "anchor(--leksah-active-pane-wide1 top, -10000px)"
        "left" -: "anchor(--leksah-active-pane-wide1 left, -10000px)"
        "width" -: "calc(anchor-size(--leksah-active-pane-wide1 width, 0px) + 1px)"
        "height" -: "calc(anchor-size(--leksah-active-pane-wide1 height, 0px) + 1px)"
    -- With the side pane hidden or parked (auto mode, unrevealed) the editor
    -- column starts AT the screen's left edge — the boundary line is gated
    -- away there (see the .tall-divider ::before gating in Layout.hs), and
    -- the ring's left border must vanish with it: no line at screen edges.
    ".leksah.tall-hide .leksah-pane-glow" ?
        ("border-left" -: "none")
    ".leksah.tall-auto:not(:has(.tall-sensor:hover, .area-tall:hover, .area-tall:focus-within, .tall-divider:hover)) .leksah-pane-glow" ?
        ("border-left" -: "none")
    ".leksah.tall-auto.tall-suppress .leksah-pane-glow" ?
        ("border-left" -: "none")
    -- …but ONLY a pane actually sitting on that edge loses the line: an
    -- active pane whose left edge is an interior divider keeps its white
    -- ring (unconditional suppression left the divider's grey showing).
    -- Flush-left is the edge-left class, stamped from model geometry
    -- (TerminalCC leafStyle / renderHlSegments).  An anchor provably NOT
    -- flush-left (its leaf, or its own layout column, is off the edge)
    -- additionally declares --leksah-active-left-line, and a dedicated 1px
    -- overlay (.leksah-pane-left-line, rendered beside the glow divs in
    -- Main.hs) anchors to it — parked offscreen by the fallbacks whenever no
    -- non-flush anchor exists (flush pane, plain tab body, no active pane).
    -- anchor-name is not additive, so these higher-specificity rules must
    -- re-declare the base name the overridden rules above set.
    -- TWO cautionary tales live here, learned 2026-08-03:
    --   * `:has()` cannot nest inside `:has()` — WebKit silently DROPS the
    --     whole rule (so "suppress when flush", whose plain-tab case needs
    --     `:not(:has(.terminal-cc))` inside an outer `:has()`, can't work).
    --   * NEVER gate on a class-argument `:has()` at the `.leksah` root: it
    --     re-evaluates on subtree mutations, and xterm/CodeMirror mutate on
    --     EVERY keystroke — the resulting style-invalidation storm pegged
    --     WebContent at 100% and made typing take ~1s/char.  The existing
    --     root `:has()` rules are `:hover`/`:focus-within`-argument only
    --     (pseudo-state invalidation — cheap); keep it that way.
    ".tab.tab-active:not(.area-tall):not(.area-wide1) .terminal-cc-leaf:not(.edge-left) .terminal-cc-hl.active, .tab.tab-active:not(.area-tall):not(.area-wide1) .terminal-cc-hl.active:not(.edge-left), .tab.tab-active:not(.area-tall):not(.area-wide1) .terminal-cc-leaf:not(.edge-left) .pane-chrome.active" ?
        ("anchor-name" -: "--leksah-active-pane, --leksah-active-left-line")
    ".leksah-pane-left-line" ? do
        "position" -: "fixed"
        "top" -: "anchor(--leksah-active-left-line top, -10000px)"
        "left" -: "anchor(--leksah-active-left-line left, -10000px)"
        "width" -: "1px"
        "height" -: "calc(anchor-size(--leksah-active-left-line height, 0px) + 1px)"
        "background" -: "var(--leksah-pane-ring-active)"
        "pointer-events" -: "none"
        "z-index" -: "25"
    -- The ⌘-drag pane move's preview (leafDragJs): the ring+glow look of the
    -- active-pane overlay, but positioned by JS and free to animate between
    -- arbitrary rects (drop candidates, tab buttons, the parked-on-source
    -- no-op state).
    ".leksah-drag-shadow" ? do
        "position" -: "fixed"
        "box-sizing" -: "border-box"
        "border" -: "1px solid var(--leksah-pane-ring-active)"
        "box-shadow" -: "0 0 64px var(--leksah-shadow-glow)"
        "pointer-events" -: "none"
        "z-index" -: "26"
        "transition" -: "top .12s ease-out, left .12s ease-out, width .12s ease-out, height .12s ease-out"
    -- While dragging: the real ring would fight the preview — hide it; the
    -- whole page shows a grabbing cursor and never starts a text selection.
    ".leksah.leksah-pane-dragging .leksah-pane-glow, .leksah.leksah-pane-dragging .leksah-pane-left-line" ?
        ("display" -: "none")
    -- Only the KEY OS window marks its active pane.  The ring and glow say
    -- "the keyboard is in this pane", and there is one keyboard: a second
    -- window drawing the same mark claims a focus it does not have, and with
    -- two windows side by side you cannot tell which one a keystroke goes to.
    -- The class is stamped on the root from the model's active window (see
    -- 'rootAttrD' in "IDE.Web.Main"); every glow variant (glow-tall,
    -- glow-wide1) shares the base class and so is covered.
    --
    -- The escape hatch: a pane MOVE in flight, aimed at this window, must be
    -- able to show its target here even though the window is not key (a drag
    -- does not change the key window until the mouse comes up).  That is what
    -- @.leksah-drop-target@ is for.  Nothing sets it yet — a ⌘-drag lives
    -- entirely in one document ('leafDragJs' pointer capture, and
    -- 'parseLeafDrop' has no OS-window field), so a pane cannot presently be
    -- dropped into another OS window at all.  Whoever adds that: stamp this
    -- class on the target window's root for the duration of the drag and the
    -- mark comes back on its own.
    ".leksah.window-unfocused:not(.leksah-drop-target) :is(.leksah-pane-glow, .leksah-pane-left-line)" ?
        ("display" -: "none")
    ".leksah.leksah-pane-dragging" ? do
        "cursor" -: "grabbing"
        "user-select" -: "none"
        "-webkit-user-select" -: "none"
    ".leksah.leksah-pane-dragging *" ? do
        "cursor" -: "grabbing !important"
        "user-select" -: "none !important"
        "-webkit-user-select" -: "none !important"
    -- A pane owned by a jsaddle-terminal app (see TerminalCC's tunnel): the
    -- iframe overlays the pane and the xterm underneath is hidden (it keeps
    -- consuming any non-frame output, so it is current again the moment the
    -- app exits and the tunnel closes).
    ".terminal-cc-pane-tunnel > .terminal" ? Clay.display none
    ".terminal-cc-iframe" ? do
        "position" -: "absolute"
        "left" -: "0"
        "top" -: "0"
        "width" -: "100%"
        "height" -: "100%"
        "border" -: "0"
        "background" -: "var(--leksah-terminal-bg, rgb(16,16,16))"
        "z-index" -: "5"
    -- A pane backing a leksah view (an editor / git log converted to a pane by
    -- ⌘D — see paneOverlays): the leksah widget overlays the pane, the xterm
    -- underneath is hidden exactly like a tunnel pane's.
    ".terminal-cc-pane-overlay > .terminal" ? Clay.display none
    ".terminal-cc-overlay" ? do
        "position" -: "absolute"
        "left" -: "0"
        "top" -: "0"
        "width" -: "100%"
        "height" -: "100%"
        "overflow" -: "hidden"
        "background" -: "var(--leksah-terminal-bg, rgb(16,16,16))"
        "z-index" -: "5"
    -- Native split panes (see IDE.Web.SplitLayout / TerminalCC): each leaf is
    -- an absolutely positioned box whose body fills it (a whole tmux window's
    -- panes, or a native view).
    ".terminal-cc-leaf-body" ? do
        "position" -: "absolute"
        "inset" -: "0"
        "overflow" -: "hidden"
    -- Draggable gutters BETWEEN native leaves (armed by LeksahNativeDrag).
    -- Like the CC tmux dividers above they carry a visible 1px mid-grey
    -- .divider-line centred in the grab strip (geometry inline, per
    -- orientation — see TerminalCC's divider render), brightened on hover on
    -- top of the blue strip glow.
    ".terminal-cc-native-divider .divider-line" ?
        ("background" -: "var(--leksah-border-line)")
    ".terminal-cc-native-divider:hover .divider-line" ?
        ("background" -: "var(--leksah-border-line-hi)")
    ".terminal-cc-native-divider:hover" ?
        ("background" -: "var(--leksah-hover, rgba(255,255,255,0.12))")
    -- A native VIEW pane (editor / git log in the split layout): covers the
    -- leaf's (empty) terminal chrome, same technique as .terminal-cc-overlay.
    ".terminal-cc-view-leaf" ? do
        "position" -: "absolute"
        "inset" -: "0"
        "overflow" -: "hidden"
        "background" -: "var(--leksah-bg, rgb(30,30,30))"
        "z-index" -: "5"
    -- The ⌘W terminal pane close menu (Kill / Hide / Move / Cancel).  It renders
    -- INSIDE its target pane; this overlay covers the pane and CSS-centres the menu
    -- box (grid place-items) — no JS geometry.  All interaction is reflex.
    ".pane-close-overlay" ? do
        "position" -: "absolute"
        "inset" -: "0"
        "display" -: "grid"
        "place-items" -: "center"
        "z-index" -: "6"
    ".pane-close-menu" ? do
        "min-width" -: "180px"
        "padding" -: "6px"
        "border-radius" -: "8px"
        "background" -: "var(--leksah-surface)"
        "color" -: "var(--leksah-fg-muted)"
        "border" -: "1px solid var(--leksah-border-control)"
        "box-shadow" -: "0 0 64px var(--leksah-shadow-glow)"
        "display" -: "flex"
        "flex-direction" -: "column"
        "outline" -: "none"
    ".pane-close-opt" ? do
        "display" -: "block"
        "width" -: "100%"
        "text-align" -: "left"
        "padding" -: "6px 12px"
        "border" -: "0"
        "background" -: "transparent"
        "color" -: "inherit"
        "border-radius" -: "5px"
        "cursor" -: "pointer"
        "font" -: "inherit"
        "white-space" -: "nowrap"
    ".pane-close-opt.selected" ? do
        "background" -: "var(--leksah-selection)"
        "color" -: "var(--leksah-fg)"
    -- The "save changes before closing?" prompt (⌘W / File ▸ Close on a dirty
    -- editor).  Same box + option styling as the pane close menu, but a fixed
    -- full-screen overlay (an editor tab isn't a tmux pane to nest inside).
    ".save-close-overlay" ? do
        "position" -: "fixed"
        "inset" -: "0"
        "z-index" -: "1000"
        "display" -: "grid"
        "place-items" -: "center"
        "background" -: "var(--leksah-scrim)"
    ".save-close-menu" ? do
        "min-width" -: "260px"
        "padding" -: "12px"
        "border-radius" -: "8px"
        "background" -: "var(--leksah-surface)"
        "color" -: "var(--leksah-fg-muted)"
        "border" -: "1px solid var(--leksah-border-control)"
        "box-shadow" -: "0 0 64px var(--leksah-shadow-glow)"
        "display" -: "flex"
        "flex-direction" -: "column"
        "outline" -: "none"
    ".save-close-menu p" ? do
        "margin" -: "0 0 8px 0"
        "padding" -: "0 4px"

-- | A terminal pane.  The 'Int' is the terminal's id; it maps to a tmux
-- session named @leksah-N@ so the shell survives a leksah restart (see
-- 'listTerminalSessions').  The 'Event' fires whenever this terminal's tab is
-- selected; the terminal grabs keyboard focus then (and on creation), so it
-- takes input without an extra click.

terminalWidget
  :: forall t m . MonadWidget t m
  => Ctx t                  -- ^ for Ctrl/Cmd-click identifier lookup in metadata
  -> Text -> Event t () -> m (Event t TerminalEvents)
terminalWidget ctx termId selectedE = do
#if defined(ghcjs_HOST_OS)
  -- Browser demo: no PTY.  The xterm below renders a canned session dump
  -- (window.leksahDemoTerminals) written once after it is built; writes and
  -- resizes to the dummy fail with a plain IOError every call site already
  -- swallows (ignorePtyError / try).
  let pty = dummyPty
#else
  -- A real PTY running the user's shell.  Created up front so the xterm
  -- `onData` callback (wired below) can write keystrokes to it.
  pty <- liftIO $ do
      shell <- getLoginShell
      -- Inherit the environment but force a sensible TERM (without it the shell
      -- can't bind the arrow-key sequences).
      baseEnv <- getEnvironment
      let env = ("TERM", "xterm-256color") : filter ((/= "TERM") . fst) baseEnv
      -- Attach to the tmux session by its stable id (@termId@, e.g. "$3"); the
      -- session was created up front (see 'createTerminalSession') or already
      -- existed.  Attaching by id (not name) means a rename doesn't break the
      -- attach.  The reader thread sees tmux's redraw on attach.  Without tmux on
      -- PATH, run the shell directly (`-i` for the line editor) — no persistence.
      mbTmux <- findExecutable "tmux"
      (cmd, args) <- case mbTmux of
          Just tmux -> return (tmux, ["-L", tmuxSocket, "attach-session", "-t", T.unpack termId])
          Nothing -> return (shell, interactiveShellArgs)
      (pty, _ph) <- spawnWithPty (Just env) True cmd args (80, 24)
      -- Expose this PTY so the Tmux menu can inject `C-b X` prefix sequences into
      -- it when this terminal is the active one (see IDE.Web.TerminalInput).
      registerTerminalPty termId pty
      -- The vim-style pane bindings live in the tmux config, but `-f` only takes
      -- effect when the server first starts; a server left running by a previous
      -- leksah session keeps the old bindings.  Re-assert them here (idempotent)
      -- so they work without having to kill every terminal first.
      forM_ mbTmux $ \_ -> do
          mapM_ (\(k, d) -> tmuxCmd ["bind-key", k, "select-pane", d])
              [("h", "-L"), ("j", "-D"), ("k", "-U"), ("l", "-R")]
          mapM_ (\(k, d) -> tmuxCmd ["bind-key", "-r", k, "resize-pane", d, "5"])
              [("H", "-L"), ("J", "-D"), ("K", "-U"), ("L", "-R")]
          mapM_ (\(k, d) -> tmuxCmd ["bind-key", "-r", k, "resize-pane", d, "1"])
              [("C-h", "-L"), ("C-j", "-D"), ("C-k", "-U"), ("C-l", "-R")]
          tmuxCmd ["bind-key", "Tab", "last-window"]
          tmuxCmd ["bind-key", "BTab", "switch-client", "-l"]
          clipboardCopyCmd >>= mapM_ (\c -> tmuxCmd ["set", "-s", "copy-command", c])
          -- Re-assert focus reporting on the running server too (the -f config
          -- above only takes effect when the server first starts, so a server
          -- left over from before this setting existed wouldn't have it).
          tmuxCmd ["set", "-g", "focus-events", "on"]
          tmuxCmd ["set", "-g", "allow-passthrough", "on"]
          tmuxCmd ["set", "-g", "monitor-activity", "on"]
          tmuxCmd ["set", "-g", "monitor-silence", "15"]
          tmuxCmd ["set", "-g", "visual-activity", "off"]
          tmuxCmd ["set", "-g", "visual-silence", "off"]
          tmuxCmd ["set", "-g", "visual-bell", "off"]
          -- terminal-features is read when a client attaches; setting it here
          -- takes effect on the next attach (relaunch), not this one.
          tmuxCmd ["set", "-sa", "terminal-features", ",xterm-256color:RGB:hyperlinks"]
          -- Post a macOS notification when any window rings the bell (Claude Code
          -- does this when a teammate wants input / finishes).  The hook passes
          -- the belling window's ids to the helper script (run in the background
          -- so it never blocks tmux).
          notifyPath <- writeNotifyScript
          tmuxCmd [ "set-hook", "-g", "alert-bell"
                  , "run-shell -b \"sh " <> notifyPath <> " '#{session_id}' '#{window_index}'\"" ]
          -- Poke leksah whenever the current window/pane changes by *any* route
          -- (⌃B n/p, ⌃B w chooser, mouse, scripts), so the flipper/tab MRU and the
          -- highlight update at once instead of waiting for the 2 s poll.  The poke
          -- re-reads the tree; leksah then floats the focused terminal's new active
          -- pane to the MRU front (via activeFlipD).  Uses leksah-cmd's absolute
          -- path (it's on PATH in the dev shell); skipped if not found.
          -- Redirect the poke's output: tmux's run-shell surfaces a command's
          -- stdout (as an "ok" view on every select otherwise).
          findExecutable "leksah-cmd" >>= mapM_ (\lc ->
            mapM_ (\ev -> tmuxCmd ["set-hook", "-g", ev, "run-shell -b \"" <> lc <> " term-activity >/dev/null 2>&1\""])
                  ["after-select-window", "after-select-pane"])
      return pty
#endif

  -- Output from the shell arrives on this trigger event from the reader
  -- thread (started once the terminal exists, so the prompt isn't dropped).
  (outputE, triggerOutput) <- newTriggerEvent
  -- The terminal's window title (set by the shell/programs via OSC sequences),
  -- surfaced so the Terminals list pane can label this terminal.
  (titleE, triggerTitle) <- newTriggerEvent
  -- Ctrl+clicking a project-file path in the terminal output asks to open it at
  -- the given line/column (the link provider, wired below, fires this).
  (linkE, triggerLink) <- newTriggerEvent
  -- Ctrl/Cmd+clicking any identifier-like token instead asks to look it up in
  -- the metadata; this fires (token, clientX, clientY) for the click.
  (lookupE, triggerLookup) <- newTriggerEvent
  -- The shell rang the bell (xterm's onBell).  Since a client is attached
  -- viewing this session's current window, tmux's alert-bell hook won't fire for
  -- a bell there — so we catch it here and let leksah surface the attention.
  (bellE, triggerBell) <- newTriggerEvent
  -- Hovering a file link asks the LSP layer for a tooltip; the reply is fired
  -- here and pushed back into JS (LeksahTermLinks.resolveHover) on this widget's
  -- own reflex network, not from the LSP client thread.
  (hoverRespE, fireHoverResp) <- newTriggerEvent
  -- The attached tmux client exited (its PTY hit EOF): the session ended — e.g.
  -- the last window's shell was `exit`ed — so the tab should close instead of
  -- lingering with a dead "[exited]" screen.
  (exitedE, triggerExited) <- newTriggerEvent

  (resizeE, el) <- resizeObserverWithAttrs ("style" =: "height:100%;width:100%") $
      fst <$> elAttr' "div" ("class" =: "terminal") (pure ())
  -- pToJSVal, not toJSVal/MakeArgs marshalling: under the GHC JS backend the
  -- Element instance diverges (undefined closure entered in the args map) —
  -- same fix as ContextMenu/Menubar's `contains` calls.
  let rawEl = pToJSVal (_element_raw el)

  postBuild <- getPostBuild
  -- Build the xterm.js terminal, attach it to our div, wire input, then start
  -- streaming the shell's output into it.
  termE <- performEvent $ ffor postBuild $ \_ -> liftJSM $ do
      term <- new (jsg ("Terminal" :: Text)) ()
      -- Register this terminal so the reader thread can route output to it by id
      -- (see window.LeksahTerm in IDE.Web.Main).
      _ <- jsg ("LeksahTerm" :: Text) ^. js2 ("register" :: Text) termId term

      -- Pin an explicit monospace font/size *before* opening: xterm measures
      -- the character cell from the configured font, and without this it
      -- inherits the page's proportional `body` font, making cells wider than
      -- the glyphs (visible gaps between characters).
      opts <- term ^. js ("options" :: Text)
      -- Monospace font/size from the prefs-driven window globals (seeded with a
      -- system default in window.LeksahTerm; see IDE.Web.Main).  The cell-metrics
      -- probe waits for the font to load before measuring, so the row count and
      -- the rendered cell agree (a system font like Monaco loads immediately).
      win <- jsg ("window" :: Text)
      monoFam <- win ^. js ("__leksahMonoFamily" :: Text)
      monoSz  <- win ^. js ("__leksahMonoSize" :: Text)
      _ <- opts ^. jss ("fontFamily" :: Text) monoFam
      _ <- opts ^. jss ("fontSize" :: Text) monoSz
      -- Line/letter spacing tuned to match a native terminal (iTerm2 with Monaco)
      -- rather than xterm's tighter default; the cell-metrics probe applies the
      -- same values so the row/column fit stays correct.
      _ <- opts ^. jss ("lineHeight" :: Text) (1.07 :: Double)
      _ <- opts ^. jss ("letterSpacing" :: Text) (-0.5 :: Double)
      -- The SearchAddon highlights matches via xterm's *proposed* decorations
      -- API, which throws ("allowProposedApi") unless this is enabled.  Must be
      -- set before the find bar drives a search (it is — before loadAddon below).
      _ <- opts ^. jss ("allowProposedApi" :: Text) True

      -- Unicode 11 widths (xterm defaults to Unicode 6, where emoji are
      -- width 1 — tmux and modern apps assume 2, so ✅ etc. misalign).
      uni <- new (jsg ("Unicode11Addon" :: Text) ^. js ("Unicode11Addon" :: Text)) ()
      _ <- term ^. js1 ("loadAddon" :: Text) uni
      unicodeApi <- term ^. js ("unicode" :: Text)
      _ <- unicodeApi ^. jss ("activeVersion" :: Text) ("11" :: Text)

      -- Handle OSC 8 hyperlinks (forwarded by tmux): hover shows the URL.  Clicking
      -- an http(s) link opens it in the browser — snapping the browser over this
      -- terminal's active pane only when Command was held; a file:// link opens in
      -- a CodeMirror editor instead (via the same path as a clicked file token).
      handler <- jsg ("LeksahOscLinks" :: Text) ^. js2 ("makeHandler" :: Text)
          (fun $ \_ _ as -> case as of
              (u:snapV:_) -> do
                  url  <- valToText u
                  snap <- valToBool snapV
                  liftIO $ do
                      _ <- (try (void $ createProcess (proc "open" [T.unpack url]))
                              :: IO (Either SomeException ()))
                      when snap $ activePaneId termId >>= mapM_ requestSnapPane
              _ -> return ())
          (fun $ \_ _ as -> case as of
              (pV:lV:cV:_) -> do
                  path <- valToText pV
                  ln   <- valToNumber lV
                  col  <- valToNumber cV
                  liftIO $ triggerLink (T.unpack path, max 1 (round ln), max 1 (round col))
              _ -> return ())
      _ <- opts ^. jss ("linkHandler" :: Text) handler

      fit  <- new (jsg ("FitAddon" :: Text) ^. js ("FitAddon" :: Text)) ()
      _ <- term ^. js1 ("loadAddon" :: Text) fit
      _ <- term ^. js1 ("open" :: Text) rawEl

      -- GPU renderer: xterm's default DOM renderer rounds the character cell up
      -- to whole CSS pixels, so on HiDPI (retina) displays glyphs don't fill the
      -- cell and look too widely spaced.  The WebGL renderer draws from a texture
      -- atlas with correct device-pixel scaling, fixing the spacing.  It must be
      -- loaded after open() (it needs the terminal's screen element).
      --
      -- Detected at RUNTIME via LeksahTerm.loadWebgl (probe + loadAddon behind
      -- a JS try/catch): when WebGL is unavailable (headless Chrome, GPU-less
      -- environments) the addon's activate() failure would propagate as an
      -- uncatchable RTS crash under the GHC JS backend — so the whole attempt
      -- stays on the JS side and only a boolean comes back.
      webglOk <- valToBool =<< jsg ("LeksahTerm" :: Text) ^. js1 ("loadWebgl" :: Text) term
      unless webglOk . liftIO $
          putStrLn ("terminal " <> T.unpack termId
                    <> ": WebGL unavailable, using the DOM renderer")

      -- xterm's SearchAddon, registered on the terminal element so the find bar
      -- can search this pane (terminals render to a canvas, so no DOM find).
      _ <- jsg ("LeksahCM" :: Text) ^. js2 ("loadTerminalSearch" :: Text) term rawEl

      -- Inline images: SIXEL, iTerm2 OSC 1337 and the kitty graphics protocol.
      -- storageLimit caps the image cache per terminal (MB) — the default 128
      -- is a lot across many panes.
      imgOpts <- obj
      _ <- imgOpts ^. jss ("storageLimit" :: Text) (32 :: Int)
      img <- new (jsg ("ImageAddon" :: Text) ^. js ("ImageAddon" :: Text)) [imgOpts]
      _ <- term ^. js1 ("loadAddon" :: Text) img
      -- OSC 52 writes land on the system clipboard (vim yank, tmux copy-mode
      -- over ssh, …).
      clip <- new (jsg ("ClipboardAddon" :: Text) ^. js ("ClipboardAddon" :: Text)) ()
      _ <- term ^. js1 ("loadAddon" :: Text) clip

      -- Make tokens in the output clickable.  Without a modifier, project-file
      -- paths (validated against the workspace file set kept in JS via
      -- LeksahTermLinks.setProjectFiles) call back with the resolved absolute
      -- path + line/column.  With Ctrl/Cmd held, any identifier is clickable and
      -- calls back with the token + click position for a metadata lookup.
      _ <- jsg ("LeksahTermLinks" :: Text) ^. js4 ("attach" :: Text) term
              (fun $ \_ _ as -> case as of
                  (p:l:c:_) -> do
                      path <- valToText p
                      ln   <- valToNumber l
                      col  <- valToNumber c
                      liftIO $ triggerLink (T.unpack path, round ln :: Int, round col :: Int)
                  _ -> return ())
              (fun $ \_ _ as -> case as of
                  (t:x:y:_) -> do
                      tok <- valToText t
                      cx  <- valToNumber x
                      cy  <- valToNumber y
                      liftIO $ triggerLookup (tok, round cx :: Int, round cy :: Int)
                  _ -> return ())
              -- Hover: (file, hoverLine, hoverCol, requestId) -> LSP tooltip -> JS.
              -- hoverCol < 0 means "column unknown" (file summary only).
              (fun $ \_ _ as -> case as of
                  (fV:lV:cV:rV:_) -> do
                      path <- valToText fV
                      hl   <- valToNumber lV
                      hc   <- valToNumber cV
                      rid  <- valToNumber rV
                      let mline = let n = round hl :: Int in if n > 0 then Just n else Nothing
                          mcol  = let n = round hc :: Int in if n >= 0 then Just n else Nothing
                      liftIO $ LSP.requestTerminalHover (T.unpack path) mline mcol $ \mt ->
                          fireHoverResp (round rid :: Int, mt)
                  _ -> return ())
      _ <- fit ^. js0 ("fit" :: Text)
      -- Re-fit on *any* size change of the terminal element: window resize and
      -- layout changes alike (e.g. showing/hiding the side pane, which resizes
      -- the editor column).  The scroll-based resize detector misses grid track
      -- changes; ResizeObserver catches them.
      ro <- new (jsg ("ResizeObserver" :: Text)) (fun $ \_ _ _ -> do
          _ <- fit ^. js0 ("fit" :: Text)
          c <- valToNumber =<< term ^. js ("cols" :: Text)
          r <- valToNumber =<< term ^. js ("rows" :: Text)
          liftIO $ ignorePtyError (resizePty pty (round c, round r)))
      _ <- ro ^. js1 ("observe" :: Text) rawEl

      -- keystrokes -> shell
      _ <- term ^. js1 ("onData" :: Text) (fun $ \_ _ args -> case args of
              (d:_) -> do
                  s <- valToText d
                  liftIO $ ignorePtyError (writePty pty (encodeUtf8 s))
              _ -> return ())
      -- Intercept the tmux C-b prefix (when the pref is on) — see window.LeksahTmux.
      _ <- jsg ("LeksahTmux" :: Text) ^. js1 ("attach" :: Text) term

      -- title changes -> Terminals list
      _ <- term ^. js1 ("onTitleChange" :: Text) (fun $ \_ _ args -> case args of
              (titleVal:_) -> valToText titleVal >>= liftIO . triggerTitle
              _ -> return ())
      -- bell (Claude Code's needs-input signal) -> leksah attention
      _ <- term ^. js1 ("onBell" :: Text) (fun $ \_ _ _ -> liftIO (triggerBell ()))
      syncPtySize term fit pty
#if defined(ghcjs_HOST_OS)
      -- Static demo content instead of a shell: write the canned dump once.
      -- The page blob is already base64 — exactly what LeksahTerm.write takes —
      -- and the terminal registered with LeksahTerm above, so this synchronous
      -- write inside the build action cannot race the output gate below.
      -- A terminal the visitor just created has no dump (there is no shell to
      -- record): say so, rather than leaving a black rectangle that looks like
      -- a terminal that failed to start.
      b64 <- liftIO $ fromMaybe demoNoticeB64 <$> demoTerminalB64 termId
      void $ jsg ("LeksahTerm" :: Text) ^. js2 ("write" :: Text) termId b64
#else
      -- shell -> screen: blocking reads on their own thread.  Each chunk is
      -- handed to xterm as raw bytes (see the output write below) rather than
      -- decoded here.  We don't coalesce reads: tmux (which backs these
      -- terminals) is a screen-diff multiplexer that already rate-limits its
      -- output, so the PTY delivers a thin, paced stream with little to batch —
      -- measured coalescing through tmux was near-neutral, so it isn't worth the
      -- added end-of-burst latency.
      _ <- liftIO . forkIO $
          let loop = (try (threadWaitReadPty pty >> readPty pty) :: IO (Either SomeException ByteString)) >>= \case
                  Right bs -> triggerOutput bs >> loop
                  -- EOF/read error: the attached tmux client is gone (the session
                  -- ended).  Tell reflex so the tab closes rather than lingering.
                  Left _   -> triggerExited ()
          in loop
#endif
      return (term, fit)

  termFitD <- holdDyn Nothing (Just <$> termE)

  -- Grab keyboard focus when the terminal is created (it's opened visible) and
  -- whenever its tab is later selected.  `delay 0` lets `termFitD` catch the
  -- freshly-created terminal; the focus itself is done inside
  -- `requestAnimationFrame` so it runs after the tab's `visibility` has been
  -- applied/laid out — focusing a still-hidden element is a silent no-op, which
  -- is why selecting an already-open (previously hidden) terminal didn't work.
  focusE <- delay 0 $ leftmost [ () <$ termE, selectedE ]
  performEvent_ $ ffor (attach (current termFitD) focusE) $ \case
      (Just (term, _), ()) -> do
          focusLog $ "[classic " <> T.unpack termId <> "] focusE -> term.focus()"
          liftJSM . void $
            jsg ("window" :: Text) ^. js1 ("requestAnimationFrame" :: Text)
              (fun $ \_ _ _ -> void $ term ^. js0 ("focus" :: Text))
      _ -> return ()

  -- Write each batch of shell output to the terminal.  Gated on the terminal
  -- existing (termFitD becomes Just only once the build action — including the
  -- LeksahTerm.register above — has run), then routed to it by id as raw bytes.
  performEvent_ $ ffor (attach (current termFitD) outputE) $ \case
      (Just _, bs) ->
          liftJSM . void $ jsg ("LeksahTerm" :: Text)
              ^. js2 ("write" :: Text) termId (decodeUtf8 (B64.encode bs))
      _ -> return ()

  -- Keep xterm and the PTY in step with the pane size.
  performEvent_ $ ffor (attach (current termFitD) (() <$ resizeE)) $ \case
      (Just (term, fit), ()) -> liftJSM $ do
          _ <- fit ^. js0 ("fit" :: Text)
          syncPtySize term fit pty
      _ -> return ()

  -- Re-fit when the tab is re-shown: the reveal is a visibility flip the
  -- ResizeObserver above cannot see, so a terminal built hidden re-measures
  -- here (one frame later, once the visibility write has been applied).
  -- Size changes after creation — including the wkwebview title bar's
  -- asynchronous start-up relayout — all reach the observer, so the old
  -- 0.3s post-creation timer is gone.
  refitE <- delay 0 selectedE
  performEvent_ $ ffor (attach (current termFitD) refitE) $ \case
      (Just (term, fit), ()) -> liftJSM $ do
          _ <- fit ^. js0 ("fit" :: Text)
          syncPtySize term fit pty
      _ -> return ()

  -- LSP hover reply -> fill the floating tooltip (this terminal's context).
  performEvent_ $ ffor hoverRespE $ \(rid, mt) -> liftJSM . void $
      jsg ("LeksahTermLinks" :: Text) ^. js2 ("resolveHover" :: Text)
          (rid :: Int) (fromMaybe "" mt)

  -- Navigation from a clicked file path.  The link callbacks report 1-based
  -- line/column (what compilers print); 'Loc' is 0-based, so convert here.
  let fileGotoE = (\(f, l, c) ->
          Loc f (pointRange (Pos (max 0 (l - 1)) (max 0 (c - 1))))) <$> linkE
  -- Navigation from a Ctrl/Cmd-clicked identifier: look it up in the metadata.
  -- No match -> nothing; one match -> jump straight there; several -> pop up a
  -- chooser of module names at the click position and jump to the picked one.
  let optsE = attachWith (\_i (_tok, x, y) -> ([], x, y))
                (current (cUi ctx)) lookupE
      singleGotoE = fmapMaybe (\(opts, _, _) -> case opts of [(_, sp)] -> Just sp; _ -> Nothing) optsE
      multiE      = fmapMaybe (\(opts, x, y) -> if length opts > 1 then Just (x, y, opts) else Nothing) optsE
  rec chooserD <- holdDyn Nothing $ leftmost [ Just <$> multiE, Nothing <$ chosenE ]
      chosenE <- switchHold never =<< dyn (ffor chooserD $ \case
        Nothing           -> return never
        Just (x, y, opts) ->
          elAttr "div" ("class" =: "context-menu"
              <> "style" =: T.pack ("position:fixed;left:" <> show x <> "px;top:" <> show y <> "px")) $
            menu [ constDyn (lbl, sp) | (lbl, sp) <- opts ])
  let gotoE = TerminalGoto <$> leftmost [ fileGotoE, singleGotoE, chosenE ]
  return $ leftmost [ TerminalTitle <$> titleE, gotoE, TerminalBell <$ bellE
                    , TerminalExited <$ exitedE ]
  where
    -- Match the PTY's window size to xterm's current cols/rows so full-screen
    -- programs (vim, htop, …) lay out correctly.
    syncPtySize term _fit pty = do
        cols <- valToNumber =<< term ^. js ("cols" :: Text)
        rows <- valToNumber =<< term ^. js ("rows" :: Text)
        liftIO $ ignorePtyError (resizePty pty (round cols, round rows))

#if defined(ghcjs_HOST_OS)
-- | What a terminal the visitor creates in the browser demo shows instead of a
-- shell prompt: there is no operating system under this page, so the honest
-- thing is to say so and point at the terminals that DO have content (the
-- recorded ones from 'IDE.Web.DemoTerminals').
--
-- CRLF, not LF: xterm is a real terminal emulator, so a bare LF moves down
-- without returning to column 0 and the text staircases (the same fix
-- @gen-demo-terminals.hs@ applies to the recorded dumps).  Lines are separate
-- literals because CPP eats Haskell string gaps.
demoNoticeB64 :: Text
demoNoticeB64 = decodeUtf8 (B64.encode (encodeUtf8 notice))
  where
    notice = T.intercalate "\r\n"
      [ ""
      , "  \ESC[1;33m\9888  This is just a demo \8212 there is no shell here.\ESC[0m"
      , ""
      , "  leksah itself is running in your browser, compiled to JavaScript by"
      , "  GHC's JS backend.  What is missing is everything underneath it: no"
      , "  processes, no tmux, no operating system \8212 so a new terminal has"
      , "  nothing to talk to."
      , ""
      , "  The \ESC[36mclaude\ESC[0m and \ESC[36mbuild\ESC[0m tabs are \ESC[1mrecordings\ESC[0m of real terminals,"
      , "  captured from a real session, so you can see how they look and hover"
      , "  the code in them."
      , ""
      , "  \ESC[32mRun leksah on your own machine to get terminals that run things.\ESC[0m"
      , ""
      ]
#endif

-- | Run a PTY write/resize, swallowing errors.  Once a terminal's shell exits
-- (e.g. the user typed @exit@) its tmux session/window can be gone and the PTY
-- dead, so a write/resize raises @fdWriteBuf: Input/output error@.  That's just
-- one dead terminal — it must never take down the whole UI (which it did: the
-- exception was uncaught and locked up leksah).
ignorePtyError :: IO () -> IO ()
ignorePtyError act = act `catch` \(_ :: SomeException) -> return ()

-- | Create a fresh leksah tmux session (detached) named @name@, applying the
-- leksah tmux config, and return its stable tmux session id (e.g. "$3") plus
-- its initial window's id (@\@N@ — what the caller's leksah window's pane
-- references).  'Nothing' if tmux is absent or the command fails.
createTerminalSession :: Text -> IO (Maybe (Text, Text))
#if defined(ghcjs_HOST_OS)
-- Browser demo: no tmux to create anything in, but the session must still come
-- into being — the caller mints its leksah window (and so its tab) from these
-- ids, and returning 'Nothing' is why "+" used to do nothing at all.  The tab
-- then shows the demo notice, having no recorded output.
createTerminalSession name = Just <$> demoCreateSession name
#else
createTerminalSession name = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            shell <- getLoginShell
            conf <- writeTmuxConf shell
            (_rc, out, _) <- readProcessWithExitCode tmux
                [ "-L", tmuxSocket, "-f", conf, "new-session", "-d"
                , "-s", T.unpack name, "-P", "-F", "#{session_id}\t#{window_id}" ] ""
            return $ listToMaybe
                [ (sid, wid) | l <- T.lines (T.pack out)
                , (sid : wid : _) <- [T.splitOn "\t" (T.strip l)]
                , not (T.null sid), not (T.null wid) ]
#endif

-- | Open a file in the external editor: run @argv@ (e.g. @["vim","+12","/f.hs"]@)
-- as a new *window* in the shared @leksah-editor@ tmux session — created on the
-- first open, reused after — so every externally-opened file is a window-tab
-- (named @winName@, the file's basename) under one Terminals-tree node.  Returns
-- the session id the editor pane lives in (the terminal tab is keyed by it).
-- When the last window is @:q@'d the session ends; the next open recreates it.
--
-- One editor per file: the pane is tagged @\<file\>#edit@ (a *pane* option, so
-- the tag follows the pane through the Terminals-tree drag-and-drop), and a
-- reopen selects + returns the existing pane — wherever it lives now — instead
-- of launching a second editor.  (Reopening at a different line just activates
-- it; the @+line@ only applies to a fresh launch.)
openFileInEditor :: FilePath -> String -> [String] -> IO (Maybe Text)
openFileInEditor file winName argv = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            shell <- getLoginShell
            conf  <- writeTmuxConf shell
            let base = ["-L", tmuxSocket, "-f", conf]
                run as = readProcessWithExitCode tmux (base ++ as) ""
                key = T.pack file <> "#edit"
            findRunPane key >>= \case
              Just (sid, wid, pid) -> do
                _ <- run ["select-window", "-t", T.unpack wid]
                _ <- run ["select-pane", "-t", T.unpack pid]
                return (Just sid)
              Nothing -> do
                -- Add a window to leksah-editor if it exists, else create the session.
                (hasRc, _, _) <- run ["has-session", "-t", "=leksah-editor"]
                let mk = if hasRc == ExitSuccess
                           then ["new-window", "-t", "=leksah-editor"]
                           else ["new-session", "-d", "-s", "leksah-editor"]
                (_rc, out, _) <- run
                    (mk ++ ["-n", winName, "-P", "-F", "#{session_id}\t#{pane_id}"] ++ argv)
                case T.splitOn "\t" (T.strip (T.pack out)) of
                  (sid : pid : _) | not (T.null pid) -> do
                    _ <- run ["set-option", "-p", "-t", T.unpack pid, "@leksah_run", T.unpack key]
                    return (Just sid)
                  _ -> return Nothing

-- | Ensure a *shell* pane tagged @key@ exists in the shared @leksah-editor@
-- session with @preTyped@ sitting UNRUN at its prompt (@send-keys -l@, no
-- Enter) — the backing pane for a file open in leksah's own editor (or a git
-- log view): someone attached to the tmux session from a plain terminal sees,
-- per file, e.g. @vi +12 '/path/file.hs'@ ready to go and can press Enter to
-- open it, while leksah shows its own editor.  Returns
-- @(session id, window id, pane id)@.
--
-- Unlike 'openFileInEditor' the dedup path does NOT select the found
-- window\/pane: this runs in the background on every file open, and yanking
-- the session's current window would fight an external attacher's navigation.
-- ('ensureShellPane' — the hidden backing-twin creator — lived here until
-- the native split layouts replaced the ⌘D pane overlays; see
-- 'cleanupStaleTwinPanes' for the one-time removal of twins an older build
-- left behind.)

-- | The editor command to pre-type in a backing pane: the external-editor
-- preference when set, else @$EDITOR@, else @vi@.
resolveEditorCmd :: Text -> IO Text
resolveEditorCmd prefCmd
  | not (T.null (T.strip prefCmd)) = return (T.strip prefCmd)
  | otherwise = maybe "vi" T.pack . mfilter (not . null) <$> lookupEnv "EDITOR"

-- | Single-quote @t@ for a shell command line (the pre-typed text sits at a
-- login-shell prompt), with the usual @'\\''@ escaping for embedded quotes.
shellQuoteArg :: Text -> Text
shellQuoteArg t = "'" <> T.replace "'" "'\\''" t <> "'"

-- | Kill the pane tagged @key@ — but only if nothing is running in it (its
-- current command is still the login shell), so an editor the user actually
-- opened (Enter on the pre-typed command), or anything else they started
-- there, is left alone.  Used when the leksah tab that created the backing
-- pane is closed by the user.
-- | One-time cleanup for the native-split-layouts migration: an OLDER build
-- created hidden backing-twin panes (run keys with the @#edit@ / @#gitlog#@ /
-- @shortcuts#view@ shapes — 'IDE.Web.ReplTmux.isBackingRunKey'); kill the
-- IDLE ones — still at the login shell with their command pre-typed, unrun.
-- A twin whose editor the user actually launched is a real pane now and is
-- left alone (it shows up as an ordinary window).  An emptied leksah-editor
-- session dies with its last window.  Delete this (and 'isBackingRunKey') a
-- release after the migration has had its chance to run everywhere.
cleanupStaleTwinPanes :: IO ()
cleanupStaleTwinPanes = (`catch` \(_ :: SomeException) -> return ()) $ do
    panes <- liveRunPanes
    mapM_ killRunPaneIfIdle [ key | (key, _, _, _) <- panes, isBackingRunKey key ]

killRunPaneIfIdle :: Text -> IO ()
killRunPaneIfIdle key = (`catch` \(_ :: SomeException) -> return ()) $
    findRunPane key >>= \case
      Nothing -> return ()
      Just (_sid, _wid, pid) -> findExecutable "tmux" >>= \case
        Nothing -> return ()
        Just tmux -> do
            (_, out, _) <- readProcessWithExitCode tmux
                [ "-L", tmuxSocket, "display-message", "-p", "-t", T.unpack pid
                , "#{pane_current_command}" ] ""
            shell <- getLoginShell
            let cmd = T.strip (T.pack out)
                idle = cmd `elem` T.pack (takeFileName shell)
                              : ["sh", "bash", "zsh", "fish", "ksh", "dash"]
            when idle . void $ readProcessWithExitCode tmux
                ["-L", tmuxSocket, "kill-pane", "-t", T.unpack pid] ""

-- | Write (idempotently) a tiny helper that posts a macOS Notification Center
-- notification for a tmux bell alert, and return its path.  Driven by the
-- @alert-bell@ hook (set in 'terminalWidget'): the hook passes the belling
-- window's session id + window index; the script looks their names up and shows
-- the notification via @osascript@ — which works from any process (leksah runs
-- as a bare binary, not a .app bundle, so the native UNUserNotification API
-- isn't available).  Bell is Claude Code's "needs input / done" signal, so this
-- tells you a teammate wants you without watching the window.
-- | The notify script lives at a *stable* path under @~/.leksah@ — not in
-- @$TMPDIR@, which under nix-shell is a per-invocation dir that gets cleaned up
-- (leaving the @alert-bell@ hook pointing at a vanished file, and littering
-- @/tmp@ with one script per launch).
notifyScriptPath :: IO FilePath
notifyScriptPath = (</> ".leksah" </> "leksah-notify.sh") <$> getHomeDirectory

writeNotifyScript :: IO FilePath
writeNotifyScript = do
    path <- notifyScriptPath
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path $ unlines
        [ "#!/bin/sh"
        , "# $1 = tmux session id (e.g. $3), $2 = window index (optional — empty"
        , "# means the session's current window).  Written by leksah."
        , "target=\"$1\"; [ -n \"$2\" ] && target=\"$1:$2\""
        , "label=$(tmux -L " <> tmuxSocket <> " display-message -p -t \"$target\" '#{session_name}: #{window_name}' 2>/dev/null)"
        , "[ -z \"$label\" ] && label=\"$target\""
        -- Pass the label as an argv item (not string-interpolated) so names with
        -- quotes can't break the AppleScript.
        , "osascript - \"$label\" >/dev/null 2>&1 <<'OSA' || true"
        , "on run argv"
        , "  display notification (item 1 of argv) with title \"leksah terminal\""
        , "end run"
        , "OSA"
        ]
    return path

-- | Post the bell notification for session @sid@'s current window from leksah
-- (via the same script the tmux hook uses).  Needed because tmux's alert-bell
-- hook does NOT fire for a bell in the window a client is viewing — which is
-- every open terminal's current window — so leksah catches those bells itself
-- (xterm @onBell@) and calls this.
notifyTerminalBell :: Text -> IO ()
notifyTerminalBell sid = (`catch` \(_ :: SomeException) -> return ()) $ do
    path <- notifyScriptPath
    exists <- doesFileExist path
    ensured <- if exists then return path else writeNotifyScript
    void $ readProcessWithExitCode "sh" [ensured, T.unpack sid, ""] ""


-- | All currently-live tmux sessions on leksah's socket as @(session id, session
-- name)@ pairs — every session, not just leksah's own, so the Terminals list can
-- show them all.  Empty if tmux is absent or no server is running.
listTerminalSessions :: IO [(Text, Text)]
#if defined(ghcjs_HOST_OS)
-- Browser demo: the canned sessions from the page stand in for tmux.
listTerminalSessions = demoTerminals
#else
listTerminalSessions = (`catch` \(_ :: SomeException) -> return []) $
    findExecutable "tmux" >>= \case
        Nothing -> return []
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-sessions", "-F", "#{session_id}\t#{session_name}"] ""
            return [ (sid, T.intercalate "\t" rest)
                   | line <- lines out
                   , (sid:rest) <- [T.splitOn "\t" (T.pack line)]
                   , not (T.null sid) ]
#endif

-- | Kill the tmux session with id @n@ (so it no longer persists).
killTerminalSession :: Text -> IO ()
killTerminalSession n = (`catch` \(_ :: SomeException) -> return ()) $ do
    unregisterTerminalPty n
    findExecutable "tmux" >>= \case
        Nothing -> return ()
        Just tmux -> void $ readProcessWithExitCode tmux
            ["-L", tmuxSocket, "kill-session", "-t", T.unpack n] ""

-- | A tmux pane within a window: its index, a display label (its index and the
-- command running in it), and whether it is the window's active pane.
data TmuxPane = TmuxPane
  { tpIndex  :: Int
  , tpId     :: Text   -- ^ tmux @#{pane_id}@ (e.g. @%5@) — the id the CC widget
                       --   keys its xterms/highlights by; lets a flip target be
                       --   located in the editor-area DOM (@.terminal-cc-pane@).
  , tpLabel  :: Text
  , tpPid    :: Int    -- ^ tmux @#{pane_pid}@ — the pid of the pane's own
                       --   process (its shell).  Used to identify the exact
                       --   Claude Code session running in a pane; @0@ where the
                       --   pid isn't known (the browser demo).
  , tpActive :: Bool
  , tpRunKey :: Text   -- ^ this pane's own @\@leksah_run@ tag (@""@ = none).
                       --   Per-PANE (not window) so a hidden editor/git-log
                       --   backing twin can be told apart from a user's own
                       --   pane sharing the same window — see the backing-twin
                       --   filter in 'IDE.Web.Widget.TerminalCC'.
  , tpClaudeTitle :: Maybe Text
                     -- ^ for a Claude Code pane, the title of the session running
                     --   in it — its @/rename@ name, else the transcript's first
                     --   prompt ('Nothing' for any other pane, and for a session
                     --   with neither).  PER PANE, so a window holding two Claude
                     --   panes, or one beside a shell, can label itself by
                     --   whichever pane is active (see 'enrichClaudeTitles').
  , tpClaudeStatus :: Maybe Text
                     -- ^ the semantic state of the Claude session running in this
                     --   pane (@busy@ / @shell@ / @waiting@ / @idle@ — see
                     --   'ClaudeLive'), resolved by pid like 'tpClaudeTitle'.
                     --   @waiting@ means blocked on an approval prompt — surfaced
                     --   as a persistent needs-input alert and preferred by the
                     --   jump-to-alert command.
  } deriving (Eq, Show)

-- | A tmux window within a session: its index, a display label (its index and
-- name), whether it is the session's active window, its tmux alert flags (bell /
-- activity / silence — surfaced in the Terminals tree so you can see which
-- teammate rang the bell, is producing output, or has gone quiet), and its panes.
data TmuxWindow = TmuxWindow
  { twIndex    :: Int
  , twId       :: Text        -- ^ tmux @#{window_id}@ (e.g. @\@7@) — stable for
                              --   the tmux server's lifetime, unlike the index,
                              --   so it's what leksah windows' tmux panes
                              --   ('PaneTmux') reference
  , twLabel    :: Text
  , twActive   :: Bool
  , twBell     :: Bool
  , twActivity :: Bool
  , twSilence  :: Bool
  , twRunKey   :: Text        -- ^ the @\@leksah_run@ marker (@""@ = none), e.g.
                              --   @\<dir\>#claude@ for a Claude Code window
  , twPanes    :: [TmuxPane]
  } deriving (Eq, Show)

-- | The tmux window/pane hierarchy of every live session, keyed by tmux session
-- id (e.g. "$3") and carrying that session's current name — the whole Terminals
-- tree in one @list-panes -a@ call.  Keying by the stable id (not the name) means
-- a rename just changes the carried name on the next poll, not the key.  Includes
-- all sessions, not only leksah's own.  Empty if tmux is absent / no server.
listTerminalTree :: IO (Map Text (Text, [TmuxWindow]))
#if defined(ghcjs_HOST_OS)
-- Browser demo: one window with one pane per session, so the Terminals pane and
-- the wide0 tab labels populate.  Select/kill actions fall into the
-- catch-everything tmux helpers, which no-op in the browser.  Both the canned
-- sessions and the ones the visitor created are listed — the latter must appear
-- here or the reconcile would prune the leksah window their tab lives in.
listTerminalTree = do
    ts      <- demoTerminals
    created <- demoCreatedSessions
    return $ M.fromListWith (\_ old -> old)
        [ (sid, (name, [ TmuxWindow 0 ("@" <> sid) name True False False False ""
                             [ TmuxPane 0 ("%" <> sid) name 0 True "" Nothing Nothing ] ]))
        | (sid, name) <- ts ++ created ]
#else
listTerminalTree = (`catch` \(_ :: SomeException) -> return M.empty) $
    findExecutable "tmux" >>= \case
        Nothing -> return M.empty
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-panes", "-a", "-F", paneTreeFormat] ""
            enrichClaudeTitles (parsePaneTree out)

-- | Fill in 'tpClaudeTitle' for every Claude Code pane in the tree: the title of
-- the session running in it — the name @/rename@ gave it, else the transcript's
-- first prompt — so a tab or flipper entry can show what the conversation is
-- about instead of the bare window name "claude".  Other panes are left
-- untouched, and with no Claude pane at all this costs nothing.
--
-- Resolution is PER PANE and by pid ('claudeLiveOwners'), which is exact even
-- with two sessions open in one directory (or in one window).  Fallbacks, in
-- order, for a pane whose process we can't see (its claude exited, @ps@
-- unavailable, a remote tmux with no pane pid): the session id in the pane's own
-- run key, then the directory's most recently used transcript.
--
-- Two IO reads for the whole tree (the live sessions, and one transcript scan per
-- distinct directory); the rest is pure.
enrichClaudeTitles
  :: Map Text (Text, [TmuxWindow]) -> IO (Map Text (Text, [TmuxWindow]))
enrichClaudeTitles tree
  | null claudeDirs = return tree
  | otherwise = do
      owners <- claudeLiveOwners
      byDir  <- M.fromList <$> mapM (\d -> (,) d <$> claudeSessionsFor d) claudeDirs
      return $ M.map (\(nm, ws) ->
          (nm, [ w { twPanes = map (fillPane owners byDir) (twPanes w) } | w <- ws ])) tree
  where
    claudeDirs = nub (mapMaybe claudePaneDir
                       [ p | (_, (_, ws)) <- M.toList tree, w <- ws, p <- twPanes w ])
    fillPane owners byDir p = case claudePaneDir p of
      Nothing  -> p
      Just dir ->
        let ss    = M.findWithDefault [] dir byDir
            title = case M.lookup (tpPid p) owners of
              -- Its own session: prefer the transcript row (same name source,
              -- plus the first-prompt fallback), else the bare live name.
              Just l  -> case find ((== clSession l) . csId) ss of
                           Just s  -> Just (csTitle s)
                           Nothing -> clName l
              Nothing -> case claudePaneSession p of
                           Just sid -> csTitle <$> find ((== sid) . csId) ss
                           Nothing  -> csTitle <$> listToMaybe ss
        in p { tpClaudeTitle = case title of
                 -- An unnamed session with no user prompt yet has no title.
                 Just t | t /= "(untitled session)" -> Just t
                 _                                   -> Nothing
             , tpClaudeStatus = clStatus =<< M.lookup (tpPid p) owners }
#endif

-- | Is this pane running a Claude Code session — i.e. does its @\@leksah_run@
-- marker have the Claude shape ('claudePaneDir')?  True even before a title is
-- known, so the tab row can show the robot icon straight away.
isClaudePane :: TmuxPane -> Bool
isClaudePane = isJust . claudePaneDir

-- | The working directory of a Claude Code pane from its @\@leksah_run@ marker
-- (@\<dir\>#claude@, @…#claude#\<id\>@, @…#claude#ask#\<file\>@, …), or 'Nothing'
-- when the pane isn't a Claude one.
claudePaneDir :: TmuxPane -> Maybe FilePath
claudePaneDir p = case T.breakOn "#claude" (tpRunKey p) of
  (d, rest)
    | not (T.null d)
    , rest == "#claude" || "#claude#" `T.isPrefixOf` rest -> Just (T.unpack d)
  _ -> Nothing

-- | The session id a Claude pane's run key names, for the plain resume shape
-- @\<dir\>#claude#\<id\>@ only.  A @#fork#@ key names the session forked FROM (the
-- new one has a fresh id), and @#ask#@ carries a filename — neither identifies
-- the pane's own session, so both give 'Nothing'.
claudePaneSession :: TmuxPane -> Maybe Text
claudePaneSession p = do
  sid <- T.stripPrefix "#claude#" (snd (T.breakOn "#claude#" (tpRunKey p)))
  if T.null sid || "fork#" `T.isPrefixOf` sid || "ask#" `T.isPrefixOf` sid
    then Nothing else Just sid

-- | Tab-separated so names / commands / titles (which won't contain tabs) stay
-- intact: session id/name, window index/name/active, pane
-- index/active/id/command/title.  pane_title is the per-pane title (what ⌃B w
-- shows) — used as the pane's display name so panes don't all share the
-- terminal's (active-pane) OSC title; command is the fallback when it's empty.
-- pane_title comes LAST because it's the one field that could itself contain a
-- tab (the parser re-joins any trailing fields into it).
paneTreeFormat :: String
paneTreeFormat = intercalate "\t"
    [ "#{session_id}", "#{session_name}", "#{window_index}", "#{window_id}"
    , "#{window_name}"
    , "#{window_active}", "#{window_bell_flag}", "#{window_activity_flag}"
    , "#{window_silence_flag}", "#{@leksah_run}", "#{pane_index}", "#{pane_active}"
    , "#{pane_id}", "#{pane_pid}", "#{pane_current_command}", "#{pane_title}" ]

-- | Run tmux on a remote host over ssh (no PTY, BatchMode — key auth only).
-- 'Nothing' when ssh or the remote tmux fails (host down, no server, …).
--
-- Each argument is single-quoted FOR THE REMOTE SHELL: ssh joins the remote
-- command's words with spaces and hands them to the login shell, so an
-- unquoted tmux format like @#{session_id}@ starts a shell COMMENT at the
-- @#@ — tmux saw @-F@ with no value, failed, and the whole host showed as
-- \"(unreachable)\" even though ssh was fine.
sshTmux :: Text -> [String] -> IO (Maybe String)
sshTmux host args = (`catch` \(_ :: SomeException) -> return Nothing) $ do
    (c, out, _) <- readProcessWithExitCode "ssh"
        ([ "-o", "BatchMode=yes", "-o", "ConnectTimeout=5", T.unpack host
         , unwords ("tmux" : map shellQuote args) ]) ""
    return $ if c == ExitSuccess then Just out else Nothing
  where
    shellQuote s = "'" <> concatMap esc s <> "'"
    esc '\'' = "'\\''"
    esc c    = [c]

-- | The session/window/pane tree of a remote host's (default-socket) tmux —
-- same shape as 'listTerminalTree'.  'Nothing' = unreachable / no server.
listRemoteTerminalTree :: Text -> IO (Maybe (Map Text (Text, [TmuxWindow])))
listRemoteTerminalTree host =
    fmap parsePaneTree <$> sshTmux host ["list-panes", "-a", "-F", paneTreeFormat]

-- | The @(host, target)@ behind an @ssh://host[#target]@ terminal tab key
-- ('Nothing' for local keys).  An empty target means the default session
-- name @leksah@ (what a plain @cc-connect HOST@ attaches).
remoteTabHostTarget :: Text -> Maybe (Text, Text)
remoteTabHostTarget n = do
    rest <- T.stripPrefix "ssh://" n
    let (host, hash) = T.breakOn "#" rest
        target0 = T.drop 1 hash
    return (host, if T.null target0 then "leksah" else target0)

-- | The display label and windows of the remote session behind an
-- @ssh://host#target@ tab key — the remote analogue of one
-- 'listTerminalTree' entry, so remote windows/panes can join the flipper.
remoteTabTree :: Text -> IO (Maybe (Text, [TmuxWindow]))
remoteTabTree n = case remoteTabHostTarget n of
    Nothing -> return Nothing
    Just (host, target) -> do
        mb <- listRemoteTerminalTree host
        return $ do
            tree <- mb
            (_, (nm, ws)) <- find (\(sid, (nm', _)) -> nm' == target || sid == target)
                                  (M.toList tree)
            return (host <> " · " <> nm, ws)

-- | Create a detached session on a remote host's tmux; returns its session id.
createRemoteSession :: Text -> IO (Maybe Text)
createRemoteSession host =
    (>>= (listToMaybe . filter (not . T.null) . map T.strip . T.lines . T.pack))
        <$> sshTmux host ["new-session", "-d", "-P", "-F", "#{session_id}"]

selectRemoteTmuxWindow :: Text -> Text -> Int -> IO ()
selectRemoteTmuxWindow host s w =
    void $ sshTmux host ["select-window", "-t", T.unpack s <> ":" <> show w]

selectRemoteTmuxPane :: Text -> Text -> Int -> Int -> IO ()
selectRemoteTmuxPane host s w p = do
    selectRemoteTmuxWindow host s w
    void $ sshTmux host ["select-pane", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- Remote analogues of the local window/pane management commands
-- ('killTmuxWindow' … 'renameTmuxWindow'), run over ssh on @host@'s
-- default-socket tmux (see 'sshTmux').  Same @session:window.pane@ targeting;
-- the session id may be a tmux id like @$0@, which 'sshTmux' single-quotes so
-- the remote login shell can't expand it (the same hazard fixed for attach).

-- | Kill remote session @s@ (so it no longer persists on @host@).
killRemoteTmuxSession :: Text -> Text -> IO ()
killRemoteTmuxSession host s =
    void $ sshTmux host ["kill-session", "-t", T.unpack s]

-- | Kill window @w@ of remote session @s@ (tmux closes the session if last).
killRemoteTmuxWindow :: Text -> Text -> Int -> IO ()
killRemoteTmuxWindow host s w =
    void $ sshTmux host ["kill-window", "-t", T.unpack s <> ":" <> show w]

-- | Kill pane @p@ of window @w@ in remote session @s@ (closes the window if last).
killRemoteTmuxPane :: Text -> Text -> Int -> Int -> IO ()
killRemoteTmuxPane host s w p =
    void $ sshTmux host ["kill-pane", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Create a new window in remote session @s@ (becomes its current window).
newRemoteTmuxWindow :: Text -> Text -> IO ()
newRemoteTmuxWindow host s = void $ sshTmux host ["new-window", "-t", T.unpack s]

-- | Toggle zoom for pane @p@ of window @w@ in remote session @s@.
zoomRemoteTmuxPane :: Text -> Text -> Int -> Int -> IO ()
zoomRemoteTmuxPane host s w p =
    void $ sshTmux host ["resize-pane", "-Z", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Break pane @p@ of window @w@ in remote session @s@ out into its own window.
breakRemoteTmuxPane :: Text -> Text -> Int -> Int -> IO ()
breakRemoteTmuxPane host s w p =
    void $ sshTmux host ["break-pane", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Move pane @srcPaneId@ (a tmux pane id like @%5@) into window @dstW@ of
-- remote session @dstS@ on @host@, splitting the target window.  Pane ids are
-- stable across the renumbering a move triggers, so this is the source target
-- used by the Terminals-tree drag-and-drop (see 'moveTmuxPane').
moveRemoteTmuxPane :: Text -> Text -> Text -> Int -> IO ()
moveRemoteTmuxPane host srcPaneId dstS dstW =
    void $ sshTmux host ["move-pane", "-s", T.unpack srcPaneId,
                         "-t", T.unpack dstS <> ":" <> show dstW]

-- | Rename remote session @s@ to @name@.
renameRemoteTmuxSession :: Text -> Text -> Text -> IO ()
renameRemoteTmuxSession host s name =
    void $ sshTmux host ["rename-session", "-t", T.unpack s, T.unpack name]

-- | Rename window @w@ of remote session @s@ to @name@.
renameRemoteTmuxWindow :: Text -> Text -> Int -> Text -> IO ()
renameRemoteTmuxWindow host s w name =
    void $ sshTmux host ["rename-window", "-t", T.unpack s <> ":" <> show w, T.unpack name]

-- | Detach tmux control-mode clients left over from previous leksah runs.
-- leksah's own @tmux -C@ child processes don't die when leksah exits — they
-- wedge writing to the closed pipe — and tmux keeps counting them as attached
-- clients; the never-resized ones clamp their sessions' windows to 80x24,
-- wrecking rendering for the live clients.  Run at startup, before any
-- terminal widget attaches (leksah has no control clients of its own yet, so
-- everything control-mode on the socket is stale).
reapControlClients :: IO ()
reapControlClients = (`catch` \(_ :: SomeException) -> return ()) $
    findExecutable "tmux" >>= \case
        Nothing -> return ()
        Just tmux -> do
            (_, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-clients", "-F"
                , "#{client_name}\t#{client_pid}\t#{client_control_mode}"] ""
            forM_ (lines out) $ \l -> case splitOn '\t' l of
                [name, pid, "1"] -> do
                    -- KILL the client process, don't just detach: a wedged
                    -- client's pipe is full, so tmux can't flush its buffer
                    -- and a plain detach-client silently never completes —
                    -- the zombie stays "attached", and once IT falls behind,
                    -- tmux stops reading the pane's pty, periodically
                    -- freezing the program inside (seen as the whole TUI
                    -- pausing every few seconds while it streams).
#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
                    forM_ (readMaybe pid :: Maybe Int) $ \p ->
                        signalProcess sigKILL (fromIntegral p)
                            `catch` \(_ :: SomeException) -> return ()
#endif
                    void $ readProcessWithExitCode tmux
                        ["-L", tmuxSocket, "detach-client", "-t", name] ""
                _ -> return ()
  where
    splitOn c s = case break (== c) s of
        (a, _ : rest) -> a : splitOn c rest
        (a, [])       -> [a]

-- | Parse the @list-panes -a@ output into the per-session (id -> (name, windows))
-- tree, grouping by session then window (ascending by index within each level).
parsePaneTree :: String -> Map Text (Text, [TmuxWindow])
parsePaneTree out = M.map toSession grouped
  where
    rows =
      [ (sid, sname, wi, wid, wn, wa == "1", wb == "1", wac == "1", ws == "1", runkey, pidx, pa == "1", pid, ppid, paneName)
      | line <- lines out
      , (sid:sname:wiT:wid:wn:wa:wb:wac:ws:runkey:piT:pa:pid:ppidT:cmd:rest) <- [T.splitOn "\t" (T.pack line)]
      , not (T.null sid)
      -- The control-mode monitor's hidden session is not a real terminal.
      , sname /= monitorSessionName
      , Just wi   <- [readMaybe (T.unpack wiT)]
      , Just pidx <- [readMaybe (T.unpack piT)]
      , let title    = T.intercalate "\t" rest
            paneName = if T.null title then cmd else title
            -- An old remote tmux with no #{pane_pid} just yields 0 (unknown).
            ppid     = maybe 0 id (readMaybe (T.unpack ppidT)) ]
    -- session id -> (name, window index -> (window id, name, active, bell, activity, silence, @leksah_run, pane idx -> (paneName, active, pane id, pane pid, @leksah_run)))
    grouped :: Map Text (Text, Map Int (Text, Text, Bool, Bool, Bool, Bool, Text, Map Int (Text, Bool, Text, Int, Text)))
    grouped = M.fromListWith mergeSess
      [ (sid, (sname, M.singleton wi (wid, wn, wa, wb, wac, ws, runkey, M.singleton pidx (paneName, pa, pid, ppid, runkey))))
      | (sid, sname, wi, wid, wn, wa, wb, wac, ws, runkey, pidx, pa, pid, ppid, paneName) <- rows ]
    mergeSess (sname, w1) (_, w2) = (sname, M.unionWith mergeWin w1 w2)
    -- The run key is a *pane* option (rows differ within a window — e.g. a
    -- claude pane dragged into a window of shell panes), so a window's
    -- 'twRunKey' is any non-empty pane key; legacy window-tagged windows give
    -- every row the same inherited key, so this reduces to the old behaviour.
    mergeWin (wid, wn, wa, wb, wac, ws, rk1, ps1) (_, _, _, _, _, _, rk2, ps2) =
      (wid, wn, wa, wb, wac, ws, if T.null rk1 then rk2 else rk1, ps1 <> ps2)
    toSession (sname, wm) =
      ( sname
      , [ TmuxWindow wi wid (T.pack (show wi) <> ": " <> wn) wa wb wac ws rk
            [ TmuxPane pidx pid (T.pack (show pidx) <> ": " <> paneName) ppid pa prk Nothing Nothing
            | (pidx, (paneName, pa, pid, ppid, prk)) <- M.toAscList ps ]
        | (wi, (wid, wn, wa, wb, wac, ws, rk, ps)) <- M.toAscList wm ] )

-- | Make window @w@ of session @s@ (a tmux session id) the current window.
selectTmuxWindow :: Text -> Int -> IO ()
selectTmuxWindow s w =
    tmuxCmd ["select-window", "-t", T.unpack s <> ":" <> show w]

-- | Make pane @p@ of window @w@ in session @s@ the active pane.  Also switch the
-- session to that window first: @select-pane@ only moves focus within a window,
-- so without this a pane in a non-current window wouldn't actually be shown.
selectTmuxPane :: Text -> Int -> Int -> IO ()
selectTmuxPane s w p = do
    selectTmuxWindow s w
    tmuxCmd ["select-pane", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Kill window @w@ of session @s@ (tmux closes the session if it was its last
-- window).
killTmuxWindow :: Text -> Int -> IO ()
killTmuxWindow s w =
    tmuxCmd ["kill-window", "-t", T.unpack s <> ":" <> show w]

-- | Kill pane @p@ of window @w@ in session @s@ (tmux closes the window if it was
-- its last pane).
killTmuxPane :: Text -> Int -> Int -> IO ()
killTmuxPane s w p =
    tmuxCmd ["kill-pane", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Create a new window in session @s@ (becomes that session's current window).
newTmuxWindow :: Text -> IO ()
newTmuxWindow s = tmuxCmd ["new-window", "-t", T.unpack s]

-- | Toggle zoom (fullscreen-within-its-window) for pane @p@ of window @w@.
zoomTmuxPane :: Text -> Int -> Int -> IO ()
zoomTmuxPane s w p =
    tmuxCmd ["resize-pane", "-Z", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Break pane @p@ of window @w@ out into its own new window (so it gets its own
-- activity/bell tracking and more room).
breakTmuxPane :: Text -> Int -> Int -> IO ()
breakTmuxPane s w p =
    tmuxCmd ["break-pane", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Kill the pane with tmux id @pid@ (e.g. @%5@) directly — targeting by pane
-- id (not session:window.index) is unambiguous even mid-relayout.  tmux closes
-- the window/session if it was the last pane.  Used by the ⌘W close menu's
-- "Kill Pane".
killTmuxPaneId :: Text -> IO ()
#if defined(ghcjs_HOST_OS)
-- Browser demo: forget the visitor-created session instead (its pane id is
-- derived from the session id), so ⌘W ▸ Kill actually closes the tab rather
-- than the next tree poll putting it straight back.  Canned sessions ignore it.
killTmuxPaneId = demoKillSession
#else
killTmuxPaneId pid = tmuxCmd ["kill-pane", "-t", T.unpack pid]
#endif

-- | Break pane @pid@ (e.g. @%5@) out into its own new window but do NOT switch
-- to it (@-d@), so the pane leaves the current tiling yet stays alive.  Returns
-- the NEW window's index (so the ⌘W close menu's "Move Pane to Hidden Window"
-- can add it to 'hiddenWindows'); 'Nothing' on any error / no tmux.
breakTmuxPaneId :: Text -> IO (Maybe Int)
breakTmuxPaneId pid = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing   -> return Nothing
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "break-pane", "-d", "-P", "-F", "#{window_index}", "-s", T.unpack pid] ""
            return $ readMaybe . T.unpack
                =<< listToMaybe (filter (not . T.null) (map T.strip (T.lines (T.pack out))))

-- | The window index of pane @pid@ (e.g. @%5@) — used by the ⌘W close menu's
-- "Hide Window" to add that window to 'hiddenWindows'.  'Nothing' on error.
windowIndexOfPane :: Text -> IO (Maybe Int)
windowIndexOfPane pid = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing   -> return Nothing
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "display-message", "-p", "-t", T.unpack pid, "#{window_index}"] ""
            return $ readMaybe . T.unpack
                =<< listToMaybe (filter (not . T.null) (map T.strip (T.lines (T.pack out))))

-- | Number of panes in session @n@'s CURRENT window (the tiling shown in wide0)
-- — 0 on any error / no tmux.  The ⌘W close menu uses @> 1@ to decide the
-- multi-pane vs single-pane form.
paneCountOfSession :: Text -> IO Int
paneCountOfSession n = (`catch` \(_ :: SomeException) -> return 0) $
    findExecutable "tmux" >>= \case
        Nothing   -> return 0
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-panes", "-t", T.unpack n, "-F", "#{pane_id}"] ""
            return $ length (filter (not . T.null) (map T.strip (T.lines (T.pack out))))

-- | Move pane @srcPaneId@ (a tmux pane id like @%5@) into window @dstW@ of
-- session @dstS@, splitting the target window.  Targeting by pane id keeps the
-- source unambiguous even after tmux renumbers pane indices on the move; used
-- by the Terminals-tree drag-and-drop (drag a pane row onto a window row).
moveTmuxPane :: Text -> Text -> Int -> IO ()
moveTmuxPane srcPaneId dstS dstW =
    tmuxCmd ["move-pane", "-s", T.unpack srcPaneId,
             "-t", T.unpack dstS <> ":" <> show dstW]

-- | Move a whole tmux WINDOW (@\@N@ — server-global, so no source session is
-- needed) to the end of session @dstS@, detached (@-d@: the destination's
-- current window stays current).  Used by the ⌘-drag pane move when a pane's
-- tmux window crosses to a leksah window backed by a different session — the
-- reconciler's 'keepPane' requires every pane's window to live in its leksah
-- window's own session.
moveTmuxWindow :: Text -> Text -> IO ()
moveTmuxWindow w dstS =
    tmuxCmd ["move-window", "-d", "-s", T.unpack w,
             "-t", T.unpack dstS <> ":"]

-- | Select window @w@ (@\@N@ — server-global) in whatever session holds it,
-- making it the session's current window.  The ⌘-drag pane move runs this
-- after landing a tmux pane so the CC widget's focus-follow settles on the
-- moved pane rather than snapping back to the destination's current window.
selectTmuxWindowId :: Text -> IO ()
selectTmuxWindowId w = tmuxCmd ["select-window", "-t", T.unpack w]

-- | Make pane @p@ (@%N@ — server-global) the active pane of its window AND
-- that window its session's current window (a pane id resolves both).
selectTmuxPaneId :: Text -> IO ()
selectTmuxPaneId p = do
    tmuxCmd ["select-window", "-t", T.unpack p]
    tmuxCmd ["select-pane", "-t", T.unpack p]

-- | The pane ids of window @w@ (@\@N@ — server-global), in tmux order.
panesOfWindow :: Text -> IO [Text]
panesOfWindow w = (`catch` \(_ :: SomeException) -> return []) $
    findExecutable "tmux" >>= \case
        Nothing   -> return []
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-panes", "-t", T.unpack w, "-F", "#{pane_id}"] ""
            return $ filter (not . T.null) (map T.strip (T.lines (T.pack out)))

-- | Join pane @p@ into @dst@'s window as a split on the given side (@horiz@ =
-- side-by-side, @before@ = left/top), detached.  @full@ (@-f@) makes the split
-- span the whole window; without it the split takes @dst@'s OWN cell and every
-- other pane keeps its size — the difference between the ⌘-drag pane move
-- landing on a leaf's outer edge and landing on ONE tmux pane inside it.
joinTmuxPane :: Text -> Text -> Bool -> Bool -> Bool -> IO ()
joinTmuxPane p dst horiz before full =
    tmuxCmd (["move-pane", "-d"] <> ["-f" | full]
             <> ["-h" | horiz] <> ["-b" | before]
             <> ["-s", T.unpack p, "-t", T.unpack dst])

-- | 'joinTmuxPane' full-size: a dragged tmux pane landing on a leaf whose
-- window shares its font joins that window as a real tmux split, across the
-- whole window on the dropped edge.
joinTmuxPaneFull :: Text -> Text -> Bool -> Bool -> IO ()
joinTmuxPaneFull p dst horiz before = joinTmuxPane p dst horiz before True

-- | The current layout string of window @w@ (@\@N@), checksum-prefixed
-- (tmux's @#{window_layout}@ — what 'IDE.Web.TmuxCC.parseLayout' reads).
windowLayoutString :: Text -> IO (Maybe Text)
windowLayoutString w = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing   -> return Nothing
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "display-message", "-p", "-t", T.unpack w,
                 "-F", "#{window_layout}"] ""
            return $ listToMaybe (filter (not . T.null) (map T.strip (T.lines (T.pack out))))

-- | Move pane @p@ into @dst@'s window (plain split beside @dst@, detached);
-- callers re-lay the window out afterwards ('selectWindowLayout').
movePaneToPane :: Text -> Text -> IO ()
movePaneToPane p dst =
    tmuxCmd ["move-pane", "-d", "-s", T.unpack p, "-t", T.unpack dst]

-- | Apply a full (checksum-prefixed) layout string to window @w@.
selectWindowLayout :: Text -> Text -> IO ()
selectWindowLayout w lay =
    tmuxCmd ["select-layout", "-t", T.unpack w, T.unpack lay]

-- | Swap two panes (@%N@ — server-global): their positions AND their spots
-- in the window's pane order, which is what a custom @select-layout@
-- assigns cells by.
swapTmuxPanes :: Text -> Text -> IO ()
swapTmuxPanes a b =
    tmuxCmd ["swap-pane", "-d", "-s", T.unpack a, "-t", T.unpack b]

-- | Break pane @p@ out into a fresh window at the end of session @dstS@
-- (cross-session works — pane ids are server-global), detached; returns the
-- NEW window's id (@\@N@).  The ⌘-drag pane move uses this when a dragged
-- tmux pane must become its own leksah pane (node/root target, view-leaf
-- target, or font mismatch).
breakTmuxPaneTo :: Text -> Text -> IO (Maybe Text)
breakTmuxPaneTo p dstS = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing   -> return Nothing
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "break-pane", "-d", "-P", "-F", "#{window_id}",
                 "-s", T.unpack p, "-t", T.unpack dstS <> ":"] ""
            return $ listToMaybe (filter (not . T.null) (map T.strip (T.lines (T.pack out))))

-- | Rename session @s@ (a session id) to @name@.
renameTmuxSession :: Text -> Text -> IO ()
renameTmuxSession s name = tmuxCmd ["rename-session", "-t", T.unpack s, T.unpack name]

-- | Rename window @w@ of session @s@ to @name@.
renameTmuxWindow :: Text -> Int -> Text -> IO ()
renameTmuxWindow s w name =
    tmuxCmd ["rename-window", "-t", T.unpack s <> ":" <> show w, T.unpack name]

-- | The id (e.g. @%3@) of the active pane of session @n@ (a tmux session id),
-- used to pick which pane to make transparent (see "IDE.Web.Main").
activePaneId :: Text -> IO (Maybe Text)
activePaneId n = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "display-message", "-p", "-t", T.unpack n, "-F", "#{pane_id}"] ""
            return $ case filter (not . T.null) (map T.strip (T.lines (T.pack out))) of
                (p:_) -> Just p
                _     -> Nothing

-- | The cell rectangle @(left, top, width, height)@ of pane @pid@ in session @n@
-- (a tmux session id) — but only while that pane's window is the session's
-- current one, so a transparency hole is hidden when its pane isn't on screen.
paneGeometry :: Text -> Text -> IO (Maybe (Int, Int, Int, Int))
paneGeometry n pid = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-panes", "-t", T.unpack n, "-F",
                 "#{pane_id} #{pane_left} #{pane_top} #{pane_width} #{pane_height} #{window_active}"] ""
            return $ listToMaybe
                [ (l, t, w, h)
                | line <- T.lines (T.pack out)
                , (pid':lT:tT:wT:hT:waT:_) <- [T.words line]
                , pid' == pid, waT == "1"
                , Just l <- [readMaybe (T.unpack lT)]
                , Just t <- [readMaybe (T.unpack tT)]
                , Just w <- [readMaybe (T.unpack wT)]
                , Just h <- [readMaybe (T.unpack hT)] ]

-- | Which tmux session (by session id) the pane @pid@ (e.g. @%20@) belongs to.
-- Used by @open-browser@ to map @$TMUX_PANE@ to a terminal so its pane can be
-- snapped.
sessionOfPane :: Text -> IO (Maybe Text)
sessionOfPane pid = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-panes", "-a", "-F", "#{session_id} #{pane_id}"] ""
            return $ listToMaybe
                [ sid
                | line <- T.lines (T.pack out)
                , (sid:pid':_) <- [T.words line]
                , pid' == pid ]

