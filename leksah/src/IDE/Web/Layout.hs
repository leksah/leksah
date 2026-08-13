{-# LANGUAGE OverloadedStrings #-}
module IDE.Web.Layout where

import qualified Data.Text as T (unwords)

import Clay
       ((-:), grid, position, relative, none, (?), (#), after,
        before, Css)
import qualified Clay (display)

layoutCss :: Css
layoutCss = do
    -- Navigation shortcut badges: pre-rendered hidden; badgesJs (IDE.Web.Main)
    -- adds .leksah-show-badges to <body> while ⌘ is held (when the preference
    -- enables the feature).  CC panes position theirs absolutely at the
    -- pane's top-left; tab buttons carry theirs inline (.tab-wrap anchors).
    ".leksah-shortcut-badge" ? do
        Clay.display none
        "background" -: "rgba(15,15,15,0.88)"
        "color" -: "#ffd866"
        "font-size" -: "11px"
        "font-weight" -: "bold"
        "padding" -: "1px 5px"
        "border-radius" -: "0 0 4px 0"
        "pointer-events" -: "none"
        "white-space" -: "nowrap"
    "body.leksah-show-badges .leksah-shortcut-badge" ?
        ("display" -: "block")
    ".tab-wrap" ? do
        position relative
    -- All tab-row badges (wide0/tall/wide1) float ABOVE their tab, centred over
    -- it.  They must be position:fixed: the tab strip is overflow:scroll, which
    -- clips any absolutely-positioned child that sits above the strip — that's
    -- why the above-row badges vanished.  hintsJs sets each badge's coordinates
    -- from its tab's live rect (fixed escapes the strip's clip).
    ".tab-buttons .leksah-shortcut-badge" ? do
        "position" -: "fixed"
        "transform" -: "translateX(-50%)"
        "border-radius" -: "4px"
        "z-index" -: "200"
    -- The ⌘` flip-destination suffix inside a numbered badge: hidden until
    -- hintsJs tags the target badge .leksah-flip-here (only while badges show).
    ".leksah-flip-suffix" ? Clay.display none
    "body.leksah-show-badges .leksah-shortcut-badge.leksah-flip-here .leksah-flip-suffix" ?
        ("display" -: "inline")
    -- Command-held navigation hint chips (⌘`, ⌘D, ⌘⇧D): a fixed overlay whose
    -- chips are positioned from live rects by leksahUpdateHints, shown only while
    -- the badges are revealed.  translate(-50%,-50%) centres each chip on its
    -- anchor point.  Both this layer and the tab badges above sit at 200 — the
    -- flipper deliberately outranks them at 300 ('flipperCss'), since ⌘ is held
    -- for the whole flip and the hints would otherwise cover the overlay.
    ".leksah-hints" ? do
        "position" -: "fixed"
        "left" -: "0"
        "top" -: "0"
        "right" -: "0"
        "bottom" -: "0"
        "pointer-events" -: "none"
        "z-index" -: "200"
    ".leksah-hint" ? do
        Clay.display none
        "position" -: "fixed"
        "transform" -: "translate(-50%,-50%)"
        "background" -: "rgba(15,15,15,0.88)"
        "color" -: "#ffd866"
        "font-size" -: "11px"
        "font-weight" -: "bold"
        "padding" -: "1px 6px"
        "border-radius" -: "4px"
        "pointer-events" -: "none"
        "white-space" -: "nowrap"
    "body.leksah-show-badges .leksah-hint" ?
        ("display" -: "block")
    -- The app fills the window and never scrolls as a whole; only individual
    -- panes scroll.  Without this, scrolling past the end of a pane (e.g. a long
    -- workspace tree) chains to the document and drags the entire UI.
    --
    -- Use `overflow: clip`, NOT `hidden`.  `hidden` still creates a scroll
    -- container, so it only suppresses *user* scrolling: a programmatic
    -- scroll — a `.focus()` / `.scrollIntoView()` on off-screen content, or
    -- xterm.js pulling its caret helper-textarea into view — can still scroll
    -- the whole document.  That is exactly what shifted the entire UI up by
    -- ~64px (toolbar off the top, statusbar too high): the bottom bar
    -- (`.area-wide1`) is parked below the viewport with `translateY`, which
    -- WebKit counts as scrollable overflow even through the `hidden`
    -- ancestor, giving a caret/focus scroll somewhere to go.  `clip` creates
    -- no scroll port at all, so `scrollTo`/`scrollIntoView` cannot move it —
    -- while still clipping the parked content, exactly as before.
    "html" ? do
        "height" -: "100%"
        "overflow" -: "clip"
        -- Window page zoom (⌘+/⌘−/⌘0), per OS window — see '_wwZoom'.  It goes
        -- on the ROOT element, not on .leksah, for two reasons: zooming the root
        -- scales the initial containing block, so the html→body→.leksah
        -- height:100% chain still resolves to exactly the viewport with no
        -- calc() compensation; and everything appended to document.body outside
        -- .leksah (the ⌘-drag shadow, tooltips, the flipper mirror, context
        -- menus, toasts) scales with the UI for free instead of needing its own
        -- rule.  The per-window value is published as --leksah-zoom by
        -- 'IDE.Web.Main.zoomVarCss'.
        --
        -- Beware the coordinate split this creates: getBoundingClientRect() and
        -- elementFromPoint are POST-zoom, while offsetWidth/clientWidth and
        -- getComputedStyle lengths are PRE-zoom.  JS that measures one and
        -- writes the other must divide — window.leksahLocal() is the one place
        -- that conversion lives.
        "--leksah-zoom" -: "1"
        "zoom" -: "var(--leksah-zoom, 1)"
    "body" ? do
        "margin" -: "0"
        "height" -: "100%"
        "overflow" -: "clip"
        "overscroll-behavior" -: "none"
    ".leksah" ? do
        Clay.display grid
        -- The bottom ("wide1") and find-bar ("bar") rows are sized via custom
        -- properties so the hide/auto-hide states (and the find-bar toggle) can
        -- each adjust just their own row without re-stating the whole template
        -- (and without a combinatorial explosion across the menubar variants).
        -- --wide1-bar is the bottom bar's CONTENT height (what the resize handle
        -- sets, persisted in localStorage).  The grid row (--wide1-row) follows it
        -- in show mode and is forced to 0 in hide/auto-hide, where the bar floats
        -- as a transform overlay sized from --wide1-bar instead of the grid track.
        "--wide1-bar" -: "150px"
        "--wide1-row" -: "var(--wide1-bar)"
        "--bar-row" -: "20px"
        -- Side ("tall") pane width, like --wide1-row for the bottom pane: a custom
        -- property so the drag-to-resize handle (see resizeBarsJs) can set it live
        -- on this element, and the collapse states override just the column track.
        "--tall-col" -: "300px"
        -- The find bar ("bar") row sits between the editor area and the bottom
        -- bar, in the editor column only (the side pane keeps its full height —
        -- a grid area must be rectangular, so a full-width bar there would cut
        -- the tall column in two).
        "grid-template-rows" -: "20px 28px 1fr var(--bar-row) var(--wide1-row) 20px"
        "grid-template-columns" -: "var(--tall-col) 1fr"
        "grid-template-areas" -: T.unwords
          [ "\"menubar   menubar\""
          , "\"toolbar   toolbar\""
          , "\"tall      wide0\""
          , "\"tall      bar\""
          , "\"tall      wide1\""
          , "\"statusbar statusbar\""
          ]
        -- 100%, NOT 100vh: viewport units are not divided by the root zoom, so
        -- at 150% a vh-sized root would be half again taller than the window.
        -- The percentage chain from a zoomed <html> resolves to exactly the
        -- viewport (measured).  Same reason for the 100vw grid tracks below.
        "height" -: "100%"
        position relative
    -- Side ("tall") pane visibility (the toolbar button cycles these classes on
    -- .leksah): hide and auto-hide both collapse the column to nothing and share
    -- the same slide-open reveal geometry (the ':is(.tall-auto, .tall-hide)'
    -- rules below); they differ ONLY in what reveals the pane.  Auto-hide opens
    -- on mouse-over (sensor / pane / divider hover) or keyboard focus; hide
    -- opens on keyboard focus alone — activating/flipping to a pane in the
    -- hidden bar shows the bar, but the mouse can't pull it open.  Both keep
    -- the pane as a real grid column, so its tab buttons and body stay in the
    -- same vertical positions as in show mode and never overlay the wide
    -- (editor) panes -- the editor just starts after the (collapsed) column.
    -- The content column has a *fixed* width (the full width minus the peek
    -- strip) that doesn't change on hover, so when the side pane slides in to its
    -- full width it pushes the wide panes to the right (their right edge slides
    -- off-screen and is clipped) instead of resizing them -- a terminal in there
    -- keeps its size, so it isn't reflowed/re-fitted while the pane is showing.
    -- Auto-hide collapses the side column all the way to 0 (nothing of it peeks);
    -- the editor column stays a constant 100vw with a 3px left padding standing in
    -- for the old peek strip -- blank editor padding, so there's nothing awkward to
    -- render there.  An invisible .tall-sensor strip laid over that padding catches
    -- the mouse and re-opens the pane (see below).  wide0 keeps its constant width
    -- (100vw, minus the 3px padding via border-box) across the hover so a terminal
    -- in there is never reflowed.
    ".leksah:is(.tall-auto, .tall-hide)" ? do
        "grid-template-columns" -: "0px 100%"
        "transition" -: "grid-template-columns 0.15s ease"
    -- Stay open while the sensor or the pane itself is hovered *or* while a pane in
    -- the area has keyboard focus (:focus-within): activating/flipping to a side
    -- pane focuses its list, which holds the bar open; it collapses again on its
    -- own once focus leaves.  In hide mode only the focus trigger applies — via
    -- the .tall-focus class barFocusJs stamps on .leksah, NOT a root
    -- :has(.area-tall:focus-within): WebKit doesn't reliably re-evaluate that
    -- :has when focus leaves the subtree, leaving the bar stuck open (auto mode
    -- gets away with it because its :hover terms re-invalidate on mouse moves).
    ".leksah.tall-auto:has(.tall-sensor:hover, .area-tall:hover, .area-tall:focus-within, .tall-divider:hover), .leksah.tall-hide.tall-focus" ?
        ("grid-template-columns" -: "var(--tall-col) 100%")
    -- Force-collapse override: a selection that activated a file/terminal adds
    -- '.tall-suppress' (see leksahCollapseAutoHide), snapping the pane shut even
    -- while the cursor is still hovering it — the extra class outranks the reveal
    -- rule above, so it wins under :hover.  Cleared on the next mouse-out.
    ".leksah.tall-auto.tall-suppress:has(.tall-sensor:hover, .area-tall:hover, .area-tall:focus-within, .tall-divider:hover)" ?
        ("grid-template-columns" -: "0px 100%")
    -- A constant 3px left pad on the editor column, in ALL side-pane states
    -- (shown, auto-hide, hidden): a small consistent gap from the side divider /
    -- window edge (in auto-hide it also gives the sensor peek strip its room).
    -- The 1px grey line separating the side (tall) pane from the editor area
    -- lives on the editor column's own left edge (see the gated colour rules by
    -- the divider section below), not on a divider overlay.  It sits inside the
    -- border-box left of the 3px pad, so it stays a constant width in every
    -- side-pane state and never reflows the editor/terminal.
    -- NO border-left here: the tall↔editor boundary line is drawn by the
    -- .tall-divider overlay (::before below) so it paints ABOVE pane
    -- content, exactly like every other divider line — one owner per
    -- boundary, and the pane boxes/rings can sit flush on the line's pixel.
    ".area-wide0" ? do
        "padding-left" -: "3px"
        "padding-top" -: "3px"
        "box-sizing" -: "border-box"
    ".area-wide1" ? do
        "padding-left" -: "3px"
        "padding-top" -: "3px"
        "box-sizing" -: "border-box"
    -- The 3px top pad above is for the editor/terminal BODY (the terminal pulls
    -- it back with a -3px margin); on the tab STRIP it just dropped the tab
    -- buttons 3px below the side-bar (tall) tabs.  Zero it on the strips so the
    -- wide0/wide1 tab buttons line up with the tall ones.  (padding-left stays,
    -- keeping the tabs aligned with the body's left edge.)
    ".tab-buttons.area-wide0" ? ("padding-top" -: "0")
    ".tab-buttons.area-wide1" ? ("padding-top" -: "0")
    -- Keep the side pane's body laid out at its full width while collapsed, so
    -- its contents (e.g. the "New Terminal" button) don't reflow as the column
    -- narrows -- the narrow column just clips them.
    ".leksah:is(.tall-auto, .tall-hide) .tab.area-tall > *" ?
        ("width" -: "var(--tall-col)")
    -- When the web menu bar is hidden (native menu present), drop its row so
    -- the toolbar sits at the top with no empty strip.
    ".leksah.no-menubar" ? do
        "grid-template-rows" -: "28px 1fr var(--bar-row) var(--wide1-row) 20px"
        "grid-template-areas" -: T.unwords
          [ "\"toolbar   toolbar\""
          , "\"tall      wide0\""
          , "\"tall      bar\""
          , "\"tall      wide1\""
          , "\"statusbar statusbar\""
          ]
    ".menubar" ? do
        "grid-area" -: "menubar"
    ".toolbar" ? do
        "grid-area" -: "toolbar"
    -- leksah-wkwebview: the window's title bar is transparent and the web
    -- content fills it (see setupMacTitlebar), so the toolbar sits in the title
    -- bar.  Pad its left so the first button clears the traffic-light buttons.
    ".leksah.mac-titlebar .toolbar" ?
        ("padding-left" -: "78px")
    ".findbar" ? do
        "grid-area" -: "bar"
        -- In the editor column (above the bottom bar) it shares the editor
        -- area's left edge (its left boundary line comes from the
        -- .tall-divider overlay, which spans all three editor-column rows);
        -- the top line marks the editor ↔ find-bar boundary (the bottom
        -- boundary is the wide1 divider's border-top).
        "padding-left" -: "3px"
        "box-sizing" -: "border-box"
        "border-top" -: "1px solid var(--leksah-border-line)"
    -- The find bar is hidden by default and toggled by Edit ▸ Find (toolbar /
    -- menu).  When hidden we also collapse its grid row to 0 so it takes no
    -- space; `:has` lets the find bar's own class drive the grid container.
    ".findbar.hidden" ?
        Clay.display none
    -- 0px, not 0: a custom property used inside calc() elsewhere must carry a
    -- unit — a unitless zero makes the whole expression invalid.
    ".leksah:has(.findbar.hidden)" ?
        ("--bar-row" -: "0px")
    -- Bottom ("wide1") pane visibility (the toolbar button cycles these classes
    -- on .leksah): hide and auto-hide both keep the grid row collapsed
    -- permanently and reveal the bar as a transform-only overlay (the shared
    -- ':is(.wide1-auto, .wide1-hide)' rules below); like the side pane, they
    -- differ only in the reveal trigger — auto opens on hover (statusbar / bar /
    -- divider) or keyboard focus, hide on keyboard focus alone, so activating a
    -- pane in the hidden bar shows the bar but the mouse can't.
    -- Bottom-bar auto-reveal must never change any element's SIZE: a size
    -- change reflows xterm (and, for CC panes, resizes the real tmux window).
    -- An earlier calc()-based version resized every wide0 terminal by ~10
    -- rows on each hover because calc(100% + 150px) tracks the *animating*
    -- grid row.  So in auto mode the wide1 grid track stays 0 forever and the
    -- reveal is pure transform (compositor-only; invisible to layout and
    -- ResizeObserver):
    --   * the bar (tab buttons + tab body) is absolutely positioned in its
    --     zero-height grid area with the same geometry it has in show mode
    --     (20px buttons on top, the rest below), parked just below the window at
    --     translateY(--wide1-bar + 20px) = its height + the 20px statusbar row --
    --     parked fully off-screen, so no residual strip peeks over the statusbar
    --     while hidden -- and slides to translateY(0);
    --   * each wide0 tab's content slides up by --wide1-bar, its top clipped by
    --     the tab's overflow:hidden, and the vacated bottom strip is covered by
    --     the revealed bar;
    --   * the statusbar stacks above the sliding bar, so the bar emerges from
    --     behind it.
    -- Sizing the overlay from --wide1-bar (not a hard-coded 150px) is what lets
    -- the bar be drag-resized in auto-hide and keep that height across mode
    -- switches.  The grid track itself stays 0, so nothing reflows:
    ".leksah:is(.wide1-auto, .wide1-hide)" ?
        ("--wide1-row" -: "0px")
    ".leksah:is(.wide1-auto, .wide1-hide) .tab-buttons.area-wide1" ? do
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "top" -: "calc(-1 * var(--wide1-bar))"
        -- 20px button row; the body below fills the rest of --wide1-bar.
        "height" -: "20px"
        "z-index" -: "1"
        "transform" -: "translateY(calc(var(--wide1-bar) + 20px))"
        "transition" -: "transform 0.15s ease"
    ".leksah:is(.wide1-auto, .wide1-hide) .tab.area-wide1" ? do
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "bottom" -: "0"
        "height" -: "calc(var(--wide1-bar) - 20px)"
        "z-index" -: "1"
        "transform" -: "translateY(calc(var(--wide1-bar) + 20px))"
        "transition" -: "transform 0.15s ease"
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .tab-buttons.area-wide1, .leksah.wide1-hide.wide1-focus .tab-buttons.area-wide1" ?
        ("transform" -: "translateY(0)")
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .tab.area-wide1, .leksah.wide1-hide.wide1-focus .tab.area-wide1" ?
        ("transform" -: "translateY(0)")
    ".leksah:is(.wide1-auto, .wide1-hide) .statusbar" ? do
        "position" -: "relative"
        "z-index" -: "2"
    ".leksah:is(.wide1-auto, .wide1-hide) .tab.area-wide0 > *" ?
        ("transition" -: "transform 0.15s ease")
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .tab.area-wide0 > *, .leksah.wide1-hide.wide1-focus .tab.area-wide0 > *" ?
        ("transform" -: "translateY(calc(-1 * var(--wide1-bar)))")
    -- The find bar sits in its own grid row directly above the (0-height) wide1
    -- area, i.e. exactly where the auto-hide bar's overlay slides up to — so it
    -- must ride up with the editor content or the revealed bar covers it.  Same
    -- transform, same timing, so the three (editor bottom / find bar / revealed
    -- bar top) tile seamlessly throughout the animation.
    ".leksah:is(.wide1-auto, .wide1-hide) .findbar" ?
        ("transition" -: "transform 0.15s ease")
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .findbar, .leksah.wide1-hide.wide1-focus .findbar" ?
        ("transform" -: "translateY(calc(-1 * var(--wide1-bar)))")
    -- The active-pane glow/ring overlays (terminalCss .leksah-pane-glow) are
    -- position:fixed and CSS-anchored to LAYOUT geometry, which transforms do
    -- not move — so through the bar's transform-only reveal they must ride
    -- the very same transforms as the content their anchors live in: the
    -- wide0-anchored overlay slides up with the editor content, the
    -- wide1-anchored one is parked off-screen and revealed with the bar, and
    -- the tall one never moves (side-pane content doesn't slide).  Same
    -- transition, same suppress and drag-resize overrides as the content.
    -- (.leksah-pane-left-line is wide0-anchored like the base glow, so it
    -- rides the base glow's transforms wherever they apply.)
    ".leksah:is(.wide1-auto, .wide1-hide) .leksah-pane-glow, .leksah:is(.wide1-auto, .wide1-hide) .leksah-pane-left-line" ?
        ("transition" -: "transform 0.15s ease")
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) :is(.leksah-pane-glow:not(.glow-tall):not(.glow-wide1), .leksah-pane-left-line), .leksah.wide1-hide.wide1-focus :is(.leksah-pane-glow:not(.glow-tall):not(.glow-wide1), .leksah-pane-left-line)" ?
        ("transform" -: "translateY(calc(-1 * var(--wide1-bar)))")
    ".leksah:is(.wide1-auto, .wide1-hide) .leksah-pane-glow.glow-wide1" ?
        ("transform" -: "translateY(calc(var(--wide1-bar) + 20px))")
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .leksah-pane-glow.glow-wide1, .leksah.wide1-hide.wide1-focus .leksah-pane-glow.glow-wide1" ?
        ("transform" -: "translateY(0)")
    ".leksah.wide1-auto.wide1-suppress:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) :is(.leksah-pane-glow:not(.glow-tall):not(.glow-wide1), .leksah-pane-left-line)" ?
        ("transform" -: "translateY(0)")
    ".leksah.wide1-auto.wide1-suppress:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .leksah-pane-glow.glow-wide1" ?
        ("transform" -: "translateY(calc(var(--wide1-bar) + 20px))")
    ".leksah:is(.wide1-auto, .wide1-hide).leksah-resizing-wide1 :is(.leksah-pane-glow:not(.glow-tall):not(.glow-wide1), .leksah-pane-left-line)" ? do
        "transform" -: "translateY(calc(-1 * var(--wide1-bar)))"
        "transition" -: "none"
    ".leksah:is(.wide1-auto, .wide1-hide).leksah-resizing-wide1 .leksah-pane-glow.glow-wide1" ? do
        "transform" -: "translateY(0)"
        "transition" -: "none"
    -- Force-collapse override (bottom bar): '.wide1-suppress' slides the bar back
    -- off-screen and un-shifts the editor content even while hovered, so a
    -- selection that activated a file/terminal snaps the bar shut with the cursor
    -- still over it.  The extra class outranks the reveal rules, so it wins.
    ".leksah.wide1-auto.wide1-suppress:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .tab-buttons.area-wide1" ?
        ("transform" -: "translateY(calc(var(--wide1-bar) + 20px))")
    ".leksah.wide1-auto.wide1-suppress:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .tab.area-wide1" ?
        ("transform" -: "translateY(calc(var(--wide1-bar) + 20px))")
    ".leksah.wide1-auto.wide1-suppress:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .tab.area-wide0 > *" ?
        ("transform" -: "translateY(0)")
    ".leksah.wide1-auto.wide1-suppress:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .findbar" ?
        ("transform" -: "translateY(0)")
    ".statusbar" ? do
        "grid-area" -: "statusbar"
    ".area-tall" ? do
        "grid-area" -: "tall"
    ".area-wide0" ? do
        "grid-area" -: "wide0"
    ".area-wide1" ? do
        "grid-area" -: "wide1"
    -- The bottom-bar panes get a solid black background: in auto-hide the bar is
    -- an absolutely-positioned overlay that slides up OVER the editor content, so
    -- a transparent pane would let that content show through behind it.
    ".tab.area-wide1" ?
        ("background" -: "var(--leksah-bg)")
    -- Divider overlays: a transparent box laid OVER the side pane / bottom bar
    -- (its own grid cell, so it sits on top of that panel's content).  It's
    -- click-through (pointer-events:none) so it never steals the panel's
    -- mouse-over (e.g. the 3px auto-hide peek keeps working).  The wide1 divider
    -- carries a 1px border on the edge next to the editor area; the tall (side)
    -- divider no longer draws its own line — that separator now lives on the
    -- editor column's own left edge (.area-wide0/.area-wide1 border-left, gated
    -- below).  The active-pane ring is per-pane chrome (see terminalCss), not
    -- this box.
    ".tall-divider" ? do
        "grid-area" -: "tall"
        "pointer-events" -: "none"
        "z-index" -: "20"
        position relative
    ".wide1-divider" ? do
        "grid-area" -: "wide1"
        "border-top" -: "1px solid var(--leksah-border-line)"
        "pointer-events" -: "none"
        "z-index" -: "20"
        position relative
    -- Drag-to-resize handles.  The dividers are pointer-events:none overlays (so
    -- clicks fall through to the pane content); a thin ::after edge strip with
    -- pointer-events:auto makes JUST the boundary grabbable.  Events on the strip
    -- still target the divider element, so resizeBarsJs keys on its class.  The
    -- tall handle sits on the sidebar↔editor edge (full height), the wide1 handle
    -- on the editor↔bottom-bar edge (full width); each drives --tall-col /
    -- --wide1-row live.
    ".tall-divider" # after ? do
        "content" -: "\"\""
        "position" -: "absolute"
        "top" -: "0"
        "bottom" -: "0"
        "right" -: "-3px"
        "width" -: "7px"
        "cursor" -: "col-resize"
        "pointer-events" -: "auto"
        "z-index" -: "21"
    ".wide1-divider" # after ? do
        "content" -: "\"\""
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "top" -: "-3px"
        "height" -: "7px"
        "cursor" -: "row-resize"
        "pointer-events" -: "auto"
        "z-index" -: "21"
    -- Consistent divider hover: the grab strip lights up with the same blue
    -- glow as the in-window pane dividers (native leaf / CC tmux gutters), so
    -- every resizable boundary answers the mouse the same way.  Also lit while
    -- a drag is live (resizeBarsJs's .leksah-resizing-* classes), so the glow
    -- can't flicker off when the cursor briefly overshoots the strip mid-drag.
    ".tall-divider:hover" # after ?
        ("background" -: "var(--leksah-hover)")
    ".wide1-divider:hover" # after ?
        ("background" -: "var(--leksah-hover)")
    ".leksah.leksah-resizing-tall .tall-divider" # after ?
        ("background" -: "var(--leksah-hover)")
    ".leksah.leksah-resizing-wide1 .wide1-divider" # after ?
        ("background" -: "var(--leksah-hover)")
    -- The tall↔editor boundary LINE lives here (::before): a 1px overlay just
    -- right of the tall column — the first pixel of the editor column — that
    -- paints ABOVE pane content (like every divider line), so pane boxes and
    -- the active ring can sit flush ON its pixel.  Grey normally; brightens
    -- on handle hover/drag (over the blue glow strip, like the native pane
    -- dividers' .divider-line).  It spans all three editor-column rows
    -- (wide0 / find bar / wide1) since grid-area tall does.
    ".tall-divider" # before ? do
        "content" -: "\"\""
        "position" -: "absolute"
        "top" -: "0"
        "bottom" -: "0"
        "right" -: "-1px"
        "width" -: "1px"
        "z-index" -: "22"
        "pointer-events" -: "none"
        "background" -: "var(--leksah-border-line)"
    ".wide1-divider" # before ? do
        "content" -: "\"\""
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "top" -: "-1px"
        "height" -: "1px"
        "z-index" -: "22"
        "pointer-events" -: "none"
        "display" -: "none"
        "background" -: "var(--leksah-border-line-hi)"
    ".leksah .tall-divider:hover" # before ?
        ("background" -: "var(--leksah-border-line-hi)")
    ".wide1-divider:hover" # before ? ("display" -: "block")
    ".leksah.leksah-resizing-tall .tall-divider" # before ?
        ("background" -: "var(--leksah-border-line-hi)")
    ".leksah.leksah-resizing-wide1 .wide1-divider" # before ?
        ("display" -: "block")
    -- Gate the line exactly as the old editor-column border-left was gated:
    -- no line while the side pane is hidden (tall-hide kills the whole
    -- divider below) or collapsed in auto mode — until the reveal.
    ".leksah.tall-auto .tall-divider" # before ?
        ("background" -: "transparent")
    ".leksah.tall-auto.tall-suppress .tall-divider" # before ?
        ("background" -: "transparent")
    ".leksah.tall-auto:has(.tall-sensor:hover, .area-tall:hover, .area-tall:focus-within, .tall-divider:hover) .tall-divider" # before ?
        ("background" -: "var(--leksah-border-line)")
    ".leksah.tall-auto:has(.tall-divider:hover) .tall-divider" # before ?
        ("background" -: "var(--leksah-border-line-hi)")
    ".leksah.tall-auto.leksah-resizing-tall .tall-divider" # before ?
        ("background" -: "var(--leksah-border-line-hi)")
    -- Hide each divider when its panel is hidden (else a stray 1px line lingers
    -- against a zero-width/height cell)…
    ".leksah.tall-hide .tall-divider" ? Clay.display none
    ".leksah.wide1-hide .wide1-divider" ? Clay.display none
    -- …except while the hidden bar is focus-revealed: then the divider (its
    -- boundary line and resize handle) comes back, matching the auto reveal.
    -- (The revealed wide1 divider's overlay geometry is in the shared reveal
    -- rule below.)
    ".leksah.tall-hide.tall-focus .tall-divider" ?
        ("display" -: "block")
    ".leksah.tall-hide.leksah-resizing-tall .tall-divider" ?
        ("display" -: "block")
    -- In bottom-bar auto-hide the wide1 row is 0 (the bar is a transform overlay),
    -- so the grid-positioned divider would be a stray 1px line above the statusbar
    -- — hide it while the bar is parked.
    ".leksah.wide1-auto .wide1-divider" ? Clay.display none
    -- …but when the bar is REVEALED, float the divider up to SPAN the revealed
    -- bar, so the bottom bar stays resizable in auto-hide.  Anchored to the bar's
    -- bottom (bottom:0 in the 0-height wide1 grid area = the statusbar top = the
    -- bar's bottom edge) with height:--wide1-bar, its TOP lands on the bar's top
    -- edge, where the ::after row-resize handle sits; its box is pointer-events:
    -- none so it doesn't block the bar's tabs, and z-index above the bar (1) and
    -- statusbar (2) keeps the handle grabbable.  Spanning the bar (rather than a
    -- 0-height line at the top) means getBoundingClientRect().bottom is the bar's
    -- bottom, which resizeBarsJs uses as the fixed edge for the drag height.
    -- Hovering the handle is in the reveal :has() above (.wide1-divider:hover), so
    -- grabbing it holds the bar open instead of collapsing it out from under you.
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .wide1-divider, .leksah.wide1-hide.wide1-focus .wide1-divider" ? do
        "display" -: "block"
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "bottom" -: "0"
        "height" -: "var(--wide1-bar)"
        "z-index" -: "3"
    -- While the side pane is collapsed to its 3px peek, the divider's ::after
    -- grab strip would sit at the window's left edge, catching hovers with a
    -- col-resize cursor.  Park it with display:none (NOT the old
    -- translateX(-8px) slide of the whole overlay: the transform snapped
    -- instantly while the column collapse animates, flashing the divider's
    -- then-line 8px left of the still-wide boundary).  It comes back when the
    -- pane expands (hover/focus), and stays during a resize drag — see
    -- leksah-resizing-tall below — so it can't vanish from under the cursor.
    ".leksah.tall-auto:not(.leksah-resizing-tall):not(:has(.tall-sensor:hover, .area-tall:hover, .area-tall:focus-within, .tall-divider:hover)) .tall-divider" # after ?
        ("display" -: "none")
    -- Keep it parked while the pane is force-collapsed (suppressed) even
    -- though we're technically still hovering — matching the collapsed pane.
    ".leksah.tall-auto.tall-suppress .tall-divider" # after ?
        ("display" -: "none")
    -- Drag-resize must not let an auto-hide pane collapse: while a drag is live
    -- resizeBarsJs marks .leksah with .leksah-resizing-tall / -wide1, and these
    -- rules pin the matching pane fully revealed (the hover-reveal geometry)
    -- regardless of where the cursor goes.  Without this, dragging OUTWARD to grow
    -- a pane moves the mouse off it and it snaps shut — so you could only ever drag
    -- it smaller.  transition:none so the live drag tracks the cursor instead of
    -- easing 0.15s behind it.
    ".leksah:is(.tall-auto, .tall-hide).leksah-resizing-tall" ? do
        "grid-template-columns" -: "var(--tall-col) 100%"
        "transition" -: "none"
    ".leksah:is(.wide1-auto, .wide1-hide).leksah-resizing-wide1 .tab-buttons.area-wide1" ?
        ("transform" -: "translateY(0)")
    ".leksah:is(.wide1-auto, .wide1-hide).leksah-resizing-wide1 .tab.area-wide1" ?
        ("transform" -: "translateY(0)")
    ".leksah:is(.wide1-auto, .wide1-hide).leksah-resizing-wide1 .tab.area-wide0 > *" ? do
        "transform" -: "translateY(calc(-1 * var(--wide1-bar)))"
        "transition" -: "none"
    ".leksah:is(.wide1-auto, .wide1-hide).leksah-resizing-wide1 .findbar" ? do
        "transform" -: "translateY(calc(-1 * var(--wide1-bar)))"
        "transition" -: "none"
    ".leksah:is(.wide1-auto, .wide1-hide).leksah-resizing-wide1 .wide1-divider" ? do
        "display" -: "block"
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "bottom" -: "0"
        "height" -: "var(--wide1-bar)"
        "z-index" -: "3"
    -- (The tall↔editor separator is the .tall-divider's ::before overlay
    -- line, gated in the divider block above — the editor columns carry no
    -- borders of their own.)
    -- The auto-hide activation strip: an invisible 3px-wide, full-height grid item
    -- pinned to the left of the (0-width, collapsed) side column, overflowing into
    -- the editor column's 3px left padding.  It exists only in tall-auto; hovering
    -- it re-opens the pane (see the :has rule above).  z-index keeps it above the
    -- editor so it catches the mouse in that padding.
    ".tall-sensor" ? do
        "grid-area" -: "tall"
        "justify-self" -: "start"
        "align-self" -: "stretch"
        "width" -: "3px"
        "z-index" -: "30"
        Clay.display none
    ".leksah.tall-auto .tall-sensor" ?
        ("display" -: "block")
    -- An empty, invisible, click-through grid item stretched over the WHOLE
    -- editor (wide0) cell, purely so JS can measure that cell.  The cross-window
    -- drag needs the editor area's box to offer a window with NO tabs as a drop
    -- target ('wide0AreaRect' in "IDE.Web.Main"), and every other element there
    -- is conditional: the '.tab-buttons.area-wide0' strip is only rendered when
    -- the window HAS a wide0 tab (listViewWithKey over the visible-tab map), so
    -- measuring from it made a fresh empty window untargetable — the one case
    -- the feature exists for.  Behind everything (z-index 0, no background) and
    -- pointer-events:none, so it neither paints nor takes the mouse.
    ".wide0-anchor" ? do
        "grid-area" -: "wide0"
        "align-self" -: "stretch"
        "justify-self" -: "stretch"
        "pointer-events" -: "none"
        "z-index" -: "0"
    -- (The active-pane highlight for every area is the per-pane chrome ring —
    -- '.terminal-cc-hl' markers / '.pane-chrome' leaf divs / the
    -- '.tab.tab-active' outline, all model-driven; the dividers carry only
    -- the line.)
    -- Keyboard list navigation: the row the arrows have moved to (a
    -- .leksah-nav-item in a focused .leksah-nav pane), highlighted like a
    -- selected/active row.  And don't draw a focus ring around a whole focused
    -- list pane.
    ".leksah-nav-item.leksah-nav-current" ? ("background" -: "var(--leksah-selection)")
    ".leksah-nav:focus" ? ("outline" -: "none")
    ".leksah-vlist:focus" ? ("outline" -: "none")

