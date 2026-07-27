{-# LANGUAGE OverloadedStrings #-}
module IDE.Web.Layout where

import qualified Data.Text as T (unwords)

import Clay
       (vh, height, (-:), grid, position, relative, none, (?), (#), after, Css)
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
    -- anchor point.
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
        "grid-template-rows" -: "20px 28px 1fr var(--wide1-row) var(--bar-row) 20px"
        "grid-template-columns" -: "var(--tall-col) 1fr"
        "grid-template-areas" -: T.unwords
          [ "\"menubar   menubar\""
          , "\"toolbar   toolbar\""
          , "\"tall      wide0\""
          , "\"tall      wide1\""
          , "\"bar       bar\""
          , "\"statusbar statusbar\""
          ]
        height (vh 100)
        position relative
    -- Side ("tall") pane visibility (the toolbar button cycles these classes on
    -- .leksah): hide collapses its column to nothing; auto-hide narrows it to a
    -- thin peek strip (~1/4 of the old ~12px peek) that widens to the full pane
    -- on hover.  Both keep the pane as a real grid column, so its tab buttons
    -- and body stay in the same vertical positions as in show mode and never
    -- overlay the wide (editor) panes -- the editor just starts after the
    -- (narrow) column.
    ".leksah.tall-hide" ?
        ("grid-template-columns" -: "0 1fr")
    ".leksah.tall-hide .area-tall" ?
        Clay.display none
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
    ".leksah.tall-auto" ? do
        "grid-template-columns" -: "0px 100vw"
        "transition" -: "grid-template-columns 0.15s ease"
    -- Stay open while the sensor or the pane itself is hovered *or* while a pane in
    -- the area has keyboard focus (:focus-within): activating/flipping to a side
    -- pane focuses its list, which holds the bar open; it collapses again on its
    -- own once focus leaves.
    ".leksah.tall-auto:has(.tall-sensor:hover, .area-tall:hover, .area-tall:focus-within, .tall-divider:hover)" ?
        ("grid-template-columns" -: "var(--tall-col) 100vw")
    -- Force-collapse override: a selection that activated a file/terminal adds
    -- '.tall-suppress' (see leksahCollapseAutoHide), snapping the pane shut even
    -- while the cursor is still hovering it — the extra class outranks the reveal
    -- rule above, so it wins under :hover.  Cleared on the next mouse-out.
    ".leksah.tall-auto.tall-suppress:has(.tall-sensor:hover, .area-tall:hover, .area-tall:focus-within, .tall-divider:hover)" ?
        ("grid-template-columns" -: "0px 100vw")
    -- A constant 3px left pad on the editor column, in ALL side-pane states
    -- (shown, auto-hide, hidden): a small consistent gap from the side divider /
    -- window edge (in auto-hide it also gives the sensor peek strip its room).
    -- The 1px grey line separating the side (tall) pane from the editor area
    -- lives on the editor column's own left edge (see the gated colour rules by
    -- the divider section below), not on a divider overlay.  It sits inside the
    -- border-box left of the 3px pad, so it stays a constant width in every
    -- side-pane state and never reflows the editor/terminal.
    ".area-wide0" ? do
        "padding-left" -: "3px"
        "padding-top" -: "3px"
        "box-sizing" -: "border-box"
        "border-left" -: "1px solid var(--leksah-border-line)"
    ".area-wide1" ? do
        "padding-left" -: "3px"
        "padding-top" -: "3px"
        "box-sizing" -: "border-box"
        "border-left" -: "1px solid var(--leksah-border-line)"
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
    ".leksah.tall-auto .tab.area-tall > *" ?
        ("width" -: "var(--tall-col)")
    -- When the web menu bar is hidden (native menu present), drop its row so
    -- the toolbar sits at the top with no empty strip.
    ".leksah.no-menubar" ? do
        "grid-template-rows" -: "28px 1fr var(--wide1-row) var(--bar-row) 20px"
        "grid-template-areas" -: T.unwords
          [ "\"toolbar   toolbar\""
          , "\"tall      wide0\""
          , "\"tall      wide1\""
          , "\"bar       bar\""
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
    -- The find bar is hidden by default and toggled by Edit ▸ Find (toolbar /
    -- menu).  When hidden we also collapse its grid row to 0 so it takes no
    -- space; `:has` lets the find bar's own class drive the grid container.
    ".findbar.hidden" ?
        Clay.display none
    -- 0px, not 0: the value is also used inside calc() (--wide0-slide-h),
    -- where a unitless zero makes the whole expression invalid.
    ".leksah:has(.findbar.hidden)" ?
        ("--bar-row" -: "0px")
    -- Bottom ("wide1") pane visibility (the toolbar button cycles these classes
    -- on .leksah): hide collapses its grid row; auto-hide keeps the row
    -- collapsed permanently and reveals the bar as a transform-only overlay --
    -- see the note below.
    -- Collapse the grid row to 0 (the bar is display:none in hide mode).  No
    -- !important needed: resizeBarsJs writes --wide1-bar, not --wide1-row, so
    -- nothing inline fights this class rule.
    ".leksah.wide1-hide" ?
        ("--wide1-row" -: "0px")
    ".leksah.wide1-hide .area-wide1" ?
        Clay.display none
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
    ".leksah.wide1-auto" ?
        ("--wide1-row" -: "0px")
    ".leksah.wide1-auto .tab-buttons.area-wide1" ? do
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "top" -: "calc(-1 * var(--wide1-bar))"
        -- 20px button row; the body below fills the rest of --wide1-bar.
        "height" -: "20px"
        "z-index" -: "1"
        "transform" -: "translateY(calc(var(--wide1-bar) + 20px))"
        "transition" -: "transform 0.15s ease"
    ".leksah.wide1-auto .tab.area-wide1" ? do
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "bottom" -: "0"
        "height" -: "calc(var(--wide1-bar) - 20px)"
        "z-index" -: "1"
        "transform" -: "translateY(calc(var(--wide1-bar) + 20px))"
        "transition" -: "transform 0.15s ease"
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .tab-buttons.area-wide1" ?
        ("transform" -: "translateY(0)")
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .tab.area-wide1" ?
        ("transform" -: "translateY(0)")
    ".leksah.wide1-auto .statusbar" ? do
        "position" -: "relative"
        "z-index" -: "2"
    ".leksah.wide1-auto .tab.area-wide0 > *" ?
        ("transition" -: "transform 0.15s ease")
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .tab.area-wide0 > *" ?
        ("transform" -: "translateY(calc(-1 * var(--wide1-bar)))")
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
    -- below).  The active-pane drop shadow is a separate overlay
    -- (.leksah-pane-hl), not this box.
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
    -- Hide each divider when its panel is hidden (else a stray 1px line lingers
    -- against a zero-width/height cell).
    ".leksah.tall-hide .tall-divider" ? Clay.display none
    ".leksah.wide1-hide .wide1-divider" ? Clay.display none
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
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within, .wide1-divider:hover) .wide1-divider" ? do
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
    ".leksah.tall-auto.leksah-resizing-tall" ? do
        "grid-template-columns" -: "var(--tall-col) 100vw"
        "transition" -: "none"
    ".leksah.wide1-auto.leksah-resizing-wide1 .tab-buttons.area-wide1" ?
        ("transform" -: "translateY(0)")
    ".leksah.wide1-auto.leksah-resizing-wide1 .tab.area-wide1" ?
        ("transform" -: "translateY(0)")
    ".leksah.wide1-auto.leksah-resizing-wide1 .tab.area-wide0 > *" ? do
        "transform" -: "translateY(calc(-1 * var(--wide1-bar)))"
        "transition" -: "none"
    ".leksah.wide1-auto.leksah-resizing-wide1 .wide1-divider" ? do
        "display" -: "block"
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "bottom" -: "0"
        "height" -: "var(--wide1-bar)"
        "z-index" -: "3"
    -- The tall↔editor separator (the editor columns' border-left, added above):
    -- show its grey only while the side (tall) pane is actually on-screen; hide
    -- it (transparent, never zero-width — so nothing reflows) otherwise.
    --   * tall-hide: side pane gone -> no line.
    --   * tall-auto: side pane collapsed to 0 -> no line, UNTIL it's revealed by
    --     hover/focus, when wide0's border reappears (the :has rule).
    --   * fully-shown mode (neither class): the base grey border stands.
    ".leksah.tall-hide .area-wide0" ? ("border-left-color" -: "transparent")
    ".leksah.tall-hide .area-wide1" ? ("border-left-color" -: "transparent")
    ".leksah.tall-auto .area-wide0" ? ("border-left-color" -: "transparent")
    ".leksah.tall-auto .area-wide1" ? ("border-left-color" -: "transparent")
    ".leksah.tall-auto:has(.tall-sensor:hover, .area-tall:hover, .area-tall:focus-within, .tall-divider:hover) .area-wide0" ?
        ("border-left-color" -: "var(--leksah-border-line)")
    -- The bottom bar in auto mode is a full-width floating overlay (left:0), so a
    -- left border there would be a stray vertical line at the window edge, not the
    -- tall boundary — keep it transparent.  (wide1 thus shows its border only when
    -- docked beside the tall pane in fully-shown mode.)
    ".leksah.wide1-auto .area-wide1" ? ("border-left-color" -: "transparent")
    -- (The tall↔editor separator is solely the editor columns' border-left,
    -- gated above; the .tall-divider draws nothing — it exists only as the
    -- ::after resize handle.  It used to also paint a 1px ::before line for the
    -- case where the editor area has no visible tab, but that risked stray
    -- second lines during the auto-hide animations and isn't worth it: a blank
    -- editor area just has no separator.)
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
    -- (The active-pane shadow for every area — side pane, editor/terminal, bottom
    -- bar — is drawn by the single '.leksah-pane-hl' overlay, positioned over the
    -- focused pane; the dividers now carry only the line.)
    -- Keyboard list navigation: the row the arrows have moved to (a
    -- .leksah-nav-item in a focused .leksah-nav pane), highlighted like a
    -- selected/active row.  And don't draw a focus ring around a whole focused
    -- list pane.
    ".leksah-nav-item.leksah-nav-current" ? ("background" -: "var(--leksah-selection)")
    ".leksah-nav:focus" ? ("outline" -: "none")
    ".leksah-vlist:focus" ? ("outline" -: "none")

