{-# LANGUAGE OverloadedStrings #-}
module IDE.Web.Layout where

import qualified Data.Text as T (unwords)

import Clay
       (vh, height, (-:), grid, position, relative, none, (?), Css)
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
    ".tab-wrap .leksah-shortcut-badge" ? do
        "position" -: "absolute"
        "left" -: "0"
        "top" -: "0"
        "z-index" -: "6"
    -- The app fills the window and never scrolls as a whole; only individual
    -- panes scroll.  Without this, scrolling past the end of a pane (e.g. a long
    -- workspace tree) chains to the document and drags the entire UI.
    "html" ? ("height" -: "100%")
    "body" ? do
        "margin" -: "0"
        "height" -: "100%"
        "overflow" -: "hidden"
        "overscroll-behavior" -: "none"
    ".leksah" ? do
        Clay.display grid
        -- The bottom ("wide1") and find-bar ("bar") rows are sized via custom
        -- properties so the hide/auto-hide states (and the find-bar toggle) can
        -- each adjust just their own row without re-stating the whole template
        -- (and without a combinatorial explosion across the menubar variants).
        "--wide1-row" -: "150px"
        "--bar-row" -: "20px"
        "grid-template-rows" -: "20px 28px 1fr var(--wide1-row) var(--bar-row) 20px"
        "grid-template-columns" -: "300px 1fr"
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
    ".leksah.tall-auto:has(.tall-sensor:hover, .area-tall:hover, .area-tall:focus-within)" ?
        ("grid-template-columns" -: "300px 100vw")
    -- A constant 3px left pad on the editor column, in ALL side-pane states
    -- (shown, auto-hide, hidden): a small consistent gap from the side divider /
    -- window edge (in auto-hide it also gives the sensor peek strip its room).
    ".area-wide0" ? do
        "padding-left" -: "3px"
        "padding-top" -: "3px"
        "box-sizing" -: "border-box"
    ".area-wide1" ? do
        "padding-left" -: "3px"
        "padding-top" -: "3px"
        "box-sizing" -: "border-box"
    -- Keep the side pane's body laid out at its full width while collapsed, so
    -- its contents (e.g. the "New Terminal" button) don't reflow as the column
    -- narrows -- the narrow column just clips them.
    ".leksah.tall-auto .tab.area-tall > *" ?
        ("width" -: "300px")
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
    --     (40px buttons on top, 130px body below), parked just below the
    --     window at translateY(170px) = its 150px height + the 20px statusbar
    --     row -- parked fully off-screen, so no residual strip of it peeks
    --     over the statusbar while hidden -- and slides to translateY(0);
    --   * each wide0 tab's content slides up by the same 150px, its top
    --     clipped by the tab's overflow:hidden, and the vacated bottom strip
    --     is covered by the revealed bar;
    --   * the statusbar stacks above the sliding bar, so the bar emerges
    --     from behind it.
    ".leksah.wide1-auto" ?
        ("--wide1-row" -: "0px")
    ".leksah.wide1-auto .tab-buttons.area-wide1" ? do
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "top" -: "-150px"
        -- 20px, matching the base tab-button row height (the 130px body below
        -- makes the 150px bar); the buttons then tile flush onto the body.
        "height" -: "20px"
        "z-index" -: "1"
        "transform" -: "translateY(170px)"
        "transition" -: "transform 0.15s ease"
    ".leksah.wide1-auto .tab.area-wide1" ? do
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "bottom" -: "0"
        "height" -: "130px"
        "z-index" -: "1"
        "transform" -: "translateY(170px)"
        "transition" -: "transform 0.15s ease"
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within) .tab-buttons.area-wide1" ?
        ("transform" -: "translateY(0)")
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within) .tab.area-wide1" ?
        ("transform" -: "translateY(0)")
    ".leksah.wide1-auto .statusbar" ? do
        "position" -: "relative"
        "z-index" -: "2"
    ".leksah.wide1-auto .tab.area-wide0 > *" ?
        ("transition" -: "transform 0.15s ease")
    ".leksah.wide1-auto:has(.statusbar:hover, .area-wide1:hover, .area-wide1:focus-within) .tab.area-wide0 > *" ?
        ("transform" -: "translateY(-150px)")
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
        ("background" -: "rgb(0,0,0)")
    -- Divider overlays: a transparent box laid OVER the side pane / bottom bar
    -- (its own grid cell, so it sits on top of that panel's content).  It's
    -- click-through (pointer-events:none) so it never steals the panel's
    -- mouse-over (e.g. the 3px auto-hide peek keeps working).  It carries a 1px
    -- border on the edge next to the editor area, and — when one of that panel's
    -- panes is active (:focus-within) — a drop shadow cast onto the editor area,
    -- the same 64px mid-grey as the active terminal pane (.terminal-cc-hl).
    -- The side divider fills the whole side column, so its border sits at the
    -- pane's actual right edge (the boundary with the editor's 3px padding).
    ".tall-divider" ? do
        "grid-area" -: "tall"
        "border-right" -: "1px solid rgb(128,128,128)"
        "pointer-events" -: "none"
        "z-index" -: "20"
        position relative
    ".wide1-divider" ? do
        "grid-area" -: "wide1"
        "border-top" -: "1px solid rgb(128,128,128)"
        "pointer-events" -: "none"
        "z-index" -: "20"
        position relative
    -- Hide each divider when its panel is hidden (else a stray 1px line lingers
    -- against a zero-width/height cell).
    ".leksah.tall-hide .tall-divider" ? Clay.display none
    ".leksah.wide1-hide .wide1-divider" ? Clay.display none
    -- In bottom-bar auto-hide the wide1 row is 0 (the bar is a transform overlay),
    -- so the divider would collapse to a stray 1px line above the statusbar.
    ".leksah.wide1-auto .wide1-divider" ? Clay.display none
    -- While the side pane is collapsed to its 3px peek, the divider's border would
    -- sit hard against the window's left edge (x=0); margin can't push it past 0,
    -- so slide the whole overlay off-screen instead.  It slides back when the pane
    -- expands (hover/focus).
    ".leksah.tall-auto:not(:has(.tall-sensor:hover, .area-tall:hover, .area-tall:focus-within)) .tall-divider" ?
        ("transform" -: "translateX(-8px)")
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
    ".leksah-nav-item.leksah-nav-current" ? ("background" -: "rgb(30,88,209)")
    ".leksah-nav:focus" ? ("outline" -: "none")
    ".leksah-vlist:focus" ? ("outline" -: "none")

