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
    ".leksah.tall-auto" ? do
        "grid-template-columns" -: "3px calc(100vw - 3px)"
        "transition" -: "grid-template-columns 0.15s ease"
    -- Stay open while hovered *or* while a pane in the area has keyboard focus
    -- (:focus-within): activating/flipping to a side pane focuses its list, which
    -- holds the bar open; it collapses again on its own once focus leaves.
    ".leksah.tall-auto:has(.area-tall:hover, .area-tall:focus-within)" ?
        ("grid-template-columns" -: "300px calc(100vw - 3px)")
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
        "height" -: "40px"
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
    -- Keyboard list navigation: the row the arrows have moved to (a
    -- .leksah-nav-item in a focused .leksah-nav pane), highlighted like a
    -- selected/active row.  And don't draw a focus ring around a whole focused
    -- list pane.
    ".leksah-nav-item.leksah-nav-current" ? ("background" -: "rgb(30,88,209)")
    ".leksah-nav:focus" ? ("outline" -: "none")
    ".leksah-vlist:focus" ? ("outline" -: "none")

