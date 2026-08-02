{-# LANGUAGE OverloadedStrings #-}
-- | The single source of truth for every UI colour.
--
-- All chrome colours (trees, tabs, toolbar, menus, panels, dialogs, the
-- status bar…) are expressed as CSS custom properties — the @--leksah-*@
-- tokens defined by 'paletteCss'.  The Clay stylesheets and the inline
-- @style@/JS strings reference them through the 'Color' helpers below
-- (e.g. 'fgColor' = @var(--leksah-fg)@), so a colour is never hard-coded at
-- a use site: to retune the palette, edit 'paletteCss' here and nothing else.
--
-- 'paletteCss' ships the dark defaults in @:root@ and a light override in an
-- @\@media (prefers-color-scheme: light)@ block, and sets @color-scheme@ to
-- match, so the whole UI follows the OS appearance automatically (and flips
-- live when the OS setting changes).  It is injected once at startup
-- (see 'IDE.Web.Main', in the @mainWidgetWithCss@ bytes, ahead of the Clay
-- stylesheet).
--
-- The user's Preferences colour pickers still apply live on top: 'themeVarsCss'
-- binds the selection colour and the hover *base* in a later @\<style\>@
-- element, which wins over the palette defaults.  The hover fill is derived
-- from that base per-mode ('paletteCss'), so a dark-navy default reads as a
-- pale tint in light mode rather than an unreadable block.
--
-- Colours that carry meaning (git status, log tags, the status traffic light,
-- error text, git-graph lanes) are deliberately NOT tokens — they stay literal
-- at their use sites and look the same in both themes.  The terminal (xterm)
-- and code-editor (Monaco/CodeMirror) surfaces keep their own dark theming and
-- are out of this palette's scope.
module IDE.Web.Theme
  ( -- * The palette (one place)
    paletteCss
  , contrastCss
  , themeVarsCss
    -- * Semantic colour tokens (all @var(--leksah-*)@)
    -- ** Text
  , fgColor
  , fgMutedColor
  , dimColor
    -- ** Surfaces / backgrounds
  , bgColor
  , bgSunkenColor
  , surfaceColor
  , surfaceAltColor
  , surfaceHiColor
  , surfaceSunkenColor
    -- ** Borders / dividers
  , borderTokenColor
  , controlBorderColor
  , lineColor
  , lineHiColor
  , faintBorderColor
    -- ** Accents
  , selectionColor
  , onAccentColor
  , selectionColorFaint
  , hoverColor
  , accentHoverColor
  , accentTextColor
    -- ** Gradient stops (top/bottom pairs)
  , barTopColor, barBottomColor
  , btnTopColor, btnBottomColor
  , btnHoverTopColor, btnHoverBottomColor
  , inputTopColor, inputBottomColor
  , menuTopColor, menuBottomColor
    -- ** Shadows / scrims
  , paneRingColor
  , glowColor
  , dropShadowColor
  , scrimColor
  , scrimFaintColor
    -- * Misc
  , dimOpacity
  ) where

import Data.Char (isHexDigit)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T (all, concat, length, uncons, unpack)

import Clay (Color(Other))
import Clay.Property (Number)

-- ---------------------------------------------------------------------------
-- Semantic colour tokens
--
-- Each is just a reference to a CSS variable defined by 'paletteCss'.  Use
-- these at CSS sites instead of Clay's @white@/@black@/@grey@ or raw @rgb()@.
-- ---------------------------------------------------------------------------

var :: Text -> Color
var name = Other (fromString ("var(--leksah-" <> T.unpack name <> ")"))

-- Text
fgColor, fgMutedColor, dimColor :: Color
fgColor      = var "fg"        -- primary / active / selected text
fgMutedColor = var "fg-muted"  -- dialog/pref/status body text
dimColor     = var "fg-dim"    -- de-emphasised labels, hints, dim icons

-- Surfaces / backgrounds
bgColor, bgSunkenColor, surfaceColor, surfaceAltColor, surfaceHiColor,
  surfaceSunkenColor :: Color
bgColor            = var "bg"             -- page / tab-strip / bottom bar
bgSunkenColor      = var "bg-sunken"      -- Preferences / Shortcuts scroll pane
surfaceColor       = var "surface"        -- dialogs, inputs, selects, osc-tip
surfaceAltColor    = var "surface-alt"    -- kbd chips, term hovertip
surfaceHiColor     = var "surface-hi"     -- toolbar tooltip
surfaceSunkenColor = var "surface-sunken" -- inline-rename box, confirm mask

-- Borders / dividers
borderTokenColor, controlBorderColor, lineColor, lineHiColor,
  faintBorderColor :: Color
borderTokenColor   = var "border"          -- general divider (gitlog idle)
controlBorderColor = var "border-control"  -- dialog/input/check/kbd borders
lineColor          = var "border-line"     -- visible 1px pane dividers
lineHiColor        = var "border-line-hi"  -- pane divider hover/drag
faintBorderColor   = var "border-faint"    -- menu separator line

-- Accents
selectionColor, onAccentColor, selectionColorFaint, hoverColor, accentHoverColor,
  accentTextColor :: Color
selectionColor      = var "selection"
-- | Text/icon colour to use *on top of* the selection/accent background — which
-- is a dark blue in BOTH themes, so this stays light (white) either way.  Use it
-- instead of 'fgColor' at selected/active sites, where 'fgColor' would flip to
-- near-black in light mode and become unreadable on the blue.
onAccentColor       = var "on-accent"
selectionColorFaint = Other "color-mix(in srgb, var(--leksah-selection) 45%, transparent)"
hoverColor          = var "hover"        -- run/action row hover fill
accentHoverColor    = var "accent-hover" -- tab/toolbar/findbar hover blue
accentTextColor     = var "accent-text"  -- light-blue sub-headings

-- Gradient stops
barTopColor, barBottomColor, btnTopColor, btnBottomColor,
  btnHoverTopColor, btnHoverBottomColor, inputTopColor, inputBottomColor,
  menuTopColor, menuBottomColor :: Color
barTopColor         = var "bar-top"
barBottomColor      = var "bar-bottom"
btnTopColor         = var "btn-top"
btnBottomColor      = var "btn-bottom"
btnHoverTopColor    = var "btn-hover-top"
btnHoverBottomColor = var "btn-hover-bottom"
inputTopColor       = var "input-top"
inputBottomColor    = var "input-bottom"
menuTopColor        = var "menu-top"
menuBottomColor     = var "menu-bottom"

-- The active pane's 1px ring: maximum contrast against the theme (white on
-- dark, black on light) — the model-driven per-pane highlight.
paneRingColor :: Color
paneRingColor = var "pane-ring-active"

-- Shadows / scrims
glowColor, dropShadowColor, scrimColor, scrimFaintColor :: Color
glowColor       = var "shadow-glow"  -- dialog box-shadow
dropShadowColor = var "shadow-drop"  -- menu/flipper/context/tooltip drop shadow
scrimColor      = var "scrim"         -- modal overlay backdrop
scrimFaintColor = var "scrim-faint"   -- region-capture overlay

-- | The matching de-emphasis level for B&W icon @\<img\>@ SVGs (which a text
-- 'color' can't touch): dim the non-selected ones to this opacity.
dimOpacity :: Number
dimOpacity = 0.5

-- ---------------------------------------------------------------------------
-- The palette
-- ---------------------------------------------------------------------------

-- | Every @--leksah-*@ token, defined once.  Dark values live in @:root@
-- (also the fallback if a browser lacks @prefers-color-scheme@); the light
-- branch overrides them under the media query.  @color-scheme@ is set in both
-- so native controls, scrollbars and form widgets follow suit.
--
-- @--leksah-selection@ / @--leksah-hover-pref@ get defaults here but are
-- normally supplied live by 'themeVarsCss' from the user's Preferences; the
-- hover *fill* (@--leksah-hover@) is derived from that base, differently per
-- mode, so it stays readable in light mode.
paletteCss :: Text
paletteCss = T.concat
  [ ":root{color-scheme:dark;"
  ,   commonVars
  ,   darkVars
  , "}"
  , "@media (prefers-color-scheme: light){:root{color-scheme:light;"
  ,   lightVars
  , "}}"
  ]
  where
    -- Mode-independent: the accent/pref-derived tokens.  --leksah-selection
    -- and --leksah-hover-pref are re-set live by themeVarsCss (a later style
    -- element wins); the derived hover fill is set per-mode below.
    commonVars = T.concat
      [ "--leksah-selection:#1e58d1;"
      , "--leksah-hover-pref:#0c1e46;"
      , "--leksah-scrim-faint:rgba(0,0,0,0.04);"
      -- Text/icons on the (dark-blue) selection background — light in both modes.
      , "--leksah-on-accent:#ffffff;"
      ]
    darkVars = T.concat
      [ "--leksah-fg:#ffffff;"
      , "--leksah-fg-muted:#dcdcdc;"
      , "--leksah-fg-dim:rgb(138,138,138);"
      , "--leksah-bg:#000000;"
      , "--leksah-bg-sunken:rgb(24,24,24);"
      , "--leksah-surface:rgb(40,40,40);"
      , "--leksah-surface-alt:rgb(44,44,44);"
      , "--leksah-surface-hi:rgb(64,64,64);"
      , "--leksah-surface-sunken:rgb(32,32,32);"
      , "--leksah-border:#333333;"
      , "--leksah-border-control:rgb(80,80,80);"
      , "--leksah-border-line:rgb(128,128,128);"
      , "--leksah-border-line-hi:rgba(190,190,190,0.9);"
      , "--leksah-border-faint:rgba(153,153,153,0.4);"
      , "--leksah-hover:var(--leksah-hover-pref,#0c1e46);"
      , "--leksah-accent-hover:#3d6096;"
      , "--leksah-accent-text:#9cdcfe;"
      , "--leksah-bar-top:rgb(32,32,32);"
      , "--leksah-bar-bottom:rgb(16,16,16);"
      , "--leksah-btn-top:rgb(64,64,64);"
      , "--leksah-btn-bottom:rgb(40,40,40);"
      , "--leksah-btn-hover-top:rgb(84,84,84);"
      , "--leksah-btn-hover-bottom:rgb(60,60,60);"
      , "--leksah-input-top:rgb(40,40,40);"
      , "--leksah-input-bottom:rgb(24,24,24);"
      , "--leksah-menu-top:rgb(64,64,64);"
      , "--leksah-menu-bottom:rgb(32,32,32);"
      , "--leksah-pane-ring-active:#ffffff;"
      , "--leksah-shadow-glow:rgba(128,128,128,0.9);"
      , "--leksah-shadow-drop:rgba(0,0,0,1);"
      , "--leksah-scrim:rgba(0,0,0,0.35);"
      , "--leksah-inset-bg:rgba(255,255,255,0.08);"
      , "--leksah-inset-line:rgba(255,255,255,0.14);"
      ]
    lightVars = T.concat
      [ "--leksah-fg:#1a1a1a;"
      , "--leksah-fg-muted:#333333;"
      , "--leksah-fg-dim:#6a6a6a;"
      , "--leksah-bg:#ffffff;"
      , "--leksah-bg-sunken:#f4f4f4;"
      , "--leksah-surface:#efefef;"
      , "--leksah-surface-alt:#e6e6e6;"
      , "--leksah-surface-hi:#ffffff;"
      , "--leksah-surface-sunken:#ededed;"
      , "--leksah-border:#d4d4d4;"
      , "--leksah-border-control:#c6c6c6;"
      , "--leksah-border-line:#c2c2c2;"
      , "--leksah-border-line-hi:rgba(110,110,110,0.9);"
      , "--leksah-border-faint:rgba(0,0,0,0.14);"
      , "--leksah-hover:color-mix(in srgb, var(--leksah-hover-pref,#0c1e46) 28%, white);"
      , "--leksah-accent-hover:#d3e2ff;"
      , "--leksah-accent-text:#0b60c0;"
      , "--leksah-bar-top:#f3f3f3;"
      , "--leksah-bar-bottom:#e3e3e3;"
      , "--leksah-btn-top:#fdfdfd;"
      , "--leksah-btn-bottom:#e8e8e8;"
      , "--leksah-btn-hover-top:#ffffff;"
      , "--leksah-btn-hover-bottom:#dcdcdc;"
      , "--leksah-input-top:#ffffff;"
      , "--leksah-input-bottom:#f0f0f0;"
      -- Dropdown / context menus: opaque very light grey (a menu must not be
      -- see-through).  The flipper has its own solid background.
      , "--leksah-menu-top:#f2f2f2;"
      , "--leksah-menu-bottom:#e8e8e8;"
      , "--leksah-pane-ring-active:#000000;"
      , "--leksah-shadow-glow:rgba(0,0,0,0.28);"
      , "--leksah-shadow-drop:rgba(0,0,0,0.22);"
      , "--leksah-scrim:rgba(0,0,0,0.28);"
      , "--leksah-inset-bg:rgba(0,0,0,0.05);"
      , "--leksah-inset-line:rgba(0,0,0,0.12);"
      ]

-- | Cross-cutting light/dark contrast fixes, cleaner as global rules than as
-- edits scattered across a dozen widgets.
--
-- (1) Text on the selection/accent background stays light in BOTH themes.  The
-- selection colour is a dark blue in either theme, so this is a no-op in dark
-- mode (the text was already white) and only changes light mode, where the
-- widgets' 'fgColor' would otherwise render near-black on the blue (the
-- unreadable active-tab label, selected tree rows, hovered menu items, …).
--
-- (2) In light mode the monochrome white-stroke @\<img\>@ icons under @\/pics@
-- are inverted to black so they show on light surfaces — but NOT the colourful
-- set (@\/pics\/color@), the tango set, the colour-encoded terminal alert icons,
-- or any icon sitting on the (dark-blue) selection background, which stay white.
-- @\!important@ on the exceptions beats the attribute-selector specificity of
-- the base rule.  Injected after 'paletteCss'.
contrastCss :: Text
contrastCss = T.concat
  [ ".flipper-content button.selected,"
  , ".findbar button.selected,"
  , ".log .log-item.selected,"
  , ".errors .error-item.selected,"
  , ".changes .change-item.selected,"
  , ".grep .grep-item.selected,"
  , ".metadata .metadata-active,"
  , ".gitlog-commit.selected,.gitlog-file.selected,"
  , ".leksah-nav-item.leksah-nav-current,"
  , ".menu ul li:hover,.menubar ul li:hover,"
  -- Only the rows that actually get a blue FILL — a selected FILE or DIRECTORY
  -- row, or a context-menu-selected row.  NOT the active project/package/
  -- component (li.active), which has no fill and just shows full-brightness text
  -- on the normal background; forcing white there was invisible in light mode.
  , ".workspace li.file.active > label,.workspace li.dir.active > label,"
  , ".workspace input:checked + div"
  , "{color:var(--leksah-on-accent) !important;}"
  -- …EXCEPT the tree keyboard-cursor (nav-current), which Workspace/Terminals
  -- render as a blue OUTLINE over the normal (light) row background, not a fill —
  -- so its text must stay the normal foreground, not white-on-light.  (List panes
  -- keep nav-current as a solid fill, so they keep the white above.)
  , ".workspace .leksah-nav-item.leksah-nav-current,"
  , ".terminals .leksah-nav-item.leksah-nav-current"
  , "{color:var(--leksah-fg) !important;}"
  , "@media (prefers-color-scheme: light){"
  ,   "img[src*=\"/pics/\"][src$=\".svg\"]"
  ,   ":not([src*=\"/pics/color/\"]):not([src*=\"/pics/tango/\"]):not(.term-alert-icon)"
  ,   "{filter:invert(1);}"
      -- The calm Claude robot is a plain white mono icon, but shares the
      -- .term-alert-icon class with the colour-encoded alert glyphs (bell gold,
      -- silence, …) that must NOT invert.  Invert this one back in by src.
  ,   "img[src$=\"/pics/tree-claude.svg\"]{filter:invert(1);}"
      -- Icons sitting on the (dark-blue) selection FILL stay white (un-inverted).
      -- The workspace rows are scoped to the row's OWN icon (> label img /
      -- + div img) so icons of the CHILDREN under an active project — which are
      -- on the normal light background — still invert to black.
  ,   ".selected img[src*=\"/pics/\"],"
  ,   ".metadata-active img[src*=\"/pics/\"],"
  ,   ".workspace li.file.active > label img.tree-icon,"
  ,   ".workspace li.dir.active > label img.tree-icon,"
  ,   ".workspace input:checked + div img.tree-icon,"
  ,   ".toolbar-item.toggled .toolbar-button,"
  ,   ".toolbar-item.tall-state-show .toolbar-button,.toolbar-item.wide1-state-show .toolbar-button,"
  ,   ".toolbar-item.tall-state-auto .toolbar-button,.toolbar-item.wide1-state-auto .toolbar-button,"
  ,   ".findbar-button.selected"
  ,   "{filter:none !important;}"
      -- …but ANY tree row that is the keyboard-focus row (nav-current) renders as
      -- a blue OUTLINE on the light background with dark text — whether it also
      -- carries a would-be blue fill (a selected file/dir, or a radio-checked
      -- git/branch row via `input:checked + div`).  So its icon must go black to
      -- match the text, not stay white.  The `li > label …nav-current` chain is
      -- specific enough (0,4,3) to beat both the `input:checked + div` and the
      -- `li.file/dir.active` un-inverts above (tie → later source wins).
  ,   ".workspace li > label .leksah-nav-item.leksah-nav-current img.tree-icon,"
  ,   ".terminals li > label .leksah-nav-item.leksah-nav-current img.tree-icon"
  ,   "{filter:invert(1) !important;}"
      -- Git file-status decorations are tuned for a dark background; darken the
      -- lightest ones (modified gold, added/updated green) for contrast on white.
  ,   ".git-modified{color:#9a6700 !important;}"
  ,   ".git-added,.git-untracked,.git-renamed{color:#1a7f37 !important;}"
  , "}"
  ]

-- | Live bindings for the user's Preferences colour pickers.  Only a
-- @#rgb@/@#rrggbb@/@#rrggbbaa@ value is interpolated (the values are user
-- input); anything else falls back to the palette default.  Injected in a
-- @\<style\>@ after 'paletteCss', so these win for the two tokens they set.
--
-- Sets the selection colour directly (a blue accent reads fine in both modes)
-- and the hover *base* (@--leksah-hover-pref@); 'paletteCss' turns the base
-- into the actual hover fill, tinted per mode.
themeVarsCss :: Text -> Text -> Text
themeVarsCss selection hover =
    ":root { --leksah-selection: " <> checked selection "#1e58d1"
    <> "; --leksah-hover-pref: " <> checked hover "#0c1e46" <> "; }"
  where
    checked v dflt = if isHexColor v then v else dflt
    isHexColor v = case T.uncons v of
        Just ('#', rest) -> T.length rest `elem` [3, 6, 8] && T.all isHexDigit rest
        _                -> False
