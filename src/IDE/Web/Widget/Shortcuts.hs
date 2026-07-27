{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- | The Shortcuts pane: a read-only keyboard cheat sheet for the web UI.
--
-- It lists the shortcuts that are actually live in the front end, drawn from
-- the two places the web UI defines them so the sheet can't drift out of date:
--
--   * 'IDE.Web.Widget.Keymap.globalBindings' — the fixed global chords the
--     document key handler listens for (⇧⌘B build, ⌃J next error, ⌘F find, …),
--     plus the flipper and numbered-navigation chords that 'keymapWidget' adds
--     at runtime (represented here as an explicit \"Navigation\" block);
--   * 'IDE.Web.MenuModel.menus' — every menu item that carries a key
--     equivalent / shortcut hint (File, Terminal, AI, …), grouped by its menu.
--
-- Descriptions come from 'commandImageAndTip'.  The layout is a CSS multi-column
-- flow: each group stays intact ('break-inside: avoid') and the groups reflow
-- into as many columns as the pane is wide enough for, so it reads well as a
-- narrow side pane, a short wide bottom bar, or a large centre tab.
--
-- The pane is ⌘D-convertible like an editor / git-log tab (see
-- 'IDE.Web.Main'); its backing tmux \"twin\" pages 'shortcutsPlainText'.
module IDE.Web.Widget.Shortcuts
  ( shortcutsWidget
  , shortcutsCss
  , shortcutsPlainText
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Clay ((?), (-:), Css)

import Reflex (Dynamic, Event, never)
import Reflex.Dom.Core
       (MonadWidget, Key(..), elClass, text)

import IDE.Core.State (IDE)
import IDE.Web.Events (ShortcutsEvents)
import IDE.Web.Command (Command(..), commandImageAndTip)
import IDE.Web.MenuModel (MenuItem(..), menus, prettyKeySpec)
import IDE.Web.Widget.Keymap (globalBindings)

-- | One line of a group: a shortcut chord (already rendered to display glyphs)
-- and what it does; or a sub-heading dividing a group (nested menus).
data Line
  = Chord Text Text   -- ^ glyphs, description
  | Sub Text          -- ^ a sub-heading within a group

-- | A titled group of shortcut lines (rendered as one uninterrupted column
-- block).
type Group = (Text, [Line])

-- | The whole cheat sheet.
shortcutSections :: [Group]
shortcutSections = globalGroup : navGroup : menuGroups

-- | The fixed global chords, straight from the shared 'globalBindings'.
globalGroup :: Group
globalGroup =
  ("Global", [ Chord (chordGlyphs mods key) (describeCommand cmd)
             | (mods, key, cmd) <- globalBindings ])

-- | The flipper + numbered-navigation chords.  These are assembled inside
-- 'keymapWidget' (the flipper's modifier is host-dependent, and the numbered
-- chords are generated 1-9), so they aren't in 'globalBindings'; spell them out.
navGroup :: Group
navGroup =
  ("Window & Pane Navigation",
    [ Chord "⌘`"    "Flip to the next tab / pane (hold ⌘, release to commit)"
    , Chord "⇧⌘`"   "Flip to the previous tab / pane"
    , Chord "⌘1–9"  "Select the Nth split of the active terminal"
    , Chord "⌥⌘1–9" "Select the Nth side-bar pane"
    , Chord "⌃⌘1–9" "Select the Nth bottom-bar pane"
    ])

-- | Every menu that defines at least one shortcut, grouped by menu name.
menuGroups :: [Group]
menuGroups =
  [ (name, ls) | (name, items) <- menus, let ls = concatMap menuLines items, not (null ls) ]

-- | The shortcut lines a menu item contributes.  'MenuShortcut' (the display
-- \"hint\") already holds rendered glyphs; the real key equivalents
-- ('MenuKey' / 'MenuGlobalKey' / 'MenuSplitKey') hold parseable specs, so run
-- them through 'prettyKeySpec'.  Submenus become a sub-heading + their lines.
menuLines :: MenuItem -> [Line]
menuLines = \case
  MenuShortcut  lbl glyphs _ -> [Chord glyphs lbl]
  MenuKey       lbl spec   _ -> [Chord (prettyKeySpec spec) lbl]
  MenuGlobalKey lbl spec   _ -> [Chord (prettyKeySpec spec) lbl]
  MenuSplitKey  lbl spec   _ -> [Chord (prettyKeySpec spec) lbl]
  Submenu       title items  -> case concatMap menuLines items of
                                  [] -> []
                                  ls -> Sub title : ls
  _                          -> []

-- | Render a global binding's modifier set + trigger key to display glyphs
-- (macOS order ⌃⌥⇧⌘, matching 'prettyKeySpec').
chordGlyphs :: [Key] -> Key -> Text
chordGlyphs mods key = modGlyphs mods <> keyGlyph key

modGlyphs :: [Key] -> Text
modGlyphs mods = T.concat [ g | (m, g) <- order, m `elem` mods ]
  where order = [ (Control, "⌃"), (Alt, "⌥"), (Shift, "⇧"), (Command, "⌘") ]

-- | A trigger 'Key' as its printed glyph.  Covers the keys 'globalBindings'
-- actually uses, with a best-effort fallback (@KeyB@ → \"B\", @Digit1@ → \"1\").
keyGlyph :: Key -> Text
keyGlyph = \case
  Comma        -> ","
  ForwardSlash -> "/"
  Period       -> "."
  Backquote -> "`"
  k -> let s = T.pack (show k)
       in maybe (maybe s id (T.stripPrefix "Digit" s)) id (T.stripPrefix "Key" s)

-- | A human description for a bound command: its tooltip when it has one, else
-- a hand-written fallback for the plain display commands that carry none.
describeCommand :: Command -> Text
describeCommand cmd = case snd (commandImageAndTip cmd) of
  t | not (T.null t) -> t
  _ -> case cmd of
    CommandShowPreferences -> "Open the Preferences pane"
    CommandFocusAlert      -> "Jump to the next terminal wanting attention"
    _                      -> ""

-- The pane widget: static, read-only, emits nothing.  The IDE 'Dynamic' is
-- accepted (and ignored) to match the tab dispatch and leave room for a future
-- editable/customisable version.
shortcutsWidget
  :: forall t m. MonadWidget t m
  => Dynamic t IDE
  -> m (Event t ShortcutsEvents)
shortcutsWidget _ide = elClass "div" "shortcuts" $ do
  elClass "div" "sc-cols" $
    mapM_ renderGroup shortcutSections
  return never
  where
    -- Each group is split into break-inside-avoid "chunks" (its leading rows,
    -- then one per sub-heading), so a big group like Terminal/tmux flows across
    -- columns at chunk boundaries instead of running down a single column.  The
    -- group title rides with the first chunk so it can't be orphaned.
    renderGroup (title, ls) = elClass "div" "sc-group" $
      case chunkLines ls of
        []           -> chunk $ titleEl title
        (first:rest) -> do
          chunk $ titleEl title >> mapM_ renderLine first
          mapM_ (chunk . mapM_ renderLine) rest
    chunk     = elClass "div" "sc-chunk"
    titleEl t = elClass "div" "sc-title" $ text t
    renderLine (Sub title)      = elClass "div" "sc-sub" $ text title
    renderLine (Chord glyphs d) = elClass "div" "sc-row" $ do
      elClass "kbd" "sc-key" $ text glyphs
      elClass "div" "sc-desc" $ text d

-- | Split a group's lines into chunks kept together across columns: the leading
-- rows (if any), then one chunk per sub-heading (the heading plus its rows).
chunkLines :: [Line] -> [[Line]]
chunkLines [] = []
chunkLines (Sub t : rest) = (Sub t : rows) : chunkLines more
  where (rows, more) = span isRow rest
chunkLines ls = leading : chunkLines more
  where (leading, more) = span isRow ls

isRow :: Line -> Bool
isRow (Chord _ _) = True
isRow (Sub _)     = False

-- | The cheat sheet as aligned plain text — the content of the backing tmux
-- pane's pager (the ⌘D \"twin\" of the HTML view).
shortcutsPlainText :: Text
shortcutsPlainText = T.intercalate "\n" (concatMap group shortcutSections) <> "\n"
  where
    group (title, ls) = ["", "== " <> title <> " =="] ++ map line ls
    line (Sub s)       = "  -- " <> s
    line (Chord g d)   = "  " <> pad 8 g <> "  " <> d
    pad n t = t <> T.replicate (max 0 (n - T.length t)) " "

shortcutsCss :: Css
shortcutsCss = do
  ".shortcuts" ? do
    "height" -: "100%"
    "box-sizing" -: "border-box"
    "overflow" -: "auto"
    "padding" -: "10px 18px 24px 18px"
    "color" -: "var(--leksah-fg-muted)"
    "background" -: "var(--leksah-bg-sunken)"
    "font-size" -: "13px"
  -- As many ~300px columns as the pane is wide enough for; groups flow between
  -- them and scroll vertically in .shortcuts.
  ".shortcuts .sc-cols" ? do
    "column-width" -: "300px"
    "column-gap" -: "28px"
  ".shortcuts .sc-group" ? do
    "margin-bottom" -: "6px"
  -- A group flows across columns at its chunk boundaries; each chunk (leading
  -- rows, or a sub-heading + its rows) stays intact in one column.
  ".shortcuts .sc-chunk" ? do
    "break-inside" -: "avoid"
    "-webkit-column-break-inside" -: "avoid"
    "display" -: "inline-block"
    "width" -: "100%"
  ".shortcuts .sc-title" ? do
    "font-size" -: "15px"
    "font-weight" -: "bold"
    "color" -: "var(--leksah-fg)"
    "margin" -: "16px 0 6px 0"
    "border-bottom" -: "1px solid var(--leksah-border-control)"
    "padding-bottom" -: "3px"
  ".shortcuts .sc-sub" ? do
    "font-weight" -: "bold"
    "color" -: "var(--leksah-accent-text)"
    "margin" -: "8px 0 2px 0"
  ".shortcuts .sc-row" ? do
    "display" -: "grid"
    "grid-template-columns" -: "minmax(52px, max-content) 1fr"
    "gap" -: "10px"
    "align-items" -: "baseline"
    "padding" -: "2px 0"
  ".shortcuts .sc-key" ? do
    "justify-self" -: "end"
    "text-align" -: "right"
    "white-space" -: "nowrap"
    "font-family" -: "Hasklig, Menlo, monospace"
    "font-size" -: "12px"
    "color" -: "var(--leksah-fg-muted)"
    "background" -: "var(--leksah-surface-alt)"
    "border" -: "1px solid var(--leksah-border-control)"
    "border-radius" -: "4px"
    "padding" -: "1px 6px"
  ".shortcuts .sc-desc" ? do
    "min-width" -: "0"
    "color" -: "var(--leksah-fg-muted)"
