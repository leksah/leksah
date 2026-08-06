{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- | The Shortcuts pane: a read-only keyboard cheat sheet for the web UI,
-- generated from the LIVE keybindings table ('IDE.Web.Keybindings') plus the
-- rendered menus — so user rules in @keybindings.json@ show here exactly as
-- they bind, and the sheet can't drift from reality:
--
--   * \"Global\" — every always-available binding (the DOM keydown table);
--   * \"Window & Pane Navigation\" — the @nav.*@ numbered chords, compressed
--     to their ranges (⌘1–9);
--   * one group per menu that carries shortcuts (Terminal, AI, tmux hints…).
--
-- The layout is a CSS multi-column flow: each group stays intact
-- ('break-inside: avoid') and the groups reflow into as many columns as the
-- pane is wide enough for, so it reads well as a narrow side pane, a short
-- wide bottom bar, or a large centre tab.
--
-- The pane is ⌘D-convertible like an editor / git-log tab (see
-- 'IDE.Web.Main'); its backing tmux \"twin\" pages 'shortcutsPlainText'.
module IDE.Web.Widget.Shortcuts
  ( shortcutsWidget
  , shortcutsCss
  , shortcutsPlainText
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (groupBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Clay ((?), (-:), Css)

import Reflex (Dynamic, Event, holdDyn, never, newTriggerEvent)
import Reflex.Dom.Core (MonadWidget, dyn, elClass, text)

import IDE.Core.State (IDE)
import IDE.Web.Chord (toGlyphs)
import IDE.Web.Command (commandImageAndTip)
import IDE.Web.Events (ShortcutsEvents)
import IDE.Web.Keybindings
       (Binding(..), CommandSpec(..), Keymap, When(..),
        registerKeymapListener)
import IDE.Web.MenuModel (MenuItem(..), prettyKeySpec, renderedMenus)

-- | One line of a group: a shortcut chord (already rendered to display glyphs)
-- and what it does; or a sub-heading dividing a group (nested menus).
data Line
  = Chord Text Text   -- ^ glyphs, description
  | Sub Text          -- ^ a sub-heading within a group

-- | A titled group of shortcut lines (rendered as one uninterrupted column
-- block).
type Group = (Text, [Line])

-- | The whole cheat sheet, from the resolved keymap.
shortcutSections :: Keymap -> [Group]
shortcutSections km = globalGroup km : navGroup km : menuGroups km

isNav :: Binding -> Bool
isNav b = "nav." `T.isPrefixOf` csId (bSpec b)

-- | Every always-available binding except the numbered navigation.
globalGroup :: Keymap -> Group
globalGroup km =
  ("Global",
    [ Chord (toGlyphs (bChord b)) (describeBinding b)
    | b <- km, csWhen (bSpec b) == WhenAlways, not (isNav b) ])

-- | The @nav.*@ chords, one row per (command, modifier set) with the digit
-- range compressed (⌘1–9); plus the flipper-commit note.
navGroup :: Keymap -> Group
navGroup km =
  ("Window & Pane Navigation",
    [ row grp
    | grp <- groupBy ((==) `on` navKey) [ b | b <- km, isNav b ] ]
    <> [ Chord "" "Hold the flip modifier, release to commit the tab flip" ])
  where
    navKey b = (csId (bSpec b), T.dropEnd 1 (toGlyphs (bChord b)))
    row grp@(b0:_) =
        let glyphs0 = toGlyphs (bChord b0)
            mods    = T.dropEnd 1 glyphs0
            range   = case map (T.takeEnd 1 . toGlyphs . bChord) grp of
                        []       -> ""
                        [d]      -> d
                        (d : ds) -> d <> "–" <> last ds
        in Chord (mods <> range) (describeBinding b0)
    row [] = Chord "" ""

-- | Every menu that defines at least one shortcut, grouped by menu name.
menuGroups :: Keymap -> [Group]
menuGroups km =
  [ (name, ls)
  | (name, items) <- renderedMenus km
  , let ls = concatMap menuLines items, not (null ls) ]

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

-- | A human description for a binding: its command's tooltip when it has
-- one, else the command's menu title.
describeBinding :: Binding -> Text
describeBinding b = fromMaybe (csTitle (bSpec b)) $ do
    cmd <- csMake (bSpec b) (bArgs b)
    case snd (commandImageAndTip cmd) of
        t | T.null t  -> Nothing
          | otherwise -> Just t

-- The pane widget: read-only; re-renders when the keybindings table reloads.
shortcutsWidget
  :: forall t m. MonadWidget t m
  => Dynamic t IDE
  -> m (Event t ShortcutsEvents)
shortcutsWidget _ide = do
  (kmE, fireKm) <- newTriggerEvent
  -- Fires immediately with the current table, then on every reload.
  liftIO (registerKeymapListener fireKm)
  kmD <- holdDyn [] kmE
  _ <- dyn (renderSheet <$> kmD)
  return never
  where
    renderSheet km = elClass "div" "shortcuts" $
      elClass "div" "sc-cols" $
        mapM_ renderGroup (shortcutSections km)
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
shortcutsPlainText :: Keymap -> Text
shortcutsPlainText km =
    T.intercalate "\n" (concatMap grp (shortcutSections km)) <> "\n"
  where
    grp (title, ls) = ["", "== " <> title <> " =="] ++ map line ls
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
