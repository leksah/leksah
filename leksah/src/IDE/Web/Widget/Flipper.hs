{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Flipper
  ( flipperCss
  , flipperWidget
  ) where

import Control.Lens ((^..))

import Data.Bool (bool)
import Data.Map (Map)
import qualified Data.Map as M (singleton, fromList)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import Clay
       (shadowWithSpread, bsColor, boxShadow, color,
        fontSize, borderStyle, textDecoration, margin, middle, start,
        padding, px, borderRadius, inlineBlock,
        overflowY, pointerEvents, textAlign, width, height, pct, top,
        zIndex, absolute, position, (-:), borderColor, borderWidth, solid,
        (?), Css, Auto(..), None(..),
        Center(..), Color(..), Background(..), VerticalAlign(..))
import qualified Clay (display)

import Reflex
       (foldDyn, selectViewListWithKey, leftmost, holdUniqDyn,
        tag, gate, Dynamic, Event, MonadHold(..), Reflex(..),
        ffilter, fmapMaybe)
import Reflex.Dom.Core
       (el, elDynAttr', (=:), elDynAttr, divClass, HasDomEvent(..), EventName(..))

import IDE.Web.Theme (selectionColor, bgSunkenColor, dropShadowColor, fgColor)
import IDE.Web.Frame (MonadWidget)

flipperCss :: Css
flipperCss = do
  ".flipper" ? do
    position absolute
    -- Above the ⌘-held shortcut layer (badges and hint chips, z-index 200 in
    -- 'IDE.Web.Layout'): the flipper is driven by ⌘\`, so ⌘ is *always* down
    -- while it is open and those hints are always showing — at 100 the yellow
    -- chips punched through the overlay.  Still below the menus/dialogs (1000+).
    -- The cross-window mirror and the AI picker reuse this class, so they move
    -- with it.
    zIndex 300
    top (pct 5)
    height (pct 90)
    width (pct 100)
    textAlign center
    pointerEvents none
  ".flipper-scroll" ? do
    overflowY auto
    Clay.display inlineBlock
    height (pct 100)
  ".flipper-content" ? do
    -- Solid, fully opaque (the translucent gradient read badly); the sunken
    -- background tone in both themes.
    background bgSunkenColor
    -- Border in the owning OS window's colour (set per window as the
    -- --leksah-window-color CSS var, see flipMirrorJs).
    borderStyle solid
    borderWidth (px 6)
    -- Idle: a darkened version of the window colour.  When the highlighted item
    -- lives in THIS window the border switches to the full-brightness colour
    -- (below) rather than widening.
    borderColor (Other "color-mix(in srgb, var(--leksah-window-color) 55%, black)")
    borderRadius (px 12) (px 12) (px 12) (px 12)
    margin (px 20) (px 20) (px 20) (px 20)
    padding (px 10) (px 10) (px 10) (px 10)
    pointerEvents auto
    textAlign start
    boxShadow (pure $ bsColor dropShadowColor $ shadowWithSpread (px 0) (px 0) (px 10) (px 3))
  -- When the highlighted item lives in THIS window, switch the border to the
  -- full-brightness window colour (rather than widening it) as a strong "the
  -- selected pane is here" cue.
  ".flipper-content.self-selected" ?
    borderColor (Other "var(--leksah-window-color)")
  ".flipper-content button" ? do
    verticalAlign middle
    borderRadius (px 3) (px 3) (px 3) (px 3)
    padding (px 2) (px 10) (px 2) (px 10)
    margin (px 0) (px 0) (px 0) (px 0)
    textDecoration none
    borderStyle none
    fontSize (px 13)
    background (Rgba 0 0 0 0.0)
    color fgColor
  ".flipper-content button.selected" ?
    background selectionColor
  -- Opened by clicking the title: drop it down from where the click was
  -- (the --flip-at-* vars, set by 'flipAtTitleJs' as it opens) rather than
  -- the middle of the window, so the item you pick next is a short move
  -- away.  Fixed, so the coordinates are the viewport's.
  ".flipper.at-title" ? do
    "position" -: "fixed"
    "top" -: "var(--flip-at-top, 30px)"
    "left" -: "var(--flip-at-left, 0px)"
    "width" -: "auto"
    "height" -: "auto"
    "max-width" -: "calc(100vw - var(--flip-at-left, 0px))"
  ".flipper.at-title .flipper-scroll" ? do
    "height" -: "auto"
    "max-height" -: "calc(100vh - var(--flip-at-top, 30px) - 16px)"
  ".flipper.at-title .flipper-content" ?
    ("margin" -: "2px 8px 8px 0")
  -- Narrow mode (phones; see "IDE.Web.Layout"): opened by tapping the title,
  -- it fills the main region below the top bar, one full-width row per pane,
  -- tall enough to tap.
  "html.leksah-narrow .flipper" ? do
    "top" -: "36px"
    "height" -: "calc(100% - 36px)"
  "html.leksah-narrow .flipper-scroll" ? do
    "display" -: "block"
  "html.leksah-narrow .flipper-content" ? do
    "margin" -: "6px"
    "border-width" -: "3px"
  "html.leksah-narrow .flipper-content button" ? do
    "display" -: "block"
    "width" -: "100%"
    "text-align" -: "start"
    "font-size" -: "16px"
    "padding" -: "10px 12px"
  -- Per-entry window icon: a small square coloured by the owning window (filled
  -- via inline background-color; a hollow grey box for shared side/bottom panes).
  ".flip-win-icon" ? do
    Clay.display inlineBlock
    width (px 11)
    height (px 11)
    borderRadius (px 3) (px 3) (px 3) (px 3)
    borderStyle solid
    borderWidth (px 2)
    borderColor (Rgba 0 0 0 0.0)
    margin (px 0) (px 8) (px 0) (px 0)
    verticalAlign middle
  ".flip-win-icon.shared" ?
    borderColor (Other "var(--leksah-fg-dim)")
  -- Per-entry type icon: file icon for editors, the tmux-window icon (with its
  -- alert fill/colour) for terminals and their panes.  Sized to sit centred on
  -- the label, a small gap before the text.
  ".flip-type-icon" ? do
    height (px 14)
    verticalAlign middle
    margin (px 0) (px 6) (px 2) (px 0)

-- | The flipper overlay, also used for the AI-session picker (hence the
-- caller-supplied item type, label renderer and @openAt0@ input).
--
-- Two ways in.  A 'flipStep' opens it ALREADY ADVANCED — ⌘\` means "the previous
-- tab", so the first press must land on entry 1, not 0.  @openAt0@ opens it
-- sitting on entry 0 instead, for a list whose first entry is the one you
-- usually want (the active pane's default AI session); stepping from there
-- walks down as usual.
--
-- Two ways to pick, besides clicking: releasing the modifier (@flipdone@),
-- and @enterE@ (Enter / ⌘Enter), which commits the highlight AS IF CLICKED —
-- it shows up in the clicked-subset output too, so callers that treat a
-- click differently (the cross-window flipper moves a clicked tab into its
-- own window) treat Enter the same way.
flipperWidget
  :: (MonadWidget t m, Ord k, Show k)
  => Dynamic t [(Text, k)]
  -> Event t Bool   -- ^ flip step: True = forward (⌘`), False = back (⌘⇧`); opens if hidden
  -> Event t ()     -- ^ commit (Command released)
  -> Dynamic t Bool -- ^ highlighted item is owned by this window (thick border)
  -> Event t ()     -- ^ open highlighting entry 0 (no step); ignored when already up
  -> Event t ()     -- ^ dismiss WITHOUT committing (Escape; the flipper: its window losing focus)
  -> Event t ()     -- ^ commit the highlight as if it had been clicked (Enter)
  -> Dynamic t Bool -- ^ drop down from the title ('flipAtTitleJs' placed it) instead of centring
  -> (Dynamic t k -> m ())
  -> m ( Dynamic t Bool                -- ^ overlay visible?
       , Dynamic t (Maybe (Text, k))   -- ^ the item currently highlighted in the flipper
       , Dynamic t Int                 -- ^ the highlighted item's index (for the mirror)
       , Event t (Map Text k)          -- ^ committed tab selection
       , Event t (Map Text k) )        -- ^ …the subset made by CLICKING an item
                                       --   (fires alongside the one above)
flipperWidget recentTabs flipStep flipdone selfSelD openAt0 cancelE enterE atTitleD label = do
  let flipdown = () <$ ffilter id  flipStep
      flipup   = () <$ ffilter not flipStep

  numberOfTabsD <- holdUniqDyn $ length <$> recentTabs
  visibleD <- holdUniqDyn =<< holdDyn False
   (leftmost [ True <$ flipdown, True <$ flipup, True <$ openAt0
             , False <$ flipdone, False <$ cancelE, False <$ enterE ])
  selectionIndexD <- foldDyn ($) (0::Int) $ leftmost
    [ (\n x -> let n' = max 1 n; x' = succ x in if x' >= n' then 0 else x') <$> tag (current numberOfTabsD) flipdown
    , (\n x -> let n' = max 1 n; x' = pred x in if x' < 0 then n' - 1 else x')  <$> tag (current numberOfTabsD) flipup
    , const 0 <$ flipdone
    , const 0 <$ openAt0
    , const 0 <$ cancelE
    , const 0 <$ enterE
    ]
  let selectionD = listToMaybe <$> (drop <$> selectionIndexD <*> recentTabs)
  clickE <- fmap (fmap (mconcat . (^.. traverse))) $
    elDynAttr "div" ((\vis atTitle -> ("class" =: ("flipper" <> bool "" " at-title" atTitle))
                                     <> bool ("style" =: "display:none") mempty vis)
                       <$> visibleD <*> atTitleD) $
      divClass "flipper-scroll" $
        elDynAttr "div" (("class" =:) . ("flipper-content" <>) . bool "" " self-selected" <$> selfSelD) $
          selectViewListWithKey selectionIndexD (M.fromList . zip [0..] <$> recentTabs) $ \_ x s -> do
            (e, _) <- el "div" $ elDynAttr' "button" (bool mempty ("class" =: "selected") <$> s) $ label (snd <$> x)
            return $ (uncurry M.singleton) <$> tag (current x) (domEvent Click e)
  -- flipdone fires on *every* Command release, even a bare ⌘ tap with no flip.
  -- Only commit a selection when the flipper was actually up (visibleD, set by
  -- flipdown/flipup) — otherwise a lone ⌘ would re-select the most-recent tab,
  -- which now focuses that pane and pops its auto-hide bar open.
  -- Enter: the highlight, committed as a click would commit it.
  let enterPickE = uncurry M.singleton <$>
        fmapMaybe id (tag (current selectionD) (gate (current visibleD) enterE))
      pickedE = leftmost [clickE, enterPickE]
  return ( visibleD
         , selectionD
         , selectionIndexD
         , leftmost [ pickedE, uncurry M.singleton <$> fmapMaybe id (tag (current selectionD) (gate (current visibleD) flipdone)) ]
         , pickedE )
