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
       (shadowWithSpread, black, bsColor, boxShadow, white, color,
        fontSize, borderStyle, textDecoration, margin, middle, start,
        padding, px, borderRadius, vGradient, backgroundImage, inlineBlock,
        overflowY, pointerEvents, textAlign, width, height, pct, top,
        zIndex, absolute, position, borderColor, borderWidth, solid,
        (?), Css, Auto(..), None(..),
        Center(..), Color(..), Background(..), VerticalAlign(..))
import qualified Clay (display)

import Reflex
       (foldDyn, selectViewListWithKey, leftmost, holdUniqDyn,
        tag, gate, Dynamic, Event, MonadHold(..), Reflex(..),
        ffilter, fmapMaybe)
import Reflex.Dom.Core
       (el, elDynAttr', (=:), elDynAttr, divClass,
        MonadWidget, HasDomEvent(..), EventName(..))

import IDE.Web.Theme (selectionColor)

flipperCss :: Css
flipperCss = do
  ".flipper" ? do
    position absolute
    zIndex 100
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
    backgroundImage (vGradient (Rgba 64 64 64 0.9) (Rgba 32 32 32 0.9))
    -- Border in the owning OS window's colour (set per window as the
    -- --leksah-window-color CSS var, see flipMirrorJs).
    borderStyle solid
    borderWidth (px 6)
    borderColor (Other "var(--leksah-window-color)")
    borderRadius (px 12) (px 12) (px 12) (px 12)
    margin (px 20) (px 20) (px 20) (px 20)
    padding (px 10) (px 10) (px 10) (px 10)
    pointerEvents auto
    textAlign start
    boxShadow (pure $ bsColor black $ shadowWithSpread (px 0) (px 0) (px 10) (px 3))
  -- When the highlighted item lives in THIS window, thicken the border (6→12px)
  -- as a strong "the selected pane is here" cue.
  ".flipper-content.self-selected" ?
    borderWidth (px 12)
  ".flipper-content button" ? do
    verticalAlign middle
    borderRadius (px 3) (px 3) (px 3) (px 3)
    padding (px 2) (px 10) (px 2) (px 10)
    margin (px 0) (px 0) (px 0) (px 0)
    textDecoration none
    borderStyle none
    fontSize (px 13)
    background (Rgba 0 0 0 0.0)
    color white
  ".flipper-content button.selected" ?
    background selectionColor
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
    borderColor (Other "#888")

flipperWidget
  :: (MonadWidget t m, Ord k, Show k)
  => Dynamic t [(Text, k)]
  -> Event t Bool   -- ^ flip step: True = forward (⌘`), False = back (⌘⇧`); opens if hidden
  -> Event t ()     -- ^ commit (Command released)
  -> Dynamic t Bool -- ^ highlighted item is owned by this window (thick border)
  -> (Dynamic t k -> m ())
  -> m ( Dynamic t Bool                -- ^ overlay visible?
       , Dynamic t (Maybe (Text, k))   -- ^ the item currently highlighted in the flipper
       , Dynamic t Int                 -- ^ the highlighted item's index (for the mirror)
       , Event t (Map Text k) )        -- ^ committed tab selection
flipperWidget recentTabs flipStep flipdone selfSelD label = do
  let flipdown = () <$ ffilter id  flipStep
      flipup   = () <$ ffilter not flipStep

  numberOfTabsD <- holdUniqDyn $ length <$> recentTabs
  visibleD <- holdUniqDyn =<< holdDyn False (leftmost [ True <$ flipdown, True <$ flipup, False <$ flipdone ])
  selectionIndexD <- foldDyn ($) (0::Int) $ leftmost
    [ (\n x -> let x' = succ x in if x' >= n then 0 else x') <$> tag (current numberOfTabsD) flipdown
    , (\n x -> let x' = pred x in if x' < 0 then n - 1 else x')  <$> tag (current numberOfTabsD) flipup
    , const 0 <$ flipdone
    ]
  let selectionD = listToMaybe <$> (drop <$> selectionIndexD <*> recentTabs)
  clickE <- fmap (fmap (mconcat . (^.. traverse))) $
    elDynAttr "div" ((("class" =: "flipper") <>) . bool ("style" =: "display:none") mempty <$> visibleD) $
      divClass "flipper-scroll" $
        elDynAttr "div" (("class" =:) . ("flipper-content" <>) . bool "" " self-selected" <$> selfSelD) $
          selectViewListWithKey selectionIndexD (M.fromList . zip [0..] <$> recentTabs) $ \_ x s -> do
            (e, _) <- el "div" $ elDynAttr' "button" (bool mempty ("class" =: "selected") <$> s) $ label (snd <$> x)
            return $ (uncurry M.singleton) <$> tag (current x) (domEvent Click e)
  -- flipdone fires on *every* Command release, even a bare ⌘ tap with no flip.
  -- Only commit a selection when the flipper was actually up (visibleD, set by
  -- flipdown/flipup) — otherwise a lone ⌘ would re-select the most-recent tab,
  -- which now focuses that pane and pops its auto-hide bar open.
  return ( visibleD
         , selectionD
         , selectionIndexD
         , leftmost [ clickE, uncurry M.singleton <$> fmapMaybe id (tag (current selectionD) (gate (current visibleD) flipdone)) ] )
