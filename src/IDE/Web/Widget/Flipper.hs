{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Flipper
  ( flipperCss
  , flipperWidget
  ) where

import Control.Lens ((^?), (^..))

import Data.Bool (bool)
import Data.Dependent.Map (DMap)
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as M (singleton, fromList)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import Clay
       (shadowWithSpread, black, bsColor, boxShadow, white, color,
        fontSize, borderStyle, textDecoration, margin, middle, start,
        padding, px, borderRadius, vGradient, backgroundImage, inlineBlock,
        overflowY, pointerEvents, textAlign, width, height, pct, top,
        zIndex, absolute, position, (?), Css, Auto(..), None(..),
        Center(..), Color(..), Background(..), VerticalAlign(..))
import qualified Clay (display)

import Reflex
       (foldDyn, selectViewListWithKey, leftmost, holdUniqDyn,
        tag, Dynamic, Event, MonadHold(..), Reflex(..),
        select)
import Reflex.Dom.Core
       (el, elDynAttr', (=:), elDynAttr, divClass,
        MonadWidget, HasDomEvent(..), EventName(..), FunctorMaybe(..))

import IDE.Web.Command (_CommandFlipDown, _CommandFlipUp, _CommandFlipDone)
import IDE.Web.Events (IDEWidget(..), _KeymapCommand)

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
    borderRadius (px 5) (px 5) (px 5) (px 5)
    margin (px 20) (px 20) (px 20) (px 20)
    padding (px 10) (px 10) (px 10) (px 10)
    pointerEvents auto
    textAlign start
    boxShadow (pure $ bsColor black $ shadowWithSpread (px 0) (px 0) (px 10) (px 3))
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
    background (Rgba 30 88 209 1.0)

flipperWidget
  :: (MonadWidget t m, Ord k, Show k)
  => Dynamic t [(Text, k)]
  -> Event t (DMap IDEWidget Identity)
  -> (Dynamic t k -> m ())
  -> m (Event t (Map Text k))
flipperWidget recentTabs allEvents label = do
  let keyEvents = select (fan allEvents) KeymapWidget
      commandE x = fmapMaybe (^? _KeymapCommand . x) keyEvents
      flipdown = commandE _CommandFlipDown
      flipup   = commandE _CommandFlipUp
      flipdone = commandE _CommandFlipDone

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
        divClass "flipper-content" $
          selectViewListWithKey selectionIndexD (M.fromList . zip [0..] <$> recentTabs) $ \_ x s -> do
            (e, _) <- el "div" $ elDynAttr' "button" (bool mempty ("class" =: "selected") <$> s) $ label (snd <$> x)
            return $ (uncurry M.singleton) <$> tag (current x) (domEvent Click e)
  return $ leftmost [ clickE, uncurry M.singleton <$> fmapMaybe id (tag (current selectionD) flipdone) ]
