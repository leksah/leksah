{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Tabs
  ( tabsCss
  , tabsWidget
  ) where

import Control.Arrow (Arrow(..))
import Control.Lens ((^..))

import Data.Bool (bool)
import Data.Map (Map)
import qualified Data.Map as M
       (toList, fromList, elems, filter)
import qualified Data.Set as S (member, fromList)
import Data.Text (Text)
import Data.Tuple (swap)

import Clay
       (nowrap, whiteSpace, marginTop, scroll, overflow, white,
        color, fontSize, borderStyle, textDecoration, middle, vGradient,
        backgroundImage, borderRadius, padding, hover, (#), background,
        margin, px, height, (?), Css, Color(..), VerticalAlign(..),
        Auto(..), Hidden(..), None(..))

import Reflex (foldDyn, holdUniqDyn, listViewWithKey, Dynamic)
import Reflex.Dom.Core
       (elDynAttr', elDynAttr, MonadWidget, (=:), divClass,
        Event, domEvent, EventName(..))

tabsCss :: Css
tabsCss = do
    ".tab-buttons" ? do
        backgroundImage (vGradient (Rgba 32 32 32 1.0) (Rgba 16 16 16 1.0))
        height (px 40)
        overflow scroll
        whiteSpace nowrap
    ".tab-buttons button" ? do
        verticalAlign middle
        borderRadius (px 3) (px 3) (px 3) (px 3)
        padding (px 2) (px 10) (px 2) (px 10)
        margin (px 0) (px 0) (px 0) (px 0)
        textDecoration none
        borderStyle none
        fontSize (px 13)
        background (Rgba 0 0 0 0.0)
        color white
    ".tab-buttons button" # hover ?
        background (Rgba 61 96 150 1.0)
    ".tab-buttons button.selected" ?
        background (Rgba 30 88 209 1.0)
    ".tab" ? do
        marginTop (px 20)
        height auto
        overflow hidden

tabsWidget
  :: (MonadWidget t m, Ord k, Show k)
  => Map k (Text, v)
  -> Map Text k
  -> Event t (Map k (Text, v))
  -> Event t (Map Text k)
  -> (k -> Dynamic t v -> m ())
  -> (k -> Dynamic t v -> m (Event t e))
  -> m (Dynamic t [(Text, k)], Event t (Map k e))
tabsWidget initialTabs initialVisibleTabs openTabE selectTabE mkLabel mkTab = mdo
  let selectOrOpenTab = selectTabE' <> selectTabE <> (M.fromList . map (swap . second fst) . M.toList <$> openTabE)
  visibleTabs <- foldDyn (<>) initialVisibleTabs selectOrOpenTab
  tabsD <- foldDyn (<>) initialTabs openTabE
  allVisibleTabs <- holdUniqDyn $ S.fromList . M.elems <$> visibleTabs
  recentTabs <- foldDyn (\new old -> new <> filter ((`notElem` map snd new) . snd) old)
    (map swap . M.toList $ fst <$> initialTabs) $ M.toList <$> selectOrOpenTab
  selectTabE' <- fmap (fmap (mconcat . (^.. traverse . traverse))) $
        listViewWithKey visibleTabs $ \gridArea visibleTab -> do
    let tabs = M.filter ((gridArea ==) . fst) <$> tabsD
    divClass ("tab-buttons area-" <> gridArea) $
      listViewWithKey tabs $ \k v -> do
        let attrD = ("class" =:) . bool "" "selected" . (==k) <$> visibleTab
        (el, _) <- elDynAttr' "button" attrD $
          mkLabel k (snd <$> v)
        return $ (gridArea =: k) <$ domEvent Click el
  tabEvents <- listViewWithKey tabsD $ \k v -> do
    gridAreaD <- holdUniqDyn $ fst <$> v
    let attrD = do
            visTabs <- allVisibleTabs
            gridArea <- gridAreaD
            return $
                 ("class" =: ("tab area-" <> gridArea))
              <> bool ("style" =: "visibility:hidden;") mempty (S.member k visTabs)
    elDynAttr "div" attrD $
      mkTab k (snd <$> v)
  return (recentTabs, tabEvents)
