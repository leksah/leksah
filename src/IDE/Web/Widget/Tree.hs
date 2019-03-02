{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module IDE.Web.Widget.Tree
  ( simpleSvgPath
  , triangleDown
  , triangleRight
  , treeSelect
  , treeSelect'
  , treeItemDynAttr
  , treeItem
  ) where

import Control.Lens
       ((#), (.~), _Wrapped)
import Control.Monad (void)

import Data.Bool (bool)
import Data.Default (def)
import Data.Function ((&))
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.Map (Map)
import Data.Text (Text)

import Reflex
       (constDyn, switchHold, leftmost, tag,
        holdUniqDyn, Dynamic, Event, never,
        holdDyn, current)
import Reflex.Dom.Core
       (inputElementConfig_setChecked, EventResult, Element, elClass',
        MonadWidget, DomBuilderSpace, dyn, (=:), elDynAttr, divClass, el,
        inputElement, inputElementConfig_elementConfig,
        elementConfig_initialAttributes, domEvent, EventName(..))

import Reflex.Dom.Widget.SVG (BasicSVG(..), svgBasicDyn_, svg_)
import Reflex.Dom.Widget.SVG.Types
       (PathCommand, makePathProps, SVG_El(..), Width(..),
        Height(..), ViewBox(..), _M, _PosX, _PosY, _l, _z)

import IDE.Web.Widget.ContextMenu (contextMenu)

simpleSvgPath
  :: MonadWidget t m
  => Dynamic t [PathCommand]
  -> m ()
simpleSvgPath path = do
  let dSVGEl = pure $ SVG_El (Width 12.0) (Height 16.0) (Just (ViewBox 0 0 (Width 12.0) (Height 16.0)))
  void . svg_ dSVGEl $
    svgBasicDyn_ Path makePathProps ((_Wrapped #) . NonEmpty.fromList <$> path)

triangleDown, triangleRight :: [PathCommand]
triangleDown =
  [ _M (_PosX # 0.0) (_PosY # 5.0)
  , _l (_PosX # 5.0) (_PosY # 5.0)
  , _l (_PosX # 5.0) (_PosY # (-5.0))
  , _z
  ]

triangleRight =
  [ _M (_PosX # 4.0) (_PosY # 13.0)
  , _l (_PosX # 5.0) (_PosY # (-5.0))
  , _l (_PosX # (-5.0)) (_PosY # (-5.0))
  , _z
  ]

treeSelect'
  :: MonadWidget t m
  => Text
  -> m (Event t a)
  -> m (Event t a)
  -> m (Element EventResult (DomBuilderSpace m) t, Event t a)
treeSelect' treeName m f = mdo
  (e, a) <- el "label" $ do
    void . inputElement $ def
      & inputElementConfig_setChecked .~ (True <$ showContextMenuE)
      & inputElementConfig_elementConfig . elementConfig_initialAttributes
        .~ ("class" =: "tree-select"
         <> "type"  =: "radio"
         <> "name"  =: treeName)
    elClass' "div" "tree-item" f
  (showContextMenuE, contextMenuE) <- contextMenu e m
  return (e, leftmost [contextMenuE, a])

treeSelect
  :: MonadWidget t m
  => Text
  -> m (Event t a)
  -> m (Event t a)
  -> m (Event t a)
treeSelect treeName m = fmap snd . treeSelect' treeName m

treeItemDynAttr
  :: MonadWidget t m
  => Dynamic t (Map Text Text)
  -> Bool
  -> m (Event t event)
  -> m (Event t event)
  -> m (Event t event)
treeItemDynAttr itemClass startExpanded item children = mdo
  expanded <- holdUniqDyn =<< holdDyn startExpanded (not <$> tag (current expanded) toggleExpanded)

  (toggleExpanded, events) <- elDynAttr "li" itemClass $ do
    (expander, _) <- elClass' "div" "tree-expand" $
      simpleSvgPath (bool triangleRight triangleDown <$> expanded)
    itemE <- item
    childE <- switchHold never =<< dyn (bool (return never) (divClass "tree-children" children) <$> expanded)
    return (domEvent Click expander, leftmost [itemE, childE])
  return events

treeItem
  :: MonadWidget t m
  => Text
  -> Bool
  -> m (Event t event)
  -> m (Event t event)
  -> m (Event t event)
treeItem itemClass = treeItemDynAttr (constDyn ("class" =: itemClass))
