{-# LANGUAGE FlexibleContexts #-}
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
  , treeItemDynAttr'
  , treeItemDynAttrSet'
  , treeItem
  , treeItem'
  , scrollIntoViewNearest
  , dblclickMods
  , clickMods
  ) where

import Control.Lens
       ((#), (.~), (^.), _Wrapped)
import Control.Monad (void)

import Data.Bool (bool)
import Data.Default (def)
import Data.Function ((&))
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.Map (Map)
import qualified Data.Map as Map (insertWith)
import Data.Text (Text)

import Language.Javascript.JSaddle
       (liftJSM, toJSVal, obj, jss, js1, fun, jsg)

import Reflex
       (constDyn, switchHold, leftmost, tag, ffor,
        holdUniqDyn, Dynamic, Event, never, ffilter, updated, getPostBuild,
        holdDyn, current)
import Reflex.Dom.Core
       (inputElementConfig_setChecked, EventResult, Element, elClass', DomBuilderSpace, dyn, (=:), elDynAttr, divClass, el,
        inputElement, inputElementConfig_elementConfig,
        elementConfig_initialAttributes, domEvent, EventName(..),
        _element_raw, wrapDomEvent)

import GHCJS.DOM.EventM (event, onSync)
import GHCJS.DOM.GlobalEventHandlers (click, dblClick)
import GHCJS.DOM.MouseEvent (getAltKey, getShiftKey)
import GHCJS.DOM.Types (HTMLElement(..), uncheckedCastTo)
-- The source 'Element(..)' constructor must be in scope for uncheckedCastTo's
-- Coercible (qualified so it doesn't clash with Reflex.Dom.Core's Element).
import qualified GHCJS.DOM.Types as DOM (Element(..))

import Reflex.Dom.Widget.SVG (BasicSVG(..), svgBasicDyn_, svg_)
import Reflex.Dom.Widget.SVG.Types
       (PathCommand, makePathProps, SVG_El(..), Width(..),
        Height(..), ViewBox(..), _M, _PosX, _PosY, _l, _z)

import IDE.Web.Widget.ContextMenu (contextMenu)
import IDE.Web.Frame (MonadWidget, performEvent_)

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

-- | Fire on a double-click of @el@, carrying the (alt, shift) modifier state.
-- Used to route ⌥ / ⌥⇧ "open into a split pane" gestures from workspace rows —
-- reflex's @domEvent Dblclick@ discards modifiers, so read the raw 'MouseEvent'.
dblclickMods
  :: MonadWidget t m
  => Element EventResult (DomBuilderSpace m) t
  -> m (Event t (Bool, Bool))
dblclickMods el =
  wrapDomEvent (uncheckedCastTo HTMLElement (_element_raw el)) (`onSync` dblClick) $ do
    ev <- event
    (,) <$> getAltKey ev <*> getShiftKey ev

-- | Like 'dblclickMods' but for a single click (rows that open on click, e.g.
-- git-log branches).
clickMods
  :: MonadWidget t m
  => Element EventResult (DomBuilderSpace m) t
  -> m (Event t (Bool, Bool))
clickMods el =
  wrapDomEvent (uncheckedCastTo HTMLElement (_element_raw el)) (`onSync` click) $ do
    ev <- event
    (,) <$> getAltKey ev <*> getShiftKey ev

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
    elClass' "div" "tree-item leksah-nav-item" f
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
treeItemDynAttr = treeItemDynAttr' (constDyn False)

-- | Like 'treeItemDynAttr' but with a reveal 'Dynamic' that forces the node open
-- while it is 'True' (e.g. to reveal a node).  Manual clicks still toggle it.
-- We open on the reveal becoming True *and* at our own postBuild if it is already
-- True, so a node rendered after its parent expands (lazy children) still opens
-- for an active reveal.  We never force-close.
treeItemDynAttr'
  :: MonadWidget t m
  => Dynamic t Bool
  -> Dynamic t (Map Text Text)
  -> Bool
  -> m (Event t event)
  -> m (Event t event)
  -> m (Event t event)
treeItemDynAttr' = treeItemDynAttrSet' never

-- | Like 'treeItemDynAttr'' but with an extra @setExpandedE@ event that forces
-- the node open (on 'True') or closed (on 'False') — e.g. to collapse every
-- project except the active one, and re-expand the newly-active one.  Manual
-- clicks and the reveal 'Dynamic' still work in between.  The @li@ also carries
-- a @tree-expanded@ / @tree-collapsed@ class reflecting the current state, so
-- CSS can show/hide a collapsed-only summary.
treeItemDynAttrSet'
  :: MonadWidget t m
  => Event t Bool
  -> Dynamic t Bool
  -> Dynamic t (Map Text Text)
  -> Bool
  -> m (Event t event)
  -> m (Event t event)
  -> m (Event t event)
treeItemDynAttrSet' setExpandedE revealD itemClass startExpanded item children = mdo
  pb <- getPostBuild
  let setOpenE = ffilter id $ leftmost [updated revealD, tag (current revealD) pb]
  expanded <- holdUniqDyn =<< holdDyn startExpanded (leftmost
      [ not <$> tag (current expanded) toggleExpanded
      , setExpandedE
      , setOpenE ])

  let itemClass' = (\ex m -> Map.insertWith (\new old -> old <> " " <> new) "class"
                              (bool "tree-collapsed" "tree-expanded" ex) m)
                     <$> expanded <*> itemClass
  (toggleExpanded, events) <- elDynAttr "li" itemClass' $ do
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

-- | 'treeItem' with a reveal 'Dynamic' (see 'treeItemDynAttr'').
treeItem'
  :: MonadWidget t m
  => Dynamic t Bool
  -> Text
  -> Bool
  -> m (Event t event)
  -> m (Event t event)
  -> m (Event t event)
treeItem' revealD itemClass = treeItemDynAttr' revealD (constDyn ("class" =: itemClass))

-- | Scroll @el@ into view (just enough — @block: nearest@) whenever the trigger
-- event fires, deferred to a 'requestAnimationFrame' so it runs after the
-- revealing tree expansion has been laid out.
scrollIntoViewNearest
  :: MonadWidget t m
  => Event t a
  -> Element EventResult (DomBuilderSpace m) t
  -> m ()
scrollIntoViewNearest e elm =
  performEvent_ $ ffor e $ \_ -> liftJSM $ do
    rawV <- toJSVal (_element_raw elm)
    o <- obj
    _ <- o ^. jss ("block" :: Text) ("nearest" :: Text)
    void $ jsg ("window" :: Text) ^. js1 ("requestAnimationFrame" :: Text)
        (fun $ \_ _ _ -> void $ rawV ^. js1 ("scrollIntoView" :: Text) o)

