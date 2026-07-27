{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Widget.ContextMenu where

import Control.Lens ((^.))

import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Text as T (pack)

import Language.Javascript.JSaddle (liftJSM, js1)

import GHCJS.Marshal (FromJSVal(..))
import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Types (HTMLElement(..), uncheckedCastTo, pToJSVal)
import qualified GHCJS.DOM.Types as DOM (Element(..))
import qualified GHCJS.DOM.Event as Event (getTargetUnchecked)
import GHCJS.DOM.EventM
       (event, uiPageY, uiPageX, onSync, preventDefault)
import qualified GHCJS.DOM.GlobalEventHandlers as DOM (contextMenu)
import GHCJS.DOM.GlobalEventHandlers (touchStart, mouseDown)

import Clay
       (shadowWithSpread, bsColor, boxShadow,
        absolute, padding, px, borderRadius, background,
        position, (?), Css, Color(..), nil, zIndex)

import IDE.Web.Theme (menuTopColor, dropShadowColor)

import Reflex
       (leftmost, ffor, Event, never, holdDyn, switchHold,
        fmapMaybe)
import Reflex.Dom.Core
       (wrapDomEventMaybe, elAttr', wrapDomEvent, EventResult,
        Element, MonadWidget, DomBuilderSpace, dyn, (=:), _element_raw)

contextMenuCss :: Css
contextMenuCss =
  ".context-menu" ? do
    position absolute
    -- Solid opaque background: Clay's 'vGradient' emits the legacy unprefixed
    -- @linear-gradient(top, …)@ syntax, which modern WebKit rejects (→ no
    -- background → a see-through menu), so use a flat colour like the flipper.
    background menuTopColor
    borderRadius (px 5) (px 5) (px 5) (px 5)
    padding nil nil nil nil
    boxShadow (pure $ bsColor dropShadowColor $ shadowWithSpread (px 0) (px 0) (px 10) (px 3))
    zIndex 1000

contextMenu
  :: MonadWidget t m
  => Element EventResult (DomBuilderSpace m) t
  -> m (Event t a)
  -> m (Event t (), Event t a)
contextMenu parent menu = mdo
  contextmenuE <- wrapDomEvent (uncheckedCastTo HTMLElement $ _element_raw parent)
    (`onSync` DOM.contextMenu) $ do
      preventDefault
      x <- uiPageX
      y <- uiPageY
      return (x, y)
  showContextMenuD <- holdDyn Nothing $ leftmost
    [ Just <$> contextmenuE
    , Nothing <$ events
    ]
  events <- (switchHold never =<<) . dyn $ ffor showContextMenuD $ \case
    Nothing -> return never
    Just (x, y) -> do
      (menuElement, menuEvent) <- elAttr' "div" ("class" =: "context-menu"
          <> "style" =: T.pack ("top:" <> show y <> "px; left:" <> show x <> "px;")) menu
      doc <- currentDocumentUnchecked
      docMouseDownE <- wrapDomEventMaybe doc (`onSync` mouseDown) $ do
        t <- event >>= Event.getTargetUnchecked
        fmap (bool (Just ()) Nothing) . liftJSM $ pToJSVal (_element_raw menuElement) ^. js1 ("contains" :: Text) (pToJSVal t) >>= fromJSValUnchecked
      docTouchStartE <- wrapDomEventMaybe doc (`onSync` touchStart) $ do
        t <- event >>= Event.getTargetUnchecked
        fmap (bool (Just ()) Nothing) . liftJSM $ pToJSVal (_element_raw menuElement) ^. js1 ("contains" :: Text) (pToJSVal t) >>= fromJSValUnchecked
--      docScrollE <- wrapDomEvent doc (`onSync` touchStart) $ return ()
      return $ leftmost [ Nothing <$ docMouseDownE, Nothing <$ docTouchStartE, Just <$> menuEvent ]
  return (() <$ contextmenuE, fmapMaybe id events)
