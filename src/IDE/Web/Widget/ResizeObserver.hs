{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | An element-resize detector built on the browser's native @ResizeObserver@.
--
-- reflex-dom ships 'Reflex.Dom.Widget.Resize.resizeDetectorWithAttrs', which
-- predates @ResizeObserver@ and emulates it with a scroll-sensor hack (three
-- hidden @overflow:scroll@ helper divs, pre-scrolled to their maximum and
-- re-armed on every @scroll@ event through a read-offset / write-scroll cycle).
-- That cycle must complete synchronously inside the scroll handler to catch the
-- next resize.  Over an asynchronous JS bridge — e.g. jsaddle-wkwebview, where
-- DOM events reach Haskell handlers asynchronously and each DOM read/write is a
-- round trip — the cycle de-syncs: the sensors stop re-arming, the scroll
-- events stop tracking size changes, and the detector silently freezes at its
-- initial dimensions.  Anything sizing itself from that event (a @virtualList@
-- viewport, an xterm fit) then stops following its container.
--
-- @ResizeObserver@ is the native primitive that hack was written to polyfill
-- (WebKit has shipped it since 2020).  It is driven by the engine's own layout
-- pipeline and calls back once per change with the new size, so there is no
-- per-frame bridge chatter and no extra DOM.  This module exposes it as a plain
-- reflex 'Event', as a drop-in for @resizeDetectorWithAttrs@.
module IDE.Web.Widget.ResizeObserver
  ( resizeObserver
  , resizeObserverWithAttrs
  ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import Data.Text (Text)

import Language.Javascript.JSaddle
       (MonadJSM, liftJSM, new, jsg, js, js1, fun, valToNumber)
import GHCJS.Marshal.Pure (pToJSVal)
import qualified GHCJS.DOM.Types as DOM (Element)

import Reflex.Dom.Core
       (Event, DomBuilder, DomBuilderSpace, GhcjsDomSpace, PostBuild,
        PerformEvent, Performable, TriggerEvent, getPostBuild, performEvent_,
        newTriggerEvent, ffor, elAttr', _element_raw)

-- | Fire an element's content-box @(width, height)@, in CSS pixels, whenever it
-- changes.  @ResizeObserver@ delivers one callback on @observe@, so the 'Event'
-- also fires once with the initial size shortly after build.
--
-- The observer lives for the lifetime of the page (as reflex-dom's own
-- 'Reflex.Dom.Widget.Resize.resizeDetector' does with its sensor divs); it is
-- not disconnected when the widget is torn down.
resizeObserver
  :: (PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m))
  => DOM.Element                     -- ^ the element to observe
  -> m (Event t (Double, Double))
resizeObserver el = do
  (resizeE, fire) <- newTriggerEvent
  pb <- getPostBuild
  performEvent_ $ ffor pb $ \_ -> liftJSM $ do
    ro <- new (jsg ("ResizeObserver" :: Text))
              (fun $ \_ _ args -> case args of
                  (entries:_) -> do
                      -- entries[0].contentRect.{width,height}; the entry's rect
                      -- is precomputed, so reading it forces no extra reflow.
                      entry0 <- entries ^. js ("0" :: Text)
                      rect   <- entry0  ^. js ("contentRect" :: Text)
                      w <- valToNumber =<< rect ^. js ("width" :: Text)
                      h <- valToNumber =<< rect ^. js ("height" :: Text)
                      liftIO $ fire (w, h)
                  _ -> return ())
    -- pToJSVal (not toJSVal marshalling): the Element instance misbehaves under
    -- the GHC JS backend (same workaround as the xterm ResizeObserver call).
    void $ ro ^. js1 ("observe" :: Text) (pToJSVal el)
  return resizeE

-- | Wrap a widget in a @div@ (with the given attributes) and observe that div's
-- size.  A drop-in for @resizeDetectorWithAttrs@, differing only in that the
-- size components are always present (@Double@, not @Maybe Double@) — the
-- observer never reports a half-known size.
resizeObserverWithAttrs
  :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m,
      PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m))
  => Map Text Text                   -- ^ attributes for the wrapping @div@
  -> m a                             -- ^ the embedded widget
  -> m (Event t (Double, Double), a)
resizeObserverWithAttrs attrs w = do
  (el, a) <- elAttr' "div" attrs w
  resizeE <- resizeObserver (_element_raw el)
  return (resizeE, a)
