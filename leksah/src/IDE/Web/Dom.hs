{-# LANGUAGE OverloadedStrings #-}
-- | Typed (ghcjs-dom) versions of the small DOM operations the jsaddle-heavy
-- widgets kept spelling out as @jsg "document" ^. js "activeElement"@ chains.
--
-- Only plain DOM goes here.  Calls into the JS libraries (xterm.js,
-- CodeMirror, Monaco) and leksah's own @window.Leksah*@ globals stay on raw
-- jsaddle where they are: there are no typed bindings for them.
module IDE.Web.Dom
  ( nextFrame
  , observeResize
  , holdsFocus
  , activeTagName
  , focusElement
  , focusFirst
  , toggleClassWhere
  ) where

import Control.Monad (forM_, void, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import GHCJS.DOM (currentDocumentUnchecked, currentWindowUnchecked)
import GHCJS.DOM.AnimationFrameProvider (requestAnimationFrame)
import GHCJS.DOM.DocumentOrShadowRoot (getActiveElement)
import GHCJS.DOM.DOMTokenList (toggle)
import GHCJS.DOM.Element (getAttribute, getClassList, getTagName)
import GHCJS.DOM.FrameRequestCallback (newFrameRequestCallback)
import GHCJS.DOM.HTMLOrSVGOrMathMLElement (focus)
import GHCJS.DOM.Node (contains)
import qualified GHCJS.DOM.NodeList as NodeList
import GHCJS.DOM.ParentNode (querySelector, querySelectorAll)
import GHCJS.DOM.ResizeObserver (newResizeObserver, observe)
import GHCJS.DOM.ResizeObserverCallback (newResizeObserverCallback)
import GHCJS.DOM.Types
       (Element(..), HTMLElement(..), IsElement, IsNode, IsParentNode, JSM, Node(..),
        toElement, uncheckedCastTo)

-- | Run an action on the next animation frame (@requestAnimationFrame@).
nextFrame :: JSM () -> JSM ()
nextFrame k = do
    win <- currentWindowUnchecked
    cb <- newFrameRequestCallback (const k)
    void $ requestAnimationFrame win cb

-- | Run an action whenever the element's size changes (a 'ResizeObserver';
-- like every observer, it also fires once when observation starts).
observeResize :: IsElement e => e -> JSM () -> JSM ()
observeResize e k = do
    ro <- newResizeObserver =<< newResizeObserverCallback (\_ _ -> k)
    observe ro e Nothing

-- | Does @n@ (or one of its descendants) hold the keyboard focus?
holdsFocus :: IsNode n => n -> JSM Bool
holdsFocus n = do
    doc <- currentDocumentUnchecked
    contains n =<< getActiveElement doc

-- | The tag name (upper case for HTML, e.g. @\"BODY\"@) of the focused
-- element, or @\"\"@ when nothing is focused.
activeTagName :: JSM Text
activeTagName = do
    doc <- currentDocumentUnchecked
    getActiveElement doc >>= maybe (pure "") getTagName

-- | Focus an element.  @focus()@ lives on HTMLElement (and SVG/MathML
-- elements), so an 'Element' is cast; every element leksah focuses is HTML.
focusElement :: IsElement e => e -> JSM ()
focusElement = focus . uncheckedCastTo HTMLElement . toElement

-- | Focus the first descendant of @p@ matching a CSS selector, if any.
focusFirst :: IsParentNode p => p -> Text -> JSM ()
focusFirst p sel = querySelector p sel >>= mapM_ focusElement

-- | For each descendant of @p@ matching a CSS selector, add the class when the
-- predicate holds for its @attr@ attribute (absent reads as @\"\"@) and remove
-- it otherwise.
toggleClassWhere :: IsParentNode p => p -> Text -> Text -> Text -> (Text -> Bool) -> JSM ()
toggleClassWhere p sel attr cls want = do
    nodes <- querySelectorAll p sel
    len <- NodeList.getLength nodes
    when (len > 0) $ forM_ [0 .. len - 1] $ \i -> do
        e <- uncheckedCastTo Element <$> NodeList.itemUnchecked nodes i
        v <- fromMaybe "" <$> getAttribute e attr
        cl <- getClassList e
        void $ toggle cl cls (Just (want v))
