{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Widget.Menubar
  ( menubarCss
  , menubarWidget
  ) where

import Control.Lens ((^.))

import Data.Bool (bool)
import Data.Text (Text)
import Data.Traversable (forM)

import Clay
       (vGradient, backgroundImage, cursorDefault, nowrap, relative,
        whiteSpace, padding, hover, (#), fontSize, nil, inlineBlock,
        margin, px, pct, position, absolute, top, left, zIndex, block,
        borderRadius, boxShadow, bsColor, shadowWithSpread,
        (?), Css, background, Cursor(..))
import qualified Clay (display)

import Language.Javascript.JSaddle (liftJSM, js1)
import GHCJS.Marshal (fromJSValUnchecked)
import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Types (pToJSVal)
import qualified GHCJS.DOM.Event as Event (getTargetUnchecked)
import GHCJS.DOM.EventM (event, onSync)
import GHCJS.DOM.GlobalEventHandlers (mouseDown)

import Control.Monad.IO.Class (liftIO)
import Reflex
       (ffor, foldDyn, holdDyn, leftmost, never, newTriggerEvent,
        switchHold, Event)
import Reflex.Dom.Core
       (text, el, el', elAttr', dyn, wrapDomEventMaybe, _element_raw,
        MonadWidget, (=:), domEvent, EventName(..))

import IDE.Web.Theme
       (selectionColor, barTopColor, barBottomColor,
        menuTopColor, menuBottomColor, dropShadowColor)
import IDE.Web.Events (MenubarEvents(..))
import IDE.Web.Keybindings (registerKeymapListener)
import IDE.Web.MenuModel (MenuItem, renderedMenus)
import IDE.Web.Widget.Menu (menuItems)

menubarCss :: Css
menubarCss = do
    ".menubar" ? do
        whiteSpace nowrap
        cursor cursorDefault
        backgroundImage (vGradient barTopColor barBottomColor)
        -- The dropdown for a menu is rendered inside its <li>; anchor it there.
        ".menu" ? do
            position absolute
            top (pct 100)
            left nil
            zIndex 1000
            -- Flat colour, like the context menu and the flipper: Clay's
            -- 'vGradient' emits the legacy @linear-gradient(top, …)@ syntax,
            -- which modern WebKit/Blink reject outright — and a dropped
            -- background declaration on a popup means a SEE-THROUGH menu.
            background menuTopColor
            borderRadius (px 5) (px 5) (px 5) (px 5)
            boxShadow (pure $ bsColor dropShadowColor $ shadowWithSpread (px 0) (px 0) (px 10) (px 3))
        -- The menubar's own li are laid out horizontally; dropdown items must
        -- stack vertically (more specific so it wins over `.menubar ul li`).
        ".menu ul li" ? Clay.display block
    ".menubar ul" ? do
        Clay.display inlineBlock
        margin nil nil nil nil
    ".menubar ul li" ? do
        Clay.display inlineBlock
        position relative
        fontSize (px 13)
        padding (px 2) (px 8) (px 2) (px 8)
    ".menubar ul li" # hover ?
        background selectionColor

-- | Rebuilt whenever the keybindings table changes, so a rebind's new key
-- equivalent shows in the dropdown hints immediately.
menubarWidget
  :: MonadWidget t m
  => m (Event t MenubarEvents)
menubarWidget = do
  (kmE, fireKm) <- newTriggerEvent
  -- Fires immediately with the current table, then on every reload.
  liftIO (registerKeymapListener fireKm)
  kmD <- holdDyn [] kmE
  switchHold never =<< dyn (menubarFor . renderedMenus <$> kmD)

menubarFor
  :: MonadWidget t m
  => [(Text, [MenuItem])] -> m (Event t MenubarEvents)
menubarFor menus = mdo
  (bar, (clicksE, hoversE, cmdE)) <-
    elAttr' "div" ("class" =: "menubar") $
      el "ul" $ do
        results <- forM (zip [0 :: Int ..] menus) $ \(idx, (label, items)) -> do
          (li, mcmd) <- el' "li" $ do
            text label
            -- Show this menu's dropdown only while it is the open one.
            let isOpenD = (== Just idx) <$> openIndexD
            switchHold never =<< dyn (ffor isOpenD $ \case
              False -> return never
              True  -> menuItems items)
          return (idx <$ domEvent Click li, idx <$ domEvent Mouseenter li, mcmd)
        return ( leftmost (map (\(c, _, _) -> c) results)
               , leftmost (map (\(_, h, _) -> h) results)
               , leftmost (map (\(_, _, m) -> m) results) )

  -- A mousedown anywhere outside the menubar closes the open menu.
  doc <- currentDocumentUnchecked
  outsideE <- wrapDomEventMaybe doc (`onSync` mouseDown) $ do
    t <- event >>= Event.getTargetUnchecked
    fmap (bool (Just ()) Nothing) . liftJSM $
      pToJSVal (_element_raw bar) ^. js1 ("contains" :: Text) (pToJSVal t) >>= fromJSValUnchecked

  -- One menu open at a time:
  --  * click a label  -> toggle it (closing any other)
  --  * hover a label   -> switch to it, but only if a menu is already open
  --  * choose an item / click outside -> close
  openIndexD <- foldDyn ($) Nothing $ leftmost
    [ (\i mo -> if mo == Just i then Nothing else Just i) <$> clicksE
    , (\i mo -> maybe Nothing (const (Just i)) mo)        <$> hoversE
    , const Nothing <$ cmdE
    , const Nothing <$ outsideE
    ]

  return $ MenubarCommand <$> cmdE
