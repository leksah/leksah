{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Widget.Menu where

import Data.Text (Text)
import Data.Traversable (forM)

import Clay
       (cursorDefault, padding, px, fontSize, hover, (#), (?), (-:), Css,
        Background(..), Color(..), margin, nil, borderRadius, Cursor(..),
        position, absolute, relative, top, left, pct, nowrap, whiteSpace,
        zIndex, block, backgroundImage, vGradient, boxShadow, bsColor,
        shadowWithSpread, (|>), color, float, floatRight, marginLeft,
        em, minWidth)
import qualified Clay (display, none)

import Reflex (leftmost, never, Event, Dynamic, tag, current, attachWith)

import Reflex.Dom.Core
       (dynText, el', el, elClass, divClass, text, MonadWidget,
        HasDomEvent(..), EventName(..))

import IDE.Web.Theme
       (selectionColor, dimColor, menuTopColor, menuBottomColor, dropShadowColor)
import IDE.Web.Command (Command)
import IDE.Web.MenuModel (MenuItem(..), prettyKeySpec)
import IDE.Web.Widget.Tree (clickMods)
import IDE.Web.SplitOpenRequest (SplitTarget, requestSplitOpen)

menuCss :: Css
menuCss = do
  ".menu" ?
    cursor cursorDefault
  ".menu ul" ?
    margin nil nil nil nil
  ".menu ul li" ? do
    fontSize (px 13)
    padding (px 4) (px 8) (px 4) (px 8)
    whiteSpace nowrap
  ".menu ul li" # hover ? do
    background selectionColor
    borderRadius (px  5) (px 5) (px 5) (px 5)
  -- The shortcut hint sits to the right of the label, greyed — like a native
  -- menu's key-equivalent column.  The min-width gives the rows a common width
  -- so the floated shortcuts line up rather than hugging each label.
  ".menu ul" ? minWidth (em 13)
  ".menu .menu-shortcut" ? do
    float floatRight
    marginLeft (em 2)
    color dimColor
  -- Separator rows: a thin line, not hoverable/clickable.
  ".menu li.menu-sep" ? do
    padding (px 0) (px 0) (px 0) (px 0)
    margin (px 4) (px 8) (px 4) (px 8)
    "border-top" -: "1px solid var(--leksah-border-faint)"
    "pointer-events" -: "none"
  -- A submenu item anchors its flyout, which is a nested `.menu` shown to the
  -- right on hover.  These selectors are more specific than `.menubar .menu`
  -- (which anchors the top-level dropdown under the bar), so they win and
  -- re-anchor nested flyouts to the side.
  ".menu li.has-submenu" ? position relative
  ".menu li.has-submenu > .menu" ? do
    position absolute
    left (pct 100)
    top nil
    Clay.display Clay.none
    -- Flat, for the reason spelled out in 'IDE.Web.Widget.Menubar': the Clay
    -- gradient helper emits syntax browsers reject, leaving the flyout
    -- see-through.
    background menuTopColor
    borderRadius (px 5) (px 5) (px 5) (px 5)
    boxShadow (pure $ bsColor dropShadowColor $ shadowWithSpread (px 0) (px 0) (px 10) (px 3))
    zIndex 1001
  -- Only the directly-hovered item's flyout opens (child combinator), so
  -- hovering an outer item doesn't reveal its grandchildren.
  (".menu li.has-submenu" # hover) |> ".menu" ? Clay.display block

menu
  :: MonadWidget t m
  => [Dynamic t (Text, a)]
  -> m (Event t a)
menu items =
  divClass "menu" $
    el "ul" $
      fmap leftmost . forM items $ \d -> do
        (li, _) <- el' "li" . dynText $ fst <$> d
        return $ tag (snd <$> current d) $ domEvent Click li

-- | Like 'menu', but each item's click also carries the (alt, shift) modifier
-- state — so a menu can offer ⌥-variants of its items (see 'menuSplit').
menuMods
  :: MonadWidget t m
  => [Dynamic t (Text, a)]
  -> m (Event t ((Bool, Bool), a))
menuMods items =
  divClass "menu" $
    el "ul" $
      fmap leftmost . forM items $ \d -> do
        (li, _) <- el' "li" . dynText $ fst <$> d
        modsE <- clickMods li
        return $ attachWith (\v mods -> (mods, v)) (snd <$> current d) modsE

-- | A context-menu whose items may declare a 'SplitTarget' (@Just@): holding ⌥
-- while clicking such an item opens it into a split of the active pane (⌥⇧ =
-- vertical) instead of running its normal action, mirroring the ⌥-open tree
-- gestures.  @Nothing@ items always run their action.  Returns the chosen 'IO'
-- action, so callers use it exactly like 'menu'.
menuSplit
  :: MonadWidget t m
  => [Dynamic t (Text, (Maybe SplitTarget, IO ()))]
  -> m (Event t (IO ()))
menuSplit = menuSplitWith (curry requestSplitOpen)

-- | 'menuSplit' for menus whose items yield a VALUE (e.g. a workspace-tree
-- command) rather than an 'IO' action: ⌥-clicking an item with a
-- 'SplitTarget' yields @wrap target shift@ — the caller embeds the
-- 'requestSplitOpen' call in its own item type.
menuSplitWith
  :: MonadWidget t m
  => (SplitTarget -> Bool -> a)
  -> [Dynamic t (Text, (Maybe SplitTarget, a))]
  -> m (Event t a)
menuSplitWith wrap = fmap (fmap decide) . menuMods
  where decide ((alt, sh), (mt, normal)) = case (alt, mt) of
          (True, Just t) -> wrap t sh
          _              -> normal

-- | Render a (possibly nested) list of 'MenuItem's as a dropdown, returning the
-- 'Command' chosen anywhere in the tree.  Leaf items fire on click; submenu
-- items open a flyout on hover (pure CSS, see 'menuCss') whose own items bubble
-- up here.  Used by the web menubar; the native macOS menu builds the same model
-- with NSMenu submenus.
menuItems
  :: MonadWidget t m
  => [MenuItem]
  -> m (Event t Command)
menuItems items =
  divClass "menu" $
    el "ul" $
      fmap leftmost . forM items $ \case
        MenuItem label cmd -> do
          (li, _) <- el' "li" $ text label
          return $ cmd <$ domEvent Click li
        MenuShortcut label shortcut cmd -> do
          (li, _) <- el' "li" $ do
            text label
            elClass "span" "menu-shortcut" $ text shortcut
          return $ cmd <$ domEvent Click li
        -- The web menubar can't intercept real key equivalents, so a MenuKey
        -- renders like a shortcut hint (the native macOS menu makes it real).
        MenuKey label spec cmd -> do
          (li, _) <- el' "li" $ do
            text label
            elClass "span" "menu-shortcut" $ text (prettyKeySpec spec)
          return $ cmd <$ domEvent Click li
        MenuGlobalKey label spec cmd -> do
          (li, _) <- el' "li" $ do
            text label
            elClass "span" "menu-shortcut" $ text (prettyKeySpec spec)
          return $ cmd <$ domEvent Click li
        MenuSplitKey label spec cmd -> do
          (li, _) <- el' "li" $ do
            text label
            elClass "span" "menu-shortcut" $ text (prettyKeySpec spec)
          return $ cmd <$ domEvent Click li
        MenuSep -> do
          _ <- elClass "li" "menu-sep" $ text ""
          return never
        Submenu label subs ->
          -- The flyout (a nested `.menu`) lives inside this <li>; its chosen
          -- command is the <li>'s result and bubbles up via leftmost.
          elClass "li" "has-submenu" $ do
            text (label <> " ▸")
            menuItems subs
