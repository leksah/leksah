{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module IDE.Web.Widget.Menu where

import Data.Text (Text)
import Data.Traversable (forM)

import Clay
       (cursorDefault, padding, px, fontSize, hover, (#), (?), Css,
        Background(..), Color(..), margin, nil, borderRadius, Cursor(..))

import Reflex (leftmost, Event, Dynamic, tag, current)

import Reflex.Dom.Core
       (dynText, el', el, divClass, MonadWidget, HasDomEvent(..),
        EventName(..))

menuCss :: Css
menuCss = do
  ".menu" ?
    cursor cursorDefault
  ".menu ul" ?
    margin nil nil nil nil
  ".menu ul li" ? do
    fontSize (px 13)
    padding (px 4) (px 8) (px 4) (px 8)
  ".menu ul li" # hover ? do
    background (Rgba 30 88 209 1.0)
    borderRadius (px  5) (px 5) (px 5) (px 5)

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
