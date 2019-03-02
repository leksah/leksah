{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Menubar
  ( menubarCss
  , menubarWidget
  ) where

import Clay
       (vGradient, backgroundImage, cursorDefault, nowrap,
        whiteSpace, padding, hover, (#), fontSize, nil, inlineBlock,
        margin, px, (?), Css, background, Color(..), Cursor(..))
import qualified Clay (display)

import Reflex (never)
import Reflex.Dom.Core
       (text, el, MonadWidget, divClass, Event)

import IDE.Web.Events (MenubarEvents)

menubarCss :: Css
menubarCss = do
    ".menubar" ? do
        whiteSpace nowrap
        cursor cursorDefault
        backgroundImage (vGradient (Rgba 32 32 32 1.0) (Rgba 16 16 16 1.0))
    ".menubar ul" ? do
        Clay.display inlineBlock
        margin nil nil nil nil
    ".menubar ul li" ? do
        Clay.display inlineBlock
        fontSize (px 13)
        padding (px 2) (px 8) (px 2) (px 8)
    ".menubar ul li" # hover ?
        background (Rgba 30 88 209 1.0)

menubarWidget
  :: MonadWidget t m
  => m (Event t MenubarEvents)
menubarWidget = do
  divClass "menubar" $
    el "ul" $ do
      el "li" $
        text "File"
      el "li" $
        text "Edit"
      el "li" $
        text "Workspace"
      el "li" $
        text "Package"
      el "li" $
        text "Debug"
      el "li" $
        text "View"
      el "li" $
        text "Tools"
      el "li" $
        text "Version Control"
      el "li" $
        text "Help"
  return never
