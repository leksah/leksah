{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Findbar
  ( findbarCss
  , findbarWidget
  ) where

import Control.Lens ((.~))
import Data.Bool (bool)
import Data.Default (Default(..))
import Data.Function ((&))
import Data.Text (Text)

import Clay
       (flexGrow, flex, display, middle, fontSize, white,
        color, borderStyle, textDecoration, vGradient, backgroundImage,
        borderRadius, padding, hover, (#), background, margin, px, width,
        height, (?), Css, Color(..), None(..), VerticalAlign(..))

import Reflex (constDyn, Dynamic, never)
import Reflex.Dom.Core
       (elDynAttr, textInput, text, MonadWidget, (=:),
        divClass, Event, attributes)

import IDE.Web.Events (FindbarEvents)

findbarCss :: Css
findbarCss = do
    ".findbar" ? do
        display flex
        backgroundImage (vGradient (Rgba 32 32 32 1.0) (Rgba 16 16 16 1.0))
    ".findbar button" ? do
        verticalAlign middle
        borderRadius (px 3) (px 3) (px 3) (px 3)
        padding (px 2) (px 10) (px 2) (px 10)
        margin (px 0) (px 0) (px 0) (px 0)
        textDecoration none
        borderStyle none
        fontSize (px 13)
        background (Rgba 0 0 0 0.0)
        color white
    ".findbar button" # hover ?
        background (Rgba 61 96 150 1.0)
    ".findbar button.selected" ?
        background (Rgba 30 88 209 1.0)
    ".findbar input" ? do
        verticalAlign middle
        backgroundImage (vGradient (Rgba 40 40 40 1.0) (Rgba 24 24 24 1.0))
        color white
        margin (px 0) (px 10) (px 0) (px 10)
        padding (px 2) (px 2) (px 2) (px 2)
        borderStyle none
        fontSize (px 13)
    ".findbar .find-text" ?
        flexGrow 100
    ".findbar-button" ? do
        verticalAlign middle
        height (px 16)
        width (px 16)
        borderRadius (px 3) (px 3) (px 3) (px 3)
        padding (px 2) (px 10) (px 2) (px 10)
        margin (px 0) (px 0) (px 0) (px 0)
    ".findbar-button" # hover ?
        background (Rgba 61 96 150 1.0)
    ".findbar-button.selected" ?
        background (Rgba 30 88 209 1.0)

findbarButton
  :: MonadWidget t m
  => Dynamic t Bool
  -> Text
  -> m ()
findbarButton selected src =
    elDynAttr "img" (
        ("src" =: src <>)
      . ("class" =:)
      . ("findbar-button" <>)
      . bool "" " selected" <$> selected) $ return ()

findbarWidget
  :: MonadWidget t m
  => m (Event t FindbarEvents)
findbarWidget =
  divClass "findbar" $ do
    elDynAttr "button" (constDyn mempty) $ text "Case"
    elDynAttr "button" (constDyn ("class" =: "selected")) $ text "Words"
    elDynAttr "button" (constDyn mempty) $ text "Regex"
    _ <- textInput (def &
        attributes .~ constDyn ("class" =: "find-text" <> "placeholder" =: "Find"))
    findbarButton (constDyn False) "/pics/tango/actions/go-previous.svg"
    findbarButton (constDyn True) "/pics/tango/actions/view-refresh.svg"
    findbarButton (constDyn False) "/pics/tango/actions/go-next.svg"
    _ <- textInput (def &
        attributes .~ constDyn ("class" =: "replace-text" <> "placeholder" =: "Replace with"))
    findbarButton (constDyn False) "/pics/tango/actions/edit-find-replace.svg"
    elDynAttr "button" (constDyn mempty) $ text "Replace All"
    elDynAttr "button" (constDyn mempty) $ text "Grep"
    return never
