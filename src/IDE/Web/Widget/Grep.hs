{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Grep
  ( grepCss
  , grepWidget
  ) where

import Clay
       (vGradient, backgroundImage, (?), Css, Color(..))

import Reflex.Class (Reflex(..))
import Reflex.Dom.Core (MonadWidget, divClass, Event)

import IDE.Web.Events (GrepEvents)

grepCss :: Css
grepCss =
    ".grep" ?
        backgroundImage (vGradient (Rgba 32 32 32 1.0) (Rgba 16 16 16 1.0))

grepWidget
  :: MonadWidget t m
  => m (Event t GrepEvents)
grepWidget =
  divClass "grep" $
    return never
