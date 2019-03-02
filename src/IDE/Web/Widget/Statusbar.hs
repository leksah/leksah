{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Statusbar
  ( statusbarCss
  , statusbarWidget
  ) where

import Clay
       (vGradient, backgroundImage, (?), Css, Color(..))

import Reflex (never)
import Reflex.Dom.Core (MonadWidget, divClass, Event)

import IDE.Web.Events (StatusbarEvents)

statusbarCss :: Css
statusbarCss =
    ".statusbar" ?
        backgroundImage (vGradient (Rgba 32 32 32 1.0) (Rgba 16 16 16 1.0))

statusbarWidget
  :: MonadWidget t m
  => m (Event t StatusbarEvents)
statusbarWidget =
  divClass "statusbar" $
    return never
