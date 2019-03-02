{-# LANGUAGE OverloadedStrings #-}
module IDE.Web.Layout where

import qualified Data.Text as T (unwords)

import Clay
       (vh, height, (-:), grid, (?), Css)
import qualified Clay (display)

layoutCss :: Css
layoutCss = do
    ".leksah" ? do
        Clay.display grid
        "grid-template-rows" -: "20px 28px 1fr 150px 20px 20px"
        "grid-template-columns" -: "300px 1fr"
        "grid-template-areas" -: T.unwords
          [ "\"menubar   menubar\""
          , "\"toolbar   toolbar\""
          , "\"tall      wide0\""
          , "\"tall      wide1\""
          , "\"bar       bar\""
          , "\"statusbar statusbar\""
          ]
        height (vh 100)
    ".menubar" ? do
        "grid-area" -: "menubar"
    ".toolbar" ? do
        "grid-area" -: "toolbar"
    ".findbar" ? do
        "grid-area" -: "bar"
    ".statusbar" ? do
        "grid-area" -: "statusbar"
    ".area-tall" ? do
        "grid-area" -: "tall"
    ".area-wide0" ? do
        "grid-area" -: "wide0"
    ".area-wide1" ? do
        "grid-area" -: "wide1"

