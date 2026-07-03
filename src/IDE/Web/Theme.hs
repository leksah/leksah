{-# LANGUAGE OverloadedStrings #-}
-- | User-configurable UI colours.
--
-- The Clay stylesheets reference CSS variables ('selectionColor',
-- 'hoverColor'); 'IDE.Web.Main' binds the variables to the preference values
-- in a live @\<style\>@ element ('themeVarsCss'), so a change in the
-- Preferences pane's colour pickers applies immediately, without rebuilding
-- the (startup-injected) stylesheet.
module IDE.Web.Theme
  ( selectionColor
  , selectionColorFaint
  , hoverColor
  , themeVarsCss
  ) where

import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T (all, length, uncons)

import Clay (Color(Other))

-- | The selection/active highlight colour (tabs, tree selections, menus…).
selectionColor :: Color
selectionColor = Other "var(--leksah-selection)"

-- | The selection colour at 45% — the toolbar's held-down shade.
selectionColorFaint :: Color
selectionColorFaint = Other "color-mix(in srgb, var(--leksah-selection) 45%, transparent)"

-- | The row highlight shown while hovering a run/action button.
hoverColor :: Color
hoverColor = Other "var(--leksah-hover)"

-- | The variable bindings for the current preference values.  Only a
-- @#rgb@/@#rrggbb@/@#rrggbbaa@ value is interpolated into the stylesheet
-- (the values are user input); anything else falls back to the default.
themeVarsCss :: Text -> Text -> Text
themeVarsCss selection hover =
    ":root { --leksah-selection: " <> checked selection "#1e58d1"
    <> "; --leksah-hover: " <> checked hover "#0c1e46" <> "; }"
  where
    checked v dflt = if isHexColor v then v else dflt
    isHexColor v = case T.uncons v of
        Just ('#', rest) -> T.length rest `elem` [3, 6, 8] && T.all isHexDigit rest
        _                -> False
