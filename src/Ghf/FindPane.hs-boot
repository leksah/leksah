module Ghf.FindPane (
    getFind
,   getCaseSensitive
,   getWrapAround
,   getEntireWord
,   getGotoLineSpin
,   getFindEntry
) where

import Graphics.UI.Gtk hiding (get)
import Ghf.Core.State

getFind :: GhfM GhfFind
getWrapAround :: GhfM (ToggleButton)
getCaseSensitive :: GhfM (ToggleButton)
getEntireWord :: GhfM (ToggleButton)
getGotoLineSpin :: GhfM (SpinButton)
getFindEntry :: GhfM (Entry)
