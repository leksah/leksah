module IDE.FindPane (
    getFind
,   getCaseSensitive
,   getWrapAround
,   getEntireWord
,   getGotoLineSpin
,   getFindEntry
) where

import Graphics.UI.Gtk hiding (get)
import IDE.Core.State

instance Pane IDEFind
getFind :: IDEM IDEFind
getWrapAround :: IDEM (ToggleButton)
getCaseSensitive :: IDEM (ToggleButton)
getEntireWord :: IDEM (ToggleButton)
getGotoLineSpin :: IDEM (SpinButton)
getFindEntry :: IDEM (Entry)
