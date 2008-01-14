module IDE.FindPane (
    IDEFind
,   FindState
,   FindView(..)
) where

import Graphics.UI.Gtk
import {-# SOURCE #-} IDE.Core.State
import {-# SOURCE #-} IDE.Core.Panes

class IDEPaneC alpha => FindView alpha where
    getFind             ::   IDEM alpha
    getFindEntry        ::   alpha -> Entry
    getCaseSensitive    ::   alpha -> ToggleButton
    getWrapAround       ::   alpha -> ToggleButton
    getEntireWord       ::   alpha -> ToggleButton
    getGotoLineSpin     ::   alpha -> SpinButton
data IDEFind
data FindState  =   FindState
    deriving(Eq,Ord,Read,Show)
instance Pane IDEFind
instance FindView IDEFind

