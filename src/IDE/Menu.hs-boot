module IDE.Menu (
    makeMenu
,   actions
,   menuDescription
) where

import Graphics.UI.Gtk.Types

import IDE.Core.State

makeMenu :: UIManager -> [ActionDescr IDERef] -> String -> IDEM (AccelGroup, [Maybe Widget])
actions :: [ActionDescr IDERef]
menuDescription :: String

