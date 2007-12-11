module Ghf.Menu (
    makeMenu
,   actions
,   menuDescription
) where

import Graphics.UI.Gtk.Types

import Ghf.Core.State

makeMenu :: UIManager -> [ActionDescr GhfRef] -> String -> GhfM (AccelGroup, [Maybe Widget])
actions :: [ActionDescr GhfRef]
menuDescription :: String

