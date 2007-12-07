module Ghf.Menu (
    makeMenu
,   actions
,   menuDescription
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Types
import qualified Data.Map as Map
import Data.Map (Map,(!))
import Control.Monad.Reader
import System.FilePath
import Data.Version

import Ghf.Core.State

makeMenu :: UIManager -> [ActionDescr GhfRef] -> String -> GhfM (AccelGroup, [Maybe Widget])
actions :: [ActionDescr GhfRef]
menuDescription :: String
