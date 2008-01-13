{-# OPTIONS_GHC -fglasgow-exts #-}
module IDE.ReplacePane (
    getReplace
,   doReplace
) where

--import Graphics.UI.Gtk hiding (get)
import IDE.Core.State

instance Pane IDEReplace
instance ModelPane IDEReplace ReplaceState
doReplace :: IDEAction
getReplace :: IDEM IDEReplace
