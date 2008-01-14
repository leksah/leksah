{-# OPTIONS_GHC -fglasgow-exts #-}
module IDE.ReplacePane (
    ReplaceView(..)
,   ReplaceAction(..)
,   IDEReplace
,   ReplaceState
) where

--import Graphics.UI.Gtk hiding (get)
import {-# SOURCE #-} IDE.Core.State
--import IDE.Core.Panes

class IDEPaneC alpha => ReplaceView alpha where
    getReplace      ::   IDEM alpha

class ReplaceAction alpha where
    doReplace       ::   alpha

data IDEReplace

data ReplaceState = ReplaceState{
    searchFor       ::   String
,   replaceWith     ::   String
,   matchCase       ::   Bool
,   matchEntire     ::   Bool
,   searchBackwards ::   Bool}
    deriving(Eq,Ord,Read,Show)
