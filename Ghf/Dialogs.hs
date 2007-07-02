module Ghf.Dialogs (
    replaceDialog
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Multiline.TextView
import Graphics.UI.Gtk.Glade
import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Directory
import Data.Maybe ( fromMaybe, isJust)
import Text.Printf
  
import Ghf.Core
import Ghf.View
import Ghf.Editor

replaceDialog :: GhfAction
replaceDialog = do
    ghfR <- ask    
    lift $ do
        dialogXmlM <- xmlNew "dialogs/ghf-replace-dialog.glade"
        let dialogXml = case dialogXmlM of
                            (Just dialogXml) -> dialogXml	       
                            Nothing -> error "can't find the glade file \"ghf-replace-dialog.glade\""
        window <- xmlGetWidget dialogXml castToWindow "dialog"
        closeButton <- xmlGetWidget dialogXml castToButton "close_button"
        replaceAllButton <- xmlGetWidget dialogXml castToButton "replace_all_button"
        replaceButton <- xmlGetWidget dialogXml castToButton "replace_button"
        findButton <- xmlGetWidget dialogXml castToButton "find_button"
        matchCaseCheckbutton <- xmlGetWidget dialogXml castToCheckButton "match_case_checkbutton"      
        entireWordCheckbutton <- xmlGetWidget dialogXml castToCheckButton "entire_word_checkbutton"      
        searchBackwardsCheckbutton <- xmlGetWidget dialogXml castToCheckButton "search_backwards_checkbutton"      
        wrapAroundCheckbutton <- xmlGetWidget dialogXml castToCheckButton "wrap_around_checkbutton"      
        searchForEntry <- xmlGetWidget dialogXml castToEntry "search_for_entry"      
        replaceWithEntry <- xmlGetWidget dialogXml castToEntry "replace_with_entry"      
    
        let findOrSearch = \f -> do
            wrapAround <- toggleButtonGetActive wrapAroundCheckbutton 
            entireWord <- toggleButtonGetActive entireWordCheckbutton
            matchCase  <- toggleButtonGetActive matchCaseCheckbutton
            backwards  <- toggleButtonGetActive searchBackwardsCheckbutton
            let hint = if backwards then Backward else Forward
            searchString <- entryGetText searchForEntry
            replaceString <- entryGetText replaceWithEntry    
            found <- runReaderT (f entireWord matchCase wrapAround searchString replaceString hint) ghfR
            widgetSetSensitivity replaceButton found
            widgetSetSensitivity replaceAllButton found
            return ()

        findButton `onClicked` do 
            putStrLn "find"
            findOrSearch editFind
        replaceButton `onClicked` do
            putStrLn "replace"            
            findOrSearch editReplace
        replaceAllButton `onClicked` do
            putStrLn "replaceAll"
            findOrSearch editReplaceAll
        closeButton `onClicked` (widgetDestroy window) 

        widgetShowAll window
    



