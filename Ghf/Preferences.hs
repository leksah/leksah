--
-- | Module for saving, restoring and editing preferences
-- 

module Ghf.Preferences (
    readPrefs
,   writePrefs
--,   applyPrefs
,   editPrefs

,   prefsDescription
) where

import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import qualified Text.PrettyPrint.HughesPJ as PP
import Control.Monad(foldM)
import Graphics.UI.Gtk hiding (afterToggleOverwrite,Focus)
import Graphics.UI.Gtk.SourceView
import Control.Monad.Reader
import Data.Maybe(isJust)
import qualified Data.Map as Map
import Data.Map(Map,(!))
import Data.IORef
import Data.List(unzip4)

import Debug.Trace

import Ghf.Core
import Ghf.Editor
import Ghf.View
import Ghf.Keymap
import Ghf.Menu(actions,makeMenu,menuDescription)
import Ghf.PreferencesBase


defaultPrefs = Prefs {
        showLineNumbers     =   True
    ,   rightMargin         =   Just 100
    ,   tabWidth            =   4
    ,   sourceCandy         =  Just("Default")
    ,   keymapName          =  "Default" 
    ,   defaultSize         =  (1024,800)}


prefsDescription :: [FieldDescription Prefs]
prefsDescription = [
        field "Show line numbers"
            "(True/False)" 
            (PP.text . show)
            boolParser
            showLineNumbers
            (\ b a -> a{showLineNumbers = b})
            boolEditor
            (\b -> do
                buffers <- allBuffers
                mapM_ (\buf -> lift$sourceViewSetShowLineNumbers (sourceView buf) b) buffers)
    ,   field "Right margin"
            "Size or 0 for no right margin"
            (\a -> (PP.text . show) (case a of Nothing -> 0; Just i -> i))
            (do i <- intParser
                return (if i == 0 then Nothing else Just i))
            rightMargin
            (\b a -> a{rightMargin = b})
            (maybeEditor (intEditor 0.0 200.0 5.0))
            (\b -> do
                buffers <- allBuffers
                mapM_ (\buf -> case b of
                                Just n -> do
                                    lift $sourceViewSetMargin (sourceView buf) n
                                    lift $sourceViewSetShowMargin (sourceView buf) True
                                Nothing -> lift $sourceViewSetShowMargin (sourceView buf) False)
                                                buffers)
    ,   field "Tab width" ""
            (PP.text . show)
            intParser
            tabWidth
            (\b a -> a{tabWidth = b})
            (intEditor 0.0 20.0 1.0)
            (\i -> do
                buffers <- allBuffers
                mapM_ (\buf -> lift $sourceViewSetTabsWidth (sourceView buf) i) buffers)
    ,   field "Source candy"
                "Empty for do not use or the name of a candy file in a config dir)"
            (\a -> PP.text (case a of Nothing -> ""; Just s -> s)) 
            (do id <- identifier
                return (if null id then Nothing else Just (id)))
            sourceCandy (\b a -> a{sourceCandy = b})
            (maybeEditor stringEditor)
            (\cs -> case cs of
                        Nothing -> do 
                            setCandyState False
                            editCandy
                        Just name -> do
                            setCandyState True
                            editCandy)
    ,   field "Name of the keymap"  "The name of a keymap file in a config dir"
            PP.text
            identifier
            keymapName
            (\b a -> a{keymapName = b})
            stringEditor
            (\ a -> return ())
    ,   field "Window default size"
            "Default size of the main ghf window specified as pair (int,int)" 
            (PP.text.show) 
            (pairParser intParser)
            defaultSize (\(c,d) a -> a{defaultSize = (c,d)})
            (pairEditor (intEditor 0.0 3000.0 25.0)(intEditor 0.0 3000.0 25.0))
            (\a -> return ()) ]

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readPrefs :: FileName -> IO Prefs
readPrefs fn = do
    res <- parseFromFile (prefsParser defaultPrefs prefsDescription) fn
    case res of
        Left pe -> error $"Error reading prefs file " ++ show fn ++ " " ++ show pe
        Right r -> return r  

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

writePrefs :: FilePath -> Prefs -> IO ()
writePrefs fpath prefs = writeFile fpath (showPrefs prefs prefsDescription)

-- ------------------------------------------------------------
-- * Editing
-- ------------------------------------------------------------

editPrefs :: GhfAction
editPrefs = do
    ghfR <- ask
    p <- readGhf prefs
    res <- lift $editPrefs' p prefsDescription ghfR
                    
    lift $putStrLn $show res

editPrefs' :: Prefs -> [FieldDescription Prefs] -> GhfRef -> IO ()
editPrefs' prefs prefsDesc ghfR  = do
    lastAppliedPrefsRef <- newIORef prefs
    dialog  <- windowNew
    vb      <- vBoxNew False 12
    bb      <- hButtonBoxNew
    apply   <- buttonNewFromStock "gtk-apply"
    restore <- buttonNewFromStock "Restore"
    ok      <- buttonNewFromStock "gtk-ok"
    cancel  <- buttonNewFromStock "gtk-cancel"
    boxPackStart bb apply PackNatural 0
    boxPackStart bb restore PackNatural 0
    boxPackStart bb ok PackNatural 0
    boxPackStart bb cancel PackNatural 0
    resList <- mapM (\ (FD _ _ _ _ editorF _) -> editorF prefs) prefsDesc
    let (widgets, setInjs, getExts,_) = unzip4 resList 
    mapM_ (\ sb -> boxPackStart vb sb PackNatural 12) widgets
    ok `onClicked` (do
        newPrefs <- foldM (\ a b -> b a) prefs getExts
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF newPrefs lastAppliedPrefs) ghfR) prefsDesc
        writePrefs "config/Default.prefs" newPrefs
        runReaderT (modifyGhf_ (\ghf -> return (ghf{prefs = newPrefs}))) ghfR
        widgetDestroy dialog)
    apply `onClicked` (do
        newPrefs <- foldM (\ prf getEx -> getEx prf) prefs getExts
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF newPrefs lastAppliedPrefs) ghfR) prefsDesc
        writeIORef lastAppliedPrefsRef newPrefs)
    restore `onClicked` (do
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF prefs lastAppliedPrefs) ghfR) prefsDesc
        mapM_ (\ setInj -> setInj prefs) setInjs
        writeIORef lastAppliedPrefsRef prefs)
    cancel `onClicked` (do
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF prefs lastAppliedPrefs) ghfR) prefsDesc
        widgetDestroy dialog)
    boxPackStart vb bb PackNatural 0
    containerAdd dialog vb
    widgetShowAll dialog    
    return ()
