--
-- | Module for saving, restoring and editing preferences
-- 

module Ghf.Editor.PreferencesEditor (
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
import Data.Maybe(fromJust)

import Debug.Trace

import Ghf.Core
import Ghf.Editor.SourceEditor
import Ghf.GUI.ViewFrame
import Ghf.GUI.Keymap
import Ghf.GUI.Menu(actions,makeMenu,menuDescription)
import Ghf.Editor.PropertyEditor
import Ghf.PrinterParser

type Applicator alpha = alpha -> GhfAction

data FieldDescription alpha =  FD {
        parameters      ::  Parameters
    ,   fieldPrinter    ::  alpha -> PP.Doc
    ,   fieldParser     ::  alpha -> CharParser () alpha    
    ,   fieldEditor     ::  alpha -> IO (Widget, Injector alpha , alpha -> Extractor alpha , Notifier)
    ,   applicator      ::  alpha -> alpha -> GhfAction     
    }

type MkFieldDescription alpha beta =
    Parameters ->      
    (Printer beta) -> 
    (Parser beta) ->                     
    (Getter alpha beta) ->            
    (Setter alpha beta) ->            
    (Editor beta) ->
    (Applicator beta) -> 
    FieldDescription alpha

mkField :: Eq beta => MkFieldDescription alpha beta
mkField parameters printer parser getter setter editor applicator =
    FD parameters
        (\ dat -> (PP.text (case paraName parameters of 
                                    Nothing -> "" 
                                    Just str -> str) PP.<> PP.colon)
                PP.$$ (PP.nest 15 (printer (getter dat)))
                PP.$$ (PP.nest 5 (case synopsis parameters of 
                                    Nothing -> PP.empty 
                                    Just str -> PP.text $"--" ++ str)))
        (\ dat -> try (do
            symbol (case paraName parameters of 
                                    Nothing -> "" 
                                    Just str -> str)
            colon
            val <- parser
            return (setter val dat)))
        (\ dat -> do
            (widget, inj,ext,notiRef) <- editor parameters
            inj (getter dat)
            return (widget,
                    (\a -> inj (getter a)), 
                    (\a -> do 
                        b <- ext
                        case b of
                            Just b -> return (Just (setter b a))
                            Nothing -> return Nothing),
                    notiRef))
        (\ newDat oldDat -> do --appicator
            let newField = getter newDat
            let oldField = getter oldDat
            if newField == oldField
                then return ()
                else applicator newField)

defaultPrefs = Prefs {
        showLineNumbers     =   True
    ,   rightMargin         =   Just 100
    ,   tabWidth            =   4
    ,   sourceCandy         =  Just("Default")
    ,   keymapName          =  "Default" 
    ,   defaultSize         =  (1024,800)}


prefsDescription :: [FieldDescription Prefs]
prefsDescription = [
        mkField (emptyParams{   paraName = Just "Show line numbers", 
                                synopsis = Just"(True/False)"}) 
            (PP.text . show)
            boolParser
            showLineNumbers
            (\ b a -> a{showLineNumbers = b})
            boolEditor
            (\b -> do
                buffers <- allBuffers
                mapM_ (\buf -> lift$sourceViewSetShowLineNumbers (sourceView buf) b) buffers)
    ,   mkField (emptyParams {paraName = Just "Right margin", 
                              synopsis = Just"Size or 0 for no right margin"})
            (\a -> (PP.text . show) (case a of Nothing -> 0; Just i -> i))
            (do i <- intParser
                return (if i == 0 then Nothing else Just i))
            rightMargin
            (\b a -> a{rightMargin = b})
            (maybeEditor (intEditor, emptyParams {spinRange= Just (0.0, 200.0, 5.0), 
                                                  paraName = Just "Position"}) 
                    True "Show right margin?")
            (\b -> do
                buffers <- allBuffers
                mapM_ (\buf -> case b of
                                Just n -> do
                                    lift $sourceViewSetMargin (sourceView buf) n
                                    lift $sourceViewSetShowMargin (sourceView buf) True
                                Nothing -> lift $sourceViewSetShowMargin (sourceView buf) False)
                                                buffers)
    ,   mkField (emptyParams{paraName = Just "Tab width", spinRange= Just (0.0, 20.0, 1.0)})
            (PP.text . show)
            intParser
            tabWidth
            (\b a -> a{tabWidth = b})
            intEditor
            (\i -> do
                buffers <- allBuffers
                mapM_ (\buf -> lift $sourceViewSetTabsWidth (sourceView buf) i) buffers)
    ,   mkField (emptyParams{   paraName = Just "Source candy", 
                                synopsis = Just"Empty for do not use or the name of a candy file in a config dir"})
            (\a -> PP.text (case a of Nothing -> ""; Just s -> s)) 
            (do id <- identifier
                return (if null id then Nothing else Just (id)))
            sourceCandy (\b a -> a{sourceCandy = b})
            (maybeEditor (stringEditor, emptyParams{paraName = Just "candy specification"}) 
                    True "Use Source Candy?")
            (\cs -> case cs of
                        Nothing -> do 
                            setCandyState False
                            editCandy
                        Just name -> do
                            setCandyState True
                            editCandy)
    ,   mkField (emptyParams{   paraName = Just "Name of the keymap" 
                            ,   synopsis = Just "The name of a keymap file in a config dir"
                            ,   direction = Just Horizontal})
            PP.text
            identifier
            keymapName
            (\b a -> a{keymapName = b})
            stringEditor
            (\ a -> return ())
    ,   mkField (emptyParams{   paraName = Just "Window default size", 
                                synopsis = Just "Default size of the main ghf window specified as pair (int,int)" })            
            (PP.text.show) 
            (pairParser intParser)
            defaultSize (\(c,d) a -> a{defaultSize = (c,d)})
            (pairEditor ((intEditor ), emptyParams {    spinRange= Just (0.0, 3000.0, 25.0), 
                                                        paraName = Just "X"})
                        ((intEditor ), emptyParams {    spinRange= Just (0.0, 3000.0, 25.0), 
                                                        paraName = Just "Y"}))
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

prefsParser ::  a ->  [FieldDescription a] ->  CharParser () a
prefsParser def descriptions =
    let parsersF = map fieldParser descriptions in do
        whiteSpace
        res <-  applyFieldParsers def parsersF
        return res
        <?> "prefs parser"

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

writePrefs :: FilePath -> Prefs -> IO ()
writePrefs fpath prefs = writeFile fpath (showPrefs prefs prefsDescription)

showPrefs ::  a ->  [FieldDescription a] ->  String
showPrefs prefs prefsDesc = PP.render $
    foldl (\ doc (FD _ printer _ _ _) ->  doc PP.$+$ printer prefs) PP.empty prefsDesc


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
{--    lastAppliedPrefsRef <- newIORef prefs
    dialog  <- windowNew
    vb      <- vBoxNew False 0
    bb      <- hButtonBoxNew
    apply   <- buttonNewFromStock "gtk-apply"
    restore <- buttonNewFromStock "Restore"
    ok      <- buttonNewFromStock "gtk-ok"
    cancel  <- buttonNewFromStock "gtk-cancel"
    boxPackStart bb apply PackNatural 0
    boxPackStart bb restore PackNatural 0
    boxPackStart bb ok PackNatural 0
    boxPackStart bb cancel PackNatural 0
    resList <- mapM (\ (FD _ _ _ editorF _) -> editorF prefs) prefsDesc
    let (widgets, setInjs, getExts,_) = unzip4 resList 
    mapM_ (\ sb -> boxPackStart vb sb PackGrow 0) widgets
    ok `onClicked` (do
        newPrefs <- foldM (\ a b -> b a) prefs getExts
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF newPrefs lastAppliedPrefs) ghfR) prefsDesc
        writePrefs "config/Default.prefs" newPrefs
        runReaderT (modifyGhf_ (\ghf -> return (ghf{prefs = newPrefs}))) ghfR
        widgetDestroy dialog)
    apply `onClicked` (do
        newPrefs <- foldM (\ prf getEx -> getEx prf) prefs (map fromJust getExts)
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
    boxPackEnd vb bb PackNatural 7
    containerAdd dialog vb
    widgetShowAll dialog  --}
    return ()