-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Preferences
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
--
-- | Module for saving, restoring and editing preferences
--
---------------------------------------------------------------------------------


module Ghf.Preferences (
    readPrefs
,   writePrefs
,   editPrefs

,   prefsDescription
) where

import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk
import Control.Monad.Reader
import qualified Text.ParserCombinators.Parsec as P
import Data.IORef
import Data.List
import qualified Text.PrettyPrint.HughesPJ as PP

import Ghf.Log
import Ghf.Core
import Ghf.ViewFrame
import Ghf.PropertyEditor hiding(parameters,fieldEditor)
import Ghf.SourceEditor
import Ghf.PrinterParser hiding (fieldParser,parameters)
import Ghf.File
import Ghf.SpecialEditors

type Applicator alpha = alpha -> GhfAction

data FieldDescription alpha =  FD {
        parameters      ::  Parameters
    ,   fieldPrinter    ::  alpha -> PP.Doc
    ,   fieldParser     ::  alpha -> P.CharParser () alpha
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
                PP.$$ (PP.nest 5 (case synopsisP parameters of
                                    Nothing -> PP.empty
                                    Just str -> PP.text $"--" ++ str)))
        (\ dat -> P.try (do
            symbol (case paraName parameters of
                                    Nothing -> ""
                                    Just str -> str)
            colon
            val <- parser
            return (setter val dat)))
        (\ dat -> do
            (widget, inj,ext,noti) <- editor parameters
            inj (getter dat)
            noti FocusOut (Left (\e -> do
                putStrLn "Handling Focus out"
                v <- ext
                case v of
                    Just _ -> do
                        widgetModifyFg widget StateNormal (Color 0 0 0)
                        return False
                    Nothing -> do
                        widgetModifyFg widget StateNormal (Color 65535 65535 0)
                        return False))
            return (widget,
                    (\a -> inj (getter a)),
                    (\a -> do
                        b <- ext
                        case b of
                            Just b -> return (Just (setter b a))
                            Nothing -> return Nothing),
                    noti))
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
    ,   sourceCandy         =   Just("Default")
    ,   keymapName          =   "Default"
    ,   forceLineEnds       =   True
    ,   textviewFont        =   Nothing
    ,   logviewFont         =   Nothing
    ,   defaultSize         =   (1024,800)
    ,   browser             =   "firefox"
    ,   sourcePanePath      =   LeftTop
    ,   logPanePath         =   RightBottom
    ,   infoPanePath        =   RightBottom
    }

prefsDescription :: [(String,[FieldDescription Prefs])]
prefsDescription = [
    ("Editor", [
        mkField (emptyParams
            {   paraName = Just "Show line numbers"
            ,   synopsisP = Just"(True/False)"})
            (PP.text . show)
            boolParser
            showLineNumbers
            (\ b a -> a{showLineNumbers = b})
            boolEditor
            (\b -> do
                buffers <- allBuffers
                mapM_ (\buf -> lift$sourceViewSetShowLineNumbers (sourceView buf) b) buffers)
    ,   mkField (emptyParams
            {   paraName = Just "TextView Font"})
            (\a -> PP.text (case a of Nothing -> show ""; Just s -> show s))
            (do str <- stringParser
                return (if null str then Nothing else Just (str)))
            textviewFont
            (\ b a -> a{textviewFont = b})
            fontEditor
            (\mbs -> do
                buffers <- allBuffers
                fdesc <- lift $fontDescriptionFromString (case mbs of Just str -> str; Nothing -> "")
                lift $mapM_ (\buf -> widgetModifyFont (castToWidget $sourceView buf) (Just fdesc)) buffers)
    ,   mkField (emptyParams
            {  paraName = Just "Right margin"
            ,  synopsisP = Just "Size or 0 for no right margin"
            ,  shadow   = Just ShadowIn})
            (\a -> (PP.text . show) (case a of Nothing -> 0; Just i -> i))
            (do i <- intParser
                return (if i == 0 then Nothing else Just i))
            rightMargin
            (\b a -> a{rightMargin = b})
            (maybeEditor (intEditor (1.0, 200.0, 5.0), emptyParams {paraName = Just "Position"})
                    True "Show it ?")
            (\b -> do
                buffers <- allBuffers
                mapM_ (\buf -> case b of
                                Just n -> do
                                    lift $sourceViewSetMargin (sourceView buf) n
                                    lift $sourceViewSetShowMargin (sourceView buf) True
                                Nothing -> lift $sourceViewSetShowMargin (sourceView buf) False)
                                                buffers)
    ,   mkField (emptyParams{paraName = Just "Tab width"})
            (PP.text . show)
            intParser
            tabWidth
            (\b a -> a{tabWidth = b})
            (intEditor (1.0, 20.0, 1.0))
            (\i -> do
                buffers <- allBuffers
                mapM_ (\buf -> lift $sourceViewSetTabsWidth (sourceView buf) i) buffers)
    ,   mkField (emptyParams{paraName = Just "Use standard line ends even on windows"})
            (PP.text . show)
            boolParser
            forceLineEnds
            (\b a -> a{forceLineEnds = b})
            boolEditor
            (\i -> return ())
    ,   mkField (emptyParams
            {   paraName = Just "Source candy"
            ,   synopsisP = Just"Empty for do not use or the name of a candy file in a config dir"
            ,   shadow   = Just ShadowIn})
            (\a -> PP.text (case a of Nothing -> ""; Just s -> s))
            (do id <- identifier
                return (if null id then Nothing else Just (id)))
            sourceCandy (\b a -> a{sourceCandy = b})
            (maybeEditor (stringEditor, emptyParams{paraName = Just "Candy specification"})
                    True "Use it ?")
            (\cs -> case cs of
                        Nothing -> do
                            setCandyState False
                            editCandy
                        Just name -> do
                            setCandyState True
                            editCandy)
    ,   mkField (emptyParams{   paraName = Just "Name of the keymap"
                            ,   synopsisP = Just "The name of a keymap file in a config dir"
                            ,   direction = Just Horizontal})
            PP.text
            identifier
            keymapName
            (\b a -> a{keymapName = b})
            stringEditor
            (\ a -> return ())
    ]),
    ("Other", [
        mkField (emptyParams
            {   paraName = Just "LogView Font"})
            (\a -> PP.text (case a of Nothing -> show ""; Just s -> show s))
            (do str <- stringParser
                return (if null str then Nothing else Just (str)))
            logviewFont
            (\ b a -> a{logviewFont = b})
            fontEditor
            (\mbs -> do
                buffer <- getLog
                fdesc <- lift $fontDescriptionFromString (case mbs of Just str -> str; Nothing -> "")
                lift $widgetModifyFont (castToWidget $textView buffer) (Just fdesc))
    ,   mkField (emptyParams
            {   paraName = Just "Window default size"
            ,   synopsisP = Just "Default size of the main ghf window specified as pair (int,int)"
            ,   shadow   = Just ShadowIn})
            (PP.text.show)
            (pairParser intParser)
            defaultSize (\(c,d) a -> a{defaultSize = (c,d)})
            (pairEditor ((intEditor (0.0, 3000.0, 25.0)), emptyParams {paraName = Just "X"})
                        ((intEditor (0.0, 3000.0, 25.0)), emptyParams {paraName = Just "Y"}))
            (\a -> return ())
    ,   mkField (emptyParams{paraName = Just "Browser"})
            (PP.text . show)
            stringParser
            browser
            (\b a -> a{browser = b})
            stringEditor
            (\i -> return ())
    ,   mkField (emptyParams{paraName = Just "Standard source editor path"})
            (PP.text . show)
            readParser
            sourcePanePath
            (\b a -> a{sourcePanePath = b})
            panePathEditor
            (\i -> return ())
    ])]

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readPrefs :: FileName -> IO Prefs
readPrefs fn = do
    res <- P.parseFromFile (prefsParser defaultPrefs (concatMap snd prefsDescription)) fn
    case res of
        Left pe -> error $"Error reading prefs file " ++ show fn ++ " " ++ show pe
        Right r -> return r

prefsParser ::  a ->  [FieldDescription a] ->  P.CharParser () a
prefsParser def descriptions =
    let parsersF = map fieldParser descriptions in do
        whiteSpace
        res <-  applyFieldParsers def parsersF
        return res
        P.<?> "prefs parser"

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

writePrefs :: FilePath -> Prefs -> IO ()
writePrefs fpath prefs = writeFile fpath (showPrefs prefs (concatMap snd prefsDescription))

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


editPrefs' :: Prefs -> [(String,[FieldDescription Prefs])] -> GhfRef -> IO ()
editPrefs' prefs prefsDesc ghfR  = do
    let flatPrefsDesc = concatMap snd prefsDesc
    lastAppliedPrefsRef <- newIORef prefs
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
    nb <- newNotebook
    notebookSetTabPos nb PosTop
    res <- mapM
        (\ (tabLabel, partPrefsDesc) -> do
            resList <- mapM (\ fd -> (fieldEditor fd) prefs) partPrefsDesc
            let (widgetsP, setInjsP, getExtsP,notifiersP) = unzip4 resList
            nbbox <- vBoxNew False 0
            mapM_ (\ w -> boxPackStart nbbox w PackNatural 0) widgetsP
            sw <- scrolledWindowNew Nothing Nothing
            scrolledWindowAddWithViewport sw nbbox
            scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
            notebookAppendPage nb sw tabLabel
            return (widgetsP, setInjsP, getExtsP, notifiersP))
                prefsDesc
    let (widgets, setInjs, getExts, notifiers) =
            foldl (\ (w,i,e,n) (w2,i2,e2,n2) -> (w ++ w2, i ++ i2, e ++ e2, n ++ n2)) ([],[],[],[]) res
    let fieldNames = map (\fd -> case paraName (parameters fd) of
                                        Just s -> s
                                        Nothing -> "Unnamed") flatPrefsDesc
    ok `onClicked` (do
        mbNewPrefs <- extractAndValidate prefs getExts fieldNames
        case mbNewPrefs of
            Nothing -> return ()
            Just newPrefs -> do
                lastAppliedPrefs <- readIORef lastAppliedPrefsRef
                mapM_ (\ (FD _ _ _ _ applyF) -> runReaderT (applyF newPrefs lastAppliedPrefs) ghfR) flatPrefsDesc
                fp <- getConfigFilePathForSave "Default.prefs"
                writePrefs fp newPrefs
                runReaderT (modifyGhf_ (\ghf -> return (ghf{prefs = newPrefs}))) ghfR
                widgetDestroy dialog
                mainQuit)
    apply `onClicked` (do
        mbNewPrefs <- extractAndValidate prefs getExts fieldNames
        case mbNewPrefs of
            Nothing -> return ()
            Just newPrefs -> do
                lastAppliedPrefs <- readIORef lastAppliedPrefsRef
                mapM_ (\ (FD _ _ _ _ applyF) -> runReaderT (applyF newPrefs lastAppliedPrefs) ghfR) flatPrefsDesc
                writeIORef lastAppliedPrefsRef newPrefs)
    restore `onClicked` (do
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FD _ _ _ _ applyF) -> runReaderT (applyF prefs lastAppliedPrefs) ghfR) flatPrefsDesc
        mapM_ (\ setInj -> setInj prefs) setInjs
        writeIORef lastAppliedPrefsRef prefs)
    cancel `onClicked` (do
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FD _ _ _ _ applyF) -> runReaderT (applyF prefs lastAppliedPrefs) ghfR) flatPrefsDesc
        widgetDestroy dialog
        mainQuit)
    dialog `onDelete` (\_ -> do
        widgetDestroy dialog
        mainQuit
        return True)
    boxPackStart vb nb PackGrow 7
    boxPackEnd vb bb PackNatural 7
    containerAdd dialog vb
    widgetSetSizeRequest dialog 500 700
    widgetShowAll dialog
    mainGUI
    return ()


