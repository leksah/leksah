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
import Ghf.Core.State
import Ghf.ViewFrame
import Ghf.BuildInfoEditor
import GUI.Ghf.EditorBasics
import GUI.Ghf.MakeEditor hiding (fieldEditor, parameters)
import GUI.Ghf.SimpleEditors
import GUI.Ghf.CompositeEditors
import GUI.Ghf.Parameters
import Ghf.SourceEditor
import Ghf.PrinterParser hiding (fieldParser,parameters)
import Ghf.File
import Ghf.SpecialEditors
import Ghf.DescriptionPP

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
    ,   modulesPanePath     =   RightTop
    ,   controlPanePath     =   RightTop
    ,   sourceDirectories   =   ["C:/ghc","C:/cygwin/home/Nicklisch-Franken/collect"]
    }

prefsDescription :: [(String,[FieldDescriptionPP Prefs])]
prefsDescription = [
    ("Editor", [
        mkFieldPP
            (paraName <<<- ParaName "Show line numbers"
                $ paraSynopsis <<<- ParaSynopsis "(True/False)" $ emptyParams)
            (PP.text . show)
            boolParser
            showLineNumbers
            (\ b a -> a{showLineNumbers = b})
            boolEditor
            (\b -> do
                buffers <- allBuffers
                mapM_ (\buf -> lift$sourceViewSetShowLineNumbers (sourceView buf) b) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName "TextView Font" $ emptyParams)
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
    ,   mkFieldPP
            (paraName <<<- ParaName "Right margin"
                $ paraSynopsis <<<- ParaSynopsis "Size or 0 for no right margin"
                    $ paraShadow <<<- ParaShadow ShadowIn $ emptyParams)
            (\a -> (PP.text . show) (case a of Nothing -> 0; Just i -> i))
            (do i <- intParser
                return (if i == 0 then Nothing else Just i))
            rightMargin
            (\b a -> a{rightMargin = b})
            (maybeEditor (intEditor (1.0, 200.0, 5.0), paraName <<<- ParaName "Position"
                    $ emptyParams)
                    True "Show it ?")
            (\b -> do
                buffers <- allBuffers
                mapM_ (\buf -> case b of
                                Just n -> do
                                    lift $sourceViewSetMargin (sourceView buf) n
                                    lift $sourceViewSetShowMargin (sourceView buf) True
                                Nothing -> lift $sourceViewSetShowMargin (sourceView buf) False)
                                                buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName "Tab width" $ emptyParams)
            (PP.text . show)
            intParser
            tabWidth
            (\b a -> a{tabWidth = b})
            (intEditor (1.0, 20.0, 1.0))
            (\i -> do
                buffers <- allBuffers
                mapM_ (\buf -> lift $sourceViewSetTabsWidth (sourceView buf) i) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName "Use standard line ends even on windows" $ emptyParams)
            (PP.text . show)
            boolParser
            forceLineEnds
            (\b a -> a{forceLineEnds = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Source candy"
                $ paraSynopsis <<<- ParaSynopsis
                    "Empty for do not use or the name of a candy file in a config dir"
                    $ paraShadow <<<- ParaShadow ShadowIn $ emptyParams)
            (\a -> PP.text (case a of Nothing -> ""; Just s -> s))
            (do id <- identifier
                return (if null id then Nothing else Just (id)))
            sourceCandy (\b a -> a{sourceCandy = b})
            (maybeEditor (stringEditor, paraName <<<- ParaName "Candy specification"
                                    $ emptyParams)
                    True "Use it ?")
            (\cs -> case cs of
                        Nothing -> do
                            setCandyState False
                            editCandy
                        Just name -> do
                            setCandyState True
                            editCandy)
    ,   mkFieldPP
            (paraName <<<- ParaName "Name of the keymap"
                $ paraSynopsis <<<- ParaSynopsis
                    "The name of a keymap file in a config dir"
                    $ paraDirection <<<- ParaDirection Horizontal $ emptyParams)
            PP.text
            identifier
            keymapName
            (\b a -> a{keymapName = b})
            stringEditor
            (\ a -> return ())
    ]),
    ("Other", [
        mkFieldPP
            (paraName <<<- ParaName "LogView Font" $ emptyParams)
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
    ,   mkFieldPP
            (paraName <<<- ParaName "Window default size"
                $ paraSynopsis <<<- ParaSynopsis
                    "Default size of the main ghf window specified as pair (int,int)"
                    $ paraShadow <<<- ParaShadow ShadowIn $ emptyParams)
            (PP.text.show)
            (pairParser intParser)
            defaultSize (\(c,d) a -> a{defaultSize = (c,d)})
            (pairEditor ((intEditor (0.0, 3000.0, 25.0)),
                            paraName <<<- ParaName "X" $ emptyParams)
                        ((intEditor (0.0, 3000.0, 25.0)),
                            paraName <<<- ParaName "Y" $ emptyParams))
            (\a -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Browser" $ emptyParams)
            (PP.text . show)
            stringParser
            browser
            (\b a -> a{browser = b})
            stringEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Standard source editor path" $ emptyParams)
            (PP.text . show)
            readParser
            sourcePanePath
            (\b a -> a{sourcePanePath = b})
            panePathEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName
                "Paths under which haskell sources for packages may be found" $ emptyParams)
            (PP.text . show)
            readParser
            sourceDirectories
            (\b a -> a{sourceDirectories = b})
            (filesEditor Nothing FileChooserActionSelectFolder "Select folder")
            (\i -> return ())
    ])]

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readPrefs :: FilePath -> IO Prefs
readPrefs fn = do
    res <- P.parseFromFile (prefsParser defaultPrefs (concatMap snd prefsDescription)) fn
    case res of
        Left pe -> error $"Error reading prefs file " ++ show fn ++ " " ++ show pe
        Right r -> return r

prefsParser ::  a ->  [FieldDescriptionPP a] ->  P.CharParser () a
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

showPrefs ::  a ->  [FieldDescriptionPP a] ->  String
showPrefs prefs prefsDesc = PP.render $
    foldl (\ doc (FDPP _ printer _ _ _) ->  doc PP.$+$ printer prefs) PP.empty prefsDesc


-- ------------------------------------------------------------
-- * Editing
-- ------------------------------------------------------------

editPrefs :: GhfAction
editPrefs = do
    ghfR <- ask
    p <- readGhf prefs
    res <- lift $editPrefs' p prefsDescription ghfR
    lift $putStrLn $show res


editPrefs' :: Prefs -> [(String,[FieldDescriptionPP Prefs])] -> GhfRef -> IO ()
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
    let fieldNames = map (\fd -> case getParameterPrim paraName (parameters fd) of
                                        Just s -> s
                                        Nothing -> "Unnamed") flatPrefsDesc
    ok `onClicked` (do
        mbNewPrefs <- extractAndValidate prefs getExts fieldNames
        case mbNewPrefs of
            Nothing -> return ()
            Just newPrefs -> do
                lastAppliedPrefs <- readIORef lastAppliedPrefsRef
                mapM_ (\ (FDPP _ _ _ _ applyF) -> runReaderT (applyF newPrefs lastAppliedPrefs) ghfR) flatPrefsDesc
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
                mapM_ (\ (FDPP _ _ _ _ applyF) -> runReaderT (applyF newPrefs lastAppliedPrefs) ghfR) flatPrefsDesc
                writeIORef lastAppliedPrefsRef newPrefs)
    restore `onClicked` (do
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FDPP _ _ _ _ applyF) -> runReaderT (applyF prefs lastAppliedPrefs) ghfR) flatPrefsDesc
        mapM_ (\ setInj -> setInj prefs) setInjs
        writeIORef lastAppliedPrefsRef prefs)
    cancel `onClicked` (do
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FDPP _ _ _ _ applyF) -> runReaderT (applyF prefs lastAppliedPrefs) ghfR) flatPrefsDesc
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


