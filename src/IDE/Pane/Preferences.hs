{-# OPTIONS_GHC -XScopedTypeVariables -XDeriveDataTypeable -XMultiParamTypeClasses
    -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Preferences
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
-- Portability :  portable
--
--
-- | Module for saving, restoring and editing preferences
--
---------------------------------------------------------------------------------


module IDE.Pane.Preferences (
    IDEPrefs(..)
,   PrefsState
,   readPrefs
,   writePrefs
,   defaultPrefs
,   prefsDescription
,   editPrefs
) where

import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk
import Control.Monad.Reader
import qualified Text.ParserCombinators.Parsec as P
import Data.List
import qualified Text.PrettyPrint.HughesPJ as PP
import Distribution.Package
import Data.IORef
import Data.Typeable

import Control.Event
import Graphics.UI.Editor.Basics
import IDE.Core.State
import Graphics.UI.Editor.Simple
import Graphics.UI.Editor.Composite
import Graphics.UI.Editor.Parameters
import Graphics.UI.Editor.MakeEditor hiding (parameters)
import IDE.DescriptionPP
import IDE.PrinterParser hiding (fieldParser,parameters)
import IDE.Pane.SourceBuffer
import IDE.Pane.Log
import IDE.SpecialEditors
import Default
import IDE.FileUtils
import System.IO
import Distribution.InstalledPackageInfo (package)
import IDE.Metainfo.GHCUtils (getInstalledPackageInfos,inGhc)

--
-- | The Preferences Pane
--

data IDEPrefs               =   IDEPrefs {
    prefsBox                ::   VBox
} deriving Typeable

data PrefsState             =   PrefsState
    deriving(Eq,Ord,Read,Show,Typeable)

instance IDEObject IDEPrefs

instance Pane IDEPrefs IDEM
    where
    primPaneName _  =   "Prefs"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . prefsBox
    paneId b        =   "*Prefs"
    makeActive prefs =  activatePane prefs []
    close pane      =   do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  liftIO $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  liftIO $ do
                sysMessage Normal "notebook page not found: unexpected"
                return ()
            Just i  ->  do
                deactivatePaneIfActive pane
                liftIO $ do
                    notebookRemovePage nb i
                    widgetDestroy (getTopWidget pane)
                removePaneAdmin pane

instance RecoverablePane IDEPrefs PrefsState IDEM where
    saveState p     =   return Nothing
    recoverState pp st  =  return ()

-- ------------------------------------------------------------
-- * Editing
-- ------------------------------------------------------------

editPrefs :: IDEAction
editPrefs = do
    getPrefs
    return ()

getPrefs :: IDEM IDEPrefs
getPrefs = do
    mbPrefs <- getPane
    case mbPrefs of
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (sourcePanePath prefs) layout
            nb          <-  getNotebook pp
            initPrefs pp nb
            mbPrefs <- getPane
            case mbPrefs of
                Nothing ->  throwIDE "Can't init prefs pane"
                Just m  ->  do
                        liftIO $bringPaneToFront m
                        return m
        Just m ->   do
            liftIO $bringPaneToFront m
            return m

initPrefs :: PanePath -> Notebook -> IDEAction
initPrefs panePath nb2 = do
    packageInfos <- inGhc getInstalledPackageInfos
    let flatPrefsDesc = flattenFieldDescriptionPP (prefsDescription (map package packageInfos))
    prefs       <-  readIDE prefs
    lastAppliedPrefsRef <- liftIO $ newIORef prefs
    panes       <-  readIDE panes
    paneMap     <-  readIDE paneMap
    currentInfo <-  readIDE currentInfo
    (buf,cids)  <-  reifyIDE $ \ideR -> do
        vb      <-  vBoxNew False 0
        bb      <-  hButtonBoxNew
        apply   <-  buttonNewFromStock "gtk-apply"
        restore <-  buttonNewFromStock "Restore"
        save    <-  buttonNewFromStock "gtk-save"
        closeB  <-  buttonNewFromStock "gtk-close"
        boxPackStart bb apply PackNatural 0
        boxPackStart bb restore PackNatural 0
        boxPackStart bb save PackNatural 0
        boxPackStart bb closeB PackNatural 0
        (widget,injb,ext,notifier) <-  buildEditor (extractFieldDescription $ prefsDescription (map package packageInfos)) prefs
        boxPackStart vb widget PackGrow 7
        boxPackEnd vb bb PackNatural 7
        let prefsPane = IDEPrefs vb
        apply `onClicked` (do
            mbNewPrefs <- extract prefs [ext]
            case mbNewPrefs of
                Nothing -> return ()
                Just newPrefs -> do
                    lastAppliedPrefs    <- readIORef lastAppliedPrefsRef
                    mapM_ (\ (FDPP _ _ _ _ applyF) -> reflectIDE (applyF newPrefs lastAppliedPrefs) ideR ) flatPrefsDesc
                    writeIORef lastAppliedPrefsRef newPrefs)
        restore `onClicked` (do
            lastAppliedPrefs <- readIORef lastAppliedPrefsRef
            mapM_ (\ (FDPP _ _ _ _ applyF) -> reflectIDE (applyF prefs lastAppliedPrefs) ideR ) flatPrefsDesc
            injb prefs
            writeIORef lastAppliedPrefsRef prefs)
        save `onClicked` (do
            lastAppliedPrefs <- readIORef lastAppliedPrefsRef
            mbNewPrefs <- extract prefs [ext]
            case mbNewPrefs of
                Nothing -> return ()
                Just newPrefs -> do
                mapM_ (\ (FDPP _ _ _ _ applyF) -> reflectIDE (applyF newPrefs lastAppliedPrefs) ideR ) flatPrefsDesc
                fp <- getConfigFilePathForSave "Default.prefs"
                writePrefs fp newPrefs
                reflectIDE (modifyIDE_ (\ide -> return (ide{prefs = newPrefs}))) ideR )
        closeB `onClicked` (reflectIDE (close prefsPane) ideR )
        registerEvent notifier FocusIn (Left (\e -> do
            reflectIDE (makeActive prefsPane) ideR
            return (e{gtkReturn=False})))
        notebookInsertOrdered nb2 vb (paneName prefsPane) Nothing
        widgetShowAll vb
        return (prefsPane,[])
    addPaneAdmin buf [] panePath
    liftIO $widgetGrabFocus (prefsBox buf)


-- ------------------------------------------------------------
-- * Dialog definition
-- ------------------------------------------------------------

prefsDescription :: [PackageIdentifier] -> FieldDescriptionPP Prefs
prefsDescription packages = NFDPP [
    ("Editor", VFDPP emptyParams [
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
                mapM_ (\buf -> liftIO $ sourceViewSetShowLineNumbers (sourceView buf) b) buffers)
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
                fdesc <- liftIO $ fontDescriptionFromString (case mbs of Just str -> str; Nothing -> "")
                liftIO $mapM_ (\buf -> widgetModifyFont (castToWidget $sourceView buf) (Just fdesc)) buffers)
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
                                    liftIO $sourceViewSetRightMarginPosition (sourceView buf) (fromIntegral n)
                                    liftIO $sourceViewSetShowRightMargin (sourceView buf) True
                                Nothing -> liftIO $sourceViewSetShowRightMargin (sourceView buf) False)
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
                mapM_ (\buf -> liftIO $sourceViewSetIndentWidth (sourceView buf) i) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName "Use standard line ends even on windows" $ emptyParams)
            (PP.text . show)
            boolParser
            forceLineEnds
            (\b a -> a{forceLineEnds = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Remove trailing blanks when saving a file" $ emptyParams)
            (PP.text . show)
            boolParser
            removeTBlanks
            (\b a -> a{removeTBlanks = b})
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
            (maybeEditor ((stringEditor (\s -> not (null s))), paraName <<<- ParaName "Candy specification"
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
            (paraName <<<- ParaName "Editor Style" $ emptyParams)
            (\a -> PP.text (case a of Nothing -> show ""; Just s -> show s))
            (do str <- stringParser
                return (if null str then Nothing else Just (str)))
            sourceStyle
            (\b a -> a{sourceStyle = b})
            styleEditor
            (\mbs -> do
                styleManager <- liftIO sourceStyleSchemeManagerNew
                mbStyle      <- case mbs of
                                    Nothing  -> return Nothing
                                    Just str -> liftIO $ do
                                        ids <- sourceStyleSchemeManagerGetSchemeIds styleManager
                                        if elem str ids
                                            then liftM Just
                                                (sourceStyleSchemeManagerGetScheme styleManager str)
                                            else return Nothing
                case mbStyle of
                    Nothing -> return ()
                    Just style -> do
                        buffers <- allBuffers
                        mapM_ (\buf -> do
                            gtkBuf  <-  liftIO $ textViewGetBuffer (sourceView buf)
                            liftIO $ sourceBufferSetStyleScheme (castToSourceBuffer gtkBuf) style) buffers)
    ]),
    ("GUI Options", VFDPP emptyParams [
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
                fdesc <- liftIO $fontDescriptionFromString (case mbs of Just str -> str; Nothing -> "")
                liftIO $widgetModifyFont (castToWidget $textView buffer) (Just fdesc))
    ,   mkFieldPP
            (paraName <<<- ParaName "Window default size"
                $ paraSynopsis <<<- ParaSynopsis
                    "Default size of the main ide window specified as pair (int,int)"
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
            (stringEditor (\s -> not (null s)))
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Standard source pane path" $ emptyParams)
            (PP.text . show)
            readParser
            sourcePanePath
            (\b a -> a{sourcePanePath = b})
            panePathEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Standard log pane path" $ emptyParams)
            (PP.text . show)
            readParser
            logPanePath
            (\b a -> a{logPanePath = b})
            panePathEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Standard modules pane path" $ emptyParams)
            (PP.text . show)
            readParser
            modulesPanePath
            (\b a -> a{modulesPanePath = b})
            panePathEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Name of the keymap"
                $ paraSynopsis <<<- ParaSynopsis
                    "The name of a keymap file in a config dir"
                    $ paraDirection <<<- ParaDirection Horizontal $ emptyParams)
            PP.text
            identifier
            keymapName
            (\b a -> a{keymapName = b})
            (stringEditor (\s -> not (null s)))
            (\ a -> return ())
    ]),
    ("Metadata", VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName
                "Paths under which haskell sources for packages may be found" $ emptyParams)
            (PP.text . show)
            readParser
            sourceDirectories
            (\b a -> a{sourceDirectories = b})
            (filesEditor Nothing FileChooserActionSelectFolder "Select folder")
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName
                "Packages which are excluded from the modules pane" $ emptyParams)
            (PP.text . show)
            readParser
            packageBlacklist
            (\b a -> a{packageBlacklist = b})
            (dependenciesEditor packages)
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Update metadata after every build" $ emptyParams)
            (PP.text . show)
            boolParser
            collectAfterBuild
            (\b a -> a{collectAfterBuild = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Update metadata at startup" $ emptyParams)
            (PP.text . show)
            boolParser
            collectAtStart
            (\b a -> a{collectAtStart = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Extract packages from cabal-install" $ emptyParams)
            (PP.text . show)
            readParser
            autoExtractTars
            (\b a -> a{autoExtractTars = b})
            (maybeEditor ((fileEditor (Just "~/.cabal/packages/") FileChooserActionSelectFolder
                "Select folder"), emptyParams) True "Yes")
            (\i -> return ())
    ])]

instance Default PackageIdentifier where
    getDefault = case toPackageIdentifier "unknown-0" of
                    Nothing -> throwIDE "Preferences.getDefault: Can't parse Package Identifier"
                    Just it -> it

defaultPrefs = Prefs {
        showLineNumbers     =   True
    ,   rightMargin         =   Just 100
    ,   tabWidth            =   4
    ,   sourceCandy         =   Just("Default")
    ,   keymapName          =   "Default"
    ,   forceLineEnds       =   True
    ,   removeTBlanks       =   True
    ,   textviewFont        =   Nothing
    ,   sourceStyle         =   Nothing
    ,   logviewFont         =   Nothing
    ,   defaultSize         =   (1024,800)
    ,   browser             =   "firefox"
    ,   sourcePanePath      =   [LeftP]
    ,   logPanePath         =   [RightP,BottomP]
    ,   modulesPanePath     =   [RightP,TopP]
    ,   sourceDirectories   =   []
    ,   packageBlacklist    =   []
    ,   collectAfterBuild   =   False
    ,   collectAtStart      =   True
    ,   autoExtractTars     =   Nothing
    }

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readPrefs :: FilePath -> IO Prefs
readPrefs fn = catch (do
    res <- P.parseFromFile (prefsParser defaultPrefs (flattenFieldDescriptionPP
                    (prefsDescription []))) fn
    case res of
                Left pe -> throwIDE $ "Error reading prefs file " ++ show fn ++ " " ++ show pe
                Right r -> return r)
    (\ e -> throwIDE $ "Error reading prefs file " ++ show fn ++ " " ++ show e)

prefsParser ::  a ->  [FieldDescriptionPP a] ->  P.CharParser () a
prefsParser def descriptions =
    let parsersF = map fieldParser descriptions in do
        res <-  applyFieldParsers def parsersF
        return res
        P.<?> "prefs parser"

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

writePrefs :: FilePath -> Prefs -> IO ()
writePrefs fpath prefs = writeFile fpath (showPrefs prefs (flattenFieldDescriptionPP (prefsDescription [])))

showPrefs ::  a ->  [FieldDescriptionPP a] ->  String
showPrefs prefs prefsDesc = PP.render $
    foldl (\ doc (FDPP _ printer _ _ _) ->  doc PP.$+$ printer prefs) PP.empty prefsDesc

