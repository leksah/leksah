{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -XScopedTypeVariables -XDeriveDataTypeable -XMultiParamTypeClasses
    -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Preferences
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
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
,   getPrefs
) where

import Graphics.UI.Gtk
       (cellText, widgetModifyFont, onClicked, boxPackEnd, boxPackStart,
        buttonNewFromStock, hButtonBoxNew, vBoxNew, castToWidget, VBox,
        ShadowType(..), Packing(..), fontDescriptionFromString, AttrOp(..),
        FileChooserAction(..), Color(..))
import Control.Monad.Reader
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
import Graphics.UI.Editor.DescriptionPP
import Text.PrinterParser hiding (fieldParser,parameters)
import IDE.TextEditor
import IDE.Pane.SourceBuffer
import IDE.Pane.Log
import Default
import IDE.Utils.FileUtils
import IDE.Utils.GUIUtils
import IDE.Debug
    (debugSetPrintBindResult,
     debugSetBreakOnError,
     debugSetBreakOnException,
     debugSetPrintEvldWithShow)
import Graphics.UI.Gtk.SourceView
    (sourceStyleSchemeManagerGetSchemeIds, sourceStyleSchemeManagerNew)
import System.Time (getClockTime)
import System.FilePath((</>))
import qualified IDE.StrippedPrefs as SP
import Control.Exception(SomeException,catch)
import Prelude hiding(catch)

-- ---------------------------------------------------------------------
-- This needs to be incremented, when the preferences format changes
--
prefsVersion :: Int
prefsVersion = 1

--
-- | The Preferences Pane
--

data IDEPrefs               =   IDEPrefs {
    prefsBox                ::   VBox
} deriving Typeable

data PrefsState             =   PrefsState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEPrefs IDEM
    where
    primPaneName _  =   "Prefs"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . prefsBox
    paneId b        =   "*Prefs"

instance RecoverablePane IDEPrefs PrefsState IDEM where
    saveState p     =   return Nothing
    recoverState pp st  =  return Nothing
    builder pp nb windows = do
        prefs <- readIDE prefs
        configDir <- liftIO getConfigDir
        lastAppliedPrefsRef <- liftIO $ newIORef prefs
        packageInfos <- liftIO $ getInstalledPackageIds
        let flatPrefsDesc = flattenFieldDescriptionPP (prefsDescription configDir packageInfos)
        reifyIDE $  \ ideR -> do
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
            (widget,injb,ext,notifier) <-  buildEditor
                                (extractFieldDescription $ prefsDescription configDir packageInfos) prefs
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
                    fp   <- getConfigFilePathForSave standardPreferencesFilename
                    writePrefs fp newPrefs
                    fp2  <-  getConfigFilePathForSave strippedPreferencesFilename
                    SP.writeStrippedPrefs fp2
                        (SP.Prefs {SP.sourceDirectories = sourceDirectories newPrefs,
                                   SP.unpackDirectory   = unpackDirectory newPrefs,
                                   SP.retrieveURL       = retrieveURL newPrefs,
                                   SP.retrieveStrategy  = retrieveStrategy newPrefs,
                                   SP.serverPort        = serverPort newPrefs,
                                   SP.endWithLastConn   = endWithLastConn newPrefs})
                    reflectIDE (modifyIDE_ (\ide -> ide{prefs = newPrefs})) ideR )
            closeB `onClicked` (reflectIDE (closePane prefsPane >> return ()) ideR )
            registerEvent notifier FocusIn (Left (\e -> do
                reflectIDE (makeActive prefsPane) ideR
                return (e{gtkReturn=False})))
            return (Just prefsPane,[])

getPrefs :: Maybe PanePath -> IDEM IDEPrefs
getPrefs Nothing    = forceGetPane (Right "*Prefs")
getPrefs (Just pp)  = forceGetPane (Left pp)

-- ------------------------------------------------------------
-- * Dialog definition
-- ------------------------------------------------------------

prefsDescription :: FilePath -> [PackageIdentifier] -> FieldDescriptionPP Prefs IDEM
prefsDescription configDir packages = NFDPP [
    ("Editor", VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName "Version number of preferences file format"
                $ paraSynopsis <<<- ParaSynopsis "Integer"
                    $ paraShowLabel <<<- ParaShowLabel False $ emptyParams)
            (PP.text . show)
            intParser
            prefsFormat
            (\ b a -> a{prefsFormat = b})
            (noEditor 0)
            (\b -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Time of last storage"
                $ paraShowLabel <<<- ParaShowLabel False $ emptyParams)
            (PP.text . show)
            stringParser
            prefsSaveTime
            (\ b a -> a{prefsSaveTime = b})
            (noEditor "")
            (\b -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Show line numbers"
                $ paraSynopsis <<<- ParaSynopsis "(True/False)"
                    $ emptyParams)
            (PP.text . show)
            boolParser
            showLineNumbers
            (\ b a -> a{showLineNumbers = b})
            boolEditor
            (\b -> do
                buffers <- allBuffers
                mapM_ (\buf -> setShowLineNumbers (sourceView buf) b) buffers)
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
                mapM_ (\buf -> setFont (sourceView buf) mbs) buffers)
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
                mapM_ (\buf -> setRightMargin (sourceView buf) b) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName "Tab width" $ emptyParams)
            (PP.text . show)
            intParser
            tabWidth
            (\b a -> a{tabWidth = b})
            (intEditor (1.0, 20.0, 1.0))
            (\i -> do
                buffers <- allBuffers
                mapM_ (\buf -> setIndentWidth (sourceView buf) i) buffers)
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
            (\a -> PP.text (case a of Nothing -> show ""; Just s -> show s))
            (do str <- stringParser
                return (if null str then Nothing else Just (str)))
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
                buffers <- allBuffers
                mapM_ (\buf -> do
                    ebuf <- getBuffer (sourceView buf)
                    setStyle ebuf mbs) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName "Found Text Background" $ emptyParams)
            (PP.text . show)
            colorParser
            foundBackground
            (\ b a -> a{foundBackground = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                forM_ buffers $ \buf -> do
                    ebuf     <- getBuffer (sourceView buf)
                    tagTable <- getTagTable ebuf
                    mbTag    <- lookupTag tagTable "found"
                    case mbTag of
                        Just tag -> background tag c
                        Nothing  -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Execution Context Text Background" $ emptyParams)
            (PP.text . show)
            colorParser
            contextBackground
            (\ b a -> a{contextBackground = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                forM_ buffers $ \buf -> do
                    ebuf     <- getBuffer (sourceView buf)
                    tagTable <- getTagTable ebuf
                    --  TODO find and set the tag background
                    return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Breakpoint Text Background" $ emptyParams)
            (PP.text . show)
            colorParser
            breakpointBackground
            (\ b a -> a{breakpointBackground = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                forM_ buffers $ \buf -> do
                    ebuf     <- getBuffer (sourceView buf)
                    tagTable <- getTagTable ebuf
                    --  TODO find and set the tag background
                    return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Use Yi - Experimental feature (could wipe your files)" $ emptyParams)
            (PP.text . show)
            boolParser
            useYi
            (\b a -> a{useYi = b})
            boolEditor
            (\i -> return ())
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
            (paraName <<<- ParaName "Use ctrl Tab for Notebook flipper" $ emptyParams)
            (PP.text . show)
            boolParser
            useCtrlTabFlipping
            (\b a -> a{useCtrlTabFlipping = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Complete only on Hotkey" $ emptyParams)
            (PP.text . show)
            boolParser
            completeRestricted
            (\b a -> a{completeRestricted = b})
            boolEditor
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
    ("Initial Pane positions", VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName
                "Categories for panes"
                $ paraShadow <<<- ParaShadow ShadowIn
                     $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (PP.text . show)
            readParser
            categoryForPane
            (\b a -> a{categoryForPane = b})
            (multisetEditor
                (ColumnDescr True [("Pane Id",\(n,_) -> [cellText := n])
                                   ,("Pane Category",\(_,v) -> [cellText := v])])
                ((pairEditor
                    (stringEditor (\s -> not (null s)),emptyParams)
                    (stringEditor (\s -> not (null s)),emptyParams)),emptyParams)
            Nothing
            Nothing)
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName
                "Pane path for category"
                $ paraShadow <<<- ParaShadow ShadowIn
                     $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            (PP.text . show)
            readParser
            pathForCategory
            (\b a -> a{pathForCategory = b})
            (multisetEditor
                (ColumnDescr True [("Pane category",\(n,_) -> [cellText := n])
                                   ,("Pane path",\(_,v) -> [cellText := show v])])
                ((pairEditor
                    (stringEditor (\s -> not (null s)),emptyParams)
                    (genericEditor,emptyParams)),emptyParams)
            Nothing
            Nothing)
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Default pane path" $ emptyParams)
            (PP.text . show)
            readParser
            defaultPath
            (\b a -> a{defaultPath = b})
            genericEditor
            (\i -> return ())
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
            (paraName <<<- ParaName "Maybe a directory for unpacking cabal packages" $ emptyParams)
            (PP.text . show)
            readParser
            unpackDirectory
            (\b a -> a{unpackDirectory = b})
            (maybeEditor (stringEditor (\ _ -> True),emptyParams) True "")
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "An URL to load prebuild metadata" $ emptyParams)
            (PP.text . show)
            stringParser
            retrieveURL
            (\b a -> a{retrieveURL = b})
            (stringEditor (\ _ -> True))
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "A strategy for downloading prebuild metadata" $ emptyParams)
            (PP.text . show)
            readParser
            retrieveStrategy
            (\b a -> a{retrieveStrategy = b})
            (enumEditor ["Retrieve then build","Build then retrieve","Never retrieve"])
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
            (paraName <<<- ParaName "Port number for server connection" $ emptyParams)
            (PP.text . show)
            intParser
            serverPort
            (\b a -> a{serverPort = b})
            (intEditor (1.0, 65535.0, 1.0))
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Server IP address " $ emptyParams)
            (PP.text . show)
            stringParser
            serverIP
            (\b a -> a{serverIP = b})
            (stringEditor (\ s -> not $ null s))
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "End the server with last connection" $ emptyParams)
            (PP.text . show)
            boolParser
            endWithLastConn
            (\b a -> a{endWithLastConn = b})
            boolEditor
            (\i -> return ())
    ]),
    ("Blacklist", VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName
                "Packages which are excluded from the modules pane" $ emptyParams)
            (PP.text . show)
            readParser
            packageBlacklist
            (\b a -> a{packageBlacklist = b})
            (dependenciesEditor packages)
            (\i -> return ())
    ]),
    ("Build", VFDPP emptyParams [
         mkFieldPP
            (paraName <<<- ParaName "Automatically save all files before building" $ emptyParams)
            (PP.text . show)
            boolParser
            saveAllBeforeBuild
            (\b a -> a{saveAllBeforeBuild = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName "Background build" $ emptyParams)
            (PP.text . show)
            boolParser
            backgroundBuild
            (\b a -> a{backgroundBuild = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName "Include linking in background builds" $ emptyParams)
            (PP.text . show)
            boolParser
            backgroundLink
            (\b a -> a{backgroundLink = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName "Auto install packages" $
                paraShadow <<<- ParaShadow ShadowIn $emptyParams)
            (PP.text . show)
            readParser
            autoInstall
            (\b a -> a{autoInstall = b})
            (enumEditor ["Install always after a succesful build",
                "Install if it's a library with dependend packages in the workspace",
                "Never install"])
            (\i -> return ())
    ]),
    ("Debug", VFDPP emptyParams [
           mkFieldPP
            (paraName <<<- ParaName "Enable usage of Show instances in :print" $ emptyParams)
            (PP.text . show)
            boolParser
            printEvldWithShow
            (\b a -> a{printEvldWithShow = b})
            boolEditor
            debugSetPrintEvldWithShow
         , mkFieldPP
            (paraName <<<- ParaName "Break on any exception thrown" $ emptyParams)
            (PP.text . show)
            boolParser
            breakOnException
            (\b a -> a{breakOnException = b})
            boolEditor
            debugSetBreakOnException
         , mkFieldPP
            (paraName <<<- ParaName "Break on uncaught exceptions and errors" $ emptyParams)
            (PP.text . show)
            boolParser
            breakOnError
            (\b a -> a{breakOnError = b})
            boolEditor
            debugSetBreakOnError
         , mkFieldPP
            (paraName <<<- ParaName "Turn on printing of binding results in GHCi" $ emptyParams)
            (PP.text . show)
            boolParser
            printBindResult
            (\b a -> a{printBindResult = b})
            boolEditor
            debugSetPrintBindResult
    ]),
    ("Help", VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName "Browser" $ emptyParams)
            (PP.text . show)
            stringParser
            browser
            (\b a -> a{browser = b})
            (stringEditor (\s -> not (null s)))
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "URL for searching documentation" $
                paraSynopsis <<<- ParaSynopsis
                    ("e.g Hoogle: http://www.haskell.org/hoogle/?q= or " ++
                        "Hayoo: http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=")
                        $ emptyParams)
            (PP.text . show)
            stringParser
            docuSearchURL
            (\b a -> a{docuSearchURL = b})
            (stringEditor (\s -> not (null s)))
            (\i -> return ())
    ])]


styleEditor :: Editor (Maybe String)
styleEditor p n = do
    styleManager <- sourceStyleSchemeManagerNew
    ids          <- sourceStyleSchemeManagerGetSchemeIds styleManager
    maybeEditor (comboSelectionEditor ids id, p) True "Select a special style?" p n


defaultPrefs = Prefs {
        prefsFormat         =   prefsVersion
    ,   prefsSaveTime       =   ""
    ,   showLineNumbers     =   True
    ,   rightMargin         =   Just 100
    ,   tabWidth            =   4
    ,   sourceCandy         =   Just("candy")
    ,   keymapName          =   "keymap"
    ,   forceLineEnds       =   True
    ,   removeTBlanks       =   True
    ,   textviewFont        =   Nothing
    ,   sourceStyle         =   Nothing
    ,   foundBackground     =   Color 65535 65535 32768
    ,   contextBackground   =   Color 65535 49152 49152
    ,   breakpointBackground =  Color 65535 49152 32768
    ,   useYi               =   False
    ,   logviewFont         =   Nothing
    ,   defaultSize         =   (1024,800)
    ,   browser             =   "firefox"
    ,   sourceDirectories   =   []
    ,   packageBlacklist    =   []
    ,   pathForCategory     =   [   ("EditorCategory",[SplitP (LeftP)])
                                ,   ("ToolCategory",[SplitP (RightP),SplitP (TopP)])
                                ,   ("LogCategory",[SplitP (RightP), SplitP (BottomP)])]
    ,   defaultPath         =   [SplitP (LeftP)]
    ,   categoryForPane     =   [   ("*ClassHierarchy","ToolCategory")
                                ,   ("*Debug","ToolCategory")
                                ,   ("*Grep","ToolCategory")
                                ,   ("*Info","ToolCategory")
                                ,   ("*Log","LogCategory")
                                ,   ("*Modules","ToolCategory")
                                ,   ("*Package","ToolCategory")
                                ,   ("*Flags","ToolCategory")
                                ,   ("*Prefs","ToolCategory")
                                ,   ("*References","ToolCategory")
                                ,   ("*Search","ToolCategory")]
    ,   collectAtStart      =   True
    ,   unpackDirectory     =   Nothing
    ,   retrieveURL         =   "http://www.leksah.org"
    ,   retrieveStrategy    =   SP.RetrieveThenBuild
    ,   useCtrlTabFlipping  =   True
    ,   docuSearchURL       =   "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query="
    ,   completeRestricted  =   False
    ,   saveAllBeforeBuild  =   True
    ,   backgroundBuild     =   True
    ,   backgroundLink      =
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
                                False
#else
                                True
#endif
    ,   autoInstall         =   InstallLibs
    ,   printEvldWithShow   =   True
    ,   breakOnException    =   True
    ,   breakOnError        =   True
    ,   printBindResult     =   False
    ,   serverPort          =   11111
    ,   serverIP            =   "127.0.0.1"
    ,   endWithLastConn     =   True
    }

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readPrefs :: FilePath -> IO Prefs
readPrefs fn = catch (do
    configDir <- getConfigDir
    readFields fn (flattenFieldDescriptionPPToS (prefsDescription configDir [])) defaultPrefs)
	(\ (e :: SomeException) -> do
		sysMessage Normal (show e)
		return defaultPrefs)
-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

writePrefs :: FilePath -> Prefs -> IO ()
writePrefs fpath prefs = do
    timeNow         <- liftIO getClockTime
    configDir <- getConfigDir
    let newPrefs    =   prefs {prefsSaveTime = show timeNow, prefsFormat = prefsVersion}
    writeFields fpath newPrefs (flattenFieldDescriptionPPToS (prefsDescription configDir []))


