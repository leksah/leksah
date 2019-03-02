{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings, LambdaCase, PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Leksah
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
--  Main function of Leksah, an Haskell IDE written in Haskell
--
---------------------------------------------------------------------------------

module IDE.Leksah (
    leksah
) where

import Prelude ()
import Prelude.Compat
import Control.Concurrent
       (tryTakeMVar, tryPutMVar, takeMVar, forkIO, yield,
        setNumCapabilities, rtsSupportsBoundThreads, newMVar, newEmptyMVar)
import Data.IORef (readIORef, newIORef, IORef)
import Data.Maybe (mapMaybe)
import System.Console.GetOpt
       (usageInfo, ArgOrder(..), getOpt, ArgDescr(..), OptDescr(..))
import Data.Version (showVersion)
import Control.Lens ((.~))

import qualified IDE.OSX as OSX
import qualified IDE.TextEditor.Yi.Config as Yi

#ifdef LEKSAH_WITH_YI_DYRE
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import qualified Config.Dyre as Dyre
#endif

import IDE.Session (recoverSession)
import IDE.Core.Types
       (SensitivityMask(..), IDEEvent(..), IDEState(..))
import IDE.Core.State
       (endWithLastConn, serverPort, prefs, readIDE,
        currentState, modifyIDE_, triggerEventIDE_, defaultSize, makeMode,
        runBenchmarks, runUnitTests, makeDocs, debug, javaScript, native,
        backgroundBuild, reflectIDE, keymapName, sourceCandy, version,
        MessageLevel(..), sysMessage, Prefs, IDE(..), KeymapI,
        sourceDirectories, unpackDirectory, retrieveURL, retrieveStrategy,
        getDataDir, standardPreferencesFilename,
        leksahSessionFileExtension, leksahWorkspaceFileExtension,
        standardSessionFilename, strippedPreferencesFilename, leksahKeymapFileExtension,
        leksahCandyFileExtension, emptySessionFilename)
import IDE.Gtk.State
       (windows, FrameState(..), IDEGtk(..), postSyncIDE',
        getWindows, handleNotebookSwitch, newNotebook,
        PaneLayout(..))
import Control.Event (EventSource(..), triggerEvent)
import IDE.SourceCandy
import IDE.Utils.FileUtils
import Graphics.UI.Editor.MakeEditor
import Graphics.UI.Editor.Parameters
import IDE.Command
import IDE.Preferences
import IDE.Keymap
import IDE.Pane.SourceBuffer
import IDE.Find
import Graphics.UI.Editor.Composite (filesEditor, maybeEditor)
import Graphics.UI.Editor.Simple
       (stringEditor, enumEditor, textEditor)
import IDE.Metainfo.Provider (initInfo)
import IDE.Workspaces
       (projectTryQuiet, projectAddPackage', workspaceTryQuiet,
        backgroundMake, projectOpenThis)
import IDE.Gtk.Workspaces (workspaceNewHere, workspaceOpenThis)
import IDE.Utils.GUIUtils
import Network (withSocketsDo)
import Control.Exception
import System.Exit (ExitCode(..), exitWith, exitFailure)
import qualified IDE.StrippedPrefs as SP
import System.Log
import System.Log.Logger
       (debugM, updateGlobalLogger,
        rootLoggerName, setLevel)
import System.Directory
       (doesDirectoryExist,
        createDirectoryIfMissing, getHomeDirectory, doesFileExist)
import System.FilePath (dropExtension, splitExtension, (</>))
import Control.Monad (forever, forM_, void, when, unless)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Applicative ((<$>))
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as T (readFile, writeFile)
import Data.Text (Text)
import GI.GLib.Functions (timeoutAdd)
import GI.GLib.Constants
       (pattern PRIORITY_LOW)
import GI.Gdk.Objects.Screen
       (screenGetDefault, screenSetResolution)
import GI.Gtk.Objects.CssProvider
       (cssProviderLoadFromData, cssProviderNew)
import GI.Gtk.Objects.StyleContext
       (styleContextAddProviderForScreen)
import qualified Data.ByteString.Char8 as B (unlines)
import GI.GLib.Structs.Source (sourceRemove)
import GI.Gtk.Functions (eventsPending)
import GI.Gtk.Objects.Window
       (windowSetIconFromFile, IsWindow, toWindow,
        setWindowWindowPosition, setWindowTitle, windowSetDefaultSize)
import GI.Gtk.Objects.Widget
       (widgetDestroy, widgetHide, widgetShowAll, widgetGetWindow,
        onWidgetRealize, widgetSetName)
import GI.Gtk.Objects.UIManager (uIManagerNew)
import GI.Gtk.Objects.Notebook (afterNotebookSwitchPage)
import GI.Gtk.Enums
       (Orientation(..), PolicyType(..), ResponseType(..),
        WindowPosition(..), FileChooserAction(..))
import GI.Gtk.Objects.Dialog
       (dialogGetContentArea, dialogNew)
import Data.GI.Base (unsafeCastTo)
import GI.Gtk.Objects.Label (labelNew)
import GI.Gtk.Objects.Box (Box(..))
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk
       (containerAdd, applicationNew, Application, applicationWindowNew)
import GI.Gio (applicationRun, onApplicationActivate)
import System.FSNotify (withManager, WatchManager)
import Criterion.Measurement (initializeTime)
-- import qualified IDE.Web.Main as Web (startJSaddle)

-- --------------------------------------------------------------------
-- Command line options
--

data Flag =  VersionF | SessionN Text | EmptySession | DefaultSession | Help | Verbosity Text | DevelopLeksah | WebOnly
       deriving (Show,Eq)

options :: [OptDescr Flag]
options =   [Option "e" ["emptySession"] (NoArg EmptySession)
                "Start with empty session"
         ,   Option "d" ["defaultSession"] (NoArg DefaultSession)
                "Start with default session (can be used together with a source file)"
         ,   Option "l" ["loadSession"] (ReqArg (SessionN . T.pack) "NAME")
                "Load session"

         ,   Option "" ["develop-leksah"] (NoArg DevelopLeksah)
                "Exit Leksah when the 'leksah' package is rebuilt."

         ,   Option "w" ["web"] (NoArg WebOnly)
                "Only show the Web UI."

         ,   Option "h" ["help"] (NoArg Help)
                "Display command line options"
         ,   Option "v" ["version"] (NoArg VersionF)
                "Show the version number of ide"

         ,   Option "e" ["verbosity"] (ReqArg (Verbosity . T.pack) "Verbosity")
             "One of DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY"]


header :: String
header = "Usage: leksah [OPTION...] [file(.lkshs|.lkshw|.hs|.lhs)]"

ideOpts :: [Text] -> IO ([Flag], [Text])
ideOpts argv =
    case getOpt Permute options $ map T.unpack argv of
          (o,n,[]  ) -> return (o,map T.pack n)
          (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options

-- ---------------------------------------------------------------------
-- | Main function
--

#ifdef LEKSAH_WITH_YI_DYRE
leksahDriver args = Dyre.wrapMain Dyre.defaultParams
    { Dyre.projectName  = "yi"
    , Dyre.realMain     = \(config, mbError) -> do
        case mbError of
            Just error -> putStrLn $ "Error in yi configuration file : " ++ error
            Nothing    -> return ()
        realMain config args
    , Dyre.showError    = \(config, _) error -> (config, Just error)
    , Dyre.configDir    = Just . getAppUserDataDirectory $ "yi"
    , Dyre.cacheDir     = Just $ ((</> "leksah") <$> (getAppUserDataDirectory "cache"))
    , Dyre.hidePackages = ["mtl"]
    , Dyre.ghcOpts = ["-DLEKSAH"]
    }

leksah yiConfig args = leksahDriver args (yiConfig, Nothing)
#else
leksah :: Yi.Config -> [String] -> IO b
leksah = realMain
#endif

realMain :: Yi.Config -> [String] -> IO b
realMain yiConfig args = do
  initializeTime
  exitCode <- newIORef ExitSuccess
  withSocketsDo $ handleExceptions $ do
    dataDir         <- getDataDir

    (flags,files)       <-  ideOpts $ map T.pack args
    isFirstStart    <-  not <$> hasSavedConfigFile standardPreferencesFilename
    let sessions      =   mapMaybe (\case
                                        SessionN s -> Just s
                                        _          -> Nothing) flags

--    let sessionFPs    =   filter (\f -> snd (splitExtension f) == leksahSessionFileExtension) $ map T.unpack files
    let workspaceFPs  =   filter (\f -> snd (splitExtension f) == leksahWorkspaceFileExtension) $ map T.unpack files
    let sourceFPs     =   filter (\f -> let (_,s) = splitExtension f
                                        in s == ".hs" ||  s == ".lhs" || s == ".chs") $ map T.unpack files
    let mbWorkspaceFP'=  case workspaceFPs of
                                [] ->  Nothing
                                w:_ -> Just w
    (mbWSessionFP, mbWorkspaceFP) <-
        case mbWorkspaceFP' of
            Nothing ->  return (Nothing,Nothing)
            Just fp ->  let spath =  dropExtension fp ++ leksahSessionFileExtension
                        in do
                            exists <- liftIO $ doesFileExist spath
                            if exists
                                then return (Just spath,Nothing)
                                else return (Nothing,Just fp)
    let ssession =  case sessions of
                        (s:_) -> T.unpack s <> leksahSessionFileExtension
                        _     -> if null sourceFPs
                                        then standardSessionFilename
                                        else emptySessionFilename

    sessionFP    <-  if  EmptySession `elem` flags
                                then getConfigFilePathForLoad
                                                        emptySessionFilename Nothing dataDir
                                else if DefaultSession `elem` flags
                                        then getConfigFilePathForLoad
                                                        standardSessionFilename Nothing dataDir
                                        else case mbWSessionFP of
                                                Just fp -> return fp
                                                Nothing -> getConfigFilePathForLoad
                                                                    ssession Nothing dataDir
    let verbosity'      =  mapMaybe (\case
                                        Verbosity s -> Just s
                                        _           -> Nothing) flags
        verbosity       =  case verbosity' of
                               [] -> INFO
                               h:_ -> read $ T.unpack h
        developLeksah = DevelopLeksah `elem` flags
        webOnly = WebOnly `elem` flags
    updateGlobalLogger rootLoggerName (setLevel verbosity)
    when (VersionF `elem` flags)
        (sysMessage Normal $ "Leksah the Haskell IDE, version " <> T.pack (showVersion version))
    when (Help `elem` flags)
        (sysMessage Normal $ "Leksah the Haskell IDE " <> T.pack (usageInfo header options))

    prefsPath       <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
    prefs'          <- readPrefs prefsPath
    when (notElem VersionF flags && notElem Help flags) $
        if webOnly
            then withManager $ \fsnotify -> Yi.start yiConfig $ \yiControl -> do
                candyPath   <-  getConfigFilePathForLoad
                                    (case sourceCandy prefs' of
                                        (_,name)   ->   T.unpack name <> leksahCandyFileExtension) Nothing dataDir
                candySt     <-  parseCandy candyPath

                triggerBuild <- newEmptyMVar
                nixCache <- loadNixCache
                externalModified <- newMVar mempty
                watchers <- newMVar (mempty, mempty)
                let ide = IDE
                      {   _ideGtk            =   Nothing
                      ,   _exitCode          =   exitCode
                      ,   _candy             =   candySt
                      ,   _prefs             =   prefs'
                      ,   _workspace         =   Nothing
                      ,   _bufferProjCache   =   mempty
                      ,   _allLogRefs        =   mempty
                      ,   _currentHist       =   0
                      ,   _currentEBC        =   (Nothing, Nothing, Nothing)
                      ,   _systemInfo        =   Nothing
                      ,   _packageInfo       =   Nothing
                      ,   _workspaceInfo     =   Nothing
                      ,   _workspInfoCache   =   mempty
                      ,   _handlers          =   mempty
                      ,   _currentState      =   IsStartingUp
                      ,   _recentFiles       =   []
                      ,   _recentWorkspaces  =   []
                      ,   _runningTool       =   Nothing
                      ,   _debugState        =   []
                      ,   _yiControl         =   yiControl
                      ,   _serverQueue       =   Nothing
                      ,   _server            =   Nothing
                      ,   _hlintQueue        =   Nothing
                      ,   _logLaunches       =   mempty
                      ,   _autoCommand       =   (("", ""), return ())
                      ,   _autoURI           =   Nothing
                      ,   _triggerBuild      =   triggerBuild
                      ,   _fsnotify          =   fsnotify
                      ,   _watchers          =   watchers
                      ,   _developLeksah     =   developLeksah
                      ,   _nixCache          =   nixCache
                      ,   _externalModified  =   externalModified
                      ,   _jsContexts        =   []
                      ,   _logLineMap        =   mempty
                }
                ideR <- liftIO $ newMVar (const (return ()), ide)
                liftIO $ (`reflectIDE` ideR) $
                    forM_ mbWorkspaceFP' (workspaceOpenThis False)
--                Web.startJSaddle 3367 ideR
            else
                startGUI exitCode developLeksah yiConfig sessionFP mbWorkspaceFP sourceFPs prefs' isFirstStart
  readIORef exitCode >>= exitWith

handleExceptions :: IO a -> IO a
handleExceptions inner =
  catch inner (\(exception :: SomeException) -> do
    sysMessage Normal ("leksah: internal IDE error: " <> T.pack (show exception))
    exitFailure
  )

-- ---------------------------------------------------------------------
-- | Start the GUI

startGUI :: IORef ExitCode -> Bool -> Yi.Config -> FilePath -> Maybe FilePath -> [FilePath] -> Prefs -> Bool -> IO ()
startGUI exitCode developLeksah yiConfig sessionFP mbWorkspaceFP sourceFPs iprefs isFirstStart =
  withManager $ \fsnotify -> Yi.start yiConfig $ \yiControl -> do
    Just app <- applicationNew (Just "org.leksah.leksah") []
    _ <- onApplicationActivate app $ do
        debugM "leksah" "Application Activate"
        timeout  <- if rtsSupportsBoundThreads
                        then do
                            setNumCapabilities 2
                            sysMessage Normal "Linked with -threaded"
                            return Nothing
                        else Just <$> timeoutAdd PRIORITY_LOW 10 (yield >> return True)
        screenGetDefault >>= \case
            Nothing -> return ()
            Just screen -> do
#if defined(darwin_HOST_OS)
                screenSetResolution screen 72
#endif
                debugM "leksah" "Add CSS"
                provider <- cssProviderNew
                cssProviderLoadFromData provider $
                    B.unlines [ ".window-frame,"
                              , ".window-frame:backdrop {"
                              , "  box-shadow: none;"
                              , "  margin: 0;}"
                              , "#errorLabel {"
                              , "  padding: 10px;"
                              , "  background: #F2DEDE;"
                              , "  color: #A94442;"
                              , "  border: 1px solid #EBCCD1;"
                              , "  border-radius: 5px;}"
                              ]
                styleContextAddProviderForScreen screen provider 600
        dataDir       <- getDataDir
        mbStartupPrefs <- if not isFirstStart
                                    then return $ Just iprefs
                                    else do
                                        firstStartOK <- firstStart
                                        if not firstStartOK
                                            then return Nothing
                                            else do
                                                prefsPath  <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
                                                prefs' <- readPrefs prefsPath
                                                return $ Just prefs'
        maybe (return ()) (void . sourceRemove) timeout
        case mbStartupPrefs of
            Nothing           -> return ()
            Just startupPrefs ->
                startMainWindow exitCode developLeksah app yiControl fsnotify sessionFP mbWorkspaceFP sourceFPs
                                startupPrefs isFirstStart
    debugM "leksah" "starting applicationRun"
    _ <- applicationRun app Nothing
    debugM "leksah" "finished applicationRun"

startMainWindow :: IORef ExitCode -> Bool -> Application -> Yi.Control -> WatchManager -> FilePath -> Maybe FilePath -> [FilePath] ->
                        Prefs -> Bool -> IO ()
startMainWindow exitCode developLeksah app yiControl fsnotify sessionFP mbWorkspaceFP sourceFPs startupPrefs _isFirstStart = do
    timeout  <- if rtsSupportsBoundThreads
                    then return Nothing
                    else Just <$> timeoutAdd PRIORITY_LOW 10 (yield >> return True)
    debugM "leksah" "startMainWindow"
    osxApp <- OSX.applicationNew
    uiManager'  <-  uIManagerNew
    newIcons
    dataDir     <- getDataDir
    candyPath   <-  getConfigFilePathForLoad
                        (case sourceCandy startupPrefs of
                            (_,name)   ->   T.unpack name <> leksahCandyFileExtension) Nothing dataDir
    candySt     <-  parseCandy candyPath
    -- keystrokes
    keysPath    <-  getConfigFilePathForLoad (T.unpack (keymapName startupPrefs) <> leksahKeymapFileExtension) Nothing dataDir
    keyMap      <-  parseKeymap keysPath
    let accelActions = setKeymap (keyMap :: KeymapI) mkActions
    specialKeys <-  buildSpecialKeys keyMap accelActions

    win         <-  toWindow =<< applicationWindowNew app
    widgetSetName win "Leksah Main Window"
    let fs = FrameState
            {   windows       =   [win]
            ,   uiManager     =   uiManager'
            ,   panes         =   mempty
            ,   activePane    =   (Nothing, [])
            ,   paneMap       =   mempty
            ,   layout        =   TerminalP mempty Nothing (-1) Nothing Nothing
            ,   panePathFromNB =  mempty
            }

    triggerBuild <- newEmptyMVar
    nixCache <- loadNixCache
    externalModified <- newMVar mempty
    watchers <- newMVar (mempty, mempty)
    let ide = IDE
          {   _ideGtk = Just IDEGtk
            {   _application       =   app
            ,   _frameState        =   fs
            ,   _flipper           =   Nothing
            ,   _typeTip           =   Nothing
            ,   _guiHistory        =   (False,[],-1)
            ,   _findbar           =   (False,Nothing)
            ,   _toolbar           =   (True,Nothing)
            ,   _specialKeys       =   specialKeys
            ,   _specialKey        =   Nothing
            ,   _completion        =   ((750,400),Nothing)
            ,   _vcsData           =   (mempty, Nothing)
            }
          ,   _exitCode          =   exitCode
          ,   _candy             =   candySt
          ,   _prefs             =   startupPrefs
          ,   _workspace         =   Nothing
          ,   _bufferProjCache   =   mempty
          ,   _allLogRefs        =   mempty
          ,   _currentHist       =   0
          ,   _currentEBC        =   (Nothing, Nothing, Nothing)
          ,   _systemInfo        =   Nothing
          ,   _packageInfo       =   Nothing
          ,   _workspaceInfo     =   Nothing
          ,   _workspInfoCache   =   mempty
          ,   _handlers          =   mempty
          ,   _currentState      =   IsStartingUp
          ,   _recentFiles       =   []
          ,   _recentWorkspaces  =   []
          ,   _runningTool       =   Nothing
          ,   _debugState        =   []
          ,   _yiControl         =   yiControl
          ,   _serverQueue       =   Nothing
          ,   _server            =   Nothing
          ,   _hlintQueue        =   Nothing
          ,   _logLaunches       =   mempty
          ,   _autoCommand       =   (("", ""), return ())
          ,   _autoURI           =   Nothing
          ,   _triggerBuild      =   triggerBuild
          ,   _fsnotify          =   fsnotify
          ,   _watchers          =   watchers
          ,   _developLeksah     =   developLeksah
          ,   _nixCache          =   nixCache
          ,   _externalModified  =   externalModified
          ,   _jsContexts        =   []
          ,   _logLineMap        =   mempty
    }
    ideR <- liftIO $ newMVar (const (return ()), ide)
    liftIO $ (`reflectIDE` ideR) $ do
        menuDescription' <- liftIO menuDescription
        makeMenu uiManager' accelActions menuDescription'
        nb               <-  newNotebook []
        _ <- afterNotebookSwitchPage nb (\_ i -> reflectIDE (handleNotebookSwitch nb (fromIntegral i)) ideR)
        widgetSetName nb "root"
        instrumentWindow win startupPrefs nb
        setBackgroundBuildToggled (backgroundBuild startupPrefs)
        setNativeToggled          (native          startupPrefs)
        setJavaScriptToggled      (javaScript      startupPrefs)
        setDebugToggled           (debug           startupPrefs)
        setMakeDocs               (makeDocs        startupPrefs)
        setRunUnitTests           (runUnitTests    startupPrefs)
        setRunBenchmarks          (runBenchmarks   startupPrefs)
        setMakeModeToggled        (makeMode        startupPrefs)
        let (x,y)   =   defaultSize startupPrefs
        windowSetDefaultSize win (fromIntegral x) (fromIntegral y)
        registerLeksahEvents
        (tbv,fbv) <- recoverSession sessionFP
        forM_ mbWorkspaceFP (workspaceOpenThis False)
        mapM_ fileOpenThis sourceFPs
        wins <- getWindows
        mapM_ instrumentSecWindow (tail wins)

        _ <- onWidgetRealize win $
            widgetGetWindow win >>= mapM_ OSX.allowFullscreen

        liftIO $ debugM "leksah" "Show main window"
        widgetShowAll win

        triggerEventIDE_ UpdateRecent
        if tbv
            then showToolbar
            else hideToolbar
        if fbv
            then showFindbar
            else hideFindbar
        OSX.updateMenu osxApp uiManager'

        OSX.applicationReady osxApp

        configDir <- liftIO getConfigDir
        let welcomePath  = configDir</>"leksah-welcome"
        welcomeExists <- liftIO $ doesDirectoryExist welcomePath
        unless welcomeExists $ do
            let welcomeSource = dataDir</>"data"</>"leksah-welcome"
                welcomeProject = welcomePath</>"cabal.project"
                welcomeCabal = welcomePath</>"leksah-welcome.cabal"
                welcomeMain  = welcomePath</>"src"</>"Main.hs"
                newFile src dest = T.readFile src >>= T.writeFile dest
            liftIO $ do
                createDirectoryIfMissing True $ welcomePath</>"src"
                createDirectoryIfMissing True $ welcomePath</>"test"
                newFile (welcomeSource</>"Setup.lhs")            (welcomePath</>"Setup.lhs")
                newFile (welcomeSource</>"cabal.project")        welcomeProject
                newFile (welcomeSource</>"leksah-welcome.cabal") welcomeCabal
                newFile (welcomeSource</>"LICENSE")              (welcomePath</>"LICENSE")
                newFile (welcomeSource</>"src"</>"Main.hs")      welcomeMain
                newFile (welcomeSource</>"test"</>"Main.hs")     (welcomePath</>"test"</>"Main.hs")
            defaultWorkspace <- liftIO $ (</> "leksah.lkshw") <$> getHomeDirectory
            defaultExists <- liftIO $ doesFileExist defaultWorkspace
            if defaultExists
                then workspaceOpenThis False defaultWorkspace
                else workspaceNewHere defaultWorkspace
            workspaceTryQuiet $ projectOpenThis welcomeProject
            projectTryQuiet $ void (projectAddPackage' welcomeCabal)
            fileOpenThis welcomeMain
        initInfo (modifyIDE_ $ currentState .~ IsRunning)
        mapM_ sourceRemove timeout
        _ <- liftIO . forkIO . forever $ do
            takeMVar triggerBuild
            reflectIDE (postSyncIDE' PRIORITY_LOW $
                eventsPending >>= \case
                    True ->
                        liftIO . void $ tryPutMVar triggerBuild ()
                    False -> do
                        _ <- liftIO $ tryTakeMVar triggerBuild
                        currentPrefs <- readIDE prefs
                        when (backgroundBuild currentPrefs) backgroundMake) ideR

        _ <- triggerEvent ideR (Sensitivity [(SensitivityInterpreting, False)])
        return ()
--    _ <- liftIO $ Web.startJSaddle ideR 3366
    return ()

fDescription :: FilePath -> FieldDescription Prefs
fDescription _configPath = VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Paths under which haskell sources for packages may be found"
                $ paraOrientation  <<<- ParaOrientation OrientationVertical
                    $ paraMinSize <<<- ParaMinSize (-1, 150)
                        $ emptyParams)
            sourceDirectories
            (\b a -> a{sourceDirectories = b})
            (filesEditor Nothing FileChooserActionSelectFolder "Select folders")
    ,   mkField
            (paraName <<<- ParaName "Unpack source for cabal packages to" $ emptyParams)
            unpackDirectory
            (\b a -> a{unpackDirectory = b})
            (maybeEditor "" (stringEditor (const True) True,emptyParams) True "")
    ,   mkField
            (paraName <<<- ParaName "URL from which to download prebuilt metadata" $ emptyParams)
            retrieveURL
            (\b a -> a{retrieveURL = b})
            (textEditor (const True) True)
    ,   mkField
            (paraName <<<- ParaName "Strategy for downloading prebuilt metadata" $ emptyParams)
            retrieveStrategy
            (\b a -> a{retrieveStrategy = b})
            (enumEditor ["Try to download and then build locally if that fails","Try to build locally and then download if that fails","Never download (just try to build locally)"])]

--
-- | Called when leksah is first called (the .leksah-xx directory does not exist)
--
firstStart' :: Prefs -> IO (Maybe Prefs)
firstStart' prefs' = do
    configDir   <- getConfigDir
    dialog      <- dialogNew
    setLeksahIcon dialog
    setWindowTitle dialog "Welcome to Leksah, the Haskell IDE"
    setWindowWindowPosition dialog WindowPositionCenter
    _ <- dialogAddButton' dialog "gtk-ok" ResponseTypeOk
    _ <- dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
    vb          <- dialogGetContentArea dialog >>= liftIO . unsafeCastTo Box
    label       <- labelNew (Just (
        "Before Leksah can provide features like autocompletion and jump to source definition " <>
        "it needs to collect and download metadata about the Haskell packages that are being used.\n" <>
        "You can add folders under which you have sources for Haskell packages not available from Hackage.\n" <>
        "You do not need to change anything in this dialog if you are happy for Leksah to use its" <>
        "default metadata collection settings"))
    (widget, _setInj, getExt, _notifier) <- buildEditor (fDescription configDir) prefs'
    boxPackStart' vb label PackNatural 7
    sw <- scrolledWindowNew noAdjustment noAdjustment
    containerAdd sw widget
    scrolledWindowSetPolicy sw PolicyTypeNever PolicyTypeAutomatic
    boxPackStart' vb sw PackGrow 7
    windowSetDefaultSize dialog 800 630
    widgetShowAll dialog
    response <- dialogRun' dialog
    widgetHide dialog
    case response of
        ResponseTypeOk -> do
            mbNewPrefs <- liftIO $ extract prefs' [getExt]
            widgetDestroy dialog
            return mbNewPrefs
        _ -> do
            widgetDestroy dialog
            return Nothing

--
-- | Called when leksah is first called (the .leksah-xx directory does not exist)
--
firstStart :: IO Bool
firstStart = do
    dataDir     <- getDataDir
    prefsPath   <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
    prefs'      <- readPrefs prefsPath
    -- configDir   <- getConfigDir
    dialog      <- dialogNew
    setLeksahIcon dialog
    setWindowTitle dialog "Welcome to Leksah, the Haskell IDE"
    setWindowWindowPosition dialog WindowPositionCenter
    _ <- dialogAddButton' dialog (__ "_Customize metadata collection") (AnotherResponseType 1)
    _ <- dialogAddButton' dialog (__ "Use _defaults") (AnotherResponseType 2)
    vb          <- dialogGetContentArea dialog >>= liftIO . unsafeCastTo Box
    label       <- labelNew . Just $
        "Before Leksah can provide features like autocompletion \n" <>
        "and jump to source definition it needs to collect and \n" <>
        "download metadata about the Haskell packages that are \n" <>
        "being used."
    boxPackStart' vb label PackNatural 7
    widgetShowAll dialog
    response <- dialogRun' dialog
    widgetHide dialog
    widgetDestroy dialog
    mbNewPrefs <- case response of
        AnotherResponseType 1 -> firstStart' prefs'
        AnotherResponseType 2 -> return $ Just prefs'
        _ -> return Nothing
    case mbNewPrefs of
        Nothing -> return False
        Just newPrefs -> do
            fp <- liftIO $ getConfigFilePathForSave standardPreferencesFilename
            liftIO $ writePrefs fp newPrefs
            fp2  <-  liftIO $ getConfigFilePathForSave strippedPreferencesFilename
            liftIO $ SP.writeStrippedPrefs fp2
                    SP.Prefs {SP.sourceDirectories = sourceDirectories newPrefs,
                               SP.unpackDirectory   = unpackDirectory newPrefs,
                               SP.retrieveURL       = retrieveURL newPrefs,
                               SP.retrieveStrategy  = retrieveStrategy newPrefs,
                               SP.serverPort        = serverPort newPrefs,
                               SP.endWithLastConn   = endWithLastConn newPrefs}
--            firstBuild
            return True

setLeksahIcon :: (IsWindow self) => self -> IO ()
setLeksahIcon window = do
    dataDir <- getDataDir
    let iconPath = dataDir </> "pics" </> "leksah.png"
    iconExists  <-  doesFileExist iconPath
    when iconExists $
        windowSetIconFromFile window iconPath

--firstBuild :: IO ()
--firstBuild = do
--    dialog      <- dialogNew
--    liftIO $ setLeksahIcon dialog
--    setWindowTitle dialog "Leksah: Updating Metadata"
--    setWindowWindowPosition dialog WindowPositionCenter
--    setWindowDeletable dialog False
--    _ <- dialogAddButton' dialog (__ "Continue in background") (AnotherResponseType 1)
--    vb          <- dialogGetContentArea dialog >>= liftIO . unsafeCastTo Box
--    label       <- labelNew . Just $
--        "Collecting metadata about the Haskell \n" <>
--        "packages that are being used."
--    boxPackStart' vb label PackGrow 7
--    progressBar <- progressBarNew
--    progressBarSetText progressBar $ Just "Collecting information about Haskell packages"
--    progressBarSetFraction progressBar 0.0
--    boxPackStart' vb progressBar PackGrow 7
--    done <- newEmptyMVar
--    _ <- forkIO $ do
--        logger <- getRootLogger
--        let verbosity = case getLevel logger of
--                            Just level -> ["--verbosity=" <> T.pack (show level)]
--                            Nothing    -> []
--        (output, pid) <- runTool "leksah-server" (["-sbo", "+RTS", "-N2", "-RTS"] ++ verbosity) Nothing Nothing
--        output $$ CL.mapM_ (update progressBar)
--        _ <- waitForProcess pid
--        tryTakeMVar done >>= \case
--            Nothing ->
--                void . idleAdd PRIORITY_DEFAULT $ do
--                    dialogResponse' dialog ResponseTypeOk
--                    return False
--            _ -> return ()
--    widgetShowAll dialog
--    _ <- dialogRun dialog
--    putMVar done ()
--    widgetHide dialog
--    return ()
--    where
--        update pb to = do
--            let str = toolline to
--            case T.stripPrefix "update_toolbar " str of
--                Just rest -> void . idleAdd PRIORITY_DEFAULT $ do
--                    progressBarSetFraction pb (read $ T.unpack rest)
--                    return False
--                Nothing   -> debugM "leksah" $ T.unpack str



