{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings, LambdaCase, PatternSynonyms #-}
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
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map
import System.Console.GetOpt
import System.Environment
import Data.Version

import qualified IDE.OSX as OSX
import qualified IDE.TextEditor.Yi.Config as Yi

#ifdef LEKSAH_WITH_YI_DYRE
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import qualified Config.Dyre as Dyre
#endif

import IDE.Session
import IDE.Core.State
import Control.Event
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
       (workspaceAddPackage', workspaceTryQuiet, workspaceNewHere,
        workspaceOpenThis, backgroundMake)
import IDE.Utils.GUIUtils
import Network (withSocketsDo)
import Control.Exception
import System.Exit(exitFailure)
import qualified IDE.StrippedPrefs as SP
import IDE.Utils.Tool (runTool, toolline, waitForProcess)
import System.Log
import System.Log.Logger
       (getLevel, getRootLogger, debugM, updateGlobalLogger,
        rootLoggerName, setLevel)
import Data.List (stripPrefix)
import System.Directory
       (doesDirectoryExist, copyFile, createDirectoryIfMissing,
        getHomeDirectory, doesFileExist)
import System.FilePath (dropExtension, splitExtension, (</>))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Conduit (($$))
import Control.Monad (forM_, void, when, unless, liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Applicative ((<$>))
import qualified Data.Text as T (pack, unpack, stripPrefix, unlines)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Sequence as Seq (empty)
import qualified GI.Gtk.Functions as Gtk (main, init)
import GI.GLib.Functions (idleAdd, timeoutAdd)
import GI.GLib.Constants (pattern PRIORITY_DEFAULT, pattern PRIORITY_LOW)
import GI.Gdk.Objects.Screen (screenGetDefault)
import GI.Gtk.Objects.CssProvider
       (cssProviderLoadFromData, cssProviderNew)
import GI.Gtk.Objects.StyleContext
       (styleContextAddProviderForScreen)
import qualified Data.ByteString.Char8 as B (unlines)
import GI.GLib.Structs.Source (sourceRemove)
import GI.Gtk.Functions
       (mainIterationDo, eventsPending, mainIteration)
import GI.Gtk.Objects.Window
       (setWindowDeletable, windowSetIconFromFile, IsWindow,
        setWindowWindowPosition, setWindowTitle, windowSetDefaultSize, windowNew)
import GI.Gtk.Objects.Widget
       (widgetDestroy, widgetHide, widgetShowAll, widgetGetWindow,
        onWidgetRealize, onWidgetDeleteEvent, widgetSetName)
import GI.Gtk.Objects.UIManager (uIManagerNew)
import GI.Gtk.Objects.Notebook (afterNotebookSwitchPage)
import GI.Gtk.Enums
       (WindowType(..), PolicyType(..), ResponseType(..),
        WindowPosition(..), FileChooserAction(..))
import GI.Gtk.Objects.Dialog
       (dialogRun, dialogResponse, dialogGetContentArea, dialogNew)
import Data.GI.Base (unsafeCastTo, set)
import GI.Gtk.Objects.Label (labelNew)
import GI.Gtk.Objects.Box (Box(..))
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowAddWithViewport,
        scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import Control.Monad.Reader (MonadReader(..))
import GI.Gtk.Objects.ProgressBar
       (progressBarSetFraction, progressBarSetText, progressBarNew)
import GI.Gtk.Objects.CssProvider (CssProvider(..))

-- --------------------------------------------------------------------
-- Command line options
--

data Flag =  VersionF | SessionN Text | EmptySession | DefaultSession | Help | Verbosity Text
       deriving (Show,Eq)

options :: [OptDescr Flag]
options =   [Option "e" ["emptySession"] (NoArg EmptySession)
                "Start with empty session"
         ,   Option "d" ["defaultSession"] (NoArg DefaultSession)
                "Start with default session (can be used together with a source file)"
         ,   Option "l" ["loadSession"] (ReqArg (SessionN . T.pack) "NAME")
                "Load session"

         ,   Option "h" ["help"] (NoArg Help)
                "Display command line options"
         ,   Option "v" ["version"] (NoArg VersionF)
                "Show the version number of ide"

         ,   Option "e" ["verbosity"] (ReqArg (Verbosity . T.pack) "Verbosity")
             "One of DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY"]


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
leksahDriver = Dyre.wrapMain Dyre.defaultParams
    { Dyre.projectName  = "yi"
    , Dyre.realMain     = \(config, mbError) -> do
        case mbError of
            Just error -> putStrLn $ "Error in yi configuration file : " ++ error
            Nothing    -> return ()
        realMain config
    , Dyre.showError    = \(config, _) error -> (config, Just error)
    , Dyre.configDir    = Just . getAppUserDataDirectory $ "yi"
    , Dyre.cacheDir     = Just $ ((</> "leksah") <$> (getAppUserDataDirectory "cache"))
    , Dyre.hidePackages = ["mtl"]
    , Dyre.ghcOpts = ["-DLEKSAH"]
    }

leksah yiConfig = leksahDriver (yiConfig, Nothing)
#else
leksah = realMain
#endif

realMain yiConfig =
  withSocketsDo $ handleExceptions $ do
    dataDir         <- getDataDir
    args            <-  getArgs

    (flags,files)       <-  ideOpts $ map T.pack args
    isFirstStart    <-  liftM not $ hasSavedConfigFile standardPreferencesFilename
    let sessions      =   mapMaybe (\case
                                        SessionN s -> Just s
                                        _          -> Nothing) flags

    let sessionFPs    =   filter (\f -> snd (splitExtension f) == leksahSessionFileExtension) $ map T.unpack files
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
    let verbosity       =  case verbosity' of
                               [] -> INFO
                               h:_ -> read $ T.unpack h
    updateGlobalLogger rootLoggerName (setLevel verbosity)
    when (VersionF `elem` flags)
        (sysMessage Normal $ "Leksah the Haskell IDE, version " <> T.pack (showVersion version))
    when (Help `elem` flags)
        (sysMessage Normal $ "Leksah the Haskell IDE " <> T.pack (usageInfo header options))

    prefsPath       <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
    prefs           <- readPrefs prefsPath
    when (notElem VersionF flags && notElem Help flags)
        (startGUI yiConfig sessionFP mbWorkspaceFP sourceFPs  prefs isFirstStart)

handleExceptions inner =
  catch inner (\(exception :: SomeException) -> do
    sysMessage Normal ("leksah: internal IDE error: " <> T.pack (show exception))
    exitFailure
  )

-- ---------------------------------------------------------------------
-- | Start the GUI

startGUI :: Yi.Config -> FilePath -> Maybe FilePath -> [FilePath] -> Prefs -> Bool -> IO ()
startGUI yiConfig sessionFP mbWorkspaceFP sourceFPs iprefs isFirstStart =
  Yi.start yiConfig $ \yiControl -> do
    st       <- Gtk.init Nothing
    timeout  <- if rtsSupportsBoundThreads
                    then do
                        setNumCapabilities 2
                        sysMessage Normal "Linked with -threaded"
                        return Nothing
                    else Just <$> timeoutAdd PRIORITY_LOW 10 (yield >> return True)
    screenGetDefault >>= \case
        Nothing -> return ()
        Just screen -> do
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
                                    firstStartOK <- firstStart iprefs
                                    if not firstStartOK
                                        then return Nothing
                                        else do
                                            prefsPath  <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
                                            prefs <- readPrefs prefsPath
                                            return $ Just prefs
    maybe (return ()) (void . sourceRemove) timeout
    case mbStartupPrefs of
        Nothing           -> return ()
        Just startupPrefs ->
            void . idleAdd PRIORITY_DEFAULT $ do
                startMainWindow yiControl sessionFP mbWorkspaceFP sourceFPs
                                startupPrefs isFirstStart

                return False
    debugM "leksah" "starting mainGUI"
    Gtk.main
    debugM "leksah" "finished mainGUI"

mainLoop :: IO () -> IO ()
mainLoop = mainLoopSingleThread

mainLoopThreaded :: IO () -> IO ()
mainLoopThreaded onIdle = loop
    where
        loop = do
            quit <- loopTillIdle
            unless quit $ do
                    active <- newEmptyMVar
                    mvarSentIdleMessage <- newEmptyMVar
                    idleThread <- forkIO $ do
                        threadDelay 200000
                        isActive <- isJust <$> tryTakeMVar active
                        unless isActive $ do
                            putMVar mvarSentIdleMessage ()
                            void . idleAdd PRIORITY_DEFAULT $ onIdle >> return False
                    quit <- mainIteration
                    putMVar active ()
                    unless quit $ do
                            -- If an idle message was sent then wait again
                            sentIdleMessage <- isJust <$> tryTakeMVar mvarSentIdleMessage
                            quit <- if sentIdleMessage
                                then mainIteration
                                else return False
                            unless quit loop
        loopTillIdle =
            eventsPending >>= \case
                False -> return False
                True  -> do
                    quit <- loopn 3
                    if quit
                        then return True
                        else loopTillIdle

mainLoopSingleThread :: IO () -> IO ()
mainLoopSingleThread onIdle = eventsPending >>= loop False 50
  where
    loop :: Bool -> Int -> Bool -> IO ()
    loop False delay False | delay > 2000 = onIdle >> loop True delay False
    loop isIdle delay n = do
        quit <- if n
                    then do
                        timeout <- timeoutAdd PRIORITY_LOW 10 $ yield >> return True
                        quit <- loopn 3
                        sourceRemove timeout
                        return quit
                    else
                        loopn 3
        unless quit $ do
                yield
                pending <- eventsPending
                if pending
                    then loop False 50 pending
                    else do
                        threadDelay delay
                        eventsPending >>= loop isIdle (if n
                                                            then 50
                                                            else min (delay+delay) 50000)

loopn :: Int -> IO Bool
loopn 0 = return False
loopn n = do
    quit <- mainIterationDo False
    if quit
        then return True
        else loopn (n - 1)

startMainWindow :: Yi.Control -> FilePath -> Maybe FilePath -> [FilePath] ->
                        Prefs -> Bool -> IO ()
startMainWindow yiControl sessionFP mbWorkspaceFP sourceFPs startupPrefs isFirstStart = do
    timeout  <- if rtsSupportsBoundThreads
                    then return Nothing
                    else Just <$> timeoutAdd PRIORITY_LOW 10 (yield >> return True)
    debugM "leksah" "startMainWindow"
    osxApp <- OSX.applicationNew
    uiManager   <-  uIManagerNew
    newIcons
    dataDir       <- getDataDir
    candyPath   <-  getConfigFilePathForLoad
                        (case sourceCandy startupPrefs of
                            (_,name)   ->   T.unpack name <> leksahCandyFileExtension) Nothing dataDir
    candySt     <-  parseCandy candyPath
    -- keystrokes
    keysPath    <-  getConfigFilePathForLoad (T.unpack (keymapName startupPrefs) <> leksahKeymapFileExtension) Nothing dataDir
    keyMap      <-  parseKeymap keysPath
    let accelActions = setKeymap (keyMap :: KeymapI) mkActions
    specialKeys <-  buildSpecialKeys keyMap accelActions

    win         <-  windowNew WindowTypeToplevel
    widgetSetName win "Leksah Main Window"
    let fs = FrameState
            {   windows       =   [win]
            ,   uiManager     =   uiManager
            ,   panes         =   Map.empty
            ,   activePane    =   Nothing
            ,   paneMap       =   Map.empty
            ,   layout        =   TerminalP Map.empty Nothing (-1) Nothing Nothing
            ,   panePathFromNB =  Map.empty
            }

    let ide = IDE
          {   frameState        =   fs
          ,   recentPanes       =   []
          ,   specialKeys       =   specialKeys
          ,   specialKey        =   Nothing
          ,   candy             =   candySt
          ,   prefs             =   startupPrefs
          ,   workspace         =   Nothing
          ,   activePack        =   Nothing
          ,   activeExe         =   Nothing
          ,   bufferProjCache   =   Map.empty
          ,   allLogRefs        =   Seq.empty
          ,   currentHist       =   0
          ,   currentEBC        =   (Nothing, Nothing, Nothing)
          ,   systemInfo        =   Nothing
          ,   packageInfo       =   Nothing
          ,   workspaceInfo     =   Nothing
          ,   workspInfoCache   =   Map.empty
          ,   handlers          =   Map.empty
          ,   currentState      =   IsStartingUp
          ,   guiHistory        =   (False,[],-1)
          ,   findbar           =   (False,Nothing)
          ,   toolbar           =   (True,Nothing)
          ,   recentFiles       =   []
          ,   recentWorkspaces  =   []
          ,   runningTool       =   Nothing
          ,   debugState        =   Nothing
          ,   completion        =   ((750,400),Nothing)
          ,   yiControl         =   yiControl
          ,   serverQueue       =   Nothing
          ,   server            =   Nothing
          ,   hlintQueue        =   Nothing
          ,   vcsData           =   (Map.empty, Nothing)
          ,   logLaunches       =   Map.empty
          ,   autoCommand       =   return ()
          ,   autoURI           =   Nothing
    }
    ideR             <-  newIORef ide
    (`reflectIDE` ideR) $ do
        menuDescription' <- liftIO $ menuDescription
        makeMenu uiManager accelActions menuDescription'
        nb               <-  newNotebook []
        afterNotebookSwitchPage nb (\_ i -> reflectIDE (handleNotebookSwitch nb (fromIntegral i)) ideR)
        widgetSetName nb "root"
        onIDE onWidgetDeleteEvent win $ liftIDE quit >> return True
        instrumentWindow win startupPrefs nb
        setBackgroundBuildToggled (backgroundBuild startupPrefs)
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

        onWidgetRealize win $
            widgetGetWindow win >>= mapM_ OSX.allowFullscreen

        liftIO $ debugM "leksah" "Show main window"
        widgetShowAll win

        triggerEventIDE UpdateRecent
        if tbv
            then showToolbar
            else hideToolbar
        if fbv
            then showFindbar
            else hideFindbar
        OSX.updateMenu osxApp uiManager

        OSX.applicationReady osxApp

        configDir <- liftIO getConfigDir
        let welcomePath  = configDir</>"leksah-welcome"
        welcomeExists <- liftIO $ doesDirectoryExist welcomePath
        unless welcomeExists $ do
            let welcomeSource = dataDir</>"data"</>"leksah-welcome"
                welcomeCabal = welcomePath</>"leksah-welcome.cabal"
                welcomeMain  = welcomePath</>"src"</>"Main.hs"
            liftIO $ do
                createDirectoryIfMissing True $ welcomePath</>"src"
                createDirectoryIfMissing True $ welcomePath</>"test"
                copyFile (welcomeSource</>"Setup.lhs")            (welcomePath</>"Setup.lhs")
                copyFile (welcomeSource</>"leksah-welcome.cabal") welcomeCabal
                copyFile (welcomeSource</>"LICENSE")              (welcomePath</>"LICENSE")
                copyFile (welcomeSource</>"src"</>"Main.hs")      welcomeMain
                copyFile (welcomeSource</>"test"</>"Main.hs")     (welcomePath</>"test"</>"Main.hs")
            defaultWorkspace <- liftIO $ (</> "leksah.lkshw") <$> getHomeDirectory
            defaultExists <- liftIO $ doesFileExist defaultWorkspace
            if defaultExists
                then workspaceOpenThis False defaultWorkspace
                else workspaceNewHere defaultWorkspace
            workspaceTryQuiet $ void (workspaceAddPackage' welcomeCabal)
            fileOpenThis welcomeMain
        initInfo (modifyIDE_ (\ide -> ide{currentState = IsRunning}))
        maybe (return ()) (void . sourceRemove) timeout
        postAsyncIDE . liftIO . mainLoop . (`reflectIDE` ideR) $ do
            currentPrefs <- readIDE prefs
            when (backgroundBuild currentPrefs) backgroundMake
--        timeoutAdd PRIORITY_DEFAULT_IDLE 1000 (do
--            reflectIDE (do
--                currentPrefs <- readIDE prefs
--                when (backgroundBuild currentPrefs) $ backgroundMake) ideR
--            return True)
        triggerEvent ideR (Sensitivity [(SensitivityInterpreting, False)])
        return ()
--        mainGUI

fDescription :: FilePath -> FieldDescription Prefs
fDescription configPath = VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Paths under which haskell sources for packages may be found"
                $ paraDirection  <<<- ParaDirection Vertical
                    $ paraMinSize <<<- ParaMinSize (-1, 150)
                        $ emptyParams)
            sourceDirectories
            (\b a -> a{sourceDirectories = b})
            (filesEditor Nothing FileChooserActionSelectFolder "Select folders")
    ,   mkField
            (paraName <<<- ParaName "Unpack source for cabal packages to" $ emptyParams)
            unpackDirectory
            (\b a -> a{unpackDirectory = b})
            (maybeEditor (stringEditor (const True) True,emptyParams) True "")
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
firstStart :: Prefs -> IO Bool
firstStart prefs = do
    dataDir     <- getDataDir
    prefsPath   <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
    prefs       <- readPrefs prefsPath
    configDir   <- getConfigDir
    dialog      <- dialogNew
    setLeksahIcon dialog
    setWindowTitle dialog "Welcome to Leksah, the Haskell IDE"
    setWindowWindowPosition dialog WindowPositionCenter
    dialogAddButton' dialog "gtk-ok" ResponseTypeOk
    dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
    vb          <- dialogGetContentArea dialog >>= liftIO . unsafeCastTo Box
    label       <- labelNew (Just (
        "Before you start using Leksah it will collect and download metadata about your installed Haskell packages.\n" <>
        "You can add folders under which you have sources for Haskell packages not available from Hackage."))
    (widget, setInj, getExt,notifier) <- buildEditor (fDescription configDir) prefs
    boxPackStart' vb label PackNatural 7
    sw <- scrolledWindowNew noAdjustment noAdjustment
    scrolledWindowAddWithViewport sw widget
    scrolledWindowSetPolicy sw PolicyTypeNever PolicyTypeAutomatic
    boxPackStart' vb sw PackGrow 7
    windowSetDefaultSize dialog 800 630
    widgetShowAll dialog
    response <- dialogRun' dialog
    widgetHide dialog
    case response of
        ResponseTypeOk -> do
            mbNewPrefs <- liftIO $ extract prefs [getExt]
            widgetDestroy dialog
            case mbNewPrefs of
                Nothing -> do
                    liftIO $ sysMessage Normal "No dialog results"
                    return False
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
                    firstBuild newPrefs
                    return True
        _ -> do
            widgetDestroy dialog
            return False

setLeksahIcon :: (IsWindow self) => self -> IO ()
setLeksahIcon window = do
    dataDir <- getDataDir
    let iconPath = dataDir </> "pics" </> "leksah.png"
    iconExists  <-  doesFileExist iconPath
    when iconExists $
        windowSetIconFromFile window iconPath

firstBuild :: Prefs -> IO ()
firstBuild newPrefs = do
    dialog      <- dialogNew
    liftIO $ setLeksahIcon dialog
    setWindowTitle dialog "Leksah: Updating Metadata"
    setWindowWindowPosition dialog WindowPositionCenter
    setWindowDeletable dialog False
    vb          <- dialogGetContentArea dialog >>= liftIO . unsafeCastTo Box
    progressBar <- progressBarNew
    progressBarSetText progressBar $ Just "Please wait while Leksah collects information about Haskell packages on your system"
    progressBarSetFraction progressBar 0.0
    boxPackStart' vb progressBar PackGrow 7
    forkIO $ do
        logger <- getRootLogger
        let verbosity = case getLevel logger of
                            Just level -> ["--verbosity=" <> T.pack (show level)]
                            Nothing    -> []
        (output, pid) <- runTool "leksah-server" (["-sbo", "+RTS", "-N2", "-RTS"] ++ verbosity) Nothing Nothing
        output $$ CL.mapM_ (update progressBar)
        waitForProcess pid
        void . idleAdd PRIORITY_DEFAULT $ do
            dialogResponse' dialog ResponseTypeOk
            return False
    widgetShowAll dialog
    dialogRun dialog
    widgetHide dialog
    widgetDestroy dialog
    return ()
    where
        update pb to = do
            let str = toolline to
            case T.stripPrefix "update_toolbar " str of
                Just rest -> void . idleAdd PRIORITY_DEFAULT $ do
                    progressBarSetFraction pb (read $ T.unpack rest)
                    return False
                Nothing   -> debugM "leksah" $ T.unpack str




