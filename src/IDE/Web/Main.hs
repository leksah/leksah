{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Main
  ( develMain
  , newIDE
  , main
  , css
  , startJSaddle
  ) where

import Control.Concurrent
       (tryPutMVar, takeMVar, readMVar, threadDelay, modifyMVar, newMVar,
        newEmptyMVar, forkIO)
import Control.Exception (SomeException)
import Control.Lens (to, (^..), (?~), (.~), (%~), _Just)
import Control.Monad (forever, when, void)
import Control.Monad.IO.Class (MonadIO(..))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Char8 as BS (unlines)
import qualified Data.ByteString.Lazy as BS (toStrict)
import qualified Data.Dependent.Map as DM (singleton, fromList)
import Data.Dependent.Sum (DSum(..))
import Data.Foldable (Foldable(..), forM_)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Misc (Const2(..))
import Data.IORef (newIORef)
import Data.Map (mapKeys)
import qualified Data.Map as M (keys)
import qualified Data.Set as S (fromList)
import qualified Data.Text as T (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import Text.Printf (printf)

import System.Directory
       (doesFileExist, getDirectoryContents)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, (</>))
import System.FSNotify (withManager)

import Network.Socket (withSocketsDo)
import qualified Network.HTTP.Types as H (status200)
import qualified Network.Wai as W
       (responseLBS, pathInfo, requestMethod)
import Network.Wai.Application.Static
       (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)

import Criterion.Measurement (initializeTime)

import Clay
       (height, pct, width, fontFaceSrc, fontWeight, fontStyle,
        fontFace, render, (?), margin, nil, px, fontFamily, background,
        color, black, white, fontSize, normal, FontFaceFormat(..),
        FontFaceSrc(..))

import Language.Javascript.JSaddle (JSM, eval, syncPoint)
import Language.Javascript.JSaddle.Warp
       (jsaddleJs, jsaddleOr, debugWrapper)
import GHCJS.DOM.Types (askJSM)
import GHCJS.DOM.Debug (addDebugMenu)

import Reflex
       (switchDyn, mergeList, foldDyn, traceEventWith, constDyn, ffor,
        Dynamic, Event, holdDyn, merge, newTriggerEvent, leftmost,
        performEvent_, getPostBuild, performEvent, select, fan, fanMap)
import Reflex.Dom.Core
       (dynText, elAttr', (=:), MonadWidget, mainWidgetWithCss)

import IDE.Core.State
       (triggerBuild, readIDE, IDEAction, wsFile, jsContexts, workspace,
        IDEState(..), Prefs(..), IDE(..), IDERef, __, reflectIDE,
        getDataDir, catchIDE, modifyIDE_, prefs)
import qualified IDE.TextEditor.Yi.Config as Yi (start)
import IDE.Preferences (readPrefs)
import IDE.SourceCandy (parseCandy)
import IDE.TextEditor.Yi.Config (defaultYiConfig)
import IDE.Utils.FileUtils (loadNixCache, getConfigFilePathForLoad)
import IDE.Utils.Utils
       (leksahCandyFileExtension, standardPreferencesFilename)
import IDE.Web.Command (commandAction)
import IDE.Web.Events
       (IDEWidget(..), TabEvents(..), TabKey(..), _ToolbarCommand,
        _KeymapCommand, _PackageCommand, _ProjectPackageEvents,
        _ProjectCommand)
import IDE.Web.Layout (layoutCss)
import IDE.Web.Widget.ContextMenu (contextMenuCss)
import IDE.Web.Widget.Editor (editorCss, editorWidget)
import IDE.Web.Widget.Errors (errorsCss, errorsWidget)
import IDE.Web.Widget.Findbar (findbarCss, findbarWidget)
import IDE.Web.Widget.Flipper (flipperCss, flipperWidget)
import IDE.Web.Widget.Grep (grepCss, grepWidget)
import IDE.Web.Widget.Keymap (keymapWidget)
import IDE.Web.Widget.Log (logCss, logWidget)
import IDE.Web.Widget.Menu (menuCss)
import IDE.Web.Widget.Menubar (menubarCss, menubarWidget)
import IDE.Web.Widget.Statusbar (statusbarCss, statusbarWidget)
import IDE.Web.Widget.Tabs (tabsWidget, tabsCss)
import IDE.Web.Widget.Toolbar (toolbarCss, toolbarWidget)
import IDE.Web.Widget.Workspace (workspaceCss, workspaceWidget)
import qualified IDE.Workspaces.Writer as Writer
       (setWorkspace, readWorkspace)
import IDE.Workspaces (backgroundMake)
import Control.Concurrent.STM (newTVarIO)
import Haskell.Ide.Engine.PluginApi (IdeState(..))
import Haskell.Ide.Engine.GhcModuleCache (emptyModuleCache)

-- > :fork 1 IDE.Web.Main.develMain

newIDE :: (JSM () -> IO ()) -> IO ()
newIDE runJs = do
  let yiConfig = defaultYiConfig
  initializeTime
  exitCode <- newIORef ExitSuccess
  withSocketsDo $ do
    dataDir         <- getDataDir

    prefsPath       <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
    initPrefs       <- readPrefs prefsPath
    withManager $ \fsnotify -> Yi.start yiConfig $ \yiControl -> do
      candyPath   <-  getConfigFilePathForLoad
                          (case sourceCandy initPrefs of
                              (_,name)   ->   T.unpack name <> leksahCandyFileExtension) Nothing dataDir
      candySt     <-  parseCandy candyPath

      triggerBuild <- newEmptyMVar
      nixCache <- loadNixCache
      externalModified <- newMVar mempty
      watchers <- newMVar (mempty, mempty)
      hieState <- newTVarIO (IdeState emptyModuleCache mempty mempty Nothing)
      let ide = IDE
            {   _ideGtk            =   Nothing
            ,   _exitCode          =   exitCode
            ,   _candy             =   candySt
            ,   _prefs             =   initPrefs
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
            ,   _autoCommand       =   Nothing
            ,   _autoURI           =   Nothing
            ,   _triggerBuild      =   triggerBuild
            ,   _fsnotify          =   fsnotify
            ,   _watchers          =   watchers
            ,   _developLeksah     =   True
            ,   _nixCache          =   nixCache
            ,   _externalModified  =   externalModified
            ,   _jsContexts        =   []
            ,   _logLineMap        =   mempty
            ,   _hieState          =   hieState
      }
      ideR <- liftIO $ newMVar (const (return ()), ide)
      let filePath = "/Users/hamish/leksah.lkshw"
      liftIO $ (`reflectIDE` ideR) $
          catchIDE (
              Writer.readWorkspace filePath >>= \case
                  Left errorMsg -> liftIO $ putStrLn $ "Could not open " <> filePath <> ". " <> errorMsg
                  Right ws -> do
                        modifyIDE_ (workspace ?~ ws)
                        Writer.setWorkspace (Just $ ws & wsFile .~ filePath)
                      )
             (\ (e :: SomeException) ->
                  liftIO $ putStrLn $ printf (T.unpack $ __ "Can't load workspace file %s\n%s") filePath (show e))
      _ <- liftIO . forkIO . forever $ do
            takeMVar triggerBuild
            reflectIDE (do
--              postSyncIDE' PRIORITY_LOW $
--                eventsPending >>= \case
--                    True ->
--                        liftIO . void $ tryPutMVar triggerBuild ()
--                    False -> do
--                        _ <- liftIO $ tryTakeMVar triggerBuild
                        currentPrefs <- readIDE prefs
                        when (backgroundBuild currentPrefs) backgroundMake) ideR
      runJs $ jsMain ideR

develMain :: IO ()
develMain = newIDE (debugJSaddle 3367)

jsMain :: IDERef -> JSM ()
jsMain ideR = do
  -- enableLogging True -- Uncomment this to add verbose JSaddle logging
  dataDir <- liftIO getDataDir
  ctx <- askJSM
  cmCss <- liftIO . BS.readFile $ dataDir </> "codemirror/lib/codemirror.css"
  let cmThemeDir = dataDir </> "codemirror/theme"
  cmThemeFiles <- filter (`notElem` [".", ".."]) <$> liftIO (getDirectoryContents cmThemeDir)
  cmThemes <- BS.unlines <$> mapM (liftIO . BS.readFile . (cmThemeDir </>)) cmThemeFiles
  _ <- liftIO (readFile $ dataDir </> "codemirror/lib/codemirror.js") >>= eval
  _ <- liftIO (readFile $ dataDir </> "codemirror/addon/mode/simple.js") >>= eval
  let cmModeDir = dataDir </> "codemirror/mode"
  cmModes <- filter (`notElem` [".", ".."]) <$> liftIO (getDirectoryContents cmModeDir)
  forM_ cmModes $ \cmMode -> do
      let js = cmModeDir </> cmMode </> cmMode <> ".js"
      liftIO (doesFileExist js) >>= (`when` (void $ liftIO (readFile js) >>= eval))

  mainWidgetWithCss (BS.unlines [cmCss, cmThemes, BS.toStrict (LT.encodeUtf8 css)]) $ mdo
      (ideE, t) <- newTriggerEvent
      ideActionE <- main ideD
      performEvent_ $ liftIO . (`reflectIDE` ideR) <$> ideActionE
      newIde <- liftIO $ modifyMVar ideR $ \(oldT, oldIde) -> do
          let newIde' = oldIde & jsContexts %~ (<> [ctx])
          return ((\x -> oldT x >> t x, newIde'), newIde')
      pb <- getPostBuild
      pbIde <- performEvent $ pb $> liftIO (snd <$> readMVar ideR)
      ideD <- holdDyn newIde $ leftmost [ideE, pbIde]
      return ()
  liftIO $ threadDelay 1000000000

indexHtml :: ByteString
indexHtml =
    "<!DOCTYPE html>\n\
    \<html>\n\
    \<head>\n\
    \<title>JSaddle</title>\n\
    \</head>\n\
    \<body>\n\
    \</body>\n\
    \</html>"

startJSaddle :: Int -> (ByteString -> ByteString -> JSM () -> IO ()) -> JSM () -> IO ()
startJSaddle p runJs jsm = do
  dataDir <- getDataDir
  _ <- forkIO $ runSettings (setPort p (setTimeout 3600 defaultSettings)) =<<
    jsaddleOr defaultConnectionOptions
              (addDebugMenu >> jsm >> syncPoint)
              (\req sendResponse ->
        case (W.requestMethod req, W.pathInfo req) of
            ("GET", ["jsaddle.js"]) ->
                 sendResponse
                    $ W.responseLBS H.status200
                        [("Content-Type", "application/javascript")]
                    $ jsaddleJs False
            _ -> staticApp (defaultWebAppSettings dataDir) req sendResponse)
  runJs indexHtml ("http://127.0.0.1:" <> encodeUtf8 (T.pack $ show p)) jsm

debugJSaddle :: Int -> JSM () -> IO ()
debugJSaddle p f = do
  dataDir <- getDataDir
  debugWrapper $ \withRefresh registerContext ->
    runSettings (setPort p (setTimeout 3600 defaultSettings)) =<<
      jsaddleOr defaultConnectionOptions
                (registerContext >> addDebugMenu >> f >> syncPoint)
                (withRefresh $ \req sendResponse ->
          case (W.requestMethod req, W.pathInfo req) of
              ("GET", ["jsaddle.js"]) ->
                   sendResponse
                      $ W.responseLBS H.status200
                          [("Content-Type", "application/javascript")]
                      $ jsaddleJs True
              _ -> staticApp (defaultWebAppSettings dataDir) req sendResponse)
--                      $ W.responseLBS H.status404
--                          [("Content-Type", "text/plain")]
--                     "Not found")

css :: LT.Text
css = render $ do
    fontFace $ do
        fontFamily ["Hasklig"] []
        fontStyle normal
        fontWeight normal
        fontFaceSrc [FontFaceSrcUrl "/fonts/Hasklig-Regular.otf" (Just OpenType)]
    "body" ? do
        fontFamily
          ["-apple-system"
          ,"BlinkMacSystemFont"
          ,"Segoe UI"
          ,"Helvetica"
          ,"Arial"
          ,"sans-serif"
          ,"Apple Color Emoji"
          ,"Segoe UI Emoji"
          ,"Segoe UI Symbol"
          ] []
        fontSize (px 12)
        margin nil nil nil nil
        background black
        color white
    ".leksah" ? do
        width (pct 100)
        height (pct 100)
    contextMenuCss
    layoutCss
    menuCss
    menubarCss
    toolbarCss
    workspaceCss
    editorCss
    errorsCss
    logCss
    grepCss
    findbarCss
    statusbarCss
    tabsCss
    flipperCss

main
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> m (Event t IDEAction)
main ide = mdo
  (top, topEvents) <- elAttr' "div" ("class" =: "leksah" <> "tabindex" =: "0") $ mdo
    keymapE <- keymapWidget top
    menubarE   <- menubarWidget
    toolbarE   <- toolbarWidget ide
    let initialTabs =
               WorkspaceKey =: ("tall", Just ())
            <> ErrorsKey    =: ("wide1", Just ())
            <> LogKey       =: ("wide1", Just ())
            <> GrepKey      =: ("wide1", Just ())
        initialVisibleTabs =
               "tall" =: WorkspaceKey
            <> "wide1" =: LogKey

    let label k = dynText $ ffor k $ \case
            WorkspaceKey   -> "Workspace"
            ErrorsKey      -> "Errors"
            LogKey         -> "Log"
            GrepKey        -> "Grep"
            EditorKey file -> T.pack $ takeFileName file

    (openFileE, makeEditor) <- editorWidget ide allE
    flipE <- flipperWidget
      recentTabs
      allE
      label
    let openFileE' = mapKeys EditorKey <$> openFileE
    openFileKeysD <- foldDyn (<>) mempty $ S.fromList . M.keys <$> openFileE'
    (recentTabs, tabE) <- tabsWidget
      initialTabs
      initialVisibleTabs
      openFileE'
      flipE
      (\k _ -> label $ constDyn k)
      (\k v -> do
        let toDM x = fmap (DM.singleton x . Identity)
        case k of
          WorkspaceKey   -> toDM WorkspaceTab <$> workspaceWidget ide
          ErrorsKey      -> toDM ErrorsTab <$> errorsWidget ide allE
          LogKey         -> toDM LogTab <$> logWidget ide
          GrepKey        -> toDM GrepTab <$> grepWidget
          EditorKey file -> toDM EditorTab <$> makeEditor file v)
    findbarE   <- findbarWidget
    statusbarE <- statusbarWidget

    let allE = merge (DM.fromList
            [ MenubarWidget   :=> menubarE
            , ToolbarWidget   :=> toolbarE
            , TabWidget       :=> tabE
            , FindbarWidget   :=> findbarE
            , StatusbarWidget :=> statusbarE
            , KeymapWidget    :=> keymapE
            ])
        workspaceE = select (fan (select (fanMap tabE) (Const2 WorkspaceKey))) WorkspaceTab
        editorE' = switchDyn $ leftmost . map (select (fanMap tabE) . Const2) . toList <$> openFileKeysD
        editorE = select (fan editorE') EditorTab

    return $ sequence_ <$>
         ((^.. _ToolbarCommand . commandAction . _Just) <$> toolbarE)
      <> ((^.. _KeymapCommand . commandAction . _Just) <$> keymapE)
      <> ((^.. traverse . _ProjectCommand . commandAction . _Just) <$> workspaceE)
      <> ((^.. traverse . _ProjectPackageEvents . traverse . _PackageCommand . commandAction . _Just) <$> workspaceE)
      <> ((^.. (to $ \() -> do
        tb <- readIDE triggerBuild
        void . liftIO $ tryPutMVar tb ())) <$> editorE)
  return topEvents
