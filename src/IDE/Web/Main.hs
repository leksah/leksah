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
import Control.Event (registerEvent)
import Control.Exception (SomeException, catch)
import GHC.Stats
       (getRTSStats, getRTSStatsEnabled, RTSStats(..), GCDetails(..))
import qualified System.IO as IO (hPutStrLn, stderr)
import Control.Lens (to, view, (^.), (^..), (^?), (?~), (.~), (%~), _Just)
import Control.Monad (forever, forM, forM_, unless, when, void)
import Control.Monad.IO.Class (MonadIO(..))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Char8 as BS (unlines)
import qualified Data.ByteString.Lazy as BS (toStrict)
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import Data.Char (isAlphaNum)
import qualified Data.Dependent.Map as DM (singleton, fromList, lookup)
import Data.Dependent.Sum (DSum(..))
import Data.Foldable (Foldable(..))
import Data.Function ((&))
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Misc (Const2(..))
import Data.IORef (newIORef, atomicModifyIORef', writeIORef, readIORef)
import Data.Map (mapKeys)
import qualified Data.Map as M
       (Map, keys, elems, toList, fromList, union, findWithDefault, lookup, null,
        insert, delete, member, filter, singleton, mapWithKey, empty)
import Data.Map (Map)
import qualified Data.Set as S
       (fromList, delete, singleton, empty, insert, member, intersection)
import Data.Time.Clock (NominalDiffTime)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, unlines, isPrefixOf, null, intercalate, breakOn, drop, stripPrefix, takeWhile, all, splitOn)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import Text.Printf (printf)
import Text.Read (readMaybe)

import System.Directory
       (doesFileExist, doesDirectoryExist, getDirectoryContents, removeFile,
        getHomeDirectory, makeRelativeToCurrentDirectory)
import System.Process (readProcessWithExitCode)
import Data.List (nub, sort, isPrefixOf, isInfixOf, find, elemIndex)
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, dropFileName, (</>))
import System.Environment (getArgs)
import System.Posix.Process (exitImmediately)
import System.FSNotify (withManager)

import Network.Socket (withSocketsDo)
import qualified Network.HTTP.Types as H (status200, status504)
import qualified Network.Wai as W
       (responseLBS, pathInfo, requestMethod, strictRequestBody)
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

import Language.Javascript.JSaddle
       (JSM, eval, syncPoint, jsg, js, js0, js1, js2, jss, fun, valToText, valToBool, liftJSM)
import Language.Javascript.JSaddle.Warp
       (jsaddleJs, jsaddleOr, debugWrapper)
import Language.Javascript.JSaddle.Terminal.Bootstrap (bootstrapHtml)
import IDE.Web.JsaddleTunnel (tunnelSyncRequest)
import GHCJS.DOM.Types (askJSM)
import GHCJS.DOM.Debug (addDebugMenu)

import Reflex
       (switchDyn, foldDyn, ffor,
        Dynamic, Event, holdDyn, merge, newTriggerEvent, leftmost, never,
        performEvent_, getPostBuild, performEvent, select, fan, fanMap,
        fmapMaybe, ffilter, attachWith, attach, current, updated, holdUniqDyn, tag, gate,
        listViewWithKey, sample,
        tagPromptlyDyn, debounce, delay, tickLossyFromPostBuildTime)
import Reflex.Dom.Core
       (dynText, el, elAttr, elAttr', elDynAttr, elDynAttr', text, domEvent, EventName(..),
        (=:), MonadWidget, mainWidgetWithCss)

import IDE.Core.State
       (triggerBuild, readIDE, IDEAction, wsFile, jsContexts, workspace,
        IDEState(..), Prefs(..), TallVisibility(..), IDE(..), IDERef, __,
        reflectIDE, getDataDir, catchIDE, modifyIDE_, prefs, currentState,
        wsProjects, pjPackages, ipdCabalFile, ipdPackageDir, wsActivePackFile,
        currentError, logRefFullFilePath, refDescription, logRefSrcSpan,
        srcSpanStartLine)
import IDE.Metainfo.Provider (initInfo)
import IDE.Web.IDERefStore (setGlobalIDERef)
import IDE.Web.CmdServer (startCmdServer, suppressNextRestart)
import IDE.Web.CloseRequest (nextCloseRequest)
import IDE.Web.OpenFileRequest (nextOpenedFile)
import IDE.Web.OpenPanel (runOpenFilePanel, runOpenProjectPanel)
import IDE.Web.SaveRequest (nextSaveRequest)
import IDE.Web.Theme (themeVarsCss)
import IDE.Web.FindRequest (nextFindRequest)
import IDE.Web.PreferencesRequest (nextPreferencesRequest)
import IDE.Web.RegionGrabRequest (nextRegionGrab)
import IDE.Web.ScreenshotRequest (requestScreenshotRegion)
import IDE.Web.RegionCapture
       (screenCaptureAllowed, grabRegionToTarget, sendPathToTarget,
        sendTextToTarget, nextRegionFile, resolveTmuxSessionId)
import IDE.Web.AIContextRequest (AIAction(..), nextAIAction)
import IDE.Web.RemoteTermRequest (nextTermRequest)
import IDE.Web.RecentFiles (updateRecentFiles)
import IDE.Web.ReplTmux (tmuxCmd)
import IDE.Web.TerminalInput
       (setActiveTerminal, tmuxCommandActiveTerminal, selectSplitActiveTerminal,
        focusTerminalPane)
import IDE.Web.TransparencyRequest (nextToggleTransparency)
import IDE.Web.SnapRequest (SnapReq(..), nextSnapRequest)
import IDE.Web.Session
       (WebSession(..), readWebSession, writeWebSession)
import qualified IDE.TextEditor.Yi.Config as Yi (start)
import IDE.Preferences (readPrefs, writePrefs)
import IDE.SourceCandy (parseCandy)
import IDE.TextEditor.Yi.Config (defaultYiConfig)
import IDE.Utils.FileUtils
       (loadNixCache, getConfigFilePathForLoad, getConfigFilePathForSave)
import IDE.Utils.Utils
       (leksahCandyFileExtension, standardPreferencesFilename)
import IDE.Web.Command
       (commandAction, Command(..), _CommandSelectSplit,
        _CommandSelectSidePane, _CommandSelectBottomPane)
import IDE.Web.Events
       (IDEWidget(..), TabEvents(..), TabKey(..), TerminalEvents(..),
        FindbarEvents(..), PreferencesEvents(..), FlipItem(..),
        _ToolbarCommand, _MenubarCommand, _KeymapCommand, _PackageCommand,
        _ProjectPackageEvents, _ProjectCommand, _NewTerminal, _SelectTerminal,
        _CloseTerminal, _SelectTerminalWindow, _SelectTerminalPane,
        _NewRemoteTerminal, _SelectRemoteHost, _SelectRemoteTerminal,
        _SelectRemoteTerminalWindow, _SelectRemoteTerminalPane)
import IDE.Web.Layout (layoutCss)
import IDE.Web.Widget.Changes (changesCss, changesWidget)
import IDE.Web.Widget.Preferences (preferencesCss, preferencesWidget)
import IDE.Web.Widget.Flake (flakeCss)
import IDE.Web.Widget.ContextMenu (contextMenuCss)
import IDE.Web.Widget.Editor (editorCss, editorWidget)
import IDE.Web.Widget.Errors (errorsCss, errorsWidget)
import IDE.Web.Widget.Findbar (findbarCss, findbarWidget, findMatcher)
import IDE.Web.Widget.Flipper (flipperCss, flipperWidget)
import IDE.Web.Widget.Grep (grepCss, grepWidget, runGrep)
import IDE.Web.Widget.Keymap (keymapWidget)
import IDE.Web.Widget.Log (logCss, logWidget)
import IDE.Web.Widget.Menu (menuCss)
import IDE.Web.Widget.Menubar (menubarCss, menubarWidget)
import IDE.Web.Widget.Metadata (metadataCss, metadataWidget)
import IDE.Web.Widget.Statusbar (statusbarCss, statusbarWidget)
import IDE.Web.Widget.Tabs (tabsWidget, tabsCss)
import IDE.Web.Widget.Terminal
       (terminalCss, terminalWidget, listTerminalSessions, killTerminalSession,
        selectTmuxWindow, selectTmuxPane, activePaneId, paneGeometry, sessionOfPane,
        listTerminalTree, createTerminalSession, openFileInEditor, notifyTerminalBell,
        createRemoteSession, selectRemoteTmuxWindow, selectRemoteTmuxPane,
        listRemoteTerminalTree, remoteTabHostTarget,
        reapControlClients, TmuxWindow(..), TmuxPane(..))
import IDE.Web.Widget.Terminals (terminalsCss, terminalsWidget, sessionAlert, windowAlert)
import IDE.Web.Widget.TerminalCC (terminalCCWidget)
import IDE.Web.Widget.Toolbar (toolbarCss, toolbarWidget)
import IDE.Web.Widget.Workspace (workspaceCss, workspaceWidget)
import qualified IDE.Workspaces.Writer as Writer
       (setWorkspace, readWorkspace)
import IDE.Workspaces (backgroundMake)

-- > :fork 1 IDE.Web.Main.develMain

-- | First 'Bool': whether to render the web menu bar (hidden for
-- @leksah-wkwebview@, which has a native macOS menu).  Second 'Bool':
-- develop-leksah mode — exit with code 2 when the leksah package is rebuilt in
-- the IDE, so a wrapper (leksah-nix.sh) can rebuild and relaunch.
newIDE :: Bool -> Bool -> Bool -> (JSM () -> IO ()) -> IO ()
newIDE showMenubar macTitlebar developLeksah runJs = do
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

      triggerBuildVar <- newEmptyMVar
      nixCache <- loadNixCache
      externalModified <- newMVar mempty
      watchers <- newMVar (mempty, mempty)
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
            ,   _triggerBuild      =   triggerBuildVar
            ,   _fsnotify          =   fsnotify
            ,   _watchers          =   watchers
            ,   _developLeksah     =   developLeksah
            ,   _nixCache          =   nixCache
            ,   _externalModified  =   externalModified
            ,   _jsContexts        =   []
            ,   _logLineMap        =   mempty
      }
      ideR <- liftIO $ newMVar (const (return ()), ide)
      liftIO $ setGlobalIDERef ideR  -- so the native macOS menu can run commands
      liftIO $ startCmdServer ideR   -- control socket for the leksah-cmd CLI
      -- Detach control-mode clients left over from previous runs BEFORE any
      -- terminal attaches: they wedge on leksah exit, stay counted as attached,
      -- and their stale 80x24 sizes clamp every window they're attached to.
      liftIO reapControlClients
      -- GC monitor (needs +RTS -T, set via -with-rtsopts): log every major
      -- collection with its pause to stderr, so UI hitches can be correlated
      -- with GC (or ruled out) by watching the leksah-nix.sh window.
      _ <- liftIO . forkIO $ do
          enabled <- getRTSStatsEnabled
          when enabled $ do
              let loop prevMajor = do
                      s <- getRTSStats
                      let mg = major_gcs s
                          d  = gc s
                          ms :: Integral n => n -> Double
                          ms n = fromIntegral n / 1e6
                      when (mg /= prevMajor) . IO.hPutStrLn IO.stderr $
                          "RTS: major GC #" <> show mg
                          <> " pause=" <> show (round (ms (gcdetails_elapsed_ns d)) :: Int) <> "ms"
                          <> " live=" <> show (gcdetails_live_bytes d `div` (1024 * 1024)) <> "MB"
                      threadDelay 250000
                      loop mg
              loop 0
      -- Develop mode: rebuilding the leksah package in the IDE triggers
      -- QuitToRestart.  The Gtk front end handles that via its application; the
      -- web front ends have no such hook, so exit with code 2 and let the
      -- wrapper (leksah-nix.sh) rebuild and relaunch.
      when developLeksah $ do
          liftIO . (`reflectIDE` ideR) . void $
              registerEvent ideR "QuitToRestart" $ \e -> do
                  -- `leksah-cmd rebuild-self --no-restart` (IDE-build path) arms
                  -- this so a successful self-build lands on disk without the
                  -- restart; the next QuitToRestart behaves normally.
                  suppress <- liftIO $ atomicModifyIORef' suppressNextRestart (\s -> (False, s))
                  if suppress
                    then liftIO $ putStrLn "leksah: QuitToRestart suppressed (rebuild-self --no-restart)"
                    else liftIO $ exitImmediately (ExitFailure 2)
                  return e
          -- External relaunch trigger (dev-relaunch.sh): poll for a request
          -- file and exit(2) so leksah-nix.sh's loop rebuilds and relaunches.
          -- We poll rather than catch a signal because the native run loop makes
          -- signal handlers unreliable here, whereas forkIO threads run fine
          -- (the terminals rely on them).
          liftIO $ do
              home <- getHomeDirectory
              let trigger = home </> ".leksah" </> "relaunch-request"
              -- Drop any stale request first, so we don't relaunch in a loop.
              removeFile trigger `catch` \(_ :: SomeException) -> return ()
              void . forkIO . forever $ do
                  threadDelay 400000
                  there <- doesFileExist trigger
                  when there $ do
                      removeFile trigger `catch` \(_ :: SomeException) -> return ()
                      exitImmediately (ExitFailure 2)
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
            takeMVar triggerBuildVar
            reflectIDE (do
--              postSyncIDE' PRIORITY_LOW $
--                eventsPending >>= \case
--                    True ->
--                        liftIO . void $ tryPutMVar triggerBuild ()
--                    False -> do
--                        _ <- liftIO $ tryTakeMVar triggerBuild
                        currentPrefs <- readIDE prefs
                        when (backgroundBuild currentPrefs) backgroundMake) ideR
      runJs $ jsMain showMenubar macTitlebar ideR

develMain :: IO ()
develMain = do
  dev <- elem "--develop-leksah" <$> getArgs
  newIDE True False dev (debugJSaddle 3367)

jsMain :: Bool -> Bool -> IDERef -> JSM ()
jsMain showMenubar macTitlebar ideR = do
  -- enableLogging True -- Uncomment this to add verbose JSaddle logging
  dataDir <- liftIO getDataDir
  ctx <- askJSM
  -- CodeMirror 6 (bundled, exposes window.LeksahCM). It injects its own
  -- styles (incl. the editor theme) at runtime, so there is no CSS to load.
  _ <- liftIO (readFile $ dataDir </> "cm6/leksah-cm6.js") >>= eval

  -- xterm.js (terminal pane) — loaded as globals `Terminal` / `FitAddon`.
  xtermCss <- liftIO . BS.readFile $ dataDir </> "xterm/xterm.css"
  _ <- liftIO (readFile $ dataDir </> "xterm/xterm.js") >>= eval
  _ <- liftIO (readFile $ dataDir </> "xterm/addon-fit.js") >>= eval
  _ <- liftIO (readFile $ dataDir </> "xterm/addon-webgl.js") >>= eval
  -- Unicode 11 width tables: xterm.js defaults to Unicode 6, where emoji
  -- measure width 1 — modern apps and tmux assume 2, so glyphs like ✅
  -- squeeze into one cell and misalign everything after them.
  _ <- liftIO (readFile $ dataDir </> "xterm/addon-unicode11.js") >>= eval
  _ <- liftIO (readFile $ dataDir </> "xterm/addon-search.js") >>= eval

  -- Makes project-file paths in terminal output Ctrl-clickable (window.LeksahTermLinks).
  _ <- eval terminalLinksJs
  _ <- eval badgesJs

  -- Builds xterm linkHandlers for OSC 8 hyperlinks (window.LeksahOscLinks): a hover
  -- tooltip with the URL, and click-to-open for http(s) links.
  _ <- eval terminalOscLinksJs

  -- Defines window.LeksahTerm: a registry of live xterm.js terminals plus a
  -- write(id, base64) that decodes straight into a Uint8Array and hands the raw
  -- bytes to xterm.  xterm does its own (stateful) UTF-8 decoding, so this both
  -- avoids decoding shell output on the Haskell side and correctly handles
  -- multibyte sequences split across PTY reads.  base64 keeps the payload to
  -- printable ASCII, so jsaddle doesn't have to escape the control bytes that
  -- pervade terminal output.
  _ <- eval terminalWriteJs

  -- Defines window.LeksahJsaddlePane: the per-pane iframe registry of the
  -- jsaddle-terminal tunnels (see IDE.Web.Widget.TerminalCC) — batches go INTO
  -- an iframe via postMessage, and one page-level message listener routes each
  -- iframe's ready/results messages back to its registered Haskell callback.
  _ <- eval jsaddlePaneJs

  -- Defines window.LeksahDividerDrag: drag-to-resize for the CC pane dividers
  -- (the drag itself runs in JS — jsaddle dispatches events asynchronously,
  -- far too laggy for mousemove; Haskell is called back once, on drop).
  _ <- eval dividerDragJs

  -- Defines window.leksahSetHoles/leksahClearHoles: clips transparent tmux panes
  -- out of the page root so the window shows through (macOS click-through holes).
  _ <- eval transparencyJs

  -- Focus the find bar's text input (called when Edit ▸ Find shows it); deferred
  -- to the next frame so the just-revealed input is laid out and focusable.
  _ <- eval focusFindJs

  -- Focus a side/bottom list pane when it's activated, and keyboard list
  -- navigation (Up/Down/Enter/arrows) for the focused list pane.
  _ <- eval focusPaneJs
  _ <- eval listNavJs
  _ <- eval focusTabJs
  _ <- eval termActivityJs

  -- Helper used to decide whether to reveal a focused file in the workspace
  -- tree: a file can appear there more than once, so if any occurrence is
  -- already on-screen we skip expanding/scrolling.
  _ <- eval revealCheckJs

  -- The single top-level active-pane shadow overlay (window.leksahUpdatePaneHl).
  _ <- eval paneHlJs

  -- window.leksahSetColorIcons: swap every /pics/*.svg between the monochrome
  -- default and the coloured set (/pics/color/*.svg), driven by the pref.
  _ <- eval colorIconsJs

  -- The Claude-coordination traffic light (top-right dot): leksahTestStart /
  -- leksahTestEnd / leksahStatus, driven over the cmd socket via `js eval`.
  _ <- eval statusLightJs

  -- window.leksahSelectRegion: the permission-free region picker for grab-region.
  _ <- eval regionSelectJs

  mainWidgetWithCss (BS.unlines [xtermCss, BS.toStrict (LT.encodeUtf8 css)]) $ mdo
      (ideE, t) <- newTriggerEvent
      ideActionE <- main showMenubar macTitlebar ideD
      performEvent_ $ liftIO . (`reflectIDE` ideR) <$> ideActionE
      newIde <- liftIO $ modifyMVar ideR $ \(oldT, oldIde) -> do
          let newIde' = oldIde & jsContexts %~ (<> [ctx])
          return ((\x -> oldT x >> t x, newIde'), newIde')
      pb <- getPostBuild
      -- Leave the start-up state and load metadata, as the GTK front end does
      -- (`initInfo` here forks the heavy load via `postAsyncIDE = forkIDE`, so
      -- this returns promptly).  Without leaving `IsStartingUp`, the metadata
      -- commands (e.g. Update Workspace Info) silently no-op.
      performEvent_ $ ffor pb $ \_ -> liftIO . (`reflectIDE` ideR) $ do
          modifyIDE_ (currentState .~ IsRunning)
          initInfo (return ())
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

-- | All files in the workspace, for the find bar's workspace-tree search.  Uses
-- @git ls-files@ (tracked + untracked, respecting .gitignore — matching what
-- the tree shows) per package directory, falling back to a recursive walk for
-- non-git directories.
enumerateWorkspaceFiles :: Bool -> Bool -> [FilePath] -> IO [FilePath]
enumerateWorkspaceFiles showHidden showIgnored dirs =
    nub . concat <$> mapM enumDir (nub dirs)
  where
    enumDir dir = gitFiles dir `catch` \(_ :: SomeException) -> walkFiles dir
    -- Without --exclude-standard, `--others` lists ignored files too.
    gitArgs = ["ls-files", "--cached", "--others"]
              ++ ["--exclude-standard" | not showIgnored]
    gitFiles dir = do
      (rc, out, _) <- readProcessWithExitCode "git" (["-C", dir] ++ gitArgs) ""
      case rc of
        ExitSuccess -> return $ withDirs dir [ l | l <- lines out, not (null l), notHidden l ]
        _           -> walkFiles dir
    -- The tree hides any entry whose name starts with '.', at any depth.
    notHidden rel = showHidden || not ("." `isPrefixOf` rel || "/." `isInfixOf` rel)
    -- Include each file's containing directories (absolute), so a directory can
    -- itself be a find match.
    withDirs dir rels = nub $
      [ dir </> rel | rel <- rels ] ++ [ dir </> d | rel <- rels, d <- dirPrefixes rel ]
    dirPrefixes rel = case reverse (splitSlash rel) of
      (_file : ds@(_:_)) -> scanl1 (</>) (reverse ds)
      _                  -> []
    splitSlash s = case break (== '/') s of
      (a, "")       -> [a]
      (a, _ : rest) -> a : splitSlash rest
    walkFiles dir = (`catch` \(_ :: SomeException) -> return []) $ do
      names <- filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
      fmap concat $ (`mapM` names) $ \name ->
        if (not showHidden && "." `isPrefixOf` name) || name == "dist-newstyle"
          then return []
          else do
            let p = dir </> name
            isDir <- doesDirectoryExist p
            if isDir then (p :) <$> walkFiles p else return [p]

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
            -- jsaddle-terminal: the per-pane iframe page (the tunnel's JS
            -- runtime); the key is TerminalCC's sanitised pane key.
            ("GET", ["jsaddle-terminal", "frame", k])
              | T.all (\c -> isAlphaNum c || c == '.' || c == '_') k ->
                 sendResponse
                    $ W.responseLBS H.status200
                        [("Content-Type", "text/html; charset=utf-8")]
                    $ bootstrapHtml (LBS.fromStrict (encodeUtf8 k))
            -- jsaddle-terminal: a sync callback's blocking XHR — round-trip
            -- the Results over the pane's stdin/stdout and answer with the
            -- next Batch.  Pure IO throughout (the XHR freezes the page's JS,
            -- so this must never wait on jsaddle) — see IDE.Web.JsaddleTunnel.
            ("POST", ["jsaddle-terminal", "sync", k]) -> do
                body <- W.strictRequestBody req
                mreply <- tunnelSyncRequest k (BS.toStrict body)
                sendResponse $ case mreply of
                  Just batch -> W.responseLBS H.status200
                      [("Content-Type", "application/json")]
                      (LBS.fromStrict batch)
                  Nothing -> W.responseLBS H.status504 [] "no such tunnel"
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
              -- jsaddle-terminal: the per-pane iframe page (see startJSaddle).
              ("GET", ["jsaddle-terminal", "frame", k])
                | T.all (\c -> isAlphaNum c || c == '.' || c == '_') k ->
                   sendResponse
                      $ W.responseLBS H.status200
                          [("Content-Type", "text/html; charset=utf-8")]
                      $ bootstrapHtml (LBS.fromStrict (encodeUtf8 k))
              -- jsaddle-terminal: sync-callback round trip (see startJSaddle).
              ("POST", ["jsaddle-terminal", "sync", k]) -> do
                  body <- W.strictRequestBody req
                  mreply <- tunnelSyncRequest k (BS.toStrict body)
                  sendResponse $ case mreply of
                    Just batch -> W.responseLBS H.status200
                        [("Content-Type", "application/json")]
                        (LBS.fromStrict batch)
                    Nothing -> W.responseLBS H.status504 [] "no such tunnel"
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
    terminalCss
    terminalsCss
    metadataCss
    changesCss
    preferencesCss
    flakeCss

-- Fallback label for a terminal that hasn't reported a window title yet.
-- | A tab's display label (shared by the tab buttons and the flipper).  Terminals
-- are labelled by their tmux session name (looked up by session id in the names
-- map), falling back to the id itself if not yet known.
tabLabelText :: TabKey -> Map Text Text -> Text
tabLabelText k names = case k of
  WorkspaceKey   -> "Workspace"
  ErrorsKey      -> "Errors"
  LogKey         -> "Log"
  GrepKey        -> "Grep"
  TerminalsKey   -> "Terminals"
  TerminalKey n  -> M.findWithDefault n n names
  MetadataKey    -> "Metadata"
  ChangesKey     -> "Changes"
  PreferencesKey -> "Preferences"
  EditorKey file -> T.pack (takeFileName file)

-- | The leading B&W icon (a @/pics/*.svg@ path) for a side-pane tree tab, or
-- 'Nothing' for tabs shown by their label alone (editors, terminals, …).
tabIconSrc :: TabKey -> Maybe Text
tabIconSrc k = case k of
  WorkspaceKey -> Just "/pics/workspace.svg"
  TerminalsKey -> Just "/pics/terminals.svg"
  MetadataKey  -> Just "/pics/metadata.svg"
  _            -> Nothing

-- | The active @(window index, pane index)@ of a tmux session (by id), from the
-- pane tree; falls back to the first window/pane, or 'Nothing' if it has none.
activePaneOfSession :: Text -> Map Text (Text, [TmuxWindow]) -> Maybe (Int, Int)
activePaneOfSession n tree = do
  (_, wins) <- M.lookup n tree
  w <- listToMaybe (filter twActive wins ++ wins)
  p <- listToMaybe (filter tpActive (twPanes w) ++ twPanes w)
  return (twIndex w, tpIndex p)

-- | The flip item for a focused tab (from @activePaneD@ — the last tab
-- mouse-pressed in any area) given a freshly-read pane tree: a terminal resolves
-- to its active tmux pane, any other tab to itself.  Used both to bump the MRU
-- when the user clicks a tab and to refresh the active pane of the terminal
-- currently at the front of the MRU on open (catching a ⌃B switch).
activeFlipFor :: Maybe TabKey -> Map Text (Text, [TmuxWindow]) -> Maybe FlipItem
activeFlipFor mk tree = case mk of
  Just (TerminalKey n) ->
    Just $ maybe (FlipTab (TerminalKey n))
                 (\(w, p) -> FlipPane n w p) (activePaneOfSession n tree)
  Just k  -> Just (FlipTab k)
  Nothing -> Nothing

-- | Flipper label for a tmux pane: "session-name · window-name", with a
-- trailing " · pane-name" only when the window has more than one pane (a
-- single-pane window IS the pane, so its name would just be noise).  Uses the
-- session's current tmux name (from the tree, so a rename shows up); the window
-- and pane names come from 'twLabel'/'tpLabel' with their "@idx: @" prefix
-- stripped.
flipPaneLabel :: Text -> Int -> Int -> Map Text (Text, [TmuxWindow]) -> Text
flipPaneLabel n w p tree =
  maybe n fst (M.lookup n tree) <> " · " <> winName
    <> (if length panes > 1 then " · " <> paneName else "")
  where
    stripIdx i lbl = fromMaybe lbl (T.stripPrefix (T.pack (show i) <> ": ") lbl)
    mbWin   = find ((== w) . twIndex) (maybe [] snd (M.lookup n tree))
    panes   = maybe [] twPanes mbWin
    winName = maybe (T.pack (show w)) (stripIdx w . twLabel) mbWin
    paneName = case [ stripIdx p (tpLabel pn) | pn <- panes, tpIndex pn == p ] of
                 (l:_) -> l
                 []    -> T.pack (show p)

-- | Drop a window/pane label's leading @"idx: "@ prefix — the tab buttons now
-- show the name alone (the index is conveyed by the ⌘-shortcut badge instead).
stripIdxPrefix :: Int -> Text -> Text
stripIdxPrefix i lbl = fromMaybe lbl (T.stripPrefix (T.pack (show i) <> ": ") lbl)

-- | A CSS @order@ style attribute for a wide0 tab button (empty off wide0).
orderStyle :: Maybe Int -> Map Text Text
orderStyle = maybe mempty (\n -> "style" =: ("order:" <> T.pack (show n)))

-- | The side- and bottom-bar panes in their strips' order: the Nth entry is
-- what ⌥⌘N / ⌃⌘N navigates to, and what its ⌘-held badge shows.
numberedTallTabs, numberedWide1Tabs :: [TabKey]
numberedTallTabs  = [WorkspaceKey, TerminalsKey, MetadataKey]
numberedWide1Tabs = [ErrorsKey, LogKey, GrepKey, ChangesKey]

-- | The ⌘-held navigation badge for a side-/bottom-bar tab button (hidden
-- until body.leksah-show-badges; see badgesJs).
tabShortcutBadge :: MonadWidget t m => Text -> TabKey -> m ()
tabShortcutBadge area k = case area of
    "tall"  -> mk "\8997\8984" numberedTallTabs
    "wide1" -> mk "\8963\8984" numberedWide1Tabs
    _       -> return ()
  where
    mk pre ks = forM_ (elemIndex k ks) $ \i ->
        when (i < 9) . elAttr "span" ("class" =: "leksah-shortcut-badge") $
            text (pre <> T.pack (show (i + 1)))

-- | The wide0 tab-button order, taken from the flipper's item list: each tmux
-- window (identified by @Left (session, window)@, collapsed from its panes) and
-- each non-terminal tab (@Right key@), in the flipper's MRU-first order, one
-- entry apiece.  Windows of one session are NOT grouped — each is ordered
-- independently, exactly as the flipper cycles panes.  (The list isn't always
-- perfectly current, as noted for the flipper MRU, but it's close.)
buttonOrderOf :: [(Text, FlipItem)] -> [Either (Text, Int) TabKey]
buttonOrderOf = nub . map (flipButtonId . snd)

-- | The wide0 button identity of a flip item: a tmux window collapses its panes
-- to @Left (session, window)@, a non-terminal tab is @Right key@.  Two panes of
-- one window share an id (one button); this keys both CSS order and ⌘-badges.
flipButtonId :: FlipItem -> Either (Text, Int) TabKey
flipButtonId (FlipPane s w _) = Left (s, w)
flipButtonId (FlipTab k)      = Right k

-- | Collapse a wide0 flip-item list to one @(identity, item)@ per button,
-- keeping the first (MRU-most) item for each identity — the representative fired
-- when its ⌘-shortcut is pressed, and the head is the active (shown) button.
dedupButtons :: [(Text, FlipItem)] -> [(Either (Text, Int) TabKey, FlipItem)]
dedupButtons = go []
  where
    go _ [] = []
    go seen ((_, fi) : rest)
      | i `elem` seen = go seen rest
      | otherwise     = (i, fi) : go (i : seen) rest
      where i = flipButtonId fi

-- | The flipper's item list: MRU order first, then every current tmux pane and
-- non-terminal tab (a terminal is represented only by its panes).
buildFlipItems :: [FlipItem] -> [(Text, TabKey)] -> Map Text (Text, [TmuxWindow]) -> [(Text, FlipItem)]
buildFlipItems mru rt tree =
  let panes = [ FlipPane n (twIndex w) (tpIndex p)
              | (n, (_, wins)) <- M.toList tree, w <- wins, p <- twPanes w ]
      notTerm (TerminalKey _) = False
      notTerm _               = True
      tabs    = [ FlipTab k | (_, k) <- rt, notTerm k ]
      present = panes ++ tabs
      ordered = filter (`elem` present) mru ++ filter (`notElem` mru) present
      areaOf (FlipPane {}) = "wide0"
      areaOf (FlipTab k)   = maybe "wide0" fst (find ((== k) . snd) rt)
  in [ (areaOf fi, fi) | fi <- ordered ]

-- | The highest @N@ among existing @leksah-N@ session names (0 if none), given
-- @(session id, name)@ pairs — so a new terminal is named one past it.
maxLeksahNum :: [(Text, Text)] -> Int
maxLeksahNum xs = foldr max 0
  [ n | (_, name) <- xs
      , Just rest <- [T.stripPrefix "leksah-" name]
      , Just n <- [readMaybe (T.unpack rest)] ]

-- | The first terminal window flagged for attention — bell (a teammate wants
-- input) first, then activity (new output) — as @(session id, window index)@.
-- The jump-to-teammate command targets this; selecting a window clears its flag,
-- so pressing the key repeatedly walks through every flagged window in turn.
firstAlertWindow :: Map Text (Text, [TmuxWindow]) -> Maybe (Text, Int)
firstAlertWindow tree =
  let flagged pick = [ (sid, twIndex w) | (sid, (_, ws)) <- M.toList tree, w <- ws, pick w ]
  in listToMaybe (flagged twBell ++ flagged twActivity)

-- | Defines @window.leksahOccurrenceVisible(path, containerSel)@: true when some
-- node tagged @data-reveal-key=path@ inside @containerSel@ is currently rendered
-- and within that container's viewport.  Scoping by container keeps the
-- workspace and metadata trees from matching each other's nodes.  Used to
-- suppress reveal/scroll when the focused file is already visible (it can appear
-- in a tree more than once).
-- | The single active-pane shadow overlay.  A per-pane '.terminal-cc-hl' marker
-- (shown for the active pane, hidden otherwise) is clipped inside the terminal,
-- so instead one top-level '.leksah-pane-hl' box (appended to <body>, outside
-- all the terminal/tab clipping) copies the VISIBLE marker's screen rect and
-- carries the shadow — which then spills onto the side/bottom bars.
-- 'leksahUpdatePaneHl' (rAF-debounced) recomputes it; callers fire it on focus
-- change, resize, and (from the CC widget) active-pane / layout changes.
paneHlJs :: Text
paneHlJs = T.unlines
  [ "(function(){"
  -- Created lazily (not now): mainWidgetWithCss rebuilds <body> after this runs,
  -- which would wipe an element appended here.  leksahUpdatePaneHl only fires
  -- after the DOM is built, so the overlay survives.
  , "  function getClip(){ var c = document.querySelector('.leksah-pane-hl-clip');"
  , "    if (!c){ c = document.createElement('div'); c.className = 'leksah-pane-hl-clip';"
  , "      c.style.display='none'; var hl = document.createElement('div');"
  , "      hl.className = 'leksah-pane-hl'; c.appendChild(hl); document.body.appendChild(c); }"
  , "    return c; }"
  , "  function shown(el){ if(!el) return false; var cs = getComputedStyle(el);"
  , "    return cs.display !== 'none' && cs.visibility !== 'hidden' && el.offsetParent !== null; }"
  -- The pane to outline = the one holding keyboard focus, in ANY area.  In a
  -- terminal that's the active split's marker (nice divider-aligned rect), with
  -- the focused pane box as fallback; elsewhere it's the focused side-pane /
  -- editor / bottom-bar tab body.  One overlay, so only the focused pane glows.
  , "  function paneOf(ae){"
  , "    if (!ae || !ae.closest) return null;"
  , "    var win = ae.closest('.terminal-cc-window');"
  , "    if (win){ var ms = win.querySelectorAll('.terminal-cc-hl');"
  , "      for (var i=0;i<ms.length;i++){ if (shown(ms[i])) return ms[i]; }"
  , "      return ae.closest('.terminal-cc-pane'); }"
  , "    return ae.closest('.tab.area-tall, .tab.area-wide0, .tab.area-wide1');"
  , "  }"
  , "  function update(){"
  , "    var clip = getClip(), hl = clip.firstChild;"
  , "    var t = paneOf(document.activeElement);"
  , "    if (!t || !shown(t)) { clip.style.display='none'; return; }"
  , "    var r = t.getBoundingClientRect();"
  , "    var top=r.top, bottom=r.bottom, left=r.left, right=r.right;"
  -- A terminal pane's box must never start above the top of the terminal itself
  -- (a marker/pane rect can extend above the visible terminal area); clamp it so
  -- the shadow never falls above the terminal.
  , "    var tcc = t.closest && t.closest('.terminal-cc');"
  , "    if (tcc){ var tccR = tcc.getBoundingClientRect();"
  -- Clamp to the terminal container's top AND left: a pane marker outsets half a
  -- cell onto its dividers, but the LEFTMOST pane has only the side-pane divider
  -- to its left — letting the shadow overshoot there paints a dark strip in the
  -- side pane (looks like the pane doesn't reach the line).  Internal panes have
  -- left > the container edge, so their divider overhang is untouched.
  , "      if (top < tccR.top) top = tccR.top;"
  , "      if (left < tccR.left) left = tccR.left; }"
  -- A bottom-bar pane: extend the rect UP over its own tab row, so the shadow
  -- reaches the terminal above instead of starting below the tab row.
  , "    if (t.closest && t.closest('.area-wide1')){"
  , "      var tb = document.querySelector('.tab-buttons.area-wide1');"
  , "      if (tb) top = tb.getBoundingClientRect().top; }"
  -- The side divider line now sits at the pane's actual right edge, so the shadow
  -- box reaches the full pane width (no trim).
  , "    if (right-left < 1 || bottom-top < 1) { clip.style.display='none'; return; }"
  -- The clip spans the whole viewport (top:0), so the shadow is free to fall on
  -- the tab-button rows above the pane (no top clip).
  , "    clip.style.top='0'; clip.style.left='0'; clip.style.right='0';"
  , "    clip.style.bottom='0'; clip.style.display='block';"
  , "    hl.style.left=left+'px'; hl.style.top=top+'px';"
  , "    hl.style.width=(right-left)+'px'; hl.style.height=(bottom-top)+'px';"
  , "  }"
  , "  var scheduled = false;"
  , "  window.leksahUpdatePaneHl = function(){ if(scheduled) return; scheduled=true;"
  , "    requestAnimationFrame(function(){ scheduled=false; update(); }); };"
  , "  document.addEventListener('focusin', window.leksahUpdatePaneHl, true);"
  , "  window.addEventListener('resize', window.leksahUpdatePaneHl);"
  -- The overlay is absolutely positioned from the pane's live rect, so when a
  -- pane slides (auto show/hide of the sidebar / bottom bar is a CSS
  -- transition, ~0.15s) a single update() would leave the shadow behind at the
  -- old spot.  While any transition/animation is running, run a bounded rAF
  -- loop that re-runs update() every frame so the shadow rides along with it.
  -- The deadline is time-bounded (refreshed only by *start* events) so the loop
  -- always terminates even if a transitionend/cancel is missed.
  , "  function now(){ return (window.performance && performance.now) ? performance.now() : 0; }"
  , "  var deadline = 0, looping = false;"
  , "  function loop(){ update(); if (now() < deadline) { requestAnimationFrame(loop); } else { looping = false; } }"
  , "  function kick(){ var d = now() + 500; if (d > deadline) deadline = d;"
  , "    if (!looping) { looping = true; requestAnimationFrame(loop); } }"
  , "  document.addEventListener('transitionrun', kick, true);"
  , "  document.addEventListener('transitionstart', kick, true);"
  , "  document.addEventListener('animationstart', kick, true);"
  , "})();"
  ]

revealCheckJs :: Text
revealCheckJs = T.unlines
  [ "window.leksahOccurrenceVisible = function(path, containerSel){"
  , "  try {"
  , "    var container = document.querySelector(containerSel);"
  , "    if (!container) return false;"
  , "    var cr = container.getBoundingClientRect();"
  , "    var els = container.querySelectorAll('[data-reveal-key=' + JSON.stringify(path) + ']');"
  , "    for (var i=0;i<els.length;i++){"
  , "      var el = els[i];"
  , "      if (el.offsetParent === null) continue;"
  , "      var r = el.getBoundingClientRect();"
  , "      if (r.width === 0 && r.height === 0) continue;"
  , "      if (r.bottom > cr.top && r.top < cr.bottom && r.right > cr.left && r.left < cr.right) return true;"
  , "    }"
  , "  } catch(e) {}"
  , "  return false;"
  , "};"
  ]

-- | Defines @window.leksahFocusFind@: focus the find bar's text input.  Run on
-- the next animation frame so the input (just un-hidden by Edit ▸ Find) is laid
-- out — focusing a still-@display:none@ element is a silent no-op.
focusFindJs :: Text
focusFindJs = T.unlines
  [ "window.leksahFocusFind = function(){"
  , "  requestAnimationFrame(function(){"
  , "    var e = document.querySelector('.findbar input.find-text');"
  , "    if (e) { e.focus(); e.select(); }"
  , "  });"
  , "};"
  ]

-- | Defines @window.leksahFocusPane(selector)@: give a side/bottom list pane
-- keyboard focus when it is activated.  Deferred to the next animation frame (the
-- bar may have just been un-hidden).  The container is made programmatically
-- focusable (@tabindex=-1@) so it can hold focus; for a keyboard-navigable list
-- (@.leksah-nav@) it also marks a current row if none is set yet, so Up/Down have
-- a starting point (see @listNavJs@).
focusPaneJs :: Text
focusPaneJs = T.unlines
  [ "window.leksahFocusPane = function(sel){"
  , "  var tries = 0;"
  , "  function go(){"
  , "    var el = document.querySelector(sel);"
  , "    if (el) {"
  , "      if (!el.hasAttribute('tabindex')) el.setAttribute('tabindex', '-1');"
  , "      el.focus();"
  , "      if (el.classList.contains('leksah-nav') && !el.querySelector('.leksah-nav-item.leksah-nav-current')) {"
  , "        var first = el.querySelector('.leksah-nav-item');"
  , "        if (first) first.classList.add('leksah-nav-current');"
  , "      }"
  , "    }"
     -- The bar may still be expanding (pref change -> CSS); retry briefly until
     -- the element actually takes focus.
  , "    if ((!el || document.activeElement !== el) && tries++ < 12) setTimeout(go, 30);"
  , "  }"
  , "  requestAnimationFrame(go);"
  , "};"
  ]

-- | Keyboard list navigation for the focused side/bottom pane.  A document
-- keydown handler that acts only when focus is inside a list pane:
--
--   * @.leksah-nav@ panes (trees, Grep, Changes — all rendered in full): Up/Down
--     move a @.leksah-nav-current@ highlight through the rendered @.leksah-nav-item@
--     rows; Enter/Space click the current row (reusing its existing handler);
--     Left/Right expand/collapse a tree node (clicking its @.tree-expand@).
--   * @.leksah-vlist[data-pane]@ panes (the virtualized Errors/Log): Up/Down/Enter
--     call back into reflex via @leksahListMove@/@leksahListActivate@ (registered
--     in the reflex network), which moves the list's selection index + scrolls.
--
-- Modified chords (⌘/⌃/⌥) are left to the keymap; plain arrows elsewhere (editor,
-- terminal, find input) are untouched because focus isn't in a list pane.
listNavJs :: Text
listNavJs = T.unlines
  [ "(function(){"
  , "  function items(p){ return Array.prototype.slice.call(p.querySelectorAll('.leksah-nav-item')); }"
  , "  document.addEventListener('keydown', function(e){"
  , "    if (e.metaKey || e.ctrlKey || e.altKey) return;"
  , "    var a = document.activeElement;"
  -- An editable field inside a nav pane (e.g. the Terminals-tree rename box) owns
  -- its own keys: arrows must move the caret, Enter/Space type/commit — not drive
  -- tree navigation.  Bail out so the browser handles them natively.
  , "    if (a && (a.tagName === 'INPUT' || a.tagName === 'TEXTAREA' || a.isContentEditable)) return;"
  , "    var nav = a && a.closest && a.closest('.leksah-nav');"
  , "    if (nav) {"
  , "      var its = items(nav); if (!its.length) return;"
  , "      var cur = nav.querySelector('.leksah-nav-item.leksah-nav-current');"
  , "      var i = cur ? its.indexOf(cur) : -1;"
  , "      if (e.key === 'ArrowDown') i = Math.min(its.length - 1, i + 1);"
  , "      else if (e.key === 'ArrowUp') i = (i <= 0 ? 0 : i - 1);"
  , "      else if (e.key === 'Enter' || e.key === ' ') {"
  , "        if (cur) { cur.click();"
  -- A workspace file row opens on double-click (single click just selects it),
  -- so Enter/Space on a file must synthesise a dblclick to open it in the editor.
  , "          if (cur.closest('li.file')) cur.dispatchEvent(new MouseEvent('dblclick', {bubbles:true, cancelable:true, view:window})); }"
  , "        e.preventDefault(); return; }"
  , "      else if (e.key === 'ArrowRight' || e.key === 'ArrowLeft') {"
  , "        if (cur) { var li = cur.closest('li');"
  , "          var ex = li && li.querySelector(':scope > .tree-expand');"
  , "          var open = li && li.querySelector(':scope > .tree-children');"
  , "          if (ex && ((e.key === 'ArrowRight' && !open) || (e.key === 'ArrowLeft' && open))) ex.click(); }"
  , "        e.preventDefault(); return; }"
  , "      else return;"
  , "      if (i < 0) i = 0;"
  , "      its.forEach(function(it){ it.classList.remove('leksah-nav-current'); });"
  , "      var sel = its[i]; sel.classList.add('leksah-nav-current'); sel.scrollIntoView({ block: 'nearest' });"
  , "      e.preventDefault(); return;"
  , "    }"
  , "    var vl = a && a.closest && a.closest('.leksah-vlist');"
  , "    if (vl && window.leksahListMove) {"
  , "      var pane = vl.getAttribute('data-pane');"
  , "      if (e.key === 'ArrowDown') { window.leksahListMove(pane, 'down'); e.preventDefault(); }"
  , "      else if (e.key === 'ArrowUp') { window.leksahListMove(pane, 'up'); e.preventDefault(); }"
  , "      else if (e.key === 'Enter') { window.leksahListActivate(pane); e.preventDefault(); }"
  , "    }"
  , "  }, false);"
  -- Clicking a row makes it the current row so Up/Down continue from there.
  , "  document.addEventListener('click', function(e){"
  , "    var it = e.target.closest && e.target.closest('.leksah-nav-item');"
  , "    if (!it) return; var p = it.closest('.leksah-nav'); if (!p) return;"
  , "    items(p).forEach(function(x){ x.classList.remove('leksah-nav-current'); });"
  , "    it.classList.add('leksah-nav-current');"
  , "  }, false);"
  , "})();"
  ]

-- | A document @focusin@ listener: whenever focus lands anywhere inside a tab
-- body (which carries its @show@-key as @data-tabkey@; see Tabs.hs), report that
-- key to reflex via @leksahListMove@'s sibling @leksahFocusTab@, which moves the
-- tab to the front of the MRU/flipper order.  Using @focusin@ (which bubbles)
-- catches focus arriving by any route — click, keyboard, or the programmatic
-- focus a terminal takes at start-up.
focusTabJs :: Text
focusTabJs = T.unlines
  [ "(function(){"
  , "  document.addEventListener('focusin', function(e){"
  , "    var t = e.target.closest && e.target.closest('.tab[data-tabkey]');"
  , "    if (t && window.leksahFocusTab) window.leksahFocusTab(t.getAttribute('data-tabkey'));"
  , "  }, true);"
  , "})();"
  ]

-- | A document listener that pokes reflex (@leksahTermActivity@) when the active
-- tmux pane may have just changed inside a terminal: a mouse-down in a @.terminal@
-- (clicking a split), or the tmux prefix ⌃B (a pane command may follow — polled
-- after a short delay so the chord has completed).  Reflex re-reads the pane tree
-- so the flipper's pane list / MRU stay current.
termActivityJs :: Text
termActivityJs = T.unlines
  [ "(function(){"
  , "  function poke(){ if (window.leksahTermActivity) window.leksahTermActivity(); }"
  , "  document.addEventListener('mousedown', function(e){"
  , "    if (e.target.closest && e.target.closest('.terminal')) setTimeout(poke, 60);"
  , "  }, true);"
  , "  document.addEventListener('keydown', function(e){"
  , "    if (e.ctrlKey && (e.key === 'b' || e.key === 'B') && e.target.closest"
  , "        && e.target.closest('.terminal')) setTimeout(poke, 250);"
  , "  }, true);"
  , "})();"
  ]

-- | Defines @window.LeksahTermLinks@, which makes file paths in xterm.js
-- terminal output into clickable links to the matching project file.
--
--   * @setProjectFiles(paths)@ — called from Haskell whenever the workspace file
--     set changes; indexes the absolute paths by basename so the link provider
--     can tell which path-like tokens are real project files.
--   * @attach(term, onOpen, onLookup)@ — registers an xterm link provider on a
--     terminal.  Without a modifier: matching project-file paths underline on
--     hover and clicking calls @onOpen(absPath, line, col)@.  While Ctrl/Cmd is
--     held: any identifier underlines and clicking calls @onLookup(token, x, y)@
--     (a metadata lookup, with the click position for a chooser popup).
terminalLinksJs :: Text
terminalLinksJs = T.unlines
  [ "window.LeksahTermLinks = (function(){"
  , "  var byBase = new Map();"
  , "  var ctrlHeld = false;"
  -- When off (a preference), the provider claims no links, so OSC 8 hyperlinks in
  -- the output aren't shadowed by our file/identifier matching.
  , "  var enabled = true;"
  , "  function setEnabled(v){ enabled = !!v; }"
  , "  window.addEventListener('keydown', function(e){ if (e.key==='Control'||e.key==='Meta') ctrlHeld=true; }, true);"
  , "  window.addEventListener('keyup',   function(e){ if (e.key==='Control'||e.key==='Meta') ctrlHeld=false; }, true);"
  , "  window.addEventListener('blur',    function(){ ctrlHeld=false; }, true);"
  , "  function setProjectFiles(paths){"
  , "    byBase = new Map();"
  , "    for (var i=0;i<paths.length;i++){"
  , "      var p = paths[i];"
  , "      var b = p.substring(p.lastIndexOf('/')+1);"
  , "      var a = byBase.get(b);"
  , "      if (a) a.push(p); else byBase.set(b,[p]);"
  , "    }"
  , "  }"
  , "  function resolve(tok){"
  , "    if (!tok) return null;"
  , "    if (tok.slice(0,2)==='./') tok = tok.slice(2);"
  , "    var b = tok.substring(tok.lastIndexOf('/')+1);"
  , "    var c = byBase.get(b);"
  , "    if (!c || !c.length) return null;"
  , "    if (tok.indexOf('/') >= 0){"
  , "      for (var i=0;i<c.length;i++){ if (c[i]===tok || c[i].endsWith('/'+tok)) return c[i]; }"
  , "      return null;"
  , "    }"
  , "    return c.length===1 ? c[0] : null;"
  , "  }"
  , "  var RE = /(\\/?(?:[\\w.+\\-]+\\/)*[\\w.+\\-]+\\.[A-Za-z][\\w]*)(?::(\\d+))?(?::(\\d+))?/g;"
  -- Claude Code edit headers, e.g. \"Update(src/Foo.hs)\"; a diff follows.
  , "  var UM = /Update\\(([^)]+)\\)/g;"
  -- Identifier tokens (optionally module-qualified) for the Ctrl/Cmd lookup mode.
  , "  var ID = /[A-Za-z_][A-Za-z0-9_']*(?:\\.[A-Za-z_][A-Za-z0-9_']*)*/g;"
  , "  function attach(term, onOpen, onLookup){"
  , "    if (!term || !term.registerLinkProvider) return;"
  , "    function mkLink(sx, ex, y, txt, f, l, c){"
  , "      return {"
  , "        range: { start: { x: sx, y: y }, end: { x: ex, y: y } },"
  , "        text: txt,"
  , "        decorations: { pointerCursor: true, underline: true },"
  , "        activate: function(ev){ if (ev.preventDefault) ev.preventDefault(); onOpen(f,l,c); }"
  , "      };"
  , "    }"
  , "    function mkLook(sx, ex, y, tok){"
  , "      return {"
  , "        range: { start: { x: sx, y: y }, end: { x: ex, y: y } },"
  , "        text: tok,"
  , "        decorations: { pointerCursor: true, underline: true },"
  , "        activate: function(ev){ if (ev.preventDefault) ev.preventDefault(); onLookup(tok, (ev&&ev.clientX)||0, (ev&&ev.clientY)||0); }"
  , "      };"
  , "    }"
  , "    term.registerLinkProvider({ provideLinks: function(y, cb){"
  , "      try {"
  , "        if (!enabled){ cb(undefined); return; }"
  , "        var buf = term.buffer && term.buffer.active;"
  , "        var line = buf && buf.getLine(y-1);"
  , "        if (!line){ cb(undefined); return; }"
  , "        var text = line.translateToString(true);"
  , "        var links = [];"
  , "        var consumed = [];"
  -- Ctrl/Cmd held: identifier-lookup mode -- every identifier is clickable and
  -- clicking looks it up in the metadata.  Otherwise fall through to file links.
  , "        if (ctrlHeld) {"
  , "          ID.lastIndex = 0;"
  , "          var im;"
  , "          while ((im = ID.exec(text))){ links.push(mkLook(im.index+1, im.index+im[0].length, y, im[0])); }"
  , "          cb(links.length ? links : undefined);"
  , "          return;"
  , "        }"
  -- Update(file): the diff starts a couple of lines below.  Take the line
  -- number at the start of the line-after-next and jump to it + 3 (the likely
  -- first changed line, after the leading context).
  , "        UM.lastIndex = 0;"
  , "        var u;"
  , "        while ((u = UM.exec(text))){"
  , "          var uf = resolve(u[1].trim());"
  , "          if (!uf) continue;"
  , "          var target = 1;"
  , "          var peek = buf.getLine(y+1);"
  , "          if (peek){ var pm = /^\\s*(\\d+)/.exec(peek.translateToString(true)); if (pm) target = parseInt(pm[1],10)+3; }"
  , "          links.push(mkLink(u.index+1, u.index+u[0].length, y, u[0], uf, target, 1));"
  , "          consumed.push([u.index, u.index+u[0].length]);"
  , "        }"
  , "        RE.lastIndex = 0;"
  , "        var m;"
  , "        while ((m = RE.exec(text))){"
  , "          var skip = false;"
  , "          for (var k=0;k<consumed.length;k++){ if (m.index>=consumed[k][0] && m.index<consumed[k][1]){ skip=true; break; } }"
  , "          if (skip) continue;"
  , "          var abs = resolve(m[1]);"
  , "          if (!abs) continue;"
  , "          var ln = m[2] ? parseInt(m[2],10) : 1;"
  , "          var col = m[3] ? parseInt(m[3],10) : 1;"
  , "          links.push(mkLink(m.index+1, m.index+m[0].length, y, m[0], abs, ln, col));"
  , "        }"
  , "        cb(links.length ? links : undefined);"
  , "      } catch(e){ cb(undefined); }"
  , "    }});"
  , "  }"
  , "  return { setProjectFiles: setProjectFiles, attach: attach, setEnabled: setEnabled };"
  , "})();"
  ]

-- | Defines @window.LeksahOscLinks@: builds an xterm @linkHandler@ for OSC 8
-- hyperlinks (which tmux forwards when the @hyperlinks@ terminal feature is on).
-- Hovering shows a floating tooltip with the destination URL; clicking an
-- http(s) link calls back @onOpen(url, cmdHeld)@ — leksah opens it in the browser
-- and snaps the window over the pane only when Command was held.  Unlike the text
-- link provider (LeksahTermLinks) these links come from the escape stream itself,
-- so no regex/decoration is involved.
terminalOscLinksJs :: Text
terminalOscLinksJs = T.unlines
  [ "window.LeksahOscLinks = (function(){"
  , "  var tip = null;"
  , "  function ensureTip(){"
  , "    if (!tip){"
  , "      tip = document.createElement('div');"
  , "      tip.className = 'leksah-osc-tip';"
  , "      tip.style.cssText = 'position:fixed;z-index:99999;pointer-events:none;'"
  , "        + 'background:rgb(40,40,40);color:#dcdcdc;border:1px solid rgb(80,80,80);'"
  , "        + 'border-radius:3px;padding:2px 6px;font-size:12px;max-width:60ch;'"
  , "        + 'overflow:hidden;text-overflow:ellipsis;white-space:nowrap;display:none';"
  , "      document.body.appendChild(tip);"
  , "    }"
  , "    return tip;"
  , "  }"
  , "  function makeHandler(onOpen, onOpenFile){"
  , "    return {"
  , "      allowNonHttpProtocols: true,"
  , "      hover: function(ev, text){"
  , "        var t = ensureTip();"
  , "        t.textContent = text;"
  , "        t.style.left = (((ev && ev.clientX) || 0) + 12) + 'px';"
  , "        t.style.top  = (((ev && ev.clientY) || 0) + 16) + 'px';"
  , "        t.style.display = 'block';"
  , "      },"
  , "      leave: function(){ if (tip) tip.style.display = 'none'; },"
  , "      activate: function(ev, text){"
  , "        if (tip) tip.style.display = 'none';"
  , "        if (/^https?:\\/\\//i.test(text)) { onOpen(text, !!(ev && ev.metaKey)); return; }"
  -- file:// opens in a CodeMirror editor, not the browser.  Parse with the URL
  -- API (decodes %20 etc.); an optional #Lnn / #nn fragment gives a line.
  , "        if (/^file:\\/\\//i.test(text)) {"
  , "          try {"
  , "            var u = new URL(text);"
  , "            var m = /(\\d+)/.exec(u.hash || '');"
  , "            onOpenFile(decodeURIComponent(u.pathname), m ? parseInt(m[1], 10) : 0, 0);"
  , "          } catch(e){}"
  , "        }"
  , "      }"
  , "    };"
  , "  }"
  , "  return { makeHandler: makeHandler };"
  , "})();"
  ]

-- | Defines @window.LeksahTerm@, the bridge for streaming shell output into the
-- xterm.js terminals (see 'IDE.Web.Widget.Terminal').
--
--   * @register(id, term)@ / @unregister(id)@ — keep a registry of the live
--     terminals keyed by their leksah terminal id, so output can be routed by id
--     (rather than holding a JS handle on the Haskell side across threads).
--   * @write(id, b64)@ — decode a base64 chunk of shell output to a @Uint8Array@
--     and hand the raw bytes to that terminal's @term.write@.  Passing bytes (not
--     a decoded string) lets xterm do its own UTF-8 decoding, which is stateful
--     and so copes with sequences split across PTY reads.
terminalWriteJs :: Text
terminalWriteJs = T.unlines
  [ "window.LeksahTerm = (function(){"
  , "  var byId = {};"
  , "  function register(id, term){ byId[id] = term; }"
  , "  function unregister(id){ delete byId[id]; }"
  , "  function write(id, b64){"
  , "    var term = byId[id];"
  , "    if (!term) return;"
  , "    var bin = atob(b64), n = bin.length, a = new Uint8Array(n);"
  , "    for (var i=0;i<n;i++) a[i] = bin.charCodeAt(i);"
  , "    term.write(a);"
  , "  }"
  -- The terminal cell size (CSS px) for leksah's font settings — measured
  -- ONCE from a throwaway offscreen xterm (the browser equivalent of reading
  -- font metrics; it also captures xterm's own rounding, which is the real
  -- authority).  Cached; warmed at startup so callers see it synchronously.
  -- Everything downstream is feed-forward from this (grid = floor(box/cell)),
  -- the way iTerm2/Ghostty size their grids — never render-then-correct.
  , "  var cellCache = null;"
  , "  function cellMetrics(){"
  , "    if (cellCache) return cellCache;"
  , "    try {"
  , "      var host = document.createElement('div');"
  , "      host.style.cssText = 'position:fixed;left:-10000px;top:0;width:900px;height:700px;';"
  , "      document.body.appendChild(host);"
  , "      var t = new Terminal({cols: 80, rows: 24});"
  , "      t.options.fontFamily = 'Menlo, Monaco, \"Courier New\", monospace';"
  , "      t.options.fontSize = 13;"
  , "      t.open(host);"
  , "      var s = host.querySelector('.xterm-screen');"
  , "      var r = s ? s.getBoundingClientRect() : null;"
  , "      if (r && r.width && r.height) cellCache = { w: r.width / 80, h: r.height / 24 };"
  , "      t.dispose();"
  , "      document.body.removeChild(host);"
  , "    } catch (e) {}"
  , "    return cellCache;"
  , "  }"
  , "  if (window.requestAnimationFrame) requestAnimationFrame(function(){ cellMetrics(); });"
  , "  return { register: register, unregister: unregister, write: write, cellMetrics: cellMetrics, byId: byId };"
  , "})();"
  ]

-- | window.LeksahJsaddlePane — the jsaddle-terminal iframe registry, keyed by
-- the sanitised pane key.  register(key, iframe, cb): cb(type, seq, data) is
-- called for the iframe's 'ready'/'results' messages; runBatch(key, json)
-- posts a batch into the iframe.  One page-level 'message' listener does all
-- the routing (iframes identify themselves by key in every message).
jsaddlePaneJs :: Text
jsaddlePaneJs = T.unlines
  [ "window.LeksahJsaddlePane = (function(){"
  , "  var reg = {};"
  -- Re-dispatch a key chord forwarded from an iframe as a synthetic event on
  -- the host document, so the global keymap listener (keymapWidget) sees it.
  -- keyCode isn't settable via the constructor, so shadow it (and `which`) with
  -- an own getter — keymapWidget reads event.keyCode.
  , "  function redispatchKey(d){"
  , "    var ev = new KeyboardEvent(d.kind, { bubbles: true, cancelable: true,"
  , "      key: d.key, code: d.code, ctrlKey: d.ctrl, shiftKey: d.shift,"
  , "      altKey: d.alt, metaKey: d.meta });"
  , "    try { Object.defineProperty(ev, 'keyCode', { get: function(){ return d.keyCode; } });"
  , "          Object.defineProperty(ev, 'which',   { get: function(){ return d.keyCode; } }); } catch(_){}"
  , "    document.dispatchEvent(ev);"
  , "  }"
  , "  window.addEventListener('message', function(e){"
  , "    var d = e.data;"
  , "    if (!d || !d.leksahJsaddle) return;"
  , "    if (d.type === 'key') { redispatchKey(d); return; }"
  , "    var r = reg[d.leksahJsaddle];"
  , "    if (r && r.cb) r.cb(d.type || '', d.seq || 0, d.data || '');"
  , "  });"
  , "  return {"
  , "    register: function(key, iframe, cb){ reg[key] = { iframe: iframe, cb: cb }; },"
  , "    unregister: function(key){ delete reg[key]; },"
  , "    runBatch: function(key, json){"
  , "      var r = reg[key];"
  , "      if (r && r.iframe && r.iframe.contentWindow)"
  , "        r.iframe.contentWindow.postMessage({leksahJsaddleBatch: key, batch: json}, '*');"
  , "    },"
  -- Give the pane's iframe keyboard focus when its pane is activated
  -- (⌘-nav, flipper, tab select).  Skip when it already holds focus (the
  -- iframe element IS document.activeElement) so a click INSIDE the app
  -- isn't yanked back to the frame root.
  , "    focus: function(key){"
  , "      var r = reg[key];"
  , "      if (r && r.iframe && r.iframe.contentWindow && document.activeElement !== r.iframe)"
  , "        r.iframe.contentWindow.focus();"
  , "    }"
  , "  };"
  , "})();"
  ]

-- | Defines @window.LeksahDividerDrag.arm(el, vert, cellPx)@: drag-to-resize
-- for a CC pane divider.  On mousedown the inner 1px line ghosts along with
-- the pointer (pure JS — smooth); on mouseup the travelled distance is
-- rounded to whole cells and passed to the divider's @__leksahResize@
-- callback (set from Haskell), which runs @resize-pane@ over the control
-- channel.  The layout-change notification then re-renders the panes at
-- their new rectangles, snapping the ghost to the grid.
dividerDragJs :: Text
dividerDragJs = T.unlines
  [ "window.LeksahDividerDrag = { arm: function(el, vert, cellPx){"
  , "  el.addEventListener('mousedown', function(e){"
  , "    if (e.button !== 0) return;"
  , "    e.preventDefault(); e.stopPropagation();"
  , "    var start = vert ? e.clientX : e.clientY;"
  , "    var line = el.firstChild;"
  , "    el.classList.add('dragging');"
  -- A drag tracks mousemove on the parent document, but the cursor crosses
  -- adjacent panes — and a tunnel iframe would swallow those events (they go
  -- to ITS document, not ours), freezing the drag.  Make every iframe
  -- click-through for the duration; restored on mouseup.
  , "    var iframes = document.querySelectorAll('.terminal-cc-iframe');"
  , "    iframes.forEach(function(f){ f.style.pointerEvents = 'none'; });"
  , "    function mv(e2){"
  , "      var d = (vert ? e2.clientX : e2.clientY) - start;"
  , "      if (line) line.style.transform = vert ? ('translateX('+d+'px)') : ('translateY('+d+'px)');"
  , "    }"
  , "    function up(e2){"
  , "      document.removeEventListener('mousemove', mv);"
  , "      document.removeEventListener('mouseup', up);"
  , "      iframes.forEach(function(f){ f.style.pointerEvents = ''; });"
  , "      el.classList.remove('dragging');"
  , "      if (line) line.style.transform = '';"
  , "      var cells = Math.round(((vert ? e2.clientX : e2.clientY) - start) / cellPx);"
  , "      if (cells !== 0 && el.__leksahResize) el.__leksahResize(cells);"
  , "    }"
  , "    document.addEventListener('mousemove', mv);"
  , "    document.addEventListener('mouseup', up);"
  , "  });"
  , "} };"
  ]

-- | Defines @window.leksahSetHoles@ / @leksahClearHoles@, which punch
-- see-through holes into the window where a transparent tmux pane is (macOS; see
-- the native side in @main/leksah-mac-menu.m@).  Given the holed panes' cell
-- rectangles (terminal id + tmux pane left/top/width/height in cells), it works
-- out each one's viewport-pixel rect from the terminal's grid (the
-- @.xterm-screen@ box divided by cols/rows), clips those rects out of the page
-- root with @clip-path@ — which clips the element's whole subtree, the WebGL
-- terminal canvas included, so the region becomes truly transparent — and
-- publishes the rects as @window.__leksahHoles@ for the native click-through
-- code to read.
transparencyJs :: Text
transparencyJs = T.unlines
  [ "window.__leksahHoles = [];"
  -- Snap state: the pick mode for the *next* bind (\"click\" = menu, click a
  -- window; \"frontmost\" = open-browser) and the pane key that window binds to.
  , "window.__leksahSnap = { armed: false, key: null };"
  -- Whether macOS Accessibility is granted (kept up to date by the native side);
  -- the snap only makes a pane transparent when this is true.
  , "window.__leksahAxTrusted = false;"
  , "window.leksahArmSnap = function(mode, key){ window.__leksahSnap.armed = mode || \"click\"; window.__leksahSnap.key = key || null; };"
  -- Per snapped pane (keyed \"tid:pid\"): its tmux-cell spec, and the rect to hold
  -- its bound window at while an auto-hide bar is revealed (so it doesn't lurch).
  , "window.__leksahSnapHoles = {};"
  , "window.__leksahSnapFrozen = {};"
  -- Every snapped pane key (even ones whose terminal is currently hidden), so the
  -- native side keeps those windows bound; leksahSnapRects only returns visible ones.
  , "window.__leksahSnapKeys = [];"
  , "window.leksahSetSnapKeys = function(json){ try { window.__leksahSnapKeys = JSON.parse(json); } catch(e) { window.__leksahSnapKeys = []; } };"
  -- True while an auto-hide side/bottom bar is transiently revealed.  Stays true
  -- through the collapse *animation* too (keyed off the rendered size, not :hover),
  -- so a mid-transition rect isn't used; in show/hide mode (no -auto) it's false.
  , "window.leksahAutoExpanded = function(){"
  , "  var root = document.querySelector('.leksah'); if (!root) return false;"
  , "  var a = root.classList.contains('tall-auto') && document.querySelector('.area-tall');"
  , "  if (a && a.getBoundingClientRect().width > 8) return true;"
  -- The wide1 bar reveals by transform (its size never changes), so
  -- "expanded" is positional: any part of it above the statusbar line.
  , "  var b = root.classList.contains('wide1-auto') && document.querySelector('.tab-buttons.area-wide1');"
  , "  if (b && b.getBoundingClientRect().top < window.innerHeight - 22) return true;"
  , "  return false;"
  , "};"
  -- Viewport-px rect of a tmux pane (cell spec h) from the *live* terminal grid,
  -- or null if its terminal isn't ready.
  , "window.leksahComputeHole = function(h){"
  , "  var reg = window.LeksahTerm && window.LeksahTerm.byId;"
  , "  if (!reg) return null;"
  -- Control-mode tabs register one xterm PER PANE (key \"tid/pid\" — see
  -- paneKey in IDE.Web.Widget.TerminalCC); its element IS the pane, so the
  -- hole is simply that element's box (zero-sized while its window is hidden
  -- -> no hole).  Classic tabs register the whole session under tid and need
  -- the cell arithmetic below.
  , "  if (!reg[h.term] && h.pane) {"
  , "    var pt = reg[h.term + '/' + h.pane];"
  , "    if (!pt || !pt.element) return null;"
  , "    var fr = pt.element.getBoundingClientRect();"
  , "    return (fr.width > 0 && fr.height > 0)"
  , "      ? { x: fr.left, y: fr.top, w: fr.width, h: fr.height } : null;"
  , "  }"
  , "  var term = reg && reg[h.term];"
  , "  if (!term || !term.element || !term.cols || !term.rows) return null;"
  , "  var screen = term.element.querySelector('.xterm-screen');"
  , "  if (!screen) return null;"
  , "  var r = screen.getBoundingClientRect();"
  , "  var cw = r.width / term.cols, ch = r.height / term.rows;"
  , "  var x = r.left + h.left*cw, y = r.top + h.top*ch;"
  , "  var x2 = x + h.w*cw, y2 = y + h.h*ch;"
     -- The grid (.xterm-screen) is exactly cols x rows cells, a little smaller than
     -- the terminal pane; against a grid edge, extend to the pane element's edge so
     -- no opaque sliver is left (and so a full-pane snap tracks the live element).
  , "  var host = term.element.closest('.terminal') || term.element;"
  , "  var tr = host.getBoundingClientRect();"
  , "  if (h.left <= 0) x = Math.min(x, tr.left);"
  , "  if (h.top <= 0) y = Math.min(y, tr.top);"
  , "  if (h.left + h.w >= term.cols) x2 = Math.max(x2, tr.right);"
  , "  if (h.top + h.h >= term.rows) y2 = Math.max(y2, tr.bottom);"
  , "  return { x: x, y: y, w: x2 - x, h: y2 - y };"
  , "};"
  -- Fresh viewport rect per snapped pane (key -> rect), recomputed live so bound
  -- windows track leksah's own move/resize — or the frozen rect while revealing.
  , "window.leksahSnapRects = function(){"
  , "  var out = {}, rev = window.leksahAutoExpanded();"
  , "  for (var k in window.__leksahSnapHoles) {"
  , "    var rect = (rev && window.__leksahSnapFrozen[k]) ? window.__leksahSnapFrozen[k]"
  , "                                                     : window.leksahComputeHole(window.__leksahSnapHoles[k]);"
  , "    if (rect) out[k] = rect;"
  , "  }"
  , "  return out;"
  , "};"
  , "window.leksahSetHoles = function(json){"
  , "  var holes = []; try { holes = JSON.parse(json); } catch(e) { holes = []; }"
  , "  var rects = [];"
  , "  var revealing = window.leksahAutoExpanded();"
  , "  window.__leksahSnapHoles = {};"
  , "  holes.forEach(function(h){"
  , "    var rect = window.leksahComputeHole(h);"
  , "    if (!rect) return;"
     -- The clip-path hole always uses the *live* rect (even for a snapped pane),
     -- so it tracks the reveal/collapse transition and never clips a bar sliding in
     -- next to it (a frozen hole would punch the revealing bar transparent).
  , "    rects.push(rect);"
  , "    if (h.snap && h.key) {"
  , "      window.__leksahSnapHoles[h.key] = h;"
     -- Hold each snapped *window* at its pre-reveal rect for the duration of a
     -- reveal (leksahSnapRects returns this while revealing); update only when not.
  , "      if (!revealing) window.__leksahSnapFrozen[h.key] = rect;"
  , "    }"
  , "  });"
  , "  window.__leksahHoles = rects;"
  , "  var root = document.documentElement;"
  , "  if (!rects.length) { root.style.clipPath = ''; return; }"
  , "  var W = window.innerWidth, H = window.innerHeight;"
  , "  var d = 'M0,0 H'+W+' V'+H+' H0 Z';"
  , "  rects.forEach(function(r){ d += ' M'+r.x+','+r.y+' h'+r.w+' v'+r.h+' h'+(-r.w)+' Z'; });"
  , "  root.style.clipPath = \"path(evenodd, '\"+d+\"')\";"
  , "};"
  , "window.leksahClearHoles = function(){ window.__leksahHoles = []; document.documentElement.style.clipPath = ''; };"
  ]

-- | Shows the navigation shortcut badges while the Command key is held —
-- IF the preference turned them on (the flag below, kept current from
-- prefs).  The badges themselves are pre-rendered, hidden elements
-- (.leksah-shortcut-badge): CC panes lay theirs at each pane's top-left
-- corner; side-/bottom-bar tab buttons carry theirs inline.

-- | window.leksahSetColorIcons(on): switch every @\/pics\/*.svg@ icon between
-- the monochrome default and the coloured set at @\/pics\/color\/*.svg@ (same
-- shapes, recoloured).  Icons are plain @<img>@s scattered across many
-- widgets and added/removed dynamically (tree nodes, tabs), so rather than
-- thread the pref through every call site this rewrites the @src@ in place and
-- a MutationObserver keeps newly-added or reflex-updated icons in the current
-- mode.  Rewrites are idempotent (a src already in the target form is skipped),
-- so the observer seeing our own change doesn't loop.
colorIconsJs :: Text
colorIconsJs = T.unlines
  [ "window.__leksahColorIcons = false;"
  , "(function(){"
  , "  var BASE = '/pics/', COLOR = '/pics/color/';"
  , "  function isSvg(s){ return s && /\\.svg(\\?|$)/.test(s); }"
  , "  function apply(img){"
  , "    var s = img.getAttribute('src'); if (!isSvg(s)) return;"
  , "    if (window.__leksahColorIcons){"
  , "      if (s.indexOf(BASE) === 0 && s.indexOf(COLOR) !== 0)"
  , "        img.setAttribute('src', COLOR + s.slice(BASE.length));"
  , "    } else if (s.indexOf(COLOR) === 0) {"
  , "      img.setAttribute('src', BASE + s.slice(COLOR.length));"
  , "    }"
  , "  }"
  , "  function applyAll(){ var xs = document.querySelectorAll('img');"
  , "    for (var i=0;i<xs.length;i++) apply(xs[i]); }"
  , "  window.leksahSetColorIcons = function(on){"
  , "    window.__leksahColorIcons = !!on; applyAll(); };"
  , "  var obs = new MutationObserver(function(muts){"
  , "    for (var i=0;i<muts.length;i++){ var m = muts[i];"
  , "      if (m.type === 'attributes'){ if (m.target.tagName === 'IMG') apply(m.target); }"
  , "      else { for (var j=0;j<m.addedNodes.length;j++){ var n = m.addedNodes[j];"
  , "        if (n.nodeType !== 1) continue;"
  , "        if (n.tagName === 'IMG') apply(n);"
  , "        else if (n.querySelectorAll){ var q = n.querySelectorAll('img');"
  , "          for (var k=0;k<q.length;k++) apply(q[k]); } } }"
  , "    }"
  , "  });"
  , "  obs.observe(document.documentElement,"
  , "    { subtree:true, childList:true, attributes:true, attributeFilter:['src'] });"
  , "  applyAll();"
  , "})();"
  ]

badgesJs :: Text
badgesJs = T.unlines
  [ "window.__leksahShortcutBadges = false;"
  , "(function(){"
  , "  function set(on){"
  , "    document.body.classList.toggle('leksah-show-badges',"
  , "        !!(on && window.__leksahShortcutBadges));"
  , "  }"
  -- ⌘-Tab swallows the Meta keyup (the app switcher takes the keyboard even
  -- when you come straight back), so besides keyup/blur, clear whenever any
  -- later event reports the key is no longer held.
  , "  function sync(e){"
  , "    if (!e.metaKey && document.body.classList.contains('leksah-show-badges')) set(false);"
  , "  }"
  , "  window.addEventListener('keydown', function(e){ if (e.key === 'Meta') set(true); else sync(e); }, true);"
  , "  window.addEventListener('keyup',   function(e){ if (e.key === 'Meta') set(false); else sync(e); }, true);"
  , "  window.addEventListener('mousemove', sync, true);"
  , "  window.addEventListener('mousedown', sync, true);"
  , "  window.addEventListener('blur',    function(){ set(false); }, true);"
  , "})();"
  ]

-- | The Claude-coordination traffic light: a dot in the top-right of the title
-- | The session token of an AI target path (@session/window/pane@, or a raw
-- tmux @session:win.pane@) — used to focus leksah's terminal for that session.
aiTargetSession :: Text -> Maybe Text
aiTargetSession t = case T.splitOn "/" t of
    (s:_:_) | not (T.null s) -> Just s               -- session/window/pane
    _ -> let s = T.takeWhile (/= ':') t              -- raw tmux target
         in if T.null s then Nothing else Just s

-- | Absolute path of the file in the focused CodeMirror editor (its @.editor@
-- carries @data-file@; see 'IDE.Web.Widget.Editor'), or 'Nothing' if no editor
-- is focused.
activeEditorFile :: JSM (Maybe Text)
activeEditorFile = nonEmpty <$> (valToText =<< eval activeEditorFileJs)
  where nonEmpty s = if T.null s then Nothing else Just s

activeEditorFileJs :: Text
activeEditorFileJs = mconcat
  [ "(function(){var v=window.LeksahCM&&window.LeksahCM.activeView;if(!v)return '';"
  , "var ed=v.dom&&v.dom.closest&&v.dom.closest('.editor');"
  , "return (ed&&ed.getAttribute('data-file'))||'';})()" ]

-- | The focused editor's file plus its selection's 1-based start/end lines.
activeEditorSelection :: JSM (Maybe (Text, Int, Int))
activeEditorSelection = do
    r <- valToText =<< eval activeEditorSelectionJs
    return $ case T.splitOn "\t" r of
      [f, a, b] | not (T.null f)
                , Just an <- readMaybe (T.unpack a)
                , Just bn <- readMaybe (T.unpack b) -> Just (f, an, bn)
      _ -> Nothing

activeEditorSelectionJs :: Text
activeEditorSelectionJs = mconcat
  [ "(function(){var v=window.LeksahCM&&window.LeksahCM.activeView;if(!v)return '';"
  , "var ed=v.dom&&v.dom.closest&&v.dom.closest('.editor');"
  , "var f=ed&&ed.getAttribute('data-file');if(!f)return '';"
  , "var s=v.state.selection.main;"
  , "var a=v.state.doc.lineAt(s.from).number,b=v.state.doc.lineAt(s.to).number;"
  , "return f+'\\t'+a+'\\t'+b;})()" ]

-- bar telling the user whether it's safe to touch leksah while an agent drives
-- it.  Green = safe; orange (+ a beep) = the agent needs it in ~3 s; red = the
-- agent is testing now.  Driven from the shell via
-- @leksah-cmd js eval 'leksahTestStart()'@ (orange→beep→red after 3 s) and
-- @'leksahTestEnd()'@ (back to green); @leksahStatus('green'|'orange'|'red')@
-- sets a state directly.  Default green (normal, un-driven use).
statusLightJs :: Text
statusLightJs = T.unlines
  [ "(function(){"
  , "  var el = null, state = 'green', timer = null;"
  -- Runs BEFORE mainWidgetWithCss rebuilds <body>, which detaches anything we
  -- append now — so (re)create the dot on demand and keep it in whatever <body>
  -- is current, preserving the colour across a rebuild.
  , "  function ensure(){"
  , "    if (el && el.isConnected) return el;"
  , "    if (!document.getElementById('leksah-status-light-css')) {"
  , "      var css = document.createElement('style');"
  , "      css.id = 'leksah-status-light-css';"
  , "      css.textContent ="
  , "        '#leksah-status-light{position:fixed;top:6px;right:10px;width:13px;height:13px;'+"
  , "        'border-radius:50%;z-index:2147483647;pointer-events:none;opacity:.9;'+"
  -- Own compositor layer so the glow never forces repaints of content beneath.
  , "        'transform:translateZ(0);'+"
  , "        'box-shadow:0 0 0 1px rgba(0,0,0,.45);transition:background .15s,box-shadow .15s}'+"
  , "        '#leksah-status-light.green{background:#2ecc40;box-shadow:0 0 6px #2ecc40,0 0 0 1px rgba(0,0,0,.45)}'+"
  , "        '#leksah-status-light.orange{background:#ff9500;box-shadow:0 0 9px #ff9500,0 0 0 1px rgba(0,0,0,.45)}'+"
  , "        '#leksah-status-light.red{background:#ff3b30;box-shadow:0 0 9px #ff3b30,0 0 0 1px rgba(0,0,0,.45)}';"
  , "      (document.head || document.documentElement).appendChild(css);"
  , "    }"
  , "    el = document.createElement('div');"
  , "    el.id = 'leksah-status-light';"
  , "    el.title = 'Green: safe to use leksah \\u2022 Orange: Claude needs it shortly \\u2022 Red: Claude is testing';"
  , "    el.className = state;"
  , "    if (document.body) document.body.appendChild(el);"
  , "    return el;"
  , "  }"
  -- Audio.  A RESUMED-and-left-running AudioContext pegs coreaudiod and drags
  -- the whole app down, so the context is kept SUSPENDED except for the ~0.16 s
  -- beep itself.  The first user gesture unlocks it (autoplay policy) then it's
  -- released again immediately.
  , "  function actx(){"
  , "    var C = window.AudioContext || window.webkitAudioContext;"
  , "    if (!C) return null;"
  , "    if (!window.__leksahAudio) window.__leksahAudio = new C();"
  , "    return window.__leksahAudio;"
  , "  }"
  , "  function prime(){"
  , "    window.removeEventListener('pointerdown', prime, true);"
  , "    window.removeEventListener('keydown', prime, true);"
  , "    var c = actx();"
  , "    if (c) c.resume().then(function(){ c.suspend(); }).catch(function(){});"
  , "  }"
  , "  window.addEventListener('pointerdown', prime, true);"
  , "  window.addEventListener('keydown', prime, true);"
  , "  function beep(){"
  , "    try {"
  , "      var c = actx(); if (!c) return;"
  , "      c.resume().then(function(){"
  , "        var o = c.createOscillator(), g = c.createGain();"
  , "        o.type = 'sine'; o.frequency.value = 880; g.gain.value = 0.06;"
  , "        o.connect(g); g.connect(c.destination);"
  , "        var t = c.currentTime; o.start(t); o.stop(t + 0.16);"
  , "        setTimeout(function(){ try { c.suspend(); } catch(e){} }, 400);"
  , "      }).catch(function(){});"
  , "    } catch(e){}"
  , "  }"
  , "  function set(s){ if (timer){ clearTimeout(timer); timer = null; } state = s; ensure().className = s; }"
  , "  window.leksahStatus = set;"
  , "  window.leksahTestStart = function(){"
  , "    if (timer) clearTimeout(timer);"
  , "    state = 'orange'; ensure().className = 'orange'; beep();"
  , "    timer = setTimeout(function(){ state = 'red'; ensure().className = 'red'; timer = null; }, 3000);"
  , "  };"
  , "  window.leksahTestEnd = function(){ set('green'); };"
  -- Land the dot in the final <body> (mainWidget replaces an early append) and
  -- keep it there: a cheap 0.5s poll re-appends it if it's ever detached.
  , "  ensure(); setInterval(ensure, 500);"
  , "})();"
  ]

-- | @window.leksahSelectRegion()@: the permission-free region picker.  Shows a
-- drag overlay over the leksah window; on mouse-up it removes itself and reports
-- the selected rectangle (viewport CSS px = the WKWebView snapshot coordinate
-- system) to @window.__leksahRegionResult@ as @\"x,y,w,h\"@ (empty on cancel /
-- Esc / a too-small drag).  Reported after two rAFs so the overlay is gone from
-- the painted frame the snapshot then captures.  The reflex side (see the region
-- grab bridge) installs __leksahRegionResult and does the native snapshot.
regionSelectJs :: Text
regionSelectJs = T.unlines
  [ "window.leksahSelectRegion = function(){"
  , "  if (window.__leksahRegionActive) return; window.__leksahRegionActive = true;"
  , "  var ov = document.createElement('div');"
  , "  ov.style.cssText = 'position:fixed;inset:0;z-index:2147483646;cursor:crosshair;background:rgba(0,0,0,0.04)';"
  , "  var box = document.createElement('div');"
  , "  box.style.cssText = 'position:fixed;border:1px solid #4a90d9;background:rgba(74,144,217,0.15);pointer-events:none;display:none';"
  , "  document.body.appendChild(ov); document.body.appendChild(box);"
  , "  var sx=0, sy=0, dragging=false;"
  , "  function report(s){"
  , "    ov.remove(); box.remove();"
  , "    document.removeEventListener('keydown', onKey, true);"
  , "    window.__leksahRegionActive = false;"
  , "    requestAnimationFrame(function(){ requestAnimationFrame(function(){"
  , "      if (window.__leksahRegionResult) window.__leksahRegionResult(s); }); });"
  , "  }"
  , "  function onKey(e){ if (e.key === 'Escape'){ e.preventDefault(); report(''); } }"
  , "  document.addEventListener('keydown', onKey, true);"
  , "  function b4(e){ return { x:Math.min(sx,e.clientX), y:Math.min(sy,e.clientY),"
  , "                           w:Math.abs(e.clientX-sx), h:Math.abs(e.clientY-sy) }; }"
  , "  ov.addEventListener('mousedown', function(e){ dragging=true; sx=e.clientX; sy=e.clientY; box.style.display='block'; e.preventDefault(); });"
  , "  ov.addEventListener('mousemove', function(e){ if(!dragging) return; var b=b4(e);"
  , "    box.style.left=b.x+'px'; box.style.top=b.y+'px'; box.style.width=b.w+'px'; box.style.height=b.h+'px'; });"
  , "  ov.addEventListener('mouseup', function(e){ if(!dragging){ report(''); return; } dragging=false;"
  , "    var b=b4(e); if(b.w<3||b.h<3){ report(''); return; }"
  , "    report(Math.round(b.x)+','+Math.round(b.y)+','+Math.round(b.w)+','+Math.round(b.h)); });"
  , "};"
  ]

-- | A snapped pane's stable key, @\"tid:pid\"@ (e.g. @\"1:%5\"@), shared between
-- the reflex set, the JS snap-rect map, and the native window bindings.
-- | @sessionId:paneId@, e.g. @$3:%7@ (the session id has no colon, so the first
-- colon splits them).
paneKey :: (Text, Text) -> Text
paneKey (n, pid) = n <> ":" <> pid

parsePaneKey :: Text -> Maybe (Text, Text)
parsePaneKey t = case T.breakOn ":" t of
    (a, b) | not (T.null b) -> Just (a, T.drop 1 b)
    _ -> Nothing

main
  :: forall t m . MonadWidget t m
  => Bool             -- ^ render the web menu bar (hidden when there's a native menu)
  -> Bool             -- ^ native mac title bar (the toolbar occupies the title bar)
  -> Dynamic t IDE
  -> m (Event t IDEAction)
main showMenubar macTitlebar ide = mdo
  let menuClass = if showMenubar then "" else " no-menubar"
      titlebarClass = if macTitlebar then " mac-titlebar" else ""
      tallClass TallShow     = ""
      tallClass TallAutoHide = " tall-auto"
      tallClass TallHide     = " tall-hide"
      wide1Class TallShow     = ""
      wide1Class TallAutoHide = " wide1-auto"
      wide1Class TallHide     = " wide1-hide"
      rootAttrD = ffor ide $ \i ->
          "class" =: ("leksah" <> menuClass <> titlebarClass
                      <> tallClass (tallVisibility (i ^. prefs))
                      <> wide1Class (wide1Visibility (i ^. prefs)))
            <> "tabindex" =: "0"
  (top, topEvents) <- elDynAttr' "div" rootAttrD $ mdo
    keymapE <- keymapWidget top
    -- The web menu bar is suppressed when a native menu is present
    -- (leksah-wkwebview); its command events then simply never fire.
    menubarE   <- if showMenubar then menubarWidget else return never
    toolbarE   <- toolbarWidget ide
    -- Transparent, click-through overlays over the side pane / bottom bar that
    -- draw the divider line next to the editor area and (via CSS :has focus) a
    -- drop shadow when one of that panel's panes is active — see layoutCss.
    elAttr "div" ("class" =: "tall-divider") (pure ())
    elAttr "div" ("class" =: "wide1-divider") (pure ())
    -- The auto-hide activation strip: an invisible 3px zone over the editor's left
    -- padding whose hover re-opens the collapsed side pane (see layoutCss).
    elAttr "div" ("class" =: "tall-sensor") (pure ())
    -- The Open / Open Project commands (from the toolbar or web menubar) show the
    -- native open panels.  (The native macOS menu triggers these directly.)
    let panelCmdE = leftmost
          [ fmapMaybe (^? _ToolbarCommand) toolbarE
          , fmapMaybe (^? _MenubarCommand) menubarE ]
    performEvent_ $ ffor panelCmdE $ \case
      CommandFileOpen    -> liftIO runOpenFilePanel
      CommandProjectOpen -> liftIO runOpenProjectPanel
      _                  -> return ()
    -- The web toolbar/menubar's Preferences command, ⌘, (keymap), or the native
    -- macOS app-menu "Settings…" item (via the bridge) opens the Preferences pane.
    (prefsBridgeE, firePrefsReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextPreferencesRequest >> firePrefsReq ()
    let showPrefsE = leftmost
          [ fmapMaybe (\case CommandShowPreferences -> Just (); _ -> Nothing) panelCmdE
          , fmapMaybe (\e -> case e ^? _KeymapCommand of
                               Just CommandShowPreferences -> Just (); _ -> Nothing) keymapE
          , prefsBridgeE ]

    -- AI ▸ Grab Region / `leksah-cmd grab-region`.  Choose the capture path by
    -- whether Screen Recording permission is granted (probed off-thread):
    -- granted → the system crosshair (screencapture, real screen — transparent/
    -- snapped holes captured correctly); missing → an in-leksah drag overlay +
    -- WKWebView snapshot of the selected rect (permission-free; holes it can't
    -- see are simply transparent).  Either way the PNG path is typed into the
    -- target pane (regionCaptureTarget pref, or a leksah-cmd override).
    (regionGrabE, fireRegionGrab)     <- newTriggerEvent
    (regionStartOverlayE, fireOverlay) <- newTriggerEvent
    (regionRectE, fireRegionRect)     <- newTriggerEvent
    regionTargetRef <- liftIO $ newIORef ("" :: Text)
    _ <- liftIO . forkIO . forever $ nextRegionGrab >>= fireRegionGrab
    -- Install the overlay's result callback (\"x,y,w,h\" | \"\").
    _ <- liftJSM $ jsg ("window" :: Text) ^. jss ("__leksahRegionResult" :: Text)
           (fun $ \_ _ args -> case args of
              (v : _) -> do
                s <- valToText v
                case map (readMaybe . T.unpack) (T.splitOn "," s) of
                  [Just x, Just y, Just w, Just h] -> liftIO $ fireRegionRect (x, y, w, h)
                  _ -> return ()
              _ -> return ())
    performEvent_ $ ffor (attach (current ide) regionGrabE) $ \(ideNow, mbT) -> liftIO $ do
        let target = fromMaybe (regionCaptureTarget (ideNow ^. prefs)) mbT
        void . forkIO $ do
            allowed <- screenCaptureAllowed
            if allowed then void (grabRegionToTarget target) else fireOverlay target
    performEvent_ $ ffor regionStartOverlayE $ \target -> do
        liftIO $ writeIORef regionTargetRef target
        liftJSM . void $ eval ("window.leksahSelectRegion && window.leksahSelectRegion()" :: Text)
    performEvent_ $ ffor regionRectE $ \rect -> liftIO $ do
        target <- readIORef regionTargetRef
        file <- nextRegionFile
        ok <- requestScreenshotRegion (T.pack file) rect
        when ok . void $ sendPathToTarget target file

    -- AI menu ▸ Send… / Focus: type an @file / @file#Lx-Ly reference (or the
    -- current error) into the AI terminal, or focus it.  The active editor's
    -- file + selection live in CodeMirror (JS, read via LeksahCM.activeView);
    -- the current error is in IDE state.  Paths are made relative to leksah's
    -- cwd (usually the project root the AI session also runs in).
    (aiActionE, fireAIAction) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextAIAction >>= fireAIAction
    performEvent_ $ ffor (attach (current ide) aiActionE) $ \(ideNow, act) -> do
        let target = regionCaptureTarget (ideNow ^. prefs)
            sendRel absf suffix = liftIO $ do
                rel <- makeRelativeToCurrentDirectory (T.unpack absf)
                void $ sendTextToTarget target ("@" <> T.pack rel <> suffix <> " ")
        case act of
          -- The target names a tmux session (e.g. "claude"), but leksah keys
          -- its terminal tabs by tmux session id ($N).  Resolve name→id, then
          -- drive the same path a repl-launch uses (fireTermRequest): bring the
          -- session's tab up in wide0 AND focus its active pane.
          FocusAITerminal -> liftIO . void . forkIO $
              forM_ (aiTargetSession target) $ \sess ->
                  resolveTmuxSessionId sess >>= mapM_ fireTermRequest
          SendError -> liftIO $
              forM_ (ideNow ^. currentError) $ \lr -> do
                  rel <- makeRelativeToCurrentDirectory (logRefFullFilePath lr)
                  let ln  = srcSpanStartLine (logRefSrcSpan lr)
                      msg = T.takeWhile (/= '\n') (refDescription lr)
                  void $ sendTextToTarget target
                      ("@" <> T.pack rel <> "#L" <> T.pack (show ln)
                       <> " " <> msg <> " ")
          SendFileRef ->
              liftJSM activeEditorFile >>= \case
                  Just absf -> sendRel absf ""
                  Nothing   -> return ()
          SendSelection ->
              liftJSM activeEditorSelection >>= \case
                  Just (absf, a, b) ->
                      sendRel absf $ if a == b
                          then "#L" <> T.pack (show a)
                          else "#L" <> T.pack (show a) <> "-L" <> T.pack (show b)
                  Nothing -> return ()

    let initialTabs =
               WorkspaceKey =: ("tall", Just ())
            <> ErrorsKey    =: ("wide1", Just ())
            <> LogKey       =: ("wide1", Just ())
            <> GrepKey      =: ("wide1", Just ())
            <> ChangesKey   =: ("wide1", Just ())
            <> TerminalsKey =: ("tall", Just ())
            <> MetadataKey  =: ("tall", Just ())
        initialVisibleTabs =
               "tall" =: WorkspaceKey
            <> "wide1" =: LogKey

    -- Render the bar button(s) for one tab.  Non-terminal tabs get a single
    -- button (label + optional × close).  A terminal gets one button per tmux
    -- window — all selecting the one session body, each with its own detach × —
    -- so a session with two windows shows two tabs (ordered independently, not
    -- grouped), and clicking a window tab also switches the shared terminal to
    -- that window (select-window).
    let
      -- The flipper's item list mapped to wide0 button order (per-window, not
      -- grouped by session); every window/tab button looks up its slot here.
      winOrderListD = buttonOrderOf <$> flipLiveD
      -- The common tab-button shape: a .tab-wrap carrying the selected/hover
      -- highlight (and wide0 MRU order), an optional × close on the left, and a
      -- label button; clicking the label selects `k` in `area` and runs `onSel`.
      tabButton :: Text -> TabKey -> Dynamic t Bool -> Dynamic t (Map Text Text)
                -> Dynamic t (Maybe Int)
                -> Maybe Text -> Maybe Text -> m () -> IO ()
                -> m (Event t (Map Text TabKey, [TabKey]))
      tabButton area k selectedD orderStyleD badgeD mbTitle mbCloseTip labelW onSel =
        elDynAttr "span"
            ((\sel ost -> "class" =: ("tab-wrap" <> if sel then " selected" else "") <> ost)
               <$> selectedD <*> orderStyleD) $ do
          closeE <- case mbCloseTip of
            Just tip -> do
              (xe, _) <- elAttr' "span" ("class" =: "tab-close" <> "title" =: tip) $ text "×"
              pure ([k] <$ domEvent Click xe)
            Nothing  -> pure never
          (be, _) <- elAttr' "button" (maybe mempty ("title" =:) mbTitle) labelW
          -- Side-/bottom-bar tabs carry a static ⌥⌘/⌃⌘ badge; wide0 tabs a
          -- dynamic ⌘-number (their bar position, minus the active one).
          tabShortcutBadge area k
          elDynAttr "span"
              ((\mb -> "class" =: maybe "" (const "leksah-shortcut-badge") mb) <$> badgeD)
              (dynText ((\mb -> maybe "" (\n -> "\8984" <> T.pack (show n)) mb) <$> badgeD))
          let clickE = domEvent Click be
          performEvent_ (liftIO onSel <$ clickE)
          pure $ leftmost [ (\_ -> (area =: k, [])) <$> clickE, (,) mempty <$> closeE ]
      -- CSS order for one wide0 button, by its identity's place in the flipper's
      -- item list; Nothing off the wide0 row.  (`ident <$ baseOrderD` keeps the
      -- Just/Nothing of baseOrderD as the "is this wide0?" flag.)
      buttonOrderStyleD ident baseOrderD =
        (\mb order -> orderStyle (fromMaybe 99999 (elemIndex ident order) <$ mb))
          <$> baseOrderD <*> winOrderListD
      mkTabButtons :: Text -> TabKey -> Dynamic t (Maybe ()) -> Dynamic t Bool
                   -> Dynamic t (Maybe Int) -> m (Event t (Map Text TabKey, [TabKey]))
      mkTabButtons area k _v isVisibleD baseOrderD = case k of
        TerminalKey s -> do
          -- One button per tmux window (fall back to a lone session button until
          -- the first pane-tree poll arrives, keyed -1 so it's distinct).  Each
          -- window tab is ordered independently (see buttonOrderOf).  No close ×:
          -- there's no non-destructive per-window close, so detach the whole
          -- session via File ▸ Close / the Terminals tree instead.
          let winsD   = maybe [] snd . M.lookup s <$> allTreeD
              winMapD = ffor winsD $ \ws ->
                          if null ws then M.singleton (-1) Nothing
                          else M.fromList [ (twIndex w, Just w) | w <- ws ]
          winButtonsE <- listViewWithKey winMapD $ \widx mwD -> do
            let curD      = maybe True twActive <$> mwD          -- fallback: current
                selectedD = (&&) <$> isVisibleD <*> curD          -- visible session + current window
                -- Label by the window alone (the session name is redundant — the
                -- Terminals tree groups by session); fall back to the session name
                -- only before the first poll, when no window is known yet.
                labelD    = (\names mw att ->
                              let bell | s `S.member` att && maybe True twActive mw = " \128276"
                                       | otherwise = ""
                              in case mw of
                                   Nothing -> M.findWithDefault s s names <> bell
                                   Just w  -> stripIdxPrefix (twIndex w) (twLabel w) <> windowAlert w <> bell)
                            <$> terminalNamesD <*> mwD <*> attentionD
                orderStyleD = buttonOrderStyleD (Left (s, widx)) baseOrderD
                -- Switch the shared terminal to this window, then poke a pane-tree
                -- refresh so the "current window" highlight updates at once (not on
                -- the next 2 s poll).
                onSel | widx < 0  = pure ()
                      | otherwise = case remoteTabHostTarget s of
                          Just (host, target) -> void . forkIO $ do
                              selectRemoteTmuxWindow host target widx
                              fireRemotePoke ()
                          Nothing -> selectTmuxWindow s widx >> fireTermActivity ()
                badgeD = M.lookup (Left (s, widx)) <$> badgeNumsD
            tabButton area k selectedD orderStyleD badgeD Nothing Nothing (dynText labelD) onSel
          pure (mconcat . M.elems <$> winButtonsE)
        _ ->
          let mbTitle    = case k of EditorKey f -> Just (T.pack f); _ -> Nothing
              mbCloseTip = case k of EditorKey _ -> Just "Close"; _ -> Nothing
              orderStyleD = buttonOrderStyleD (Right k) baseOrderD
              -- Side-pane tree tabs carry a leading B&W icon before their label.
              labelW = do
                mapM_ (\s -> elAttr "img" ("class" =: "tab-icon" <> "src" =: s) (pure ()))
                      (tabIconSrc k)
                dynText (tabLabelText k <$> terminalTabLabelsD)
              badgeD = M.lookup (Right k) <$> badgeNumsD
          in tabButton area k isVisibleD orderStyleD badgeD mbTitle mbCloseTip labelW (pure ())

    -- File ▸ Save / the Save toolbar button: a background thread turns native-
    -- menu save requests into a reflex event; the in-page toolbar/menubar Save
    -- command routes to the same event (without the round-trip).  We save
    -- whichever editor is the active pane (a non-editor active pane saves nothing).
    (saveBridgeE, fireSaveReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextSaveRequest >> fireSaveReq ()
    let inPageSaveE = fmapMaybe (\case CommandFileSave -> Just (); _ -> Nothing) panelCmdE
        saveReqE    = leftmost [saveBridgeE, inPageSaveE]
        saveFileE   = fmapMaybe (\case Just (EditorKey f) -> Just f; _ -> Nothing)
                        (tag (current activePaneD) saveReqE)
    (openFileE, openExternalE, makeEditor) <- editorWidget ide allE saveFileE
    -- File ▸ Open (the native NSOpenPanel on wkwebview) delivers chosen files via
    -- a background thread; open each one in the editor area like any other file.
    (nativeOpenedFileE, fireOpenedFile) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextOpenedFile >>= fireOpenedFile
    -- Terminal tabs requested from outside the reflex network: `leksah-cmd
    -- cc-connect HOST` (a remote control-mode tab keyed "ssh://HOST") and the
    -- workspace-tree repl buttons (a local session id from
    -- 'IDE.Web.RemoteTermRequest.requestLocalTerm').
    (termRequestE, fireTermRequest) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextTermRequest >>= fireTermRequest
    -- Hosts shown as top-level Terminals-tree nodes: the preference list plus
    -- any host that has an open ssh:// tab.
    remoteHostsD <- holdUniqDyn $ (\p rt -> nub $ remoteHosts p ++
          [ T.takeWhile (/= '#') rest
          | (_, TerminalKey n) <- rt, Just rest <- [T.stripPrefix "ssh://" n] ])
        <$> prefsD <*> recentTabs
    -- Native File▸Open / `leksah-cmd cm open` honour the external-editor pref too:
    -- when set, they open in the external editor (line 1) rather than CodeMirror.
    let extActiveMainB = current ((not . T.null . externalEditor) <$> prefsD)
        nativeOpenE = (\fp -> EditorKey fp =: ("wide0", Just ()))
                        <$> gate (not <$> extActiveMainB) nativeOpenedFileE
        nativeOpenExtE = (\fp -> (fp, 1)) <$> gate extActiveMainB nativeOpenedFileE
    -- The tmux pane tree, re-read whenever the active pane might have changed, so
    -- the flipper's per-pane list + MRU stay current (tmux-internal switches like
    -- ⌃B o / clicking a split aren't otherwise visible to leksah).
    treePb <- getPostBuild
    -- Flip commands from the keymap: a step (True = forward/⌃`, False = back/⌃⇧`)
    -- and the commit (Control released).
    let rawFlipStepE = fmapMaybe (\e -> case e ^? _KeymapCommand of
                                      Just CommandFlipDown -> Just True
                                      Just CommandFlipUp   -> Just False
                                      _                    -> Nothing) keymapE
        rawFlipDoneE = fmapMaybe (\e -> case e ^? _KeymapCommand of
                                      Just CommandFlipDone -> Just ()
                                      _                    -> Nothing) keymapE
    -- Opening the flipper must reflect the *current* active tmux pane, which can
    -- have changed invisibly (⌃B o, clicking a split).  So the first press (while
    -- the flipper is hidden) polls the pane tree, and only once that result is in
    -- — the list reordered while still hidden — do we actually open, one frame
    -- later (changing the list on the same frame the flipper opens crashes its
    -- selectViewListWithKey).  Presses while it's already up just advance.
    let taggedStepE  = attach (current flipperVisibleD) rawFlipStepE
        advanceStepE = fmapMaybe (\(vis, dir) -> if vis     then Just dir else Nothing) taggedStepE
        openStepE    = fmapMaybe (\(vis, dir) -> if vis     then Nothing else Just dir) taggedStepE
    -- On the *first* press (flipper hidden) poll the tree, and if the current MRU
    -- front is a terminal pane refresh it to that terminal's active pane — this is
    -- what floats a ⌃B / click pane switch to the top.  We deliberately only touch
    -- the front terminal, never some other (wide0) pane, so an item you flipped or
    -- clicked to in the side/bottom bar stays on top instead of being shoved down.
    openPollE <- performEvent $ ffor (attach (current flipMruD) openStepE) $ \(mru, dir) -> do
        tree <- liftIO listTerminalTree
        let refreshed = case mru of
              (FlipPane n _ _ : _) -> activeFlipFor (Just (TerminalKey n)) tree
              _                    -> Nothing
        pure (dir, tree, refreshed)
    -- Only open once the poll is in and the list has reordered (one frame later —
    -- opening on the same frame the list changes crashes selectViewListWithKey).
    openStepDelayedE <- performEvent $ ffor openPollE $ \(dir, _, _) -> pure dir
    let flipStepE = leftmost [ advanceStepE, openStepDelayedE ]
    -- Other reasons to re-read the tree (labels, MRU while hidden): post-build,
    -- terminal activity (click / ⌃B poll), open/close/select, session save.
    let treeRefreshE = leftmost
          [ () <$ treePb, termActivityE
          , () <$ closeTermE, () <$ selectAnyTermE, () <$ saveSessE
          -- A requested tab may be a freshly-created session/window (e.g. a
          -- workspace repl button): re-read the tree so it shows at once.
          , () <$ termRequestE
          -- A local CC tab saw a window created/closed.
          , () <$ ffilter (any (not . ("ssh://" `T.isPrefixOf`))) treeChangedTabsE ]
    -- Same for remote CC tabs: poke the ssh poll.
    performEvent_ $ ffor (ffilter (any ("ssh://" `T.isPrefixOf`)) treeChangedTabsE) $ \_ ->
        liftIO $ fireRemotePoke ()
    -- OFF the reflex thread: a tmux subprocess per poke (term-activity fires
    -- on every window/pane select) would hitch the UI run synchronously.
    (otherPollE, fireOtherPoll) <- newTriggerEvent
    performEvent_ $ ffor treeRefreshE $ \_ ->
        liftIO . void . forkIO $ listTerminalTree >>= fireOtherPoll
    -- A freshly-created terminal, polled once it exists: carries the new tree and
    -- the new session's active-pane flip item, so the pane both appears in the
    -- list and is floated to the MRU front (creating a terminal makes it current,
    -- but no mouse-down set activePaneD to it).  Fired from newTermIdE below; a
    -- trigger event (defined here, fired later) avoids a forward performEvent ref.
    (newTermPolledE, fireNewTermPolled) <- newTriggerEvent
    paneTreeD <- holdUniqDyn =<< holdDyn mempty
      (leftmost [ otherPollE, (\(_, t, _) -> t) <$> openPollE, fst <$> newTermPolledE ])
    -- ONE ssh poll per remote host (10s, off the reflex thread, plus pokes),
    -- feeding BOTH remote surfaces: the Terminals tree's host nodes (all
    -- sessions of each host, with reachability) and the flipper/tab row's
    -- per-tab trees (derived below).  Previously each surface ran its own
    -- ssh — per host AND per open tab.
    remoteTabsD <- holdUniqDyn $
        (\rt -> nub [ n | (_, TerminalKey n) <- rt, "ssh://" `T.isPrefixOf` n ])
          <$> recentTabs
    remoteFlipTick <- tickLossyFromPostBuildTime 10
    (hostTreesE, fireHostTrees) <- newTriggerEvent
    -- Poked right after a remote window/pane select (and on CC window
    -- add/close), so the active-window highlight moves at once instead of on
    -- the next 10s poll.
    (remotePokeE, fireRemotePoke) <- newTriggerEvent
    performEvent_ $ ffor (leftmost [ tag (current remoteHostsD) remoteFlipTick
                                   , tag (current remoteHostsD) remotePokeE
                                   , updated remoteHostsD ]) $ \hosts ->
        liftIO . void . forkIO $ do
            entries <- forM hosts $ \h -> (,) h <$> listRemoteTerminalTree h
            fireHostTrees (M.fromList entries)
    -- host -> (reachable, sessions).  An unreachable host keeps its last-known
    -- sessions (marked unreachable); hosts no longer listed drop out.
    hostTreesD <- holdUniqDyn =<< foldDyn
        (\new old -> M.mapWithKey
            (\h mb -> case (mb, M.lookup h old) of
                (Just t,  _)                -> (True, t)
                (Nothing, Just (_, oldT))   -> (False, oldT)
                (Nothing, Nothing)          -> (False, M.empty))
            new)
        M.empty hostTreesE
    -- The open remote tabs' trees, keyed by TAB key and merged into the same
    -- tree the flipper and tab ordering consume ('allTreeD'), so remote
    -- windows flip and order exactly like local ones.  A tab's target may be
    -- the session's name or its id.
    remoteFlipD <- holdUniqDyn $
        (\hostTrees tabs -> M.fromList
            [ (n, (host <> " · " <> nm, ws))
            | n <- tabs
            , Just (host, target) <- [remoteTabHostTarget n]
            , Just (_, tree) <- [M.lookup host hostTrees]
            , Just (_, (nm, ws)) <- [find (\(sid, (nm', _)) -> nm' == target || sid == target)
                                          (M.toList tree)] ])
          <$> hostTreesD <*> remoteTabsD
    let allTreeD = M.union <$> paneTreeD <*> remoteFlipD
    -- The item the user is currently focused on: the last tab pressed
    -- (activePaneD, in any area — side bar, bottom bar, editor) resolved to its
    -- active tmux pane if it's a terminal.  Its changes drive the MRU so a click
    -- promotes that pane/tab; the terminal-pane part also catches ⌃B while focused.
    activeFlipD <- holdUniqDyn (activeFlipFor <$> activePaneD <*> allTreeD)
    -- Focusing a tab by any route moves it to the flipper MRU front.  activeFlipD
    -- (above) catches mouse-down / tab-button clicks, but not the *programmatic*
    -- focus an editor takes when it's opened by a workspace double-click or a
    -- terminal link — that arrives as a focusin (focusTabE).  Map the focused
    -- key back to a FlipItem (a terminal → its active pane) so those navigations
    -- float to the top too.
    let focusFlipE = fmapMaybe id $ attachWith
          (\(rt, tree) str ->
             case [ k | (_, k) <- rt, T.pack (show k) == str ] of
               (k:_) -> activeFlipFor (Just k) tree
               []    -> Nothing)
          ((,) <$> current recentTabs <*> current allTreeD) focusTabE
        -- Opening a file to navigate to it (workspace double-click, terminal link)
        -- floats that editor to the MRU front directly, whether or not focus moves.
        openEditorFlipE = fmapMaybe (fmap FlipTab . listToMaybe . M.keys) openFileE'
        -- Opening Preferences (⌘, / menu) floats it to the MRU front too — it
        -- doesn't take focus the way an editor does, so it needs an explicit bump.
        openPrefsFlipE = FlipTab PreferencesKey <$ showPrefsE
    -- The MRU: move an item to the front when it is focused/clicked (activeFlipD /
    -- focusFlipE), opened to navigate to (openEditorFlipE / openPrefsFlipE), when the
    -- flipper commits a selection (flipSelE), and when opening refreshes the front
    -- terminal's active pane after a ⌃B switch.
    flipMruD <- holdUniqDyn =<< foldDyn (\fi mru -> fi : filter (/= fi) mru) []
                        (leftmost [ fmapMaybe (\(_, _, a) -> a) openPollE
                                  , snd <$> flipSelE
                                  , snd <$> newTermPolledE
                                  , focusFlipE
                                  , openEditorFlipE
                                  , openPrefsFlipE
                                  , fmapMaybe id (updated activeFlipD) ])
    -- The flip list, kept populated and updated ONLY while the flipper is hidden
    -- (frozen during a flip) and only on genuine changes (holdUniqDyn).  This
    -- mirrors the old tab MRU, which never changed the list under the flipper —
    -- changing it on the open event, or churning it every tmux poll, crashes the
    -- flipper's selectViewListWithKey ("Same key fired multiple times for Merge").
    flipLiveD <- holdUniqDyn (buildFlipItems <$> flipMruD <*> recentTabs <*> allTreeD)
    -- The list the flipper shows.  It updates freely while hidden (labels, tabs),
    -- but the authoritative refresh is a *snapshot taken on open* (openListE):
    -- built from the current MRU with the freshly-polled active pane floated to
    -- the front, using the fresh tree.  Relying on @updated flipLiveD@ alone left
    -- it stale — after a flip, holdUniqDyn suppresses the (already-front) re-bump,
    -- so the flipper reopened with the pre-flip order and flipped to the wrong
    -- pane.  The snapshot fires one frame before the flipper actually opens.
    let openListE = attachWith
          (\(mru, rt, rtree) (_, tree, active) ->
             let mru' = maybe mru (\a -> a : filter (/= a) mru) active
             in buildFlipItems mru' rt (M.union tree rtree))
          ((,,) <$> current flipMruD <*> current recentTabs <*> current remoteFlipD)
          openPollE
    flipItemsD <- holdDyn [] (leftmost
          [ openListE
          , gate (current (not <$> flipperVisibleD)) (updated flipLiveD) ])
    let flipLabel fiD = dynText $
          (\fi names tree -> case fi of
             FlipTab k      -> tabLabelText k names
             FlipPane n w p -> flipPaneLabel n w p tree)
            <$> fiD <*> terminalNamesD <*> allTreeD
    (flipperVisibleD, flipRawE) <- flipperWidget flipItemsD flipStepE rawFlipDoneE flipLabel
    -- Split the flipper selection: a tab selects as before; a pane brings its
    -- terminal up in wide0 (below) and makes that tmux pane active.
    -- A ⌘-number on a non-active wide0 button feeds a synthetic flip selection
    -- (always wide0), so it navigates through the same path as the flipper.
    (numFlipE, fireNumFlip) <- newTriggerEvent
    let flipSelE  = leftmost [ fmapMaybe (listToMaybe . M.toList) flipRawE
                             , (\fi -> ("wide0", fi)) <$> numFlipE ]
        flipTabE  = fmapMaybe (\(a, fi) -> case fi of FlipTab k -> Just (M.singleton a k); _ -> Nothing) flipSelE
        flipPaneE = fmapMaybe (\(_, fi) -> case fi of FlipPane s w p -> Just (s, w, p); _ -> Nothing) flipSelE
    performEvent_ $ ffor flipPaneE $ \(s, w, p) -> liftIO $
        case remoteTabHostTarget s of
          -- remote pane: select over ssh (off the reflex thread); the tab's
          -- control client hears %session-window-changed and re-renders
          Just (host, target) -> void . forkIO $ do
              selectRemoteTmuxPane host target w p
              fireRemotePoke ()
          Nothing -> selectTmuxPane s w p
    -- Jump-to-teammate (⌃⌥A): pick the next attention-flagged window from the
    -- current pane tree, switch tmux to it (clears the flag), and bring its
    -- session's terminal up in wide0 (via openTabsE / selectTabE below).
    let focusAlertE = fmapMaybe (\e -> case e ^? _KeymapCommand of
                                        Just CommandFocusAlert -> Just (); _ -> Nothing) keymapE
        alertTargetE = fmapMaybe firstAlertWindow (tag (current paneTreeD) focusAlertE)
    performEvent_ $ ffor alertTargetE $ \(s, w) -> liftIO (selectTmuxWindow s w)
    let openFileE' = mapKeys EditorKey <$> openFileE
    openFileKeysD <- foldDyn ($) mempty $ leftmost
      [ (\new s -> s <> new) . S.fromList . M.keys <$> openFileE'
      , (\new s -> s <> new) <$> restoreFileKeysE
      , (\fp s -> s <> S.singleton (EditorKey fp)) <$> nativeOpenedFileE
      , (\ks s -> foldr S.delete s ks) . filter (\case EditorKey _ -> True; _ -> False) <$> detachCloseE ]
    -- Recently opened *files* (most recent first), for the File ▸ Open Recent
    -- menu.  Terminals aren't files, so they're excluded; the list accumulates
    -- across opens (capped) and is seeded from the saved session.
    let openedFilesE = leftmost [ M.keys <$> openFileE, (:[]) <$> nativeOpenedFileE ]
        addRecent new old = take 20 (new ++ filter (`notElem` new) old)
    recentFilesD <- foldDyn ($) [] $ leftmost
      [ addRecent <$> openedFilesE
      , const <$> restoreRecentFilesE ]
    recentFilesUniqD <- holdUniqDyn recentFilesD
    performEvent_ $ liftIO . updateRecentFiles <$> updated recentFilesUniqD
    -- Terminals: the list pane (TerminalsKey, on the side) creates/selects
    -- terminals, which render as tabs in the editor area (wide0).  Each terminal
    -- reports its window title up through `tabE`; we fold those into the
    -- id->title map the list pane and tab buttons label themselves from.
    let terminalsListE = select (fan (select (fanMap tabE) (Const2 TerminalsKey))) TerminalsTab
        newTermClickE = fmapMaybe (^? _NewTerminal) terminalsListE
        selectTermE   = fmapMaybe (^? _SelectTerminal) terminalsListE
        -- The Terminals pane confirms before this fires, so just act on it:
        -- kill the tmux session and drop the terminal from the list/tabs.
        closeTermE    = fmapMaybe (^? _CloseTerminal) terminalsListE
        -- Drilling into the tmux tree: switch tmux to the chosen window/pane,
        -- then (below) bring its session's terminal up so the change is visible.
        selectWinE    = fmapMaybe (^? _SelectTerminalWindow) terminalsListE
        selectPaneE   = fmapMaybe (^? _SelectTerminalPane) terminalsListE
        -- The session to bring up, however the selection was made.
        selectAnyTermE = leftmost
          [ selectTermE
          , (\(s, _)    -> s) <$> selectWinE
          , (\(s, _, _) -> s) <$> selectPaneE ]
        -- Remote host nodes: sessions/windows/panes on another machine's tmux,
        -- rendered in control-mode tabs keyed "ssh://host#session".
        newRemoteE     = fmapMaybe (^? _NewRemoteTerminal) terminalsListE
        selRemoteE     = fmapMaybe (^? _SelectRemoteTerminal) terminalsListE
        selRemoteWinE  = fmapMaybe (^? _SelectRemoteTerminalWindow) terminalsListE
        selRemotePaneE = fmapMaybe (^? _SelectRemoteTerminalPane) terminalsListE
        -- Clicking the server row brings up that host's one per-server
        -- connection (its default "leksah" session) — keyed "ssh://host".
        selRemoteHostE = fmapMaybe (^? _SelectRemoteHost) terminalsListE
        remoteKey h sid = "ssh://" <> h <> "#" <> sid
    performEvent_ $ ffor closeTermE $ liftIO . killTerminalSession
    performEvent_ $ ffor selectWinE  $ \(s, w)    -> liftIO (selectTmuxWindow s w)
    performEvent_ $ ffor selectPaneE $ \(s, w, p) -> liftIO (selectTmuxPane s w p)
    performEvent_ $ ffor selRemoteWinE  $ \(h, s, _, w) ->
        liftIO . void . forkIO $ selectRemoteTmuxWindow h s w
    performEvent_ $ ffor selRemotePaneE $ \(h, s, _, w, p) ->
        liftIO . void . forkIO $ selectRemoteTmuxPane h s w p
    -- "+" on a remote host: create a session there, then open its tab.
    remoteNewSidE <- performEvent $ ffor newRemoteE $ \h ->
        liftIO $ fmap (remoteKey h) <$> createRemoteSession h
    -- The tab to bring up for a tree selection: an already-open tab for the
    -- same session under EITHER identity — its id ($3) or its current name
    -- (a cc-connect HOST#NAME tab) — else a fresh id-keyed tab.  Without
    -- the match, clicking the tree opened a second tab for a session that
    -- was already open under its name.
    let resolveRemoteKey rt h sid nm =
          case [ n | (_, TerminalKey n) <- rt
                   , Just (h', t) <- [remoteTabHostTarget n]
                   , h' == h, t == sid || t == nm ] of
            (n : _) -> n
            []      -> remoteKey h sid
        remoteOpenKeyE = leftmost
          [ fmapMaybe id remoteNewSidE
            -- Server row → the per-server tab (its default "leksah" session).
          , ("ssh://" <>) <$> selRemoteHostE
          , attachWith (\rt (h, sid, nm) -> resolveRemoteKey rt h sid nm)
                       (current recentTabs) selRemoteE
          , attachWith (\rt (h, sid, nm, _) -> resolveRemoteKey rt h sid nm)
                       (current recentTabs) selRemoteWinE
          , attachWith (\rt (h, sid, nm, _, _) -> resolveRemoteKey rt h sid nm)
                       (current recentTabs) selRemotePaneE ]
    -- Restore the saved web session (open files, open terminals, visible tabs)
    -- together with the tmux sessions left over from a previous run, in one read
    -- so the two can't race.
    restorePb <- getPostBuild
    restoreE <- performEvent $ ffor restorePb $ \_ -> liftIO $
      (,) <$> readWebSession <*> listTerminalSessions
    let existingIdsE = snd <$> restoreE
        -- Editor + (still-existing) terminal tabs to reopen, as one map so they
        -- open together in a single openTabsE firing.
        -- The open editors/terminals are exactly the Editor/Terminal entries of
        -- the saved tab list; reopen them (terminals only if still in tmux).
        restoreOpenE = fmapMaybe
          (\(ms, ids) -> case ms of
             Nothing -> Nothing
             Just s ->
               let files = [ (EditorKey f, ("wide0", Just ())) | EditorKey f <- wsTabs s ]
                   terms = [ (TerminalKey n, ("wide0", Just ())) | TerminalKey n <- wsTabs s, n `elem` map fst ids ]
                   m = M.fromList (files ++ terms)
               in if M.null m then Nothing else Just m)
          restoreE
        -- Editor keys also seed openFileKeysD so the editor event routing knows
        -- about the restored files (they don't flow through openFileE').
        restoreFileKeysE = fmapMaybe
          (\(ms, _) -> case ms of
             Just s -> case [ EditorKey f | EditorKey f <- wsTabs s ] of
                         []  -> Nothing
                         ks  -> Just (S.fromList ks)
             _ -> Nothing)
          restoreE
        -- Which tab is visible in each layout area.
        restoreVisibleE = fmapMaybe
          (\(ms, _) -> case ms of
             Just s | not (null (wsVisible s)) -> Just (M.fromList (wsVisible s))
             _ -> Nothing)
          restoreE
        -- Side-pane visibility (show / auto-hide / hide); applied as a prefs
        -- change so the root element's class (and the toggle button) update.
        restoreTallE = fmapMaybe (\(ms, _) -> ms >>= wsTall) restoreE
        -- Bottom-pane (wide1) visibility, same idea as the side pane.
        restoreWide1E = fmapMaybe (\(ms, _) -> ms >>= wsWide1) restoreE
        -- The saved flipper (MRU) order = the saved tab list itself.
        restoreRecentE = fmapMaybe
          (\(ms, _) -> case ms of
             Just s | not (null (wsTabs s)) -> Just (wsTabs s)
             _ -> Nothing)
          restoreE
        -- The saved recent-files list (for the Open Recent menu).
        restoreRecentFilesE = fmapMaybe (\(ms, _) -> ms >>= wsRecentFiles) restoreE
    -- Apply the saved MRU order just after the restored tabs have opened (so the
    -- reorder sees them all), giving the flipper the same order as last run.
    setRecentE <- delay 0.05 restoreRecentE
    -- "New Terminal": name the session @leksah-<k>@ (k past the highest existing
    -- leksah-N), create it up front, and key the new tab by the session id tmux
    -- assigns.  Falls back to the name as the key if tmux is unavailable.
    nameCounterD <- foldDyn ($) (0 :: Int) $ leftmost
      [ const . maxLeksahNum <$> existingIdsE
      , (\() k -> k + 1) <$> newTermClickE ]
    let newNameE = attachWith (\k () -> "leksah-" <> T.pack (show (k + 1))) (current nameCounterD) newTermClickE
    newTermIdE <- performEvent $ ffor newNameE $ \nm ->
      liftIO $ fromMaybe nm <$> createTerminalSession nm
    -- External-editor opens (when the "External editor command" pref is set):
    -- run e.g. `vim +<line> <file>` as a window in the shared @leksah-editor@
    -- session (file per window-tab); returns that session's id.  Routed exactly
    -- like a new terminal below.
    editTermSidE <- fmapMaybe id <$> performEvent
      (ffor (attach (current prefsD) (leftmost [openExternalE, nativeOpenExtE])) $ \(p, (file, line)) ->
         liftIO $ openFileInEditor (takeFileName file)
           (words (T.unpack (externalEditor p)) ++ ["+" <> show line, file]))
    let newOrEditTermE = leftmost [newTermIdE, editTermSidE]
    -- Once the new session/window exists, poll the tree and float its active pane
    -- to the MRU front (see newTermPolledE above) so Ctrl-` lists it on top.
    performEvent_ $ ffor newOrEditTermE $ \sid -> liftIO $ do
      tree <- listTerminalTree
      fireNewTermPolled
        (tree, maybe (FlipTab (TerminalKey sid))
                     (\(w, p) -> FlipPane sid w p) (activePaneOfSession sid tree))
    -- A repl launched from the workspace tree (termRequestE) has already selected
    -- its window in tmux; treat it exactly like a freshly-created terminal so its
    -- (now-current) active pane floats to the MRU front and the pane takes keyboard
    -- focus — i.e. it becomes THE active pane, not wherever the tree poll lands it.
    -- (Remote "ssh://" requests use their own remote-tree machinery.)
    let localTermRequestE = ffilter (not . ("ssh://" `T.isPrefixOf`)) termRequestE
    performEvent_ $ ffor localTermRequestE $ \sid -> liftIO $ do
      tree <- listTerminalTree
      fireNewTermPolled
        (tree, maybe (FlipTab (TerminalKey sid))
                     (\(w, p) -> FlipPane sid w p) (activePaneOfSession sid tree))
      focusTerminalPane sid
    -- Retry the focus once the tab has had time to mount and register its focus
    -- callback (the first repl into a session opens the tab fresh).
    delayedTermFocusE <- delay 0.3 localTermRequestE
    performEvent_ $ ffor delayedTermFocusE $ liftIO . focusTerminalPane
    -- Bells rung in a terminal's *viewed* (current) window: tmux's alert-bell
    -- hook skips those, so terminalWidget catches them (xterm onBell) and bubbles
    -- TerminalBell up through tabE.  Collect the session id(s) that just belled.
    let bellSessE = ffilter (not . null) $ ffor tabE $ \m ->
          [ n | (TerminalKey n, dm) <- M.toList m
              , Just (Identity TerminalBell) <- [DM.lookup TerminalTab dm] ]
        -- A control-mode tab saw a window created/closed: refresh the trees
        -- now instead of on the next 2s (local) / 10s (ssh) poll.
        treeChangedTabsE = ffilter (not . null) $ ffor tabE $ \m ->
          [ n | (TerminalKey n, dm) <- M.toList m
              , Just (Identity TerminalTreeChanged) <- [DM.lookup TerminalTab dm] ]
        -- A terminal whose attached tmux client exited (the session ended, e.g.
        -- `exit` in its last window): close its tab so it doesn't linger showing
        -- "[exited]".  Event-driven off tabE — the same safe feedback path as
        -- closeTermE (NOT derived from recentTabs/paneTreeD, which cycles).
        exitedTermE = ffilter (not . null) $ ffor tabE $ \m ->
          [ TerminalKey n | (TerminalKey n, dm) <- M.toList m
                          , Just (Identity TerminalExited) <- [DM.lookup TerminalTab dm] ]
        -- A session whose *displayed* window just closed: rather than follow
        -- tmux's default replacement, activate the ⌘1 button (see handler by the
        -- ⌘-number navigation).  Only meaningful for the session shown in wide0.
        activeWinClosedSessE = ffilter (not . null) $ ffor tabE $ \m ->
          [ n | (TerminalKey n, dm) <- M.toList m
              , Just (Identity TerminalActiveWinClosed) <- [DM.lookup TerminalTab dm] ]
        -- Only raise attention for a bell you weren't already looking at (i.e. the
        -- belling session isn't the active pane) — otherwise you've seen it.
        bellAwayE = fmapMaybe
          (\(active, ns) -> case filter (\n -> active /= Just (TerminalKey n)) ns of
                              [] -> Nothing; xs -> Just xs)
          (attach (current activePaneD) bellSessE)
    -- Which sessions want attention (a viewed-window bell): set on bellAwayE,
    -- cleared when the session becomes the active pane (you focused its tab), and
    -- pruned to still-live sessions.  Drives the 🔔 badge on the tab + tree.
    attentionD <- foldDyn ($) S.empty $ leftmost
      [ (\ns s -> foldr S.insert s ns) <$> bellAwayE
      , (\mk s -> case mk of Just (TerminalKey n) -> S.delete n s; _ -> s) <$> updated activePaneD
      , (\tree s -> S.intersection s (S.fromList (M.keys tree))) <$> updated paneTreeD ]
    -- Desktop notification for those bells (tmux's hook won't fire for a viewed
    -- window), via the same osascript notify script.
    performEvent_ $ ffor bellAwayE $ \ns -> liftIO (mapM_ notifyTerminalBell ns)
    -- Session id -> current name, from the flipper's pane-tree poll; labels
    -- terminal tabs (a rename shows up on the next poll).
    let terminalNamesD = fmap fst <$> paneTreeD
        -- Same, but with the session's alert badge appended (🔔/●/○), so the
        -- editor-area tab heading surfaces attention just like the Terminals-tree
        -- row does.  A leksah-tracked attention (viewed-window bell) forces 🔔;
        -- otherwise the badge comes from tmux's window flags (background windows).
        -- Only the tab labels use this; the raw names feed everything else.
        terminalTabLabelsD = (\tree att ->
             M.mapWithKey (\sid (nm, ws) ->
                 nm <> (if sid `S.member` att then " \128276" else sessionAlert ws)) tree)
             <$> paneTreeD <*> attentionD
    -- The terminal currently shown in the editor area (for highlighting in the
    -- Terminals list).
    activeTermD <- holdUniqDyn $ (\vis -> case M.lookup "wide0" vis of
                                            Just (TerminalKey n) -> Just n
                                            _                    -> Nothing) <$> visibleTabsD
    -- Publish the active terminal so the Tmux menu can send C-b sequences to it.
    performEvent_ $ liftIO . setActiveTerminal <$> updated activeTermD
    -- The wide0 tab buttons in bar order (MRU-first, deduped by identity so a
    -- multi-pane terminal window is one button); the head is the active (shown)
    -- wide0 tab.  ⌘-navigation and the ⌘-held badges both index this.  flipLiveD
    -- also carries panes of tmux sessions that AREN'T open as tabs (so the
    -- flipper can reach them) and windows that just closed but linger in the
    -- MRU — neither renders a button, so filter to identities that do: an open
    -- tab, and for a terminal window one still present in the tree.  Otherwise a
    -- phantom would consume a ⌘-number (the badges would start at ⌘2, etc.).
    orderedButtonsD <- holdUniqDyn $ (\rt tree flip' ->
            let openKeys = S.fromList [ k | (_, k) <- rt ]
                winsOf s = maybe [] (map twIndex . snd) (M.lookup s tree)
                keep (Left (s, w)) = TerminalKey s `S.member` openKeys && w `elem` winsOf s
                keep (Right k)     = k `S.member` openKeys
            in filter (keep . fst) (dedupButtons (filter ((== "wide0") . fst) flip')))
        <$> recentTabs <*> allTreeD <*> flipLiveD
    -- Panes of the active wide0 window (0 unless it's a terminal): when ≥2, they
    -- take ⌘1…⌘P and the other-window shortcuts start at ⌘(P+1).
    activeWinPaneCountD <- holdUniqDyn $ (\buttons tree -> case buttons of
            (Left (s, w) : _) -> case M.lookup s tree of
                Just (_, wins) -> maybe 0 (length . twPanes) (find ((== w) . twIndex) wins)
                Nothing        -> 0
            _                 -> 0)
        <$> (map fst <$> orderedButtonsD) <*> allTreeD
    -- ⌘-held navigation badge per non-active wide0 button: the j-th non-active
    -- button (1-based, left→right) shows ⌘(offset+j), where offset = P panes of
    -- the active window (0 when <2).  Capped at 9.
    let badgeNumsD = (\pc buttons ->
            let offset = if pc >= 2 then pc else 0
            in M.fromList [ (i, offset + j)
                          | ((i, _), j) <- zip (drop 1 buttons) [1 :: Int ..]
                          , offset + j <= 9 ])
          <$> activeWinPaneCountD <*> orderedButtonsD
    -- ⌘1…9.  When the active wide0 window is a terminal split into P≥2 panes,
    -- ⌘1…⌘P select those panes (control-mode selector, else tmux pane indexes);
    -- the rest — ⌘(P+1)… (or ⌘1… when the active window isn't split) — switch to
    -- the other wide0 tabs left→right, skipping the active one.
    let numKeyE p = fmapMaybe (\e -> e ^? _KeymapCommand . p) keymapE
    performEvent_ $ ffor (attach ((,,) <$> current activeTermD
                                        <*> current activeWinPaneCountD
                                        <*> current orderedButtonsD)
                                 (numKeyE _CommandSelectSplit)) $
        \((mbT, pc, buttons), n) -> liftIO $
            let offset = if pc >= 2 then pc else 0
            in if pc >= 2 && n <= pc
                 then do
                   ok <- selectSplitActiveTerminal n
                   unless ok $ mapM_ (\tid ->
                       tmuxCmd ["select-pane", "-t", T.unpack tid <> ":." <> show (n - 1)]) mbT
                 else case drop (n - offset) buttons of
                        ((_, fi) : _) -> fireNumFlip fi
                        _             -> return ()
    -- The displayed window closed (its last pane exited): activate the ⌘1 button
    -- — the second entry in the tab-button list — instead of tmux's own pick.
    -- The just-closed window is still buttons[0] here (the tree re-poll it
    -- triggers hasn't landed), so buttons[1] is the one used most recently
    -- before it.  Gated to the session actually shown in wide0.
    performEvent_ $ ffor (attach ((,) <$> current activeTermD <*> current orderedButtonsD)
                                 activeWinClosedSessE) $
        \((mVis, buttons), sessions) -> liftIO $
            when (maybe False (`elem` sessions) mVis) $
                case drop 1 buttons of
                    ((_, fi) : _) -> fireNumFlip fi
                    _             -> return ()
    -- Tmux pane transparency (macOS): the menu drops a toggle token; drain it to
    -- a reflex event, then toggle the active terminal's active tmux pane in/out
    -- of the holed set.  A timer re-queries each holed pane's tmux cell geometry
    -- and hands it to window.leksahSetHoles, which clips it out of the window as a
    -- see-through, click-through hole (see transparencyJs + leksah-mac-menu.m).
    (toggleTransE, fireToggleTrans) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextToggleTransparency >> fireToggleTrans ()
    -- Snapping another app's window onto a pane (macOS): the pane is also made
    -- transparent (so the window shows through); the native side captures the
    -- window and tracks it to the pane (see leksah-mac-menu.m).  Two sources: the
    -- menu (toggle the active pane, click-to-pick) and `leksah-cmd open-browser`
    -- (snap a specific pane by pane_id, bind the frontmost window).
    (snapReqE, fireSnapReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextSnapRequest >>= fireSnapReq
    let snapActiveE  = fmapMaybe (\case SnapActive -> Just (); _ -> Nothing) snapReqE
        snapPaneCmdE = fmapMaybe (\case SnapPane p -> Just p;  _ -> Nothing) snapReqE
        unsnapKeyE   = fmapMaybe (\case SnapUnsnap k -> parsePaneKey k; _ -> Nothing) snapReqE
        -- Accessibility-granted flag (published by the native side): the snap only
        -- makes a pane transparent when granted.
        readTrusted = liftJSM $ valToBool =<< jsg ("window" :: Text) ^. js ("__leksahAxTrusted" :: Text)
    let activePaneOf e = performEvent $ ffor (tag (current activeTermD) e) $ \case
            Just tid -> liftIO $ fmap (\pid -> (tid, pid)) <$> activePaneId tid
            Nothing  -> return Nothing
    toggleHoleE <- activePaneOf toggleTransE
    -- Menu snap → active terminal's active pane, with the trusted flag.
    menuSnapE <- fmap (fmapMaybe id) . performEvent $ ffor (tag (current activeTermD) snapActiveE) $ \case
        Just tid -> do
            trusted <- readTrusted
            mpid <- liftIO $ activePaneId tid
            return $ fmap (\pid -> (trusted, (tid, pid))) mpid
        Nothing  -> return Nothing
    -- Command snap → resolve which terminal the pane_id belongs to, with trusted.
    cmdSnapE <- fmap (fmapMaybe id) . performEvent $ ffor snapPaneCmdE $ \paneId -> do
        trusted <- readTrusted
        msession <- liftIO $ sessionOfPane paneId
        return $ fmap (\n -> (trusted, (n, paneId))) msession
    holedTermsD <- foldDyn (\k m -> if M.member k m then M.delete k m else M.insert k () m)
        (mempty :: M.Map (Text, Text) ()) (fmapMaybe id toggleHoleE)
    -- The set of snapped panes (one bound window each).  Menu snap toggles a pane;
    -- command snap adds one; the native Unsnap menu removes one.  Adds only take
    -- effect (make the pane transparent) when Accessibility is granted.
    snappedPanesD <- foldDyn ($) (mempty :: M.Map (Text, Text) ()) $ leftmost
        [ ffor menuSnapE  $ \(trusted, k) m -> if M.member k m then M.delete k m
                                               else if trusted then M.insert k () m else m
        , ffor cmdSnapE   $ \(trusted, k) m -> if trusted then M.insert k () m else m
        , ffor unsnapKeyE $ \k m -> M.delete k m ]
    -- Arm the native window-pick: menu → click-to-pick (only when newly snapping),
    -- command → frontmost.  Each carries the pane key the bound window belongs to.
    -- We arm even when not trusted, which just triggers the Accessibility prompt.
    performEvent_ $ ffor (fmapMaybe id (attachWith (\m (_, k) -> if M.member k m then Nothing else Just (paneKey k)) (current snappedPanesD) menuSnapE)) $ \key ->
        liftJSM . void $ jsg ("window" :: Text) ^. js2 ("leksahArmSnap" :: Text) ("click" :: Text) key
    performEvent_ $ ffor cmdSnapE $ \(_, k) ->
        liftJSM . void $ jsg ("window" :: Text) ^. js2 ("leksahArmSnap" :: Text) ("frontmost" :: Text) (paneKey k)
    -- Publish the full snapped set (regardless of visibility) so the native side
    -- keeps a window bound while its terminal is hidden, unbinding only on unsnap.
    performEvent_ $ ffor (updated snappedPanesD) $ \m ->
        liftJSM . void $ jsg ("window" :: Text) ^. js1 ("leksahSetSnapKeys" :: Text)
            ("[" <> T.intercalate "," [ "\"" <> paneKey k <> "\"" | k <- M.keys m ] <> "]")
    holeTick <- tickLossyFromPostBuildTime 0.5
    -- Refresh the holes on the tick, when the holed/snapped sets change, and when
    -- the flipper overlay shows/hides (so transparency clears while it's up).
    let refreshHolesE = leftmost [() <$ holeTick, () <$ updated holedTermsD
                                 , () <$ updated snappedPanesD, () <$ updated flipperVisibleD
                                 , () <$ updated activeTermD]
    holeGeomE <- performEvent $ ffor (tag ((,,) <$> current holedTermsD <*> current snappedPanesD <*> current activeTermD) refreshHolesE) $ \(holed, snapped, mVis) ->
        -- Only cut holes for panes of the *visible* terminal; a hidden terminal's
        -- panes are still laid out (over the visible tab) and would otherwise punch
        -- through it.  Snapped windows of hidden terminals keep their binding (see
        -- leksahSetSnapKeys below) but stay hidden until their terminal is visible.
        liftIO . fmap catMaybes . forM (filter (\(tid, _) -> Just tid == mVis) (nub (M.keys holed ++ M.keys snapped))) $ \(tid, pid) ->
            fmap (\(l, t, w, h) -> (tid, pid, l, t, w, h, M.member (tid, pid) snapped)) <$> paneGeometry tid pid
    -- While the flipper overlay is visible, clear the transparent holes so the
    -- overlay isn't punched see-through where a pane is transparent.
    performEvent_ $ ffor (attach (current flipperVisibleD) holeGeomE) $ \(flipVis, holes) ->
      if flipVis
        then liftJSM . void $ jsg ("window" :: Text) ^. js0 ("leksahClearHoles" :: Text)
        else let holeJson = "[" <> T.intercalate ","
                   [ "{\"term\":" <> T.pack (show tid)
                     <> ",\"pane\":" <> T.pack (show pid)
                     <> ",\"left\":" <> T.pack (show l) <> ",\"top\":" <> T.pack (show t)
                     <> ",\"w\":" <> T.pack (show w) <> ",\"h\":" <> T.pack (show h)
                     <> (if isSnap then ",\"snap\":true,\"key\":\"" <> paneKey (tid, pid) <> "\"" else "") <> "}"
                   | (tid, pid, l, t, w, h, isSnap) <- holes ] <> "]"
             in liftJSM . void $ jsg ("window" :: Text) ^. js1 ("leksahSetHoles" :: Text) holeJson
    -- The source file currently shown in the editor area, highlighted in the
    -- workspace and metadata trees (the "reveal the focused file" cue).
    activeFileD <- holdUniqDyn $ (\vis -> case M.lookup "wide0" vis of
                                            Just (EditorKey f) -> Just f
                                            _                  -> Nothing) <$> visibleTabsD
    -- The file to *reveal* in a tree: the focused file, unless an occurrence of
    -- it is already on-screen in that tree (it can appear more than once).
    -- Checked against the live DOM (per pane) before any expansion happens.
    let mkRevealE sel = performEvent (ffor (updated activeFileD) $ \case
            Nothing -> return Nothing
            Just f -> do
                vis <- liftJSM $ valToBool =<< jsg ("window" :: Text)
                         ^. js2 ("leksahOccurrenceVisible" :: Text) (T.pack f) (sel :: Text)
                return $ if vis then Nothing else Just f)
    revealFileE <- mkRevealE ".workspace"
    revealMetaE <- mkRevealE ".metadata"
    revealMetaD <- holdDyn Nothing revealMetaE
    -- Workspace-tree find (in Haskell): enumerate all workspace files; on find
    -- next/prev select+reveal the matching file in the tree by feeding it as the
    -- tree's highlight + reveal target (so we reuse the tree's own select/scroll
    -- rather than DOM-searching a virtualized list).
    wsFindPb <- getPostBuild
    pkgDirsD <- holdUniqDyn $ nub . map (dropFileName . ipdCabalFile)
        . (>>= pjPackages) . fromMaybe [] . (^? (workspace . _Just . wsProjects)) <$> ide
    -- Match what the tree shows: respect the show-hidden / show-ignored toggles
    -- (revealing a file the tree filters out would silently no-op).
    wsShowHiddenD  <- holdUniqDyn $ view (prefs . to showHiddenFiles)  <$> ide
    wsShowIgnoredD <- holdUniqDyn $ view (prefs . to showIgnoredFiles) <$> ide
    enumInputsD <- holdUniqDyn $ (,,) <$> pkgDirsD <*> wsShowHiddenD <*> wsShowIgnoredD
    workspaceFilesD <- holdDyn [] =<< performEvent
        (liftIO . (\(dirs, h, i) -> enumerateWorkspaceFiles h i dirs)
            <$> leftmost [updated enumInputsD, tag (current enumInputsD) wsFindPb])
    -- Keep the terminal link provider's project-file index in sync, so terminal
    -- output only turns real project files into Ctrl-clickable links.
    performEvent_ $ ffor (updated workspaceFilesD) $ \files ->
        liftJSM . void $ jsg ("LeksahTermLinks" :: Text) ^. js1 ("setProjectFiles" :: Text) files
    -- Drive the provider on/off from the "Clickable file paths…" preference: when
    -- off it claims no links, leaving OSC 8 hyperlinks in the output unobstructed.
    termLinksEnabledD <- holdUniqDyn (terminalFileLinks . view prefs <$> ide)
    termLinksEnabledPb <- getPostBuild
    performEvent_ $ ffor (leftmost [ updated termLinksEnabledD
                                   , tag (current termLinksEnabledD) termLinksEnabledPb ]) $ \en ->
        liftJSM . void $ jsg ("LeksahTermLinks" :: Text) ^. js1 ("setEnabled" :: Text) en
    -- Consume find commands only while the workspace tree is the active pane, so
    -- returning focus to it doesn't re-trigger an old search.
    let wsFindE = gate (current ((Just WorkspaceKey ==) <$> activePaneD)) findbarE
    findQueryD <- holdDyn ("", 0 :: Int) $
        fmapMaybe (\case FindUpdate q fl -> Just (q, fl); _ -> Nothing) wsFindE
    -- Parent-most matches: a directory match stands in for its children (so we
    -- don't also list every file inside a matching directory).
    let wsMatchesD = (\(q, fl) items ->
            if T.null q then []
            else let match    = findMatcher q fl
                     matching = filter (match . T.pack) items
                     covered p = any (\d -> d /= p && (d ++ "/") `isPrefixOf` p) matching
                 in sort (filter (not . covered) matching))
            <$> findQueryD <*> workspaceFilesD
    findIdxD <- foldDyn ($) (0 :: Int) $ leftmost
        [ const 0 <$ updated findQueryD
        , (\d i -> i + d) <$> fmapMaybe (\case FindStep nxt -> Just (if nxt then 1 else -1); _ -> Nothing) wsFindE ]
    let findSelectedD = (\ms i -> if null ms then Nothing else Just (ms !! (i `mod` length ms)))
            <$> wsMatchesD <*> findIdxD
        -- Reveal + highlight are event-driven: they fire on an actual find action
        -- or when the active editor file changes, never merely on focus.
        findRevealE = fmapMaybe id $ tagPromptlyDyn findSelectedD wsFindE
        -- Find commands routed to whichever list pane is active.
        paneFind k = gate (current ((Just k ==) <$> activePaneD)) findbarE
    treeRevealD    <- holdDyn Nothing $ leftmost [revealFileE, Just <$> findRevealE]
    treeHighlightD <- holdDyn Nothing $ leftmost [updated activeFileD, Just <$> findRevealE]
    -- Grep: the find bar's Grep button searches the workspace (active package
    -- first, then the rest) for the current query and lists the matches in the
    -- Grep pane.  Done by shelling out to `grep -rEn` (like the GTK pane).
    let grepReqE = fmapMaybe (\case FindGrep q fl -> Just (q, fl); _ -> Nothing) findbarE
    grepResultsE <- performEvent $ ffor (attach (current ide) grepReqE) $ \(i, (q, fl)) -> liftIO $ do
        let pkgs    = (>>= pjPackages) . fromMaybe [] $ i ^? (workspace . _Just . wsProjects)
            allDirs = nub (map ipdPackageDir pkgs)
            activeD = dropFileName <$> (i ^? workspace . _Just . wsActivePackFile . _Just)
            dirs    = case activeD of
                        Just a  -> a : filter (/= a) allDirs
                        Nothing -> allDirs
        runGrep q fl dirs
    grepResultsD <- holdDyn [] grepResultsE
    -- Opening a terminal tab (in the editor area) both creates it if needed and
    -- selects it.  Selecting a restored terminal therefore goes through the open
    -- path too: re-opening an already-open key is a no-op for `listViewWithKey`
    -- (no widget rebuild, so tmux isn't re-attached), it just becomes visible.
    -- File ▸ Close: a background thread turns close requests (from the menu
    -- command's IDEAction, via the close bridge) into a reflex event.
    (closeReqE, fireCloseReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextCloseRequest >> fireCloseReq ()
    -- Close via the menu acts on the active pane (only editors and terminals
    -- have something to close).  A terminal showing a MULTI-pane window
    -- closes the split (kills the active tmux pane) rather than the tab —
    -- the tab only goes once a single pane remains.  The pane count comes
    -- from the same tree the flipper uses, so remote (ssh://) tabs behave
    -- identically.
    let closeDecisionE = attachWith
          (\(mk, tree) () -> case mk of
              Just k@(EditorKey _) -> Just (Right [k])
              Just k@(TerminalKey n)
                | Just (_, ws) <- M.lookup n tree
                , (w : _) <- filter twActive ws
                , length (twPanes w) > 1 -> Just (Left n)
                | otherwise -> Just (Right [k])
              _ -> Nothing)
          ((,) <$> current activePaneD <*> current allTreeD) closeReqE
        killSplitE = fmapMaybe (>>= either Just (const Nothing)) closeDecisionE
        menuCloseE = fmapMaybe (>>= either (const Nothing) Just) closeDecisionE
        -- A tab × or File ▸ Close just removes the tab.  For a terminal this
        -- detaches: the tmux session (and its Terminals-list entry) survive, so
        -- it can be reopened — unlike the Terminals pane's close, which kills it.
        detachCloseE = leftmost [tabCloseBtnE, menuCloseE]
    -- Kill the split through the tab's control channel when it has one (CC
    -- tabs, remote included — the client's current pane is the displayed
    -- one); a classic local tab falls back to tmux directly.
    performEvent_ $ ffor killSplitE $ \n -> liftIO $ do
        ok <- tmuxCommandActiveTerminal "kill-pane"
        unless ok $ tmuxCmd ["kill-pane", "-t", T.unpack n]
    let openInWide0 n = TerminalKey n =: ("wide0", Just ())
        openTabsE = leftmost
          [ openFileE'
          , nativeOpenE
          , restoreOpenE
          , openInWide0 <$> newOrEditTermE
          , openInWide0 <$> termRequestE
          , openInWide0 <$> remoteOpenKeyE
          , openInWide0 <$> selectAnyTermE
          , (\(s, _, _) -> openInWide0 s) <$> flipPaneE
          , (\(s, _)    -> openInWide0 s) <$> alertTargetE
          , (PreferencesKey =: ("wide0", Just ())) <$ showPrefsE ]
        closeTabsE = leftmost [ (\n -> [TerminalKey n]) <$> closeTermE, detachCloseE, exitedTermE ]
        -- Running a grep brings the Grep pane to the front of its area; Preferences…
        -- opens and shows the Preferences pane in the editor area.  The flipper
        -- selects a tab directly, or brings up a pane's terminal in wide0.
        -- ⌥⌘N / ⌃⌘N: the Nth side- / bottom-bar pane (the strips' order —
        -- what the ⌘-held badges show).
        pickNth ks n = if n >= 1 && n <= length ks then Just (ks !! (n - 1)) else Nothing
        numSelTabE = leftmost
          [ fmapMaybe (fmap ("tall" =:)  . pickNth numberedTallTabs)
                      (numKeyE _CommandSelectSidePane)
          , fmapMaybe (fmap ("wide1" =:) . pickNth numberedWide1Tabs)
                      (numKeyE _CommandSelectBottomPane) ]
        selectTabE = leftmost [flipTabE, restoreVisibleE, ("wide1" =: GrepKey) <$ grepReqE
                              , (\(s, _, _) -> "wide0" =: TerminalKey s) <$> flipPaneE
                              , (\(s, _)    -> "wide0" =: TerminalKey s) <$> alertTargetE
                              , ("wide0" =: PreferencesKey) <$ showPrefsE
                              , numSelTabE]
    (recentTabs, tabE, visibleTabsD, activePaneD, tabCloseBtnE) <- tabsWidget
      initialTabs
      initialVisibleTabs
      openTabsE
      closeTabsE
      selectTabE
      setRecentE
      focusTabE
      mkTabButtons
      (\k selectedE v -> do
        let toDM x = fmap (DM.singleton x . Identity)
        case k of
          WorkspaceKey   -> toDM WorkspaceTab <$> workspaceWidget ide treeHighlightD treeRevealD
          ErrorsKey      -> toDM ErrorsTab <$> errorsWidget ide allE (paneFind ErrorsKey) (paneMoveE "errors") (paneActivateE "errors")
          LogKey         -> toDM LogTab <$> logWidget ide (paneFind LogKey) (paneMoveE "log") (paneActivateE "log")
          GrepKey        -> toDM GrepTab <$> grepWidget grepResultsD (paneFind GrepKey)
          TerminalsKey   -> toDM TerminalsTab <$> terminalsWidget activeTermD attentionD remoteHostsD hostTreesD
          TerminalKey n  -> toDM TerminalTab <$> do
              -- Control mode (-CC) vs classic PTY attach, decided when the
              -- tab is created (toggling the pref affects new terminals).
              -- Remote terminals ("ssh://host", from leksah-cmd cc-connect)
              -- are control-mode by construction.
              cm <- terminalControlMode . view prefs <$> sample (current ide)
              let useCC = cm || "ssh://" `T.isPrefixOf` n
              if useCC
                then terminalCCWidget ide n selectedE
                else terminalWidget ide n selectedE
          MetadataKey    -> toDM MetadataTab <$> metadataWidget ide activeFileD revealMetaD (paneFind MetadataKey)
          ChangesKey     -> toDM ChangesTab <$> changesWidget ide (paneFind ChangesKey)
          PreferencesKey -> toDM PreferencesTab <$> preferencesWidget ide
          EditorKey file -> toDM EditorTab <$> makeEditor file selectedE v)
    -- Edit ▸ Find (toolbar button / menu item) toggles the find bar; showing it
    -- focuses its text input.  It starts hidden.  The native macOS menu routes
    -- here via a background thread draining the find-toggle bridge.
    (findBridgeE, fireFindReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextFindRequest >> fireFindReq ()
    -- Toolbar/menu Find toggles the bar; Cmd+F (keymap) always shows + focuses it.
    let findToggleE = leftmost
          [ fmapMaybe (\case CommandFind -> Just (); _ -> Nothing) panelCmdE
          , findBridgeE ]
        findShowE = fmapMaybe (\e -> case e ^? _KeymapCommand of
                                       Just CommandFind -> Just (); _ -> Nothing) keymapE
    findbarVisibleD <- foldDyn ($) False $ leftmost [ not <$ findToggleE, const True <$ findShowE ]
    -- Focus the find input whenever the bar newly shows, and on every Cmd+F.
    performEvent_ $ ffor (leftmost
        [ () <$ findShowE
        , fmapMaybe (\v -> if v then Just () else Nothing) (updated findbarVisibleD) ]) $ \_ ->
      liftJSM . void $ jsg ("window" :: Text) ^. js0 ("leksahFocusFind" :: Text)
    findbarE   <- findbarWidget activePaneD findbarVisibleD
    statusbarE <- statusbarWidget ide

    -- The virtualized list panes (Errors/Log) can't be driven by DOM roving (off-
    -- screen rows aren't in the DOM), so the keyboard handler (listNavJs) calls
    -- back into reflex through these registered window callbacks, which move the
    -- pane's selection index / activate the selected row.
    (listMoveE, fireListMove)         <- newTriggerEvent
    (listActivateE, fireListActivate) <- newTriggerEvent
    -- focusin on any tab body reports its show-key here (see focusTabJs), moving
    -- that tab to the front of the MRU/flipper order.
    (focusTabE, fireFocusTab)         <- newTriggerEvent
    -- A mouse-down or ⌃B inside a terminal may have changed the active tmux pane
    -- (see leksahTermActivityJs); this asks the pane tree to be re-read.
    (termActivityE, fireTermActivity) <- newTriggerEvent
    listNavPb <- getPostBuild
    performEvent_ $ ffor listNavPb $ \_ -> liftJSM $ do
        w <- jsg ("window" :: Text)
        _ <- w ^. jss ("leksahListMove" :: Text) (fun $ \_ _ args -> case args of
                (paneV:dirV:_) -> do
                    pane <- valToText paneV
                    dir  <- valToText dirV
                    liftIO $ fireListMove (pane, dir == ("down" :: Text))
                _ -> return ())
        _ <- w ^. jss ("leksahListActivate" :: Text) (fun $ \_ _ args -> case args of
                (paneV:_) -> valToText paneV >>= liftIO . fireListActivate
                _ -> return ())
        _ <- w ^. jss ("leksahFocusTab" :: Text) (fun $ \_ _ args -> case args of
                (kV:_) -> valToText kV >>= liftIO . fireFocusTab
                _ -> return ())
        _ <- w ^. jss ("leksahTermActivity" :: Text) (fun $ \_ _ _ -> liftIO (fireTermActivity ()))
        return ()
    let paneMoveE p   = fmapMaybe (\(pane, dir) -> if pane == p then Just dir else Nothing) listMoveE
        paneActivateE p = fmapMaybe (\pane -> if pane == p then Just () else Nothing) listActivateE

    -- Activating a side ("tall") or bottom ("wide1") pane gives its list keyboard
    -- focus.  We don't change the bar's visibility pref: an auto-hidden bar is
    -- kept open purely by focus (the autohide CSS reveals on :focus-within, just
    -- like :hover — see Layout.hs), so it collapses again on its own once focus
    -- leaves.  Keyed off activePaneD (mouse-down) and the flipper (flipE) — the
    -- flipper is the only way to reach an auto-hidden pane (its tab buttons are
    -- collapsed out of view), so it must be a focus trigger here too.
    let sideBottomOf = \case
            WorkspaceKey -> Just ("tall"  :: Text, ".workspace" :: Text)
            MetadataKey  -> Just ("tall",  ".metadata")
            TerminalsKey -> Just ("tall",  ".terminals")
            ErrorsKey    -> Just ("wide1", ".errors")
            LogKey       -> Just ("wide1", ".log")
            GrepKey      -> Just ("wide1", ".grep")
            ChangesKey   -> Just ("wide1", ".changes")
            _            -> Nothing
        -- A side/bottom pane becomes active either by mouse-down in its body
        -- (activePaneD) or by the flipper selecting its tab (flipE).  The flipper
        -- is the *only* way to reach a hidden pane (its tab buttons are display:none
        -- while the bar is collapsed), so it must be a trigger here too — keying
        -- off activePaneD alone (mouse-down only) left flipped-to panes hidden.
        activatedPaneE = fmapMaybe sideBottomOf $ leftmost
            [ fmapMaybe id (updated activePaneD)
            , fmapMaybe (listToMaybe . M.elems) flipTabE
            -- ⌥⌘N / ⌃⌘N navigation focuses the pane it shows, like the flipper.
            , fmapMaybe (listToMaybe . M.elems) numSelTabE ]
    performEvent_ $ ffor activatedPaneE $ \(_, sel) ->
        liftJSM . void $ jsg ("window" :: Text) ^. js1 ("leksahFocusPane" :: Text) sel

    -- Persist the session (open files, open terminals, visible tabs) whenever it
    -- changes, but only once the saved session has been restored, so the initial
    -- (empty) state can't clobber the file before we've read it.  Debounced so a
    -- burst of restore/open events collapses into a single write.
    restoredFlagD <- holdDyn False (True <$ restoreE)
    tallD <- holdUniqDyn $ view (prefs . to tallVisibility) <$> ide
    wide1D <- holdUniqDyn $ view (prefs . to wide1Visibility) <$> ide
    -- The Preferences pane is transient — never save/restore it as an open tab.
    let notPrefs = (/= PreferencesKey)
    sessionD <- holdUniqDyn $
      (\rt vis tall recF wide1 ->
          WebSession 2 (map snd (filter (notPrefs . snd) rt))
                       (M.toList (M.filter notPrefs vis)) (Just tall) (Just recF) (Just wide1))
        <$> recentTabs <*> visibleTabsD <*> tallD <*> recentFilesD <*> wide1D
    saveSessE <- debounce (1 :: NominalDiffTime) (gate (current restoredFlagD) (updated sessionD))
    performEvent_ $ ffor saveSessE $ liftIO . writeWebSession

    -- Persist preference toggles (toolbar buttons: show hidden/ignored files,
    -- build flags, ...) so they survive a restart.  They already load at startup
    -- via readPrefs; here we write them back on change.  (Side-pane visibility is
    -- session-only and lives in the web session, not the prefs file.)
    prefsD <- holdUniqDyn $ view prefs <$> ide
    prefsSaveE <- debounce (1 :: NominalDiffTime) (updated prefsD)
    performEvent_ $ ffor prefsSaveE $ \p -> liftIO $
      getConfigFilePathForSave standardPreferencesFilename >>= \path -> writePrefs path p
    -- The colour prefs bind the CSS variables the stylesheets reference
    -- (--leksah-selection / --leksah-hover; see "IDE.Web.Theme") in a live
    -- style element, so the Preferences colour pickers apply immediately.
    themeCssD <- holdUniqDyn $
        (\p -> themeVarsCss (uiSelectionColor p) (uiHoverColor p)) <$> prefsD
    el "style" $ dynText themeCssD
    -- Publish the shortcut-badges preference to the ⌘-held handler (badgesJs).
    badgesPrefD <- holdUniqDyn (showShortcutBadges <$> prefsD)
    badgesPb <- getPostBuild
    performEvent_ $ ffor (leftmost [updated badgesPrefD, tag (current badgesPrefD) badgesPb]) $ \v ->
        liftJSM . void $ jsg ("window" :: Text)
            ^. jss ("__leksahShortcutBadges" :: Text) v
    -- Colourful-icons preference → the icon-src swapper (colorIconsJs).
    colorPrefD <- holdUniqDyn (colorfulIcons <$> prefsD)
    colorPb <- getPostBuild
    performEvent_ $ ffor (leftmost [updated colorPrefD, tag (current colorPrefD) colorPb]) $ \v ->
        liftJSM . void $ jsg ("window" :: Text)
            ^. js1 ("leksahSetColorIcons" :: Text) v

    let allE = merge (DM.fromList
            [ MenubarWidget   :=> menubarE
            , ToolbarWidget   :=> toolbarE
            , TabWidget       :=> tabE
            , FindbarWidget   :=> findbarE
            , StatusbarWidget :=> statusbarE
            , KeymapWidget    :=> keymapE
            ])
        workspaceE = select (fan (select (fanMap tabE) (Const2 WorkspaceKey))) WorkspaceTab
        prefsPaneE = select (fan (select (fanMap tabE) (Const2 PreferencesKey))) PreferencesTab
        editorE' = switchDyn $ leftmost . map (select (fanMap tabE) . Const2) . toList <$> openFileKeysD
        editorE = select (fan editorE') EditorTab

    return $ sequence_ <$>
         ((^.. _ToolbarCommand . commandAction . _Just) <$> toolbarE)
      <> ((^.. _MenubarCommand . commandAction . _Just) <$> menubarE)
      <> ((^.. _KeymapCommand . commandAction . _Just) <$> keymapE)
      <> ((^.. traverse . _ProjectCommand . commandAction . _Just) <$> workspaceE)
      <> ((^.. traverse . _ProjectPackageEvents . traverse . _PackageCommand . commandAction . _Just) <$> workspaceE)
      <> ((^.. (to $ \() -> do
        tb <- readIDE triggerBuild
        void . liftIO $ tryPutMVar tb ())) <$> editorE)
      <> ((\v -> [modifyIDE_ (prefs %~ \p -> p { tallVisibility = v })]) <$> restoreTallE)
      <> ((\v -> [modifyIDE_ (prefs %~ \p -> p { wide1Visibility = v })]) <$> restoreWide1E)
      <> ((\(PrefsUpdate f) -> [modifyIDE_ (prefs %~ f)]) <$> prefsPaneE)
  return topEvents
