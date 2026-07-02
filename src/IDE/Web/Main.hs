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
import Control.Monad (forever, forM, when, void)
import Control.Monad.IO.Class (MonadIO(..))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Char8 as BS (unlines)
import qualified Data.ByteString.Lazy as BS (toStrict)
import qualified Data.Dependent.Map as DM (singleton, fromList, lookup)
import Data.Dependent.Sum (DSum(..))
import Data.Foldable (Foldable(..), forM_)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Misc (Const2(..))
import Data.IORef (newIORef, atomicModifyIORef')
import Data.Map (mapKeys)
import qualified Data.Map as M
       (Map, keys, elems, toList, fromList, union, findWithDefault, lookup, null,
        insert, delete, member, filter, singleton, map, mapWithKey, empty)
import Data.Map (Map)
import qualified Data.Set as S
       (fromList, delete, singleton, empty, insert, member, intersection)
import Data.Time.Clock (NominalDiffTime)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, unlines, isInfixOf, isPrefixOf, toLower, null, intercalate, breakOn, drop, stripPrefix, takeWhile)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import Text.Printf (printf)
import Text.Read (readMaybe)

import System.Directory
       (doesFileExist, doesDirectoryExist, getDirectoryContents, removeFile,
        getHomeDirectory)
import System.Process (readProcessWithExitCode)
import Data.List (nub, sort, isPrefixOf, isInfixOf, find, elemIndex)
import Data.Maybe (fromMaybe, catMaybes, maybeToList, listToMaybe)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, dropFileName, (</>))
import System.Environment (getArgs)
import System.Posix.Process (exitImmediately)
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

import Language.Javascript.JSaddle
       (JSM, eval, syncPoint, jsg, js, js0, js1, js2, jss, fun, valToText, valToBool, liftJSM)
import Language.Javascript.JSaddle.Warp
       (jsaddleJs, jsaddleOr, debugWrapper)
import GHCJS.DOM.Types (askJSM)
import GHCJS.DOM.Debug (addDebugMenu)

import Reflex
       (switchDyn, mergeList, foldDyn, traceEventWith, constDyn, ffor,
        Dynamic, Event, holdDyn, merge, newTriggerEvent, leftmost, never,
        performEvent_, getPostBuild, performEvent, select, fan, fanMap,
        fmapMaybe, ffilter, attachWith, attach, current, updated, holdUniqDyn, tag, gate,
        listViewWithKey, sample,
        tagPromptlyDyn, debounce, delay, tickLossyFromPostBuildTime)
import Reflex.Dom.Core
       (dynText, elAttr', elDynAttr, elDynAttr', text, domEvent, EventName(..),
        (=:), MonadWidget, mainWidgetWithCss)

import IDE.Core.State
       (triggerBuild, readIDE, IDEAction, wsFile, jsContexts, workspace,
        IDEState(..), Prefs(..), TallVisibility(..), IDE(..), IDERef, __,
        reflectIDE, getDataDir, catchIDE, modifyIDE_, prefs, currentState,
        wsProjects, pjPackages, ipdCabalFile, ipdPackageDir, wsActivePackFile)
import IDE.Metainfo.Provider (initInfo)
import IDE.Web.IDERefStore (setGlobalIDERef)
import IDE.Web.CmdServer (startCmdServer, suppressNextRestart)
import IDE.Web.CloseRequest (nextCloseRequest)
import IDE.Web.OpenFileRequest (nextOpenedFile)
import IDE.Web.OpenPanel (runOpenFilePanel, runOpenProjectPanel)
import IDE.Web.SaveRequest (nextSaveRequest)
import IDE.Web.FindRequest (nextFindRequest)
import IDE.Web.RemoteTermRequest (nextRemoteTerm)
import IDE.Web.RecentFiles (updateRecentFiles)
import IDE.Web.TerminalInput (setActiveTerminal)
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
import IDE.Web.Command (commandAction, Command(..))
import IDE.Web.Events
       (IDEWidget(..), TabEvents(..), TabKey(..), TerminalEvents(..),
        FindbarEvents(..), PreferencesEvents(..), FlipItem(..),
        _ToolbarCommand, _MenubarCommand, _KeymapCommand, _PackageCommand,
        _ProjectPackageEvents, _ProjectCommand, _NewTerminal, _SelectTerminal,
        _CloseTerminal, _SelectTerminalWindow, _SelectTerminalPane,
        _NewRemoteTerminal, _SelectRemoteTerminal, _SelectRemoteTerminalWindow,
        _SelectRemoteTerminalPane)
import IDE.Web.Layout (layoutCss)
import IDE.Web.Widget.Changes (changesCss, changesWidget)
import IDE.Web.Widget.Preferences (preferencesCss, preferencesWidget)
import IDE.Web.Widget.Flake (flakeCss)
import IDE.Web.Widget.ContextMenu (contextMenuCss)
import IDE.Web.Widget.Editor (editorCss, editorWidget)
import IDE.Web.Widget.Errors (errorsCss, errorsWidget)
import IDE.Web.Widget.Findbar (findbarCss, findbarWidget, findMatcher)
import IDE.Web.Widget.Flipper (flipperCss, flipperWidget)
import IDE.Web.Widget.Grep (grepCss, grepWidget, runGrep, GrepResult)
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
        remoteTabTree, remoteTabHostTarget,
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

      triggerBuild <- newEmptyMVar
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

-- | Flipper label for a tmux pane: "session-name · w.pane: pane-title".  Uses the
-- session's current tmux name (from the tree, so a rename shows up) and the
-- per-pane title carried in 'tpLabel'.
flipPaneLabel :: Text -> Int -> Int -> Map Text (Text, [TmuxWindow]) -> Text
flipPaneLabel n w p tree =
  maybe n fst (M.lookup n tree) <> " · " <> T.pack (show w) <> "."
    <> case [ tpLabel pn | wn <- maybe [] snd (M.lookup n tree), twIndex wn == w
                         , pn <- twPanes wn, tpIndex pn == p ] of
         (l:_) -> l
         []    -> T.pack (show p)

-- | A CSS @order@ style attribute for a wide0 tab button (empty off wide0).
orderStyle :: Maybe Int -> Map Text Text
orderStyle = maybe mempty (\n -> "style" =: ("order:" <> T.pack (show n)))

-- | The wide0 tab-button order, taken from the flipper's item list: each tmux
-- window (identified by @Left (session, window)@, collapsed from its panes) and
-- each non-terminal tab (@Right key@), in the flipper's MRU-first order, one
-- entry apiece.  Windows of one session are NOT grouped — each is ordered
-- independently, exactly as the flipper cycles panes.  (The list isn't always
-- perfectly current, as noted for the flipper MRU, but it's close.)
buttonOrderOf :: [(Text, FlipItem)] -> [Either (Text, Int) TabKey]
buttonOrderOf = nub . map (ident . snd)
  where ident (FlipPane s w _) = Left (s, w)
        ident (FlipTab k)      = Right k

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
  , "      else if (e.key === 'Enter' || e.key === ' ') { if (cur) cur.click(); e.preventDefault(); return; }"
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
  , "    function mv(e2){"
  , "      var d = (vert ? e2.clientX : e2.clientY) - start;"
  , "      if (line) line.style.transform = vert ? ('translateX('+d+'px)') : ('translateY('+d+'px)');"
  , "    }"
  , "    function up(e2){"
  , "      document.removeEventListener('mousemove', mv);"
  , "      document.removeEventListener('mouseup', up);"
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
    -- The Open / Open Project commands (from the toolbar or web menubar) show the
    -- native open panels.  (The native macOS menu triggers these directly.)
    let panelCmdE = leftmost
          [ fmapMaybe (^? _ToolbarCommand) toolbarE
          , fmapMaybe (^? _MenubarCommand) menubarE ]
    performEvent_ $ ffor panelCmdE $ \case
      CommandFileOpen    -> liftIO runOpenFilePanel
      CommandProjectOpen -> liftIO runOpenProjectPanel
      _                  -> return ()
    -- Edit ▸ Preferences… (menu/toolbar) or ⌘, (keymap) opens the Preferences pane.
    let showPrefsE = leftmost
          [ fmapMaybe (\case CommandShowPreferences -> Just (); _ -> Nothing) panelCmdE
          , fmapMaybe (\e -> case e ^? _KeymapCommand of
                               Just CommandShowPreferences -> Just (); _ -> Nothing) keymapE ]
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
                -> Maybe Text -> Maybe Text -> m () -> IO ()
                -> m (Event t (Map Text TabKey, [TabKey]))
      tabButton area k selectedD orderStyleD mbTitle mbCloseTip labelW onSel =
        elDynAttr "span"
            ((\sel ost -> "class" =: ("tab-wrap" <> if sel then " selected" else "") <> ost)
               <$> selectedD <*> orderStyleD) $ do
          closeE <- case mbCloseTip of
            Just tip -> do
              (xe, _) <- elAttr' "span" ("class" =: "tab-close" <> "title" =: tip) $ text "×"
              pure ([k] <$ domEvent Click xe)
            Nothing  -> pure never
          (be, _) <- elAttr' "button" (maybe mempty ("title" =:) mbTitle) labelW
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
                                   Just w  -> twLabel w <> windowAlert w <> bell)
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
            tabButton area k selectedD orderStyleD Nothing Nothing (dynText labelD) onSel
          pure (mconcat . M.elems <$> winButtonsE)
        _ ->
          let mbTitle    = case k of EditorKey f -> Just (T.pack f); _ -> Nothing
              mbCloseTip = case k of EditorKey _ -> Just "Close"; _ -> Nothing
              orderStyleD = buttonOrderStyleD (Right k) baseOrderD
          in tabButton area k isVisibleD orderStyleD mbTitle mbCloseTip
               (dynText (tabLabelText k <$> terminalTabLabelsD)) (pure ())

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
    -- `leksah-cmd cc-connect HOST` → a remote control-mode terminal tab
    -- (TerminalCC over ssh), keyed "ssh://HOST".
    (remoteTermHostE, fireRemoteTerm) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextRemoteTerm >>= fireRemoteTerm
    let remoteTermE = ("ssh://" <>) <$> remoteTermHostE
    -- Hosts shown as top-level Terminals-tree nodes: the preference list plus
    -- any host that has an open ssh:// tab.
    remoteHostsD <- holdUniqDyn $ (\p rt -> nub $ remoteHosts p ++
          [ T.takeWhile (/= '#') rest
          | (_, TerminalKey n) <- rt, Just rest <- [T.stripPrefix "ssh://" n] ])
        <$> prefsD <*> recentTabs
    -- Which widget each open terminal tab got (session id -> control mode?),
    -- so the Terminals tree can show the terminal type.
    (ccTypeE, fireCCType) <- newTriggerEvent
    ccTypesD <- foldDyn (uncurry M.insert) M.empty ccTypeE
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
          , () <$ closeTermE, () <$ selectAnyTermE, () <$ saveSessE ]
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
    -- Remote (ssh://) tabs' window/pane trees, keyed by the TAB key and merged
    -- into the same tree the flipper and tab ordering consume ('allTreeD'), so
    -- remote windows flip and order exactly like local ones.  Polled over ssh
    -- (10s, off the reflex thread) for whichever remote tabs are open.
    remoteTabsD <- holdUniqDyn $
        (\rt -> nub [ n | (_, TerminalKey n) <- rt, "ssh://" `T.isPrefixOf` n ])
          <$> recentTabs
    remoteFlipTick <- tickLossyFromPostBuildTime 10
    (remoteTreesE, fireRemoteTrees) <- newTriggerEvent
    -- Poked right after a remote window/pane select, so the active-window
    -- highlight moves at once instead of on the next 10s poll.
    (remotePokeE, fireRemotePoke) <- newTriggerEvent
    performEvent_ $ ffor (leftmost [ tag (current remoteTabsD) remoteFlipTick
                                   , tag (current remoteTabsD) remotePokeE
                                   , updated remoteTabsD ]) $ \tabs ->
        liftIO . void . forkIO $ do
            entries <- forM tabs $ \n -> fmap (\t -> (n, t)) <$> remoteTabTree n
            fireRemoteTrees (M.fromList (catMaybes entries))
    remoteFlipD <- holdDyn M.empty remoteTreesE
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
    let flipSelE  = fmapMaybe (listToMaybe . M.toList) flipRawE
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
        remoteKey h sid = "ssh://" <> h <> "#" <> sid
    performEvent_ $ ffor closeTermE $ liftIO . killTerminalSession
    performEvent_ $ ffor selectWinE  $ \(s, w)    -> liftIO (selectTmuxWindow s w)
    performEvent_ $ ffor selectPaneE $ \(s, w, p) -> liftIO (selectTmuxPane s w p)
    performEvent_ $ ffor selRemoteWinE  $ \(h, s, w)    -> liftIO (selectRemoteTmuxWindow h s w)
    performEvent_ $ ffor selRemotePaneE $ \(h, s, w, p) -> liftIO (selectRemoteTmuxPane h s w p)
    -- "+" on a remote host: create a session there, then open its tab.
    remoteNewSidE <- performEvent $ ffor newRemoteE $ \h ->
        liftIO $ fmap (remoteKey h) <$> createRemoteSession h
    let remoteOpenKeyE = leftmost
          [ fmapMaybe id remoteNewSidE
          , (\(h, sid)       -> remoteKey h sid) <$> selRemoteE
          , (\(h, sid, _)    -> remoteKey h sid) <$> selRemoteWinE
          , (\(h, sid, _, _) -> remoteKey h sid) <$> selRemotePaneE ]
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
    -- Bells rung in a terminal's *viewed* (current) window: tmux's alert-bell
    -- hook skips those, so terminalWidget catches them (xterm onBell) and bubbles
    -- TerminalBell up through tabE.  Collect the session id(s) that just belled.
    let bellSessE = ffilter (not . null) $ ffor tabE $ \m ->
          [ n | (TerminalKey n, dm) <- M.toList m
              , Just (Identity TerminalBell) <- [DM.lookup TerminalTab dm] ]
        -- A terminal whose attached tmux client exited (the session ended, e.g.
        -- `exit` in its last window): close its tab so it doesn't linger showing
        -- "[exited]".  Event-driven off tabE — the same safe feedback path as
        -- closeTermE (NOT derived from recentTabs/paneTreeD, which cycles).
        exitedTermE = ffilter (not . null) $ ffor tabE $ \m ->
          [ TerminalKey n | (TerminalKey n, dm) <- M.toList m
                          , Just (Identity TerminalExited) <- [DM.lookup TerminalTab dm] ]
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
    -- have something to close).
    let menuCloseE = fmapMaybe (\case Just k@(EditorKey _)   -> Just [k]
                                      Just k@(TerminalKey _) -> Just [k]
                                      _                      -> Nothing)
                       (tag (current activePaneD) closeReqE)
        -- A tab × or File ▸ Close just removes the tab.  For a terminal this
        -- detaches: the tmux session (and its Terminals-list entry) survive, so
        -- it can be reopened — unlike the Terminals pane's close, which kills it.
        detachCloseE = leftmost [tabCloseBtnE, menuCloseE]
    let openInWide0 n = TerminalKey n =: ("wide0", Just ())
        openTabsE = leftmost
          [ openFileE'
          , nativeOpenE
          , restoreOpenE
          , openInWide0 <$> newOrEditTermE
          , openInWide0 <$> remoteTermE
          , openInWide0 <$> remoteOpenKeyE
          , openInWide0 <$> selectAnyTermE
          , (\(s, _, _) -> openInWide0 s) <$> flipPaneE
          , (\(s, _)    -> openInWide0 s) <$> alertTargetE
          , (PreferencesKey =: ("wide0", Just ())) <$ showPrefsE ]
        closeTabsE = leftmost [ (\n -> [TerminalKey n]) <$> closeTermE, detachCloseE, exitedTermE ]
        -- Running a grep brings the Grep pane to the front of its area; Preferences…
        -- opens and shows the Preferences pane in the editor area.  The flipper
        -- selects a tab directly, or brings up a pane's terminal in wide0.
        selectTabE = leftmost [flipTabE, restoreVisibleE, ("wide1" =: GrepKey) <$ grepReqE
                              , (\(s, _, _) -> "wide0" =: TerminalKey s) <$> flipPaneE
                              , (\(s, _)    -> "wide0" =: TerminalKey s) <$> alertTargetE
                              , ("wide0" =: PreferencesKey) <$ showPrefsE]
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
          TerminalsKey   -> toDM TerminalsTab <$> terminalsWidget activeTermD attentionD remoteHostsD ccTypesD
          TerminalKey n  -> toDM TerminalTab <$> do
              -- Control mode (-CC) vs classic PTY attach, decided when the
              -- tab is created (toggling the pref affects new terminals).
              -- Remote terminals ("ssh://host", from leksah-cmd cc-connect)
              -- are control-mode by construction.
              cm <- terminalControlMode . view prefs <$> sample (current ide)
              let useCC = cm || "ssh://" `T.isPrefixOf` n
              liftIO $ fireCCType (n, useCC)
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
            , fmapMaybe (listToMaybe . M.elems) flipTabE ]
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
