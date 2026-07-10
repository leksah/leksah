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
  , jsMain
  , indexHtml
  , mintWindowId
  ) where

import Control.Concurrent
       (tryPutMVar, takeMVar, putMVar, readMVar, threadDelay, modifyMVar,
        newMVar, newEmptyMVar, forkIO, myThreadId)
import GHC.Conc.Sync (labelThread)
import Control.Event (registerEvent)
import Control.Exception (SomeException, catch)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import GHC.Stats
       (getRTSStats, getRTSStatsEnabled, RTSStats(..), GCDetails(..))
import qualified System.IO as IO (hPutStrLn, stderr, hSetBuffering, BufferMode(..))
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
       (Map, keys, elems, toList, fromList, union, findWithDefault, lookup,
        insert, insertWith, adjust, delete, member, filterWithKey,
        singleton, mapWithKey, empty, size)
import Data.Map (Map)
import qualified Data.Set as S
       (fromList, delete, singleton, empty, insert, member, intersection)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, unlines, isPrefixOf, null, intercalate, breakOn, drop, stripPrefix, takeWhile, all, splitOn)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import Text.Printf (printf)
import Text.Read (readMaybe)

import System.Directory
       (doesFileExist, doesDirectoryExist, getDirectoryContents, removeFile,
        getHomeDirectory, makeRelativeToCurrentDirectory)
import System.Process (readProcessWithExitCode)
import Data.Aeson (encode)
import Data.List (nub, sort, isPrefixOf, isInfixOf, find, elemIndex)
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, takeExtension, dropFileName, (</>))
import System.Environment (getArgs, setEnv)
import IDE.Utils.ExitImmediately (exitImmediately)
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
       (JSM, eval, syncPoint, jsg, js, js0, js1, js2, js3, jss, fun, valToText, valToBool, liftJSM)
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
        fmapMaybe, ffilter, attachWith, attachWithMaybe, attach, current, updated, holdUniqDyn, tag, gate,
        listViewWithKey, sample,
        tagPromptlyDyn, debounce, delay, tickLossyFromPostBuildTime)
import Reflex.Dom.Core
       (dynText, el, elAttr, elAttr', elDynAttr, elDynAttr', text, domEvent, EventName(..),
        (=:), MonadWidget, mainWidgetWithCss)

import IDE.Core.State
       (triggerBuild, readIDE, IDEAction, wsFile, jsContexts, workspace,
        IDEState(..), Prefs(..), TallVisibility(..), IDE(..), IDERef, __,
        reflectIDE, getDataDir, catchIDE, modifyIDE_, modifyIDE, prefs, currentState,
        wsProjects, pjPackages, ipdCabalFile, ipdPackageDir, wsActivePackFile,
        currentError, logRefFullFilePath, refDescription, logRefSrcSpan,
        srcSpanStartLine,
        WindowId(..), WebWindow(..), webWindows, activeWindow, nextWindowId,
        flipMirror, flipMru, ideVersion, focusLog, metaLog)
import IDE.Metainfo.Provider (initInfo)
import IDE.Web.IDERefStore (setGlobalIDERef)
import IDE.Web.Instance (leksahPort)
import IDE.Web.CmdServer (startCmdServer, suppressNextRestart)
import IDE.Web.OpenPanel (runOpenFilePanel, runOpenProjectPanel)
import IDE.Web.Theme (themeVarsCss)
import IDE.Web.WindowBridge
       (WindowBridge(..), registerWindowBridge, startWindowBridgeDrains,
        registerResync, notifyResync)
import IDE.Web.RegionGrabRequest (nextRegionGrab)
import IDE.Web.ScreenshotRequest (requestScreenshotRegion)
import IDE.Web.RegionCapture
       (screenCaptureAllowed, grabRegionToTarget, sendPathToTarget,
        sendTextToTarget, nextRegionFile, resolveTmuxSessionId)
import IDE.Web.AIContextRequest (AIAction(..), nextAIAction)
import IDE.Web.RemoteTermRequest (nextTermRequest)
import IDE.Web.RecentFiles (updateRecentFiles)
import IDE.Web.ReplTmux (tmuxCmd, tmuxSupported)
import IDE.Web.TerminalInput
       (setActiveTerminal, tmuxCommandActiveTerminal, selectSplitActiveTerminal,
        focusTerminalPane, dispatchTmuxPrefix)
import IDE.Web.TransparencyRequest (nextToggleTransparency)
import IDE.Web.SnapRequest (SnapReq(..), nextSnapRequest)
import IDE.Web.Session
       (WebSession(..), WebWindowSession(..), readWebSession, writeWebSession)
import IDE.Web.NewWindowRequest (requestOpenWindow, requestRaiseWindow)
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
        _SelectRemoteTerminalWindow, _SelectRemoteTerminalPane,
        _CloseRemoteTerminal, _NewRemoteTerminalWindow, _KillRemoteTerminalWindow,
        _RenameRemoteTerminalSession, _RenameRemoteTerminalWindow,
        _ZoomRemoteTerminalPane, _BreakRemoteTerminalPane, _KillRemoteTerminalPane)
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
        killRemoteTmuxSession, killRemoteTmuxWindow, killRemoteTmuxPane,
        newRemoteTmuxWindow, zoomRemoteTmuxPane, breakRemoteTmuxPane,
        renameRemoteTmuxSession, renameRemoteTmuxWindow,
        listRemoteTerminalTree, remoteTabHostTarget,
        reapControlClients, TmuxWindow(..), TmuxPane(..))
import IDE.Web.Widget.Terminals (terminalsCss, terminalsWidget, sessionAlert, windowAlertSrc)
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
  -- The Changes / file-tree / workspace panes poll `git status` and `git diff`
  -- every few seconds; those refresh the index and so take .git/index.lock,
  -- contending with git commands the user (or an agent) runs in a terminal.
  -- GIT_OPTIONAL_LOCKS=0 makes git skip that optional index-refresh lock for
  -- read-only operations (write locks for commit/add etc. are unaffected), so
  -- the pollers no longer fight foreground git.  Inherited by every child git.
  setEnv "GIT_OPTIONAL_LOCKS" "0"
  -- The bundled JS (cm6, xterm) is UTF-8 and read with locale-dependent
  -- readFile; force UTF-8 so a C/POSIX locale (headless/CI) can't break it.
  setLocaleEncoding utf8
  -- stderr defaults to NoBuffering, so concurrent diagnostic writes from many
  -- threads (wlog, [MUT], [meta], focus) interleave char-by-char.  LineBuffering
  -- makes each hPutStrLn atomic under GHC's handle lock, keeping log lines whole.
  IO.hSetBuffering IO.stderr IO.LineBuffering
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
            ,   _webWindows        =   mempty
            ,   _activeWindow      =   Nothing
            ,   _nextWindowId      =   0
            ,   _flipMirror        =   Nothing
            ,   _flipMru           =   []
            ,   _ideVersion        =   0
      }
      -- The trigger slot runs after every 'modifyIDEM', on the mutating thread:
      -- keep it down to the non-blocking resync signals (see 'notifyResync' —
      -- each window's own notifier thread then fires that window's coalesced
      -- resync).  NEVER put reflex trigger fires or JS in this slot.
      ideR <- liftIO $ newMVar (const notifyResync, ide)
      liftIO $ setGlobalIDERef ideR  -- so the native macOS menu can run commands
      liftIO $ startCmdServer ideR   -- control socket for the leksah-cmd CLI
      -- Single process-wide drains for the "act on the active window" bridges
      -- (close/save/find/prefs/open-file); each window's network registers its
      -- triggers via 'registerWindowBridge' and the drain routes to the frontmost.
      liftIO $ startWindowBridgeDrains ideR
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
      -- Multi-window restore: read the saved session and SEED every saved
      -- window's per-window state (wide0 tabs — terminals filtered to still-live
      -- tmux sessions — plus its side/bottom visibility) into the shared MVar
      -- BEFORE any reflex network attaches, so each window renders its own wide0
      -- directly (editors instantiate via 'makeEditor').  Positions persist
      -- natively (Cocoa per-window frame autosave), so none are stored here.
      liftIO $ do
        mbSession <- readWebSession
        liveTerms <- listTerminalSessions
        let liveIds  = map fst liveTerms
            keepTab k = case k of
              TerminalKey n  -> n `elem` liveIds
              PreferencesKey -> False   -- transient, never restore
              _              -> True
            dfltWin  = WebWindowSession [] Nothing (tallVisibility initPrefs)
                                        (wide1Visibility initPrefs)
            wwsList  = case mbSession of
                         Just s | not (null (wsWindows s)) -> wsWindows s
                         _                                 -> [dfltWin]
            seeded   = M.fromList
              [ (WindowId i
                , WebWindow (filter keepTab (wwsWide0 w))
                            (wwsActive w >>= \a -> if keepTab a then Just a else Nothing)
                            (wwsTall w) (wwsWide1 w) Nothing)
              | (i, w) <- zip [0 ..] wwsList ]
            nWins    = length wwsList
        (`reflectIDE` ideR) $ modifyIDE_ $ \i ->
          i & webWindows .~ seeded & nextWindowId .~ nWins & activeWindow ?~ WindowId 0
        -- Ask native to create the windows past the first (the first is created
        -- by the wkwebview AppDelegate / warp connection and attached below).
        -- No-op on warp (no handler), which stays single-window.
        mapM_ requestOpenWindow [1 .. nWins - 1]
      runJs $ jsMain showMenubar macTitlebar (Just (WindowId 0)) ideR

develMain :: IO ()
develMain = do
  dev <- elem "--develop-leksah" <$> getArgs
  newIDE True False dev (debugJSaddle leksahPort)

-- | The default per-window state a freshly-minted (or adopted-but-unseeded)
-- window inherits: no wide0 tabs, and side/bottom pane visibility taken from the
-- global prefs (which stay the shared default; see 'IDE.Core.Types.WebWindow').
defaultWebWindow :: Prefs -> WebWindow
defaultWebWindow p = WebWindow
  { _wwWide0 = [], _wwActive = Nothing
  , _wwTall = tallVisibility p, _wwWide1 = wide1Visibility p, _wwFrame = Nothing }

-- | Allocate a fresh 'WindowId', seed a default 'WebWindow' for it, and make it
-- the active window if none is yet.  Runs the shared trigger so any already-open
-- window observes the new (empty) window immediately (e.g. in its flipper).
mintWindowId :: IDERef -> IO WindowId
mintWindowId ideR = (`reflectIDE` ideR) $ modifyIDE $ \ide ->
  let n   = ide ^. nextWindowId
      wid = WindowId n
      ide' = ide & nextWindowId .~ (n + 1)
                 & webWindows %~ M.insert wid (defaultWebWindow (ide ^. prefs))
                 & activeWindow %~ Just . fromMaybe wid
  in (ide', wid)

-- | Adopt a 'WindowId' the native side already created, ensuring its 'WebWindow'
-- exists (idempotent: keeps any entry the native seed already inserted).
adoptWindowId :: WindowId -> IDERef -> IO ()
adoptWindowId wid ideR = (`reflectIDE` ideR) $ modifyIDE_ $ \ide ->
  ide & webWindows %~ M.insertWith (\_ old -> old) wid (defaultWebWindow (ide ^. prefs))
      & activeWindow %~ Just . fromMaybe wid

-- | The wide0 (editor/terminal) state a window has before it is seeded — used
-- only as a 'M.findWithDefault' fallback (every live window is seeded first).
emptyWebWindow :: WebWindow
emptyWebWindow = WebWindow [] Nothing TallShow TallShow Nothing

-- | Move wide0 tab @k@ into window @w@ as its new MRU-front active tab, removing
-- it from whichever window currently owns it.  This is how "open from the shared
-- side panes moves it into the acting window" and cross-window moves both work:
-- a wide0 tab belongs to exactly one window at a time.
moveTabTo :: WindowId -> TabKey -> Map WindowId WebWindow -> Map WindowId WebWindow
moveTabTo w k = M.mapWithKey $ \wid' ww ->
  if wid' == w
    then ww { _wwWide0 = k : filter (/= k) (_wwWide0 ww), _wwActive = Just k }
    else let w0' = filter (/= k) (_wwWide0 ww)
         in ww { _wwWide0 = w0'
               , _wwActive = if _wwActive ww == Just k then listToMaybe w0' else _wwActive ww }

-- | Float an already-owned wide0 tab to the MRU-front of window @w@ and make it
-- active.  No-op if @w@ doesn't own @k@ (so activating a side/bottom tab is safe).
activateWide0 :: WindowId -> TabKey -> Map WindowId WebWindow -> Map WindowId WebWindow
activateWide0 w k = M.adjust
  (\ww -> if k `elem` _wwWide0 ww
            then ww { _wwWide0 = k : filter (/= k) (_wwWide0 ww), _wwActive = Just k }
            else ww) w

-- | Remove wide0 tabs @ks@ from window @w@; if the active tab was among them,
-- fall back to the new MRU-front (the close-reselect rule).
closeWide0 :: WindowId -> [TabKey] -> Map WindowId WebWindow -> Map WindowId WebWindow
closeWide0 w ks = M.adjust
  (\ww -> let w0' = filter (`notElem` ks) (_wwWide0 ww)
          in ww { _wwWide0 = w0'
                , _wwActive = if maybe False (`elem` ks) (_wwActive ww)
                              then listToMaybe w0' else _wwActive ww }) w

-- | Each OS window gets its own vivid, stable hue derived from its id (golden-ish
-- step for good separation).  Used for the flipper border and the per-entry
-- window icons — the same formula lives in JS ('flipMirrorJs'), so a window's
-- colour is identical everywhere it appears.
windowHue :: WindowId -> Int
windowHue (WindowId n) = (n * 67) `mod` 360

windowColorCss :: WindowId -> Text
windowColorCss w = "hsl(" <> T.pack (show (windowHue w)) <> ",85%,55%)"

-- | Verbose multi-window diagnostic logging to stderr (lands in
-- ~/.leksah/leksah-nix-wkwebview.log).  Every line is tagged @LEK <time> [win N]@
-- so the two OS windows' event streams can be told apart and correlated; grep
-- @LEK@ to isolate.  Pure Haskell — no JS involved.
wlog :: MonadIO m => WindowId -> String -> m ()
wlog (WindowId n) msg = liftIO $ do
    t <- getCurrentTime
    let tod = takeWhile (/= ' ') . drop 11 $ show t   -- HH:MM:SS.sss
    IO.hPutStrLn IO.stderr ("LEK " <> tod <> " [win " <> show n <> "] " <> msg)

-- | Which OS window a flip item currently lives in (the window whose wide0 owns
-- its tab key), if any.  Shared side/bottom tabs and panes not open in any
-- window's wide0 have no owner ('Nothing').
flipOwnerWindow :: Map WindowId WebWindow -> FlipItem -> Maybe WindowId
flipOwnerWindow wins fi =
  let k = case fi of FlipTab t -> t; FlipPane s _ _ -> TerminalKey s
  in listToMaybe [ w | (w, ww) <- M.toList wins, k `elem` _wwWide0 ww ]

jsMain :: Bool -> Bool -> Maybe WindowId -> IDERef -> JSM ()
jsMain showMenubar macTitlebar mbWid ideR = do
  -- enableLogging True -- Uncomment this to add verbose JSaddle logging
  dataDir <- liftIO getDataDir
  ctx <- askJSM
  -- Resolve this connection's window identity: adopt the id the native side
  -- pre-created (restore / New Window), or mint one (the first wkwebview window
  -- and every leksah-warp browser connection).
  wid <- liftIO $ maybe (mintWindowId ideR) (\w -> adoptWindowId w ideR >> return w) mbWid
  -- Tag this context with its window id, so tooling (leksah-cmd js eval, which
  -- broadcasts to every context) can tell the windows apart.
  _ <- eval ("window.leksahWindowId = " <> T.pack (show (case wid of WindowId n -> n)))
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
  -- Inline images (SIXEL + iTerm2 OSC 1337) rendered into the scrollback —
  -- img2sixel/chafa/imgcat etc. just work (tmux passes them through via
  -- allow-passthrough, see ReplTmux.writeTmuxConf).
  _ <- liftIO (readFile $ dataDir </> "xterm/addon-image.js") >>= eval
  -- OSC 52: programs in a terminal (vim/tmux copy-mode, incl. over ssh where
  -- pbcopy can't reach) set the system clipboard.
  _ <- liftIO (readFile $ dataDir </> "xterm/addon-clipboard.js") >>= eval

  -- Makes project-file paths in terminal output Ctrl-clickable (window.LeksahTermLinks).
  _ <- eval terminalLinksJs
  _ <- eval badgesJs

  -- Defines window.LeksahTmux: the tmux C-b prefix interceptor attached to each
  -- xterm.  Enabled per the tmuxInterceptPrefix pref (mirrored in from reflex).
  _ <- eval leksahTmuxJs

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

  -- window.leksahCollapseAutoHide: snap an auto-hidden side/bottom pane shut when
  -- a selection in it activates a file/terminal, even if the cursor is still over
  -- the pane (the CSS reveal is hover-driven; this overrides it).
  _ <- eval collapseAutoHideJs

  -- Esc collapses the auto-hidden side/bottom bar when focus is inside it.
  _ <- eval escAutoHideJs

  -- window.leksahFlipMirror/Hide: the global flipper mirror overlay (a copy of
  -- another window's open flipper), driven by ideJSM_ broadcasts from main.
  _ <- eval flipMirrorJs

  -- The single top-level active-pane shadow overlay (window.leksahUpdatePaneHl).
  _ <- eval paneHlJs
  _ <- eval hintsJs

  -- window.leksahSetColorIcons: swap every /pics/*.svg between the monochrome
  -- default and the coloured set (/pics/color/*.svg), driven by the pref.
  _ <- eval colorIconsJs

  -- The Claude-coordination traffic light (top-right dot): leksahTestStart /
  -- leksahTestEnd / leksahStatus, driven over the cmd socket via `js eval`.
  _ <- eval statusLightJs

  -- window.leksahSelectRegion: the permission-free region picker for grab-region.
  _ <- eval regionSelectJs

  mainWidgetWithCss (BS.unlines [xtermCss, BS.toStrict (LT.encodeUtf8 css)]) $ mdo
      ideActionE <- main showMenubar macTitlebar wid ideD
      performEvent_ $ ffor ideActionE $ \act -> do
          wlog wid "ENTER ideAction (reflectIDE)"
          liftIO ((`reflectIDE` ideR) act)
          wlog wid "EXIT ideAction"
      -- Register this context, but DO NOT compose a reflex trigger into the ideR
      -- trigger slot.  The old design had modifyIDE_ run @oldT x >> t x@ — one
      -- un-coalesced frame per mutation for EVERY window.  All windows share the
      -- single global Spider lock, and each frame holds it across synchronous
      -- jsaddle JS, so a mutation burst floods frames that pile up on that lock
      -- and hard-freezes a background window (observed: its heartbeat stops dead
      -- mid-idle).  Instead each window learns of shared-state changes through
      -- its COALESCED resync (below): the trigger slot only sets a per-window
      -- binary signal, and a per-window notifier thread fires at most one resync
      -- frame at a time.  Pure reflex, no JS.
      newIde <- liftIO $ modifyMVar ideR $ \(oldT, oldIde) -> do
          let newIde' = oldIde & jsContexts %~ (<> [ctx])
          return ((oldT, newIde'), newIde')
      wlog wid ("network attached; contexts now=" <> show (length (newIde ^. jsContexts))
                <> " initial ideVer=" <> show (newIde ^. ideVersion))
      pb <- getPostBuild
      -- Leave the start-up state and load metadata, as the GTK front end does
      -- (`initInfo` here forks the heavy load via `postAsyncIDE = forkIDE`, so
      -- this returns promptly).  Without leaving `IsStartingUp`, the metadata
      -- commands (e.g. Update Workspace Info) silently no-op.
      performEvent_ $ ffor pb $ \_ -> do
        wlog wid "ENTER pb-initInfo"
        (liftIO . (`reflectIDE` ideR) $ do
          -- Load metadata exactly ONCE across all OS windows.  initInfo is called
          -- from every window's post-build; running it per window forked N full
          -- metadata loads into the shared state, blowing the heap (79→687MB) and
          -- freezing a window under GC thrash / OOM.  Atomically flip currentState
          -- to IsRunning and let only the FIRST window to do so (old state
          -- IsStartingUp) run initInfo — the result lives in shared state, so one
          -- load serves every window.
          firstToRun <- modifyIDE $ \i ->
              ( i & currentState .~ IsRunning
              , case i ^. currentState of IsStartingUp -> True; _ -> False )
          -- Load-once guard (above) means one metadata load serves every OS
          -- window.  Historically kept OFF because the load ballooned the heap to
          -- 500MB+; re-enabled here — flip back to False if the heap regresses.
          let metadataEnabled = True
          metaLog $ "post-build " <> show wid <> " firstToRun=" <> show firstToRun
                  <> " metadataEnabled=" <> show metadataEnabled
          if metadataEnabled && firstToRun
            then do metaLog $ "post-build " <> show wid <> " -> initInfo"
                    initInfo (return ())
                    metaLog $ "post-build " <> show wid <> " initInfo returned (load forked)"
            else return ())
        wlog wid "EXIT pb-initInfo"
      pbIde <- performEvent $ pb $> liftIO (snd <$> readMVar ideR)
      -- Cross-window updates, event-driven but COALESCED and SERIALIZED:
      -- 'modifyIDEM' sets this window's binary resync signal (see the ideR trigger
      -- slot / 'notifyResync'); the per-window notifier fires resyncE, then blocks
      -- until the handler below acks — so at most one resync frame is in flight
      -- per window and a mutation burst collapses into the already-set signal.
      -- The notifier ALSO holds a process-global lock across fire→ack
      -- ('resyncGlobalLock'), so no two windows run a resync frame at once: the
      -- real freeze was two windows' frames doing synchronous jsaddle flushes
      -- CONCURRENTLY on the shared WKWebView main-thread bridge, wedging one
      -- frame thread forever (see WindowBridge).  (A 'newTriggerEvent' fire is
      -- only a writeChan; the frame runs on this window's own host thread
      -- regardless of who fires.)  The version guard means a no-op signal
      -- advances nothing, and the 1 s heartbeat below re-feeds the guard so a
      -- momentarily-starved background window self-heals.
      (resyncE, fireResync) <- newTriggerEvent
      resyncAck <- liftIO newEmptyMVar
      liftIO $ registerResync wid (fireResync ()) resyncAck
      polledIdeE <- performEvent $ ffor resyncE $ \() -> do
          wlog wid "ENTER resync (readMVar ideR)"
          i <- liftIO (snd <$> readMVar ideR)
          wlog wid "EXIT resync"
          return i
      -- Ack (release the global serialize lock) only AFTER this resync frame's DOM
      -- has been built AND flushed: deferring one frame ('delay 0') pushes the ack
      -- past the frame-end jsaddle syncPoint, so the synchronous flush that wedges
      -- falls INSIDE the lock — no two windows ever flush the shared WKWebView
      -- main-thread bridge at once.  (Releasing at the source event, before the
      -- syncPoint, still let sustained bursts overlap and wedge a window.)
      resyncSettledE <- delay 0 (void polledIdeE)
      performEvent_ $ ffor resyncSettledE $ \_ -> liftIO (putMVar resyncAck ())
      -- Freeze detector: a coarse (1s) keepalive per window.  If a window stops
      -- emitting "alive" lines, its reflex network has frozen.  Doubles as the
      -- fallback poll: it feeds the same version guard, so a lost resync signal
      -- self-heals within a second.
      heartbeatTick <- tickLossyFromPostBuildTime 1
      heartbeatE <- performEvent $ ffor heartbeatTick $ \_ -> liftIO (snd <$> readMVar ideR)
      let freshPolledE = attachWithMaybe
            (\cur new -> if new ^. ideVersion > cur ^. ideVersion then Just new else Nothing)
            (current ideD) (leftmost [polledIdeE, heartbeatE])
      performEvent_ $ ffor (attach (current ideD) heartbeatE) $ \(cur, new) -> do
          -- Label this window's frame thread (processAsyncEvents) so a
          -- `leksah-cmd threads` dump can tell the windows apart.  Idempotent.
          liftIO (myThreadId >>= (`labelThread` ("reflex-frames-" <> show wid)))
          wlog wid ("alive ideVer=" <> show (cur ^. ideVersion) <> " mvarVer=" <> show (new ^. ideVersion)
                    <> if new ^. ideVersion > cur ^. ideVersion then " STALE(+" <> show (new ^. ideVersion - cur ^. ideVersion) <> ")" else "")
      performEvent_ $ ffor freshPolledE $ \i -> wlog wid ("ideD<-resync ver=" <> show (i ^. ideVersion))
      ideD <- holdDyn newIde $ leftmost [pbIde, freshPolledE]
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
            -- Front ends that NAVIGATE to the server instead of injecting the
            -- index HTML natively (webview2) land here.
            ("GET", []) ->
                 sendResponse
                    $ W.responseLBS H.status200
                        [("Content-Type", "text/html; charset=utf-8")]
                    $ LBS.fromStrict indexHtml
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
  WorkspaceKey   -> Just "/pics/workspace.svg"
  TerminalsKey   -> Just "/pics/terminals.svg"
  MetadataKey    -> Just "/pics/metadata.svg"
  EditorKey file -> Just (fileIconSrc file)
  _              -> Nothing

-- | The file-type icon (same set the Workspace file tree uses) for an editor
-- tab, keyed by extension.
fileIconSrc :: FilePath -> Text
fileIconSrc file = "/pics/" <> case takeExtension file of
  ".cabal" -> "tree-file-cabal.svg"
  ".hs"    -> "tree-file-hs.svg"
  ".lhs"   -> "tree-file-hs.svg"
  _        -> "tree-file.svg"

-- | The active @(window index, pane index)@ of a tmux session (by id), from the
-- pane tree; falls back to the first window/pane, or 'Nothing' if it has none.
activePaneOfSession :: Text -> Map Text (Text, [TmuxWindow]) -> Maybe (Int, Int)
activePaneOfSession n tree = do
  (_, wins) <- M.lookup n tree
  w <- listToMaybe (filter twActive wins ++ wins)
  p <- listToMaybe (filter tpActive (twPanes w) ++ twPanes w)
  return (twIndex w, tpIndex p)

-- | The spoken location of a session's belled window — the tmux (terminal)
-- window NAME, then the pane number when the window has more than one pane, e.g.
-- "leksah, 1" (or just "leksah" for a single-pane window; no "window"/"pane"
-- words, and NOT the OS window name) — for the terminal-bell announcement.  The
-- bell fires in the session's viewed (current/active) window; the pane is that
-- window's active pane.
bellLocation :: Text -> Map Text (Text, [TmuxWindow]) -> Maybe Text
bellLocation n tree = do
  (_, wins) <- M.lookup n tree
  w <- listToMaybe (filter twActive wins ++ wins)
  let name = stripIdxPrefix (twIndex w) (twLabel w)
      pnum = maybe (twIndex w) tpIndex
                   (listToMaybe (filter tpActive (twPanes w) ++ twPanes w))
  -- A lone pane needs no number — the window name alone locates it.
  return $ if length (twPanes w) <= 1
             then name
             else name <> ", " <> T.pack (show pnum)

-- | Resolve a tmux @#{pane_id}@ (e.g. @%5@, server-global) to its flipper item
-- by scanning the pane tree for the pane's @(session, window index, pane index)@.
-- This is a pure *translation* of a pane leksah already identified (a click or a
-- leksah-issued select) into the indices 'FlipPane' carries — NOT a reading of
-- "which pane tmux thinks is active", so it stays a leksah-owned recency signal.
flipForPaneId :: Text -> Map Text (Text, [TmuxWindow]) -> Maybe FlipItem
flipForPaneId pid tree = listToMaybe
  [ FlipPane n (twIndex w) (tpIndex p)
  | (n, (_, wins)) <- M.toList tree, w <- wins, p <- twPanes w, tpId p == pid ]

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

-- | The leading icon (a @/pics/*.svg@ path) for a flipper entry, or 'Nothing'
-- for entries shown by label alone.  A terminal — whether the item is a whole
-- terminal tab or one of its panes (which is all a terminal flip item ever is) —
-- gets its tmux *window* icon, carrying the same fill/colour alert state the
-- Terminals tree shows (bell/activity/…); a file tab gets its file-type icon;
-- the side-pane tabs reuse 'tabIconSrc'.  The state-carrying window icon means a
-- belled terminal is spottable in the flipper at a glance.
flipIconSrc :: Map Text (Text, [TmuxWindow]) -> FlipItem -> Maybe Text
flipIconSrc tree = \case
    FlipPane n w _          -> Just (winIcon n w)
    FlipTab (TerminalKey n) -> Just (winIcon n (activeWinIdx n))
    FlipTab k               -> tabIconSrc k
  where
    winIcon n w = case M.lookup n tree >>= find ((== w) . twIndex) . snd of
        Just win -> windowAlertSrc win
        Nothing  -> "/pics/tree-window-idle.svg"
    activeWinIdx sid = maybe 0 twIndex $ M.lookup sid tree
        >>= (\wins -> listToMaybe (filter twActive wins ++ wins)) . snd

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
        when (i < 9) . elAttr "span" ("class" =: "leksah-shortcut-badge") $ do
            text (pre <> T.pack (show (i + 1)))
            elAttr "span" ("class" =: "leksah-flip-suffix") $ text " \8984`"

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

-- | The @data-flipkey@ string a tab button carries, so a published flip target
-- (see 'hintTarget'/'hintsJs') can find the button in the DOM.  Mirrors
-- 'flipButtonId': a tmux window is @win:SESSION:INDEX@, any other tab
-- @tab:KEY@.  'hintTarget' produces the same strings for the published target.
winFlipKey :: Text -> Int -> Text
winFlipKey s w = "win:" <> s <> ":" <> T.pack (show w)

tabFlipKey :: TabKey -> Text
tabFlipKey k = "tab:" <> case k of
    WorkspaceKey   -> "workspace"
    ErrorsKey      -> "errors"
    LogKey         -> "log"
    GrepKey        -> "grep"
    TerminalsKey   -> "terminals"
    MetadataKey    -> "metadata"
    ChangesKey     -> "changes"
    PreferencesKey -> "preferences"
    TerminalKey s  -> "terminal:" <> s
    EditorKey f    -> "editor:" <> T.pack f

-- | Where the ⌘` hint chip should sit for the flipper's one-press destination
-- @fi@: on the on-screen terminal pane it would go to (@Left paneId@, when that
-- terminal is the front wide0 tab and its shown window holds the pane), else on
-- the tab button that would be selected (@Right flipKey@).  'front' is the
-- session id shown in wide0 (Nothing if wide0 isn't a terminal).
hintTarget :: Maybe Text -> Map Text (Text, [TmuxWindow]) -> FlipItem -> Maybe (Either Text Text)
hintTarget front tree = \case
    FlipPane s w p
      | front == Just s
      , Just win <- windowOf s w
      , twActive win
      , Just pn <- find ((== p) . tpIndex) (twPanes win)
      -> Just (Left (tpId pn))                       -- on-screen sibling pane
      | otherwise -> Just (Right (winFlipKey s w))   -- off-screen: its window button
    FlipTab (TerminalKey n)                          -- a terminal tab = its current window button
      | Just (_, wins) <- M.lookup n tree
      , w <- maybe 0 twIndex (listToMaybe (filter twActive wins) `orElse` listToMaybe wins)
      -> Just (Right (winFlipKey n w))
      | otherwise -> Nothing
    FlipTab k -> Just (Right (tabFlipKey k))
  where
    windowOf s w = M.lookup s tree >>= find ((== w) . twIndex) . snd
    orElse a b = maybe b Just a

-- | How to highlight the flipper's live selection @fi@: @(Just paneId, tabKey)@
-- where @paneId@ is set only when the selection is a pane in the on-screen active
-- window (→ move the active-pane shadow onto it), and @tabKey@ is the
-- @data-flipkey@ of the tab button to tint with the hover colour (always).
flipSelHighlight :: Maybe Text -> Map Text (Text, [TmuxWindow]) -> FlipItem -> (Maybe Text, Text)
flipSelHighlight front tree = \case
    FlipPane s w p ->
        ( case windowOf s w of
            Just win | front == Just s, twActive win ->
                tpId <$> find ((== p) . tpIndex) (twPanes win)
            _ -> Nothing
        , winFlipKey s w )
    FlipTab (TerminalKey n) -> (Nothing, winFlipKey n (activeWinIdx n))
    FlipTab k               -> (Nothing, tabFlipKey k)
  where
    windowOf s w = M.lookup s tree >>= find ((== w) . twIndex) . snd
    activeWinIdx sid = maybe 0 twIndex $ M.lookup sid tree
        >>= (\wins -> listToMaybe (filter twActive wins ++ wins)) . snd

-- | The flipper's item list: MRU order first, then every current tmux pane and
-- non-terminal tab (a terminal is represented only by its panes).  The flipper
-- is global across OS windows: @otherTabs@ are the non-terminal wide0 tabs
-- (editors) owned by OTHER windows — tmux panes are already global (they come
-- from @tree@, not per-window), so only editor tabs need adding here.  On
-- commit, a selection owned by another window raises that window (see the
-- ownership split in 'main').
buildFlipItems :: [FlipItem] -> [(Text, TabKey)] -> [TabKey] -> Map Text (Text, [TmuxWindow]) -> [(Text, FlipItem)]
buildFlipItems mru rt otherTabs tree =
  let panes = [ FlipPane n (twIndex w) (tpIndex p)
              | (n, (_, wins)) <- M.toList tree, w <- wins, p <- twPanes w ]
      notTerm (TerminalKey _) = False
      notTerm _               = True
      tabs    = [ FlipTab k | (_, k) <- rt, notTerm k ]
             ++ [ FlipTab k | k <- otherTabs, notTerm k ]
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
  -- While the flipper is open, the shadow highlights its selected pane instead of
  -- the focused one: leksahFlipSel.pane is the on-screen pane id (null => the
  -- selection isn't an on-screen pane, so show no shadow).  Closed => follow focus.
  , "    var fs = window.leksahFlipSel, t;"
  , "    if (fs) { t = fs.pane ? document.querySelector('.terminal-cc-pane[data-pane='+JSON.stringify(fs.pane)+']') : null; }"
  , "    else { t = paneOf(document.activeElement); }"
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

-- | Command-held navigation hints, shown/hidden by the @body.leksah-show-badges@
-- class:
--
--   * ⌘` — the flipper's one-press destination.  Reflex publishes the target via
--     'leksahSetFlipTarget' as either a terminal pane (@%id@) or a tab button
--     (@data-flipkey@); this tags that target's numbered badge (⌘N pane badge,
--     or a tab button's ⌘N/⌥⌘N/⌃⌘N badge) @.leksah-flip-here@, which reveals the
--     hidden \" ⌘`\" suffix inside it — so a coincident target reads \"⌘N ⌘`\".
--   * ⌘D / ⌘⇧D — absolute overlay chips (like 'paneHlJs') on the active terminal
--     pane (the one a split would act on, found via its shown highlight marker):
--     ⌘D at the bottom-middle, ⌘⇧D at the right-middle.
--
-- Recomputed (rAF-debounced) on focus change, resize, the Meta keydown that
-- reveals the badges, and whenever the flip target changes.
hintsJs :: Text
hintsJs = T.unlines
  [ "(function(){"
  , "  window.leksahFlipTarget = null;"
  , "  function box(){ var c = document.querySelector('.leksah-hints');"
  , "    if(!c){ c = document.createElement('div'); c.className='leksah-hints';"
  , "      c.innerHTML = '<div class=\"leksah-hint leksah-hint-splitd\">\\u2318D</div>'"
  , "        + '<div class=\"leksah-hint leksah-hint-splitr\">\\u2318\\u21e7D</div>';"
  , "      document.body.appendChild(c); }"
  , "    return c; }"
  , "  function shown(el){ if(!el) return false; var cs=getComputedStyle(el);"
  , "    return cs.display!=='none' && cs.visibility!=='hidden' && el.offsetParent!==null; }"
  , "  function put(el,x,y){ el.style.left=x+'px'; el.style.top=y+'px'; el.style.display=''; }"
  -- The on-screen terminal's active-pane box (its shown highlight marker); the
  -- pane a ⌘D/⌘⇧D split would act on.  data-pane pairs marker and pane.
  , "  function activePaneEl(){ var ms=document.querySelectorAll('.terminal-cc-hl');"
  , "    for(var i=0;i<ms.length;i++){ if(shown(ms[i])){"
  , "      var win=ms[i].closest('.terminal-cc-window'), pid=ms[i].getAttribute('data-pane');"
  , "      var p = (win&&pid) ? win.querySelector('.terminal-cc-pane[data-pane='+JSON.stringify(pid)+']') : null;"
  , "      if(shown(p)) return p; } } return null; }"
  , "  function update(){ var c=box();"
  , "    var sd=c.querySelector('.leksah-hint-splitd'), sr=c.querySelector('.leksah-hint-splitr');"
  , "    sd.style.display='none'; sr.style.display='none';"
  , "    var ap=activePaneEl();"
  , "    if(ap){ var r=ap.getBoundingClientRect();"
  , "      put(sd, r.right - 18, r.top + r.height/2);"
  , "      put(sr, r.left + r.width/2, r.bottom - 12); }"
  -- Flip destination: reveal the ⌘` suffix on its numbered badge (a pane by
  -- data-pane, else a tab button's badge by data-flipkey), so it reads "⌘N ⌘`".
  , "    var prev=document.querySelectorAll('.leksah-shortcut-badge.leksah-flip-here');"
  , "    for(var i=0;i<prev.length;i++) prev[i].classList.remove('leksah-flip-here');"
  , "    var ft=window.leksahFlipTarget, badge=null;"
  , "    if(ft && ft.pane){ badge=document.querySelector('.leksah-shortcut-badge[data-pane='+JSON.stringify(ft.pane)+']'); }"
  , "    else if(ft && ft.button){ var tw=document.querySelector('[data-flipkey='+JSON.stringify(ft.button)+']');"
  , "      badge = tw ? tw.querySelector('.leksah-shortcut-badge') : null; }"
  , "    if(badge) badge.classList.add('leksah-flip-here');"
  -- Tab-row badges are position:fixed (to clear the strip's overflow clip): set
  -- each one's coordinates from its tab's live rect, centred just above the row.
  -- Hide any whose tab is scrolled out of its strip.
  , "    var tb=document.querySelectorAll('.tab-buttons .leksah-shortcut-badge');"
  , "    for(var j=0;j<tb.length;j++){ var bg=tb[j];"
  , "      var tw2=bg.closest('.tab-wrap'), strip=bg.closest('.tab-buttons');"
  , "      if(!tw2||!strip||!shown(tw2)){ bg.style.visibility='hidden'; continue; }"
  , "      var tr=tw2.getBoundingClientRect(), srect=strip.getBoundingClientRect();"
  , "      if(tr.right<=srect.left+1 || tr.left>=srect.right-1){ bg.style.visibility='hidden'; continue; }"
  , "      bg.style.visibility=''; bg.style.left=(tr.left+tr.width/2)+'px'; bg.style.top=(tr.top-14)+'px'; }"
  , "  }"
  , "  var scheduled=false;"
  , "  window.leksahUpdateHints=function(){ if(scheduled) return; scheduled=true;"
  , "    requestAnimationFrame(function(){ scheduled=false; update(); }); };"
  , "  window.leksahSetFlipTarget=function(kind,val){"
  , "    window.leksahFlipTarget = kind==='pane' ? {pane:val} : (kind==='button' ? {button:val} : null);"
  , "    window.leksahUpdateHints(); };"
  -- The flipper's LIVE selection (while it's open): move the active-pane shadow
  -- onto the selected pane (paneHlJs reads window.leksahFlipSel) and tint its tab
  -- button with the hover colour.  Empty button => flipper closed (clear both).
  , "  window.leksahFlipSel = null;"
  , "  window.leksahSetFlipSel=function(pane,button){"
  , "    window.leksahFlipSel = button ? {pane: pane||null, button:button} : null;"
  , "    var prev=document.querySelectorAll('.leksah-flip-sel');"
  , "    for(var i=0;i<prev.length;i++) prev[i].classList.remove('leksah-flip-sel');"
  , "    if(button){ var tw=document.querySelector('[data-flipkey='+JSON.stringify(button)+']');"
  , "      if(tw) tw.classList.add('leksah-flip-sel'); }"
  , "    if(window.leksahUpdatePaneHl) window.leksahUpdatePaneHl(); };"
  , "  document.addEventListener('focusin', window.leksahUpdateHints, true);"
  , "  window.addEventListener('resize', window.leksahUpdateHints);"
  , "  window.addEventListener('keydown', function(e){ if(e.key==='Meta') window.leksahUpdateHints(); }, true);"
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

-- | Defines @window.leksahCollapseAutoHide()@: collapse any auto-hidden side
-- ("tall") or bottom ("wide1") pane NOW, even while the mouse is over it.  The
-- auto-hide reveal is pure CSS (@:has(:hover, :focus-within)@), so a selection
-- that activates a file/terminal — moving keyboard focus to the editor but
-- leaving the cursor parked over the tree it was clicked in — would otherwise
-- keep the pane open.  We add a @…-suppress@ class that overrides the hover
-- reveal back to the collapsed state, then drop it again once the pointer is no
-- longer over that pane's reveal triggers (so the very next fresh hover reopens
-- it as usual); a timeout is a safety net if the mouse never moves.
collapseAutoHideJs :: Text
collapseAutoHideJs = T.unlines
  -- Optional `only` ('tall' | 'wide1') collapses just that bar; omitted -> both.
  [ "window.leksahCollapseAutoHide = function(only){"
  , "  var root = document.querySelector('.leksah'); if (!root) return;"
  , "  var specs = ["
  , "    { auto:'tall-auto',  sup:'tall-suppress',  sel:'.tall-sensor, .area-tall' },"
  , "    { auto:'wide1-auto', sup:'wide1-suppress', sel:'.statusbar, .tab.area-wide1, .tab-buttons.area-wide1' }"
  , "  ];"
  , "  specs.forEach(function(s){"
  , "    if (only && s.auto.indexOf(only) !== 0) return;"
  , "    if (!root.classList.contains(s.auto)) return;"
  , "    root.classList.add(s.sup);"
  , "    var timer = null;"
  , "    function clear(){ root.classList.remove(s.sup);"
  , "      document.removeEventListener('mousemove', onMove, true);"
  , "      if (timer) clearTimeout(timer); }"
  , "    function overTrigger(x, y){ var el = document.elementFromPoint(x, y);"
  , "      return !!(el && el.closest && el.closest(s.sel)); }"
  , "    function onMove(e){ if (!overTrigger(e.clientX, e.clientY)) clear(); }"
  , "    document.addEventListener('mousemove', onMove, true);"
  , "    timer = setTimeout(clear, 1500);"
  , "  });"
  , "};"
  ]

-- | Esc, when keyboard focus is inside ANY side ('tall') or bottom ('wide1') bar
-- pane, returns focus to the top non-sidebar/bottombar pane in the flipper MRU —
-- i.e. the central (@wide0@) pane you were last working in.  That MRU-front wide0
-- pane is, by construction, the currently *shown* wide0 tab (activating a wide0
-- pane floats it to the MRU front and makes it the visible one), so we just find
-- the visible @.tab.area-wide0@ and focus its natural target: a CodeMirror
-- editor's @.cm-content@, a terminal's xterm textarea, or the tab body itself.
-- This works whether or not the bar is in auto-hide mode; when it IS auto-hide,
-- we additionally 'leksahCollapseAutoHide' that bar so a revealed list pane
-- (Workspace, Errors, Log, …) snaps shut on the same Esc — the collapse/re-reveal
-- behaviour then matches a selection-driven collapse exactly.  Moving focus into
-- wide0 also releases the bar's @:focus-within@, so we never strand focus in a
-- now-hidden pane.  A focused TERMINAL in the bar is exempt — Esc is a real key
-- there (vim, less, …) — as are INPUT/TEXTAREA/contentEditable fields (rename
-- boxes, the find input) and any Esc a more specific handler already consumed
-- (@defaultPrevented@).  Handled in JS (jsaddle's async dispatch makes a Haskell
-- @preventDefault@ unreliable, and the focus move must be synchronous).
escAutoHideJs :: Text
escAutoHideJs = T.unlines
  [ "(function(){"
  , "  function shown(el){ if(!el) return false; var cs=getComputedStyle(el);"
  , "    return cs.display!=='none' && cs.visibility!=='hidden' && el.offsetParent!==null; }"
  -- Focus the top non-sidebar/bottombar (wide0) pane: the visible central tab,
  -- which is this window's MRU-front wide0 pane.  Returns true iff focus moved.
  , "  function focusTopWide0(){"
  , "    var tabs = document.querySelectorAll('.tab.area-wide0');"
  , "    for (var i=0;i<tabs.length;i++){ var t=tabs[i]; if(!shown(t)) continue;"
  , "      var cm = t.querySelector('.cm-content');"
  , "      if (cm){ cm.focus(); return true; }"
  , "      var tx = t.querySelector('.xterm-helper-textarea');"
  , "      if (tx){ tx.focus(); return true; }"
  , "      if (!t.hasAttribute('tabindex')) t.setAttribute('tabindex','-1');"
  , "      t.focus(); return true; }"
  , "    return false;"
  , "  }"
  , "  document.addEventListener('keydown', function(e){"
  , "    if (e.key !== 'Escape' || e.defaultPrevented) return;"
  , "    if (e.metaKey || e.ctrlKey || e.altKey || e.shiftKey) return;"
  , "    var root = document.querySelector('.leksah'); if (!root) return;"
  , "    var a = document.activeElement; if (!a || !a.closest) return;"
  -- Terminals and text fields own Esc; leave it to them.
  , "    if (a.closest('.xterm')) return;"
  , "    if (a.tagName === 'INPUT' || a.tagName === 'TEXTAREA' || a.isContentEditable) return;"
  -- Only act when focus is inside a side (tall) or bottom (wide1) bar pane.
  , "    var inTall = !!a.closest('.area-tall'), inWide1 = !!a.closest('.area-wide1');"
  , "    if (!inTall && !inWide1) return;"
  -- Return focus to the top wide0 pane; if there is none, at least blur so the
  -- bar pane releases keyboard focus (and :focus-within, so an auto bar collapses).
  , "    try { if (!focusTopWide0() && a.blur) a.blur(); } catch(_) {}"
  -- If that bar is in auto-hide mode, snap it shut now (no-op otherwise).
  , "    if (inTall  && root.classList.contains('tall-auto'))  window.leksahCollapseAutoHide('tall');"
  , "    if (inWide1 && root.classList.contains('wide1-auto')) window.leksahCollapseAutoHide('wide1');"
  , "    e.preventDefault(); e.stopPropagation();"
  , "  }, false);"
  , "})();"
  ]

-- | Defines @window.leksahFlipMirror(owner, labelsJson, index)@ and
-- @window.leksahFlipMirrorHide(owner)@: a read-only copy of another OS window's
-- open flipper, so the flipper shows on every window at once.  The active window
-- broadcasts these (via 'ideJSM_') on open / step / close; a window ignores the
-- call for its OWN flipper (@owner@ == its id — it shows the real reflex one).
-- Deliberately plain DOM over the existing @.flipper@ CSS, NOT reflex shared
-- state: a per-keystroke burst of cross-window @modifyIDE_@ fan-out drops and
-- reorders the trigger fires to background webviews (leaving the mirror stuck),
-- whereas ordered fire-and-forget @evaluateJavaScript@ always lands the final
-- hide.  The label list is rebuilt only when it changes (@__leksahKey@); a step
-- just moves the @selected@ class.
-- | Defines the per-window colour helper, tags this window with its own colour
-- (the @--leksah-window-color@ CSS var the flipper border reads), and the flipper
-- mirror.  The mirror shows this window's open flipper on every OTHER OS window;
-- each entry carries its owner window id so we can draw an owner-coloured window
-- icon, and thicken the border on the window that owns the highlighted item.  The
-- hue formula matches 'windowHue' so a window's colour is identical everywhere.
flipMirrorJs :: Text
flipMirrorJs = T.unlines
  [ "window.leksahWindowColor = function(id){ return 'hsl(' + ((id*67)%360) + ',85%,55%)'; };"
  , "document.documentElement.style.setProperty('--leksah-window-color', window.leksahWindowColor(window.leksahWindowId));"
  -- items: [[label, ownerWinId, iconSrc], ...]  (ownerWinId < 0 = shared / no
  -- owner; iconSrc "" = no type icon)
  , "window.leksahFlipMirror = function(owner, itemsJson, index){"
  , "  if (window.leksahWindowId === owner) return;"   -- our own flipper is the real one
  , "  var el = document.getElementById('leksah-flip-mirror');"
  , "  if (!el){"
  , "    el = document.createElement('div'); el.id = 'leksah-flip-mirror';"
  , "    el.className = 'flipper';"
  , "    el.innerHTML = '<div class=\"flipper-scroll\"><div class=\"flipper-content\"></div></div>';"
  , "    if (document.body) document.body.appendChild(el);"
  , "  }"
  , "  var c = el.querySelector('.flipper-content');"
  , "  if (el.__leksahKey !== itemsJson){"                 -- rebuild the list only when it changes
  , "    var items = JSON.parse(itemsJson);"
  , "    c.innerHTML = '';"
  , "    el.__leksahOwners = items.map(function(it){ return it[1]; });"
  , "    items.forEach(function(it){"
  , "      var d = document.createElement('div'); var b = document.createElement('button');"
  , "      var ic = document.createElement('span');"
  , "      ic.className = 'flip-win-icon' + (it[1] < 0 ? ' shared' : '');"
  , "      if (it[1] >= 0) ic.style.backgroundColor = window.leksahWindowColor(it[1]);"
  , "      b.appendChild(ic);"
  , "      if (it[2]){ var ti = document.createElement('img');"
  -- tree-window icons carry a meaningful alert colour → exempt from the swap.
  , "        ti.className = 'flip-type-icon' + (it[2].indexOf('tree-window') >= 0 ? ' term-alert-icon' : '');"
  , "        ti.src = it[2]; b.appendChild(ti); }"
  , "      b.appendChild(document.createTextNode(it[0]));"
  , "      d.appendChild(b); c.appendChild(d);"
  , "    });"
  , "    el.__leksahKey = itemsJson;"
  , "  }"
  , "  var btns = c.querySelectorAll('button');"
  , "  for (var i=0;i<btns.length;i++) btns[i].className = (i===index ? 'selected' : '');"
  -- thick border when the highlighted item lives in THIS window
  , "  var owners = el.__leksahOwners || [];"
  , "  c.classList.toggle('self-selected', owners[index] === window.leksahWindowId);"
  , "  el.style.display = '';"
  , "};"
  , "window.leksahFlipMirrorHide = function(owner){"
  , "  var el = document.getElementById('leksah-flip-mirror');"
  , "  if (el) el.style.display = 'none';"
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
  -- The tmux pane id (data-pane) of the terminal pane that currently holds INPUT
  -- focus (document.activeElement), or "" when focus isn't in a terminal.  Used
  -- by the became-key handler to promote the focused pane after a window is
  -- brought forward — read from real DOM focus, so it never follows a background
  -- control-mode pane switch in another client.
  , "  window.leksahFocusedTermPane = function(){"
  , "    var ae = document.activeElement;"
  , "    var pn = ae && ae.closest && ae.closest('.terminal-cc-pane');"
  , "    return pn ? (pn.getAttribute('data-pane') || '') : '';"
  , "  };"
  , "  document.addEventListener('mousedown', function(e){"
  , "    if (!(e.target.closest)) return;"
  , "    if (e.target.closest('.terminal')) setTimeout(poke, 60);"
  -- A click inside a specific CC pane is a leksah-owned pane-focus: publish its
  -- tmux pane id so reflex can float exactly that pane to the flipper MRU (no
  -- tmux poll, so it can't be moved by another client, and no stale-tree race).
  , "    var pn = e.target.closest('.terminal-cc-pane');"
  , "    if (pn && window.leksahPaneFocus){ var pid = pn.getAttribute('data-pane');"
  , "      if (pid) window.leksahPaneFocus(pid); }"
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
-- | @window.LeksahTmux@: intercepts the tmux @C-b@ prefix inside each xterm
-- (attached via @LeksahTmux.attach(term)@ from the terminal widgets).  The state
-- machine and the swallow decision run entirely in JS — jsaddle dispatches
-- events to Haskell asynchronously, so a Haskell handler can neither reliably
-- @preventDefault@ nor return a value to xterm's synchronous
-- @attachCustomKeyEventHandler@.  When enabled (mirrored from the
-- @tmuxInterceptPrefix@ pref), @C-b@ arms; the next key is swallowed and its
-- token handed to @window.leksahTmuxKey@ (per-window callback): @w@ activates
-- the Terminals pane, everything else routes to 'dispatchTmuxPrefix'.
leksahTmuxJs :: Text
leksahTmuxJs = T.unlines
  [ "(function(){"
  , "  var S = window.LeksahTmux = window.LeksahTmux || {};"
  , "  S.enabled = false;"
  , "  function swallow(e){ e.preventDefault(); e.stopPropagation(); return false; }"
  , "  S.attach = function(term){"
  , "    if (!term || typeof term.attachCustomKeyEventHandler !== 'function') return;"
  , "    var armed = false;"
  , "    term.attachCustomKeyEventHandler(function(e){"
  , "      if (e.type !== 'keydown') return true;"
  , "      if (!S.enabled) { armed = false; return true; }"
  , "      var k = e.key;"
  , "      if (!armed) {"
  , "        if (e.ctrlKey && !e.altKey && !e.metaKey && (k === 'b' || k === 'B'))"
  , "          { armed = true; return swallow(e); }"
  , "        return true;"
  , "      }"
  , "      if (k === 'Control' || k === 'Shift' || k === 'Alt' || k === 'Meta') return swallow(e);"
  , "      armed = false;"
  , "      if (k === 'Escape') return swallow(e);"        -- C-b Esc: cancel the prefix
  , "      if (k.indexOf('Arrow') === 0) k = k.slice(5);" -- ArrowUp -> Up
  , "      var tok = (e.ctrlKey ? 'C-' : '') + (e.altKey ? 'M-' : '') + k;"
  , "      try { if (window.leksahTmuxKey) window.leksahTmuxKey(tok); } catch(_) {}"
  , "      return swallow(e);"
  , "    });"
  , "  };"
  , "})();"
  ]

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
  -- Hover tooltip for file links: a floating box that first shows the link
  -- text, then is filled in asynchronously by the LSP round-trip (see
  -- 'onHoverFile' below and IDE.LSP.requestTerminalHover).  shownRid tracks the
  -- in-flight request so a late reply only lands if that link is still hovered.
  , "  var tip = null, hoverSeq = 0, shownRid = -1;"
  -- Escape HTML, then apply a markdown-lite pass so an HLS hover blurb (code
  -- fences, `inline code`, **bold**, and a '---'/'***' rule between the type
  -- signature and the docs) renders as a styled card rather than raw markup.
  -- All text is escaped BEFORE any tag is introduced, and the only tags emitted
  -- are a fixed, attribute-free set, so server-supplied hover text can't inject.
  , "  function esc(s){ return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;'); }"
  , "  function inlineMd(s){"
  , "    return s.replace(/`([^`]+)`/g, function(_,c){ return '<code>'+c+'</code>'; })"
  , "            .replace(/\\*\\*([^*]+)\\*\\*/g, '<strong>$1</strong>')"
  , "            .replace(/__([^_]+)__/g, '<strong>$1</strong>');"
  , "  }"
  , "  function fmtTip(text){"
  , "    var lines = String(text).split('\\n'), out = [], code = [], inCode = false;"
  , "    for (var i=0;i<lines.length;i++){"
  , "      var ln = lines[i];"
  , "      if (/^\\s*```/.test(ln)){"
  , "        if (inCode){ out.push('<pre>'+esc(code.join('\\n'))+'</pre>'); code=[]; inCode=false; }"
  , "        else inCode = true;"
  , "        continue;"
  , "      }"
  , "      if (inCode){ code.push(ln); continue; }"
  , "      if (/^\\s*([-*_])(\\s*\\1){2,}\\s*$/.test(ln)){ out.push('<hr>'); continue; }"
  , "      if (/^\\s*$/.test(ln)){ out.push('<br>'); continue; }"
  , "      out.push(inlineMd(esc(ln))+'<br>');"
  , "    }"
  , "    if (inCode && code.length) out.push('<pre>'+esc(code.join('\\n'))+'</pre>');"
  -- Drop <br>s butting against a block (<pre>/<hr>) so they don't double the gap
  -- their own margins already give, and trim a trailing break.
  , "    return out.join('').replace(/<br>(<(?:pre|hr))/g,'$1').replace(/(<\\/pre>|<hr>)<br>/g,'$1').replace(/(<br>)+$/,'');"
  , "  }"
  , "  function ensureTip(){"
  , "    if (!tip){"
  , "      if (!document.getElementById('leksah-hovertip-style')){"
  , "        var st = document.createElement('style'); st.id = 'leksah-hovertip-style';"
  , "        st.textContent = '.leksah-term-hovertip code{font-family:Menlo,Monaco,\"Courier New\",monospace;background:rgba(255,255,255,0.09);border-radius:3px;padding:0 3px;font-size:11.5px;}'"
  , "          + '.leksah-term-hovertip pre{margin:4px 0;padding:5px 8px;background:rgba(255,255,255,0.06);border:1px solid rgba(255,255,255,0.10);border-radius:4px;font-family:Menlo,Monaco,\"Courier New\",monospace;font-size:11.5px;line-height:1.3;white-space:pre;overflow-x:hidden;}'"
  , "          + '.leksah-term-hovertip hr{border:none;border-top:1px solid rgba(255,255,255,0.16);margin:5px 0;}'"
  , "          + '.leksah-term-hovertip strong{color:#fff;font-weight:600;}';"
  , "        document.head.appendChild(st);"
  , "      }"
  , "      tip = document.createElement('div');"
  , "      tip.className = 'leksah-term-hovertip';"
  , "      tip.style.cssText = 'position:fixed;z-index:99999;pointer-events:none;'"
  , "        + 'background:rgb(37,37,38);color:#d4d4d4;border:1px solid rgb(70,70,72);'"
  , "        + 'border-radius:5px;padding:6px 9px;font-size:12px;max-width:72ch;max-height:60vh;'"
  , "        + 'white-space:normal;overflow:hidden;display:none;box-shadow:0 4px 14px rgba(0,0,0,0.45);'"
  , "        + 'font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif;line-height:1.4;';"
  , "      document.body.appendChild(tip);"
  , "    }"
  , "    return tip;"
  , "  }"
  , "  function showTip(ev, text){"
  , "    var t = ensureTip();"
  , "    t.innerHTML = fmtTip(text);"
  , "    t.style.left = (((ev && ev.clientX) || 0) + 12) + 'px';"
  , "    t.style.top  = (((ev && ev.clientY) || 0) + 16) + 'px';"
  , "    t.style.display = 'block';"
  , "  }"
  , "  function hideTip(){ if (tip) tip.style.display = 'none'; shownRid = -1; }"
  -- Called from Haskell when requestTerminalHover replies; ignore stale replies.
  , "  function resolveHover(rid, text){"
  , "    if (rid !== shownRid || !tip) return;"
  , "    if (text && text.length){ tip.innerHTML = fmtTip(text); } else { hideTip(); }"
  , "  }"
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
  -- git diff paths carry an a/ or b/ prefix (--- a/src/Foo.hs, +++ b/src/Foo.hs).
  , "    else if (tok.slice(0,2)==='a/' || tok.slice(0,2)==='b/') tok = tok.slice(2);"
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
  -- A rendered diff/content line under a Claude edit or a unified diff: an
  -- indent, a right-aligned source line number, then a fixed 4-column field
  -- (\"    \" for context, \" - \"/\" + \" + a space for -/+ lines) and then the
  -- verbatim source text.  Group 2 is the source line; the whole match's length
  -- is where the source text starts, so a hovered identifier's offset past it is
  -- its 0-based column in the file.
  , "  var GUT = /^(\\s+)(\\d+) ([-+ ])  /;"
  -- Headers that name the file a following diff belongs to: Claude tool headers
  -- Update/Edit/Write/Read(path), or a unified-diff '+++ b/path' line.
  , "  var HDR = /(?:Update|Edit|Write|Read)\\(([^)]+)\\)|^\\s*\\+\\+\\+ (?:b\\/)?(\\S+)/;"
  -- From a diff/content line, walk UP to the header naming its file and resolve
  -- it to a workspace path.  Give up on the first line that is neither a
  -- diff/content line nor blank/decoration, so we never wander past the block;
  -- bounded so a huge scrollback can't make hover expensive.
  , "  function governingFile(buf, y){"
  , "    for (var i=y-2, n=0; i>=0 && n<400; i--, n++){"
  , "      var ln = buf.getLine(i); if (!ln) break;"
  , "      var t = ln.translateToString(true);"
  , "      var h = HDR.exec(t);"
  , "      if (h) return resolve((h[1]||h[2]).trim());"
  , "      if (GUT.test(t) || /^\\s*$/.test(t) || /^\\s*[\\u23bf\\u25cf]/.test(t)) continue;"
  , "      break;"
  , "    }"
  , "    return null;"
  , "  }"
  , "  function attach(term, onOpen, onLookup, onHoverFile){"
  , "    if (!term || !term.registerLinkProvider) return;"
  , "    function mkLink(sx, ex, y, txt, f, l, c, hl, hc){"
  -- hc = 0-based column of the symbol to hover (default -1 = unknown, so the
  -- tooltip is the file's diagnostics summary only).
  , "      var hcol = (hc == null) ? -1 : hc;"
  , "      return {"
  , "        range: { start: { x: sx, y: y }, end: { x: ex, y: y } },"
  , "        text: txt,"
  , "        decorations: { pointerCursor: true, underline: true },"
  , "        activate: function(ev){ if (ev.preventDefault) ev.preventDefault(); onOpen(f,l,c); },"
  -- Hover: show the file link immediately, then ask Haskell (LSP diagnostics for
  -- the file, plus a symbol hover when hl>0) to fill the tooltip in.  hl is the
  -- symbol line for the LSP hover (0 = don't hover a symbol, file summary only).
  , "        hover: function(ev){ if (!onHoverFile) return; var rid = ++hoverSeq; shownRid = rid; showTip(ev, txt); onHoverFile(f, hl||0, hcol, rid); },"
  , "        leave: function(){ hideTip(); }"
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
  -- A diff CODE line (below an Update(...)/+++ header): make every identifier on
  -- it an LSP-hover target at its real (line, column) in the governing file.
  -- Clicking jumps there.  Gated on GUT + a header above, so ordinary output
  -- isn't turned into a field of links.
  , "        var g = GUT.exec(text);"
  , "        if (g) {"
  , "          var gov = governingFile(buf, y);"
  , "          if (gov) {"
  , "            var cs = g[0].length, srcLn = parseInt(g[2],10), code = text.slice(cs);"
  , "            ID.lastIndex = 0;"
  , "            var dm;"
  , "            while ((dm = ID.exec(code))){"
  , "              var ci = dm.index;"
  , "              links.push(mkLink(cs+ci+1, cs+ci+dm[0].length, y, dm[0], gov, srcLn, ci+1, srcLn, ci));"
  , "            }"
  , "            if (links.length){ cb(links); return; }"
  , "          }"
  , "        }"
  , "        UM.lastIndex = 0;"
  , "        var u;"
  , "        while ((u = UM.exec(text))){"
  , "          var uf = resolve(u[1].trim());"
  , "          if (!uf) continue;"
  , "          var target = 1;"
  , "          var peek = buf.getLine(y+1);"
  , "          if (peek){ var pm = /^\\s*(\\d+)/.exec(peek.translateToString(true)); if (pm) target = parseInt(pm[1],10)+3; }"
  , "          links.push(mkLink(u.index+1, u.index+u[0].length, y, u[0], uf, target, 1, 0));"
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
  , "          links.push(mkLink(m.index+1, m.index+m[0].length, y, m[0], abs, ln, col, m[2] ? parseInt(m[2],10) : 0, m[3] ? col-1 : -1));"
  , "        }"
  , "        cb(links.length ? links : undefined);"
  , "      } catch(e){ cb(undefined); }"
  , "    }});"
  , "  }"
  , "  return { setProjectFiles: setProjectFiles, attach: attach, setEnabled: setEnabled, resolveHover: resolveHover };"
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
  , "    if (img.classList && img.classList.contains('term-alert-icon')) return;"
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
-- it.  Each state has a distinct shape as well as colour (colour-blind
-- accessibility): green circle = safe; orange triangle (+ a beep) = the agent
-- needs it in ~3 s; red octagon = the agent is testing now; blue diamond = the
-- agent is rebuilding/restarting.  Driven from the shell via
-- @leksah-cmd js eval 'leksahTestStart()'@ (orange→beep→red after 3 s),
-- @'leksahTestEnd()'@ (back to green) and @'leksahRestarting()'@ (blue);
-- @leksahStatus('green'|'orange'|'red'|'blue')@ sets a state directly.
-- Default green (normal, un-driven use).
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
  , "        '#leksah-status-light{position:fixed;top:5px;right:10px;width:14px;height:14px;'+"
  , "        'z-index:2147483647;pointer-events:none;opacity:.95;'+"
  -- Own compositor layer so the glow never forces repaints of content beneath.
  , "        'transform:translateZ(0);transition:background .15s,filter .15s}'+"
  -- Each state has a distinct SHAPE as well as colour (colour-blind
  -- accessibility): green=circle safe, orange=triangle needed-soon,
  -- red=octagon (stop) testing, blue=diamond rebuilding/restarting.  The glow
  -- uses filter:drop-shadow (not box-shadow) so it follows the clipped shape.
  , "        '#leksah-status-light.green{background:#2ecc40;border-radius:50%;'+"
  , "        'filter:drop-shadow(0 0 1px rgba(0,0,0,.55)) drop-shadow(0 0 4px #2ecc40)}'+"
  , "        '#leksah-status-light.orange{background:#ff9500;'+"
  , "        'clip-path:polygon(50% 2%,98% 96%,2% 96%);'+"
  , "        'filter:drop-shadow(0 0 1px rgba(0,0,0,.55)) drop-shadow(0 0 4px #ff9500)}'+"
  , "        '#leksah-status-light.red{background:#ff3b30;'+"
  , "        'clip-path:polygon(30% 0,70% 0,100% 30%,100% 70%,70% 100%,30% 100%,0 70%,0 30%);'+"
  , "        'filter:drop-shadow(0 0 1px rgba(0,0,0,.55)) drop-shadow(0 0 5px #ff3b30)}'+"
  , "        '#leksah-status-light.blue{background:#0a84ff;'+"
  , "        'clip-path:polygon(50% 0,100% 50%,50% 100%,0 50%);'+"
  , "        'filter:drop-shadow(0 0 1px rgba(0,0,0,.55)) drop-shadow(0 0 5px #0a84ff)}';"
  , "      (document.head || document.documentElement).appendChild(css);"
  , "    }"
  , "    el = document.createElement('div');"
  , "    el.id = 'leksah-status-light';"
  , "    el.title = 'Green circle: safe to use \\u2022 Orange triangle: Claude needs it shortly \\u2022 Red octagon: Claude is testing \\u2022 Blue diamond: Claude is rebuilding/restarting';"
  , "    el.className = state;"
  , "    if (document.body) document.body.appendChild(el);"
  , "    return el;"
  , "  }"
  -- Beep via a NATIVE macOS system sound (the "leksahBeep" script message
  -- handler, see LeksahBeepHandler in leksah-mac-menu.m).  A system sound mixes
  -- with any audio already playing on the machine and never interrupts it —
  -- unlike a Web AudioContext, which grabbed the audio session and silenced
  -- other playback (why the in-page beep had to be disabled).  The handler
  -- exists only on the wkwebview front end; the try/catch no-ops elsewhere.
  , "  function beep(){ try { window.webkit.messageHandlers.leksahBeep.postMessage(1); } catch(e){} }"
  -- A terminal bell (Claude Code's "needs input" signal in a window you're not
  -- viewing): ring the ping, then — once it's had time to sound — SPEAK the
  -- belling terminal's location via the native "leksahSpeak" handler
  -- (NSSpeechSynthesizer, which mixes with other audio just like the ping).
  -- Called from the reflex bell handler with "window <name>, pane <n>".
  , "  window.leksahSpeak = function(t){ try { window.webkit.messageHandlers.leksahSpeak.postMessage(String(t)); } catch(e){} };"
  , "  window.leksahTermBell = function(t){ beep(); setTimeout(function(){ window.leksahSpeak(t); }, 700); };"
  , "  function set(s){ if (timer){ clearTimeout(timer); timer = null; } state = s; ensure().className = s; }"
  , "  window.leksahStatus = set;"
  , "  window.leksahTestStart = function(){"
  , "    if (timer) clearTimeout(timer);"
  , "    state = 'orange'; ensure().className = 'orange'; beep();"
  , "    timer = setTimeout(function(){ state = 'red'; ensure().className = 'red'; timer = null; }, 3000);"
  , "  };"
  , "  window.leksahTestEnd = function(){ set('green'); };"
  -- Blue (diamond): Claude is rebuilding/restarting leksah — informational,
  -- distinct from red (an active test).  Set before rebuild-self/restart.
  , "  window.leksahRestarting = function(){ set('blue'); };"
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
  -> WindowId         -- ^ which OS window this reflex network drives
  -> Dynamic t IDE
  -> m (Event t IDEAction)
main showMenubar macTitlebar wid ide = mdo
  let widN = case wid of WindowId n -> n   -- this window's id as an Int (for JS)
  -- This OS window's slice of the shared per-window state (used here for the root
  -- CSS classes and the toolbar's visibility indicators, and inside the inner
  -- widget for wide0 ownership).  Defined in main's outer scope so rootAttrD —
  -- which wraps the inner widget — can read it.
  myWinD <- holdUniqDyn (M.findWithDefault emptyWebWindow wid . _webWindows <$> ide)
  -- All windows' state (shared) — the global flipper reads other windows' wide0
  -- from here, and the session writer serialises every window from it.
  webWindowsD <- holdUniqDyn (_webWindows <$> ide)
  tallVisD  <- holdUniqDyn (_wwTall  <$> myWinD)
  wide1VisD <- holdUniqDyn (_wwWide1 <$> myWinD)
  let menuClass = if showMenubar then "" else " no-menubar"
      titlebarClass = if macTitlebar then " mac-titlebar" else ""
      tallClass TallShow     = ""
      tallClass TallAutoHide = " tall-auto"
      tallClass TallHide     = " tall-hide"
      wide1Class TallShow     = ""
      wide1Class TallAutoHide = " wide1-auto"
      wide1Class TallHide     = " wide1-hide"
      -- Side/bottom pane visibility is now per-OS-window (from this window's
      -- WebWindow), not the global prefs.
      rootAttrD = (\tv wv ->
          "class" =: ("leksah" <> menuClass <> titlebarClass
                      <> tallClass tv <> wide1Class wv)
            <> "tabindex" =: "0") <$> tallVisD <*> wide1VisD
  (top, topEvents) <- elDynAttr' "div" rootAttrD $ mdo
    keymapE <- keymapWidget top
    -- The web menu bar is suppressed when a native menu is present
    -- (leksah-wkwebview); its command events then simply never fire.
    menubarE   <- if showMenubar then menubarWidget else return never
    toolbarE   <- toolbarWidget ide tallVisD wide1VisD
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
      tabButton :: Text -> Text -> TabKey -> Dynamic t Bool -> Dynamic t (Map Text Text)
                -> Dynamic t (Maybe Int)
                -> Maybe Text -> Maybe Text -> m () -> IO ()
                -> m (Event t (Map Text TabKey, [TabKey]))
      tabButton area flipKey k selectedD orderStyleD badgeD mbTitle mbCloseTip labelW onSel =
        elDynAttr "span"
            -- data-flipkey lets the ⌘` flip-target hint (hintsJs) find this button.
            ((\sel ost -> "class" =: ("tab-wrap" <> if sel then " selected" else "")
                          <> "data-flipkey" =: flipKey <> ost)
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
              ((\mb -> "class" =: maybe "" (const "leksah-shortcut-badge") mb) <$> badgeD) $ do
              dynText ((\mb -> maybe "" (\n -> "\8984" <> T.pack (show n)) mb) <$> badgeD)
              -- Hidden ⌘` suffix, revealed by hintsJs when this button is the
              -- flip target — so a numbered tab reads "⌘N ⌘`" as one hint.
              elAttr "span" ("class" =: "leksah-flip-suffix") $ text " \8984`"
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
                labelTextD = (\names mw -> case mw of
                                Nothing -> M.findWithDefault s s names
                                Just w  -> stripIdxPrefix (twIndex w) (twLabel w))
                             <$> terminalNamesD <*> mwD
                -- The leading state icon carries the notification (bell / activity
                -- / silence) instead of a trailing 🔔/●/○; a leksah-tracked bell
                -- (viewed-window bell tmux's hook skips) forces the bell icon too.
                iconSrcD  = (\mw att -> case mw of
                              Nothing | s `S.member` att -> "/pics/tree-window-bell.svg"
                                      | otherwise        -> "/pics/tree-window-idle.svg"
                              Just w  | s `S.member` att && twActive w -> "/pics/tree-window-bell.svg"
                                      | otherwise                      -> windowAlertSrc w)
                            <$> mwD <*> attentionD
                labelW = do
                    void $ elDynAttr' "img"
                        ((\src -> "class" =: "tab-icon term-alert-icon" <> "src" =: src) <$> iconSrcD)
                        (pure ())
                    dynText labelTextD
                orderStyleD = buttonOrderStyleD (Left (s, widx)) baseOrderD
                -- Switch the shared terminal to this window, then poke a pane-tree
                -- refresh so the "current window" highlight updates at once (not on
                -- the next 2 s poll).
                onSel | widx < 0  = pure ()
                      | otherwise = case remoteTabHostTarget s of
                          Just (host, target) -> void . forkIO $ do
                              selectRemoteTmuxWindow host target widx
                              fireRemotePoke ()
                          -- Clicking a terminal-window button in the tab bar: we
                          -- already know leksah's MRU pane for this (session,
                          -- window), so promote it and switch straight to it (the
                          -- handler below) — no waiting to see which pane wins
                          -- focus.  fireTermActivity refreshes the tree/highlight.
                          Nothing -> fireTermWinSel (s, widx) >> fireTermActivity ()
                badgeD = M.lookup (Left (s, widx)) <$> badgeNumsD
            tabButton area (winFlipKey s widx) k selectedD orderStyleD badgeD Nothing Nothing labelW onSel
          pure (mconcat . M.elems <$> winButtonsE)
        _ ->
          let mbTitle    = case k of EditorKey f -> Just (T.pack f); _ -> Nothing
              -- No close × on tab buttons: close via ⌘W / File ▸ Close instead.
              mbCloseTip = Nothing
              orderStyleD = buttonOrderStyleD (Right k) baseOrderD
              -- Side-pane tree tabs carry a leading B&W icon before their label.
              labelW = do
                mapM_ (\s -> elAttr "img" ("class" =: "tab-icon" <> "src" =: s) (pure ()))
                      (tabIconSrc k)
                dynText (tabLabelText k <$> terminalTabLabelsD)
              badgeD = M.lookup (Right k) <$> badgeNumsD
          in tabButton area (tabFlipKey k) k isVisibleD orderStyleD badgeD mbTitle mbCloseTip labelW (pure ())

    -- File ▸ Save / the Save toolbar button: a background thread turns native-
    -- menu save requests into a reflex event; the in-page toolbar/menubar Save
    -- command routes to the same event (without the round-trip).  We save
    -- whichever editor is the active pane (a non-editor active pane saves nothing).
    (saveBridgeE, fireSaveReq) <- newTriggerEvent
    let inPageSaveE = fmapMaybe (\case CommandFileSave -> Just (); _ -> Nothing) panelCmdE
        saveReqE    = leftmost [saveBridgeE, inPageSaveE]
        saveFileE   = fmapMaybe (\case Just (EditorKey f) -> Just f; _ -> Nothing)
                        (tag (current activePaneD) saveReqE)
    (openFileE, openExternalE, lspRefsE, makeEditor) <- editorWidget ide allE saveFileE
    -- File ▸ Open (the native NSOpenPanel on wkwebview) delivers chosen files via
    -- a background thread; open each one in the editor area like any other file.
    (nativeOpenedFileE, fireOpenedFile) <- newTriggerEvent
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
    -- Flip commands from the keymap: a step (True = forward/⌘`, False = back/⌘⇧`)
    -- and the commit (Command released).
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
    -- On the *first* press (flipper hidden) poll the tree so the pane list/labels
    -- are current when the flipper opens (a pane created/renamed elsewhere shows
    -- up).  Membership only — the MRU order is our own and is left untouched here.
    openPollE <- performEvent $ ffor openStepE $ \dir -> do
        tree <- liftIO listTerminalTree
        pure (dir, tree)
    -- Only open once the poll is in and the list has reordered (one frame later —
    -- opening on the same frame the list changes crashes selectViewListWithKey).
    openStepDelayedE <- performEvent $ ffor openPollE $ \(dir, _) -> pure dir
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
      (leftmost [ otherPollE, snd <$> openPollE, fst <$> newTermPolledE ])
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
    -- The MRU is built ENTIRELY from leksah-originated interactions — never from
    -- reading "which pane tmux thinks is active" (that follows any client on the
    -- session, and races the tree poll).  The tmux tree is used only to LIST
    -- panes/labels and to TRANSLATE a pane leksah already identified into indices.
    --
    -- Terminal recency comes solely from precise pane signals that carry an exact
    -- pane id: a pane click (jsPaneFocusE, from the global mousedown listener), a
    -- ⌘-number split select and a flipper pane commit (both via TerminalPaneFocused
    -- / flipSelE), and terminal creation (newTermPolledE).  A bare tab-activation
    -- (activePaneD) floats only NON-terminal tabs — a terminal already sits in the
    -- MRU from whichever pane signal last touched it, so there is no stale-tree
    -- resolution to double-bump the wrong pane.
    let notTermKey (TerminalKey _) = False
        notTermKey _               = True
        -- A non-terminal tab gaining focus/click (side bar, bottom bar, editor).
        tabFlipE = fmapMaybe (\mk -> case mk of
                     Just k | notTermKey k -> Just (FlipTab k)
                     _                     -> Nothing) (updated activePaneD)
        -- A leksah-issued pane focus bubbled up from a CC tab (⌘-number split).
        ccPaneFocusE = fmapMaybe (\m -> listToMaybe
                     [ p | (TerminalKey _, dm) <- M.toList m
                         , Just (Identity (TerminalPaneFocused p)) <- [DM.lookup TerminalTab dm] ]) tabE
        -- Translate a clicked/selected pane id to its FlipPane (pure tree lookup).
        paneFocusFlipE = fmapMaybe id $ attachWith (\tree pid -> flipForPaneId pid tree)
                     (current allTreeD) (leftmost [ jsPaneFocusE, ccPaneFocusE ])
        -- A tab focused programmatically (editor opened by workspace double-click
        -- or terminal link) arrives as a focusin.  Float NON-terminal tabs only:
        -- a terminal's programmatic focus also fires on a control-mode follow of
        -- another client, so it is not a trustworthy pane-recency signal — real
        -- terminal focus comes through paneFocusFlipE instead.
        focusFlipE = fmapMaybe id $ attachWith
          (\rt str -> case [ k | (_, k) <- rt, T.pack (show k) == str ] of
             (TerminalKey _ : _) -> Nothing
             (k:_)               -> Just (FlipTab k)
             []                  -> Nothing)
          (current recentTabs) focusTabE
        -- Opening a file to navigate to it floats that editor to the MRU front.
        openEditorFlipE = fmapMaybe (fmap FlipTab . listToMaybe . M.keys) openFileE'
        -- Opening Preferences (⌘, / menu) needs an explicit bump (it takes no focus).
        openPrefsFlipE = FlipTab PreferencesKey <$ showPrefsE
        -- Every leksah-originated promotion from THIS window (all but the
        -- OS-window-became-key bump, which reads the log held below).
        localFlipBumpE = leftmost [ snd <$> flipSelE
                                  , snd <$> newTermPolledE
                                  , paneFocusFlipE
                                  , termWinFlipE
                                  , tabFlipE
                                  , focusFlipE
                                  , openEditorFlipE
                                  , openPrefsFlipE ]
    -- The MRU: THE shared flip order — one '_flipMru' list in the IDE record used
    -- by every OS window (single source of truth, no per-window copies).  This
    -- window WRITES its promotions via modifyIDE_ (flipBumpE, in the ideAction
    -- chain below) and READS the order back through its polled ideD, so all
    -- windows always show the same list.
    let becameKeyE = void (ffilter id (updated isActiveD))
        -- Became key (window brought forward by mouse OR by a cross-window flip
        -- raise): promote what the user is actually focused on HERE — never a
        -- stale remembered item, which would shove a just-flipped-to pane down to
        -- #2 (the raise re-promoting the window's previous active pane).
        --
        -- Editors (and other non-terminal wide0 tabs): the shown wide0 tab, at
        -- once.  Terminals: NOT here — a terminal's focused pane is read from the
        -- DOM after a 0.5 s settle (becameKeyPaneFlipE below), so a cross-window
        -- flip's now-focused target wins and an external control-mode switch (no
        -- input focus here) never promotes.
        becameKeyEditorE = fmapMaybe (\wa -> case wa of
                Just (TerminalKey _) -> Nothing
                Just k               -> Just (FlipTab k)
                Nothing              -> Nothing)
              (tag (current wide0ActiveD) becameKeyE)
    -- Terminal side of became-key: 0.5 s after the window is brought forward,
    -- read the pane that STILL holds input focus and promote it.  This is what
    -- floats the active pane of a raised terminal window to the top (mouse
    -- activation), and re-affirms a cross-window flip's target pane.
    becameKeyDelayedE <- delay 0.5 becameKeyE
    becameKeyPaneIdE <- performEvent $ ffor becameKeyDelayedE $ \_ -> do
        wlog wid "ENTER becameKeyPaneId (liftJSM leksahFocusedTermPane)"
        r <- liftJSM $
            valToText =<< jsg ("window" :: Text) ^. js0 ("leksahFocusedTermPane" :: Text)
        wlog wid "EXIT becameKeyPaneId"
        return r
    let becameKeyPaneFlipE = fmapMaybe id $ attachWith
          (\tree pid -> if T.null pid then Nothing else flipForPaneId pid tree)
          (current allTreeD) becameKeyPaneIdE
        flipBumpE = leftmost [ localFlipBumpE, becameKeyEditorE, becameKeyPaneFlipE ]
    flipMruD <- holdUniqDyn (_flipMru <$> ide)
    -- Terminal-window button clicked (from 'fireTermWinSel'): look up leksah's
    -- MRU pane for that (session, window) in the shared flip list, switch tmux
    -- straight to it, and promote it — all at once, no focus round-trip.  Falls
    -- back to plain select-window (tmux picks the pane) when we have no recorded
    -- pane for that window yet.
    termWinFlipE <- fmap (fmapMaybe id) . performEvent $
        ffor (attach (current flipMruD) termWinSelE) $ \(mru, (s, widx)) -> do
            wlog wid ("ENTER termWinFlip " <> show (s, widx))
            r <- liftIO $
                case listToMaybe [ p | FlipPane s' w p <- mru, s' == s, w == widx ] of
                    Just p  -> selectTmuxPane s widx p >> return (Just (FlipPane s widx p))
                    Nothing -> selectTmuxWindow s widx >> return Nothing
            wlog wid "EXIT termWinFlip"
            return r
    performEvent_ $ ffor flipBumpE $ \fi -> wlog wid ("flipBump " <> show fi)
    performEvent_ $ ffor (updated flipMruD) $ \mru -> wlog wid ("flipMru<-shared front=" <> show (take 3 mru))
    -- The flip list, kept populated and updated ONLY while the flipper is hidden
    -- (frozen during a flip) and only on genuine changes (holdUniqDyn).  This
    -- mirrors the old tab MRU, which never changed the list under the flipper —
    -- changing it on the open event, or churning it every tmux poll, crashes the
    -- flipper's selectViewListWithKey ("Same key fired multiple times for Merge").
    -- Other OS windows' wide0 tabs (editors) — added to the flip list so the
    -- flipper spans all windows; terminals are excluded (already global via the
    -- pane tree).  Selecting one raises its owning window (the ownership split
    -- below), rather than stealing it into this window.
    otherTabsD <- holdUniqDyn
      ((\wins -> [ k | (w, ww) <- M.toList wins, w /= wid, k <- _wwWide0 ww ]) <$> webWindowsD)
    flipLiveD <- holdUniqDyn (buildFlipItems <$> flipMruD <*> recentTabs <*> otherTabsD <*> allTreeD)
    -- The list the flipper shows.  It updates freely while hidden (labels, tabs),
    -- but the authoritative refresh is a *snapshot taken on open* (openListE),
    -- built from the current shared MRU and the freshly-read tree.  We trust our
    -- own MRU order as-is: no float of "tmux's active pane" (that recency call is
    -- the ambiguous read this design removes).  Relying on @updated flipLiveD@
    -- alone left it stale — after a flip, holdUniqDyn suppresses the (already-
    -- front) re-bump, so the flipper reopened with the pre-flip order.  The
    -- snapshot fires one frame before the flipper actually opens.
    let openListE = attachWith
          (\(mru, rt, rtree, other) (_, tree) ->
             buildFlipItems mru rt other (M.union tree rtree))
          ((,,,) <$> current flipMruD <*> current recentTabs <*> current remoteFlipD
                 <*> current otherTabsD)
          openPollE
    flipItemsD <- holdDyn [] (leftmost
          [ openListE
          , gate (current (not <$> flipperVisibleD)) (updated flipLiveD) ])
    let flipItemLabel names tree fi = case fi of
          FlipTab k      -> tabLabelText k names
          FlipPane n w p -> flipPaneLabel n w p tree
        -- A small square before each entry, coloured by the OS window that owns
        -- the pane — a visual cue of where selecting it will take you.  Only when
        -- there is more than one window (a single window needs no disambiguation).
        flipIconAttr :: Int -> Map WindowId WebWindow -> FlipItem -> Map Text Text
        flipIconAttr cnt wins fi
          | cnt <= 1 = "style" =: "display:none"
          | otherwise = case flipOwnerWindow wins fi of
              Just w  -> "class" =: "flip-win-icon"
                      <> "style" =: ("background-color:" <> windowColorCss w)
              Nothing -> "class" =: "flip-win-icon shared"
        -- The type icon (file icon for editors, tmux-window icon for terminals /
        -- their panes) between the owner-window square and the label.  A terminal
        -- window icon is colour-meaningful (its alert state), so it carries
        -- 'term-alert-icon' to stay exempt from the mono/colour icon swap; a file
        -- icon is a plain B&W glyph and swaps like the editor-tab icons.  Hidden
        -- when the entry has no icon.
        flipTypeClass :: FlipItem -> Text
        flipTypeClass fi = case fi of
          FlipPane {}             -> "flip-type-icon term-alert-icon"
          FlipTab (TerminalKey _) -> "flip-type-icon term-alert-icon"
          _                       -> "flip-type-icon"
        flipTypeIconAttr :: Map Text (Text, [TmuxWindow]) -> FlipItem -> Map Text Text
        flipTypeIconAttr tree fi = case flipIconSrc tree fi of
          Just src -> "class" =: flipTypeClass fi <> "src" =: src
          Nothing  -> "style" =: "display:none"
        flipLabel fiD = do
          elDynAttr "span" (flipIconAttr <$> winCountD <*> webWindowsD <*> fiD) (pure ())
          elDynAttr "img" (flipTypeIconAttr <$> allTreeD <*> fiD) (pure ())
          dynText $ flipItemLabel <$> terminalNamesD <*> allTreeD <*> fiD
    winCountD <- holdUniqDyn (M.size <$> webWindowsD)
    (flipperVisibleD, flipperSelD, flipSelIndexD, flipRawE) <- flipperWidget flipItemsD flipStepE rawFlipDoneE selfSelectedD flipLabel
    -- Thicken THIS window's flipper border when the highlighted item lives here.
    selfSelectedD <- holdUniqDyn $
      (\wins msel -> case msel of
          Just (_, fi) -> flipOwnerWindow wins fi == Just wid
          Nothing      -> False)
      <$> webWindowsD <*> flipperSelD
    -- Global flipper mirror: show this window's open flipper on every OTHER OS
    -- window too.  We drive it with a DIRECT JS broadcast (ideJSM_ →
    -- leksahFlipMirror in every context, see flipMirrorJs) on open / step / close,
    -- NOT shared reflex state: a per-keystroke burst of modifyIDE_ fan-out drops
    -- and reorders the cross-window trigger fires to background webviews (which
    -- left the mirror stuck open), whereas ordered evaluateJavaScript always lands
    -- the final hide.  Only the active window's flipper is ever up, so only it
    -- broadcasts; leksahFlipMirror skips the owning window by id.
    -- Each mirror entry carries its owner window id (-1 = shared) so the mirror
    -- can draw the owner-coloured icon and thicken the border on the owning window.
    flipMirrorItemsD <- holdUniqDyn $
      (\items wins names tree ->
         [ ( flipItemLabel names tree fi
           , maybe (-1) (\(WindowId n) -> n) (flipOwnerWindow wins fi)
           , fromMaybe "" (flipIconSrc tree fi) )
         | (_, fi) <- items ])
      <$> flipItemsD <*> webWindowsD <*> terminalNamesD <*> allTreeD
    flipMirrorStateD <- holdUniqDyn $
      (,,) <$> flipperVisibleD <*> flipMirrorItemsD <*> flipSelIndexD
    let flipMirrorShowE = fmapMaybe (\(v, is, i) -> if v then Just (is, i) else Nothing)
                                    (updated flipMirrorStateD)
        flipMirrorHideE = fmapMaybe (\(v, _, _) -> if v then Nothing else Just ())
                                    (updated flipMirrorStateD)
    performEvent_ $ ffor (updated flipperVisibleD) $ \v -> wlog wid ("flipper visible=" <> show v)
    performEvent_ $ ffor (updated flipSelIndexD) $ \i -> wlog wid ("flipper selIndex=" <> show i)
    performEvent_ $ ffor flipMirrorShowE $ \(is, i) -> wlog wid ("flipMirror write show idx=" <> show i <> " nItems=" <> show (length is) <> " (shared state)")
    performEvent_ $ ffor flipMirrorHideE $ \() -> wlog wid "flipMirror write hide (shared state)"
    -- Draw the flipper mirror in THIS window from the SHARED '_flipMirror' state
    -- (which fans out to every window via the MVar poll), evaluating ONLY in our
    -- OWN jsaddle context.  Never broadcast JS across windows: 'ideJSM_' into every
    -- WKWebView deadlocks jsaddle-wkwebview's synchronous main-thread bridge once a
    -- second window exists.  'leksahFlipMirror' skips the owning window (owner == our
    -- id), so the acting window keeps showing its real reflex flipper; the poll
    -- gives background windows their mirror within one tick (≤0.5s), and because we
    -- render the LATEST shared state (not replayed steps) the hide always lands — no
    -- stuck mirror even under a rapid ⌘` burst.
    flipMirrorSharedD <- holdUniqDyn (_flipMirror <$> ide)
    performEvent_ $ ffor (updated flipMirrorSharedD) $ \m -> do
        wlog wid ("ENTER flipMirrorRead " <> maybe "hide" (const "show") m)
        case m of
          Just (owner, is, i) ->
            liftJSM . void $ jsg ("window" :: Text)
              ^. js3 ("leksahFlipMirror" :: Text)
                   owner (decodeUtf8 (BS.toStrict (encode is))) i
          Nothing ->
            liftJSM . void $ jsg ("window" :: Text)
              ^. js1 ("leksahFlipMirrorHide" :: Text) widN
        wlog wid "EXIT flipMirrorRead"
    -- Split the flipper selection: a tab selects as before; a pane brings its
    -- terminal up in wide0 (below) and makes that tmux pane active.
    -- A ⌘-number on a non-active wide0 button feeds a synthetic flip selection
    -- (always wide0), so it navigates through the same path as the flipper.
    (numFlipE, fireNumFlip) <- newTriggerEvent
    let flipSelE  = leftmost [ fmapMaybe (listToMaybe . M.toList) flipRawE
                             , (\fi -> ("wide0", fi)) <$> numFlipE ]
        -- The global flipper: classify each selection by the OS window that owns
        -- the tab (from the shared per-window state).  A wide0 item owned by
        -- ANOTHER window is a cross-window select — raise that window and make
        -- the item active THERE (never move it here); everything else (this
        -- window's tabs, shared side/bottom tabs, panes not yet open anywhere)
        -- follows the in-place path below.
        flipKeyOf (FlipTab k)      = Just k
        flipKeyOf (FlipPane s _ _) = Just (TerminalKey s)
        classifyFlip wins (a, fi) =
          case flipKeyOf fi >>= \k ->
                 listToMaybe [ w | (w, ww) <- M.toList wins, w /= wid, k `elem` _wwWide0 ww ] of
            Just w  -> Left (w, fi)
            Nothing -> Right (a, fi)
        classifiedFlipE = attachWith classifyFlip (current webWindowsD) flipSelE
        crossFlipE = fmapMaybe (either Just (const Nothing)) classifiedFlipE
        localFlipE = fmapMaybe (either (const Nothing) Just) classifiedFlipE
        flipTabE  = fmapMaybe (\(a, fi) -> case fi of FlipTab k -> Just (M.singleton a k); _ -> Nothing) localFlipE
        flipPaneE = fmapMaybe (\(_, fi) -> case fi of FlipPane s w p -> Just (s, w, p); _ -> Nothing) localFlipE
    performEvent_ $ ffor flipPaneE $ \(s, w, p) -> do
        wlog wid ("ENTER flipPaneE selectTmuxPane " <> show (s, w, p))
        liftIO $ case remoteTabHostTarget s of
          -- remote pane: select over ssh (off the reflex thread); the tab's
          -- control client hears %session-window-changed and re-renders
          Just (host, target) -> void . forkIO $ do
              selectRemoteTmuxPane host target w p
              fireRemotePoke ()
          Nothing -> selectTmuxPane s w p
        wlog wid "EXIT flipPaneE"
    -- Cross-window flip: raise the owning OS window and make the selected tab
    -- active there; for a terminal pane, also switch tmux to that pane.  The tab
    -- stays where it is (no move) — the flipper only navigates.
    performEvent_ $ ffor crossFlipE $ \(WindowId n, fi) -> do
        wlog wid ("crossFlip commit -> raise window " <> show n)
        liftIO $ requestRaiseWindow n
        liftIO $ case fi of
          FlipPane s w p -> case remoteTabHostTarget s of
            Just (host, target) -> void . forkIO $ selectRemoteTmuxPane host target w p >> fireRemotePoke ()
            Nothing             -> selectTmuxPane s w p
          _ -> return ()
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
    performEvent_ $ ffor selectWinE  $ \(s, w)    -> do
        wlog wid ("ENTER selectWinE selectTmuxWindow " <> show (s, w))
        liftIO (selectTmuxWindow s w)
        wlog wid "EXIT selectWinE"
    performEvent_ $ ffor selectPaneE $ \(s, w, p) -> do
        wlog wid ("ENTER selectPaneE selectTmuxPane " <> show (s, w, p))
        liftIO (selectTmuxPane s w p)
        wlog wid "EXIT selectPaneE"
    -- A selection made in an auto-hidden side/bottom pane that brings a file or
    -- terminal up in the editor area snaps that pane shut again — even with the
    -- cursor still over it (leksahCollapseAutoHide overrides the hover reveal).
    -- Fires on file opens (workspace tree, Changes, Errors/Grep/Metadata jumps —
    -- everything 'openFileE' carries) and on any terminal-activating selection
    -- (local or remote); management actions
    -- (rename/kill/zoom) deliberately don't, so you stay in the pane.
    let collapseAutoHideE = leftmost
          [ () <$ openFileE
          , () <$ selectAnyTermE, () <$ newTermClickE
          , () <$ selRemoteE, () <$ selRemoteWinE, () <$ selRemotePaneE
          , () <$ selRemoteHostE, () <$ newRemoteE ]
    performEvent_ $ ffor collapseAutoHideE $ \_ ->
        liftJSM . void $ jsg ("window" :: Text) ^. js0 ("leksahCollapseAutoHide" :: Text)
    performEvent_ $ ffor selRemoteWinE  $ \(h, s, _, w) ->
        liftIO . void . forkIO $ selectRemoteTmuxWindow h s w
    performEvent_ $ ffor selRemotePaneE $ \(h, s, _, w, p) ->
        liftIO . void . forkIO $ selectRemoteTmuxPane h s w p
    -- Remote management actions bubbled from the Terminals tree (the analogues
    -- of the local session/window/pane controls): run the tmux command over ssh
    -- off the reflex thread, then poke the shared host poll so the row updates at
    -- once rather than on the next 10s tick.  Killing a whole session also drops
    -- its tab (closeTabsE below).
    let selRemoteNewWinE = fmapMaybe (^? _NewRemoteTerminalWindow)    terminalsListE
        killRemoteSessE  = fmapMaybe (^? _CloseRemoteTerminal)        terminalsListE
        killRemoteWinE   = fmapMaybe (^? _KillRemoteTerminalWindow)   terminalsListE
        killRemotePaneE  = fmapMaybe (^? _KillRemoteTerminalPane)     terminalsListE
        zoomRemotePaneE  = fmapMaybe (^? _ZoomRemoteTerminalPane)     terminalsListE
        breakRemotePaneE = fmapMaybe (^? _BreakRemoteTerminalPane)    terminalsListE
        renRemoteSessE   = fmapMaybe (^? _RenameRemoteTerminalSession) terminalsListE
        renRemoteWinE    = fmapMaybe (^? _RenameRemoteTerminalWindow) terminalsListE
        remoteAct io     = liftIO . void . forkIO $ io >> fireRemotePoke ()
    performEvent_ $ ffor selRemoteNewWinE $ \(h, s)        -> remoteAct (newRemoteTmuxWindow h s)
    performEvent_ $ ffor killRemoteSessE  $ \(h, s, _)     -> remoteAct (killRemoteTmuxSession h s)
    performEvent_ $ ffor killRemoteWinE   $ \(h, s, w)     -> remoteAct (killRemoteTmuxWindow h s w)
    performEvent_ $ ffor killRemotePaneE  $ \(h, s, w, p)  -> remoteAct (killRemoteTmuxPane h s w p)
    performEvent_ $ ffor zoomRemotePaneE  $ \(h, s, w, p)  -> remoteAct (zoomRemoteTmuxPane h s w p)
    performEvent_ $ ffor breakRemotePaneE $ \(h, s, w, p)  -> remoteAct (breakRemoteTmuxPane h s w p)
    performEvent_ $ ffor renRemoteSessE   $ \(h, s, nm)    -> remoteAct (renameRemoteTmuxSession h s nm)
    performEvent_ $ ffor renRemoteWinE    $ \(h, s, w, nm) -> remoteAct (renameRemoteTmuxWindow h s w nm)
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
    -- Only the original window (WindowId 0) restores the saved session; a New
    -- Window (WindowId 1+) starts empty.  Otherwise every new network's
    -- PostBuild would re-run the restore and 'moveTabTo' every saved editor into
    -- the just-created window.  (Step 8 generalises this to per-window restore.)
    -- v4 multi-window restore: this window's wide0 tabs + its side/bottom
    -- visibility are SEEDED into the shared per-window state ('_webWindows') by
    -- 'newIDE' before any network attaches, so the tab grid renders them
    -- directly (editors instantiate via 'makeEditor' on render) — there is no
    -- reopen event for wide0.  Reading the session here only supplies the bits
    -- that aren't per-window shared state: openFileKeysD seeding, the shared
    -- side/bottom visible tab, the recent-files menu, and the tmux id list.
    restoreE <- performEvent $ ffor restorePb $ \_ -> do
                  wlog wid "ENTER restore (readWebSession + listTerminalSessions)"
                  r <- liftIO $ (,) <$> readWebSession <*> listTerminalSessions
                  wlog wid "EXIT restore"
                  return r
    let existingIdsE = snd <$> restoreE
        -- Editor keys of THIS window's seeded wide0 seed openFileKeysD so the
        -- editor event routing knows about the restored files (they don't flow
        -- through openFileE').
        restoreFileKeysE = fmapMaybe
          (\ww -> case [ EditorKey f | EditorKey f <- _wwWide0 ww ] of
                    [] -> Nothing
                    ks -> Just (S.fromList ks))
          (tag (current myWinD) restorePb)
        -- Which shared side/bottom tab is visible (the wide0 shown tab is the
        -- per-window '_wwActive', seeded separately).
        restoreVisibleE = fmapMaybe
          (\(ms, _) -> case ms of
             Just s | not (null (wsVisible s)) -> Just (M.fromList (wsVisible s))
             _ -> Nothing)
          restoreE
        -- The saved recent-files list (for the Open Recent menu).
        restoreRecentFilesE = fmapMaybe (\(ms, _) -> ms >>= wsRecentFiles) restoreE
        -- The flipper MRU seeds from this window's wide0 order (Step 6 makes the
        -- flipper global); no separate saved-order event any more.
        setRecentE = never
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
    -- Clicking a row in the Terminals tree (a session/window/pane, local or
    -- remote) must hand the keyboard to the terminal too, not just bring its tab
    -- up.  The tab-selected path focuses only when the *shown* tab actually
    -- changes, so clicking the already-shown session — or another window/pane
    -- within it — switches tmux but leaves the keyboard on the tree.  Fire the
    -- terminal's explicit focus request (past the "did we already own the
    -- keyboard" gate, exactly like a workspace repl launch), now and again after
    -- the tab has had time to mount / the tmux select-window/pane to propagate.
    -- Retry across a few delays: a network (ssh) terminal opened fresh only
    -- registers its focus callback once its control client is up, which can lag
    -- the click by a moment — so a single immediate call would find nothing
    -- registered and no-op.  One call landing after registration is enough; it
    -- arms the terminal's own focus retry (which then waits for the panes to
    -- actually show up — see 'forcedFocusE' in TerminalCC).  Re-arming an
    -- already-focused terminal is harmless (its retry re-checks and clears).
    let treeSelectKeyE = leftmost [ selectAnyTermE, remoteOpenKeyE ]
    treeFocusEs <- forM [0.3, 0.8, 1.6] $ \d -> delay d treeSelectKeyE
    performEvent_ $ ffor (leftmost (treeSelectKeyE : treeFocusEs)) $
        liftIO . focusTerminalPane
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
    -- Only raise attention for a bell you weren't already looking at.  You ARE
    -- looking at it only when it's the active tab AND this OS window currently
    -- has focus — so a bell while leksah is unfocused / in the background (the
    -- usual Claude Code "needs input" case: you've switched to another app, the
    -- terminal is still the active tab) DOES ping.  Reads document.hasFocus() per
    -- bell, hence performEvent rather than a pure gate.
    bellAwayRawE <- performEvent $ ffor (attach (current activePaneD) bellSessE) $ \(active, ns) -> do
        foc <- liftJSM $ valToBool =<< eval ("document.hasFocus()" :: Text)
        let away = filter (\n -> not (foc && active == Just (TerminalKey n))) ns
        wlog wid ("bell caught=" <> show ns <> " active=" <> show active
                  <> " focus=" <> show foc <> " ping=" <> show away)
        return away
    let bellAwayE = ffilter (not . null) bellAwayRawE
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
    -- ...and, after the ping, SPEAK the belling terminal's location so you know
    -- which window/pane wants you without looking.  Location comes from the same
    -- pane-tree poll; multiple simultaneous bells are joined into one utterance.
    let bellAnnounceE = ffilter (not . null) $ attachWith
          (\tree ns -> [ p | n <- ns, Just p <- [bellLocation n tree] ])
          (current paneTreeD) bellAwayE
    performEvent_ $ ffor bellAnnounceE $ \phrases ->
        liftJSM . void $ jsg ("window" :: Text)
          ^. js1 ("leksahTermBell" :: Text) (T.intercalate ". " phrases)
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
    -- The ⌘` flipper hint's target: the one-press destination is the second
    -- entry of the MRU flip list (index 0 is the current pane).  Resolve it to
    -- an on-screen pane %id or a tab button and publish to hintsJs.
    hintTargetD <- holdUniqDyn $
        (\front tree items -> (snd <$> listToMaybe (drop 1 items)) >>= hintTarget front tree)
          <$> activeTermD <*> allTreeD <*> flipLiveD
    hintTargetPb <- getPostBuild
    performEvent_ $ ffor (leftmost [updated hintTargetD, tag (current hintTargetD) hintTargetPb]) $ \mt ->
        liftJSM . void $ do
            let (kind, val) = case mt of
                  Just (Left pid)   -> ("pane" :: Text, pid)
                  Just (Right bkey) -> ("button", bkey)
                  Nothing           -> ("", "" :: Text)
            jsg ("window" :: Text) ^. js2 ("leksahSetFlipTarget" :: Text) kind val
    -- While the flipper is open, highlight its LIVE selection (updates as ⌘` is
    -- tapped): the active-pane shadow on the selected pane (if on-screen) and the
    -- hover colour on its tab button.  An empty tab key means the flipper closed
    -- (revert the shadow to following focus, drop the tint).
    flipSelPubD <- holdUniqDyn $
        (\vis front tree msel -> case (vis, msel) of
            (True, Just (_, fi)) -> flipSelHighlight front tree fi
            _                    -> (Nothing, ""))
          <$> flipperVisibleD <*> activeTermD <*> allTreeD <*> flipperSelD
    performEvent_ $ ffor (updated flipSelPubD) $ \(mpane, tabKey) ->
        liftJSM . void $ jsg ("window" :: Text)
            ^. js2 ("leksahSetFlipSel" :: Text) (fromMaybe "" mpane) tabKey
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
    -- The Grep pane doubles as the LSP find-references list (Shift-F12): both a
    -- workspace grep and a references result replace its contents.
    grepResultsD <- holdDyn [] (leftmost [grepResultsE, lspRefsE])
    -- Opening a terminal tab (in the editor area) both creates it if needed and
    -- selects it.  Selecting a restored terminal therefore goes through the open
    -- path too: re-opening an already-open key is a no-op for `listViewWithKey`
    -- (no widget rebuild, so tmux isn't re-attached), it just becomes visible.
    -- File ▸ Close: a background thread turns close requests (from the menu
    -- command's IDEAction, via the close bridge) into a reflex event.
    (closeReqE, fireCloseReq) <- newTriggerEvent
    -- Close via the menu (⌘W) acts on the active pane (only editors and
    -- terminals have something to close).  ⌘W on a terminal DETACHES the whole
    -- tab — the tmux session (and its splits) survive so it can be reopened; it
    -- never kills a pane/split.  (The tree is still sampled so the shape is
    -- available should the decision ever need it again.)
    let closeDecisionE = attachWith
          (\(mk, _tree) () -> case mk of
              Just k@(EditorKey _)   -> Just (Right [k])
              Just k@(TerminalKey _) -> Just (Right [k])
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
          , openInWide0 <$> newOrEditTermE
          , openInWide0 <$> termRequestE
          , openInWide0 <$> remoteOpenKeyE
          , openInWide0 <$> selectAnyTermE
          , (\(s, _, _) -> openInWide0 s) <$> flipPaneE
          , (\(s, _)    -> openInWide0 s) <$> alertTargetE
          , (PreferencesKey =: ("wide0", Just ())) <$ showPrefsE ]
        -- Killing a remote session server-side also drops its tab if open — under
        -- either identity it may be keyed by (its id, or its name from a
        -- cc-connect HOST#NAME tab); closeWide0 ignores whichever isn't present.
        closeRemoteSessTabE = ffor killRemoteSessE $ \(h, s, nm) ->
            [ TerminalKey (remoteKey h s), TerminalKey (remoteKey h nm) ]
        closeTabsE = leftmost [ (\n -> [TerminalKey n]) <$> closeTermE, detachCloseE
                              , exitedTermE, closeRemoteSessTabE ]
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
                      (numKeyE _CommandSelectBottomPane)
          -- C-b w (tmux prefix interceptor): show + focus the Terminals pane,
          -- exactly like the ⌥⌘N side-pane navigation it rides through here.
          , ("tall" =: TerminalsKey) <$ activateTerminalsE ]
        selectTabE = leftmost [flipTabE, restoreVisibleE, ("wide1" =: GrepKey) <$ grepReqE
                              , ("wide1" =: GrepKey) <$ lspRefsE
                              , (\(s, _, _) -> "wide0" =: TerminalKey s) <$> flipPaneE
                              , (\(s, _)    -> "wide0" =: TerminalKey s) <$> alertTargetE
                              , ("wide0" =: PreferencesKey) <$ showPrefsE
                              , numSelTabE
                              -- Keep the wide0 visible tab in sync with the shared
                              -- per-window active tab (restore, or a tab moved to/from
                              -- this window changing what's active here).
                              , ("wide0" =:) <$> fmapMaybe id (updated wide0ActiveD)
                              -- Apply the seeded active tab on first build too:
                              -- 'updated' skips the initial value, so a restored or
                              -- background window (never focused → no wide0ActiveD
                              -- change) would otherwise leave its wide0 tab
                              -- visibility:hidden until it happened to change.
                              , ("wide0" =:) <$> fmapMaybe id (tag (current wide0ActiveD) restorePb)]
    -- wide0 membership/order/active for THIS window come from the shared
    -- per-window state (myWinD, bound in main's outer scope) — single source of
    -- truth in the shared MVar, so wide0 tabs can be owned by / moved between
    -- windows; the side/bottom-bar content is shared and rendered identically.
    --
    -- Order, though, is overlaid with a LOCAL activation MRU: the shared _wwWide0
    -- reorder happens via a modifyIDE_ (activateWide0, below), and relying on that
    -- write to fan out back to *this* window to re-derive the order is unreliable —
    -- the trigger fan-out can lag, drop, or deliver a stale snapshot to a busy or
    -- background window, so an activated pane would fail to float to the flipper /
    -- tab MRU front (even though the MVar, hence the saved session, is correct).
    -- So float this window's own activations (wide0ActivateE) to the front locally
    -- and immediately; a healthy fan-out echo then just agrees.  Membership still
    -- comes from the shared state, so tabs moved to/from this window are respected.
    localWide0MruD <- foldDyn (\k ks -> k : filter (/= k) ks) [] wide0ActivateE
    wide0OrderD  <- holdUniqDyn $
        (\ww mru ->
            let base     = _wwWide0 ww
                promoted = filter (`elem` base) mru
            in map (\k -> (k, Just ())) (promoted ++ filter (`notElem` promoted) base))
        <$> myWinD <*> localWide0MruD
    wide0ActiveD <- holdUniqDyn (_wwActive <$> myWinD)
    (recentTabs, tabE, visibleTabsD, activePaneD, tabCloseBtnE) <- tabsWidget
      initialTabs
      initialVisibleTabs
      wide0OrderD
      never   -- wide0 opens go through the shared state (moveTabTo), not here
      never   -- wide0 closes go through the shared state (closeWide0), not here
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
              -- Control mode needs tmux, which has no Windows build; there the
              -- classic ConPTY-backed widget is the only option.
              let useCC = tmuxSupported && (cm || "ssh://" `T.isPrefixOf` n)
              if useCC
                then terminalCCWidget ide n selectedE
                else terminalWidget ide n selectedE
          MetadataKey    -> toDM MetadataTab <$> metadataWidget ide activeFileD revealMetaD (paneFind MetadataKey)
          ChangesKey     -> toDM ChangesTab <$> changesWidget ide (paneFind ChangesKey)
          PreferencesKey -> toDM PreferencesTab <$> preferencesWidget ide
          EditorKey file -> toDM EditorTab <$> makeEditor file selectedE v)
    -- The active pane became a wide0 tab THIS window owns: float it to the MRU
    -- front / mark it active in the shared state (ignored for side/bottom tabs and
    -- for tabs owned by other windows).
    let wide0ActivateE = attachWithMaybe
          (\ord mk -> case mk of
                        Just k | k `elem` map fst ord -> Just k
                        _                             -> Nothing)
          (current wide0OrderD) (updated activePaneD)
    performEvent_ $ ffor (updated activePaneD) $ \mk -> do
        wlog wid ("activePane -> " <> show mk)
        focusLog ("[" <> show wid <> "] activePaneD -> " <> show mk)
    performEvent_ $ ffor wide0ActivateE $ \k -> do
        wlog wid ("wide0Activate " <> show k <> " (modifyIDE_ activateWide0)")
        focusLog ("[" <> show wid <> "] wide0Activate " <> show k)
    performEvent_ $ ffor (updated wide0OrderD) $ \o -> wlog wid ("wide0Order = " <> show (map fst o))
    -- Edit ▸ Find (toolbar button / menu item) toggles the find bar; showing it
    -- focuses its text input.  It starts hidden.  The native macOS menu routes
    -- here via a background thread draining the find-toggle bridge.
    (findBridgeE, fireFindReq) <- newTriggerEvent
    -- All five window-scoped bridges (close/save/find/prefs/open-file) are now
    -- defined; register this window's triggers so the process-wide drains
    -- (started in 'newIDE') can route each token to the frontmost window.
    liftIO $ registerWindowBridge wid WindowBridge
      { wbClose = fireCloseReq ()
      , wbSave  = fireSaveReq ()
      , wbFind  = fireFindReq ()
      , wbPrefs = firePrefsReq ()
      , wbOpenedFile = fireOpenedFile
      }
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
    -- A terminal-window button in the tab bar was clicked: (session id, window
    -- index).  Handled below by switching to that window's MRU pane and promoting
    -- it (defined here; consumed in the flip block above via MonadFix).
    (termWinSelE, fireTermWinSel) <- newTriggerEvent
    -- A pane click, published by the global mousedown listener (termActivityJs)
    -- as a tmux pane id.  A leksah-owned pane-recency signal: it fires only for
    -- a real click in THIS leksah window, never for a control-mode broadcast, so
    -- it floats exactly the clicked pane to the flipper MRU.  Consumed in the MRU
    -- block above via MonadFix (defined here next to its sibling term listeners).
    (jsPaneFocusE, fireJsPaneFocus) <- newTriggerEvent
    -- C-b w in a terminal (intercepted by window.LeksahTmux, dispatched via the
    -- window.leksahTmuxKey callback below) activates THIS window's Terminals side
    -- pane.  A per-window trigger, not a global Chan, so the activation lands in
    -- the window whose terminal received the key.  Consumed by numSelTabE above.
    (activateTerminalsE, fireActivateTerminals) <- newTriggerEvent
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
                (paneV:_) -> do
                    pane <- valToText paneV
                    focusLog ("[" <> show wid <> "] JS leksahListActivate " <> T.unpack pane)
                    liftIO (fireListActivate pane)
                _ -> return ())
        _ <- w ^. jss ("leksahFocusTab" :: Text) (fun $ \_ _ args -> case args of
                (kV:_) -> do
                    k <- valToText kV
                    focusLog ("[" <> show wid <> "] JS leksahFocusTab (focusin) " <> T.unpack k)
                    liftIO (fireFocusTab k)
                _ -> return ())
        _ <- w ^. jss ("leksahTermActivity" :: Text) (fun $ \_ _ _ -> liftIO (fireTermActivity ()))
        _ <- w ^. jss ("leksahPaneFocus" :: Text) (fun $ \_ _ args -> case args of
                (pV:_) -> do
                    pid <- valToText pV
                    focusLog ("[" <> show wid <> "] JS leksahPaneFocus " <> T.unpack pid)
                    liftIO (fireJsPaneFocus pid)
                _ -> return ())
        -- The tmux C-b prefix interceptor (window.LeksahTmux) hands the key that
        -- followed C-b here: 'w' shows this window's Terminals pane, the rest run
        -- the tmux command on the active terminal (see dispatchTmuxPrefix).
        _ <- w ^. jss ("leksahTmuxKey" :: Text) (fun $ \_ _ args -> case args of
                (tokV:_) -> do
                    tok <- valToText tokV
                    liftIO $ if tok == ("w" :: Text)
                               then fireActivateTerminals ()
                               else dispatchTmuxPrefix tok
                _ -> return ())
        return ()
    -- Mirror the tmuxInterceptPrefix pref into window.LeksahTmux.enabled so the
    -- JS interceptor turns on/off the instant the menu toggle flips it (and on
    -- first build, since 'updated' skips the initial value).
    tmuxInterceptD <- holdUniqDyn ((tmuxInterceptPrefix . view prefs) <$> ide)
    tmuxEnabledPb  <- getPostBuild
    performEvent_ $ ffor (leftmost [ updated tmuxInterceptD
                                   , tag (current tmuxInterceptD) tmuxEnabledPb ]) $ \on ->
        liftJSM . void $ jsg ("LeksahTmux" :: Text) ^. jss ("enabled" :: Text) on
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
    performEvent_ $ ffor activatedPaneE $ \(_, sel) -> do
        focusLog ("[" <> show wid <> "] activatedPaneE -> leksahFocusPane " <> T.unpack sel)
        liftJSM . void $ jsg ("window" :: Text) ^. js1 ("leksahFocusPane" :: Text) sel

    -- Persist the session (open files, open terminals, visible tabs) whenever it
    -- changes, but only once the saved session has been restored, so the initial
    -- (empty) state can't clobber the file before we've read it.  Debounced so a
    -- burst of restore/open events collapses into a single write.
    restoredFlagD <- holdDyn False (True <$ restoreE)
    -- Only the ACTIVE window's network persists the session: its visibleTabsD is
    -- the one saved as the shared side/bottom selection, and the per-window wide0
    -- + visibility all come from the shared '_webWindows', so any single window
    -- writes the full multi-window layout.  Gating to one avoids N racing writers.
    isActiveD   <- holdUniqDyn ((== Just wid) . _activeWindow <$> ide)
    -- The Preferences pane is transient — never save/restore it as an open tab.
    let notPrefs = (/= PreferencesKey)
    sessionD <- holdUniqDyn $
      (\wins vis recF ->
          WebSession 4
            [ WebWindowSession (filter notPrefs (_wwWide0 ww)) (_wwActive ww)
                               (_wwTall ww) (_wwWide1 ww)
            | (_, ww) <- M.toList wins ]
            (M.toList (M.filterWithKey (\a k -> a /= "wide0" && notPrefs k) vis))
            (Just recF))
        <$> webWindowsD <*> visibleTabsD <*> recentFilesD
    let writeGateD = (&&) <$> restoredFlagD <*> isActiveD
    saveSessE <- debounce (1 :: NominalDiffTime) (gate (current writeGateD) (updated sessionD))
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
      -- (Per-window side/bottom visibility is seeded into '_webWindows' by
      -- 'newIDE' at restore, so there is no restore-visibility event here.)
      <> ((\(PrefsUpdate f) -> [modifyIDE_ (prefs %~ f)]) <$> prefsPaneE)
      -- wide0 tab ownership lives in the shared per-window state: opening a tab
      -- moves it into THIS window (front, active), closing removes it, and
      -- focusing/clicking one floats it to the MRU front.  Every window's reflex
      -- network then re-renders its own wide0 row from that shared state.
      <> ((\m -> [modifyIDE_ (webWindows %~ \wins ->
                    foldl' (\ws k -> moveTabTo wid k ws) wins (M.keys m))]) <$> openTabsE)
      <> ((\ks -> [modifyIDE_ (webWindows %~ closeWide0 wid ks)]) <$> closeTabsE)
      <> ((\k -> [modifyIDE_ (webWindows %~ activateWide0 wid k)]) <$> wide0ActivateE)
      -- Cross-window flip: make the selected tab active in ITS window (which was
      -- just raised), without moving it here.
      <> (fmapMaybe (\(w, fi) -> (\k -> [modifyIDE_ (webWindows %~ activateWide0 w k)])
                                 <$> flipKeyOf fi) crossFlipE)
      -- Float an item to the front of the SHARED flip MRU ('_flipMru') — the one
      -- flipper order every window reads (see flipBumpE above).
      <> ((\fi -> [modifyIDE_ (flipMru %~ \mru -> fi : filter (/= fi) mru)]) <$> flipBumpE)
      -- Mirror this window's open flipper onto every other OS window purely through
      -- SHARED state: write '_flipMirror' and let each window draw it from its own
      -- reflex network (see flipMirrorSharedD above).  NO cross-window JS broadcast
      -- — 'ideJSM_' into every WKWebView deadlocks the jsaddle-wkwebview main-thread
      -- bridge with 2+ windows.  We tag the state with THIS window's id so the mirror
      -- knows the owner (and skips redrawing it here).
      <> ((\(items, idx) -> [modifyIDE_ (flipMirror .~ Just (widN, items, idx))])
            <$> flipMirrorShowE)
      <> ((\() -> [modifyIDE_ (flipMirror .~ Nothing)])
            <$> flipMirrorHideE)
  return topEvents
