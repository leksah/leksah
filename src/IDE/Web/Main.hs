{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Main
  (
#if defined(ghcjs_HOST_OS)
  -- GHC JavaScript backend (the in-browser web demo): no warp server, no
  -- jsaddle websocket transport — jsaddle-warp's `run` drives the page's own
  -- DOM directly, so the server-side entry points don't exist here.
    browserMain
#else
    develMain
  , splitFrontendMain
  , startJSaddle
  , indexHtml
#endif
  , newIDE
  , main
  , css
  , jsMain
  , mintWindowId
  ) where

import Control.Concurrent
       (tryPutMVar, takeMVar, putMVar, readMVar, threadDelay, modifyMVar,
        newMVar, newEmptyMVar, forkIO, killThread, myThreadId,
        rtsSupportsBoundThreads, getNumCapabilities, setNumCapabilities)
import GHC.Conc (getNumProcessors)
import Control.Concurrent.Chan (readChan)
import Control.Concurrent.MVar (MVar, mkWeakMVar, withMVar)
import Control.Concurrent.STM (readTVarIO)
import GHC.Conc.Sync (labelThread)
import Control.Event (registerEvent)
import Control.Exception (SomeException, catch)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import GHC.Stats
       (getRTSStats, getRTSStatsEnabled, RTSStats(..), GCDetails(..))
#if defined(ghcjs_HOST_OS)
import qualified System.IO as IO
       (hPutStrLn, stderr, stdout, hSetBuffering, BufferMode(..))
#else
import qualified System.IO as IO (hPutStrLn, stderr, hSetBuffering, BufferMode(..))
#endif
import Control.Lens (to, view, (^.), (^..), (^?), (?~), (.~), (%~), (<&>), _Just)
import Control.Applicative ((<|>), optional)
import Control.Monad (forever, forM, forM_, guard, unless, when, void)
import Control.Monad.IO.Class (MonadIO(..))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Char8 as BS (unlines)
import qualified Data.ByteString.Lazy as BS (toStrict)
import qualified Data.ByteString.Lazy as LBS (fromStrict, readFile)
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
       (Map, keys, elems, toList, toAscList, fromList, fromListWith, union,
        findWithDefault, lookup, insert, insertWith, adjust, alter, delete,
        member, filterWithKey, singleton, mapWithKey, empty, size, null,
        withoutKeys)
import Data.Map (Map)
import qualified Data.Set as S
       (Set, fromList, delete, null, singleton, empty, insert, member, intersection, toList)
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, unlines, isPrefixOf, null, intercalate, breakOn, drop, stripPrefix, takeWhile, all, splitOn, take, length, replace)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import Text.Printf (printf)
import Text.Read (readMaybe)

import System.Directory
       (doesFileExist, doesDirectoryExist, getDirectoryContents, removeFile,
        getHomeDirectory, getTemporaryDirectory, makeRelativeToCurrentDirectory)
import System.Process (readProcessWithExitCode)
import Data.Aeson (Value, decodeStrict', encode, withObject, (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT (parseMaybe)
import Data.List (nub, sort, isPrefixOf, isInfixOf, find, elemIndex, findIndex)
import Data.Maybe (fromMaybe, catMaybes, listToMaybe, isNothing)
import System.Exit (ExitCode(..))
import System.FilePath
       (takeFileName, takeExtension, dropFileName, takeDirectory, (</>),
        dropTrailingPathSeparator)
import System.Environment (getArgs, setEnv)
import IDE.Utils.ExitImmediately (exitImmediately)
#if !defined(ghcjs_HOST_OS)
import System.FSNotify (withManager)
#endif

#if !defined(ghcjs_HOST_OS)
import Network.Socket
       (withSocketsDo, socket, bind, listen, socketPort, setSocketOption,
        SocketOption(ReuseAddr), SockAddr(SockAddrInet), Family(AF_INET),
        SocketType(Stream), defaultProtocol, tupleToHostAddress)
import qualified Network.HTTP.Types as H (status200, status504)
import qualified Network.Wai as W
       (responseLBS, pathInfo, requestMethod, strictRequestBody)
import Network.Wai.Application.Static
       (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings, runSettingsSocket)
import Network.WebSockets (defaultConnectionOptions)

import Criterion.Measurement (initializeTime)
#endif

import Clay
       (height, pct, width, fontFaceSrc, fontWeight, fontStyle,
        fontFace, render, (?), margin, nil, px, fontFamily, background,
        color, black, white, fontSize, normal, FontFaceFormat(..),
        FontFaceSrc(..))

import Language.Javascript.JSaddle
       (JSM, eval, syncPoint, jsg, js, js0, js1, js2, js3, jss, fun, toJSVal, valToText, valToBool, valToNumber, liftJSM, runJSM)
#if defined(ghcjs_HOST_OS)
-- Under the JS backend jsaddle-warp is a base-only shim whose `run` executes
-- the JSM directly against the page (no port, no server) — the websocket
-- transport (jsaddleJs/jsaddleOr/debugWrapper) doesn't exist there.
import qualified Language.Javascript.JSaddle.Warp as JSW (run)
#else
import Language.Javascript.JSaddle.Warp
       (jsaddleJs, jsaddleOr, debugWrapper)
import Language.Javascript.JSaddle.Terminal.Bootstrap (bootstrapHtml)
import IDE.Web.JsaddleTunnel (tunnelSyncRequest)
import GHCJS.DOM.Debug (addDebugMenu)
#endif
import GHCJS.DOM.Types (askJSM)

import Reflex
       (switchDyn, switchHold, foldDyn, ffor,
        Dynamic, Event, holdDyn, merge, newTriggerEvent, leftmost, never,
        performEvent_, getPostBuild, performEvent, select, fan, fanMap,
        fmapMaybe, ffilter, attachWith, attachWithMaybe, attach, current, updated, holdUniqDyn, tag, gate, zipDyn,
        listViewWithKey, sample, constDyn,
        tagPromptlyDyn, debounce, delay, tickLossyFromPostBuildTime)
import Reflex.Dom.Core
       (dyn, dynText, el, elAttr, elAttr', elDynAttr, elDynAttr', divClass,
        text, blank, domEvent, EventName(..),
        _element_raw, (=:), MonadWidget, mainWidgetWithCss)

import IDE.Core.State
       (triggerBuild, readIDE, IDEAction, wsFile, jsContexts, workspace,
        IDEState(..), Prefs(..), TallVisibility(..), IDE(..), IDERef, __,
        externalEditor, monacoEditor,
        reflectIDE, getDataDir, catchIDE, modifyIDE_, modifyIDE, prefs, currentState,
        wsProjects, pjPackages, ipdCabalFile, ipdPackageDir, wsActivePackFile,
        currentError, logRefFullFilePath, refDescription, logRefSrcSpan,
        srcSpanStartLine,
        WindowId(..), WebWindow(..), webWindows, activeWindow, nextWindowId,
        leksahWindows, nextLeksahWin, hiddenWindows,
        LeksahWindow(..), PaneContent(..), PaneKind(..), LeafId(..),
        SplitOrientation(..), SplitTree(..),
        flipMirror, flipMru, AIPaneRef(..), paneAISession,
        ideVersion, focusLog, metaLog)
#ifdef LEKSAH_METADATA
import IDE.Metainfo.Provider (initInfo)
#endif
import IDE.Web.IDERefStore (setGlobalIDERef, getGlobalIDERef)
import IDE.Web.HostFlags (setBrowserHosted, getBrowserHosted, flipHintText)
import IDE.Web.Bridge
       (Bridge, BridgeError, BridgeValue(..), HelloInfo(..), Side(..),
        bridgeProtocolVersion, call, callJSON, encodeValue1, expose,
        exposeJSON, helloHandshake, newDirectBridge, newJsBridge)
import IDE.Web.BridgeStore
       (getBackendBridge, getFrontendBridge, setBackendBridge,
        setFrontendBridge)
#if defined(ghcjs_HOST_OS)
import IDE.Web.DemoTerminals (demoTerminals)
#endif
import IDE.Git (runGit)
import IDE.Utils.RemotePath (isRemotePath)
import IDE.Utils.RemoteExec (remoteInFlight, remoteInFlightChanged)
import IDE.Web.FS (fsListFilesRecursive, fsReadFile)
import IDE.Web.RemoteRefresh (registerRemoteRefresh)
import IDE.Web.TerminalRefresh (registerTerminalRefresh, ensureTerminalMonitor)
import IDE.Web.Instance (leksahPort)
import IDE.Web.Handoff
       (handoffEnabled, isHandoffSuccessor, requestHandoff, handingOff, signalHandoffReady,
        registerSessionFlush, signalSessionFlushDone)
import IDE.Web.CmdServer (startCmdServer, suppressNextRestart)
import IDE.Web.OpenFileRequest (deliverOpenedFile)
import IDE.Web.OpenPanel (runOpenFilePanel, runOpenProjectPanel, runOpenFolderPanel)
import IDE.Web.Theme (themeVarsCss, paletteCss, contrastCss, bgColor, fgColor)
import IDE.Web.WindowBridge
       (WindowBridge(..), registerWindowBridge, startWindowBridgeDrains,
        registerResync, notifyResync, setFocusedLeaf)
import IDE.Web.RegionGrabRequest (nextRegionGrab)
import IDE.Web.AddRemoteRequest (nextAddRemoteRequest)
import IDE.Web.AddServerRequest (nextAddServerRequest)
import IDE.Web.RemoteSettingsRequest (nextRemoteSettings)
import IDE.Web.ScreenshotRequest (requestScreenshotRegion)
import IDE.Web.RegionCapture
       (screenCaptureAllowed, grabRegionToTarget, grabRegionToFile,
        sendPathToTarget, sendTextToTarget, nextRegionFile,
        resolveTmuxSessionId)
import IDE.Web.AIContextRequest (AIAction(..), nextAIAction)
import IDE.Web.RemoteTermRequest (nextTermRequest, requestLocalTerm)
import IDE.Web.ConvertRequest (nextConvertRequest)
import IDE.Web.SplitLayout
       (readLeksahWindows, saveSessionLayouts, reconcileWindows,
        encodeLayoutOption, splitLeaf, closeLeaf, treeLeafIds,
        viewLeafAllowed, singlePaneWindow, lwIdText, lwIdNum, lwWindowIds,
        ConvertedPath(..), spliceConverted, setPaneFont,
        DropSpec(..), detachLeaf, insertLeafAt, moveLeafInWindow,
        pickDropTarget, successorLeaf, windowOwner, subtreeRects,
        MergeGroup(..), consolidateGroups, collapseGroup)
import IDE.Web.SplitOpenRequest
       (SplitTarget(..), nextSplitOpenRequest, requestSplitOpen)
import IDE.Web.RecentFiles (updateRecentFiles)
import IDE.Web.GhciMode
       (ghciMode, recordPreRunThreads, registerGhciCleanupNamed, stopForGhci)
import IDE.LSP (shutdownServers)
import IDE.Web.ThreadPriority (ThreadPriority(..), raiseCurrentThreadPriority)
import IDE.Web.ReplTmux
       (tmuxCmd, tmuxSupported, activePaneIdOfSession, activePaneIdOfWindow,
        openTerminalInDir, splitPane, newSessionWindow, newWindowInSession,
        shellCommandForDir, freshSessionName, tmuxOut, paneCountOfWindow,
        runInTerminal, prefixedCmdLine)
import IDE.Web.SaveRequest (requestSaveActiveFileWait)
import IDE.Web.Claude
       (runClaudeCmd, claudeCommandLine, ClaudeCmd(..), ClaudeLive(..),
        claudeLiveBySession, paneForSession, sendToSession, showLiveSession)
import IDE.Web.AISession
       (AIChoice(..), AIRow(..), activeAIPaneRef, aiPickerChoices, choiceKey)
#if !defined(ghcjs_HOST_OS)
import IDE.Web.ClaudeStatus
       (ClaudeStatus(..), emptyClaudeStatus, claudeStatusNow,
        claudeStatusTooltip, startClaudeStatusPoll)
import IDE.Web.AgentInfo (agentTitles)
#endif
import IDE.Web.NewLwRequest
       (nextNewLwRequest, beginConversion, endConversion, conversionActive,
        nextFontConvert, nextConsolidate, consolidateLock)
import IDE.Web.TmuxLayout
       (TmuxCell(..), parseWindowLayout, printWindowLayout, cellPanes,
        rerootCell, combineCells)
import IDE.Web.TerminalInput
       (setActiveTerminal, setActiveConvertible, setActiveViewSplit,
        setActiveSplitWindow, tmuxCommandActiveTerminal,
        selectSplitActiveTerminal, focusTerminalPane, dispatchTmuxPrefix,
        getActiveTerminal)
import IDE.Web.TransparencyRequest (nextToggleTransparency)
import IDE.Web.SnapRequest (SnapReq(..), nextSnapRequest)
import IDE.Web.Session
       (WebSession(..), WebWindowSession(..), readWebSession, writeWebSession)
import IDE.Web.NewWindowRequest (requestOpenWindow, requestRaiseWindow)
#if defined(ghcjs_HOST_OS)
-- Browser: no config dir or data files — 'newIDE' bakes in the defaults
-- instead of loading prefs from disk.  WatchManager is Core.Types'
-- fsnotify stand-in (see the withManager shim below).
import IDE.Core.Types (WatchManager(..))
import IDE.Preferences (defaultPrefs, writePrefs)
#else
import IDE.Preferences (readPrefs, writePrefs)
#endif
import IDE.Utils.FileUtils
       (loadNixCache, getConfigFilePathForLoad, getConfigFilePathForSave)
import IDE.Utils.Utils
       (standardPreferencesFilename)
import IDE.Web.Command
       (commandAction, Command(..), _CommandSelectSplit,
        _CommandSelectSidePane, _CommandSelectBottomPane)
import IDE.Web.Events
       (IDEWidget(..), TabEvents(..), TabKey(..), TerminalEvents(..),
        FindbarEvents(..), PreferencesEvents(..), FlipItem(..),
        KeymapEvents(..),
        _ToolbarCommand, _MenubarCommand, _KeymapCommand, _PackageCommand,
        _ProjectPackageEvents, _ProjectCommand, _NewTerminal, _SelectTerminal,
        _CloseTerminal, _SelectTerminalWindow, _SelectTerminalPane,
        _NewRemoteTerminal, _SelectRemoteHost, _SelectRemoteTerminal,
        _SelectRemoteTerminalWindow, _SelectRemoteTerminalPane,
        _CloseRemoteTerminal, _NewRemoteTerminalWindow, _KillRemoteTerminalWindow,
        _RenameRemoteTerminalSession, _RenameRemoteTerminalWindow,
        _ZoomRemoteTerminalPane, _BreakRemoteTerminalPane, _KillRemoteTerminalPane)
import IDE.Web.Layout (layoutCss)
import IDE.Web.KittyGraphics (kittyGraphicsJs, installImageBytesBridge)
import IDE.Web.Widget.Changes (changesCss, changesWidget)
import IDE.Web.Widget.GitLog (gitLogCss, gitLogWidget, gitLogSplitJs)
import IDE.Web.Widget.Review (reviewCss, reviewWidget)
import IDE.Web.Widget.NewWorktree (newWorktreeDialog)
import IDE.Web.Widget.Tasks (tasksCss, tasksWidget)
import IDE.Web.Widget.Agents (agentsCss, agentsWidget, agentLinksJs)
import IDE.Web.Widget.Plan (planCss, planWidget)
import IDE.Web.Widget.Compare (compareCss, compareWidget)
import IDE.Web.Worktree (nextNewWorktreeRequest, nextReviewRequest)
import IDE.Web.ClaudeQueue
       (nextTaskQueueRequest, nextPlanReviewRequest, nextCompareRequest,
        armQueueScheduler)
import IDE.Web.GitLogRequest (nextGitLogRequest, requestGitLog)
import IDE.Web.Widget.Preferences (preferencesCss, preferencesWidget)
import IDE.Web.Widget.Shortcuts (shortcutsCss, shortcutsWidget, shortcutsPlainText)
import IDE.Web.GitInfo (openUrl)
import IDE.Web.Widget.Browser
       (browserCss, browserWidget, nextBrowserId, rememberUrl, isOwnUrl)
import IDE.Web.Widget.Flake (flakeCss)
import IDE.Web.Widget.ContextMenu (contextMenuCss)
import IDE.Web.Widget.Editor (editorCss, editorWidget)
import IDE.Web.Widget.Errors (errorsCss, errorsWidget)
import IDE.Web.Widget.Findbar (findbarCss, findbarWidget, findMatcher)
import IDE.Web.Widget.AddRemote (addRemoteDialog)
import IDE.Web.Widget.AddServer (addServerDialog)
import IDE.Web.Widget.RemoteSettings (remoteSettingsDialog)
import IDE.Web.Widget.Flipper (flipperCss, flipperWidget)
import IDE.Web.Widget.AIPicker (aiPickerCss, aiChoiceLabel, aiPickerKeysJs)
import IDE.Web.Widget.Grep (grepCss, grepWidget, runGrep)
import IDE.Web.Widget.Keymap (keymapWidget)
import IDE.Web.Widget.Log (logCss, logWidget)
import IDE.Web.Widget.Menu (menuCss)
import IDE.Web.Widget.Menubar (menubarCss, menubarWidget)
#ifdef LEKSAH_METADATA
import IDE.Web.Widget.Metadata (metadataCss, metadataWidget)
#endif
import IDE.Web.Widget.Statusbar (statusbarCss, statusbarWidget)
import IDE.Web.Widget.Tabs (tabsWidget, tabsCss)
import IDE.Web.Widget.Terminal
       (terminalCss, terminalWidget, listTerminalSessions, killTerminalSession,
        selectTmuxWindow, selectTmuxPane, activePaneId, paneGeometry, sessionOfPane,
        listTerminalTree, createTerminalSession, openFileInEditor, notifyTerminalBell,
        cleanupStaleTwinPanes, resolveEditorCmd, shellQuoteArg,
        killTmuxPaneId, breakTmuxPaneId, windowIndexOfPane, paneCountOfSession,
        moveTmuxWindow, selectTmuxWindowId, selectTmuxPaneId,
        panesOfWindow, joinTmuxPane, joinTmuxPaneFull, breakTmuxPaneTo,
        windowLayoutString, movePaneToPane, selectWindowLayout, swapTmuxPanes,
        createRemoteSession, selectRemoteTmuxWindow, selectRemoteTmuxPane,
        killRemoteTmuxSession, killRemoteTmuxWindow, killRemoteTmuxPane,
        newRemoteTmuxWindow, zoomRemoteTmuxPane, breakRemoteTmuxPane,
        renameRemoteTmuxSession, renameRemoteTmuxWindow,
        listRemoteTerminalTree, remoteTabHostTarget,
        reapControlClients, TmuxWindow(..), TmuxPane(..), isClaudePane)
import IDE.Web.Widget.Terminals
       (terminalsCss, terminalsWidget, sessionAlert, windowAlertSrc,
        windowActivePane, isClaudeWindow, windowIconSrc, windowTabLabel,
        stripIdxPrefix)
import IDE.Web.Widget.TerminalCC (terminalCCWidget)
import IDE.Web.Widget.LwView (sessionlessLwWidget)
import IDE.Web.Widget.Toolbar (toolbarCss, toolbarWidget)
import IDE.Web.Widget.Workspace (workspaceCss, workspaceWidget)
import qualified IDE.Workspaces.Writer as Writer
       (setWorkspace, readWorkspace, resolveDeferredProjects)
import IDE.Workspaces (backgroundMake)

-- > :fork 1 IDE.Web.Main.develMain

-- | 'convertWindowPane' aimed at tmux window @w@'s ACTIVE pane — what the font
-- convert and ⌥-open want, since both act on where the keyboard is.
convertWindowMinimal :: Text -> Text -> IO Bool
convertWindowMinimal lwi w =
    tmuxOut ["display-message", "-p", "-t", T.unpack w, "#{pane_id}"]
      >>= maybe (return False) (convertWindowPane lwi w)

-- | Minimal-ancestor-path conversion: isolate pane @p@ of tmux window @w@
-- by re-hosting every sibling subtree on the root→pane path into its own
-- fresh detached window (single panes via @break-pane@; multi-pane subtrees
-- via break + @join-pane@ + @select-layout@ with the subtree's own geometry,
-- see "IDE.Web.TmuxLayout"), then mirror the path as native split nodes in
-- leksah window @lwi@ ('spliceConverted': the isolated pane keeps the
-- original leaf id so its xterm survives; siblings inherit its font and
-- keep their internal tmux layouts).  Stray adoption is suppressed while
-- the tmux surgery is in flight.  False when there was nothing to convert
-- (single-pane window) or any step failed.
--
-- Because the mirrored splits carry tmux's own cell proportions, the isolated
-- pane ends up exactly where it was on screen — which is why the ⌘-drag pane
-- move can use this to land a pane that CANNOT live inside a tmux window (an
-- editor, a differently-sized terminal) at one tmux pane's edge.
convertWindowPane :: Text -> Text -> Text -> IO Bool
convertWindowPane lwi w actP = do
    beginConversion
    r <- doIt `catch` \(_ :: SomeException) -> return False
    endConversion
    return r
  where
    doIt = do
      mtree <- (>>= parseWindowLayout)
                 <$> tmuxOut ["display-message", "-p", "-t", T.unpack w
                             , "#{window_layout}"]
      msid  <- tmuxOut ["display-message", "-p", "-t", T.unpack w
                       , "#{session_id}"]
      case (,) <$> mtree <*> msid of
        Just (tree, sid) | length (cellPanes tree) > 1
                         , Just pre <- prePath actP tree -> do
            mcp <- realize sid pre
            case mcp of
              Nothing -> return False
              Just cp -> do
                getGlobalIDERef >>= mapM_ (\r' -> (`reflectIDE` r') $
                  modifyIDE_ $ \i ->
                    case [ l | Just lw <- [M.lookup lwi (i ^. leksahWindows)]
                             , l <- treeLeafIds (lwTree lw)
                             , Just pc <- [M.lookup l (lwPanes lw)]
                             , pcKind pc == PaneTmux w ] of
                      (l : _) -> i & leksahWindows
                                     %~ M.adjust (spliceConverted l cp) lwi
                      []      -> i)
                return True
        _ -> return False
    prePath p c = case c of
      TCPane { tcId = q }      -> if q == p then Just PreLeaf else Nothing
      TCRow  { tcKids = ks }   -> node SplitH tcW ks
      TCCol  { tcKids = ks }   -> node SplitV tcH ks
      where
        node o extent ks = do
          idx <- findIndex (\k -> p `elem` cellPanes k) ks
          sub <- prePath p (ks !! idx)
          let exts  = map (fromIntegral . extent) ks :: [Double]
              total = max 1 (sum exts)
          Just . PreNode o $
            [ (e / total, if j == idx then Right sub else Left k)
            | (j, (e, k)) <- zip [0 :: Int ..] (zip exts ks) ]
    realize _   PreLeaf = return (Just CPLeaf)
    realize sid (PreNode o ks) = do
        mks <- mapM (\(s, ek) -> case ek of
            Left cell -> fmap (fmap ((,) s . Left))  (rehost sid cell)
            Right pp  -> fmap (fmap ((,) s . Right)) (realize sid pp)) ks
        return (CPNode o <$> sequence mks)
    rehost sid cell = case cellPanes cell of
        []         -> return Nothing
        [p]        -> breakPaneTo sid p
        (p : rest) -> breakPaneTo sid p >>= \case
            Nothing -> return Nothing
            Just wn -> do
                -- Join in traversal order, each AFTER the previous pane —
                -- select-layout assigns cells by the window's pane order, so
                -- joining everything onto the window (its active pane) would
                -- swap slots.
                let joinAfter prev [] = prev `seq` return ()
                    joinAfter prev (q : qs) = do
                        tmuxCmd ["join-pane", "-d", "-s", T.unpack q
                                , "-t", T.unpack prev]
                        joinAfter q qs
                joinAfter p rest
                tmuxCmd ["select-layout", "-t", T.unpack wn
                        , T.unpack (printWindowLayout (rerootCell cell))]
                return (Just wn)
    -- The destination session must be EXPLICIT: an untargeted break-pane
    -- lands the new window in the client's current session, not the source
    -- pane's.
    breakPaneTo sid p = tmuxOut ["break-pane", "-d", "-s", T.unpack p
                                , "-t", T.unpack sid <> ":"
                                , "-P", "-F", "#{window_id}"]

-- | The root→pane path before its sibling subtrees are re-hosted (the
-- 'Left's still carry the tmux cells to move).
data PrePath = PreLeaf | PreNode SplitOrientation [(Double, Either TmuxCell PrePath)]

-- | Group the leksah windows by their backing tmux session (id-ascending
-- within each group) — the unit @\@leksah_layout@ persistence works in.
-- Sessionless windows are absent (they persist in the web session file).
lwsBySession :: M.Map Text LeksahWindow -> M.Map Text [(Text, LeksahWindow)]
lwsBySession lws = M.fromListWith (flip (<>))
  [ (sid, [(i, lw)]) | (i, lw) <- M.toAscList lws, Just sid <- [lwSession lw] ]

-- | The tmux session a terminal-family tab is backed by: a leksah window's
-- backing session, or a remote tab's ssh:// key directly.
tabSessionOf :: M.Map Text LeksahWindow -> TabKey -> Maybe Text
tabSessionOf lws k = case k of
  TerminalKey n  -> Just n
  LeksahWinKey n -> M.lookup n lws >>= lwSession
  _              -> Nothing

-- | The tab key that shows tmux session @s@: its first leksah window's tab,
-- or the remote 'TerminalKey' tab for ssh:// keys.  'Nothing' for a local
-- session with no leksah window yet (the reconcile will mint one and place
-- its tab within a poll tick).  Also accepts a leksah-window id directly.
termTabKey :: M.Map Text LeksahWindow -> Text -> Maybe TabKey
termTabKey lws n
  | n `M.member` lws            = Just (LeksahWinKey n)
  | "ssh://" `T.isPrefixOf` n   = Just (TerminalKey n)
  | otherwise = listToMaybe
      [ LeksahWinKey i | (i, lw) <- M.toAscList lws, lwSession lw == Just n ]

-- | Session-name base for a directory: its basename ("shell" when empty).
sessionBase :: FilePath -> Text
sessionBase dir = case T.pack (takeFileName (dropTrailingPathSeparator dir)) of
  "" -> "shell"
  t  -> t

-- | Bind a (sessionless) leksah window to its backing tmux session — the
-- moment its first terminal pane arrives (⌘D beside a materialized view).
bindLwSession :: Text -> Text -> IO ()
bindLwSession lwi sid = getGlobalIDERef >>= mapM_ (\r -> (`reflectIDE` r) $
    modifyIDE_ (leksahWindows %~
        M.adjust (\lw -> lw { lwSession = Just sid }) lwi))

-- | Mint a fresh single-pane leksah window in the shared map (advancing the
-- id minter) and return its id.  IO-side — the open paths run in forked
-- handlers; the tab is placed by 'requestLocalTerm' (or the reconcile's tab
-- sync).  'Nothing' before the global IDE ref exists.
mintLeksahWindow :: Maybe Text -> PaneContent -> IO (Maybe Text)
mintLeksahWindow msess pc = getGlobalIDERef >>= \case
    Nothing   -> return Nothing
    Just ideR -> (`reflectIDE` ideR) $ fmap Just . modifyIDE $ \i ->
      let n   = i ^. nextLeksahWin
          lwi = lwIdText n
      in ( i & nextLeksahWin .~ (n + 1)
             & leksahWindows %~ M.insert lwi (singlePaneWindow msess pc)
         , lwi )

-- | Keep every OS window's tab list consistent with the leksah-window map:
-- prune tabs whose window is gone (falling the active tab back to the next
-- one), and append a tab for any window in the map that no OS window shows.
-- Runs at the end of 'applyReconcile' and after every cross-window pane move
-- (which can dissolve an emptied source window with no tmux change to wake
-- the reconcile).
syncLwTabs :: IDE -> IDE
syncLwTabs i =
  let lws = i ^. leksahWindows
      liveTab k = case k of LeksahWinKey n -> n `M.member` lws
                            _              -> True
      pruneWin ww =
        let w0 = filter liveTab (_wwWide0 ww)
            act = case _wwActive ww of
              Just a | liveTab a -> Just a
              Just _             -> listToMaybe w0
              Nothing            -> Nothing
        in ww { _wwWide0 = w0, _wwActive = act }
      pruned = fmap pruneWin (i ^. webWindows)
      placed = S.fromList
        [ n | ww <- M.elems pruned, LeksahWinKey n <- _wwWide0 ww ]
      -- First OS window (id order) showing each session.
      sessionHome = M.fromListWith (\_ old -> old)
        [ (s, osW)
        | (osW, ww) <- M.toAscList pruned
        , LeksahWinKey n <- _wwWide0 ww
        , Just s <- [M.lookup n lws >>= lwSession] ]
      targetFor lw = case lwSession lw >>= (`M.lookup` sessionHome) of
        Just osW -> Just osW
        Nothing  -> case i ^. activeWindow of
          Just osW | osW `M.member` pruned -> Just osW
          _ -> listToMaybe (M.keys pruned)
      place ws (n, lw) = case targetFor lw of
        Nothing  -> ws
        Just osW -> M.adjust (\ww -> ww
          { _wwWide0  = _wwWide0 ww <> [LeksahWinKey n]
          , _wwActive = case _wwActive ww of
              Nothing -> Just (LeksahWinKey n)
              a       -> a
          }) osW ws
      final = foldl' place pruned
        [ (n, lw) | (n, lw) <- M.toAscList lws
        , not (n `S.member` placed) ]
  in i & webWindows .~ final

-- | The ⌘-drag pane move's payload (leafDragJs delivers it as one JSON blob
-- on release or Escape): what was dragged, and where it was let go.
data LeafDragSrc = LDPane Text Int   -- ^ pane @leaf@ of leksah window @lw@
                 | LDTmuxPane Text Int Text
                     -- ^ ONE tmux pane (@%N@) of a multi-pane window living
                     -- in pane @leaf@ of leksah window @lw@ — joined into a
                     -- same-font destination window with tmux, broken out
                     -- into a fresh window + leaf otherwise
                 | LDTab Text        -- ^ a plain wide0 tab (its @show@-key)
  deriving (Eq, Show)

-- | One tmux PANE of a multi-pane window as the drop target — the thing a
-- 'DropSpec' cannot say, since a leksah leaf is a whole tmux window.  Picked in
-- the DOM by 'leafDragJs' (from the pane marker boxes, whose geometry is tmux's,
-- not the split tree's) and only ever trusted after the commit side has checked
-- that leaf @ttLeaf@ still holds a tmux window and that @ttPane@ is still one of
-- its panes; the pointer travels with it, so a stale pick degrades to the
-- ordinary leaf-level recompute.
data TmuxTarget = TmuxTarget
  { ttLeaf   :: Int
  , ttPane   :: Text                 -- ^ @%N@ — server-global
  , ttOrient :: SplitOrientation     -- ^ side-by-side or above/below
  , ttAfter  :: Bool                 -- ^ right\/below it, rather than left\/above
  } deriving (Eq, Show)

data LeafDragDst
  = LDDstPane Text (Double, Double) (Double, Double) (Maybe TmuxTarget)
      -- ^ inside leksah window @lw@: pointer px, container (w, h) px — the
      -- drop target is RECOMPUTED from these against the live tree — plus the
      -- tmux pane the pointer was aimed at, when it was aimed at one
  | LDDstTab Text                    -- ^ on a tab button (its @show@-key)
  | LDDstNone                        -- ^ nowhere / cancelled
  deriving (Eq, Show)

parseLeafDrop :: Value -> Maybe (LeafDragSrc, LeafDragDst)
parseLeafDrop = AT.parseMaybe $ withObject "leafDrop" $ \o -> do
    src <- o .: "src" >>= withObject "src" (\s ->
             -- pane-carrying sources also have lw/leaf keys: try first
             (LDTmuxPane <$> s .: "lw" <*> s .: "leaf" <*> s .: "pane")
             <|> (LDPane <$> s .: "lw" <*> s .: "leaf")
             <|> (LDTab <$> s .: "tab"))
    dst <- o .: "dst" >>= withObject "dst" (\d -> d .: "kind" >>= \k ->
             case k :: Text of
               "pane" -> LDDstPane <$> d .: "lw"
                           <*> ((,) <$> d .: "px" <*> d .: "py")
                           <*> ((,) <$> d .: "cw" <*> d .: "ch")
                           <*> optional (TmuxTarget
                                 <$> d .: "tleaf" <*> d .: "tpane"
                                 <*> ((\v -> if v then SplitV else SplitH)
                                        <$> d .: "tv")
                                 <*> d .: "tafter")
               "tab"  -> LDDstTab <$> d .: "tab"
               _      -> pure LDDstNone)
    return (src, dst)

-- | The cross-window pane move — ONE pure step, so no intermediate state (an
-- unowned tmux window ripe for stray adoption, an emptied source window) is
-- ever observed by the reconcile or the persistence debounce.  The moved
-- pane's id is REMINTED in the destination ('LeafId's are per-window) and its
-- flip-MRU entry follows; a tmux pane landing in a SESSIONLESS destination
-- binds it to the source's session (when the sessions genuinely differ the
-- caller has already run 'moveTmuxWindow' under the conversion guard).  The
-- destination un-zooms so the arrival is visible; source focus falls to the
-- leaf absorbing the space.  Returns @i@ unchanged whenever a piece vanished
-- under the drag.  Callers compose 'syncLwTabs' after it.
moveLeafAcross :: Text -> LeafId -> Text -> DropSpec -> IDE -> IDE
moveLeafAcross srcLwId srcLeaf dstLwId spec i = fromMaybe i $ do
    guard (srcLwId /= dstLwId)
    slw <- M.lookup srcLwId lws
    dlw <- M.lookup dstLwId lws
    pc  <- M.lookup srcLeaf (lwPanes slw)
    let newId   = LeafId (lwNext dlw)
        dstTree = insertLeafAt spec newId (lwTree dlw)
    guard (newId `elem` treeLeafIds dstTree)
    let dlw' = dlw
          { lwTree    = dstTree
          , lwPanes   = M.insert newId pc (lwPanes dlw)
          , lwFocused = Just newId
          , lwZoomed  = Nothing
          , lwNext    = lwNext dlw + 1
          , lwSession = case pcKind pc of
              PaneTmux _ | isNothing (lwSession dlw) -> lwSession slw
              _                                      -> lwSession dlw
          }
        srcRes = withoutLeaf srcLeaf slw
        lws' = M.insert dstLwId dlw' $
          maybe (M.delete srcLwId lws) (\s -> M.insert srcLwId s lws) srcRes
        moveFlip fi = case (fi, newId) of
          (FlipView n l, LeafId newN)
            | n == srcLwId, LeafId l == srcLeaf -> FlipView dstLwId newN
          _ -> fi
    return $ i & leksahWindows .~ lws'
               & flipMru %~ map moveFlip
  where lws = i ^. leksahWindows

-- | One leksah window without leaf @l@: focus falls to the leaf absorbing its
-- space, and 'Nothing' means that was the last pane, so the whole leksah window
-- dissolves (compose 'syncLwTabs' to prune its tab).  The source half of
-- 'moveLeafAcross' — also what a drop that empties a tmux window needs, since
-- tmux deletes the window under us and the leaf has nothing left to show.
withoutLeaf :: LeafId -> LeksahWindow -> Maybe LeksahWindow
withoutLeaf l lw = detachLeaf l (lwTree lw) <&> \t' -> lw
  { lwTree    = t'
  , lwPanes   = M.delete l (lwPanes lw)
  , lwFocused = if lwFocused lw == Just l
      then successorLeaf (/= l) l (lwTree lw) <|> listToMaybe (treeLeafIds t')
      else lwFocused lw
  , lwZoomed  = if lwZoomed lw == Just l then Nothing else lwZoomed lw
  }

-- | Merge every run of adjacent same-font tmux windows in one leksah
-- window's split tree into single tmux windows ('consolidateGroups'): the
-- first window of each run survives, the others' panes are moved into it,
-- and the combined tmux layout — each old window's internal layout re-fitted
-- to its leaf's share — is applied in one 'selectWindowLayout'.  Runs after
-- ⌘-drag drops, after leaf font changes (the complement of the font
-- convert's isolate-first step), and once over every window at session
-- restore, upholding the invariant: leksah splits exist only at font
-- boundaries and around view panes.  Serialized by 'consolidateLock'
-- (drop handlers, font changes and the restore sweep would otherwise
-- interleave surgery on one window); the tmux moves + the model edit run
-- under the conversion guard so the reconcile never sees the half-moved
-- state (a drained window dies with its last pane).  Skipped while zoomed —
-- restructuring under a zoom would yank the zoomed pane around.
consolidateLw :: Text -> IO ()
consolidateLw lwId = withMVar consolidateLock $ \_ ->
  getGlobalIDERef >>= mapM_ (\ideR -> do
    lws0 <- (`reflectIDE` ideR) (readIDE leksahWindows)
    defFont <- monospaceFontSize <$> (`reflectIDE` ideR) (readIDE prefs)
    case M.lookup lwId lws0 of
      Just lw | Nothing <- lwZoomed lw ->
        forM_ (consolidateGroups defFont (lwPanes lw) (lwTree lw)) $ \g ->
          case mgLeaves g of
            allLs@((survivorLeaf, survivorWin) : rest@(_ : _)) -> do
              mlays <- forM allLs $ \(l, w) ->
                  fmap ((,) l) . (parseWindowLayout =<<)
                    <$> windowLayoutString w
              case buildCombined . M.fromList =<< sequence mlays of
                Nothing -> return ()   -- a window vanished under the pass
                Just combined -> do
                  -- Where the keyboard is (the session's active pane):
                  -- re-selected below so focus can't drift.
                  mact <- maybe (return Nothing) activePaneId (lwSession lw)
                  beginConversion
                  forM_ rest $ \(_, w) ->
                      panesOfWindow w >>= mapM_ (`movePaneToPane` survivorWin)
                  -- A custom select-layout assigns its cells to the window's
                  -- panes by INDEX ORDER — the pane ids in the string are
                  -- informational — and move-pane splices arrivals next to
                  -- the target's active pane, so first force the pane order
                  -- to the combined layout's leaf order or every pane lands
                  -- in some other pane's cell.
                  actual <- panesOfWindow survivorWin
                  let sortSwaps _ [] = []
                      sortSwaps [] _ = []
                      sortSwaps (cur : acts) (want : wants)
                        | cur == want = sortSwaps acts wants
                        | otherwise   = (cur, want) : sortSwaps
                            (map (\p -> if p == want then cur else p) acts)
                            wants
                  forM_ (sortSwaps actual (cellPanes combined))
                        (uncurry swapTmuxPanes)
                  selectWindowLayout survivorWin (printWindowLayout combined)
                  -- Land tmux focus BEFORE the model edit: the focus
                  -- reconciler resolves the window's active pane when
                  -- lwFocused changes, and must already see the user's pane
                  -- active (selecting afterwards raced it, and losing that
                  -- race dropped the keyboard on the wrong pane).
                  forM_ mact selectTmuxPaneId
                  (`reflectIDE` ideR) $ modifyIDE_ $ leksahWindows %~
                    M.adjust (\lw' -> lw'
                      { lwTree    = collapseGroup (map fst allLs) (lwTree lw')
                      , lwPanes   = foldr (M.delete . fst) (lwPanes lw') rest
                      , lwFocused = case lwFocused lw' of
                          Just f | f `elem` map fst rest -> Just survivorLeaf
                          x -> x }) lwId
                  endConversion
                  -- The moved panes' xterms REBUILD under the survivor's
                  -- widget; the reconciler's bounded retry can expire
                  -- against the OLD (still-registered) xterm and drop the
                  -- keyboard on <body>.  The sticky focus request's ladder
                  -- (0.15–5s, gated on this being the on-screen terminal)
                  -- keeps trying until the keyboard actually lands.
                  focusTerminalPane lwId
              where
                buildCombined lays = build (mgTree g)
                  where
                    build (SplitLeaf l)    = M.lookup l lays
                    build (SplitNode o cs) = do
                        ks <- mapM (\(s, c) -> (,) s <$> build c) cs
                        pure (combineCells (o == SplitH) ks)
            _ -> return ()
      _ -> return ())

-- | Run 'consolidateLw' over every leksah window (a drop or restore can
-- create same-font adjacency anywhere, and the pass is idempotent and free
-- when nothing matches).
consolidateAll :: IO ()
consolidateAll = getGlobalIDERef >>= mapM_ (\ideR -> do
    lws <- (`reflectIDE` ideR) (readIDE leksahWindows)
    mapM_ consolidateLw (M.keys lws))


#if defined(ghcjs_HOST_OS)
-- | The JS backend has no sockets (and no Network.Socket import above); the
-- native no-op wrapper is reproduced here so 'newIDE' reads the same.
withSocketsDo :: IO a -> IO a
withSocketsDo = id

-- | No criterion-measurement on the JS backend (its cycle-counter cbits
-- don't build); nothing in the browser reads the timer it would initialise.
initializeTime :: IO ()
initializeTime = return ()

-- | No fsnotify on the JS backend (unix-compat doesn't build).  The stand-in
-- 'WatchManager' in IDE.Core.Types carries no state, and the JS branch of
-- IDE.Workspaces.Writer registers no watchers, so this just runs the body.
withManager :: (WatchManager -> IO a) -> IO a
withManager f = f NoWatchManager
#endif

-- | First 'Bool': whether to render the web menu bar (hidden for
-- @leksah-wkwebview@, which has a native macOS menu).  Second 'Bool':
-- develop-leksah mode — exit with code 2 when the leksah package is rebuilt in
-- the IDE, so a wrapper (leksah-nix.sh) can rebuild and relaunch.
newIDE :: Bool -> Bool -> Bool -> (JSM () -> IO ()) -> IO ()
newIDE showMenubar macTitlebar developLeksah runJs = do
  metaLog "boot: newIDE enter"
#if !defined(ghcjs_HOST_OS)
  -- Both ways this runs default to ONE capability — ghci (no way to hand the
  -- repl RTS options; see leksah.sh) and the compiled exe (-with-rtsopts has
  -- no -N) — so the reflex frame threads, the tmux readers and the boot-time
  -- metadata scan all time-share a single core: measured 48s from skeleton to
  -- first terminal content, all of it the (interpreted) metadata scan starving
  -- the frame thread.  Spread out at runtime instead; skip when -N was given
  -- explicitly (numCapabilities > 1) so a user override still wins.
  when rtsSupportsBoundThreads $ do
    caps <- getNumCapabilities
    when (caps == 1) $ do
      n <- getNumProcessors
      let caps' = min 8 (max 2 n)
      setNumCapabilities caps'
      metaLog ("boot: setNumCapabilities " <> show caps'
               <> " (of " <> show n <> " processors)")
#endif
  -- Before anything is forked: remember which threads pre-date this run, so a
  -- ghci teardown can reap exactly the ones this run creates (and never ghci's
  -- own).  This is what stops each :reload leaking the previous run's whole IDE
  -- state — see 'IDE.Web.GhciMode.killAppThreads'.
  recordPreRunThreads
  -- Browser-hosted (warp / web demo) exactly when the web menu bar shows;
  -- recorded process-globally for deeply nested readers (tab flip hints).
  setBrowserHosted showMenubar
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
#if defined(ghcjs_HOST_OS)
  -- In the browser stdout is BLOCK-buffered: putStrLn diagnostics sit in the
  -- buffer until it fills, so console ordering lies and a crash swallows the
  -- tail.  LineBuffering flushes each line as one console write (NoBuffering
  -- would emit one console line PER CHARACTER).
  IO.hSetBuffering IO.stdout IO.LineBuffering
#endif
  initializeTime
  exitCode <- newIORef ExitSuccess
  withSocketsDo $ do
#if defined(ghcjs_HOST_OS)
    -- Browser: no config dir and no data files to read — bake in the
    -- defaults (prefs, an empty candy table, an empty nix cache), tweaked
    -- for the demo: ⌘-held shortcut badges on (defaultPrefs has them off,
    -- and there is no prefs file to turn them on).
    let initPrefs = defaultPrefs { showShortcutBadges = True }
    withManager $ \fsnotify -> do
      triggerBuildVar <- newEmptyMVar
      let nixCache = mempty
#else
    dataDir         <- getDataDir

    prefsPath       <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
    initPrefs       <- readPrefs prefsPath
    metaLog "boot: prefs read"
    withManager $ \fsnotify -> do
      metaLog "boot: fsnotify started"

      triggerBuildVar <- newEmptyMVar
      nixCache <- loadNixCache
      metaLog "boot: nix cache loaded"
#endif
      externalModified <- newMVar mempty
      watchers <- newMVar (mempty, mempty)
      let ide = IDE
            {   _ideGtk            =   Nothing
            ,   _exitCode          =   exitCode
            ,   _prefs             =   initPrefs
            ,   _workspace         =   Nothing
            ,   _bufferProjCache   =   mempty
            ,   _allLogRefs        =   mempty
            ,   _currentHist       =   0
            ,   _currentEBC        =   (Nothing, Nothing, Nothing)
            ,   _handlers          =   mempty
            ,   _currentState      =   IsStartingUp
            ,   _recentFiles       =   []
            ,   _recentWorkspaces  =   []
            ,   _runningTool       =   Nothing
            ,   _debugState        =   []
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
            ,   _leksahWindows     =   mempty
            ,   _nextLeksahWin     =   0
            ,   _hiddenWindows     =   mempty
            ,   _activeWindow      =   Nothing
            ,   _nextWindowId      =   0
            ,   _flipMirror        =   Nothing
            ,   _flipMru           =   []
            ,   _paneAISession     =   mempty
            ,   _ideVersion        =   0
      }
      -- The trigger slot runs after every 'modifyIDEM', on the mutating thread:
      -- keep it down to the non-blocking resync signals (see 'notifyResync' —
      -- each window's own notifier thread then fires that window's coalesced
      -- resync).  NEVER put reflex trigger fires or JS in this slot.
      ideR <- liftIO $ newMVar (const notifyResync, ide)
      -- Leak probe (ghci mode): fires once this instance's IDE-state root — and
      -- so its whole object graph: workspace, panes, log refs, the reflex
      -- network hanging off them — has become unreachable, which should happen
      -- during the NEXT reload's teardown.  A reload cycle that never logs a
      -- @[gc]@ line for the previous boot is RETAINING that instance; that is
      -- the difference between "teardown frees it" and "the session grows by an
      -- instance per reload".  Deliberately built from compiled-base pieces
      -- only (a String and 'IO.hPutStrLn'): a finalizer that called back into an
      -- interpreted module would run reverted-CAF code after the reload.  The
      -- finalizer must not mention @ideR@ itself, or it would keep it alive.
      when ghciMode . liftIO $ do
        born <- getCurrentTime
        let msg = "LEK [gc] IDE state born " <> show born <> " collected"
        void $ mkWeakMVar ideR (IO.hPutStrLn IO.stderr msg)
      liftIO $ setGlobalIDERef ideR  -- so the native macOS menu can run commands
      -- Frontend↔backend bridge (UI-split stage 1, see IDE.Web.Bridge): here
      -- both halves share this RTS, so the seam is a direct in-process pair.
      -- The backend end serves the proof endpoints; the frontend end's are
      -- registered per window in 'jsMain'.
      liftIO $ do
        (feBr, beBr) <- newDirectBridge
        setFrontendBridge feBr
        setBackendBridge beBr
        exposeBackendProofEndpoints beBr
#if !defined(ghcjs_HOST_OS)
      liftIO $ startCmdServer ideR   -- control socket for the leksah-cmd CLI
#endif
      -- Single process-wide drains for the "act on the active window" bridges
      -- (close/save/find/prefs/open-file); each window's network registers its
      -- triggers via 'registerWindowBridge' and the drain routes to the frontmost.
      liftIO $ startWindowBridgeDrains ideR
#if !defined(ghcjs_HOST_OS)
      -- The one poll of the live Claude Code sessions, feeding every status
      -- surface: the in-page traffic light (each window pulls it on its tick,
      -- see 'statusLightJs') and, on macOS, the menu-bar status item (pushed —
      -- IDE.Web.MacMenu registers a handler).  Titles come from 'agentTitles',
      -- so those two and the Agents pane all name a session the way the agent
      -- named itself.  Killed at ghci teardown, or every :reload would leave
      -- another poll running.
      liftIO $ do
        tid <- startClaudeStatusPoll agentTitles
        when ghciMode $ registerGhciCleanupNamed "claude-status-poll" (killThread tid)
#endif
#if !defined(ghcjs_HOST_OS)
      -- Detach control-mode clients left over from previous runs BEFORE any
      -- terminal attaches: they wedge on leksah exit, stay counted as attached,
      -- and their stale 80x24 sizes clamp every window they're attached to.
      liftIO reapControlClients
#endif
#if !defined(ghcjs_HOST_OS)
      -- GC monitor (needs +RTS -T, set via -with-rtsopts): log every major
      -- collection with its pause to stderr, so UI hitches can be correlated
      -- with GC (or ruled out) by watching the leksah-nix.sh window.
      -- (JS backend: getRTSStatsEnabled has no JS-RTS shim.)
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
#endif
      -- Develop mode: rebuilding the leksah package in the IDE triggers
      -- QuitToRestart.  The Gtk front end handles that via its application; the
      -- web front ends have no such hook, so exit with code 2 and let the
      -- wrapper (leksah-nix.sh) rebuild and relaunch.
      -- Terminate language servers on a rebuild/restart.  In ghci mode this
      -- runs from 'stopForGhci' before the @:reload@ wipes the registry, so a
      -- fresh @:main@ no longer orphans the previous run's HLS/nixd processes
      -- (the pile-up: each rebuild spawned new ones and nothing killed the old,
      -- all under the long-lived ghci process).  NOT gated on develop mode —
      -- the ghci session leaks servers regardless of how it was launched.
      liftIO $ when ghciMode $ registerGhciCleanupNamed "lsp-servers" shutdownServers
      when developLeksah $ do
          liftIO . (`reflectIDE` ideR) . void $
              registerEvent ideR "QuitToRestart" $ \e -> do
                  -- `leksah-cmd rebuild-self --no-restart` (IDE-build path) arms
                  -- this so a successful self-build lands on disk without the
                  -- restart; the next QuitToRestart behaves normally.
                  suppress <- liftIO $ atomicModifyIORef' suppressNextRestart (\s -> (False, s))
                  if suppress
                    then liftIO $ putStrLn "leksah: QuitToRestart suppressed (rebuild-self --no-restart)"
                    -- ghci mode: never exit the process (it IS the ghci
                    -- session) — tear down and return to the prompt instead.
                    -- Handoff: the in-IDE build already ran, so hand off with no
                    -- rebuild and stay up until the successor is ready.
                    else liftIO $ if ghciMode then stopForGhci
                                  else if handoffEnabled then requestHandoff True
                                  else shutdownServers >> exitImmediately (ExitFailure 2)
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
                      if ghciMode then stopForGhci
                                  else if handoffEnabled then requestHandoff False
                                  else shutdownServers >> exitImmediately (ExitFailure 2)
#if defined(ghcjs_HOST_OS)
      -- The browser demo's workspace lives in the page-seeded mock tree
      -- (window.leksahDemoFiles → IDE.Web.FS).
      let filePath = "/demo/demo.lkshw"
#else
      let filePath = "/Users/hamish/leksah.lkshw"
#endif
      metaLog ("boot: reading workspace " <> filePath)
      liftIO $ (`reflectIDE` ideR) $
          catchIDE (
              Writer.readWorkspace filePath >>= \case
                  Left errorMsg -> liftIO $ putStrLn $ "Could not open " <> filePath <> ". " <> errorMsg
                  Right (ws, deferred) -> do
                        modifyIDE_ (workspace ?~ ws)
                        Writer.setWorkspace (Just $ ws & wsFile .~ filePath)
                        -- Remote (ssh://) projects are placeholders at this
                        -- point; fill them in behind the UI instead of making
                        -- startup (and every ghci reload) wait on ssh.
                        Writer.resolveDeferredProjects deferred
                      )
             (\ (e :: SomeException) ->
                  liftIO $ putStrLn $ printf (T.unpack $ __ "Can't load workspace file %s\n%s") filePath (show e))
      metaLog "boot: workspace read"
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
#if defined(ghcjs_HOST_OS)
        -- Browser demo: the canned sessions from the page (see
        -- IDE.Web.DemoTerminals) stand in for live tmux sessions.  Seed the
        -- leksah windows directly: a SESSIONLESS showcase window (the game's
        -- editor beside a browser pane running the compiled game — rendered
        -- by the shared 'sessionlessLwWidget', draggable with ⌘-drag), plus
        -- one single-pane window per canned session.  Owning the canned
        -- sessions here also keeps the reconcile's stray-adoption pass from
        -- minting a SECOND window (and tab) for each of them.
        liveTerms <- demoTerminals
        let showcase = LeksahWindow
              { lwSession = Nothing
              , lwTree    = SplitNode SplitH
                              [ (0.55, SplitLeaf (LeafId 0))
                              , (0.45, SplitLeaf (LeafId 1)) ]
              , lwPanes   = M.fromList
                  [ (LeafId 0, PaneContent
                      (PaneView (EditorKey "/demo/breakout/app/Main.hs")) Nothing)
                  , (LeafId 1, PaneContent (PaneView (BrowserKey 1)) Nothing) ]
              , lwFocused = Just (LeafId 0)
              , lwZoomed  = Nothing
              , lwNext    = 2 }
            termLws  = [ (lwIdText (i + 1), singlePaneWindow (Just sid)
                            (PaneContent (PaneTmux ("@" <> sid)) Nothing))
                       | (i, (sid, _)) <- zip [0 ..] liveTerms ]
            lws0     = M.fromList ((lwIdText 0, showcase) : termLws)
            nextSeed = 1 + length liveTerms
#else
        -- Leksah windows live ON their backing tmux sessions
        -- (@leksah_layout v2, one option per session); read them all in one
        -- call, then merge the SESSIONLESS ones (pure native views) from the
        -- web session file.  Reconciliation against each session's live
        -- windows happens later, once the pane tree has actually answered
        -- (never on "no data yet").
        (optLws, optNext) <- readLeksahWindows
        -- One-time migration cleanup: retire the idle backing-twin panes an
        -- older build left in tmux (their overlays no longer exist).
        void . forkIO $ cleanupStaleTwinPanes
        let sessionless = M.fromList
              [ (i, lw) | Just s <- [mbSession], Just lws <- [wsLeksahWindows s]
              , (i, lw) <- lws ]
            lws0     = M.union optLws sessionless   -- option wins a collision
            nextSeed = maximum (optNext
              : [ n + 1 | i <- M.keys lws0, Just n <- [lwIdNum i] ])
#endif
            -- Restored tab slots: LeksahWinKey tabs keep their place if the
            -- window survived the read; a v5 file's TerminalKey slot expands
            -- into that session's leksah windows (in id order).  Remote
            -- (ssh://) session tabs are session-lived and never restored, as
            -- before.  Sessions with live windows but no restored tab slot
            -- get tabs appended by the reconcile's tab sync.
            expandTab k = case k of
              LeksahWinKey n | n `M.member` lws0 -> [k]
                             | otherwise         -> []
              TerminalKey n  -> [ LeksahWinKey i
                                | (i, lw) <- M.toAscList lws0
                                , lwSession lw == Just n ]
              PreferencesKey -> []   -- transient, never restore
              ShortcutsKey   -> []   -- transient, never restore
              GitLogKey{}    -> []   -- transient, never restore
              ReviewKey{}    -> []   -- transient, never restore
              TasksKey       -> []   -- transient, never restore
              PlanKey{}      -> []   -- transient, never restore
              CompareKey{}   -> []   -- transient, never restore
              _              -> [k]
#if defined(ghcjs_HOST_OS)
            -- One tab per seeded leksah window, the showcase window first and
            -- active (there is no saved web session in the browser).  The
            -- page's auto-open of the game's Main.hs resolves to the showcase
            -- window's editor leaf (the file-lives-as-a-leaf intercept), so
            -- it focuses this tab rather than opening a duplicate editor.
            demoTabs = map LeksahWinKey (M.keys lws0)
            dfltWin  = WebWindowSession demoTabs (listToMaybe demoTabs)
                                        (tallVisibility initPrefs)
                                        (wide1Visibility initPrefs)
#else
            dfltWin  = WebWindowSession [] Nothing (tallVisibility initPrefs)
                                        (wide1Visibility initPrefs)
#endif
            wwsList  = case mbSession of
                         Just s | not (null (wsWindows s)) -> wsWindows s
                         _                                 -> [dfltWin]
            seeded   = M.fromList
              [ (WindowId i
                , WebWindow (nub (concatMap expandTab (wwsWide0 w)))
                            (wwsActive w >>= listToMaybe . expandTab)
                            (wwsTall w) (wwsWide1 w) Nothing)
              | (i, w) <- zip [0 ..] wwsList ]
            nWins    = length wwsList
        metaLog $ "boot: session + leksah windows read (" <> show nWins <> " window(s))"
        (`reflectIDE` ideR) $ modifyIDE_ $ \i ->
          i & webWindows .~ seeded & nextWindowId .~ nWins & activeWindow ?~ WindowId 0
            & leksahWindows .~ lws0
            & nextLeksahWin .~ nextSeed
            -- Restore the flipper MRU (buildFlipItems filters out any entries
            -- whose tab/pane no longer exists, so stale ones are harmless).
            & flipMru .~ fromMaybe [] (mbSession >>= wsFlipMru)
            -- Restore the per-pane AI-session bindings.  Entries for panes that
            -- are gone are pruned as panes close; entries naming a session that
            -- has since exited are KEPT (they get resumed on next use).
            & paneAISession .~ M.fromList (fromMaybe [] (mbSession >>= wsPaneAI))
        -- Ask native to create the windows past the first (the first is created
        -- by the wkwebview AppDelegate / warp connection and attached below).
        -- No-op on warp (no handler), which stays single-window.
        metaLog $ "boot: requesting native windows 1.." <> show (nWins - 1)
        mapM_ requestOpenWindow [1 .. nWins - 1]
      metaLog "boot: entering runJs (window 0 UI)"
      runJs $ jsMain showMenubar macTitlebar (Just (WindowId 0)) ideR

#if defined(ghcjs_HOST_OS)
-- | Entry point of the JS-backend front end (src-ghcjs/Main.hs): the shared
-- 'newIDE' → 'jsMain' pipeline, run directly against the hosting page's DOM.
-- Web menu bar on (there is no native menu), mac title bar off, develop mode
-- off (there is no wrapper loop to relaunch us).
browserMain :: IO ()
browserMain = newIDE True False False (JSW.run leksahPort)
#else
develMain :: IO ()
develMain = do
  dev <- elem "--develop-leksah" <$> getArgs
  newIDE True False dev (debugJSaddle leksahPort)

-- | Mode-B proof of the UI split (see IDE.Web.Bridge): serve the ghcjs demo
-- page in SPLIT mode — the page runs the frontend half compiled with the GHC
-- JS backend, while THIS native process runs only the backend half of the
-- bridge inside each connecting page's jsaddle context (no jsMain — the
-- frontend owns the DOM).  `leksah-warp --split-frontend`.
splitFrontendMain :: IO ()
splitFrontendMain = do
  dataDir <- getDataDir
  let site = dataDir </> "docs" </> "website"
  IO.hPutStrLn IO.stderr $
    "leksah split-frontend backend on http://127.0.0.1:" <> show leksahPort
    <> "/ (site root " <> site <> ")"
  runSettings (setPort leksahPort (setTimeout 3600 defaultSettings)) =<<
    jsaddleOr defaultConnectionOptions
              (bridgeBackendMain >> syncPoint)
              (\req sendResponse ->
        case (W.requestMethod req, W.pathInfo req) of
            -- The split page (the ghcjs demo page + the split flag +
            -- jsaddle.js) — served at the root so its root-absolute asset
            -- paths (/cm6, /pics, …) resolve against this server.
            ("GET", []) -> do
                 html <- LBS.readFile (site </> "try" </> "index-split.html")
                 sendResponse
                    $ W.responseLBS H.status200
                        [("Content-Type", "text/html; charset=utf-8")]
                      html
            ("GET", ["jsaddle.js"]) ->
                 sendResponse
                    $ W.responseLBS H.status200
                        [("Content-Type", "application/javascript")]
                    $ jsaddleJs False
            _ -> staticApp (defaultWebAppSettings site) req sendResponse)

-- | The backend half's per-page-context entry: attach the backend bridge
-- end to the page's @window.leksahBridge@, expose the proof endpoints, and
-- (off the jsaddle thread) handshake and push one notification across.
bridgeBackendMain :: JSM ()
bridgeBackendMain = do
  beBr <- newJsBridge BackendSide
  liftIO $ do
    setBackendBridge beBr
    exposeBackendProofEndpoints beBr
    void . forkIO $ do
      r <- helloHandshake beBr (HelloInfo bridgeProtocolVersion "native-backend" [])
      IO.hPutStrLn IO.stderr $ "[Bridge] backend handshake: " <> show r
      case r of
        Right _ -> do
          -- give the frontend's widget postBuild a moment to expose its
          -- showNotification endpoint, then prove backend→frontend push
          threadDelay 2000000
          nr <- call beBr "showNotification" [BText "hello from the native backend"]
          IO.hPutStrLn IO.stderr $ "[Bridge] backend→frontend notify: " <> show nr
        Left _ -> return ()
#endif

-- | The backend half's stage-1 proof endpoints, identical in dev (direct
-- bridge in 'newIDE') and split mode ('bridgeBackendMain').
exposeBackendProofEndpoints :: Bridge -> IO ()
exposeBackendProofEndpoints beBr = do
  exposeJSON beBr "echo" (return :: Value -> IO Value)
  expose beBr "fsReadFile" $ \case
    [BText fp] -> BBytes <$> fsReadFile (T.unpack fp)
    _ -> ioError (userError "fsReadFile: expected one path argument")

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

-- | The user's pick from the ⌘W terminal pane close menu.
data TermCloseChoice = TCKill | TCHide | TCMove | TCCancel deriving Eq

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

-- | The leksah window OWNING tmux window index @widx@ of session @s@: the
-- tree provides the index→@id mapping, the lw map the ownership.  'Nothing'
-- for windows no lw holds (a just-created stray for one poll tick, remote
-- sessions).  With several leksah windows on one session this is what gives
-- each its own flipper/tab identity — a pane promotes/selects ITS tab, not
-- the session's first.
paneOwnerLw :: Map Text LeksahWindow -> Map Text (Text, [TmuxWindow]) -> Text -> Int -> Maybe Text
paneOwnerLw lws tree s widx = do
    (_, wins) <- M.lookup s tree
    wid' <- twId <$> find ((== widx) . twIndex) wins
    listToMaybe [ i | (i, lw) <- M.toAscList lws
                    , lwSession lw == Just s
                    , wid' `elem` lwWindowIds lw ]

-- | The (leksah window, leaf) holding a 'FlipPane' target's tmux window: the
-- flip's window INDEX resolved to a window id in the tree, then to the leaf
-- that owns it.  Used by the flip commit to write 'lwFocused' up front.
flipPaneLeaf :: Map Text LeksahWindow -> Map Text (Text, [TmuxWindow]) -> Text -> Int -> Maybe (Text, LeafId)
flipPaneLeaf lws tree s widx = do
    (_, wins) <- M.lookup s tree
    wid' <- twId <$> find ((== widx) . twIndex) wins
    windowOwner wid' lws

-- | The lw's focused tmux window (or its first one, when the focused leaf is
-- a view pane), resolved in the tree.  What a leksah-window tab's icon and
-- ⌘1…P pane numbering key on.
lwFocusedWindow :: Map Text (Text, [TmuxWindow]) -> LeksahWindow -> Maybe TmuxWindow
lwFocusedWindow tree lw = do
    s <- lwSession lw
    (_, wins) <- M.lookup s tree
    let wids = [ w | l <- maybe [] (:[]) (lwFocused lw) ++ treeLeafIds (lwTree lw)
                   , Just (PaneContent (PaneTmux w) _) <- [M.lookup l (lwPanes lw)] ]
    listToMaybe [ win | w <- wids, win <- wins, twId win == w ]

-- | Which OS window a flip item currently lives in (the window whose wide0 owns
-- its tab key), if any.  A pane belongs to the window showing its OWNING leksah
-- window when the tree resolves one (falling back to any of its session's tabs,
-- or its remote TerminalKey tab).  Shared side/bottom tabs and panes not open
-- in any window's wide0 have no owner ('Nothing').
flipOwnerWindow :: Map Text LeksahWindow -> Map Text (Text, [TmuxWindow]) -> Map WindowId WebWindow -> FlipItem -> Maybe WindowId
flipOwnerWindow lws tree wins fi =
  let ks = case fi of
        FlipTab t      -> [t]
        FlipView n _   -> [LeksahWinKey n]
        FlipPane s w _ -> case paneOwnerLw lws tree s w of
          Just i  -> [LeksahWinKey i]
          Nothing -> TerminalKey s
            : [ LeksahWinKey i | (i, lw) <- M.toAscList lws, lwSession lw == Just s ]
  in listToMaybe [ w | (w, ww) <- M.toList wins, any (`elem` _wwWide0 ww) ks ]

-- | The view 'TabKey' a 'FlipView' item points at — the lw's leaf must still
-- hold a native view ('Nothing' once either is gone).
viewKeyOf :: Map Text LeksahWindow -> Text -> Int -> Maybe TabKey
viewKeyOf lws n l = do
  lw <- M.lookup n lws
  pc <- M.lookup (LeafId l) (lwPanes lw)
  case pcKind pc of
    PaneView k -> Just k
    _          -> Nothing

jsMain :: Bool -> Bool -> Maybe WindowId -> IDERef -> JSM ()
jsMain showMenubar macTitlebar mbWid ideR = do
  -- enableLogging True -- Uncomment this to add verbose JSaddle logging
  metaLog $ "boot: jsMain enter " <> show mbWid
#if !defined(ghcjs_HOST_OS)
  dataDir <- liftIO getDataDir
#endif
  ctx <- askJSM
  -- Resolve this connection's window identity: adopt the id the native side
  -- pre-created (restore / New Window), or mint one (the first wkwebview window
  -- and every leksah-warp browser connection).
  wid <- liftIO $ maybe (mintWindowId ideR) (\w -> adoptWindowId w ideR >> return w) mbWid
  -- Tag this context with its window id, so tooling (leksah-cmd js eval, which
  -- broadcasts to every context) can tell the windows apart.
  _ <- eval ("window.leksahWindowId = " <> T.pack (show (case wid of WindowId n -> n)))
  metaLog $ "boot: jsMain JS bridge live " <> show wid
#if defined(ghcjs_HOST_OS)
  -- Split-bridge mode (UI-split stage 1, see IDE.Web.Bridge): the hosting
  -- page set window.leksahSplitBridge, meaning a NATIVE backend shares this
  -- page through jsaddle.  Replace the in-process frontend end 'newIDE' made
  -- with a JS-glue end (before any widget registers endpoints on it), then —
  -- off this thread — handshake and prove a frontend→backend round trip.
  split <- valToBool =<< eval ("!!window.leksahSplitBridge" :: Text)
  when split $ do
    feBr <- newJsBridge FrontendSide
    liftIO $ do
      setFrontendBridge feBr
      void . forkIO $ do
        hr <- helloHandshake feBr (HelloInfo bridgeProtocolVersion "ghcjs-frontend" [])
        IO.hPutStrLn IO.stderr $ "[Bridge] frontend handshake: " <> show hr
        er <- callJSON feBr "echo" (A.String "hello from the ghcjs frontend")
        IO.hPutStrLn IO.stderr $
          "[Bridge] echo across the split: " <> show (er :: Either BridgeError Value)
  -- JS backend: there is no datadir (and no filesystem) to read the bundles
  -- from — the hosting page loads cm6/leksah-cm6.js, xterm.js and its addons
  -- via <script> tags BEFORE the compiled leksah starts, so window.LeksahCM /
  -- Terminal / FitAddon etc. already exist here.  The xterm CSS however must
  -- come through mainWidgetWithCss like it does natively (page-seeded text,
  -- window.leksahXtermCss), NOT via a page <link>: mainWidgetWithCss rebuilds
  -- <head> with the app's own <style>, dropping any page stylesheet link —
  -- the terminals then render unstyled (.xterm-screen falls to static flow
  -- far below its pane, xterm's IntersectionObserver reports it off-screen
  -- and PAUSES rendering, and the pane shows empty).
  xtermCss <- encodeUtf8 <$> (valToText =<< eval ("window.leksahXtermCss || ''" :: Text))
#else
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
  -- Inline images rendered into the scrollback — SIXEL, iTerm2 OSC 1337 and the
  -- kitty graphics protocol, so img2sixel/chafa/imgcat/icat just work.  A
  -- program in a tmux pane wraps them in tmux's DCS passthrough
  -- (allow-passthrough, see ReplTmux.writeTmuxConf), which IDE.Web.KittyGraphics
  -- unwraps on the way in — that module also serves the transmission media the
  -- addon can't (kitty's t=s shared memory, t=f files).
  _ <- liftIO (readFile $ dataDir </> "xterm/addon-image.js") >>= eval
  -- OSC 52: programs in a terminal (vim/tmux copy-mode, incl. over ssh where
  -- pbcopy can't reach) set the system clipboard.
  _ <- liftIO (readFile $ dataDir </> "xterm/addon-clipboard.js") >>= eval
#endif
  metaLog "boot: editor + xterm bundles eval'd"

  -- Makes project-file paths in terminal output Ctrl-clickable (window.LeksahTermLinks).
  _ <- eval terminalLinksJs
  -- Routes clicks on the links in an Agents-pane description (window.LeksahAgentLinks).
  _ <- eval agentLinksJs
  _ <- eval (badgesJs showMenubar)

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

  -- Defines window.LeksahKitty, the filter LeksahTerm.write feeds terminal
  -- output through: it unwraps tmux's DCS passthrough (which a control-mode
  -- pane receives verbatim, so xterm would drop it) and serves the kitty
  -- transmission media the image addon can't (data passed by reference).
  _ <- eval kittyGraphicsJs
  -- ... plus window.LeksahImageBytes, how that filter gets at data a program
  -- passed BY REFERENCE (kitty's t=s shared memory / t=f file transmission),
  -- which is nothing a browser engine can read for itself.
  installImageBytesBridge

  -- Defines window.LeksahJsaddlePane: the per-pane iframe registry of the
  -- jsaddle-terminal tunnels (see IDE.Web.Widget.TerminalCC) — batches go INTO
  -- an iframe via postMessage, and one page-level message listener routes each
  -- iframe's ready/results messages back to its registered Haskell callback.
  _ <- eval jsaddlePaneJs

  -- Defines window.LeksahDividerDrag: drag-to-resize for the CC pane dividers
  -- (the drag itself runs in JS — jsaddle dispatches events asynchronously,
  -- far too laggy for mousemove; Haskell is called back once, on drop).
  _ <- eval dividerDragJs

  -- Defines window.LeksahGitLogSplit: drag-to-resize the git log viewer's panes
  -- (live in JS, for the same async-dispatch reason as the divider drag above).
  _ <- eval gitLogSplitJs

  -- Defines window.LeksahPaneDrag: drag a Terminals-tree pane row onto a window
  -- row to move the pane there (the DnD gesture runs in JS — jsaddle can't do
  -- the synchronous dragover preventDefault a drop needs; see IDE.Web.Widget.Terminals).
  _ <- eval paneDragJs

  -- The ⌘-drag pane move: gesture + drop-target preview live in JS (60fps
  -- mousemove can't round-trip jsaddle); Haskell hears one leksahLeafDrop.
  _ <- eval leafDragJs

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

  -- Drag-to-resize the side (tall) and bottom (wide1) panes by their divider
  -- edge handles; the widths persist in localStorage across reloads.
  _ <- eval resizeBarsJs
  _ <- eval nativeDragJs

  -- Esc collapses the auto-hidden side/bottom bar when focus is inside it.
  _ <- eval escAutoHideJs

  -- window.leksahFlipMirror/Hide: the global flipper mirror overlay (a copy of
  -- another window's open flipper), driven by ideJSM_ broadcasts from main.
  _ <- eval flipMirrorJs

  -- The ⌘W close-menu highlight hooks (the active-pane ring itself is pure
  -- model-driven CSS on the per-pane chrome — see terminalCss).
  _ <- eval paneHlJs
  _ <- eval hintsJs

  -- window.leksahSetColorIcons: swap every SVG icon under /pics between the
  -- monochrome default and the coloured set under /pics/color, per the pref.
  -- (No glob spellings in comments here: CPP reads slash-star as a comment.)
  _ <- eval colorIconsJs

  -- window.leksahRetheme + the matchMedia listener: switch the Monaco/CodeMirror
  -- editors and xterm terminals between their light and dark themes with the OS.
  _ <- eval themeSwitchJs

  -- Keep context menus inside the viewport (they're placed at the click point).
  _ <- eval contextMenuClampJs

  -- The Claude-coordination traffic light (top-right dot): leksahTestStart /
  -- leksahTestEnd / leksahStatus, driven over the cmd socket via `js eval`.
  _ <- eval statusLightJs

  -- Native browser panes: report every .browser-native pane's rect+visibility
  -- to the ObjC glue, which overlays a real WKWebView on each (wkwebview
  -- front end only; elsewhere no such elements exist and this is dormant).
  _ <- eval browserNativeReporterJs

  -- window.leksahSelectRegion: the permission-free region picker for grab-region.
  _ <- eval regionSelectJs

  -- The colour palette (all --leksah-* tokens, dark + light) goes in first, so
  -- every stylesheet below resolves them; see "IDE.Web.Theme".
  metaLog "boot: helper JS eval'd, entering mainWidgetWithCss"
  mainWidgetWithCss (BS.unlines [xtermCss, encodeUtf8 paletteCss, encodeUtf8 contrastCss, BS.toStrict (LT.encodeUtf8 css)]) $ mdo
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
      -- Leave the start-up state (commands gate on it).  The metadata load that
      -- used to happen here is soft-deleted behind the `metadata` cabal flag.
      performEvent_ $ ffor pb $ \_ -> do
        wlog wid "ENTER pb-initInfo"
        (liftIO . (`reflectIDE` ideR) $ do
          -- Only the FIRST window to leave IsStartingUp would have loaded
          -- metadata; the atomic flip is still how every window agrees the app
          -- is running.
          firstToRun <- modifyIDE $ \i ->
              ( i & currentState .~ IsRunning
              , case i ^. currentState of IsStartingUp -> True; _ -> False )
#ifdef LEKSAH_METADATA
          if firstToRun
            then do metaLog $ "post-build " <> show wid <> " -> initInfo"
                    initInfo (return ())
                    metaLog $ "post-build " <> show wid <> " initInfo returned (load forked)"
            else return ()
#else
          metaLog $ "post-build " <> show wid <> " firstToRun=" <> show firstToRun
#endif
          )
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
      -- Freeze detector: a coarse (5s) keepalive per window.  If a window stops
      -- emitting "alive" lines, its reflex network has frozen.  Doubles as the
      -- resync fallback: it feeds the same version guard, so a lost resync signal
      -- self-heals within a few seconds (resync normally fires via WindowBridge
      -- events — this timer is only the backstop, so a slow tick is fine and
      -- keeps the idle window from waking every second).
      heartbeatTick <- tickLossyFromPostBuildTime 5
      heartbeatE <- performEvent $ ffor heartbeatTick $ \_ -> liftIO (snd <$> readMVar ideR)
      let freshPolledE = attachWithMaybe
            (\cur new -> if new ^. ideVersion > cur ^. ideVersion then Just new else Nothing)
            (current ideD) (leftmost [polledIdeE, heartbeatE])
      performEvent_ $ ffor (attach (current ideD) heartbeatE) $ \(cur, new) -> do
          -- Label this window's frame thread (processAsyncEvents) so a
          -- `leksah-cmd threads` dump can tell the windows apart, and raise its
          -- OS-thread priority so UI reactivity keeps CPU when background
          -- compilations saturate the machine.  This handler runs ON the frame
          -- thread (reflex processes a network's effects on its single frame
          -- thread), so the priority sticks to the right OS thread.  Both are
          -- idempotent; the 5s cadence makes the repeat cost negligible.
          liftIO $ do
              myThreadId >>= (`labelThread` ("reflex-frames-" <> show wid))
              raiseCurrentThreadPriority Interactive
          wlog wid ("alive ideVer=" <> show (cur ^. ideVersion) <> " mvarVer=" <> show (new ^. ideVersion)
                    <> if new ^. ideVersion > cur ^. ideVersion then " STALE(+" <> show (new ^. ideVersion - cur ^. ideVersion) <> ")" else "")
      performEvent_ $ ffor freshPolledE $ \i -> wlog wid ("ideD<-resync ver=" <> show (i ^. ideVersion))
#if !defined(ghcjs_HOST_OS)
      -- The top-right traffic light shows what the live Claude sessions are
      -- doing — the same states the macOS menu-bar status item draws, off the
      -- same poll ('IDE.Web.ClaudeStatus'), so the two can't disagree.
      -- PULLED here, per window, on this window's own tick: a background thread
      -- pushing JS into every window's context is exactly what wedges
      -- jsaddle-wkwebview, so each network reads the shared value itself.  The
      -- IORef read is free; only a real change reaches the page.
      claudeStatusTick <- tickLossyFromPostBuildTime 3
      claudeStatusE <- performEvent $
          ffor (leftmost [() <$ pb, () <$ claudeStatusTick]) $ \_ ->
              liftIO claudeStatusNow
      claudeStatusD <- holdUniqDyn =<< holdDyn emptyClaudeStatus claudeStatusE
      performEvent_ $ ffor (updated claudeStatusD) $ \st ->
          liftJSM . void $ jsg ("window" :: Text)
            ^. js3 ("leksahClaudeStatus" :: Text) (csState st) (claudeStatusTooltip st)
                  (csCount st)
#endif
      ideD <- holdDyn newIde $ leftmost [pbIde, freshPolledE]
      return ()
  liftIO $ threadDelay 1000000000

#if !defined(ghcjs_HOST_OS)
-- (A list rather than a multi-line string gap: CPP splices the gap's
-- backslash-newlines, mangling the literal.)
indexHtml :: ByteString
indexHtml = BS.unlines
    [ "<!DOCTYPE html>"
    , "<html>"
    , "<head>"
    , "<title>JSaddle</title>"
    , "</head>"
    , "<body>"
    , "</body>"
    , "</html>"
    ]
#endif

-- | All files in the workspace, for the find bar's workspace-tree search.  Uses
-- @git ls-files@ (tracked + untracked, respecting .gitignore — matching what
-- the tree shows) per package directory, falling back to a recursive walk for
-- non-git directories.
enumerateWorkspaceFiles :: Bool -> Bool -> [FilePath] -> IO [FilePath]
#if defined(ghcjs_HOST_OS)
-- Browser demo: enumerate the mock tree (window.leksahDemoFiles).  Feeds the
-- find bar AND LeksahTermLinks.setProjectFiles — the file set terminal
-- output links/hovers resolve against.
enumerateWorkspaceFiles _ _ dirs =
    nub . concat <$> mapM fsListFilesRecursive (nub dirs)
#else
enumerateWorkspaceFiles showHidden showIgnored dirs =
    nub . concat <$> mapM enumDir (nub dirs)
  where
    enumDir dir
      -- Remote project dir: `git ls-files` over ssh (one round trip,
      -- respects .gitignore like the local path), falling back to one
      -- `find`.  Errors mean an empty list, not a hang.
      | isRemotePath dir =
          (do (rc, out, _) <- runGit dir (map T.pack gitArgs)
              case rc of
                ExitSuccess -> return $ withDirs dir
                    [ l | l <- lines (T.unpack out), not (null l), notHidden l ]
                _ -> filter notHiddenAbs <$> fsListFilesRecursive dir)
            `catch` \(_ :: SomeException) -> return []
      | otherwise = gitFiles dir `catch` \(_ :: SomeException) -> walkFiles dir
    notHiddenAbs p = showHidden || not ("/." `isInfixOf` p)
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
#endif

#if !defined(ghcjs_HOST_OS)
startJSaddle :: Int -> (ByteString -> ByteString -> JSM () -> IO ()) -> JSM () -> IO ()
startJSaddle p runJs jsm = do
  dataDir <- getDataDir
  -- Normally serve on the fixed port p; but p<=0 (a handoff successor — see
  -- IDE.Web.Handoff) means "pick a free loopback port", which we must bind
  -- ourselves so we know it for the WKWebView base URL (warp's setPort 0
  -- wouldn't report the chosen port back).
  (actualPort, runServer) <-
    if p > 0
      then return (p, \app -> runSettings (setPort p (setTimeout 3600 defaultSettings)) app)
      else do
        s <- socket AF_INET Stream defaultProtocol
        setSocketOption s ReuseAddr 1
        bind s (SockAddrInet 0 (tupleToHostAddress (127,0,0,1)))
        listen s 1024
        pn <- socketPort s
        return (fromIntegral pn, \app -> runSettingsSocket (setTimeout 3600 defaultSettings) s app)
  -- ghci mode: killing the warp thread closes the listener (warp brackets the
  -- bind), so a fresh :main after :reload can rebind it.
  warpTid <- forkIO $ runServer =<<
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
  when ghciMode $ registerGhciCleanupNamed "jsaddle-warp-server" (killThread warpTid)
  metaLog $ "boot: startJSaddle server up on port " <> show actualPort <> ", handing to front end"
  runJs indexHtml ("http://127.0.0.1:" <> encodeUtf8 (T.pack $ show actualPort)) jsm

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
#endif

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
        background bgColor
        color fgColor
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
    aiPickerCss
    terminalCss
    terminalsCss
#ifdef LEKSAH_METADATA
    metadataCss
#endif
    agentsCss
    changesCss
    gitLogCss
    reviewCss
    tasksCss
    planCss
    compareCss
    preferencesCss
    shortcutsCss
    browserCss
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
  -- The pane is the tmux hierarchy (sessions ▸ windows ▸ panes), and the label
  -- says so: "Tmux", not "Terminals" — which read as a list of terminal tabs.
  TerminalsKey   -> "Tmux"
  TerminalKey n  -> M.findWithDefault n n names
  LeksahWinKey n -> n   -- tab buttons label leksah windows themselves
                        -- (session name / first view); this is a fallback
  MetadataKey    -> "Metadata"
  AgentsKey      -> "Agents"
  ChangesKey     -> "Changes"
  PreferencesKey -> "Preferences"
  ShortcutsKey   -> "Shortcuts"
  BrowserKey n   -> "Browser " <> T.pack (show n)
  GitLogKey _ b  -> "Log: " <> b
  ReviewKey d    -> "Review: " <> T.pack (takeFileName (dropTrailingPathSeparator d))
  TasksKey       -> "Claude Tasks"
  PlanKey d _    -> "Plan: " <> T.pack (takeFileName (dropTrailingPathSeparator d))
  CompareKey d _ -> "Compare: " <> T.pack (takeFileName (dropTrailingPathSeparator d))
  EditorKey file -> T.pack (takeFileName file)

-- | The leading B&W icon (an SVG path under @/pics@) for a side-pane tree tab, or
-- 'Nothing' for tabs shown by their label alone (editors, terminals, …).
tabIconSrc :: TabKey -> Maybe Text
tabIconSrc k = case k of
  WorkspaceKey   -> Just "/pics/workspace.svg"
  TerminalsKey   -> Just "/pics/terminals.svg"
  MetadataKey    -> Just "/pics/metadata.svg"
  AgentsKey      -> Just "/pics/tree-claude.svg"
  ErrorsKey      -> Just "/pics/errors.svg"
  LogKey         -> Just "/pics/log.svg"
  GrepKey        -> Just "/pics/grep.svg"
  ChangesKey     -> Just "/pics/changes.svg"
  ShortcutsKey   -> Just "/pics/shortcuts.svg"
  BrowserKey{}   -> Just "/pics/browser.svg"
  GitLogKey{}    -> Just "/pics/tree-git.svg"
  ReviewKey{}    -> Just "/pics/tree-git.svg"
  TasksKey       -> Just "/pics/tree-claude.svg"
  PlanKey{}      -> Just "/pics/tree-claude.svg"
  CompareKey{}   -> Just "/pics/tree-claude.svg"
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

-- | The flip item representing a whole session — its active (or first) window's
-- active (or first) pane, from the tree.  Used to float a terminal to the MRU
-- front when we know only its session (e.g. its connection dropped and the
-- Retry page appeared); 'Nothing' when the session isn't in the tree.
flipForSession :: Text -> Map Text (Text, [TmuxWindow]) -> Maybe FlipItem
flipForSession s tree = do
  (_, wins) <- M.lookup s tree
  w <- listToMaybe (filter twActive wins ++ wins)
  p <- listToMaybe (filter tpActive (twPanes w) ++ twPanes w)
  pure (FlipPane s (twIndex w) (tpIndex p))

-- | The flip item for a Terminals-tree selection of a session ('Nothing'
-- window), a window ('Just' window, 'Nothing' pane) or an exact pane, resolved
-- against the tree.  A given pane index is trusted; otherwise the selected
-- window's (or the session's active/first window's) active-or-first pane is
-- used.  'Nothing' when the session/window isn't in the tree (e.g. a remote
-- terminal not open yet).
flipForSel :: Text -> Maybe Int -> Maybe Int -> Map Text (Text, [TmuxWindow]) -> Maybe FlipItem
flipForSel s mw mp tree = do
  (_, wins) <- M.lookup s tree
  w <- case mw of
         Just wi -> find ((== wi) . twIndex) wins
         Nothing -> listToMaybe (filter twActive wins ++ wins)
  pidx <- case mp of
            Just p  -> Just p
            Nothing -> tpIndex <$> listToMaybe (filter tpActive (twPanes w) ++ twPanes w)
  pure (FlipPane s (twIndex w) pidx)

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
    winName = maybe (T.pack (show w)) windowTabLabel mbWin
    paneName = case [ stripIdx p (tpLabel pn) | pn <- panes, tpIndex pn == p ] of
                 (l:_) -> l
                 []    -> T.pack (show p)

-- | The leading icon (an SVG path under @/pics@) for a flipper entry, or 'Nothing'
-- for entries shown by label alone.  A terminal — whether the item is a whole
-- terminal tab or one of its panes (which is all a terminal flip item ever is) —
-- gets its tmux *window* icon, carrying the same fill/colour alert state the
-- Terminals tree shows (bell/activity/…); a file tab gets its file-type icon;
-- the side-pane tabs reuse 'tabIconSrc'.  The state-carrying window icon means a
-- belled terminal is spottable in the flipper at a glance.
-- ('windowActivePane', 'isClaudeWindow', 'windowIconSrc' and 'windowTabLabel'
-- moved to IDE.Web.Widget.Terminals so the stack tile rows inside the native
-- split leaves — rendered by IDE.Web.Widget.TerminalCC — share them.)

-- (The ⌘D pane-overlay icon lookup — 'flipOverlayKey' — lived here until the
-- native split layouts retired the overlays.)

flipIconSrc :: Map Text LeksahWindow -> Map Text (Text, [TmuxWindow]) -> FlipItem -> Maybe Text
flipIconSrc lws tree fi = case fi of
        FlipPane n w _          -> Just (winIcon n w)
        FlipTab (TerminalKey n) -> Just (winIcon n (activeWinIdx n))
        FlipTab k               -> tabIconSrc k
        FlipView n l            -> viewKeyOf lws n l >>= tabIconSrc
  where
    winIcon n w = case M.lookup n tree >>= find ((== w) . twIndex) . snd of
        Just win -> windowIconSrc win
        Nothing  -> "/pics/tree-window-idle.svg"
    activeWinIdx sid = maybe 0 twIndex $ M.lookup sid tree
        >>= (\wins -> listToMaybe (filter twActive wins ++ wins)) . snd

-- ('stripIdxPrefix' moved to IDE.Web.Widget.Terminals with the tile helpers.)

-- | A CSS @order@ style attribute for a wide0 tab button (empty off wide0).
orderStyle :: Maybe Int -> Map Text Text
orderStyle = maybe mempty (\n -> "style" =: ("order:" <> T.pack (show n)))

-- | The side- and bottom-bar panes in their strips' order: the Nth entry is
-- what ⌥⌘N / ⌃⌘N navigates to, and what its ⌘-held badge shows.
numberedTallTabs, numberedWide1Tabs :: [TabKey]
numberedTallTabs  = [WorkspaceKey, AgentsKey, TerminalsKey]
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
            hosted <- liftIO getBrowserHosted
            elAttr "span" ("class" =: "leksah-flip-suffix") $ text (flipHintText hosted)

-- | The wide0 tab-button order, taken from the flipper's item list: each tmux
-- window (identified by @Left (session, window)@, collapsed from its panes) and
-- each non-terminal tab (@Right key@), in the flipper's MRU-first order, one
-- entry apiece.  Windows of one session are NOT grouped — each is ordered
-- independently, exactly as the flipper cycles panes.  (The list isn't always
-- perfectly current, as noted for the flipper MRU, but it's close.)
buttonOrderOf :: (Text -> Int -> Maybe Text) -> [(Text, FlipItem)] -> [Either (Text, Int) TabKey]
buttonOrderOf owner = nub . map (flipButtonId owner . snd)

-- | The wide0 button identity of a flip item, given the pane→lw resolver
-- ('paneOwnerLw' applied to the current maps): a tmux pane collapses to its
-- OWNING LEKSAH WINDOW's tab — several leksah windows on one session each
-- have their own button, and a pane promotes only its own — falling back to
-- the session's @TerminalKey@ (remote tabs, unadopted strays); a non-terminal
-- tab is @Right key@.  This keys CSS order, ⌘-badges and flip hint chips.
flipButtonId :: (Text -> Int -> Maybe Text) -> FlipItem -> Either (Text, Int) TabKey
flipButtonId owner (FlipPane s w _) =
    Right (maybe (TerminalKey s) LeksahWinKey (owner s w))
flipButtonId _ (FlipView n _) = Right (LeksahWinKey n)
flipButtonId _ (FlipTab k) = Right k

-- | Collapse a wide0 flip-item list to one @(identity, item)@ per button,
-- keeping the first (MRU-most) item for each identity — the representative fired
-- when its ⌘-shortcut is pressed, and the head is the active (shown) button.
dedupButtons :: (Text -> Int -> Maybe Text) -> [(Text, FlipItem)] -> [(Either (Text, Int) TabKey, FlipItem)]
dedupButtons owner = go []
  where
    go _ [] = []
    go seen ((_, fi) : rest)
      | i `elem` seen = go seen rest
      | otherwise     = (i, fi) : go (i : seen) rest
      where i = flipButtonId owner fi

-- | The @data-flipkey@ string a tab button carries, so a published flip target
-- (see 'hintTarget'/'hintsJs') can find the button in the DOM.  'hintTarget'
-- produces the same strings for the published target.  (Tmux windows resolve
-- through their owning leksah window's @tab:lw:N@ — see 'paneButtonKey'.)
tabFlipKey :: TabKey -> Text
tabFlipKey k = "tab:" <> case k of
    WorkspaceKey   -> "workspace"
    ErrorsKey      -> "errors"
    LogKey         -> "log"
    GrepKey        -> "grep"
    TerminalsKey   -> "terminals"
    MetadataKey    -> "metadata"
    AgentsKey      -> "agents"
    ChangesKey     -> "changes"
    PreferencesKey -> "preferences"
    ShortcutsKey   -> "shortcuts"
    BrowserKey n   -> "browser:" <> T.pack (show n)
    TerminalKey s  -> "terminal:" <> s
    LeksahWinKey n -> "lw:" <> n
    GitLogKey d b  -> "gitlog:" <> T.pack d <> ":" <> b
    ReviewKey d    -> "review:" <> T.pack d
    TasksKey       -> "tasks"
    PlanKey d p    -> "plan:" <> T.pack d <> ":" <> p
    CompareKey d p -> "compare:" <> T.pack d <> ":" <> p
    EditorKey f    -> "editor:" <> T.pack f

-- | Where the ⌘` hint chip should sit for the flipper's one-press destination
-- @fi@: on the on-screen terminal pane it would go to (@Left paneId@, when the
-- front wide0 tab is the leksah window OWNING the pane's tmux window — every
-- owned window renders all the time now, so ownership IS visibility), else on
-- the tab button that would be selected (@Right flipKey@).  'owner' is
-- 'paneOwnerLw' over the current maps; 'front' the active wide0 tab key.
hintTarget :: (Text -> Int -> Maybe Text) -> Maybe TabKey -> Map Text (Text, [TmuxWindow]) -> FlipItem -> Maybe (Either Text Text)
hintTarget owner front tree = \case
    FlipPane s w p
      | paneOnScreen owner front tree s w
      , Just win <- windowOf s w
      , Just pn <- find ((== p) . tpIndex) (twPanes win)
      -> Just (Left (tpId pn))                       -- on-screen sibling pane
      | otherwise -> Just (Right (paneButtonKey owner s w))
                                                     -- off-screen: its own tab
    FlipTab (TerminalKey n)                          -- a remote terminal tab
      | M.member n tree -> Just (Right (tabFlipKey (TerminalKey n)))
      | otherwise -> Nothing
    FlipTab k -> Just (Right (tabFlipKey k))
    FlipView n _ -> Just (Right (tabFlipKey (LeksahWinKey n)))
  where
    windowOf s w = M.lookup s tree >>= find ((== w) . twIndex) . snd

-- | The @data-flipkey@ of the tab button a pane item resolves to: its owning
-- leksah window's, else its session's remote tile.  Matches what
-- 'mkTabButtons' stamps on the buttons.
paneButtonKey :: (Text -> Int -> Maybe Text) -> Text -> Int -> Text
paneButtonKey owner s w =
    maybe (tabFlipKey (TerminalKey s)) (tabFlipKey . LeksahWinKey) (owner s w)

-- | Is pane-carrying tmux window @w@ of session @s@ rendered by the ACTIVE
-- wide0 tab?  Its owning leksah window's tab for local sessions; for a remote
-- tab (no lw), tmux's current window of that session.
paneOnScreen :: (Text -> Int -> Maybe Text) -> Maybe TabKey -> Map Text (Text, [TmuxWindow]) -> Text -> Int -> Bool
paneOnScreen owner front tree s w = case front of
    Just (LeksahWinKey i) -> owner s w == Just i
    Just (TerminalKey n)  -> n == s && maybe False twActive
        (M.lookup s tree >>= find ((== w) . twIndex) . snd)
    _                     -> False

-- | How to highlight the flipper's live selection @fi@: @(Just paneId, tabKey)@
-- where @paneId@ is set only when the selection is a pane the active wide0 tab
-- renders (→ move the active-pane shadow onto it), and @tabKey@ is the
-- @data-flipkey@ of the tab button to tint with the hover colour (always).
flipSelHighlight :: (Text -> Int -> Maybe Text) -> Maybe TabKey -> Map Text (Text, [TmuxWindow]) -> FlipItem -> (Maybe Text, Text)
flipSelHighlight owner front tree = \case
    FlipPane s w p ->
        ( case windowOf s w of
            Just win | paneOnScreen owner front tree s w ->
                tpId <$> find ((== p) . tpIndex) (twPanes win)
            _ -> Nothing
        , paneButtonKey owner s w )
    FlipTab (TerminalKey n) -> (Nothing, tabFlipKey (TerminalKey n))
    FlipTab k               -> (Nothing, tabFlipKey k)
    FlipView n _            -> (Nothing, tabFlipKey (LeksahWinKey n))
  where
    windowOf s w = M.lookup s tree >>= find ((== w) . twIndex) . snd

-- | The flipper's item list: MRU order first, then every current tmux pane and
-- non-terminal tab (a terminal is represented only by its panes).  The flipper
-- is global across OS windows: @otherTabs@ are the non-terminal wide0 tabs
-- (editors) owned by OTHER windows — tmux panes are already global (they come
-- from @tree@, not per-window), so only editor tabs need adding here.  On
-- commit, a selection owned by another window raises that window (see the
-- ownership split in 'main').
buildFlipItems :: Map Text LeksahWindow -> [FlipItem] -> [(Text, TabKey)] -> [TabKey] -> Map Text (Text, [TmuxWindow]) -> S.Set (Text, Int) -> [(Text, FlipItem)]
buildFlipItems lws mru rt otherTabs tree hidden =
  let panes = [ FlipPane n (twIndex w) (tpIndex p)
              | (n, (_, wins)) <- M.toList tree, w <- wins, p <- twPanes w
              , not ((n, twIndex w) `S.member` hidden) ]
      -- Terminal-family tabs are represented by their PANES (from the tree):
      -- a leksah window's tmux panes flip individually, and its native VIEW
      -- panes (editor/git-log/browser leaves) each flip as a 'FlipView'.
      views = [ FlipView n l
              | (n, lw) <- M.toList lws
              , (LeafId l, pc) <- M.toList (lwPanes lw)
              , PaneView _ <- [pcKind pc] ]
      notTerm (TerminalKey _)  = False
      notTerm (LeksahWinKey _) = False
      notTerm _                = True
      tabs    = [ FlipTab k | (_, k) <- rt, notTerm k ]
             ++ [ FlipTab k | k <- otherTabs, notTerm k ]
      present = panes ++ views ++ tabs
      ordered = filter (`elem` present) mru ++ filter (`notElem` mru) present
      areaOf (FlipPane {}) = "wide0"
      areaOf (FlipView {}) = "wide0"
      areaOf (FlipTab k)   = maybe "wide0" fst (find ((== k) . snd) rt)
  in [ (areaOf fi, fi) | fi <- ordered ]

-- | The highest @N@ among existing @leksah-N@ session names (0 if none), given
-- @(session id, name)@ pairs — so a new terminal is named one past it.
maxLeksahNum :: [(Text, Text)] -> Int
maxLeksahNum xs = foldr max 0
  [ n | (_, name) <- xs
      , Just rest <- [T.stripPrefix "leksah-" name]
      , Just n <- [readMaybe (T.unpack rest)] ]

-- | The first terminal window flagged for attention — a BLOCKED Claude session
-- (waiting on an approval prompt) first, then bell (a teammate wants input),
-- then activity (new output) — as @(session id, window index)@.  The
-- jump-to-teammate command targets this; selecting a window clears its
-- bell/activity flag — and a blocked window only counts while it is NOT its
-- session's current window (mirroring tmux's bell rule) — so pressing the key
-- repeatedly walks through every window wanting attention in turn.
firstAlertWindow :: Map Text (Text, [TmuxWindow]) -> Maybe (Text, Int)
firstAlertWindow tree =
  let flagged pick = [ (sid, twIndex w) | (sid, (_, ws)) <- M.toList tree, w <- ws, pick w ]
      blocked w = not (twActive w)
               && any ((== Just "waiting") . tpClaudeStatus) (twPanes w)
  in listToMaybe (flagged blocked ++ flagged twBell ++ flagged twActivity)

-- | Defines @window.leksahOccurrenceVisible(path, containerSel)@: true when some
-- node tagged @data-reveal-key=path@ inside @containerSel@ is currently rendered
-- and within that container's viewport.  Scoping by container keeps the
-- workspace and metadata trees from matching each other's nodes.  Used to
-- suppress reveal/scroll when the focused file is already visible (it can appear
-- in a tree more than once).
-- | The ⌘W close-menu highlight hooks.  The active-pane highlight itself is
-- now pure model-driven CSS — every pane's chrome ('.terminal-cc-hl' markers
-- \/ '.pane-chrome' leaf divs \/ the '.tab.tab-active' outline) carries its
-- own ring, so nothing here measures or follows anything.  These two entry
-- points survive because 'renderCloseMenu' highlights an ARBITRARY target
-- (the pane being closed, or the whole terminal for \"Hide Window\") while
-- the menu is open: they just move a class.  'window.leksahUpdatePaneHl'
-- remains as a no-op so any stale caller is harmless.
paneHlJs :: Text
paneHlJs = T.unlines
  [ "(function(){"
  , "  function markerEl(id){ return document.querySelector('.terminal-cc-hl[data-pane='+JSON.stringify(id)+']'); }"
  , "  function paneEl(id){ return document.querySelector('.terminal-cc-pane[data-pane='+JSON.stringify(id)+']'); }"
  , "  function clearCls(cls){ document.querySelectorAll('.'+cls).forEach(function(e){ e.classList.remove(cls); }); }"
  , "  window.leksahMenuShadow = function(paneId, whole){"
  , "    clearCls('menu-target');"
  , "    var t = markerEl(paneId);"
  , "    if (whole) { var p = paneEl(paneId); t = p && p.closest('.terminal-cc'); }"
  , "    if (t) t.classList.add('menu-target');"
  , "  };"
  , "  window.leksahMenuShadowClear = function(){ clearCls('menu-target'); };"
  , "  window.leksahUpdatePaneHl = function(){};"
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
--   * ⌘D / ⌘⇧D — absolute overlay chips on the active terminal
--     pane (the one a split would act on, found via its shown highlight marker):
--     ⌘D at the bottom-middle, ⌘⇧D at the right-middle.  When no terminal pane
--     is on screen, the same chips land on the visible CONVERTIBLE tab body
--     (.leksah-convertible — an editor / git-log view, see withConvertHint):
--     there ⌘D/⌘⇧D convert the tab to its backing tmux pane and split it.
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
  -- The on-screen terminal's active-pane box (its .active chrome marker —
  -- markers are always shown now, the class marks the active one); the pane a
  -- ⌘D/⌘⇧D split would act on.  data-pane pairs marker and pane.
  , "  function activePaneEl(){ var ms=document.querySelectorAll('.terminal-cc-hl.active');"
  , "    for(var i=0;i<ms.length;i++){ if(shown(ms[i])){"
  , "      var win=ms[i].closest('.terminal-cc-window'), pid=ms[i].getAttribute('data-pane');"
  , "      var p = (win&&pid) ? win.querySelector('.terminal-cc-pane[data-pane='+JSON.stringify(pid)+']') : null;"
  , "      if(shown(p)) return p; } } return null; }"
  , "  function update(){ var c=box();"
  , "    var sd=c.querySelector('.leksah-hint-splitd'), sr=c.querySelector('.leksah-hint-splitr');"
  , "    sd.style.display='none'; sr.style.display='none';"
  , "    var ap=activePaneEl();"
  , "    if(!ap){ var cs=document.querySelectorAll('.leksah-convertible');"
  , "      for(var k=0;k<cs.length;k++){ if(shown(cs[k])){ ap=cs[k]; break; } } }"
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
  -- The flipper's LIVE selection (while it's open): ring the selected pane's
  -- chrome marker (class flip-target — see terminalCss) and tint its tab
  -- button with the hover colour.  Empty button => flipper closed (clear both).
  , "  window.leksahFlipSel = null;"
  , "  window.leksahSetFlipSel=function(pane,button){"
  , "    window.leksahFlipSel = button ? {pane: pane||null, button:button} : null;"
  , "    var prev=document.querySelectorAll('.leksah-flip-sel');"
  , "    for(var i=0;i<prev.length;i++) prev[i].classList.remove('leksah-flip-sel');"
  , "    if(button){ var tw=document.querySelector('[data-flipkey='+JSON.stringify(button)+']');"
  , "      if(tw) tw.classList.add('leksah-flip-sel'); }"
  , "    var pm=document.querySelectorAll('.terminal-cc-hl.flip-target');"
  , "    for(var j=0;j<pm.length;j++) pm[j].classList.remove('flip-target');"
  , "    if(button && pane){ var mk=document.querySelector('.terminal-cc-hl[data-pane='+JSON.stringify(pane)+']');"
  , "      if(mk) mk.classList.add('flip-target'); } };"
  , "  document.addEventListener('focusin', window.leksahUpdateHints, true);"
  , "  window.addEventListener('resize', window.leksahUpdateHints);"
  , "  window.addEventListener('keydown', function(e){ if(e.key==='Meta') window.leksahUpdateHints(); }, true);"
  , "})();"
  ]

-- | Drag-to-resize the side ("tall") and bottom ("wide1") panes.  The dividers'
-- ::after edge strips (see IDE.Web.Layout) are the only grabbable bits; a
-- mousedown on one starts a drag that drives the @--tall-col@ / @--wide1-row@
-- custom properties on the @.leksah@ root live, clamped to sane bounds, and
-- persisted in localStorage so the sizes survive a reload/relaunch.  Handled
-- entirely in JS (document-level, capture phase) because jsaddle-wkwebview
-- dispatches events to Haskell asynchronously, so a reflex handler couldn't
-- track the mouse or preventDefault synchronously.
-- | Drag support for the NATIVE split-leaf dividers (see
-- IDE.Web.Widget.TerminalCC): document-level capture-phase mouse tracking in
-- pure JS — jsaddle-wkwebview dispatches events asynchronously, so a Haskell
-- handler can neither preventDefault nor track a drag smoothly.  Commit on
-- mouseup only: the armed element's @__leksahNativeResize@ callback (set from
-- Haskell) gets the px delta along the divider's axis and folds it into the
-- shared layout.  A translucent bar follows the pointer during the drag so
-- there is feedback without streaming positions through the bridge.
nativeDragJs :: Text
nativeDragJs = T.unlines
  [ "window.LeksahNativeDrag = { arm: function(el, vert) {"
  , "  el.addEventListener('mousedown', function(e){"
  , "    e.preventDefault(); e.stopPropagation();"
  , "    var start = vert ? e.clientX : e.clientY;"
  , "    el.style.background = 'var(--leksah-selection, rgba(100,150,255,0.35))';"
  , "    function mm(ev){ ev.preventDefault();"
  , "      var d = (vert ? ev.clientX : ev.clientY) - start;"
  , "      el.style.transform = vert ? ('translateX('+d+'px)') : ('translateY('+d+'px)');"
  , "    }"
  , "    function mu(ev){"
  , "      document.removeEventListener('mousemove', mm, true);"
  , "      document.removeEventListener('mouseup', mu, true);"
  , "      el.style.transform = ''; el.style.background = '';"
  , "      var d = (vert ? ev.clientX : ev.clientY) - start;"
  , "      if (d !== 0 && el.__leksahNativeResize) el.__leksahNativeResize(d);"
  , "    }"
  , "    document.addEventListener('mousemove', mm, true);"
  , "    document.addEventListener('mouseup', mu, true);"
  , "  });"
  , "}};"
  -- Per-leaf font for native VIEW leaves: CodeMirror follows the wrapper's
  -- --leksah-mono-size CSS var by itself; Monaco snapshots its font at
  -- creation, so poke every Monaco editor inside the leaf (n = the override
  -- in px, 0 = back to the global monospace size).
  , "window.leksahSetLeafFont = function(el, n){"
  , "  try {"
  , "    if (window.monaco && monaco.editor && monaco.editor.getEditors) {"
  , "      monaco.editor.getEditors().forEach(function(e){"
  , "        var d = e.getDomNode();"
  , "        if (d && el.contains(d)) e.updateOptions({fontSize: (n || window.__leksahMonoSize)});"
  , "      });"
  , "    }"
  , "  } catch (err) {}"
  , "};"
  ]

resizeBarsJs :: Text
resizeBarsJs = T.unlines
  [ "(function(){"
  , "  if (window.leksahResizeInit) return; window.leksahResizeInit = true;"
  , "  function root(){ return document.querySelector('.leksah'); }"
  , "  var drag = null;"
  , "  document.addEventListener('mousedown', function(e){"
  , "    var t = e.target;"
  , "    if (!t || !t.classList) return;"
  , "    if (t.classList.contains('tall-divider')) drag = 'tall';"
  , "    else if (t.classList.contains('wide1-divider')) drag = 'wide1';"
  , "    else return;"
  , "    e.preventDefault();"
  , "    var r0 = root();"
  , "    if (r0) r0.classList.add(drag === 'tall' ? 'leksah-resizing-tall' : 'leksah-resizing-wide1');"
  , "    document.body.style.cursor = (drag === 'tall') ? 'col-resize' : 'row-resize';"
  , "  }, true);"
  , "  document.addEventListener('mousemove', function(e){"
  , "    if (!drag) return;"
  , "    var r = root(); if (!r) return;"
  , "    if (drag === 'tall') {"
  , "      var maxW = Math.max(200, Math.min(700, window.innerWidth - 200));"
  , "      var w = Math.max(120, Math.min(maxW, e.clientX - r.getBoundingClientRect().left));"
  , "      r.style.setProperty('--tall-col', w + 'px');"
  , "    } else {"
  , "      var wd = document.querySelector('.wide1-divider');"
  , "      var bottom = wd ? wd.getBoundingClientRect().bottom : (window.innerHeight - 20);"
  , "      var h = Math.max(60, Math.min(window.innerHeight - 120, bottom - e.clientY));"
  , "      r.style.setProperty('--wide1-bar', h + 'px');"
  , "    }"
  , "    e.preventDefault();"
  , "  }, true);"
  , "  document.addEventListener('mouseup', function(){"
  , "    if (!drag) return;"
  , "    var r = root();"
  , "    if (r) { try {"
  , "      localStorage.setItem('leksahTallCol', r.style.getPropertyValue('--tall-col'));"
  , "      localStorage.setItem('leksahWide1Row', r.style.getPropertyValue('--wide1-bar'));"
  , "    } catch(_){} }"
  , "    if (r) r.classList.remove('leksah-resizing-tall', 'leksah-resizing-wide1');"
  , "    drag = null; document.body.style.cursor = '';"
  , "  }, true);"
  , "  (function restore(){"
  , "    var r = root(); if (!r) { setTimeout(restore, 200); return; }"
  , "    try {"
  , "      var tc = localStorage.getItem('leksahTallCol'); if (tc) r.style.setProperty('--tall-col', tc);"
  , "      var wr = localStorage.getItem('leksahWide1Row'); if (wr) r.style.setProperty('--wide1-bar', wr);"
  , "    } catch(_){}"
  , "  })();"
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
  , "      var cm = t.querySelector('.cm-content')"
  , "            || t.querySelector('.monaco-editor textarea.inputarea');"
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
  -- ⌘/⌃ are always the keymap's; ⌥ is meaningful only as the "open into a split
  -- pane" modifier on Enter/Space (⌥⇧ for the other direction), so let those
  -- through but leave ⌥+other keys to the keymap.
  , "    if (e.metaKey || e.ctrlKey) return;"
  , "    if (e.altKey && e.key !== 'Enter' && e.key !== ' ') return;"
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
  -- Forward the ⌥/⇧ modifier bits into the synthetic click/dblclick (via the
  -- MouseEvent init dict) so the workspace row's ⌥-open-into-split handler sees
  -- them — a bare cur.click() would carry none.
  , "        if (cur) { var mo = {bubbles:true, cancelable:true, view:window, altKey:e.altKey, shiftKey:e.shiftKey};"
  , "          cur.dispatchEvent(new MouseEvent('click', mo));"
  -- A workspace file row opens on double-click (single click just selects it),
  -- so Enter/Space on a file must synthesise a dblclick to open it in the editor.
  -- Claude nodes (the "Claude" row and its session children, nested in li.claude)
  -- likewise act on double-click, so Enter/Space there resumes/launches.
  , "          if (cur.closest('li.file, li.claude')) cur.dispatchEvent(new MouseEvent('dblclick', mo)); }"
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
  -- Focus a just-selected tab's BODY unless something inside it takes focus
  -- itself (Tabs.hs calls this on every tab select).  A panel with no input
  -- (Preferences, Review, …) otherwise leaves DOM focus in the OLD pane —
  -- which keeps re-crowning itself as the active pane (the focusin sensor
  -- above feeds activePane and the MRU), so the highlight and tab order
  -- never move, and keystrokes keep going to the old pane.  setTimeout so a
  -- widget's own focus grab (editors/terminals focus their inputs a beat
  -- after select) runs after and wins.
  , "  window.leksahFocusTabBody = function(el){ setTimeout(function(){"
  , "    if (el && el.isConnected && !el.classList.contains('tab-hidden')"
  , "        && !el.contains(document.activeElement))"
  , "      el.focus({preventScroll: true});"
  , "  }, 0); };"
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
    -- Anchor (file\\0hoverLine\\0hoverCol) of the tooltip currently shown, plus a
    -- debounce timer for hiding.  xterm rebuilds its link objects on every
    -- redraw/reflow (Claude Code repaints constantly), firing leave->hover for
    -- the SAME token; without this the tooltip blanks and re-requests LSP each
    -- repaint (visible flicker).
  , "  var shownKey = null, hideTimer = null;"
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
  , "        st.textContent = '.leksah-term-hovertip code{font-family:Menlo,Monaco,\"Courier New\",monospace;background:var(--leksah-inset-bg);border-radius:3px;padding:0 3px;font-size:11.5px;}'"
  , "          + '.leksah-term-hovertip pre{margin:4px 0;padding:5px 8px;background:var(--leksah-inset-bg);border:1px solid var(--leksah-inset-line);border-radius:4px;font-family:Menlo,Monaco,\"Courier New\",monospace;font-size:11.5px;line-height:1.3;white-space:pre;overflow-x:hidden;}'"
  , "          + '.leksah-term-hovertip hr{border:none;border-top:1px solid var(--leksah-inset-line);margin:5px 0;}'"
  , "          + '.leksah-term-hovertip strong{color:var(--leksah-fg);font-weight:600;}';"
  , "        document.head.appendChild(st);"
  , "      }"
  , "      tip = document.createElement('div');"
  , "      tip.className = 'leksah-term-hovertip';"
  , "      tip.style.cssText = 'position:fixed;z-index:99999;pointer-events:none;'"
  , "        + 'background:var(--leksah-surface-alt);color:var(--leksah-fg-muted);border:1px solid var(--leksah-border-control);'"
  , "        + 'border-radius:5px;padding:6px 9px;font-size:12px;max-width:72ch;max-height:60vh;'"
  , "        + 'white-space:normal;overflow:hidden;display:none;box-shadow:0 4px 14px var(--leksah-shadow-drop);'"
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
  , "  function hideTip(){ if (tip) tip.style.display = 'none'; shownRid = -1; shownKey = null; }"
    -- Defer the hide: a redraw fires leave immediately followed by hover on the
    -- rebuilt link for the same token, which cancels this before it runs.
  , "  function scheduleHide(){ if (hideTimer) clearTimeout(hideTimer); hideTimer = setTimeout(function(){ hideTimer = null; hideTip(); }, 150); }"
  , "  function cancelHide(){ if (hideTimer){ clearTimeout(hideTimer); hideTimer = null; } }"
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
  -- Plain-text http(s) URLs (agents print dev-server addresses as plain text,
  -- not OSC 8): clickable → a leksah browser pane (see __leksahOpenUrl).
  , "  var URLRE = /https?:\\/\\/[^\\s\"'<>`)\\]]+/g;"
  -- A rendered diff/content line under a Claude edit or a unified diff: an
  -- indent, a right-aligned source line number, then a fixed 4-column field
  -- (\"    \" for context, \" - \"/\" + \" + a space for -/+ lines) and then the
  -- verbatim source text.  Group 2 is the source line; the whole match's length
  -- is where the source text starts, so a hovered identifier's offset past it is
  -- its 0-based column in the file.
  --
  -- NOTE: the web demo's docs/website/try/gen-demo-hovers.py carries Python
  -- ports of HDR/GUT/ID (it precomputes hover tooltips for exactly the
  -- tokens these match) — keep them in sync.
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
  -- Testability seam: the last attached hover callback, so a scripted check
  -- (docs/website/try/autotest.js) can drive the file->LSP->resolveHover
  -- round-trip without synthesizing xterm mouse events.
  , "    window.LeksahTermLinks.debugHover = onHoverFile;"
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
  , "        hover: function(ev){"
  , "          if (!onHoverFile) return;"
  , "          var key = f + '\\u0000' + (hl||0) + '\\u0000' + hcol;"
  , "          cancelHide();"
    -- Same token still hovered (xterm rebuilt the link under a redraw): keep the
    -- existing tooltip and its in-flight/resolved LSP content untouched.
  , "          if (tip && tip.style.display !== 'none' && key === shownKey) return;"
  , "          shownKey = key;"
  , "          var rid = ++hoverSeq; shownRid = rid;"
  , "          showTip(ev, txt);"
  , "          onHoverFile(f, hl||0, hcol, rid);"
  , "        },"
  , "        leave: function(){ scheduleHide(); }"
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
  -- URLs first (and marked consumed, so the file-path regex can't re-match
  -- inside one): click = open in a leksah browser pane; ⌥-click = open it as
  -- a native split beside this terminal (⌥⇧ = the other direction).
  , "        URLRE.lastIndex = 0;"
  , "        var uu;"
  , "        while ((uu = URLRE.exec(text))){"
  , "          var utxt = uu[0].replace(/[.,;:!?]+$/, '');"
  , "          (function(u, sx, ex){"
  , "            links.push({"
  , "              range: { start: { x: sx, y: y }, end: { x: ex, y: y } },"
  , "              text: u,"
  , "              decorations: { pointerCursor: true, underline: true },"
  , "              activate: function(ev){"
  , "                if (ev && ev.preventDefault) ev.preventDefault();"
  , "                if (window.__leksahOpenUrl)"
  , "                  window.__leksahOpenUrl(u, !!(ev&&ev.altKey), !!(ev&&ev.shiftKey));"
  , "              }"
  , "            });"
  , "            consumed.push([sx-1, ex]);"
  , "          })(utxt, uu.index+1, uu.index+utxt.length);"
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
  , "        + 'background:var(--leksah-surface);color:var(--leksah-fg-muted);border:1px solid var(--leksah-border-control);'"
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
  -- Terminal font (monospace-font pref, published by IDE.Web.Main); seed a system
  -- default so the probe and terminals have a value before the prefs publish.
  , "  window.__leksahMonoFamily = window.__leksahMonoFamily || 'Menlo, Monaco, \"Courier New\", monospace';"
  , "  window.__leksahMonoSize = window.__leksahMonoSize || 13;"
  , "  function register(id, term){ byId[id] = term;"
  , "    try { if (window.__leksahXtermActive) term.options.theme = window.__leksahXtermActive; } catch (e) {} }"
  , "  function unregister(id){ delete byId[id];"
  , "    try { window.LeksahKitty.drop(id); } catch (e) {} }"
  , "  function write(id, b64){"
  , "    var term = byId[id];"
  , "    if (!term) return;"
  , "    var bin = atob(b64), n = bin.length, a = new Uint8Array(n);"
  , "    for (var i=0;i<n;i++) a[i] = bin.charCodeAt(i);"
  -- Output goes through the passthrough/kitty filter (IDE.Web.KittyGraphics);
  -- everything it does not intercept it hands to term.write unchanged, in one
  -- write per chunk.
  , "    if (window.LeksahKitty) window.LeksahKitty.feed(id, term, a);"
  , "    else term.write(a);"
  , "  }"
  -- The terminal cell size (CSS px) for leksah's font settings — measured
  -- ONCE from a throwaway offscreen xterm (the browser equivalent of reading
  -- font metrics; it also captures xterm's own rounding, which is the real
  -- authority).  Cached; warmed at startup so callers see it synchronously.
  -- Everything downstream is feed-forward from this (grid = floor(box/cell)),
  -- the way iTerm2/Ghostty size their grids — never render-then-correct.
  -- GPU renderer, guarded: xterm's WebglAddon throws from activate() when no
  -- WebGL context can be created (headless Chrome --disable-gpu, GPU-less
  -- environments), and under the GHC JS backend that failure is an
  -- uncatchable RTS crash — so the probe AND the loadAddon both live here,
  -- behind a JS try/catch, and Haskell only sees the boolean.
  , "  function loadWebgl(term){"
  , "    try {"
  , "      if (!(window.WebglAddon && window.WebglAddon.WebglAddon)) return false;"
  , "      var c = document.createElement('canvas');"
  , "      if (!(c.getContext('webgl2') || c.getContext('webgl'))) return false;"
  , "      term.loadAddon(new window.WebglAddon.WebglAddon());"
  , "      return true;"
  , "    } catch (e) { return false; }"
  , "  }"
  -- Cell metrics are keyed by the font they were measured with (cellCache =
  -- {fam,size,w,h}).  The monospace-font pref publishes __leksahMono* AFTER page
  -- init, so an early measure sees the seed font; re-measuring whenever the active
  -- font differs guarantees the probe metric matches the font the terminals
  -- actually render (else the row count overflows the pane and clips the bottom).
  -- With per-leaf font sizes (native split layouts) the cache is a map keyed
  -- fam+'/'+size; measureCell/cellMetrics take an optional size override
  -- (default: the global __leksahMonoSize).  fontOpts single-sources the
  -- xterm font options (family/size/lineHeight/letterSpacing) so the probe
  -- and the real terminals can never disagree (a mismatch overflows the row
  -- count and clips the pane bottom).
  , "  var cellCache = {};"
  , "  function fontOpts(sz){"
  , "    return { fontFamily: window.__leksahMonoFamily,"
  , "             fontSize: (sz || window.__leksahMonoSize),"
  , "             lineHeight: 1.07, letterSpacing: -0.5 };"
  , "  }"
  , "  function measureCell(sz){"
  , "    var o = fontOpts(sz);"
  , "    var key = o.fontFamily + '/' + o.fontSize;"
  , "    if (cellCache[key]) return cellCache[key];"
  , "    try {"
  , "      var host = document.createElement('div');"
  , "      host.style.cssText = 'position:fixed;left:-10000px;top:0;width:900px;height:700px;';"
  , "      document.body.appendChild(host);"
  , "      var t = new Terminal({cols: 80, rows: 24});"
  , "      t.options.fontFamily = o.fontFamily;"
  , "      t.options.fontSize = o.fontSize;"
  , "      t.options.lineHeight = o.lineHeight;"
  , "      t.options.letterSpacing = o.letterSpacing;"
  , "      t.open(host);"
  , "      var s = host.querySelector('.xterm-screen');"
  , "      var r = s ? s.getBoundingClientRect() : null;"
  , "      if (r && r.width && r.height) cellCache[key] = { w: r.width / 80, h: r.height / 24 };"
  , "      t.dispose();"
  , "      document.body.removeChild(host);"
  , "    } catch (e) {}"
  , "    return cellCache[key];"
  , "  }"
  , "  function cellMetrics(sz){ return measureCell(sz); }"
  -- After the configured font has actually loaded, drop the cache and re-measure:
  -- a web font (e.g. bundled Hasklig) reports fallback metrics before it loads.
  -- document.fonts.ready settles after pending loads (immediately for system
  -- fonts like Menlo/Monaco).
  , "  function warmCell(){"
  , "    try {"
  , "      if (document.fonts && document.fonts.ready) document.fonts.ready.then(function(){ cellCache = {}; measureCell(); }, measureCell);"
  , "      else measureCell();"
  , "    } catch (e) { measureCell(); }"
  , "  }"
  , "  if (window.requestAnimationFrame) requestAnimationFrame(warmCell); else warmCell();"
  , "  return { register: register, unregister: unregister, write: write, loadWebgl: loadWebgl, cellMetrics: cellMetrics, fontOpts: fontOpts, byId: byId };"
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
  , "    var iframes = document.querySelectorAll('.terminal-cc-iframe, .browser-frame');"
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

-- | Defines @window.LeksahPaneDrag.arm(root)@: HTML5 drag-and-drop for the
-- Terminals tree — drag a pane row (@[data-pane-src]@ = @\"host|paneId\"@) onto
-- a window row (@[data-win-dst]@ = @\"host|session|widx\"@) to move the pane
-- into that window.  Listeners are delegated on @root@ (the @.terminals@
-- container) so they survive reflex re-rendering the rows.  A drop is only
-- allowed within the SAME host (you can't relocate a pane's process across an
-- ssh boundary), which is why the whole gesture — including the synchronous
-- @preventDefault@ on @dragover@ that HTML5 DnD needs to permit a drop (jsaddle
-- dispatches Haskell handlers async, too late for that) — runs here in JS; only
-- the final move is called back to Haskell via @root.__leksahMovePane@.
paneDragJs :: Text
paneDragJs = T.unlines
  [ "window.LeksahPaneDrag = { arm: function(root){"
  , "  if (!root || root.__leksahPaneDragArmed) return;"
  , "  root.__leksahPaneDragArmed = true;"
  , "  var dragSrc = null, dropEl = null;"
  , "  function hostOf(spec){ return spec ? spec.split('|')[0] : null; }"
  , "  function clearDrop(){ if (dropEl) { dropEl.classList.remove('drop-target'); dropEl = null; } }"
  , "  root.addEventListener('dragstart', function(e){"
  , "    var src = e.target && e.target.closest && e.target.closest('[data-pane-src]');"
  , "    if (!src) { dragSrc = null; return; }"
  , "    dragSrc = src.getAttribute('data-pane-src');"
  , "    if (e.dataTransfer) { e.dataTransfer.effectAllowed = 'move';"
  , "      try { e.dataTransfer.setData('text/plain', dragSrc); } catch(_){} }"
  , "    src.classList.add('tdrag-src');"
  , "  });"
  , "  root.addEventListener('dragend', function(e){"
  , "    var src = e.target && e.target.closest && e.target.closest('[data-pane-src]');"
  , "    if (src) src.classList.remove('tdrag-src');"
  , "    dragSrc = null; clearDrop();"
  , "  });"
  , "  root.addEventListener('dragover', function(e){"
  , "    if (!dragSrc) return;"
  , "    var dst = e.target && e.target.closest && e.target.closest('[data-win-dst]');"
  , "    if (!dst || hostOf(dst.getAttribute('data-win-dst')) !== hostOf(dragSrc)) { clearDrop(); return; }"
  , "    e.preventDefault();"                       -- MUST be sync → allows the drop
  , "    if (e.dataTransfer) e.dataTransfer.dropEffect = 'move';"
  , "    if (dropEl !== dst) { clearDrop(); dropEl = dst; dst.classList.add('drop-target'); }"
  , "  });"
  , "  root.addEventListener('dragleave', function(e){"
  , "    var dst = e.target && e.target.closest && e.target.closest('[data-win-dst]');"
  , "    if (dst && dst === dropEl && !(e.relatedTarget && dst.contains(e.relatedTarget))) clearDrop();"
  , "  });"
  , "  root.addEventListener('drop', function(e){"
  , "    if (!dragSrc) return;"
  , "    var dst = e.target && e.target.closest && e.target.closest('[data-win-dst]');"
  , "    if (!dst) { clearDrop(); return; }"
  , "    var dstSpec = dst.getAttribute('data-win-dst');"
  , "    if (hostOf(dstSpec) !== hostOf(dragSrc)) { clearDrop(); return; }"
  , "    e.preventDefault();"
  , "    var src = dragSrc; clearDrop();"
  , "    if (root.__leksahMovePane) root.__leksahMovePane(src, dstSpec);"
  , "  });"
  , "} };"
  ]

-- | The ⌘-drag pane move (Ctrl off-mac).  A ⌘+mousedown on a split-tree pane
-- (@.terminal-cc-leaf[data-leaf]@), a draggable plain wide0 tab's BODY, or that
-- tab's BUTTON in the row (the handle for a tab whose body is off screen) arms a
-- gesture that ENGAGES after 5px of movement — a stationary ⌘-click keeps its
-- existing meaning (OSC links, editor go-to).  Engaged, a fixed
-- @.leksah-drag-shadow@ previews the drop: over an LW container it animates to
-- the candidate split (the same algorithm as 'pickDropTarget' in
-- IDE.Web.SplitLayout — the commit RECOMPUTES there from the raw pointer, this
-- copy is preview-only); over the wide0 tab row it peeks the hovered tab
-- (shown, not activated — @leksahLeafPeek@); over the dragged pane itself or
-- anywhere invalid it parks on the SOURCE pane ("release here and nothing
-- changes" — and static, so no transition lag on every mousemove).  All
-- tracking is pure JS (jsaddle dispatches async — a 60fps loop can never
-- round-trip); Haskell hears ONE call, @leksahLeafDrop(json)@, on release or
-- Escape.  Geometry comes from @window.__leksahLwGeom@ (published per window
-- by TerminalCC) and @window.__leksahLeafDragTabs@ (the draggable plain-tab
-- show-keys).  Native browser panes are made click-through for the duration
-- via a @{drag:bool}@ post on the leksahBrowserFrame message handler.
--
-- One kind of target the pointer alone cannot express: the individual tmux
-- panes of a multi-pane window ('tmuxCands').  A leksah leaf is one whole tmux
-- window, so 'pickDropTarget' can only ever name the leaf; the panes inside it
-- are found here from their @.terminal-cc-hl[data-pane]@ marker boxes and, when
-- one wins, ride along in the payload as @tleaf@/@tpane@/@tv@/@tafter@ — a
-- 'TmuxTarget' the commit side VALIDATES against the live model and otherwise
-- ignores, falling back to the leaf-level recompute.  The leaf's own four edges
-- stay in the pool, so the full-size join is still reachable, and the shadow is
-- the honest description of which of the two the release will do.
--
-- Two things a mouse gesture built on mousemove/mouseup has to defend itself
-- against, both of which cost the user the gesture ENTIRELY and were how the
-- editor panes came to look undraggable, along with the retargeting 'track'
-- defends against.  A native HTML5 drag (an editor with
-- text drag-and-drop on, any selection under the pointer) takes the mouse away:
-- no more mousemove, never a mouseup.  So a tracked move calls
-- @preventDefault@ from the very first pixel — before the engage threshold,
-- which is precisely the window WebKit would start its drag in — and a
-- @dragstart@ that gets through anyway is refused.  And whenever the mouseup
-- goes missing regardless, the arming state must not survive: a leftover @st@
-- made every LATER ⌘-drag a silent no-op, so window blur and the next
-- ⌘+mousedown both 'cancel' it.
leafDragJs :: Text
leafDragJs = T.unlines
  [ "(function(){"
  , "  var MOD = (navigator.platform||'').toUpperCase().indexOf('MAC') >= 0 ? 'metaKey' : 'ctrlKey';"
  , "  var st = null;"
  , "  function shadowTo(x,y,w,h){ var s = st.shadow.style;"
  , "    s.left = x+'px'; s.top = y+'px'; s.width = w+'px'; s.height = h+'px'; }"
  -- "Nothing will change if released here": park the shadow ON the source
  -- pane (static — a mouse-following box restarts its transition every move
  -- and lags).  A detached source element measures 0×0: leave the shadow be.
  , "  function srcBox(){"
  , "    var r = st.srcEl && st.srcEl.getBoundingClientRect();"
  , "    if (r && (r.width > 2 || r.height > 2)) shadowTo(r.left, r.top, r.width, r.height); }"
  , "  function clearPeekHl(){ if (!st) return;"
  , "    if (st.peekTimer){ clearTimeout(st.peekTimer); st.peekTimer = null; }"
  , "    if (st.peekEl){ st.peekEl.classList.remove('leksah-drag-peek'); st.peekEl = null; } }"
  , "  function dist(px,py,mx,my){ var dx=px-mx, dy=py-my; return dx*dx+dy*dy; }"
  -- Deterministic tie order: lexicographic (path, orient==V, after), matching
  -- pickDropTarget's specKey.
  , "  function cmpSpec(a,b){"
  , "    for (var i=0; i<Math.max(a.p.length,b.p.length); i++){"
  , "      var av = i<a.p.length ? a.p[i] : -1, bv = i<b.p.length ? b.p[i] : -1;"
  , "      if (av!==bv) return av-bv; }"
  , "    if (a.v!==b.v) return a.v?1:-1;"
  , "    if (a.after!==b.after) return a.after?1:-1;"
  , "    return 0; }"
  , "  function halves(c){"
  , "    return [ {v:false, after:false, rect:[c.x,c.y,c.w/2,c.h]}"
  , "           , {v:false, after:true,  rect:[c.x+c.w/2,c.y,c.w/2,c.h]}"
  , "           , {v:true,  after:false, rect:[c.x,c.y,c.w,c.h/2]}"
  , "           , {v:true,  after:true,  rect:[c.x,c.y+c.h/2,c.w,c.h/2]} ]; }"
  , "  function leafPathOf(subs, lid){"
  , "    for (var i=0; i<subs.length; i++) if (subs[i].leaf===lid) return subs[i].p;"
  , "    return null; }"
  -- The tmux panes of the multi-pane window under the pointer, as EXTRA
  -- candidate rects (container-relative): each is a target in its own right,
  -- alongside its leaf's four edges and its ancestors'.  Their marker boxes
  -- are the pane's true visual box (renderHlSegments) — the same geometry the
  -- mousedown hit-test picks the dragged pane out of.  A pane's OWN halves are
  -- never offered to itself; a whole-leaf drag never reaches here for its own
  -- leaf (pickTarget bails on the excluded leaf before extras are added).
  , "  function tmuxCands(el, subs, cr, px, py){"
  , "    var leaf = el && el.closest && el.closest('.terminal-cc-leaf[data-leaf]');"
  , "    if (!leaf) return [];"
  , "    var hls = leaf.querySelectorAll('.terminal-cc-hl[data-pane]');"
  , "    if (hls.length < 2) return [];"
  , "    var lid = parseInt(leaf.getAttribute('data-leaf'),10);"
  , "    var lp = leafPathOf(subs, lid);"
  , "    if (lp === null) return [];"
  , "    var out = [];"
  , "    for (var i=0; i<hls.length; i++){"
  , "      var pid = hls[i].getAttribute('data-pane');"
  , "      if (st.srcKind==='tmux' && pid===st.srcPane) continue;"
  , "      var r = hls[i].getBoundingClientRect();"
  , "      var x = r.left-cr.left, y = r.top-cr.top;"
  , "      if (px<x || px>x+r.width || py<y || py>y+r.height) continue;"
  , "      out.push({p:lp.concat([i]), tp:{leaf:lid, pane:pid},"
  , "                x:x, y:y, w:r.width, h:r.height}); }"
  , "    return out; }"
  , "  function pickTarget(subs, cw, ch, px, py, excl, extra){"
  , "    var containing = [];"
  , "    for (var i=0; i<subs.length; i++){ var s=subs[i];"
  , "      var x=s.x*cw, y=s.y*ch, w=s.w*cw, h=s.h*ch;"
  , "      if (px>=x && px<=x+w && py>=y && py<=y+h){"
  , "        if (excl!==null && s.leaf!==null && s.leaf===excl) return null;"
  , "        containing.push({p:s.p, x:x, y:y, w:w, h:h, tp:null}); } }"
  , "    if (!containing.length) return null;"
  , "    (extra||[]).forEach(function(c){ containing.push(c); });"
  , "    var cands = [];"
  -- Score = distance to the CENTRE of the half the pane would occupy
  -- (mirrors pickDropTarget).
  , "    containing.forEach(function(c){"
  , "      halves(c).forEach(function(k){"
  , "        cands.push({d:dist(px,py,k.rect[0]+k.rect[2]/2,k.rect[1]+k.rect[3]/2),"
  , "                    p:c.p, v:k.v, after:k.after, rect:k.rect, tp:c.tp});"
  , "      });"
  , "    });"
  , "    var best = Infinity;"
  , "    cands.forEach(function(c){ if (c.d < best) best = c.d; });"
  , "    var ties = cands.filter(function(c){ return Math.sqrt(c.d) <= Math.sqrt(best)+0.5; });"
  , "    ties.sort(cmpSpec);"
  , "    return ties[Math.abs(Math.floor(px)+Math.floor(py)) % ties.length]; }"
  , "  function setBrowserDrag(on){"
  , "    try { if (window.webkit && window.webkit.messageHandlers"
  , "              && window.webkit.messageHandlers.leksahBrowserFrame)"
  , "      window.webkit.messageHandlers.leksahBrowserFrame.postMessage({drag:on}); } catch(_){} }"
  , "  function swallow(e){ e.preventDefault(); e.stopPropagation(); }"
  , "  function engage(e){"
  , "    st.engaged = true;"
  , "    var root = document.querySelector('.leksah');"
  , "    if (root) root.classList.add('leksah-pane-dragging');"
  -- Browser-pane iframes too: the showcase drag crosses them, and an iframe
  -- eats mousemove for its own document, freezing the tracking loop.
  , "    st.iframes = Array.prototype.slice.call(document.querySelectorAll('.terminal-cc-iframe, .browser-frame'));"
  , "    st.iframes.forEach(function(f){ f.style.pointerEvents = 'none'; });"
  , "    setBrowserDrag(true);"
  , "    try { window.getSelection && window.getSelection().removeAllRanges(); } catch(_){}"
  , "    var sh = document.createElement('div');"
  , "    sh.className = 'leksah-drag-shadow';"
  , "    st.shadow = sh;"
  -- First paint highlights the pane being dragged; the transitions animate
  -- away from it on the next tracked move.
  , "    var r = st.srcEl.getBoundingClientRect();"
  , "    shadowTo(r.left, r.top, r.width, r.height);"
  , "    document.body.appendChild(sh);"
  , "    document.addEventListener('click', swallow, true);"
  , "    if (window.leksahLeafDragStart) window.leksahLeafDragStart();"
  , "  }"
  , "  function track(e){"
  -- What is under the POINTER, not what the event calls its target.  An editor
  -- runs its own pointer monitor for drag-select and takes pointer capture with
  -- it, which retargets every mousemove of the drag back to the editor: e.target
  -- then swears the pointer never left the source pane, so the tab row never
  -- peeked and no other window was reachable — while dropping INSIDE the source
  -- window still worked, because that lookup lands on the same container either
  -- way.  Terminals never do this, which is why only editor panes were stuck.
  -- The shadow is pointer-events:none, so the hit test can never return it.
  , "    var t = document.elementFromPoint(e.clientX, e.clientY) || e.target;"
  , "    if (!t || !t.closest) { srcBox(); st.lastDst = {kind:'none'}; return; }"
  -- The wide0 tab row: peek the hovered tab (dwell 150ms) so the user can
  -- drag on into its body.
  , "    var tw = t.closest('.tab-buttons.area-wide0 .tab-wrap[data-tabkey]');"
  , "    if (tw){"
  -- Back over the button the drag STARTED from: park, don't peek at ourselves.
  , "      if (st.srcKind==='tab' && tw.getAttribute('data-tabkey')===st.srcTab){"
  , "        clearPeekHl(); srcBox(); st.lastDst = {kind:'none'}; return; }"
  , "      if (st.peekEl !== tw){ clearPeekHl();"
  , "        st.peekEl = tw; tw.classList.add('leksah-drag-peek');"
  , "        var key = tw.getAttribute('data-tabkey');"
  , "        st.peekTimer = setTimeout(function(){"
  , "          if (window.leksahLeafPeek) window.leksahLeafPeek(key); }, 150);"
  , "      }"
  , "      var r = tw.getBoundingClientRect();"
  , "      shadowTo(r.left, r.top, r.width, r.height);"
  , "      st.lastDst = {kind:'tab', tab: tw.getAttribute('data-tabkey')};"
  , "      return; }"
  , "    clearPeekHl();"
  -- Inside a leksah-window container: the candidate-split preview.
  , "    var cc = t.closest('.terminal-cc[data-lw]');"
  , "    if (cc){"
  , "      var lw = cc.getAttribute('data-lw');"
  , "      var geom = (window.__leksahLwGeom||{})[lw];"
  , "      if (geom && geom.subs){"
  , "        var cr = cc.getBoundingClientRect();"
  , "        var px = e.clientX - cr.left, py = e.clientY - cr.top;"
  , "        var excl = (st.srcKind==='pane' && lw===st.srcLw) ? st.srcLeaf : null;"
  , "        var pick = pickTarget(geom.subs, cr.width, cr.height, px, py, excl,"
  , "                              tmuxCands(t, geom.subs, cr, px, py));"
  , "        if (pick){"
  , "          shadowTo(cr.left+pick.rect[0], cr.top+pick.rect[1], pick.rect[2], pick.rect[3]);"
  , "          st.lastDst = {kind:'pane', lw:lw, px:px, py:py, cw:cr.width, ch:cr.height};"
  -- A tmux pane won: the pointer alone can't express it, so the pick rides
  -- along (Haskell VALIDATES it and falls back to the pointer recompute).
  , "          if (pick.tp){ st.lastDst.tleaf = pick.tp.leaf;"
  , "                        st.lastDst.tpane = pick.tp.pane;"
  , "                        st.lastDst.tv = pick.v; st.lastDst.tafter = pick.after; }"
  , "          return; } }"
  , "      srcBox(); st.lastDst = {kind:'none'}; return; }"
  -- A draggable plain tab's visible body: land beside it (right half).
  , "    var tb = t.closest('.tab.area-wide0[data-tabkey]');"
  , "    if (tb){ var k = tb.getAttribute('data-tabkey');"
  , "      if (window.__leksahLeafDragTabs && window.__leksahLeafDragTabs[k]"
  , "          && !(st.srcKind==='tab' && k===st.srcTab)){"
  , "        var r2 = tb.getBoundingClientRect();"
  , "        shadowTo(r2.left+r2.width/2, r2.top, r2.width/2, r2.height);"
  , "        st.lastDst = {kind:'tab', tab:k};"
  , "        return; } }"
  , "    srcBox(); st.lastDst = {kind:'none'};"
  , "  }"
  -- Pointer events are a separate stream from the mouse events this gesture is
  -- built on, and the editor's drag-select monitor listens on THAT one — hence
  -- the selection creeping along under the drag, which no amount of
  -- preventDefault on mousemove can stop (the editor sets it programmatically).
  -- While a gesture is armed the stream is swallowed outright, so nothing else
  -- can act on the drag: stopImmediatePropagation, because the monitor sits on
  -- document too and mere stopPropagation would not reach it.
  , "  function pm(e){ if (!st) return;"
  , "    e.preventDefault(); e.stopImmediatePropagation(); }"
  , "  function unlisten(){"
  , "    document.removeEventListener('mousemove', mv, true);"
  , "    document.removeEventListener('pointermove', pm, true);"
  , "    document.removeEventListener('mouseup', up, true);"
  , "    document.removeEventListener('keydown', kd, true); }"
  -- Give up without moving anything.  An engaged gesture goes out through
  -- 'finish' so the shadow and the dragging chrome are torn down exactly once
  -- (dst 'none' → the commit side only reactivates the source pane, as Escape
  -- does); armed-but-not-engaged just forgets the arming.
  , "  function cancel(){ if (!st) return; unlisten();"
  , "    if (st.engaged) finish({kind:'none'}); else st = null; }"
  , "  function finish(dst){"
  , "    var root = document.querySelector('.leksah');"
  , "    if (root) root.classList.remove('leksah-pane-dragging');"
  , "    (st.iframes||[]).forEach(function(f){ f.style.pointerEvents = ''; });"
  , "    setBrowserDrag(false);"
  , "    clearPeekHl();"
  , "    if (st.shadow && st.shadow.parentNode) st.shadow.parentNode.removeChild(st.shadow);"
  -- The trailing click (dispatched between mouseup and this timeout) is
  -- swallowed; then the guard goes away so the NEXT click is real.
  , "    setTimeout(function(){ document.removeEventListener('click', swallow, true); }, 0);"
  , "    var payload = { src: (st.srcKind==='tmux') ? {lw:st.srcLw, leaf:st.srcLeaf, pane:st.srcPane}"
  , "                       : (st.srcKind==='pane') ? {lw:st.srcLw, leaf:st.srcLeaf}"
  , "                       : {tab:st.srcTab},"
  , "                    dst: dst || {kind:'none'} };"
  , "    st = null;"
  , "    if (window.leksahLeafDrop) window.leksahLeafDrop(JSON.stringify(payload));"
  , "  }"
  , "  function mv(e){"
  , "    if (!st) return;"
  -- preventDefault from the FIRST tracked move, BEFORE the 5px threshold: this is
  -- what stops the pane content starting a native (HTML5) drag out from under the
  -- gesture — a Monaco/CodeMirror text drag, or any selection under the pointer.
  -- WebKit's own drag threshold is smaller than ours, so the arming window is
  -- exactly where a native drag wins, and a native drag is fatal: it swallows
  -- every further mousemove and the mouseup, so the gesture can neither engage
  -- nor clean up.  stopPropagation still waits for engagement, so a
  -- below-threshold ⌘-click reaches the content with its meaning intact.
  , "    e.preventDefault();"
  , "    if (!st.engaged){"
  , "      if (Math.abs(e.clientX-st.sx) + Math.abs(e.clientY-st.sy) < 5) return;"
  , "      engage(e); }"
  , "    e.stopPropagation();"
  , "    track(e);"
  , "  }"
  , "  function up(e){"
  , "    unlisten();"
  , "    if (!st) return;"
  , "    if (!st.engaged){ st = null; return; }"   -- plain ⌘-click: untouched
  , "    e.preventDefault(); e.stopPropagation();"
  , "    track(e);"
  , "    finish(st.lastDst);"
  , "  }"
  , "  function kd(e){"
  , "    if (e.key !== 'Escape' || !st) return;"
  , "    e.preventDefault(); e.stopPropagation();"
  , "    cancel();"
  , "  }"
  -- Belt to the mousemove braces: a dragstart that gets through anyway is
  -- refused outright, so the gesture keeps its mousemove/mouseup stream.  Only
  -- while OUR gesture is armed — the Terminals tree's HTML5 pane drag
  -- ('paneDragJs') never arms one, so it is untouched.
  , "  document.addEventListener('dragstart', function(e){"
  , "    if (st){ e.preventDefault(); e.stopPropagation(); } }, true);"
  -- Losing the window mid-gesture means the mouseup is never coming; forget it
  -- rather than stay armed (an armed gesture makes the NEXT ⌘-drag a no-op).
  , "  window.addEventListener('blur', function(){ cancel(); });"
  , "  document.addEventListener('mousedown', function(e){"
  , "    if (e.button !== 0 || !e[MOD]) return;"
  -- Arming state that outlived its gesture (a native drag that slipped past
  -- both guards, a missed mouseup) must not deafen every later drag.
  , "    if (st) cancel();"
  , "    if (!e.target || !e.target.closest) return;"
  -- A divider owns its own drag; never contest it.
  , "    if (e.target.closest('.terminal-cc-divider, .terminal-cc-native-divider, .tall-divider, .wide1-divider')) return;"
  , "    var src = null;"
  , "    var leaf = e.target.closest('.terminal-cc-leaf[data-leaf]');"
  , "    var cc = leaf && leaf.closest('.terminal-cc[data-lw]');"
  , "    if (leaf && cc && (window.__leksahLwGeom||{})[cc.getAttribute('data-lw')]){"
  , "      src = {kind:'pane', lw: cc.getAttribute('data-lw'),"
  , "             leaf: parseInt(leaf.getAttribute('data-leaf'),10), el: leaf};"
  -- A multi-pane tmux window: the drag picks up the ONE tmux pane under the
  -- pointer (its hl marker box), not the whole window.
  , "      var hls = leaf.querySelectorAll('.terminal-cc-hl[data-pane]');"
  , "      if (hls.length > 1){"
  , "        for (var hi=0; hi<hls.length; hi++){"
  , "          var hr = hls[hi].getBoundingClientRect();"
  , "          if (e.clientX>=hr.left && e.clientX<=hr.right"
  , "              && e.clientY>=hr.top && e.clientY<=hr.bottom){"
  , "            src.kind='tmux'; src.pane=hls[hi].getAttribute('data-pane');"
  , "            src.el=hls[hi]; break; } } }"
  , "    } else {"
  -- The tab ROW first: a plain tab's BUTTON is the obvious handle to pick its
  -- pane up by — for an editor sharing the area with other tabs it is the only
  -- part of it you can be sure is on screen.  Same 'tab' source as its body, and
  -- gated on the same registry, so only the tabs materialize can host arm; an LW
  -- tab button never does (the payload has no way to name a whole window).
  , "      var twb = e.target.closest('.tab-buttons.area-wide0 .tab-wrap[data-tabkey]');"
  , "      var kb = twb && twb.getAttribute('data-tabkey');"
  , "      if (kb && window.__leksahLeafDragTabs && window.__leksahLeafDragTabs[kb]){"
  , "        src = {kind:'tab', tab:kb, el:twb};"
  , "      } else {"
  , "        var tb = e.target.closest('.tab.area-wide0[data-tabkey]');"
  , "        var k = tb && tb.getAttribute('data-tabkey');"
  , "        if (k && window.__leksahLeafDragTabs && window.__leksahLeafDragTabs[k])"
  , "          src = {kind:'tab', tab:k, el:tb}; }"
  , "    }"
  , "    if (!src) return;"
  -- No preventDefault here: below the threshold this must stay an ordinary
  -- ⌘-click for whatever the pane content does with it.
  , "    st = { srcKind:src.kind, srcLw:src.lw, srcLeaf:src.leaf, srcTab:src.tab,"
  , "           srcPane:src.pane, srcEl:src.el, sx:e.clientX, sy:e.clientY,"
  , "           engaged:false, peekEl:null, peekTimer:null, lastDst:{kind:'none'} };"
  , "    document.addEventListener('mousemove', mv, true);"
  , "    document.addEventListener('pointermove', pm, true);"
  , "    document.addEventListener('mouseup', up, true);"
  , "    document.addEventListener('keydown', kd, true);"
  , "  }, true);"
  , "})();"
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

-- | window.leksahSetColorIcons(on): switch every SVG icon under @/pics@ between
-- the monochrome default and the coloured set under @/pics/color@ (same
-- shapes, recoloured).  Icons are plain @<img>@s scattered across many
-- widgets and added/removed dynamically (tree nodes, tabs), so rather than
-- thread the pref through every call site this rewrites the @src@ in place and
-- a MutationObserver keeps newly-added or reflex-updated icons in the current
-- mode.  Rewrites are idempotent (a src already in the target form is skipped),
-- so the observer seeing our own change doesn't loop.
-- | Editor/terminal theming that follows the OS light/dark setting.  The chrome
-- (trees, tabs, panels) already switches via CSS @prefers-color-scheme@ (see
-- "IDE.Web.Theme"); the Monaco and CodeMirror editors and the xterm.js terminals
-- have their own theming, driven here.  @window.leksahRetheme@ reads the six
-- theme-name globals published from the Preferences (@window.__leksahMonacoDark@
-- etc.), picks the light or dark one with
-- @matchMedia('(prefers-color-scheme: dark)')@, and applies it: Monaco via its
-- global @editor.setTheme@, CodeMirror via @LeksahCM.setTheme@, and xterm by
-- iterating @LeksahTerm.byId@ (also stashing the active ITheme in
-- @__leksahXtermActive@ so a terminal created later themes itself in
-- 'register', and setting @--leksah-terminal-bg@ so the pane backing matches).
-- A @matchMedia@ 'change' listener re-runs it when the OS appearance flips.
-- The xterm ITheme palettes live here since xterm has no named themes of its own.
themeSwitchJs :: Text
themeSwitchJs = T.unlines
  [ "(function(){"
  , "  'use strict';"
  , "  var DARK = {"
  , "    foreground:'#e6edf3', background:'#0d1117', cursor:'#e6edf3', cursorAccent:'#0d1117',"
  , "    selectionBackground:'rgba(56,139,253,0.40)',"
  , "    black:'#484f58', red:'#ff7b72', green:'#3fb950', yellow:'#d29922', blue:'#58a6ff', magenta:'#bc8cff', cyan:'#39c5cf', white:'#b1bac4',"
  , "    brightBlack:'#6e7681', brightRed:'#ffa198', brightGreen:'#56d364', brightYellow:'#e3b341', brightBlue:'#79c0ff', brightMagenta:'#d2a8ff', brightCyan:'#56d4dd', brightWhite:'#f0f6fc' };"
  , "  var LIGHT = {"
  , "    foreground:'#1f2328', background:'#ffffff', cursor:'#1f2328', cursorAccent:'#ffffff',"
  , "    selectionBackground:'rgba(84,174,255,0.40)',"
  , "    black:'#24292f', red:'#cf222e', green:'#116329', yellow:'#953800', blue:'#0969da', magenta:'#8250df', cyan:'#1b7c83', white:'#6e7781',"
  , "    brightBlack:'#57606a', brightRed:'#a40e26', brightGreen:'#1a7f37', brightYellow:'#633c01', brightBlue:'#218bff', brightMagenta:'#a475f9', brightCyan:'#3192aa', brightWhite:'#8c959f' };"
  , "  var SOL_DARK = {"
  , "    foreground:'#839496', background:'#002b36', cursor:'#93a1a1', cursorAccent:'#002b36',"
  , "    selectionBackground:'rgba(88,110,117,0.40)',"
  , "    black:'#073642', red:'#dc322f', green:'#859900', yellow:'#b58900', blue:'#268bd2', magenta:'#d33682', cyan:'#2aa198', white:'#eee8d5',"
  , "    brightBlack:'#586e75', brightRed:'#cb4b16', brightGreen:'#586e75', brightYellow:'#657b83', brightBlue:'#839496', brightMagenta:'#6c71c4', brightCyan:'#93a1a1', brightWhite:'#fdf6e3' };"
  , "  var SOL_LIGHT = {"
  , "    foreground:'#657b83', background:'#fdf6e3', cursor:'#586e75', cursorAccent:'#fdf6e3',"
  , "    selectionBackground:'rgba(147,161,161,0.40)',"
  , "    black:'#073642', red:'#dc322f', green:'#859900', yellow:'#b58900', blue:'#268bd2', magenta:'#d33682', cyan:'#2aa198', white:'#eee8d5',"
  , "    brightBlack:'#586e75', brightRed:'#cb4b16', brightGreen:'#586e75', brightYellow:'#657b83', brightBlue:'#839496', brightMagenta:'#6c71c4', brightCyan:'#93a1a1', brightWhite:'#fdf6e3' };"
  , "  window.leksahXtermThemes = { 'leksah-dark':DARK, 'leksah-light':LIGHT, 'solarized-dark':SOL_DARK, 'solarized-light':SOL_LIGHT };"
  , "  function osDark(){ try { return window.matchMedia('(prefers-color-scheme: dark)').matches; } catch(e){ return true; } }"
  , "  window.leksahRetheme = function(){"
  , "    var dark = osDark();"
  , "    var mo = dark ? window.__leksahMonacoDark : window.__leksahMonacoLight;"
  , "    var cm = dark ? window.__leksahCmDark     : window.__leksahCmLight;"
  , "    var xn = dark ? window.__leksahXtermDark  : window.__leksahXtermLight;"
  , "    try { if (mo && window.LeksahMonaco) window.LeksahMonaco.monaco.editor.setTheme(mo); } catch(e){}"
  , "    try { if (cm && window.LeksahCM && window.LeksahCM.setTheme) window.LeksahCM.setTheme(cm); } catch(e){}"
  , "    try {"
  , "      var th = (window.leksahXtermThemes||{})[xn] || DARK;"
  , "      window.__leksahXtermActive = th;"
  , "      var byId = (window.LeksahTerm && window.LeksahTerm.byId) || {};"
  , "      Object.keys(byId).forEach(function(k){ try { byId[k].options.theme = th; } catch(e){} });"
  , "      document.documentElement.style.setProperty('--leksah-terminal-bg', th.background || '');"
  , "    } catch(e){}"
  , "  };"
  , "  try {"
  , "    var mq = window.matchMedia('(prefers-color-scheme: dark)');"
  , "    var h = function(){ window.leksahRetheme(); };"
  , "    if (mq.addEventListener) mq.addEventListener('change', h); else if (mq.addListener) mq.addListener(h);"
  , "  } catch(e){}"
  , "})();"
  ]

-- | Keep context menus on-screen.  The menu is positioned at the raw click
-- coordinates (see 'IDE.Web.Widget.ContextMenu'), so one opened near the bottom
-- or right edge overflows the window.  A MutationObserver spots each
-- @.context-menu@ as it is inserted and, after layout (rAF, so its items are
-- measured), nudges it back inside the viewport — shifting it up/left by its
-- overflow rather than letting it spill off.
contextMenuClampJs :: Text
contextMenuClampJs = T.unlines
  [ "(function(){"
  , "  'use strict';"
  , "  var M = 4;"  -- viewport margin
  , "  function clamp(el){"
  , "    try {"
  , "      var r = el.getBoundingClientRect();"
  , "      var vw = window.innerWidth, vh = window.innerHeight;"
  , "      var top = parseFloat(el.style.top); if (isNaN(top)) top = r.top;"
  , "      var left = parseFloat(el.style.left); if (isNaN(left)) left = r.left;"
  , "      if (top + r.height > vh - M) top = vh - M - r.height;"
  , "      if (left + r.width > vw - M) left = vw - M - r.width;"
  , "      if (top < M) top = M;"
  , "      if (left < M) left = M;"
  , "      el.style.top = top + 'px';"
  , "      el.style.left = left + 'px';"
  , "    } catch (e) {}"
  , "  }"
  , "  function check(n){"
  , "    if (!n || n.nodeType !== 1) return;"
  , "    var el = (n.classList && n.classList.contains('context-menu')) ? n"
  , "           : (n.querySelector && n.querySelector('.context-menu'));"
  , "    if (el) requestAnimationFrame(function(){ clamp(el); });"
  , "  }"
  , "  new MutationObserver(function(muts){"
  , "    for (var i=0;i<muts.length;i++)"
  , "      for (var j=0;j<muts[i].addedNodes.length;j++) check(muts[i].addedNodes[j]);"
  , "  }).observe(document.body, { childList: true, subtree: true });"
  , "})();"
  ]

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

badgesJs :: Bool -> Text
badgesJs browserHosted = T.unlines
  [ "window.__leksahShortcutBadges = false;"
  , "(function(){"
  -- Browser-hosted builds flip tabs with Ctrl (Cmd+` belongs to the OS), so
  -- holding Ctrl reveals the badges there too.
  , "  var BROWSER = " <> (if browserHosted then "true" else "false") <> ";"
  , "  function isMod(e){ return e.key === 'Meta' || (BROWSER && e.key === 'Control'); }"
  , "  function set(on){"
  , "    document.body.classList.toggle('leksah-show-badges',"
  , "        !!(on && window.__leksahShortcutBadges));"
  , "  }"
  -- ⌘-Tab swallows the Meta keyup (the app switcher takes the keyboard even
  -- when you come straight back), so besides keyup/blur, clear whenever any
  -- later event reports the key is no longer held.
  , "  function sync(e){"
  , "    if (!e.metaKey && !(BROWSER && e.ctrlKey)"
  , "        && document.body.classList.contains('leksah-show-badges')) set(false);"
  , "  }"
  , "  window.addEventListener('keydown', function(e){ if (isMod(e)) set(true); else sync(e); }, true);"
  , "  window.addEventListener('keyup',   function(e){ if (isMod(e)) set(false); else sync(e); }, true);"
  , "  window.addEventListener('mousemove', sync, true);"
  , "  window.addEventListener('mousedown', sync, true);"
  , "  window.addEventListener('blur',    function(){ set(false); }, true);"
  -- The native flags-changed monitor's entry point (see leksah-mac-menu.m's
  -- leksah_install_cmdheld_monitor): key events inside a cross-origin iframe
  -- never reach this document, so the DOM listeners above can't track ⌘ while
  -- a browser pane's page has focus.  Idempotent with them otherwise.
  , "  window.leksahCmdHeld = function(on){ set(on);"
  , "    if (on && window.leksahUpdateHints) window.leksahUpdateHints(); };"
  , "})();"
  ]

-- | The Claude-coordination traffic light: a dot in the top-right of the title
-- | What an AI tool wants delivered to the session the user picks: text to type
-- into its composer (an @\@file#Lx-Ly@ reference, an error, a screenshot path),
-- or nothing at all — AI ▸ Focus just goes there.  Computed BEFORE the picker
-- opens, because committing moves focus away from the pane it was read from.
data AIPayload = AIText Text | AIFocusOnly

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
  -- CM views expose .dom; Monaco editors (also tracked via activeView) getDomNode().
  , "var dom=v.dom||(v.getDomNode&&v.getDomNode());"
  , "var ed=dom&&dom.closest&&dom.closest('.editor');"
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
  , "if(v.__leksahMonaco){"                       -- Monaco editor handle
  , "var md=v.getDomNode&&v.getDomNode();var me=md&&md.closest&&md.closest('.editor');"
  , "var mf=me&&me.getAttribute('data-file');if(!mf)return '';"
  , "var ms=v.getSelection();if(!ms)return '';"
  , "return mf+'\\t'+ms.startLineNumber+'\\t'+ms.endLineNumber;}"
  , "var ed=v.dom&&v.dom.closest&&v.dom.closest('.editor');"
  , "var f=ed&&ed.getAttribute('data-file');if(!f)return '';"
  , "var s=v.state.selection.main;"
  , "var a=v.state.doc.lineAt(s.from).number,b=v.state.doc.lineAt(s.to).number;"
  , "return f+'\\t'+a+'\\t'+b;})()" ]

-- bar showing what the Claude Code sessions running right now are doing — the
-- same states, shapes and colours as the macOS menu-bar status item, so the two
-- surfaces never disagree (see 'IDE.Web.ClaudeStatus', the one poll behind
-- both):
--
--   * red triangle — a session is blocked on an approval prompt (needs you)
--   * amber diamond — a session is working (the agent, or a shell command)
--   * green circle — every session is idle, ready for input
--   * grey ring — nothing running
--
-- Distinct shape per state, not just colour (colour-blind accessibility).  The
-- two states that want you also carry HOW MANY sessions are in them, just left of
-- the dot — the glyph says what, the number says how much of it.  Green and grey
-- carry no number: there is no quantity worth reading when everything is idle or
-- nothing is running.  Same pairing on the menu-bar item.
--
-- Each window's reflex network pushes its own updates through
-- @leksahClaudeStatus(state, tooltip, count)@ from the shared poll — never a
-- cross-window JS broadcast.  Hovering lists the sessions.
--
-- The AGENT-COORDINATION state (is it safe to touch leksah while an agent drives
-- it) is set from the shell — @leksah-cmd js eval 'leksahTestStart()'@ (beep,
-- then \"testing\" after 3 s), @'leksahTestEnd()'@, @'leksahRestarting()'@, or
-- @leksahStatus('green'|'orange'|'red'|'blue')@ — and is drawn as a RING AROUND
-- the dot (orange = needed shortly, red = testing now, blue =
-- rebuilding\/restarting; nothing at all when it's safe, so the ring only ever
-- means \"hands off\").  Two independent signals, two independent parts of one
-- indicator: the shape says what the sessions are doing, the ring says whether
-- the UI is yours.  Same ring on the menu-bar item (leksah-mac-menu.m), and the
-- state is spelled out in the last line of the hover text either way.
--
-- The ring is why the dot is a WRAPPER with an inner @.shape@ child: a CSS
-- @clip-path@ clips everything its element paints, pseudo-elements and outline
-- included, so a triangle would have eaten a ring drawn on the same element.
statusLightJs :: Text
statusLightJs = T.unlines
  [ "(function(){"
  -- el = the wrapper (carries the coordination ring), shapeEl = its inner
  -- .shape child (carries the live-session shape); state = the coordination
  -- state.
  , "  var el = null, shapeEl = null, countEl = null, state = 'green', timer = null;"
  -- The live-session state (what the dot draws) and its hover text, pushed by
  -- the reflex network; 'none' until the first push, a tick after boot.
  , "  var claude = 'none', claudeTip = 'Claude: no sessions running', claudeCount = 0;"
  -- Runs BEFORE mainWidgetWithCss rebuilds <body>, which detaches anything we
  -- append now — so (re)create the dot on demand and keep it in whatever <body>
  -- is current, preserving the colour across a rebuild.
  , "  function ensure(){"
  , "    if (el && el.isConnected) return el;"
  , "    if (!document.getElementById('leksah-status-light-css')) {"
  , "      var css = document.createElement('style');"
  , "      css.id = 'leksah-status-light-css';"
  , "      css.textContent ="
  -- The wrapper carries the coordination ring; the inner .shape carries the
  -- session shape.  Sized so the shape sits exactly where the bare dot used to
  -- (18px box at top:3/right:8 = the old 14px box at top:5/right:10), with 2px
  -- of clearance for the ring.
  -- pointer-events:auto ONLY so the hover text appears (the dot has no click
  -- action).  It does not steal the title-bar drag: the native drag watches
  -- NSEvents, not DOM hit-testing (see leksah_configure_titlebar).
  -- NO transition on the ring: WebKit freezes transitions while the window is
  -- occluded, so a fading-in ring would still be invisible when you looked at a
  -- background window — measured (the computed box-shadow sat at the
  -- transition's transparent start value indefinitely).  A hands-off signal
  -- appears at once.
  , "        '#leksah-status-light{position:fixed;top:3px;right:8px;width:18px;height:18px;'+"
  , "        'z-index:2147483647;pointer-events:auto;border-radius:50%}'+"
  , "        '#leksah-status-light .shape{position:absolute;left:2px;top:2px;'+"
  -- Own compositor layer so the glow never forces repaints of content beneath.
  , "        'width:14px;height:14px;opacity:.95;transform:translateZ(0);'+"
  , "        'transition:background .15s,filter .15s}'+"
  -- One shape per state, matching the menu-bar item AND the workspace tree's
  -- badge colours (#f85149 / #d29922 / #3fb950): triangle=needs you,
  -- diamond=working, circle=idle, ring=nothing running.  The glow uses
  -- filter:drop-shadow (not box-shadow) so it follows the clipped shape.
  , "        '#leksah-status-light .shape.c-waiting{background:#f85149;'+"
  , "        'clip-path:polygon(50% 2%,98% 96%,2% 96%);'+"
  , "        'filter:drop-shadow(0 0 1px rgba(0,0,0,.55)) drop-shadow(0 0 5px #f85149)}'+"
  , "        '#leksah-status-light .shape.c-busy{background:#d29922;'+"
  , "        'clip-path:polygon(50% 0,100% 50%,50% 100%,0 50%);'+"
  , "        'filter:drop-shadow(0 0 1px rgba(0,0,0,.55)) drop-shadow(0 0 4px #d29922)}'+"
  , "        '#leksah-status-light .shape.c-idle{background:#3fb950;border-radius:50%;'+"
  , "        'filter:drop-shadow(0 0 1px rgba(0,0,0,.55)) drop-shadow(0 0 4px #3fb950)}'+"
  -- Nothing running: a hollow disc in a grey that reads on either theme, and no
  -- glow — it must not draw the eye.
  , "        '#leksah-status-light .shape.c-none{background:transparent;border-radius:50%;'+"
  , "        'opacity:.6;box-shadow:inset 0 0 0 2px rgba(140,140,140,.9)}'+"
  -- The coordination ring, drawn around the whole dot: an agent has claimed the
  -- UI.  No class (and so no ring) while it's safe — the ring appearing at all
  -- is the signal.  Its own soft glow keeps it legible over toolbar content.
  , "        '#leksah-status-light.coord-orange{box-shadow:inset 0 0 0 2px #ff9500,'+"
  , "        '0 0 5px rgba(255,149,0,.75)}'+"
  , "        '#leksah-status-light.coord-red{box-shadow:inset 0 0 0 2px #ff3b30,'+"
  , "        '0 0 6px rgba(255,59,48,.8)}'+"
  , "        '#leksah-status-light.coord-blue{box-shadow:inset 0 0 0 2px #0a84ff,'+"
  , "        '0 0 6px rgba(10,132,255,.8)}'+"
  -- How many sessions are in the state the shape is showing, to the LEFT of the
  -- dot (its right side is the window edge).  Anchored by `right`, with no width,
  -- so it grows leftwards into empty toolbar as the count gets wider.  A SIBLING
  -- of .shape, never the same element: .shape's clip-path clips everything that
  -- element paints, text included.  pointer-events:none so the dot keeps the
  -- hover text (and nothing steals the title-bar drag).  Only the non-green
  -- states get a class, so idle/none simply have nothing to draw.
  , "        '#leksah-status-light .count{position:absolute;right:20px;top:0;'+"
  , "        'height:18px;line-height:18px;font-size:11px;font-weight:600;'+"
  , "        'font-variant-numeric:tabular-nums;pointer-events:none;'+"
  , "        'white-space:nowrap;text-shadow:0 0 2px rgba(0,0,0,.55)}'+"
  , "        '#leksah-status-light .count.c-waiting{color:#f85149}'+"
  , "        '#leksah-status-light .count.c-busy{color:#d29922}';"
  , "      (document.head || document.documentElement).appendChild(css);"
  , "    }"
  , "    el = document.createElement('div');"
  , "    el.id = 'leksah-status-light';"
  , "    shapeEl = document.createElement('div');"
  , "    el.appendChild(shapeEl);"
  , "    countEl = document.createElement('div');"
  , "    el.appendChild(countEl);"
  , "    paint();"
  , "    if (document.body) document.body.appendChild(el);"
  , "    return el;"
  , "  }"
  -- Both halves of the indicator, from both states.  Kept out of ensure() so a
  -- state change repaints without rebuilding the element.
  , "  function paint(){"
  , "    if (!el) return;"
  , "    el.className = (state === 'green') ? '' : ('coord-' + state);"
  , "    el.title = tipText();"
  , "    if (shapeEl) shapeEl.className = 'shape c-' + claude;"
  -- The count only exists for the states that want you; Haskell already sends 0
  -- for idle/none (csCount), so an empty string is all idle/none needs.
  , "    if (countEl) {"
  , "      countEl.className = 'count' + (claudeCount > 0 ? ' c-' + claude : '');"
  , "      countEl.textContent = claudeCount > 0 ? String(claudeCount) : '';"
  , "    }"
  , "  }"
  -- The coordination line, worded exactly as the menu-bar item's menu line
  -- (leksah_coord_line in leksah-mac-menu.m) — and, like it, EMPTY while it's
  -- safe: the line exists only to say hands off, so the hover text is otherwise
  -- purely about the sessions.
  , "  function coordText(){"
  , "    if (state === 'orange') return 'Leksah: Claude needs it shortly';"
  , "    if (state === 'red')    return 'Leksah: Claude is testing \\u2014 hands off';"
  , "    if (state === 'blue')   return 'Leksah: Claude is rebuilding/restarting';"
  , "    return '';"
  , "  }"
  , "  function tipText(){ var c = coordText(); return claudeTip + (c ? '\\n' + c : ''); }"
  -- Make sure the dot exists, then repaint both halves of it.
  , "  function apply(){ ensure(); paint(); }"
  -- Beep via a NATIVE macOS system sound (the "leksahBeep" script message
  -- handler, see LeksahBeepHandler in leksah-mac-menu.m).  A system sound mixes
  -- with any audio already playing on the machine and never interrupts it —
  -- unlike a Web AudioContext, which grabbed the audio session and silenced
  -- other playback (why the in-page beep had to be disabled).  The handler
  -- exists only on the wkwebview front end; the try/catch no-ops elsewhere.
  , "  function beep(){ try { window.webkit.messageHandlers.leksahBeep.postMessage(1); } catch(e){} }"
  -- Mirror every state change to the native macOS menu-bar status item (the
  -- "leksahStatusItem" handler in leksah-mac-menu.m), so the traffic light is
  -- visible even when leksah is hidden.  try/catch no-ops on the other front
  -- ends, like beep.
  , "  function post(s){ try { window.webkit.messageHandlers.leksahStatusItem.postMessage(String(s)); } catch(e){} }"
  -- A terminal bell (Claude Code's "needs input" signal in a window you're not
  -- viewing): ring the ping, then — once it's had time to sound — SPEAK the
  -- belling terminal's location via the native "leksahSpeak" handler
  -- (NSSpeechSynthesizer, which mixes with other audio just like the ping).
  -- Called from the reflex bell handler with "window <name>, pane <n>".
  , "  window.leksahSpeak = function(t){ try { window.webkit.messageHandlers.leksahSpeak.postMessage(String(t)); } catch(e){} };"
  , "  window.leksahTermBell = function(t){ beep(); setTimeout(function(){ window.leksahSpeak(t); }, 700); };"
  , "  function set(s){ if (timer){ clearTimeout(timer); timer = null; } state = s; apply(); post(s); }"
  , "  window.leksahStatus = set;"
  , "  window.leksahTestStart = function(){"
  , "    if (timer) clearTimeout(timer);"
  , "    state = 'orange'; apply(); post('orange'); beep();"
  , "    timer = setTimeout(function(){ state = 'red'; apply(); post('red'); timer = null; }, 3000);"
  , "  };"
  , "  window.leksahTestEnd = function(){ set('green'); };"
  -- Blue (diamond): Claude is rebuilding/restarting leksah — informational,
  -- distinct from red (an active test).  Set before rebuild-self/restart.
  , "  window.leksahRestarting = function(){ set('blue'); };"
  -- What the dot actually draws: the live Claude sessions, pushed by this
  -- window's reflex network from the shared poll (IDE.Web.ClaudeStatus).  @tip@
  -- is the summary line plus one line per session, @n@ how many sessions are in
  -- state @st@ (0 for idle\/none — the states that get no number).
  , "  window.leksahClaudeStatus = function(st, tip, n){"
  , "    claude = st || 'none';"
  , "    if (tip) claudeTip = tip;"
  , "    claudeCount = (typeof n === 'number' && n > 0) ? n : 0;"
  , "    apply();"
  , "  };"
  -- Land the dot in the final <body> (mainWidget replaces an early append) and
  -- keep it there: a MutationObserver re-appends it only when body's children
  -- change (a reflex rebuild detaches it) — no idle timer, silent while idle.
  , "  ensure();"
  -- Sync the menu-bar item with this window's initial state.  Harmless if the
  -- native handler isn't installed yet (the native side seeds green itself).
  , "  post(state);"
  , "  try { new MutationObserver(function(){ ensure(); }).observe(document.body, { childList: true }); } catch(e){}"
  , "})();"
  ]

-- | The native-browser rect reporter.  Every ~250ms, snapshot each
-- @.browser-native@ pane placeholder (rect, CSS px; visibility = attached,
-- non-trivially sized, and no flipper\/context-menu overlay open — a native
-- view always paints ABOVE the DOM, so it must yield to overlays) and post it
-- to the @leksahBrowserFrame@ script-message handler.  The ObjC side
-- (leksah-mac-menu.m) reconciles: creates a real WKWebView over a pane on
-- first sight, tracks its rect\/visibility\/window, and destroys it once its
-- bid stops reporting (the element left the DOM — pane closed).  After the
-- last pane closes we keep posting empty snapshots for a few seconds so that
-- absence-GC can run; with no panes (and none recently) the tick is a no-op,
-- so the loop is dormant on front ends without native browsers.
--
-- The snapshot also carries the current light\/dark mode (the same
-- @matchMedia('(prefers-color-scheme: dark)')@ signal the rest of the UI
-- themes from, see 'themeSwitchJs'), which the ObjC side turns into an
-- explicit @NSAppearance@ on each browser view so pages inside a pane report
-- the same @prefers-color-scheme@ as leksah itself.  Riding the existing
-- snapshot means new views are born with the right appearance and an OS
-- appearance flip reaches them within one tick — no extra plumbing.
--
-- The same message handler also carries the KEYBOARD HAND-OVER, in both
-- directions, because a native view and the page are two separate first
-- responders and only one of them can have the keys:
--
--   * native → page: a click in a browser view can't reach the DOM, so the
--     native side calls @leksahBrowserActivate(bid)@, which replays it as the
--     bubbling @focusin@ a click on any other pane produces — so the tab float,
--     @lwFocused@ and the pane ring all move exactly as they normally do.
--   * page → native: a trusted @focusin@ on a pane container that holds a
--     browser placeholder posts @{focus: bid}@ (the view takes the first
--     responder); any other trusted @focusin@ posts @{release: wid}@ (the main
--     webview takes it back from that window's browser views).  Without the
--     release, flipping from a browser pane to a terminal moved the ring but
--     left the keystrokes going to the web page.
browserNativeReporterJs :: Text
browserNativeReporterJs = T.unlines
  [ "(function(){"
  , "  if (!(window.webkit && window.webkit.messageHandlers"
  , "        && window.webkit.messageHandlers.leksahBrowserFrame)) return;"
  , "  var H = window.webkit.messageHandlers.leksahBrowserFrame;"
  -- ── Keyboard hand-over between the page and the native views ──────────
  -- A click inside a native view never reaches the DOM, so the native side
  -- reports it here (leksah_browser_notify_activate) and we replay it as the
  -- synthetic focusin a real click on the pane would have bubbled: the
  -- document listener floats the tab (focusTabJs) and the leaf listener sets
  -- lwFocused (TerminalCC) — one dispatch, and every activation consumer
  -- behaves exactly as it does for every other kind of pane.
  , "  window.leksahBrowserActivate = function(bid){"
  , "    var el = document.querySelector('.browser-native[data-bid=\"' + bid + '\"]');"
  , "    if (!el) return;"
  -- The keys are in the page now, so a caret left in this pane's own address
  -- bar is a lie — and it would also read as \"the chrome wants the keyboard\"
  -- to the visibility self-heal below.
  , "    var ae = document.activeElement;"
  , "    if (ae && ae.blur && ae.closest"
  , "        && ae.closest('.browser') === el.closest('.browser')) ae.blur();"
  , "    try { el.dispatchEvent(new FocusEvent('focusin', {bubbles: true})); } catch(e){}"
  , "  };"
  , "  window.leksahBrowserFocusNative = function(bid){"
  , "    try { H.postMessage({focus: bid}); } catch(e){}"
  , "  };"
  -- \"the page needs the keyboard NOW\" — for keyboard UI that can't wait for
  -- the reporter tick to notice it (the flipper: see 'flipperVisibleD').
  , "  window.leksahBrowserRelease = function(){"
  , "    try { H.postMessage({release: (window.leksahWindowId || 0)}); } catch(e){}"
  , "  };"
  -- The other direction: DOM focus moving is how the page says the keyboard is
  -- ITS again, so take the first responder back from whichever browser view
  -- holds it.  Who gives it TO a view is decided in one place only — the pane
  -- widget's focus pulse ('IDE.Web.Widget.Browser'), which knows whether the
  -- pane wants its page or its (empty) address bar; a rule here would race it,
  -- since the tab-body focus below lands 0.2s BEFORE the widget's pulse and a
  -- fresh pane would lose its address bar to its blank page.
  --
  -- Two exemptions: focus on a pane CONTAINER that holds a browser placeholder
  -- (the tab body a select focuses, a view leaf) is the pane itself being
  -- activated, not the page asking for the keyboard — releasing there would
  -- yank it straight back out of the view; and !isTrusted skips our own
  -- dispatch above, so a click in a native view doesn't bounce the keyboard
  -- into the page.  __lkNbAny keeps the listener idle until a pane exists.
  , "  document.addEventListener('focusin', function(e){"
  , "    if (!e.isTrusted || !window.__lkNbAny) return;"
  , "    var t = e.target;"
  , "    if (!(t && t.matches && t.querySelector)) return;"
  , "    var isPaneBox = t.matches('.tab[data-tabkey]')"
  , "                    || t.matches('.terminal-cc-leaf[data-leaf]');"
  , "    if (isPaneBox && t.querySelector('.browser-native')) return;"
  , "    try { H.postMessage({release: (window.leksahWindowId || 0)}); } catch(e){}"
  , "  }, true);"
  -- Should this pane's view hold the keyboard?  Read straight off the classes
  -- the model drives: the enclosing tab is the selected one and the enclosing
  -- split leaf (if any) is the focused one — plus two abstentions.  DOM focus
  -- in this pane's own chrome means the address bar wants the keys; DOM focus
  -- in ANY text control means the page is being typed into (a terminal's xterm
  -- textarea, an editor, a filter box), and taking the keyboard out of a live
  -- caret is never worth it — the pane's own click or select pulse will hand
  -- the keys over when the user actually goes there.
  , "  function isActiveBrowserPane(el){"
  , "    var tab = el.closest('.tab');"
  , "    if (tab && !tab.classList.contains('tab-active')) return false;"
  , "    var leaf = el.closest('.terminal-cc-leaf');"
  , "    if (leaf && !leaf.querySelector('.pane-chrome.active')) return false;"
  , "    var ae = document.activeElement;"
  , "    if (ae && ae.closest && ae.closest('.browser') === el.closest('.browser'))"
  , "      return false;"
  , "    if (ae && ae.matches"
  , "        && ae.matches('input, textarea, select, [contenteditable=\"true\"]'))"
  , "      return false;"
  , "    return true;"
  , "  }"
  , "  function hasPage(bid){"
  , "    var s = (window.__lkNb || {})[bid];"
  , "    return !!(s && s.u && s.u !== 'about:blank');"
  , "  }"
  , "  var wanted = {};"
  , "  var emptyLeft = 0;"
  , "  setInterval(function(){"
  , "    try {"
  , "      var els = document.querySelectorAll('.browser-native');"
  , "      window.__lkNbAny = els.length > 0;"
  , "      if (els.length === 0 && emptyLeft <= 0) return;"
  , "      emptyLeft = els.length > 0 ? 20 : (emptyLeft - 1);"
  , "      var fl = document.querySelector('.flipper');"
  , "      var overlay = (fl && getComputedStyle(fl).display !== 'none')"
  , "                    || !!document.querySelector('.context-menu');"
  , "      var panes = [];"
  , "      els.forEach(function(el){"
  , "        var bid = parseInt(el.getAttribute('data-bid'), 10);"
  , "        if (isNaN(bid)) return;"
  , "        var r = el.getBoundingClientRect();"
  -- Hidden wide0 tabs keep display:block and hide via visibility:hidden
  -- (.tab-hidden), so offsetParent alone misses a tab switch entirely (the
  -- native view stayed painted over the newly-selected tab).  Check computed
  -- visibility AND that the pane is actually the topmost thing at its centre
  -- (elementFromPoint) — which also yields to any DOM stacked above it.
  , "        var vis = !overlay && el.offsetParent !== null"
  , "                  && r.width > 4 && r.height > 4"
  , "                  && getComputedStyle(el).visibility !== 'hidden';"
  , "        if (vis) {"
  , "          var hit = document.elementFromPoint(r.left + r.width/2,"
  , "                                              r.top + r.height/2);"
  , "          vis = !!(hit && el.contains(hit));"
  , "        }"
  , "        panes.push({bid: bid, x: Math.round(r.left), y: Math.round(r.top),"
  , "                    w: Math.round(r.width), h: Math.round(r.height),"
  , "                    vis: !!vis});"
  -- Hand the keyboard to the view on the RISING EDGE of \"this pane should have
  -- it\" — never on every tick, so a wrong answer can't fight the page for the
  -- keys, it can only lose them once.  The edge covers what the widget's select
  -- pulse can't: a page that finishes loading in the already-active pane (at
  -- restore the view is recreated and its page commits AFTER the pulse), and a
  -- flipper that hid every pane and then landed back on the pane it started
  -- from (no select pulse at all).
  , "        var want = vis && isActiveBrowserPane(el) && hasPage(bid);"
  , "        if (want && !wanted[bid]) window.leksahBrowserFocusNative(bid);"
  , "        wanted[bid] = want;"
  , "      });"
  , "      var dark = true;"
  , "      try { dark = window.matchMedia('(prefers-color-scheme: dark)').matches; } catch(e){}"
  , "      window.webkit.messageHandlers.leksahBrowserFrame.postMessage("
  , "        {wid: (window.leksahWindowId || 0), dark: !!dark, panes: panes});"
  , "    } catch(e){}"
  , "  }, 250);"
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
  , "  ov.style.cssText = 'position:fixed;inset:0;z-index:2147483646;cursor:crosshair;background:var(--leksah-scrim-faint)';"
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
  -- Handoff readiness: when this process is the successor, touch the ready file
  -- once window 0's DOM has built, so the supervisor loop knows it may retire
  -- the predecessor (see IDE.Web.Handoff).  Harmless / no-op otherwise.
  when (isHandoffSuccessor && widN == 0) $ do
    handoffPb <- getPostBuild
    performEvent_ (liftIO signalHandoffReady <$ handoffPb)
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
    -- The active pane's soft outward glow: ONE fixed overlay whose position
    -- is tied to the active chrome element (anchor-name --leksah-active-pane,
    -- declared by the .active/.tab-active CSS rules) purely by CSS anchor
    -- positioning — no JS measurement anywhere.  With no anchor on screen the
    -- anchor()/anchor-size() fallbacks park it offscreen at zero size.  See
    -- terminalCss ".leksah-pane-glow".
    elAttr "div" ("class" =: "leksah-pane-glow") blank
    -- …and the per-area variants: tall (anchors --leksah-active-pane-tall;
    -- no border-left, side tabs sit on the screen's left edge) and wide1
    -- (anchors --leksah-active-pane-wide1; rides the bottom bar's auto-hide
    -- transforms).  Split per area because the auto-hide reveals move each
    -- area's content by DIFFERENT transforms, which CSS anchors ignore.
    elAttr "div" ("class" =: "leksah-pane-glow glow-tall") blank
    elAttr "div" ("class" =: "leksah-pane-glow glow-wide1") blank
    -- The active pane's 1px left ring line, as its own anchored overlay
    -- (--leksah-active-left-line, declared only by active markers provably
    -- NOT flush with the area's left edge): the glow's own border-left is
    -- suppressed whenever the side column is hidden (no line at screen
    -- edges), and this element supplies the line for the interior-edge
    -- actives that must keep it.  See terminalCss ".leksah-pane-left-line".
    elAttr "div" ("class" =: "leksah-pane-left-line") blank
    keymapDomE <- keymapWidget showMenubar top
    -- Commands with no 'IDEAction' are handled by matching the keymap event
    -- stream (flipper, next/previous error, focus-alert, …).  The DOM listener
    -- can't see key events while focus is inside a browser pane's cross-origin
    -- iframe, so the native menus carry real key equivalents for them and
    -- inject the command here via the keymap bridge ('IDE.Web.KeymapRequest',
    -- routed to the active window).  Web-menubar clicks of the same commands
    -- (warp front end, where there is no native menu) join through
    -- 'menubarStreamE' below.  Every downstream extractor sees one stream.
    (keymapBridgeE, fireKeymapCmd) <- newTriggerEvent
    let keymapE = leftmost
          [ keymapDomE
          , KeymapCommand <$> leftmost [keymapBridgeE, menubarStreamE] ]
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
        -- Web-menubar clicks of the stream-handled commands (no 'IDEAction';
        -- see keymapBridgeE above) — without this they would be no-ops on the
        -- front ends that show the web menubar.
        menubarStreamE = fmapMaybe (\c -> case c of
          CommandFlipDown      -> Just c
          CommandFlipUp        -> Just c
          CommandNextError     -> Just c
          CommandPreviousError -> Just c
          CommandFocusAlert    -> Just c
          _                    -> Nothing) panelCmdE
    performEvent_ $ ffor panelCmdE $ \case
      CommandFileOpen          -> liftIO runOpenFilePanel
      CommandProjectOpen       -> liftIO runOpenProjectPanel
      CommandProjectOpenFolder -> liftIO runOpenFolderPanel
      _                        -> return ()
    -- The web toolbar/menubar's Preferences command, ⌘, (keymap), or the native
    -- macOS app-menu "Settings…" item (via the bridge) opens the Preferences pane.
    (prefsBridgeE, firePrefsReq) <- newTriggerEvent
    -- The native menu's "Keyboard Shortcuts…" item (Mac/Win32/Gtk) can't touch
    -- reflex state, so it drops a token drained here (mirrors prefsBridgeE).
    (shortcutsBridgeE, fireShortcutsReq) <- newTriggerEvent
    let showPrefsE = leftmost
          [ fmapMaybe (\case CommandShowPreferences -> Just (); _ -> Nothing) panelCmdE
          , fmapMaybe (\e -> case e ^? _KeymapCommand of
                               Just CommandShowPreferences -> Just (); _ -> Nothing) keymapE
          , prefsBridgeE ]
        -- The Edit ▸ Keyboard Shortcuts command, ⌘/ (keymap), or the native menu
        -- item (via the bridge) opens the read-only cheat-sheet pane, like Preferences.
        showShortcutsE = leftmost
          [ fmapMaybe (\case CommandShowShortcuts -> Just (); _ -> Nothing) panelCmdE
          , fmapMaybe (\e -> case e ^? _KeymapCommand of
                               Just CommandShowShortcuts -> Just (); _ -> Nothing) keymapE
          , shortcutsBridgeE ]
    -- View ▸ New Browser Pane, ⌃⌘B (keymap), or the native menu item (via the
    -- bridge): mint a fresh persistent pane id and open its tab.  A terminal
    -- URL click arrives with the URL to load ('Just'); the menu paths start
    -- blank ('Nothing').
    (browserBridgeE, fireBrowserReq) <- newTriggerEvent
    (browserUrlTabE, fireBrowserUrlTab) <- newTriggerEvent
    (browserKeyOpenE, fireBrowserKeyOpen) <- newTriggerEvent
    let openBrowserReqE = leftmost
          [ Nothing <$ fmapMaybe (\case CommandOpenBrowser -> Just (); _ -> Nothing) panelCmdE
          , Nothing <$ fmapMaybe (\e -> case e ^? _KeymapCommand of
                               Just CommandOpenBrowser -> Just (); _ -> Nothing) keymapE
          , Nothing <$ browserBridgeE
          , Just <$> browserUrlTabE ]
    mintedBrowserKeyE <- performEvent $
        ffor openBrowserReqE $ \mu -> liftIO $ do
          n <- nextBrowserId
          mapM_ (rememberUrl n) mu
          return (BrowserKey n)
        -- browserKeyOpenE: a PRE-minted pane (its URL already remembered) —
        -- the ⌥-split fallback path when no split target exists.
    let newBrowserKeyE = leftmost [mintedBrowserKeyE, browserKeyOpenE]
    -- Terminal URL links (see URLRE in terminalLinksJs): plain click → a
    -- browser pane tab on that URL; ⌥-click → mint the pane and route it
    -- through the split-open pipeline (a native split beside the session).
    (urlClickE, fireUrlClick) <- newTriggerEvent
    _ <- liftJSM $ jsg ("window" :: Text) ^. jss ("__leksahOpenUrl" :: Text)
           (fun $ \_ _ args -> case args of
              (u : rest) -> do
                url <- valToText u
                alt <- case rest of (a : _) -> valToBool a; _ -> pure False
                sh  <- case rest of (_ : s : _) -> valToBool s; _ -> pure False
                liftIO $ fireUrlClick (url, alt, sh)
              _ -> return ())
    performEvent_ $ ffor urlClickE $ \(url, alt, sh) -> liftIO $
        if isOwnUrl url
          -- leksah's own UI must never nest inside itself (recursive jsaddle
          -- clients wedge the bridge) — view it in the system browser.
          then openUrl url
          else if alt
            then void . forkIO $ do
                   n <- nextBrowserId
                   rememberUrl n url
                   requestSplitOpen (STBrowser n, sh)
            else fireBrowserUrlTab url

    -- Links in an Agents-pane description (see 'agentLinksJs').  Same two
    -- destinations as a terminal link, opposite default: these are PRs, CI
    -- builds and issues — pages you go and deal with — so a plain click hands
    -- them to the real browser, and ⌥ (⌥⇧ for the other direction) is how you
    -- keep one inside leksah, in a split beside what you were reading.
    (extUrlClickE, fireExtUrlClick) <- newTriggerEvent
    _ <- liftJSM $ jsg ("window" :: Text) ^. jss ("__leksahOpenExtUrl" :: Text)
           (fun $ \_ _ args -> case args of
              (u : rest) -> do
                url <- valToText u
                alt <- case rest of (a : _) -> valToBool a; _ -> pure False
                sh  <- case rest of (_ : s : _) -> valToBool s; _ -> pure False
                liftIO $ fireExtUrlClick (url, alt, sh)
              _ -> return ())
    performEvent_ $ ffor extUrlClickE $ \(url, alt, sh) -> liftIO $
        -- isOwnUrl wins over ⌥: leksah's own UI must never nest inside itself.
        if alt && not (isOwnUrl url)
          then void . forkIO $ do
                 n <- nextBrowserId
                 rememberUrl n url
                 requestSplitOpen (STBrowser n, sh)
          else openUrl url

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
    -- The destination is chosen AFTER the capture (a cancelled grab asks
    -- nothing), so the PNG path is delivered through the AI-session picker like
    -- any other payload.  An explicit `leksah-cmd grab-region TARGET` names a
    -- tmux pane itself and keeps the old one-shot path — no picker.
    performEvent_ $ ffor regionGrabE $ \mbT -> liftIO . void . forkIO $ do
        allowed <- screenCaptureAllowed
        case mbT of
            Just t -> if allowed then void (grabRegionToTarget t) else fireOverlay t
            Nothing
              | allowed -> grabRegionToFile >>= \case
                    -- Left = cancelled, or blocked after all; nothing to send.
                    Right f -> fireAIPayload (AIText (T.pack f <> " "))
                    Left _  -> return ()
              | otherwise -> fireOverlay ""
    performEvent_ $ ffor regionStartOverlayE $ \target -> do
        liftIO $ writeIORef regionTargetRef target
        liftJSM . void $ eval ("window.leksahSelectRegion && window.leksahSelectRegion()" :: Text)
    performEvent_ $ ffor regionRectE $ \rect -> liftIO $ do
        target <- readIORef regionTargetRef
        file <- nextRegionFile
        ok <- requestScreenshotRegion (T.pack file) rect
        when ok $ if T.null target
            then fireAIPayload (AIText (T.pack file <> " "))
            else void $ sendPathToTarget target file

    -- AI menu ▸ Send… / Focus: type an @file / @file#Lx-Ly reference (or the
    -- current error) into the AI terminal, or focus it.  The active editor's
    -- file + selection live in CodeMirror (JS, read via LeksahCM.activeView);
    -- the current error is in IDE state.  Paths are made relative to leksah's
    -- cwd (usually the project root the AI session also runs in).
    -- The payload is computed HERE, before the picker opens, because committing
    -- moves focus to the chosen session — the editor selection has to be read
    -- while the editor still has it.
    (aiActionE, fireAIAction) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextAIAction >>= fireAIAction
    performEvent_ $ ffor (attach (current ide) aiActionE) $ \(ideNow, act) -> do
        let mkRel absf suffix = liftIO $ do
                rel <- makeRelativeToCurrentDirectory (T.unpack absf)
                return (Just (AIText ("@" <> T.pack rel <> suffix <> " ")))
        mPayload <- case act of
          -- Focus carries nothing: the picker still opens, so "go to my AI
          -- session" and "go to a DIFFERENT one" are the same gesture.
          FocusAITerminal -> return (Just AIFocusOnly)
          SendError -> case ideNow ^. currentError of
              Nothing -> return Nothing
              Just lr -> liftIO $ do
                  rel <- makeRelativeToCurrentDirectory (logRefFullFilePath lr)
                  let ln  = srcSpanStartLine (logRefSrcSpan lr)
                      msg = T.takeWhile (/= '\n') (refDescription lr)
                  return . Just . AIText $
                      "@" <> T.pack rel <> "#L" <> T.pack (show ln)
                      <> " " <> msg <> " "
          SendFileRef ->
              liftJSM activeEditorFile >>= \case
                  Just absf -> mkRel absf ""
                  Nothing   -> return Nothing
          SendSelection ->
              liftJSM activeEditorSelection >>= \case
                  Just (absf, a, b) ->
                      mkRel absf $ if a == b
                          then "#L" <> T.pack (show a)
                          else "#L" <> T.pack (show a) <> "-L" <> T.pack (show b)
                  Nothing -> return Nothing
        liftIO $ mapM_ fireAIPayload mPayload

    --
    -- The AI-session picker.  Every AI tool aims at the ACTIVE PANE's default
    -- session and shows the list with that default on top, so you can always see
    -- where the payload is about to go: tap the chord and let go → the default;
    -- keep the modifier down and press again (or ↓/↑) to walk the list; the
    -- modifier coming up commits, as does Enter or a click; Escape cancels.
    --
    -- Commit-on-release works for the same reason the flipper's does: ⌘⌃S is a
    -- native menu key equivalent, so the page never sees the keyDOWN — but a
    -- keyUP is not a key equivalent, so 'rawFlipDoneE' (Command released) does
    -- arrive.  Each overlay gates that signal on its own visibility, so the two
    -- can share it.
    --
    (aiPayloadE, fireAIPayload) <- newTriggerEvent
    -- The payload waiting for a destination, and the pane it came from (captured
    -- on open, so later focus changes can't re-aim it).
    aiPendingRef <- liftIO $ newIORef (Nothing :: Maybe AIPayload)
    (aiKeyE, fireAIKey) <- newTriggerEvent
    aiPb <- getPostBuild
    performEvent_ $ ffor aiPb $ \_ -> liftJSM $ do
        void $ eval aiPickerKeysJs
        void $ jsg ("window" :: Text) ^. jss ("leksahAIPickerKey" :: Text)
               (fun $ \_ _ args -> case args of
                  (v : _) -> valToText v >>= liftIO . fireAIKey
                  _       -> return ())
    -- A payload arriving while the picker is already up is the same chord pressed
    -- again: step, don't reopen (and keep the payload we already hold).
    let aiOpenPayloadE = gate (not <$> current aiPickerVisibleD) aiPayloadE
        aiStepPayloadE = gate (current aiPickerVisibleD) aiPayloadE
    -- Opening reads the live sessions once and snapshots the list, so it can't
    -- change under the highlight while you are choosing.
    -- Gathering the list is NOT frame-thread work (two `ps` calls, sometimes a
    -- tmux round trip and a transcript read), so it happens on a forked thread
    -- and arrives back as an event.  The snapshots it needs — the IDE value and
    -- the pane tree — are taken here, so a later change can't re-aim it.
    (aiOpenRowsE, fireAIRows) <- newTriggerEvent
    performEvent_ $ ffor
        (attach ((,) <$> current ide <*> current allTreeD) aiOpenPayloadE) $
        \((ideNow, tree), payload) -> liftIO $ do
            writeIORef aiPendingRef (Just payload)
            void . forkIO $ do
                let mref = activeAIPaneRef ideNow tree
                (choices, _) <- aiPickerChoices ideNow tree mref
                fireAIRows (mref, choices)
    -- Nothing to offer at all (no session running anywhere, and no directory to
    -- start one in): fall back to what the AI tools did before — the
    -- 'regionCaptureTarget' pref — rather than opening an empty overlay.
    let aiHaveChoicesE = ffilter (not . null . snd) aiOpenRowsE
    performEvent_ $ ffor (attach (current ide) (ffilter (null . snd) aiOpenRowsE)) $
        \(ideNow, _) -> liftIO $ do
            mp <- readIORef aiPendingRef
            writeIORef aiPendingRef Nothing
            let target = regionCaptureTarget (ideNow ^. prefs)
            void . forkIO $ case mp of
                Just (AIText txt) -> void $ sendTextToTarget target txt
                -- Focus-only: resolve the pref's session name to the tmux id
                -- leksah keys its terminal tabs by, and raise that tab.
                _ -> forM_ (aiTargetSession target) $ \sess ->
                        resolveTmuxSessionId sess >>= mapM_ fireTermRequest
    aiRefD   <- holdDyn Nothing (fst <$> aiHaveChoicesE)
    aiItemsD <- holdDyn [] (map (\c -> (choiceKey c, c)) . snd <$> aiHaveChoicesE)
    let aiOpenE   = () <$ aiHaveChoicesE
        aiStepE   = leftmost
          [ True  <$ aiStepPayloadE
          , True  <$ ffilter (== "down") aiKeyE
          , False <$ ffilter (== "up")   aiKeyE ]
        aiCancelE = gate (current aiPickerVisibleD) (() <$ ffilter (== "cancel") aiKeyE)
        aiDoneE   = gate (current aiPickerVisibleD) $
            leftmost [ rawFlipDoneE, () <$ ffilter (== "commit") aiKeyE ]
    -- Failures worth a word (nothing to resume, the session went away between
    -- opening the picker and committing): a transient toast, on the frame thread.
    (aiStatusE, fireAIStatus) <- newTriggerEvent
    performEvent_ $ ffor aiStatusE $ \msg -> liftJSM . void $
        jsg ("window" :: Text) ^. js1 ("__leksahBridgeToast" :: Text)
            ("AI: " <> msg :: Text)
    -- Remembering a pane's chosen session joins the shared-state mutation stream
    -- (below) rather than writing from the delivery thread.
    (aiBindE, fireAIBind) <- newTriggerEvent
    let aiStatus = fireAIStatus
        -- Bring a chosen-but-exited session back: `claude --resume <id>` in the
        -- directory it ran in, then wait for its pane to appear.  Returns the id
        -- that is now live — normally the same one, but if --resume ever mints a
        -- fresh id we adopt whatever turned up in that directory instead of
        -- silently sending nowhere.
        resumeClosedSession :: AIRow -> IO (Maybe Text)
        resumeClosedSession row = do
            let dir = arDir row
                sid = arSession row
            here <- if null dir then return False else doesDirectoryExist dir
            if not here
              -- An archived/deleted worktree: nothing to resume into.
              then return Nothing
              else do
                runClaudeCmd (ClaudeResume dir sid)
                let wait :: Int -> IO Bool
                    wait 0 = return False
                    wait n = paneForSession sid >>= \case
                        Just _  -> return True
                        Nothing -> threadDelay 500000 >> wait (n - 1)
                back <- wait 30            -- ~15s: claude's own start-up
                if back then return (Just sid) else do
                    live <- claudeLiveBySession
                    return $ listToMaybe
                        [ s | (s, l) <- M.toList live, clDir l == dir ]
    -- Arm/disarm the capture-phase key gate, and take the keyboard back from a
    -- native browser pane the instant we open (its WKWebView is a first
    -- responder of its own, and would otherwise swallow the committing keyup —
    -- the same fix the flipper needs).
    performEvent_ $ ffor (updated aiPickerVisibleD) $ \v -> liftJSM . void . eval $
        "window.leksahAIPicker && window.leksahAIPicker("
        <> (if v then "true" else "false") <> ");"
        <> (if v then "window.leksahBrowserRelease && window.leksahBrowserRelease();"
                 else "" :: Text)
    -- Commit: deliver the payload, remember the choice, and go there.
    performEvent_ $ ffor (attach (current aiRefD) aiCommitE) $ \(mref, m) ->
        forM_ (listToMaybe (M.elems m)) $ \choice -> liftIO $ do
            mPayload <- readIORef aiPendingRef
            writeIORef aiPendingRef Nothing
            -- Off the frame thread: this spawns tmux processes, and resuming a
            -- closed session means waiting for it to come up.
            void . forkIO $ case choice of
              AINewSession dir -> do
                  -- A brand-new session has no id yet, so there is nothing to
                  -- bind or to send to; start it and leave the payload for the
                  -- user to re-send once it is up (its pane gets focus).
                  runClaudeCmd (ClaudeNew dir)
              AISessionChoice row -> do
                  -- Explicitly choosing a session makes it this pane's default
                  -- (an unchanged default writes nothing).
                  when (not (arDefault row)) $
                      forM_ mref $ \ref -> fireAIBind (ref, arSession row)
                  msid <- if arLive row then return (Just (arSession row))
                                        else resumeClosedSession row
                  case msid of
                    Nothing -> aiStatus $
                        if null (arDir row)
                          then "that session has exited, and left no directory"
                                 <> " to resume it in"
                          else T.pack (arDir row)
                                 <> " is gone, so that session can't be resumed"
                    Just sid -> do
                      -- A resume that came back under a different id: re-point the
                      -- pane at the one that is actually live.
                      when (sid /= arSession row) $
                          forM_ mref $ \ref -> fireAIBind (ref, sid)
                      forM_ mPayload $ \p -> case p of
                          AIFocusOnly -> return ()
                          AIText txt  -> do
                              ok <- sendToSession sid txt
                              when (not ok) $ aiStatus
                                  "couldn't reach that session's pane to send to it"
                      void $ showLiveSession sid

    let initialTabs =
               WorkspaceKey =: ("tall", Just ())
            <> ErrorsKey    =: ("wide1", Just ())
            <> LogKey       =: ("wide1", Just ())
            <> GrepKey      =: ("wide1", Just ())
            <> ChangesKey   =: ("wide1", Just ())
            <> TerminalsKey =: ("tall", Just ())
            <> AgentsKey    =: ("tall", Just ())
        initialVisibleTabs =
               "tall" =: WorkspaceKey
            <> "wide1" =: LogKey

    -- Clicking a leksah-window tab promotes ITS focused pane in the flip MRU
    -- (terminal tabs are excluded from the generic tab-activation float, and
    -- the old session tiles' select trigger went with them) — without this a
    -- clicked terminal tab never rises in the flipper or the tab row.
    (lwTabClickE, fireLwTabClick) <- newTriggerEvent
    -- Render the bar button(s) for one tab.  Non-terminal tabs get a single
    -- button (label + optional × close).  A terminal gets one button per tmux
    -- window — all selecting the one session body, each with its own detach × —
    -- so a session with two windows shows two tabs (ordered independently, not
    -- grouped), and clicking a window tab also switches the shared terminal to
    -- that window (select-window).
    let
      -- The flipper's item list mapped to wide0 button order (per-window, not
      -- grouped by session); every window/tab button looks up its slot here.
      -- 'ownerOfD' (the pane→owning-lw resolver over the live maps) is defined
      -- beside leksahWindowsD' below and referenced here recursively.
      winOrderListD = buttonOrderOf <$> ownerOfD <*> flipLiveD
      -- The common tab-button shape: a .tab-wrap carrying the selected/hover
      -- highlight (and wide0 MRU order), an optional × close on the left, and a
      -- label button; clicking the label selects `k` in `area` and runs `onSel`.
      tabButton :: Text -> Text -> TabKey -> Dynamic t Bool -> Dynamic t (Map Text Text)
                -> Dynamic t (Maybe Int)
                -> Maybe Text -> Maybe Text -> m () -> IO ()
                -> m (Event t (Map Text TabKey, [TabKey]))
      tabButton area flipKey k selectedD orderStyleD badgeD mbTitle mbCloseTip labelW onSel =
        elDynAttr "span"
            -- data-flipkey lets the ⌘` flip-target hint (hintsJs) find this
            -- button; data-tabkey lets the ⌘-drag pane move (leafDragJs)
            -- report tab-button hovers/drops (resolved back by show-key).
            ((\sel ost -> "class" =: ("tab-wrap" <> if sel then " selected" else "")
                          <> "data-flipkey" =: flipKey
                          <> "data-tabkey" =: T.pack (show k) <> ost)
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
              -- Hidden flip suffix (⌘`, or ⌃` browser-hosted), revealed by
              -- hintsJs when this button is the flip target — so a numbered
              -- tab reads "⌘N ⌘`" as one hint.
              elAttr "span" ("class" =: "leksah-flip-suffix") $ text (flipHintText showMenubar)
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
        LeksahWinKey n -> do
          -- ONE tab per leksah window, and the button IS its own identity
          -- (flipper order / ⌘-badges / hint chips key @Right k@): several
          -- leksah windows on one session each have their own button, and
          -- the flipper's pane items resolve to their owning window's (see
          -- 'flipButtonId'/'paneOwnerLw').  A session-backed window is
          -- labelled by its session name and carries ITS OWN focused tmux
          -- window's alert icon (a leksah-tracked session bell beats it);
          -- a sessionless window is labelled by its first native view.
          let ident = Right k
              fkey  = tabFlipKey k
              lwD'  = (\i -> M.lookup n (i ^. leksahWindows)) <$> ide
              -- The focused-or-first VIEW leaf's icon — what a window with no
              -- tmux window to show (views-only, e.g. a lone browser pane)
              -- carries instead of the idle-terminal glyph.
              viewIcon lw = listToMaybe
                [ src | l <- maybe [] (:[]) (lwFocused lw) ++ treeLeafIds (lwTree lw)
                      , Just pc <- [M.lookup l (lwPanes lw)]
                      , PaneView vk <- [pcKind pc]
                      , Just src <- [tabIconSrc vk] ]
              idleOrView mlw =
                fromMaybe "/pics/tree-window-idle.svg" (mlw >>= viewIcon)
              iconD = (\mlw tree att -> case mlw >>= lwSession of
                        Just s
                          | s `S.member` att -> "/pics/tree-window-bell.svg"
                          | otherwise ->
                              maybe (idleOrView mlw) windowIconSrc
                                (mlw >>= lwFocusedWindow tree)
                        Nothing -> idleOrView mlw)
                      <$> lwD' <*> allTreeD <*> attentionD
              -- First-pane label, for sessionless windows and to tell apart
              -- SEVERAL leksah windows backed by one session.
              contentLabel lw tree = fromMaybe "(empty)" $ listToMaybe $
                  [ case pcKind pc of
                      PaneView (EditorKey f)   -> T.pack (takeFileName f)
                      PaneView (GitLogKey _ b) -> b
                      PaneView k               -> tabLabelText k M.empty
                      PaneTmux w ->
                        let wins = maybe [] snd
                              (lwSession lw >>= \s -> M.lookup s tree)
                        in maybe w windowTabLabel
                             (find ((== w) . twId) wins)
                  | l <- treeLeafIds (lwTree lw)
                  , Just pc <- [M.lookup l (lwPanes lw)] ]
              labelD = (\i names tree -> case M.lookup n (i ^. leksahWindows) of
                        Just lw -> case lwSession lw of
                          Just s ->
                            let base = M.findWithDefault s s names
                                siblings = length
                                  [ () | (_, lw') <- M.toAscList (i ^. leksahWindows)
                                       , lwSession lw' == Just s ]
                            in if siblings <= 1 then base
                               else base <> " · " <> contentLabel lw tree
                          Nothing -> contentLabel lw tree
                        Nothing -> "")
                      <$> ide <*> terminalNamesD <*> allTreeD
              labelW = do
                  void $ elDynAttr' "img"
                      ((\src -> "class" =: "tab-icon term-alert-icon" <> "src" =: src) <$> iconD)
                      (pure ())
                  dynText labelD
              orderStyleD = buttonOrderStyleD ident baseOrderD
              badgeD = M.lookup ident <$> badgeNumsD
          tabButton area fkey k isVisibleD orderStyleD badgeD Nothing Nothing labelW
              (fireLwTabClick n)
        TerminalKey s -> do
          -- RETAINED for remote (ssh://) session tabs (local terminals are
          -- LeksahWinKey tabs now).  Labelled by the session name; the icon
          -- carries the session-level alert.
          let iconD = (\tree att ->
                        let wins = maybe [] snd (M.lookup s tree) in
                        if s `S.member` att
                          then "/pics/tree-window-bell.svg"
                          else maybe "/pics/tree-window-idle.svg" windowIconSrc
                                 (listToMaybe (filter twActive wins ++ wins)))
                      <$> allTreeD <*> attentionD
              labelW = do
                  void $ elDynAttr' "img"
                      ((\src -> "class" =: "tab-icon term-alert-icon" <> "src" =: src) <$> iconD)
                      (pure ())
                  dynText (M.findWithDefault s s <$> terminalNamesD)
              orderStyleD = buttonOrderStyleD (Right k) baseOrderD
              badgeD = M.lookup (Right k) <$> badgeNumsD
          tabButton area (tabFlipKey k) k isVisibleD orderStyleD badgeD Nothing Nothing labelW (pure ())
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
        -- A request may carry a completion slot ('requestSaveActiveFileWait'
        -- callers block on it — the ⌘D save-then-split sequences).
        saveReqE    = leftmost [saveBridgeE, Nothing <$ inPageSaveE]
        -- The file a save request targets: the active editor tab, else the
        -- active leksah window's FOCUSED editor view leaf (pure lookups on
        -- shared state); Nothing = nothing to save.
        saveTargetOf mk i = case mk of
          Just (EditorKey f)    -> Just f
          Just (LeksahWinKey n) -> do
              lw <- M.lookup n (i ^. leksahWindows)
              l  <- lwFocused lw
              case M.lookup l (lwPanes lw) of
                Just (PaneContent (PaneView (EditorKey f)) _) -> Just f
                _                                             -> Nothing
          _ -> Nothing
        saveReqTargetE = attachWith
          (\(mk, i) mv -> (saveTargetOf mk i, mv))
          ((,) <$> current activePaneD <*> current ide) saveReqE
    let saveFileE   = leftmost
          [ fmapMaybe fst saveReqTargetE
          -- The save prompt's "Save" button (dirty editor being closed).
          , promptSaveE
          -- ⌘W on a dirty view-leaf editor saves it before the leaf closes.
          , leafCloseSaveE
          -- ⌘D conversion of a dirty editor saves it first.
          , convertSaveE ]
    (openFileE, fileLineE, lspRefsE, savedFileE, makeEditor) <- editorWidget ide allE saveFileE
    -- Completion routing for waiting save requests: acked when the target
    -- file's editor reports its write settled ('savedFileE'), or at once
    -- when there was nothing to save.  This is what replaced the guessed
    -- \"sleep 250ms and hope the save landed\" in the ⌘D paths.
    pendingSaveAcksRef <- liftIO $ newIORef (M.empty :: M.Map FilePath [MVar ()])
    performEvent_ $ ffor saveReqTargetE $ \(mf, mmv) -> liftIO $
        forM_ mmv $ \mv -> case mf of
          Nothing -> void $ tryPutMVar mv ()
          Just f  -> atomicModifyIORef' pendingSaveAcksRef $ \m ->
                       (M.insertWith (++) f [mv] m, ())
    performEvent_ $ ffor savedFileE $ \f -> liftIO $ do
        mvs <- atomicModifyIORef' pendingSaveAcksRef $ \m ->
                 (M.delete f m, M.findWithDefault [] f m)
        mapM_ (`tryPutMVar` ()) mvs
    -- Change pulses from native view-leaf editors (see leafViewW below).
    (layoutEditorChangedE, fireLayoutEditorChanged) <- newTriggerEvent
    -- Native VIEW LEAVES ('LeafView' in a session's split layout): the tab
    -- widgets, living directly in the layout — no tmux pane underneath, so no
    -- kill-at-any-moment data-loss hazard and NO autosave.  Changes route
    -- through their own trigger into dirtyFilesD / the build trigger (the tab
    -- fan never sees them).
    -- View-leaf bodies get the convertible marker too: in a SESSIONLESS
    -- leksah window there are no tmux pane markers, so hintsJs falls back to
    -- the first shown .leksah-convertible — without it the ⌘D/⌘⇧D chips
    -- never show even though ⌘D works (terminal-beside-view).  Session-backed
    -- windows are unaffected (their shown .terminal-cc-hl wins first).
    -- 'focusE' is the leaf's OWN \"take keyboard focus\" pulse (fired by the
    -- widgets' focus reconciler for the focused leaf only — never the shared
    -- tab select, which had every view grabbing the keyboard at once);
    -- 'focD' gates grab-on-create to the leaf that is actually focused.
    let leafViewW k focusE focD = withConvertHint $ case k of
          EditorKey f -> do
            changeE <- makeEditor f focusE focD
            performEvent_ $ liftIO (fireLayoutEditorChanged f) <$ changeE
          GitLogKey d b -> do
            mon <- monacoEditor . view prefs <$> sample (current ide)
            void $ gitLogWidget mon d b
          ReviewKey d -> do
            mon <- monacoEditor . view prefs <$> sample (current ide)
            void $ reviewWidget mon d
          BrowserKey n -> void $ browserWidget n focusE focD
          _ -> return ()
        -- Mark convertible tab bodies (editor / git-log) so the ⌘-held
        -- navigation hints (hintsJs) can drop their yellow ⌘D / ⌘⇧D chips on
        -- them — here the keys move the tab into a session tab's native split
        -- layout as a view leaf.
        withConvertHint body =
          elAttr "div" ("class" =: "leksah-convertible"
              <> "style" =: "position:relative;width:100%;height:100%") body
    -- File ▸ Open (the native NSOpenPanel on wkwebview) delivers chosen files via
    -- a background thread; open each one in the editor area like any other file.
    (nativeOpenedFileE, fireOpenedFile) <- newTriggerEvent
    -- Terminal tabs requested from outside the reflex network: `leksah-cmd
    -- cc-connect HOST` (a remote control-mode tab keyed "ssh://HOST") and the
    -- workspace-tree repl buttons (a local session id from
    -- 'IDE.Web.RemoteTermRequest.requestLocalTerm').
    (termRequestE, fireTermRequest) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextTermRequest >>= fireTermRequest
    -- Fresh tmux windows from the session-per-open paths (openTerminalInDir /
    -- runClaudeCmd): mint the leksah window and open its tab at once.
    (newLwReqE, fireNewLwReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextNewLwRequest >>= fireNewLwReq
    performEvent_ $ ffor newLwReqE $ \(sid, wid') ->
        liftIO . void . forkIO $ do
            mlwi <- mintLeksahWindow (Just sid) (PaneContent (PaneTmux wid') Nothing)
            mapM_ requestLocalTerm mlwi
    -- ⌘+/⌘− on a tmux pane in a MULTI-pane window (see leafFontAdjust):
    -- isolate the pane first (per-pane fonts can't share a tmux window),
    -- then apply the size to the isolated leaf.
    (fontConvE, fireFontConv) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextFontConvert >>= fireFontConv
    performEvent_ $ ffor fontConvE $ \(lwi, w, f) ->
        liftIO . void . forkIO $ do
            -- Serialized under the consolidate lock: a second font key
            -- arriving while the first conversion's tmux work is in flight
            -- used to start a CONCURRENT conversion (leafFontAdjust's
            -- pane-count check still saw the old window) — the interleaved
            -- conversions duplicated leaves and corrupted the tree.  Inside
            -- the lock the pane count is re-checked: by the time a queued
            -- request runs, the window may already be single-pane (the
            -- previous conversion isolated it) and needs no conversion.
            ok <- withMVar consolidateLock $ \_ -> do
                n  <- paneCountOfWindow w
                ok <- if n > 1 then convertWindowMinimal lwi w else pure True
                when ok $ getGlobalIDERef >>= mapM_ (\r' -> (`reflectIDE` r') $ do
                    ps <- readIDE prefs
                    modifyIDE_ $ \i ->
                      case M.lookup lwi (i ^. leksahWindows)
                             >>= \lw -> (,) lw <$> lwFocused lw of
                        Just (lw, l)
                          | Just (PaneContent _ cur) <- M.lookup l (lwPanes lw) ->
                            let eff = fromMaybe (monospaceFontSize ps) cur
                            in i & leksahWindows
                                   %~ M.adjust (setPaneFont l (f eff)) lwi
                        _ -> i)
                pure ok
            -- The isolated pane's new size may now match a neighbour.
            when ok $ consolidateLw lwi
    -- Consolidation requests from the command layer (a direct leaf font
    -- change, see leafFontAdjust) — same Chan seam as the font converts.
    (consolidateReqE, fireConsolidateReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextConsolidate >>= fireConsolidateReq
    performEvent_ $ ffor consolidateReqE $ \lwi ->
        liftIO . void . forkIO $ consolidateLw lwi
    -- Git log viewer requested from the workspace git tree (a branch click drops
    -- (repo dir, branch) on the GitLogRequest queue); open it as a center tab.
    (gitLogReqE, fireGitLogReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextGitLogRequest >>= fireGitLogReq
    -- Review pane requested from the git tree ("Review Changes…" / "Review
    -- Worktree…") or the Claude worktree flow; opens as a center tab too.
    (reviewReqE, fireReviewReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextReviewRequest >>= fireReviewReq
    -- The Claude task queue ("Claude Task Queue…" on a Claude node) and the
    -- plan-review pane ("Review Plan…" on a session row).  TasksKey is one
    -- global tab; the requesting dir seeds its add-form via the ref.
    tasksSeedRef <- liftIO (newIORef ".")
    (tasksReqE, fireTasksReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ do
        d <- nextTaskQueueRequest
        writeIORef tasksSeedRef d
        fireTasksReq ()
    (planReqE, firePlanReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextPlanReviewRequest >>= firePlanReq
    (compareReqE, fireCompareReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextCompareRequest >>= fireCompareReq
    -- The queue scheduler (one background loop per process; idempotent).
    liftIO armQueueScheduler
    -- Hosts shown as top-level Terminals-tree nodes: the preference list plus
    -- any host that has an open ssh:// tab.
    remoteHostsD <- holdUniqDyn $ (\p rt -> nub $ remoteHosts p ++
          [ T.takeWhile (/= '#') rest
          | (_, TerminalKey n) <- rt, Just rest <- [T.stripPrefix "ssh://" n] ])
        <$> prefsD <*> recentTabs
    -- Native File▸Open / `leksah-cmd editor open` honour the external-editor pref too:
    -- when set, they open in the external editor (line 1) rather than CodeMirror.
    let extActiveMainB = current ((not . T.null . externalEditor) <$> prefsD)
        -- fileLineE is UNGATED (every open, with its line); external-editor
        -- opens are the gated slice, and the built-in slice drives the
        -- backing shell panes below.
        openExternalE = gate extActiveMainB fileLineE
        nativeOpenE0 = (\fp -> EditorKey fp =: ("wide0", Just ()))
                        <$> gate (not <$> extActiveMainB) nativeOpenedFileE
        nativeOpenExtE = (\fp -> (fp, 1)) <$> gate extActiveMainB nativeOpenedFileE
    -- (The hidden backing-twin panes that used to shadow every built-in
    -- editor open in the leksah-editor session are gone with the pane
    -- overlays; 'cleanupStaleTwinPanes' retires leftovers at startup.)
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
    -- A flip step while keyboard focus is inside a browser pane's cross-origin
    -- iframe (the step arrived via the native menu's ⌘` key equivalent — the
    -- DOM keymap can't see keys in there): the ⌘ keyup that commits the flip
    -- would ALSO be invisible, so pull focus back to the page for the duration
    -- of the flip.  blur() alone does NOT move focus out of a cross-origin
    -- iframe in WebKit (activeElement stays the iframe) — focus the body
    -- instead.  The commit refocuses whatever pane it selects.
    performEvent_ $ ffor rawFlipStepE $ \_ -> liftJSM . void . eval $
      ("(function(){var a=document.activeElement;"
       <> "if(a&&a.tagName==='IFRAME'){document.body.tabIndex=-1;"
       <> "document.body.focus();}})()" :: Text)
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
          , () <$ ffilter (any (not . ("ssh://" `T.isPrefixOf`))) treeChangedTabsE
          -- A session just died (its client exited): re-read + reconcile NOW,
          -- so a leksah window healing to sessionless (view panes kept, tab
          -- label back to the view's) does so at once, not on the next
          -- debounced poll.
          , () <$ exitedRawE ]
    -- Same for remote CC tabs: poke the ssh poll.
    performEvent_ $ ffor (ffilter (any ("ssh://" `T.isPrefixOf`)) treeChangedTabsE) $ \_ ->
        liftIO $ fireRemotePoke ()
    -- OFF the reflex thread: a tmux subprocess per poke (term-activity fires
    -- on every window/pane select) would hitch the UI run synchronously.
    (otherPollE, fireOtherPoll) <- newTriggerEvent
    -- Each read also (idempotently) ensures the tmux control-mode monitor is
    -- running — the source of the structural pokes below.  Without this the tab
    -- row would depend on the Terminals side pane having been built to start it.
    performEvent_ $ ffor treeRefreshE $ \_ ->
        liftIO . void . forkIO $ ensureTerminalMonitor >> listTerminalTree >>= fireOtherPoll
    -- A freshly-created terminal, polled once it exists: carries the new tree and
    -- the new session's active-pane flip item, so the pane both appears in the
    -- list and is floated to the MRU front (creating a terminal makes it current,
    -- but no mouse-down set activePaneD to it).  Fired from newTermIdE below; a
    -- trigger event (defined here, fired later) avoids a forward performEvent ref.
    (newTermPolledE, fireNewTermPolled) <- newTriggerEvent
    paneTreeD <- holdUniqDyn =<< holdDyn mempty
      (leftmost [ otherPollE, snd <$> openPollE, fst <$> newTermPolledE ])
    -- A Claude window's tab/flipper label is its session's title
    -- ('tpClaudeTitle' of its active pane), which can change with nothing
    -- tmux-visible happening at all — a @/rename@ inside the session, or its
    -- first prompt landing.  None of the refresh triggers above fire for that
    -- (typing in a terminal isn't a mouse-down, and tmux sees no event at all),
    -- so poll while any Claude pane is open.  Gated on that, so the usual case
    -- costs nothing.
    claudeTitleTick <- tickLossyFromPostBuildTime 5
    let anyClaudeWinD = (\tree -> or
            [ isClaudePane p
            | (_, (_, ws)) <- M.toList tree, w <- ws, p <- twPanes w ]) <$> paneTreeD
    performEvent_ $ ffor (gate (current anyClaudeWinD) claudeTitleTick) $ \_ ->
        liftIO . void . forkIO $ listTerminalTree >>= fireOtherPoll
    -- Pane splits/kills/moves change a window's pane set without adding or
    -- closing a window, and the tab-row tile reads its ACTIVE pane — so the tile
    -- has to be recomputed whenever that set changes.  A mounted control-mode tab
    -- reports its OWN session (TerminalTreeChanged above), but nothing covered a
    -- change made in a session with no open tab, or by an external tmux client.
    -- So listen to the same server-wide control-mode monitor the Terminals tree
    -- uses: it fires on every structural notification (%layout-change included)
    -- for every session on the socket.
    (tmuxStructE, fireTmuxStruct) <- newTriggerEvent
    _ <- liftIO $ registerTerminalRefresh (fireTmuxStruct ())
    -- One logical change arrives as a burst (a window add is add + layout-change
    -- + rename), so read the tree once it settles rather than once per line.
    tmuxStructE' <- debounce 0.3 tmuxStructE
    performEvent_ $ ffor tmuxStructE' $ \_ ->
        liftIO . void . forkIO $ listTerminalTree >>= fireOtherPoll
    -- ONE ssh poll per remote host (10s, off the reflex thread, plus pokes),
    -- feeding BOTH remote surfaces: the Terminals tree's host nodes (all
    -- sessions of each host, with reachability) and the flipper/tab row's
    -- per-tab trees (derived below).  Previously each surface ran its own
    -- ssh — per host AND per open tab.
    remoteTabsD <- holdUniqDyn $
        (\rt -> nub [ n | (_, TerminalKey n) <- rt, "ssh://" `T.isPrefixOf` n ])
          <$> recentTabs
    (hostTreesE, fireHostTrees) <- newTriggerEvent
    -- No periodic remote polling (a high-latency ssh link must not tick): the
    -- host trees load once on post-build and re-load only when the host set
    -- changes or something pokes — a remote window/pane select and remote CC
    -- window add/close both fire 'fireRemotePoke' (see treeChangedTabsE above),
    -- so the active-window highlight and tree still track user actions.
    (remotePokeE, fireRemotePoke) <- newTriggerEvent
    performEvent_ $ ffor (leftmost [ tag (current remoteHostsD) treePb
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
    let notTermKey (TerminalKey _)  = False
        notTermKey (LeksahWinKey _) = False
        notTermKey _                = True
        -- A non-terminal tab gaining focus/click (side bar, bottom bar, editor).
        tabFlipE = fmapMaybe (\mk -> case mk of
                     Just k | notTermKey k -> Just (FlipTab k)
                     _                     -> Nothing) (updated activePaneD)
        -- A leksah-issued pane focus bubbled up from a CC tab (⌘-number split).
        ccPaneFocusE = fmapMaybe (\m -> listToMaybe
                     [ p | (_, dm) <- M.toList m
                         , Just (Identity (TerminalPaneFocused p)) <- [DM.lookup TerminalTab dm] ]) tabE
        -- A leksah-window tab clicked in the tab row: promote its focused
        -- pane (see fireLwTabClick) — a focused VIEW leaf as its 'FlipView',
        -- a tmux window's active pane as its 'FlipPane' (falling back to any
        -- view for a views-only window).  This is what floats a clicked tab
        -- in the flipper/tab order — terminal tabs are excluded from tabFlipE
        -- (a bare activation isn't a pane recency signal) and the pane click
        -- promoter needs a real mousedown.
        lwTabFlipE = fmapMaybe id $ attachWith
          (\(lws, tree) lwi -> do
              lw <- M.lookup lwi lws
              let viewAt ls = listToMaybe
                    [ FlipView lwi l
                    | LeafId l <- ls
                    , Just pc <- [M.lookup (LeafId l) (lwPanes lw)]
                    , PaneView _ <- [pcKind pc] ]
                  focusedView = viewAt (maybe [] (:[]) (lwFocused lw))
                  anyView     = viewAt (treeLeafIds (lwTree lw))
                  tmuxPane = do
                    s   <- lwSession lw
                    win <- lwFocusedWindow tree lw
                    p   <- listToMaybe (filter tpActive (twPanes win) ++ twPanes win)
                    pure (FlipPane s (twIndex win) (tpIndex p))
              case focusedView of
                Just v  -> Just v
                Nothing -> case tmuxPane of
                  Just fp -> Just fp
                  Nothing -> anyView)
          ((,) <$> current leksahWindowsD' <*> current allTreeD) lwTabClickE
        -- Translate a clicked/selected pane id to its FlipPane (pure tree lookup).
        paneFocusFlipE = fmapMaybe id $ attachWith (\tree pid -> flipForPaneId pid tree)
                     (current allTreeD) (leftmost [ jsPaneFocusE, ccPaneFocusE ])
        -- The user went to a terminal's Retry/error page: (deliberate?, session)
        -- from the tab that bubbled TerminalConnErrShown.  Resolve the session
        -- to its current pane via the tree and float it to the flipper MRU
        -- front.  A deliberate navigation (nav) always floats; the page merely
        -- appearing on a drop floats only when that terminal is the visible
        -- wide0 tab, so a background drop doesn't reorder tabs under the user.
        connErrShownE = fmapMaybe (\m -> listToMaybe
                     [ (nav, s) | (TerminalKey s, dm) <- M.toList m
                         , Just (Identity (TerminalConnErrShown nav)) <- [DM.lookup TerminalTab dm] ]) tabE
        connErrFlipE = fmapMaybe id $ attachWith
                     (\(tree, vis) (nav, s) ->
                        if nav || vis == Just (TerminalKey s)
                          then flipForSession s tree else Nothing)
                     ((,) <$> current allTreeD <*> current wide0ActiveD) connErrShownE
        -- A terminal's Retry attempt reconnected: the session id whose tab
        -- bubbled TerminalReconnected (see the raise-window handler below).
        reconnectedE = fmapMaybe (\m -> listToMaybe
                     [ s | (TerminalKey s, dm) <- M.toList m
                         , Just (Identity TerminalReconnected) <- [DM.lookup TerminalTab dm] ]) tabE
        -- A tab focused programmatically (editor opened by workspace double-click
        -- or terminal link) arrives as a focusin.  Float NON-terminal tabs only:
        -- a terminal's programmatic focus also fires on a control-mode follow of
        -- another client, so it is not a trustworthy pane-recency signal — real
        -- terminal focus comes through paneFocusFlipE instead.
        focusFlipE = fmapMaybe id $ attachWith
          (\rt str -> case [ k | (_, k) <- rt, T.pack (show k) == str ] of
             (TerminalKey _ : _)  -> Nothing
             (LeksahWinKey _ : _) -> Nothing
             (k:_)                -> Just (FlipTab k)
             []                   -> Nothing)
          (current recentTabs) focusTabE
        -- Opening a file to navigate to it floats that editor to the MRU front.
        openEditorFlipE = fmapMaybe (fmap FlipTab . listToMaybe . M.keys) openFileE'
        -- Opening Preferences (⌘, / menu) needs an explicit bump (it takes no focus).
        openPrefsFlipE = FlipTab PreferencesKey <$ showPrefsE
        -- The Shortcuts pane (⌘/ / menu) likewise takes no focus — bump it too.
        openShortcutsFlipE = FlipTab ShortcutsKey <$ showShortcutsE
        -- …and a freshly opened browser pane (its address bar takes focus, but
        -- focusin floats only via the generic tab float, which can miss).
        openBrowserFlipE = FlipTab <$> newBrowserKeyE
        -- Every leksah-originated promotion from THIS window (all but the
        -- OS-window-became-key bump, which reads the log held below).
        localFlipBumpE = leftmost [ snd <$> flipSelE
                                  , snd <$> newTermPolledE
                                  , paneFocusFlipE
                                  , lwTabFlipE
                                  , connErrFlipE
                                  , treeSelFlipE
                                  , tabFlipE
                                  , focusFlipE
                                  , openEditorFlipE
                                  , openPrefsFlipE
                                  , openShortcutsFlipE
                                  , openBrowserFlipE ]
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
                Just (TerminalKey _)  -> Nothing
                Just (LeksahWinKey _) -> Nothing
                Just k                -> Just (FlipTab k)
                Nothing               -> Nothing)
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
    performEvent_ $ ffor flipBumpE $ \fi -> wlog wid ("flipBump " <> show fi)
    performEvent_ $ ffor (updated flipMruD) $ \mru -> wlog wid ("flipMru<-shared front=" <> show (take 3 mru))
    -- A terminal's Retry reconnected: if that terminal is STILL this window's
    -- active tab (the user didn't navigate away while it reconnected), bring
    -- this window forward so the reconnected terminal is visible again.  The
    -- event fires in the window that owns the tab, so raising 'widN' is right.
    performEvent_ $ ffor
        (attachWithMaybe (\act s -> if act == Just (TerminalKey s) then Just s else Nothing)
                         (current wide0ActiveD) reconnectedE) $ \s -> do
        wlog wid ("reconnected while active -> raise window " <> T.unpack s)
        liftIO (requestRaiseWindow widN)
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
    hiddenWinsD   <- holdUniqDyn ((^. hiddenWindows) <$> ide)
    flipLiveD <- holdUniqDyn (buildFlipItems <$> leksahWindowsD' <*> flipMruD <*> recentTabs <*> otherTabsD <*> allTreeD <*> hiddenWinsD)
    -- The list the flipper shows.  It updates freely while hidden (labels, tabs),
    -- but the authoritative refresh is a *snapshot taken on open* (openListE),
    -- built from the current shared MRU and the freshly-read tree.  We trust our
    -- own MRU order as-is: no float of "tmux's active pane" (that recency call is
    -- the ambiguous read this design removes).  Relying on @updated flipLiveD@
    -- alone left it stale — after a flip, holdUniqDyn suppresses the (already-
    -- front) re-bump, so the flipper reopened with the pre-flip order.  The
    -- snapshot fires one frame before the flipper actually opens.
    let openListE = attachWith
          (\(lws, mru, rt, rtree, other, hidden) (_, tree) ->
             buildFlipItems lws mru rt other (M.union tree rtree) hidden)
          ((,,,,,) <$> current leksahWindowsD' <*> current flipMruD
                   <*> current recentTabs <*> current remoteFlipD
                   <*> current otherTabsD <*> current hiddenWinsD)
          openPollE
    flipItemsD <- holdDyn [] (leftmost
          [ openListE
          , gate (current (not <$> flipperVisibleD)) (updated flipLiveD) ])
    -- The shared leksah-window map, for pane→owning-tab resolution here.
    leksahWindowsD' <- holdUniqDyn ((^. leksahWindows) <$> ide)
    -- The pane→owning-leksah-window resolver over the live maps, threaded to
    -- everything that maps flip items to buttons/tabs ('paneOwnerLw').
    let ownerOfD = paneOwnerLw <$> leksahWindowsD' <*> allTreeD
    -- The active wide0 tab KEY (not just its session): 'hintTarget' /
    -- 'flipSelHighlight' need to know WHICH leksah window is front to decide
    -- whether a pane is on screen.
    activeWide0KeyD <- holdUniqDyn (M.lookup "wide0" <$> visibleTabsD)
    let flipItemLabel lws names tree fi = case fi of
          FlipTab k      -> tabLabelText k names
          FlipPane n w p -> flipPaneLabel n w p tree
          FlipView n l   -> maybe "(closed view)" (`tabLabelText` names) (viewKeyOf lws n l)
        -- A small square before each entry, coloured by the OS window that owns
        -- the pane — a visual cue of where selecting it will take you.  Only when
        -- there is more than one window (a single window needs no disambiguation).
        flipIconAttr :: Int -> (Map Text LeksahWindow, Map Text (Text, [TmuxWindow]), Map WindowId WebWindow) -> FlipItem -> Map Text Text
        flipIconAttr cnt (lws, tree, wins) fi
          | cnt <= 1 = "style" =: "display:none"
          | otherwise = case flipOwnerWindow lws tree wins fi of
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
        flipTypeIconAttr :: Map Text LeksahWindow -> Map Text (Text, [TmuxWindow]) -> FlipItem -> Map Text Text
        flipTypeIconAttr lws tree fi = case flipIconSrc lws tree fi of
          Just src -> "class" =: flipTypeClass fi <> "src" =: src
          Nothing  -> "style" =: "display:none"
        flipLabel fiD = do
          elDynAttr "span" (flipIconAttr <$> winCountD
                              <*> ((,,) <$> leksahWindowsD' <*> allTreeD <*> webWindowsD) <*> fiD) (pure ())
          elDynAttr "img" (flipTypeIconAttr <$> leksahWindowsD' <*> allTreeD <*> fiD) (pure ())
          dynText $ flipItemLabel <$> leksahWindowsD' <*> terminalNamesD <*> allTreeD <*> fiD
    winCountD <- holdUniqDyn (M.size <$> webWindowsD)
    -- The tab flipper: always opens already-advanced (⌘` = the previous tab), and
    -- has no dismiss key — hence 'never' for both of those inputs.
    (flipperVisibleD, flipperSelD, flipSelIndexD, flipRawE) <-
        flipperWidget flipItemsD flipStepE rawFlipDoneE selfSelectedD
                      never never flipLabel
    -- The AI-session picker, in the same overlay slot (only one is ever up).  It
    -- opens ON entry 0 — the active pane's default session — so a bare chord tap
    -- commits the default, and Escape dismisses it.
    -- Wrapped so the two overlays are told apart in the DOM (both are
    -- '.flipper'); the wrapper is static, so it doesn't disturb the absolute
    -- positioning inside.
    (aiPickerVisibleD, _aiPickerSelD, _aiPickerIdxD, aiCommitE) <-
        divClass "ai-picker-host" $
            flipperWidget aiItemsD aiStepE aiDoneE (pure False)
                          aiOpenE aiCancelE aiChoiceLabel
    -- Thicken THIS window's flipper border when the highlighted item lives here.
    selfSelectedD <- holdUniqDyn $
      (\lws tree wins msel -> case msel of
          Just (_, fi) -> flipOwnerWindow lws tree wins fi == Just wid
          Nothing      -> False)
      <$> leksahWindowsD' <*> allTreeD <*> webWindowsD <*> flipperSelD
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
      (\items lws wins names tree ->
         [ ( flipItemLabel lws names tree fi
           , maybe (-1) (\(WindowId n) -> n) (flipOwnerWindow lws tree wins fi)
           , fromMaybe "" (flipIconSrc lws tree fi) )
         | (_, fi) <- items ])
      <$> flipItemsD <*> leksahWindowsD' <*> webWindowsD <*> terminalNamesD <*> allTreeD
    flipMirrorStateD <- holdUniqDyn $
      (,,) <$> flipperVisibleD <*> flipMirrorItemsD <*> flipSelIndexD
    let flipMirrorShowE = fmapMaybe (\(v, is, i) -> if v then Just (is, i) else Nothing)
                                    (updated flipMirrorStateD)
        flipMirrorHideE = fmapMaybe (\(v, _, _) -> if v then Nothing else Just ())
                                    (updated flipMirrorStateD)
    performEvent_ $ ffor (updated flipperVisibleD) $ \v -> wlog wid ("flipper visible=" <> show v)
    -- The flipper is keyboard-only, and a native browser pane may be holding
    -- the keyboard (its WKWebView is a first responder of its own).  ⌘\` itself
    -- arrives as a menu key equivalent and opens this, but the ⌘ KEYUP that
    -- commits the flip goes to the first responder — so hand the keyboard back
    -- to the page the instant the flipper opens, rather than waiting for the
    -- reporter's next tick to notice the overlay (a lost keyup left the flipper
    -- stuck open).
    performEvent_ $ ffor (ffilter id (updated flipperVisibleD)) $ \_ ->
        liftJSM . void $ eval
            ("window.leksahBrowserRelease && window.leksahBrowserRelease();" :: Text)
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
        classifyFlip (lws, tree, wins) (a, fi) =
          case flipOwnerWindow lws tree wins fi of
            Just w | w /= wid -> Left (w, fi)
            _                 -> Right (a, fi)
        classifiedFlipE = attachWith classifyFlip
            ((,,) <$> current leksahWindowsD' <*> current allTreeD <*> current webWindowsD) flipSelE
        crossFlipE = fmapMaybe (either Just (const Nothing)) classifiedFlipE
        localFlipE = fmapMaybe (either (const Nothing) Just) classifiedFlipE
        flipTabE  = fmapMaybe (\(a, fi) -> case fi of
                        -- A view leaf commits by activating its leksah
                        -- window's tab (the leaf focus rides flipViewE below).
                        FlipTab k    -> Just (M.singleton a k)
                        FlipView n _ -> Just ("wide0" =: LeksahWinKey n)
                        _            -> Nothing) localFlipE
        flipPaneE = fmapMaybe (\(_, fi) -> case fi of FlipPane s w p -> Just (s, w, p); _ -> Nothing) localFlipE
        flipViewE = fmapMaybe (\(_, fi) -> case fi of FlipView n l -> Just (n, l); _ -> Nothing) localFlipE
    performEvent_ $ ffor (attachWith (,) ((,) <$> lwsB <*> current allTreeD) flipPaneE) $
      \((lws, tree), (s, w, p)) -> do
        wlog wid ("ENTER flipPaneE selectTmuxPane " <> show (s, w, p))
        -- Commit the flip target into the MODEL first: the leaf owning the
        -- target window becomes its lw's focused pane, so the tab-select
        -- focus reconciler lands the keyboard there directly.  Leaving this
        -- to the CC client's %session-window-changed round trip let the
        -- reconciler focus the PREVIOUSLY focused leaf first — whose async
        -- focusin then raced (and sometimes overwrote) the late model
        -- write, leaving the flipper "landed" on the old pane.
        liftIO $ forM_ (flipPaneLeaf lws tree s w) $ \(lwi, l) -> setFocusedLeaf lwi l
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
    performEvent_ $ ffor (attachWith (,) ((,) <$> lwsB <*> current allTreeD) crossFlipE) $
      \((lws, tree), (WindowId n, fi)) -> do
        wlog wid ("crossFlip commit -> raise window " <> show n)
        liftIO $ requestRaiseWindow n
        liftIO $ case fi of
          FlipPane s w p -> do
            -- Same up-front model commit as the local flip above.
            forM_ (flipPaneLeaf lws tree s w) $ \(lwi, l) -> setFocusedLeaf lwi l
            case remoteTabHostTarget s of
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
    let openFileE'0 = mapKeys EditorKey <$> openFileE
        -- A file living as a NATIVE PANE in some leksah window must not
        -- reopen as an editor tab: activate that leksah window's tab and
        -- refocus the pane instead.  Applied to the MERGED editor-open
        -- stream, so both in-page opens (tree/goto) and native ones
        -- (File ▸ Open, `leksah-cmd editor open`) are intercepted.
        leafViewOf i k = listToMaybe
          [ lwi | (lwi, lw) <- M.toList (i ^. leksahWindows)
                , l <- treeLeafIds (lwTree lw)
                , Just (PaneContent (PaneView k') _) <- [M.lookup l (lwPanes lw)]
                , k' == k ]
        openFileSplitE = attachWith
          (\i m -> ( [ (lwi, k) | (k, _) <- M.toList m
                                , Just lwi <- [leafViewOf i k] ]
                   , M.filterWithKey (\k _ -> isNothing (leafViewOf i k)) m ))
          (current ide) (leftmost [openFileE'0, nativeOpenE0])
        openFileE' = ffilter (not . M.null) (snd <$> openFileSplitE)
        openLeafE = ffilter (not . null) (fst <$> openFileSplitE)
    performEvent_ $ ffor openLeafE $ \hits ->
      liftIO . void . forkIO $ forM_ hits $ \(lwi, k) -> do
        -- termTabKey resolves a leksah-window id directly, so this raises
        -- the owning tab (in whichever OS window holds it).
        requestLocalTerm lwi
        -- leafOpenPane's already-in-layout branch just refocuses the pane.
        fireLeafOpen (lwi, PaneContent (PaneView k) Nothing, False)
    -- Files open as native view panes anywhere in the leksah windows
    -- (restored or ⌥-opened), for the editor routing set below.
    layoutFileKeysD <- holdUniqDyn $
        (\i -> S.fromList
           [ k | lw <- M.elems (i ^. leksahWindows)
               , l <- treeLeafIds (lwTree lw)
               , Just (PaneContent (PaneView k@(EditorKey _)) _)
                   <- [M.lookup l (lwPanes lw)] ])
        <$> ide
    let layoutFileKeysE = leftmost
          [ ffilter (not . S.null) (updated layoutFileKeysD)
          , ffilter (not . S.null) (tag (current layoutFileKeysD) restorePb) ]
    openFileKeysD <- foldDyn ($) mempty $ leftmost
      [ (\new s -> s <> new) . S.fromList . M.keys <$> openFileE'
      , (\new s -> s <> new) <$> restoreFileKeysE
      -- Editors living as native view leaves in the seeded/edited layouts
      -- (they never flow through openFileE'; the editor event routing needs
      -- to know their files).  Derived from the live layouts, so it also
      -- covers ⌥-opens into leaves.
      , (\new s -> s <> new) <$> layoutFileKeysE
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
        -- Selecting a terminal in the Terminals tree (session / window / pane,
        -- local or remote) is a precise, leksah-issued choice, so float that
        -- terminal to the flipper MRU front — the tree-select path otherwise
        -- brings the tab up without touching recency (see 'treeSelFlipE' in
        -- 'localFlipBumpE').  Resolve each selection to its FlipPane via the tree
        -- (exact pane when given, else the window's / session's active-or-first
        -- pane); remote selects resolve the open tab key with resolveRemoteKey.
        treeRtB = (,) <$> current allTreeD <*> current recentTabs
        treeSelFlipE = fmapMaybe id $ leftmost
          [ attachWith (\tr s       -> flipForSel s Nothing  Nothing  tr) (current allTreeD) selectTermE
          , attachWith (\tr (s,w)   -> flipForSel s (Just w) Nothing  tr) (current allTreeD) selectWinE
          , attachWith (\tr (s,w,p) -> flipForSel s (Just w) (Just p) tr) (current allTreeD) selectPaneE
          , attachWith (\(tr,_)  h              -> flipForSel ("ssh://" <> h)            Nothing  Nothing  tr) treeRtB selRemoteHostE
          , attachWith (\(tr,rt) (h,sid,nm)     -> flipForSel (resolveRemoteKey rt h sid nm) Nothing  Nothing  tr) treeRtB selRemoteE
          , attachWith (\(tr,rt) (h,sid,nm,w)   -> flipForSel (resolveRemoteKey rt h sid nm) (Just w) Nothing  tr) treeRtB selRemoteWinE
          , attachWith (\(tr,rt) (h,sid,nm,w,p) -> flipForSel (resolveRemoteKey rt h sid nm) (Just w) (Just p) tr) treeRtB selRemotePaneE ]
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
    -- Session-restore normalization: one consolidation sweep once the
    -- restored windows and the first reconcile have settled, so layouts
    -- from before the invariant (adjacent same-font tmux windows) converge
    -- to it.  Idempotent + globally locked: several OS windows firing this
    -- is harmless.
    consolidateRestoreE <- delay 5 restorePb
    performEvent_ $ ffor consolidateRestoreE $ \_ ->
        liftIO . void . forkIO $ consolidateAll
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
        -- A saved selection of a tab that is no longer there would leave its area
        -- blank, so Metadata is dropped from it while the pref is off (a session
        -- saved with it selected is the normal case for anyone who had it open
        -- before the pref existed).
        restoreVisibleE = attachWithMaybe
          (\metaOn (ms, _) -> case ms of
             Just s | vis <- [ av | av@(_, k) <- wsVisible s
                                  , metaOn || k /= MetadataKey ]
                    , not (null vis) -> Just (M.fromList vis)
             _ -> Nothing)
          (current metaOnD) restoreE
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
    newTermE <- performEvent $ ffor newNameE $ \nm -> liftIO $ do
      mids <- createTerminalSession nm
      -- Mint the leksah window EAGERLY so the tab (via termTabKey) opens at
      -- once rather than waiting for the reconcile's stray adoption.  KEEP its
      -- id: the tab has to be opened and activated by THAT key, because
      -- resolving the session id through 'leksahWindowsD'' (what 'openInWide0'
      -- does) still answers Nothing here — this very mint has not reached that
      -- Dynamic yet, since it travels via the shared MVar and a resync.  That
      -- is why a new terminal used to appear (put there later by the
      -- reconcile's tab sync) without ever becoming the active pane.
      mlw <- case mids of
        Just (sid, wid') ->
          mintLeksahWindow (Just sid) (PaneContent (PaneTmux wid') Nothing)
        Nothing -> return Nothing
      return (maybe nm fst mids, mlw)
    let newTermIdE = fst <$> newTermE
        -- The freshly minted leksah window, when there is one: opened (and so
        -- activated) by its own key in 'openTabsE', and handed the keyboard
        -- below.
        newTermLwE = fmapMaybe snd newTermE
    -- External-editor opens (when the "External editor command" pref is set):
    -- run e.g. `vim +<line> <file>` as a window in the shared @leksah-editor@
    -- session (file per window-tab); returns that session's id.  Routed exactly
    -- like a new terminal below.
    editTermSidE <- fmapMaybe id <$> performEvent
      (ffor (attach (current prefsD) (leftmost [openExternalE, nativeOpenExtE])) $ \(p, (file, line)) ->
         liftIO $ openFileInEditor file (takeFileName file)
           (words (T.unpack (externalEditor p)) ++ ["+" <> show line, file]))
    let newOrEditTermE = leftmost [newTermIdE, editTermSidE]
    -- Once the new session/window exists, poll the tree and float its active pane
    -- to the MRU front (see newTermPolledE above) so Ctrl-` lists it on top.
    performEvent_ $ ffor newOrEditTermE $ \sid -> liftIO $ do
      tree <- listTerminalTree
      fireNewTermPolled
        (tree, maybe (FlipTab (TerminalKey sid))
                     (\(w, p) -> FlipPane sid w p) (activePaneOfSession sid tree))
    -- …and hand it the keyboard, so it is the ACTIVE pane and not merely the
    -- shown tab.  Keyed by the leksah window (as the tree-launched repl below
    -- explains: with several windows on one session the lw key reaches THIS
    -- one), and sticky — the widget is still mounting, and picks the request up
    -- when it registers its focus callback.
    performEvent_ $ ffor newTermLwE $ liftIO . focusTerminalPane
    -- A repl launched from the workspace tree (termRequestE) has already selected
    -- its window in tmux; treat it exactly like a freshly-created terminal so its
    -- (now-current) active pane floats to the MRU front and the pane takes keyboard
    -- focus — i.e. it becomes THE active pane, not wherever the tree poll lands it.
    -- The payload is either a tmux session id (Terminals-tree paths) or a
    -- LEKSAH-WINDOW id (the session-per-open / leaf-open paths): the MRU bump
    -- and the focus registry are keyed by session, so an lw id must resolve to
    -- its backing session — and its OWN focused window, not whichever window
    -- tmux says is active on the session (another lw's, when they share one).
    -- (Remote "ssh://" requests use their own remote-tree machinery.)
    let localTermRequestE = ffilter (not . ("ssh://" `T.isPrefixOf`)) termRequestE
        lwSessionOf lws n = fromMaybe n (M.lookup n lws >>= lwSession)
    performEvent_ $ ffor (attach (current leksahWindowsD') localTermRequestE) $
      \(lws, n) -> liftIO $ do
        tree <- listTerminalTree
        let sid = lwSessionOf lws n
            bump = case M.lookup n lws of
              Just lw
                | Just win <- lwFocusedWindow tree lw
                , (p:_)    <- filter tpActive (twPanes win) ++ twPanes win
                            -> FlipPane sid (twIndex win) (tpIndex p)
                | otherwise -> FlipTab (LeksahWinKey n)
              Nothing -> maybe (FlipTab (fromMaybe (TerminalKey n) (termTabKey lws n)))
                               (\(w, p) -> FlipPane sid w p)
                               (activePaneOfSession sid tree)
        fireNewTermPolled (tree, bump)
        -- Focus by the LEKSAH-WINDOW id when we have one: with several
        -- windows sharing a session, the session key reaches whichever
        -- widget registered last — the lw key reaches THIS window's.
        focusTerminalPane (if n `M.member` lws then n else sid)
    -- Clicking a row in the Terminals tree (a session/window/pane, local or
    -- remote) must hand the keyboard to the terminal too, not just bring its tab
    -- up.  The tab-selected path focuses only when the *shown* tab actually
    -- changes, so clicking the already-shown session — or another window/pane
    -- within it — switches tmux but leaves the keyboard on the tree.  Fire the
    -- terminal's explicit focus request (past the "did we already own the
    -- keyboard" gate, exactly like a workspace repl launch).  One call is
    -- enough: requests are STICKY — a widget that hasn't registered its focus
    -- callback yet (a fresh ssh tab whose control client is still coming up)
    -- receives the request the moment it registers (see
    -- 'IDE.Web.TerminalInput.focusTerminalPane'), and the remote widget's own
    -- 'forcedFocusE' retry then waits for the panes to actually appear.
    let treeSelectKeyE = leftmost [ selectAnyTermE, remoteOpenKeyE ]
    performEvent_ $ ffor treeSelectKeyE $ liftIO . focusTerminalPane
    -- Bells rung in a terminal's *viewed* (current) window: tmux's alert-bell
    -- hook skips those, so terminalWidget catches them (xterm onBell) and bubbles
    -- TerminalBell up through tabE.  Collect the session id(s) that just belled.
    let termSessionsBy f = ffilter (not . null) $ attachWith
          (\lws m -> [ s | (k, dm) <- M.toList m
                         , Just s <- [tabSessionOf lws k]
                         , Just (Identity e) <- [DM.lookup TerminalTab dm]
                         , f e ])
          (current leksahWindowsD') tabE
        bellSessE = termSessionsBy (\case TerminalBell -> True; _ -> False)
        -- A control-mode tab saw a window created/closed: refresh the trees
        -- now instead of on the next 2s (local) / 10s (ssh) poll.
        treeChangedTabsE = termSessionsBy
          (\case TerminalTreeChanged -> True; _ -> False)
        -- A terminal whose attached tmux client exited (the session ended, e.g.
        -- `exit` in its last window): close its tab so it doesn't linger showing
        -- "[exited]".  Event-driven off tabE — the same safe feedback path as
        -- closeTermE (NOT derived from recentTabs/paneTreeD, which cycles).
        -- EXCEPT a leksah-window tab that still holds native VIEW panes (an
        -- editor whose ⌘D terminal was exited): closing it would yank the
        -- user to another tab, only for the reconcile to resurrect this one a
        -- beat later.  The window heals in place instead — the reconcile
        -- unbinds the dead session, the tab swaps back to the sessionless
        -- renderer, and the focus reconciler puts the keyboard in the
        -- surviving view leaf.
        exitedRawE = ffilter (not . null) $ ffor tabE $ \m ->
          [ k | (k, dm) <- M.toList m
              , Just (Identity TerminalExited) <- [DM.lookup TerminalTab dm] ]
        exitedTermE = ffilter (not . null) $ attachWith
          (\lws ks ->
            [ k | k <- ks
                , case k of
                    LeksahWinKey n -> case M.lookup n lws of
                      Just lw -> null [ () | pc <- M.elems (lwPanes lw)
                                           , PaneView _ <- [pcKind pc] ]
                      Nothing -> True
                    _ -> True ])
          (current leksahWindowsD') exitedRawE
        -- A session whose *displayed* window just closed: rather than follow
        -- tmux's default replacement, activate the ⌘1 button (see handler by the
        -- ⌘-number navigation).  Only meaningful for the session shown in wide0.
        activeWinClosedSessE = termSessionsBy
          (\case TerminalActiveWinClosed -> True; _ -> False)
    -- Only raise attention for a bell you weren't already looking at.  You ARE
    -- looking at it only when it's the active tab AND this OS window currently
    -- has focus — so a bell while leksah is unfocused / in the background (the
    -- usual Claude Code "needs input" case: you've switched to another app, the
    -- terminal is still the active tab) DOES ping.  Reads document.hasFocus() per
    -- bell, hence performEvent rather than a pure gate.
    bellAwayRawE <- performEvent $ ffor
        (attach ((,) <$> current activePaneD <*> current leksahWindowsD')
                bellSessE) $ \((active, lws), ns) -> do
        foc <- liftJSM $ valToBool =<< eval ("document.hasFocus()" :: Text)
        let away = filter (\n -> not (foc
                     && (active >>= tabSessionOf lws) == Just n)) ns
        wlog wid ("bell caught=" <> show ns <> " active=" <> show active
                  <> " focus=" <> show foc <> " ping=" <> show away)
        return away
    let bellAwayE = ffilter (not . null) bellAwayRawE
    -- Which sessions want attention (a viewed-window bell): set on bellAwayE,
    -- cleared when the session becomes the active pane (you focused its tab), and
    -- pruned to still-live sessions.  Drives the 🔔 badge on the tab + tree.
    attentionD <- foldDyn ($) S.empty $ leftmost
      [ (\ns s -> foldr S.insert s ns) <$> bellAwayE
      , (\(lws, mk) s -> case mk >>= tabSessionOf lws of
            Just n -> S.delete n s; _ -> s)
          <$> attach (current leksahWindowsD') (updated activePaneD)
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
    -- The terminal SESSION currently shown in the editor area (for
    -- highlighting in the Terminals list and C-b routing): a leksah window's
    -- backing session, or a remote tab's ssh:// key.
    activeTermD <- holdUniqDyn $ (\vis i -> case M.lookup "wide0" vis of
                                              Just (TerminalKey n)   -> Just n
                                              Just (LeksahWinKey n)  ->
                                                M.lookup n (i ^. leksahWindows) >>= lwSession
                                              _                      -> Nothing)
                     <$> visibleTabsD <*> ide
    -- Publish the active terminal so the Tmux menu can send C-b sequences to it.
    -- ALL of these native-gate publications (active terminal, convertible,
    -- ⌘D view-split override) must also push their INITIAL value: `updated`
    -- alone misses a value already in place when the network builds (a tab
    -- restored active at boot never changes, so no event ever fires).  A
    -- plain postBuild push suffices — a notifier registered later syncs
    -- itself from the refs ('setActiveTerminalNotifier' /
    -- 'setSplitActiveNotifier' replay the current state at registration).
    gatePb <- getPostBuild
    performEvent_ $ liftIO . setActiveTerminal
        <$> leftmost [updated activeTermD, tag (current activeTermD) gatePb]
    -- …and whether the active tab, though not a terminal, can CONVERT to a
    -- tmux pane (⌘D): any editor/git-log tab qualifies — the conversion
    -- pipeline ensures the backing pane on demand, so no registry race here.
    activeConvD <- holdUniqDyn $ (\vis i -> case M.lookup "wide0" vis of
            Just k@(EditorKey _) -> Just k
            Just k@GitLogKey{}   -> Just k
            Just k@ReviewKey{}   -> Just k
            Just k@BrowserKey{}  -> Just k
            -- A leksah window whose FOCUSED pane is a native view: ⌘D splits a
            -- terminal in beside it (the activeViewSplitRef override below runs
            -- before the tab-conversion fallback ever consults this key), but
            -- the native Split items' enablement gate needs to know — a
            -- sessionless views-only window has no active terminal either, so
            -- without this the ⌘D key equivalent is simply disabled.
            Just (LeksahWinKey n) -> do
                lw <- M.lookup n (i ^. leksahWindows)
                l  <- lwFocused lw
                pc <- M.lookup l (lwPanes lw)
                case pcKind pc of
                  PaneView vk -> Just vk
                  _           -> Nothing
            _ -> Nothing) <$> visibleTabsD <*> ide
    performEvent_ $ liftIO . setActiveConvertible
        <$> leftmost [updated activeConvD, tag (current activeConvD) gatePb]
    -- ⌘D override: while the active leksah window's FOCUSED pane is a native
    -- VIEW, ⌘D opens a terminal in the view's directory as a new native
    -- sibling pane (creating + binding a session when the window has none —
    -- the tab widget rebuilds then, so a dirty editor view is saved first)
    -- instead of splitting a tmux pane.
    viewSplitD <- holdUniqDyn $
        (\i mk -> do
            k  <- mk
            n  <- case k of LeksahWinKey n' -> Just n'; _ -> Nothing
            lw <- M.lookup n (i ^. leksahWindows)
            l  <- lwFocused lw
            pc <- M.lookup l (lwPanes lw)
            case pcKind pc of
              PaneTmux w  -> Just (Right w)
              PaneView vk -> do
                dir <- case vk of
                  EditorKey f   -> Just (takeDirectory f)
                  GitLogKey d _ -> Just d
                  ReviewKey d   -> Just d
                  BrowserKey _  -> Just "."   -- no directory of its own: leksah's cwd
                  _             -> Nothing
                return (Left (n, lwSession lw, dir)))
        <$> ide <*> activePaneD
    performEvent_ $ ffor (leftmost [updated viewSplitD, tag (current viewSplitD) gatePb])
        $ \mb -> liftIO $ do
      -- ⌘D / C-b % on a tmux pane target the FOCUSED pane's window.
      setActiveSplitWindow (mb >>= either (const Nothing) Just)
      setActiveViewSplit $ (mb >>= either Just (const Nothing))
        <&> \(lwi, msess, dir0) horiz ->
          void . forkIO $ do
            let dir = dropTrailingPathSeparator dir0
            -- Blocks until the (possibly dirty) editor's write settled —
            -- acked by the editor itself, no guessed sleep.
            requestSaveActiveFileWait
            cmd <- shellCommandForDir dir
            let mkey = Just (T.pack dir <> "#shell")
            case msess of
              Just sid -> newWindowInSession sid dir mkey cmd True
                >>= mapM_ (\w ->
                  fireLeafOpen (lwi, PaneContent (PaneTmux w) Nothing, not horiz))
              Nothing -> do
                name <- freshSessionName (sessionBase dir)
                newSessionWindow name dir mkey cmd True
                  >>= mapM_ (\(sid, w, _) -> do
                    bindLwSession lwi sid
                    fireLeafOpen (lwi, PaneContent (PaneTmux w) Nothing, not horiz))
    -- The ⌘` flipper hint's target: the one-press destination is the second
    -- entry of the MRU flip list (index 0 is the current pane).  Resolve it to
    -- an on-screen pane %id or a tab button and publish to hintsJs.
    hintTargetD <- holdUniqDyn $
        (\owner front tree items -> (snd <$> listToMaybe (drop 1 items)) >>= hintTarget owner front tree)
          <$> ownerOfD <*> activeWide0KeyD <*> allTreeD <*> flipLiveD
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
        (\vis owner front tree msel -> case (vis, msel) of
            (True, Just (_, fi)) -> flipSelHighlight owner front tree fi
            _                    -> (Nothing, ""))
          <$> flipperVisibleD <*> ownerOfD <*> activeWide0KeyD <*> allTreeD <*> flipperSelD
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
    orderedButtonsD <- holdUniqDyn $ (\rt tree lws flip' ->
            let openKeys = S.fromList [ k | (_, k) <- rt ]
                winsOf s = maybe [] (map twIndex . snd) (M.lookup s tree)
                -- A session's button is "open" when any of its leksah
                -- windows is an open tab (or its remote TerminalKey tab is).
                sessionOpen s = TerminalKey s `S.member` openKeys
                  || any (\(i, lw) -> lwSession lw == Just s
                                      && LeksahWinKey i `S.member` openKeys)
                         (M.toAscList lws)
                keep (Left (s, w))            = sessionOpen s && w `elem` winsOf s
                keep (Right (TerminalKey s))  = sessionOpen s
                keep (Right k)                = k `S.member` openKeys
            in filter (keep . fst)
                 (dedupButtons (paneOwnerLw lws tree) (filter ((== "wide0") . fst) flip')))
        <$> recentTabs <*> allTreeD <*> leksahWindowsD' <*> flipLiveD
    -- Panes of the active wide0 window (0 unless it's a terminal): when ≥2, they
    -- take ⌘1…⌘P and the other-window shortcuts start at ⌘(P+1).
    activeWinPaneCountD <- holdUniqDyn $ (\buttons lws tree -> case buttons of
            -- A leksah-window tab: the ⌘-numbered panes are those of ITS
            -- focused tmux window (the leaf the keyboard is in).
            (Right (LeksahWinKey i) : _) -> fromMaybe 0 $ do
                lw  <- M.lookup i lws
                win <- lwFocusedWindow tree lw
                pure (length (twPanes win))
            -- A remote session tile: its tmux-ACTIVE window's panes.
            (Right (TerminalKey s) : _) -> case M.lookup s tree of
                Just (_, wins) -> maybe 0 (length . twPanes) (find twActive wins)
                Nothing        -> 0
            _                 -> 0)
        <$> (map fst <$> orderedButtonsD) <*> leksahWindowsD' <*> allTreeD
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
    -- The displayed window closing used to override tmux's replacement pick
    -- with the ⌘1 (MRU) window button.  With one tile per session there is no
    -- per-window button to redirect to: tmux picks its replacement window and
    -- the CC widget's follow-tmux handler activates it in its stack leaf.
    let _unusedActiveWinClosed = activeWinClosedSessE
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
    -- The 0.5s geometry poll catches the terminal panes drifting under a native
    -- window move/resize; only run it while some pane is actually holed or
    -- snapped, so an idle window with no transparent panes never wakes.
    holesActiveD <- holdUniqDyn $ (\h s -> not (M.null h) || not (M.null s))
                        <$> holedTermsD <*> snappedPanesD
    holeTick <- switchHold never =<< dyn (ffor holesActiveD $ \active ->
        if active then (() <$) <$> tickLossyFromPostBuildTime 0.5
                  else return never)
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
    -- Enumerate OFF the frame thread (remote dirs = ssh round trips), and
    -- re-enumerate remote-project files on RemoteRefresh events (save/build/⟳
    -- — they have no watchers or polling).
    (wsFilesE, fireWsFiles) <- newTriggerEvent
    (remoteEnumRefreshE, fireRemoteEnumRefresh) <- newTriggerEvent
    _ <- liftIO $ registerRemoteRefresh fireRemoteEnumRefresh
    let hasRemoteB = any isRemotePath . (\(dirs, _, _) -> dirs) <$> current enumInputsD
    performEvent_ $ ffor (leftmost
            [ updated enumInputsD
            , tag (current enumInputsD) wsFindPb
            , tag (current enumInputsD) (gate hasRemoteB (() <$ remoteEnumRefreshE)) ]) $
        \(dirs, h, i) -> liftIO . void . forkIO $
            enumerateWorkspaceFiles h i dirs >>= fireWsFiles
    workspaceFilesD <- holdDyn [] wsFilesE
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
    -- The grep runs OFF the frame thread: remote project dirs make it one
    -- ssh round trip per host, and even locally a big workspace grep can
    -- take a while.
    (grepResultsE, fireGrepResults) <- newTriggerEvent
    performEvent_ $ ffor (attach (current ide) grepReqE) $ \(i, (q, fl)) -> liftIO $ do
        let pkgs    = (>>= pjPackages) . fromMaybe [] $ i ^? (workspace . _Just . wsProjects)
            allDirs = nub (map ipdPackageDir pkgs)
            activeD = dropFileName <$> (i ^? workspace . _Just . wsActivePackFile . _Just)
            dirs    = case activeD of
                        Just a  -> a : filter (/= a) allDirs
                        Nothing -> allDirs
        void . forkIO $ runGrep q fl dirs >>= fireGrepResults
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
    -- Close via the menu (⌘W) acts on the active pane.  Non-terminal targets
    -- (editors, and the transient center tabs Preferences / git-log / Shortcuts
    -- — none carry a × so ⌘W is their only close) decide-then-close: a dirty
    -- editor prompts to save, everything else closes at once.  A TERMINAL
    -- instead opens the pane close MENU (Kill Pane / Hide Window / Move Pane to
    -- Hidden Window / Cancel) — see 'termMenuOpenE' / 'menuChoiceE' below.
    let closeTargetE = fmapMaybe id $ attachWith
          (\mk () -> case mk of
              Just k@(EditorKey _)    -> Just k
              Just k@(TerminalKey _)  -> Just k
              Just k@(LeksahWinKey _) -> Just k
              Just k@GitLogKey{}      -> Just k
              Just k@ReviewKey{}      -> Just k
              Just k@TasksKey         -> Just k
              Just k@PlanKey{}        -> Just k
              Just k@CompareKey{}     -> Just k
              Just k@PreferencesKey   -> Just k
              Just k@ShortcutsKey     -> Just k
              Just k@BrowserKey{}     -> Just k
              _ -> Nothing)
          (current activePaneD) closeReqE
        -- ⌘W with a leksah window active whose FOCUSED pane is a native VIEW
        -- (an editor / git log in the split layout): close that pane instead
        -- of opening the tmux pane menu.  A dirty editor pane is SAVED first
        -- (v1 — the tab world's three-way prompt can come later); the layout
        -- edit itself rides the IDEAction stream below.
        leafOfTerm i n = do
            lw <- M.lookup n (i ^. leksahWindows)
            l  <- lwFocused lw
            case M.lookup l (lwPanes lw) of
              Just (PaneContent (PaneView vk) _) -> Just (n, l, vk)
              _                                  -> Nothing
        leafCloseReqE = fmapMaybe id $ attachWith
          (\i k -> case k of LeksahWinKey n -> leafOfTerm i n; _ -> Nothing)
          (current ide) closeTargetE
        -- A dirty editor leaf saves before it closes (leafCloseSaveE joins
        -- saveFileE; the close is delayed past the save).
        leafCloseSaveE = fmapMaybe id $ attachWith
          (\dirty (_, _, vk) -> case vk of
              EditorKey f | f `S.member` dirty -> Just f
              _                                -> Nothing)
          (current dirtyFilesD) leafCloseReqE
        -- Terminals branch off to the close menu, keyed by the BACKING
        -- session (unless the focused pane is a view — that closes above);
        -- everything else keeps decide-then-close.
        termCloseReqE = fmapMaybe id $ attachWith
          (\i k -> case k of
              TerminalKey n -> Just n
              LeksahWinKey n | Nothing <- leafOfTerm i n ->
                  M.lookup n (i ^. leksahWindows) >>= lwSession
              _ -> Nothing)
          (current ide) closeTargetE
        nonTermCloseE = fmapMaybe
          (\case TerminalKey _ -> Nothing; LeksahWinKey _ -> Nothing
                 k -> Just k) closeTargetE
        -- A dirty editor is held for a save prompt (below); everything else
        -- closes straight away.
        decidedCloseE = attachWith
          (\dirty k -> case k of
              EditorKey f | f `S.member` dirty -> Left k
              _                                -> Right k)
          (current dirtyFilesD) nonTermCloseE
        promptCloseE = fmapMaybe (either Just (const Nothing)) decidedCloseE
        directCloseE = fmapMaybe (either (const Nothing) Just) decidedCloseE
        -- A tab × still closes immediately; so does ⌘W / File ▸ Close of a clean
        -- non-terminal tab.  A terminal's no-tmux fallback also detaches the whole
        -- session here.  ("Hide Window" does NOT detach — it hides the pane's tmux
        -- window via 'hiddenWindows'; see the close-menu wiring below.)
        detachCloseE = leftmost [ tabCloseBtnE, (:[]) <$> directCloseE
                                , termFallbackDetachE ]
    -- Close the view leaf: clean leaves at once; a dirty editor leaf the
    -- moment its save SETTLES (savedFileE, editor-acked) — sequenced, not
    -- a guessed delay.
    let leafCloseNowE = attachWithMaybe
          (\dirty (n, l, vk) -> case vk of
              EditorKey f | f `S.member` dirty -> Nothing
              _                                -> Just (n, l))
          (current dirtyFilesD) leafCloseReqE
        leafCloseArmE = attachWithMaybe
          (\dirty (n, l, vk) -> case vk of
              EditorKey f | f `S.member` dirty -> Just (f, (n, l))
              _                                -> Nothing)
          (current dirtyFilesD) leafCloseReqE
    pendingLeafCloseD <- foldDyn ($) M.empty $ leftmost
          [ uncurry M.insert <$> leafCloseArmE
          , M.delete <$> savedFileE ]
    let leafCloseSavedE = attachWithMaybe (flip M.lookup)
          (current pendingLeafCloseD) savedFileE
        delayedLeafCloseE = leftmost [ leafCloseNowE, leafCloseSavedE ]
    -- Editors whose CodeMirror buffer differs from disk: a CM edit surfaces as an
    -- EditorTab event keyed by file; Save (saveFileE) and closing the tab
    -- (closeTabsE) clear the flag.
    let editorChangedFilesE = ffor tabE $ \m ->
          [ f | (EditorKey f, dm) <- M.toList m, Just _ <- [DM.lookup EditorTab dm] ]
    dirtyFilesD <- foldDyn ($) S.empty $ leftmost
          [ (\fs s -> foldr S.insert s fs)                        <$> editorChangedFilesE
          -- …and native view-leaf editors (see leafViewW).
          , S.insert                                              <$> layoutEditorChangedE
          , S.delete                                              <$> saveFileE
          , (\ks s -> foldr S.delete s [ f | EditorKey f <- ks ]) <$> closeTabsE ]
    -- ⌘D on a plain editor / git-log TAB (splitActiveTerminal → the
    -- ConvertRequest bridge) MATERIALIZES it: the tab becomes a new leksah
    -- window holding the view beside a fresh terminal in the view's
    -- directory (its own new tmux session; ⌘⇧D stacks them).  A dirty
    -- editor is saved first (convertSaveE joins saveFileE), the old tab
    -- closes (leafConvertDoneE joins closeTabsE).  Views no pane may hold
    -- (Shortcuts) no-op.
    (convertReqE, fireConvertReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextConvertRequest >>= fireConvertReq
    let convertSaveE = attachWithMaybe
          (\dirty (k, _) -> case k of
              EditorKey f | f `S.member` dirty -> Just f
              _                                -> Nothing)
          (current dirtyFilesD) convertReqE
        -- Convert at once when clean; a dirty editor converts the moment its
        -- save SETTLES (savedFileE) — sequenced, not a guessed delay.
        convertNowE = attachWithMaybe
          (\dirty (k, h) -> case k of
              EditorKey f | f `S.member` dirty -> Nothing
              _                                -> Just (k, h))
          (current dirtyFilesD) convertReqE
        convertArmE = attachWithMaybe
          (\dirty (k, h) -> case k of
              EditorKey f | f `S.member` dirty -> Just (f, (k, h))
              _                                -> Nothing)
          (current dirtyFilesD) convertReqE
    pendingConvertD <- foldDyn ($) M.empty $ leftmost
          [ uncurry M.insert <$> convertArmE
          , M.delete <$> savedFileE ]
    let convertReadyE = leftmost
          [ convertNowE
          , attachWithMaybe (flip M.lookup) (current pendingConvertD) savedFileE ]
    (leafConvertDoneE, fireLeafConvertDone) <- newTriggerEvent
    performEvent_ $ ffor convertReadyE $ \(k, horiz) ->
        liftIO . void . forkIO $ when (viewLeafAllowed k) $ do
          let dir = dropTrailingPathSeparator $ case k of
                EditorKey f   -> takeDirectory f
                GitLogKey d _ -> d
                ReviewKey d   -> d
                _             -> "."
          name <- freshSessionName (sessionBase dir)
          cmd  <- shellCommandForDir dir
          newSessionWindow name dir (Just (T.pack dir <> "#shell")) cmd True
            >>= mapM_ (\(sid, wid', _) -> do
              mlwi <- mintLeksahWindow (Just sid) (PaneContent (PaneView k) Nothing)
              forM_ mlwi $ \lwi -> do
                fireLeafOpen (lwi, PaneContent (PaneTmux wid') Nothing, not horiz)
                fireLeafConvertDone [k]
                requestLocalTerm lwi)
    -- ⌥-open-into-split: holding Option while opening from the workspace splits
    -- the ACTIVE pane and puts the item there (⌥⇧ = the other direction, like
    -- ⌘⇧D) instead of opening a new tab.  Reuses the convert machinery:
    --   * resolve the active pane — a terminal's active pane, else convert a
    --     convertible active tab first (its backing pane), else give up;
    --   * place the item into a fresh split half: a file/git-log by joining its
    --     backing pane in beside the active one (pane-scoped run keys make the
    --     overlay follow — cf. the drag-survival feature); a terminal/claude by
    --     a plain split (Stage B).
    -- When the active tab is neither a terminal nor convertible (or is remote,
    -- which can't cross tmux servers) it falls back to a normal open.
    (splitOpenReqE, fireSplitOpen) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextSplitOpenRequest >>= fireSplitOpen
    -- ⌥-open into a leksah window: split its focused pane with the new
    -- content (⌥⇧ = below) — or just refocus the pane when the same content
    -- is already in the layout.  Rides the IDEAction mutation stream.
    (leafOpenE, fireLeafOpen) <- newTriggerEvent
    let leafOpenPane pc vert lw
          | Just l <- listToMaybe
              [ l | l <- treeLeafIds (lwTree lw)
                  , Just pc' <- [M.lookup l (lwPanes lw)]
                  , samePane (pcKind pc') (pcKind pc) ]
          = lw { lwFocused = Just l }
          | otherwise =
              let tgt = case lwFocused lw of
                          Just l | l `M.member` lwPanes lw -> Just l
                          _ -> listToMaybe (treeLeafIds (lwTree lw))
              in case tgt of
                   Nothing -> lw
                   Just l  -> snd (splitLeaf l (if vert then SplitV else SplitH)
                                     True pc lw)
        samePane (PaneView a) (PaneView b) = a == b
        samePane (PaneTmux a) (PaneTmux b) = a == b
        samePane _            _            = False
    performEvent_ $ ffor (attachWith (\(ww, i) req -> (ww, i, req))
                            (current (zipDyn myWinD ide)) splitOpenReqE) $
      \(ww, i, (target, vertical)) ->
      liftIO . void . forkIO $ do
        let horiz     = not vertical
            -- Plain opens: terminal/Claude items mint a NEW tmux session per
            -- open (inside openTerminalInDir / runClaudeCmd, via the
            -- requestNewLw seam); files/git logs stay ordinary tabs.
            normalOpen = case target of
              STFile f            -> deliverOpenedFile f
              STGitLog d b        -> requestGitLog d b
              STBrowser n         -> fireBrowserKeyOpen (BrowserKey n)
              STTermDir d         -> openTerminalInDir d
              STClaudeNew d       -> runClaudeCmd (ClaudeNew d)
              STClaudeContinue d  -> runClaudeCmd (ClaudeContinue d)
              STClaudeResume d i' -> runClaudeCmd (ClaudeResume d i')
              STClaudeFork d i'   -> runClaudeCmd (ClaudeResumeFork d i')
              STClaudeAsk f       -> runClaudeCmd (ClaudeAsk f)
              STRunCmd keep d ks nm c -> runInTerminal keep d ks nm c
            -- A terminal-family item (a plain shell, or a claude command): a
            -- real split of the active pane running the command.
            placeShell activePid cwd mcmd mkey = splitPane horiz activePid cwd mcmd >>= \case
              Nothing  -> return ()
              Just pid -> do
                forM_ mkey $ \k ->
                  tmuxCmd ["set-option", "-p", "-t", T.unpack pid, "@leksah_run", T.unpack k]
                tmuxCmd ["select-pane", "-t", T.unpack pid]
                sessionOfPane activePid >>= mapM_ requestLocalTerm
            placeClaude activePid ccmd = do
              (d, key, line') <- claudeCommandLine ccmd
              placeShell activePid d (Just line') (Just key)
        mTerm <- getActiveTerminal
        -- ⌥-split dispatch, against the active LEKSAH WINDOW's focused pane:
        --   * a file / git log becomes a native VIEW pane beside it;
        --   * a terminal/claude beside a tmux pane is a tmux split-window of
        --     THAT window's active pane (tmux-first);
        --   * a terminal/claude beside a VIEW pane gets a fresh detached
        --     tmux window in the backing session, joined as a native sibling.
        -- With no leksah window active everything falls back to the session
        -- path / a plain open.
        let activeLw = case _wwActive ww of
              Just (LeksahWinKey n) | n `M.member` (i ^. leksahWindows) -> Just n
              _                                                         -> Nothing
            focusedPane = do
              lwi <- activeLw
              lw  <- M.lookup lwi (i ^. leksahWindows)
              l   <- lwFocused lw
              pc  <- M.lookup l (lwPanes lw)
              return (lwi, lw, pc)
            -- (dir, run key, command, keep-shell) for a terminal-family item.
            claudeSpec c = do
              (d', key, line') <- claudeCommandLine c
              return (d', Just key, line', False)
            termSpec = case target of
              STTermDir d | not ("ssh://" `T.isPrefixOf` T.pack d) -> Just $ do
                  let dir = dropTrailingPathSeparator d
                  cmd <- shellCommandForDir dir
                  return (dir, Just (T.pack dir <> "#shell"), cmd, True)
              STClaudeNew d       -> Just (claudeSpec (ClaudeNew d))
              STClaudeContinue d  -> Just (claudeSpec (ClaudeContinue d))
              STClaudeResume d i' -> Just (claudeSpec (ClaudeResume d i'))
              STClaudeFork d i'   -> Just (claudeSpec (ClaudeResumeFork d i'))
              STClaudeAsk f       -> Just (claudeSpec (ClaudeAsk f))
              STRunCmd keep d ks _ c | not ("ssh://" `T.isPrefixOf` T.pack d) -> Just $ do
                  let dir = dropTrailingPathSeparator d
                  full <- prefixedCmdLine dir c
                  return (dir, Just (T.pack dir <> "#" <> ks), full, keep)
              _ -> Nothing
        -- ⌥-open while a PLAIN convertible tab (editor / git log) is active:
        -- MATERIALIZE it into a leksah window first (a dirty editor saves via
        -- the active-file save bridge), then split with the new item.
        let activePlainView = case _wwActive ww of
              Just k0 | viewLeafAllowed k0, isNothing activeLw -> Just k0
              _ -> Nothing
            materialize k0 msess pc = do
              -- Blocks until the dirty editor's write settled (editor-acked).
              requestSaveActiveFileWait
              mlwi <- mintLeksahWindow msess (PaneContent (PaneView k0) Nothing)
              forM_ mlwi $ \lwi -> do
                fireLeafOpen (lwi, pc, vertical)
                fireLeafConvertDone [k0]
                requestLocalTerm lwi
        -- A native view splitting a MULTI-pane tmux window must land beside
        -- the ACTIVE tmux pane, which tmux can't host — minimal-path
        -- conversion isolates that pane into its own leaf first (the
        -- conversion leaves it focused, so the view splits it).
        let isolateFocusedThen act = do
              case focusedPane of
                Just (lwi', _, PaneContent (PaneTmux fw) _) -> do
                    cnt <- paneCountOfWindow fw
                    when (cnt > 1) . void $ convertWindowMinimal lwi' fw
                _ -> return ()
              act
        case (target, activeLw) of
          (STFile f, Just lwi) -> isolateFocusedThen $
              fireLeafOpen (lwi, PaneContent (PaneView (EditorKey f)) Nothing, vertical)
          (STGitLog d b, Just lwi) -> isolateFocusedThen $
              fireLeafOpen (lwi, PaneContent (PaneView (GitLogKey d b)) Nothing, vertical)
          (STBrowser n, Just lwi) -> isolateFocusedThen $
              fireLeafOpen (lwi, PaneContent (PaneView (BrowserKey n)) Nothing, vertical)
          (STFile f, Nothing) | Just k0 <- activePlainView ->
              materialize k0 Nothing (PaneContent (PaneView (EditorKey f)) Nothing)
          (STGitLog d b, Nothing) | Just k0 <- activePlainView ->
              materialize k0 Nothing (PaneContent (PaneView (GitLogKey d b)) Nothing)
          (STBrowser n, Nothing) | Just k0 <- activePlainView ->
              materialize k0 Nothing (PaneContent (PaneView (BrowserKey n)) Nothing)
          _ | Just k0 <- activePlainView, Just spec <- termSpec -> do
              (dir, mkey, cmd, keep) <- spec
              name <- freshSessionName (sessionBase dir)
              newSessionWindow name dir mkey cmd keep >>= \case
                Just (sid, w, _) ->
                    materialize k0 (Just sid) (PaneContent (PaneTmux w) Nothing)
                Nothing -> normalOpen
          _ -> case (focusedPane, termSpec) of
            -- Terminal beside a VIEW pane: new detached window in the
            -- backing session → native sibling.
            (Just (lwi, lw, PaneContent (PaneView _) _), Just spec)
              | Just sid <- lwSession lw, not ("ssh://" `T.isPrefixOf` sid) -> do
                  (dir, mkey, cmd, keep) <- spec
                  newWindowInSession sid dir mkey cmd keep >>= \case
                    Just w  -> fireLeafOpen (lwi, PaneContent (PaneTmux w) Nothing, vertical)
                    Nothing -> normalOpen
            _ -> do
              -- tmux-first: split the focused tmux pane's WINDOW (else the
              -- active session's pane, else fall back to a plain open).
              mActivePid <- case focusedPane of
                Just (_, _, PaneContent (PaneTmux w) _) -> activePaneIdOfWindow w
                _ -> case mTerm of
                  Just sid | "ssh://" `T.isPrefixOf` sid -> return Nothing
                           | otherwise                   -> activePaneIdOfSession sid
                  Nothing -> return Nothing
              case mActivePid of
                Nothing        -> normalOpen
                Just activePid -> case target of
                  -- (Files/git logs with a leksah window active were handled
                  -- above; with none they fall to normalOpen via mActivePid.)
                  STFile f            -> deliverOpenedFile f
                  STGitLog d b        -> requestGitLog d b
                  STBrowser n         -> fireBrowserKeyOpen (BrowserKey n)
                  STTermDir d         -> placeShell activePid d Nothing Nothing
                  STClaudeNew d       -> placeClaude activePid (ClaudeNew d)
                  STClaudeContinue d  -> placeClaude activePid (ClaudeContinue d)
                  STClaudeResume d i' -> placeClaude activePid (ClaudeResume d i')
                  STClaudeFork d i'   -> placeClaude activePid (ClaudeResumeFork d i')
                  STClaudeAsk f       -> placeClaude activePid (ClaudeAsk f)
                  -- A command split mirrors 'runInTerminal''s window: prefix
                  -- applied, and a shell kept always (keep) or only after a
                  -- FAILING command, so errors stay visible but a clean run
                  -- closes the split.
                  STRunCmd keep d ks _ c -> do
                      let dir = dropTrailingPathSeparator d
                      full <- prefixedCmdLine dir c
                      placeShell activePid dir
                        (Just (full <> (if keep then " ; " else " || ")
                                    <> "exec \"${SHELL:-bash}\" -l"))
                        (Just (T.pack dir <> "#" <> ks))
    -- ⌘D moved the tab into a session layout as a native view leaf: close it.
    let convertCloseE = leafConvertDoneE
    -- ══ ⌘-DRAG PANE MOVE (leafDragJs is the gesture; see its comment) ═════
    -- Three per-window callbacks land here: leaf-peek (show a hovered tab
    -- without activating it — tabsWidget's peekTabE input), and the ONE
    -- drop/cancel call, dispatched in a forked thread with every model edit
    -- a single atomic modifyIDE_ (the modifyLeksahWindow pattern — the
    -- reconcile recomputes from the live IDE, so an atomic edit can't be
    -- clobbered; cross-session tmux surgery additionally holds the
    -- conversion guard so no reconcile runs between move-window and the
    -- model edit).
    (leafPeekReqE, fireLeafPeekReq) <- newTriggerEvent
    (leafSelectReqE, fireLeafSelectReq) <- newTriggerEvent
    (leafDropRawE, fireLeafDrop) <- newTriggerEvent
    let resolveWide0Key order str = listToMaybe
          [ k | (k, _) <- order, T.pack (show k) == str ]
        leafPeekE = attachWithMaybe
          (\order str -> ("wide0" =:) <$> resolveWide0Key order str)
          (current wide0OrderD) leafPeekReqE
        -- Cancel with a plain-tab source: a REAL select of that tab (flips
        -- any peek back and refocuses the tab body).
        leafSelectTabE = attachWithMaybe
          (\order str -> ("wide0" =:) <$> resolveWide0Key order str)
          (current wide0OrderD) leafSelectReqE
    -- The draggable plain wide0 tabs (viewLeafAllowed — what materialize can
    -- host), published for leafDragJs's source/destination checks.
    dragTabsPb <- getPostBuild
    performEvent_ $ ffor (leftmost [ updated wide0OrderD
                                   , tag (current wide0OrderD) dragTabsPb ]) $
      \order -> liftJSM . void . eval $
        let esc = T.replace "'" "\\'" . T.replace "\\" "\\\\"
        in "window.__leksahLeafDragTabs = {"
           <> T.intercalate ","
                [ "'" <> esc (T.pack (show k)) <> "':1"
                | (k, _) <- order, viewLeafAllowed k ]
           <> "};"
    performEvent_ $ ffor leafDropRawE $ \json ->
      liftIO . void . forkIO $ getGlobalIDERef >>= mapM_ (\ideR -> do
        let parsed = parseLeafDrop =<< decodeStrict' (encodeUtf8 json)
            -- "Nothing happened": activate the dragged pane — its window
            -- flips back visible (the drag can only start in the visible
            -- tab) and the pane takes the focus, exactly as if clicked.
            activateSrc src = case src of
              LDPane slwId slInt -> do
                (`reflectIDE` ideR) $ modifyIDE_ $ leksahWindows %~
                  M.adjust (\lw -> if LeafId slInt `M.member` lwPanes lw
                                     then lw { lwFocused = Just (LeafId slInt) }
                                     else lw) slwId
                requestLocalTerm slwId
              LDTmuxPane slwId slInt p -> do
                activateSrc (LDPane slwId slInt)
                selectTmuxPaneId p
              LDTab kStr -> fireLeafSelectReq kStr
            -- Land the source beside/at `spec` in leksah window dlwId.
            dropInto src dlwId spec = case src of
              LDPane slwId slInt -> do
                let sl = LeafId slInt
                if slwId == dlwId
                  then do   -- same window: pure move, LeafId preserved
                    lws0 <- (`reflectIDE` ideR) (readIDE leksahWindows)
                    (`reflectIDE` ideR) $ modifyIDE_ $ leksahWindows %~
                      M.adjust (moveLeafInWindow sl spec) slwId
                    -- Focus-follow: a moved tmux pane's window becomes the
                    -- session's current window (see the cross-window case).
                    case M.lookup slwId lws0 >>= (M.lookup sl . lwPanes) of
                      Just (PaneContent (PaneTmux w) _) -> selectTmuxWindowId w
                      _ -> return ()
                    requestLocalTerm dlwId
                  else do
                    lws0 <- (`reflectIDE` ideR) (readIDE leksahWindows)
                    case (M.lookup slwId lws0, M.lookup dlwId lws0
                         ,M.lookup slwId lws0 >>= (M.lookup sl . lwPanes)) of
                      (Just slw0, Just dlw0, Just pc) -> do
                        -- A tmux pane crossing to a DIFFERENT session must
                        -- reparent its window first (keepPane prunes
                        -- wrong-session panes), under the conversion guard.
                        case (pcKind pc, lwSession slw0, lwSession dlw0) of
                          (PaneTmux w, Just ss, Just ds) | ss /= ds -> do
                            beginConversion
                            moveTmuxWindow w ds
                            (`reflectIDE` ideR) $ modifyIDE_ $
                              syncLwTabs . moveLeafAcross slwId sl dlwId spec
                            endConversion
                          _ ->
                            (`reflectIDE` ideR) $ modifyIDE_ $
                              syncLwTabs . moveLeafAcross slwId sl dlwId spec
                        -- Land the focus ON the moved pane: make its window
                        -- current in the (possibly new) session, or the CC
                        -- widget's focus-follow snaps back to the
                        -- destination's current window.
                        case pcKind pc of
                          PaneTmux w -> selectTmuxWindowId w
                          _          -> return ()
                        requestLocalTerm dlwId
                      _ -> activateSrc src
              -- ONE tmux pane of a multi-pane window: landing on a leaf whose
              -- tmux window has the SAME font joins it there as a real tmux
              -- split (full-size, on the dropped edge — including rearranging
              -- within its own window); any other target (node/root split,
              -- view leaf, font mismatch) breaks it out into a fresh tmux
              -- window + leaf inheriting the source leaf's font.
              LDTmuxPane slwId slInt p -> do
                lws0 <- (`reflectIDE` ideR) (readIDE leksahWindows)
                defFont <- monospaceFontSize
                             <$> (`reflectIDE` ideR) (readIDE prefs)
                case ( M.lookup dlwId lws0
                     , M.lookup slwId lws0 >>= (M.lookup (LeafId slInt) . lwPanes)
                     , M.lookup slwId lws0 >>= lwSession ) of
                  (Just dlw0, Just (PaneContent (PaneTmux ws) sfont), mss) -> do
                    srcPanes <- panesOfWindow ws
                    let joinTarget = do
                          l <- listToMaybe [ l' | (pth, Just l', _)
                                                    <- subtreeRects (lwTree dlw0)
                                                , pth == dsPath spec ]
                          PaneContent (PaneTmux wd) dfont <- M.lookup l (lwPanes dlw0)
                          -- effective sizes: Nothing = the global pref
                          guard (fromMaybe defFont dfont == fromMaybe defFont sfont)
                          pure (l, wd)
                    if p `notElem` srcPanes
                      then activateSrc src
                      else if length srcPanes <= 1
                        -- the pane IS its window: the whole-leaf move
                        then dropInto (LDPane slwId slInt) dlwId spec
                        else case joinTarget of
                          Just (l, wd) -> do
                            -- -t must not resolve to the moved pane itself
                            -- (own-window rearrange): aim at another pane.
                            tgt <- fromMaybe wd . listToMaybe . filter (/= p)
                                     <$> panesOfWindow wd
                            joinTmuxPaneFull p tgt (dsOrient spec == SplitH)
                                                   (not (dsAfter spec))
                            setFocusedLeaf dlwId l
                            selectTmuxPaneId p
                            requestLocalTerm dlwId
                          Nothing -> case lwSession dlw0 <|> mss of
                            Nothing -> activateSrc src
                            Just ds -> do
                              -- The new window (strayable) + the leaf insert
                              -- must be atomic wrt the reconcile, exactly
                              -- like moveLeafAcross's cross-session arm.
                              beginConversion
                              mW <- breakTmuxPaneTo p ds
                              case mW of
                                Nothing -> do
                                  endConversion
                                  activateSrc src
                                Just newW -> do
                                  (`reflectIDE` ideR) $ modifyIDE_ $ \i ->
                                    case M.lookup dlwId (i ^. leksahWindows) of
                                      Nothing -> i
                                      Just dlw ->
                                        let newId = LeafId (lwNext dlw)
                                            t'    = insertLeafAt spec newId (lwTree dlw)
                                        in if newId `elem` treeLeafIds t'
                                             then i & leksahWindows %~ M.insert dlwId dlw
                                               { lwTree    = t'
                                               , lwPanes   = M.insert newId
                                                   (PaneContent (PaneTmux newW) sfont)
                                                   (lwPanes dlw)
                                               , lwFocused = Just newId
                                               , lwZoomed  = Nothing
                                               , lwNext    = lwNext dlw + 1
                                               , lwSession = lwSession dlw <|> Just ds }
                                             else i
                                  endConversion
                                  selectTmuxWindowId newW
                                  requestLocalTerm dlwId
                  _ -> activateSrc src
              LDTab kStr -> do
                wws <- (`reflectIDE` ideR) (readIDE webWindows)
                case [ k | ww <- M.elems wws, k <- _wwWide0 ww
                         , T.pack (show k) == kStr ] of
                  (k:_) | viewLeafAllowed k -> do
                    -- The dragged plain tab is the active one (the drag
                    -- started on its visible body): settle a dirty editor
                    -- exactly like the ⌘D pipeline, then it becomes a fresh
                    -- view leaf and its tab closes.
                    requestSaveActiveFileWait
                    (`reflectIDE` ideR) $ modifyIDE_ $ \i ->
                      case M.lookup dlwId (i ^. leksahWindows) of
                        Nothing -> i
                        Just dlw ->
                          let newId = LeafId (lwNext dlw)
                              t'    = insertLeafAt spec newId (lwTree dlw)
                          in if newId `elem` treeLeafIds t'
                               then i & leksahWindows %~ M.insert dlwId dlw
                                 { lwTree    = t'
                                 , lwPanes   = M.insert newId
                                     (PaneContent (PaneView k) Nothing)
                                     (lwPanes dlw)
                                 , lwFocused = Just newId
                                 , lwZoomed  = Nothing
                                 , lwNext    = lwNext dlw + 1 }
                               else i
                    fireLeafConvertDone [k]
                    requestLocalTerm dlwId
                  _ -> activateSrc src
            -- The target is ONE tmux pane of a multi-pane window: leaf @l@ of
            -- @dlwId@ holds its window @wd@ (the caller has checked both, and
            -- that @ttPane@ is still one of that window's panes).  A dragged
            -- tmux pane of the same effective font joins that pane's own cell
            -- with tmux — no leksah split at all, and no @-f@, so the window's
            -- other panes keep their sizes.  Anything else CANNOT live inside a
            -- tmux window (a view, a plain tab, a whole multi-pane window, a
            -- differently-sized terminal), so the aimed-at pane is first
            -- isolated into a leaf of its own ('convertWindowPane') and the
            -- ordinary leaf-level drop finishes the job — landing where the
            -- shadow promised, because the conversion mirrors tmux's own cell
            -- proportions.
            dropOntoTmuxPane src dlwId l wd tt = do
              lws0 <- (`reflectIDE` ideR) (readIDE leksahWindows)
              defFont <- monospaceFontSize
                           <$> (`reflectIDE` ideR) (readIDE prefs)
              let fontOf lwId lf = fromMaybe Nothing $
                    fmap pcFontSize (M.lookup lwId lws0
                                       >>= M.lookup lf . lwPanes)
                  sameFont slwId sl =
                    fromMaybe defFont (fontOf slwId sl)
                      == fromMaybe defFont (fontOf dlwId l)
                  srcLeaf = case src of
                    LDTmuxPane slwId slInt p -> Just (slwId, LeafId slInt, Just p)
                    LDPane slwId slInt       -> Just (slwId, LeafId slInt, Nothing)
                    LDTab _                  -> Nothing
              -- The pane a tmux-level join would move: the dragged pane itself,
              -- or the lone pane of a dragged terminal leaf (that pane IS its
              -- window, so it can join like any other).  Nothing = the drag has
              -- to become a leksah split instead.
              joinable <- case srcLeaf of
                Just (slwId, sl, mp)
                  | sameFont slwId sl
                  , Just (PaneTmux ws) <- fmap pcKind (M.lookup slwId lws0
                                            >>= M.lookup sl . lwPanes) -> do
                      sps <- panesOfWindow ws
                      return $ case mp of
                        Nothing -> case sps of
                          [only] -> Just (slwId, sl, only, True)
                          _      -> Nothing
                        Just p
                          | p `elem` sps -> Just (slwId, sl, p, length sps <= 1)
                          | otherwise    -> Nothing
                _ -> return Nothing
              case joinable of
                Just (slwId, sl, ps, lastPane)
                  | ps /= ttPane tt -> do
                      -- The source window dying (its last pane left) and the
                      -- leaf going with it must be one step, like
                      -- moveLeafAcross: no emptied window is ever observed.
                      beginConversion
                      joinTmuxPane ps (ttPane tt) (ttOrient tt == SplitH)
                                                  (not (ttAfter tt)) False
                      when lastPane $ (`reflectIDE` ideR) $ modifyIDE_ $
                        syncLwTabs . (leksahWindows %~ \lws ->
                          case M.lookup slwId lws >>= withoutLeaf sl of
                            Just slw' -> M.insert slwId slw' lws
                            Nothing   -> M.delete slwId lws)
                      endConversion
                      setFocusedLeaf dlwId l
                      selectTmuxPaneId ps
                      requestLocalTerm dlwId
                  | otherwise -> activateSrc src   -- onto itself: nothing to do
                Nothing -> do
                  -- Isolate the target pane, then re-find its leaf: the tree
                  -- has changed under us but the pane KEPT its leaf id.
                  _ <- convertWindowPane dlwId wd (ttPane tt)
                  lws1 <- (`reflectIDE` ideR) (readIDE leksahWindows)
                  case [ pth | Just dlw <- [M.lookup dlwId lws1]
                             , (pth, Just l', _) <- subtreeRects (lwTree dlw)
                             , l' == l ] of
                    (pth:_) -> dropInto src dlwId
                                 (DropSpec pth (ttOrient tt) (ttAfter tt))
                    []      -> activateSrc src
        forM_ parsed $ \(src, dst) -> case dst of
          LDDstNone -> activateSrc src
          LDDstPane dlwId ptr box mtt -> do
            lws0 <- (`reflectIDE` ideR) (readIDE leksahWindows)
            case M.lookup dlwId lws0 of
              Nothing -> activateSrc src
              Just dlw0 -> do
                -- A tmux pane the JS aimed at counts only while the model still
                -- agrees it exists: leaf still there, still a tmux window, pane
                -- still one of its own.  Otherwise the pointer decides, exactly
                -- as before.
                tmuxTgt <- case mtt of
                  Nothing -> return Nothing
                  Just tt -> case M.lookup (LeafId (ttLeaf tt)) (lwPanes dlw0) of
                    Just (PaneContent (PaneTmux wd) _) -> do
                      ps <- panesOfWindow wd
                      return $ if ttPane tt `elem` ps
                                 then Just (LeafId (ttLeaf tt), wd, tt)
                                 else Nothing
                    _ -> return Nothing
                case tmuxTgt of
                  Just (l, wd, tt) -> dropOntoTmuxPane src dlwId l wd tt
                  Nothing -> do
                    let excl = case src of
                          LDPane slwId slInt | slwId == dlwId -> Just (LeafId slInt)
                          _ -> Nothing
                    case pickDropTarget ptr box excl (lwZoomed dlw0)
                                        (lwTree dlw0) of
                      Nothing   -> activateSrc src
                      Just spec -> dropInto src dlwId spec
          LDDstTab tstr
            -- dropping a plain tab onto its own button: nothing to do
            | LDTab s <- src, s == tstr -> activateSrc src
            | otherwise -> do
                lws0 <- (`reflectIDE` ideR) (readIDE leksahWindows)
                case [ n | n <- M.keys lws0
                         , T.pack (show (LeksahWinKey n)) == tstr ] of
                  -- An LW tab button: land at the right edge of its tree.
                  (n:_) -> dropInto src n (DropSpec [] SplitH True)
                  []    -> do
                    -- A draggable plain tab's button: MATERIALIZE it, then
                    -- land beside the fresh view leaf.
                    wws <- (`reflectIDE` ideR) (readIDE webWindows)
                    case [ k | ww <- M.elems wws, k <- _wwWide0 ww
                             , T.pack (show k) == tstr ] of
                      (k':_) | viewLeafAllowed k' ->
                        mintLeksahWindow Nothing
                            (PaneContent (PaneView k') Nothing) >>= \case
                          Nothing  -> activateSrc src
                          Just lwi -> do
                            fireLeafConvertDone [k']
                            dropInto src lwi (DropSpec [] SplitH True)
                      _ -> activateSrc src
        -- A drop can create same-font adjacency anywhere (the destination,
        -- the source's survivors, a freshly minted window): normalize —
        -- adjacent same-font tmux windows merge into one ('consolidateLw';
        -- idempotent and free where nothing matches).
        forM_ parsed $ \_ -> consolidateAll)
    -- Prompt to save a dirty editor before closing it (⌘W / File ▸ Close).  Same
    -- look and interaction as the terminal pane close menu (renderCloseMenu): a
    -- centred keyboard-navigable menu.  Save (default) writes then closes one
    -- frame later, Discard closes, Cancel dismisses; ↑/↓ move, Enter/Space commit,
    -- Esc cancels.  Rendered in a fixed full-screen overlay (an editor tab is not
    -- a tmux pane, so it can't nest inside one like the terminal menu does).
    promptTargetD <- holdDyn Nothing $ leftmost [ Just <$> promptCloseE, Nothing <$ promptDoneE ]
    promptDoneE <- switchHold never =<< dyn (ffor promptTargetD $ \case
        Nothing -> return never
        Just k  -> do
          let lbl  = case k of EditorKey f -> T.pack (takeFileName f); _ -> ""
              opts = [ ("Save",    Just (k, True))
                     , ("Discard", Just (k, False))
                     , ("Cancel",  Nothing) ]
              nOpt = length opts
          pb <- getPostBuild
          rec
            let keyE   = domEvent Keydown menuEl
                upE    = ffilter (`elem` [38, 37]) keyE
                downE  = ffilter (`elem` [40, 39]) keyE
                enterE = ffilter (`elem` [13, 32]) keyE
                escE   = ffilter (== 27) keyE
            selD <- foldDyn ($) 0 $ leftmost
                      [ (\s -> (s - 1) `mod` nOpt) <$  upE
                      , (\s -> (s + 1) `mod` nOpt) <$  downE
                      , const                      <$> hoverE ]
            (menuEl, (clickE, hoverE)) <-
              elAttr "div" ("class" =: "save-close-overlay") $
                elAttr' "div" ("class" =: "save-close-menu" <> "tabindex" =: "-1") $ do
                  el "p" $ text ("Save changes to " <> lbl <> " before closing?")
                  rows <- forM (zip [0 :: Int ..] opts) $ \(i, (lbl', res)) -> do
                    let attrD = ffor selD $ \s ->
                          "class" =: ("pane-close-opt" <> if s == i then " selected" else "")
                    (b, _) <- elDynAttr' "button" attrD (text lbl')
                    return (res <$ domEvent Click b, i <$ domEvent Mouseenter b)
                  return (leftmost (map fst rows), leftmost (map snd rows))
          focusE <- delay 0.03 pb
          performEvent_ $ ffor focusE $ \_ -> liftJSM . void $
              toJSVal (_element_raw menuEl) >>= \o -> o ^. js0 ("focus" :: Text)
          let keyChoiceE = ffor (tag (current selD) enterE) $ \s -> snd (opts !! s)
          return $ leftmost [ clickE, keyChoiceE, Nothing <$ escE ])
    let answeredE     = fmapMaybe id promptDoneE
        promptSaveE   = fmapMaybe (\(k, s) -> case k of EditorKey f | s -> Just f; _ -> Nothing) answeredE
        discardCloseE = fmapMaybe (\(k, s) -> if s then Nothing else Just [k]) answeredE
    savedCloseE <- delay 0 (fmapMaybe (\(k, s) -> if s then Just [k] else Nothing) answeredE)

    -- ══ ⌘W on a terminal: the pane close menu ══════════════════════════════
    -- Gather the focused tmux pane %id + the pane count of its current window
    -- (to choose the 4-option vs 2-option form).  No tmux / no pane → fall back
    -- to the old whole-terminal detach.
    termGatheredE <- performEvent $ ffor termCloseReqE $ \sess -> liftIO $
        (\mp c -> (sess, mp, c)) <$> activePaneId sess <*> paneCountOfSession sess
    let termMenuOpenE       = fmapMaybe (\(s,mp,c) -> (\p -> (s,p,c)) <$> mp) termGatheredE
        termFallbackDetachE = fmapMaybe (\(s,mp,_) -> if isNothing mp then Just [TerminalKey s] else Nothing) termGatheredE
    -- The menu renders INSIDE its target pane (paneWidget's slot), so CSS centres
    -- it — no JS geometry, no top-level modal.  'closeMenuD' tells every pane
    -- whether it is the target (and multi-pane?); 'renderCloseMenu' is the reflex
    -- menu, which reports its pick through 'fireCloseChoice'.  All interaction is
    -- reflex: a Dynamic selection driven by ↑/↓ keydown + mouse hover, Enter/Space
    -- commit, Esc / "Cancel Close" cancel.
    (closeChoiceE, fireCloseChoice) <- newTriggerEvent
    closeMenuTargetD <- holdDyn Nothing $ leftmost [ Just <$> termMenuOpenE, Nothing <$ closeChoiceE ]
    let closeMenuD  = ffor closeMenuTargetD (fmap (\(_, p, c) -> (p, c > 1)))
        menuChoiceE = attachWithMaybe (\mt ch -> (\(s, p, _) -> (ch, s, p)) <$> mt)
                          (current closeMenuTargetD) closeChoiceE
        renderCloseMenu paneId multi = do
          let opts | multi     = [ (TCHide,   "Hide Window")
                                 , (TCKill,   "Kill Pane")
                                 , (TCMove,   "Move Pane to Hidden Window")
                                 , (TCCancel, "Cancel Close") ]
                   | otherwise = [ (TCHide, "Hide"), (TCKill, "Kill"), (TCCancel, "Cancel Close") ]
              nOpt = length opts
          pb <- getPostBuild
          rec
            -- ↑/↓ (37/38 & 39/40) move the selection; Enter/Space (13/32) commit
            -- it; Esc (27) cancels.  Keydown fires on the menu div once focused.
            let keyE   = domEvent Keydown menuEl
                upE    = ffilter (`elem` [38, 37]) keyE
                downE  = ffilter (`elem` [40, 39]) keyE
                enterE = ffilter (`elem` [13, 32]) keyE
                escE   = ffilter (== 27) keyE
            selD <- foldDyn ($) 0 $ leftmost
                      [ (\s -> (s - 1) `mod` nOpt) <$  upE
                      , (\s -> (s + 1) `mod` nOpt) <$  downE
                      , const                      <$> hoverE ]
            -- The overlay covers the pane and CSS-centres the menu box (grid).
            (menuEl, (clickE, hoverE)) <-
              elAttr "div" ("class" =: "pane-close-overlay") $
                elAttr' "div" ("class" =: "pane-close-menu" <> "tabindex" =: "-1") $ do
                  rows <- forM (zip [0 :: Int ..] opts) $ \(i, (ch, lbl)) -> do
                    let attrD = ffor selD $ \s ->
                          "class" =: ("pane-close-opt" <> if s == i then " selected" else "")
                    (b, _) <- elDynAttr' "button" attrD (text lbl)
                    return (ch <$ domEvent Click b, i <$ domEvent Mouseenter b)
                  return (leftmost (map fst rows), leftmost (map snd rows))
          -- Focus the menu one tick after build (attached + laid out) so its
          -- keydown reaches reflex — Haskell-side, via jsaddle, no JS helper.
          focusE <- delay 0.03 pb
          performEvent_ $ ffor focusE $ \_ -> liftJSM . void $
              toJSVal (_element_raw menuEl) >>= \o -> o ^. js0 ("focus" :: Text)
          -- Shadow: whole terminal while "Hide Window" is selected (multi), else
          -- the focused pane — drives the existing shadow overlay (paneHlJs).
          performEvent_ $ ffor (leftmost [tag (current selD) pb, updated selD]) $ \s ->
              liftJSM . void $ jsg ("window" :: Text) ^. js2 ("leksahMenuShadow" :: Text)
                  paneId (multi && fst (opts !! s) == TCHide)
          let keyChoiceE = ffor (tag (current selD) enterE) $ \s -> fst (opts !! s)
              chE = leftmost [ clickE, keyChoiceE, TCCancel <$ escE ]
          performEvent_ $ ffor chE $ \ch -> do
              liftJSM . void $ jsg ("window" :: Text) ^. js0 ("leksahMenuShadowClear" :: Text)
              liftIO (fireCloseChoice ch)
    let termKillE = fmapMaybe (\(ch,_,p) -> if ch == TCKill then Just p       else Nothing) menuChoiceE
        termHideE = fmapMaybe (\(ch,s,p) -> if ch == TCHide then Just (s, p)  else Nothing) menuChoiceE
        termMoveE = fmapMaybe (\(ch,s,p) -> if ch == TCMove then Just (s, p)  else Nothing) menuChoiceE
    -- Kill: just kill the pane (it vanishes from the live tree → flipper/tabs).
    performEvent_ $ ffor termKillE $ liftIO . void . forkIO . killTmuxPaneId
    -- Hide: hide the focused pane's window.  Move: break the pane into a new
    -- window and hide THAT.  Both resolve to (session, windowIndex) → 'hiddenWindows'.
    hideWinE <- performEvent $ ffor termHideE $ \(s, p) -> liftIO $ fmap (\w -> (s, w)) <$> windowIndexOfPane p
    moveWinE <- performEvent $ ffor termMoveE $ \(s, p) -> liftIO $ fmap (\w -> (s, w)) <$> breakTmuxPaneId p
    let hideOrMoveWinE = fmapMaybe id (leftmost [hideWinE, moveWinE])
    -- Un-hide: whenever a pane is focused (Terminals tree select, a Claude session
    -- reopened, an editor pane reopened — all focus the pane), drop its window from
    -- 'hiddenWindows'.  Suppress the just-hidden window for ~1s so the focus that
    -- settles right after "Hide" doesn't immediately un-hide it.
    unsuppressE  <- delay 1.0 hideOrMoveWinE
    suppressHideD <- foldDyn ($) S.empty $ leftmost
                       [ S.insert <$> hideOrMoveWinE, S.delete <$> unsuppressE ]
    -- Un-hide: a hidden window comes back the moment leksah shows it again — i.e.
    -- it's the current tmux window (twActive) of the session that is the active
    -- wide0 tab.  Every "show" path — Terminals-tree select, a Claude session
    -- reopened, an editor pane reopened — select-window's to it, so this single
    -- poll-driven rule covers them all (no per-path hooks, and no reliance on DOM
    -- focus which a programmatic reopen doesn't fire).  The ~1s suppress stops the
    -- window just hidden (still tmux-current for a beat) from instantly returning.
    let unhideWinE = fmapMaybe id $ attachWith
          (\(hidden, sup, mact, lws) tree -> case mact >>= tabSessionOf lws of
             Just s -> listToMaybe
               [ sw | (s', (_, wins)) <- M.toList tree, s' == s
                    , w <- wins, twActive w
                    , let sw = (s, twIndex w)
                    , sw `S.member` hidden, not (sw `S.member` sup) ]
             _ -> Nothing)
          ((,,,) <$> current hiddenWinsD <*> current suppressHideD
                 <*> current wide0ActiveD <*> current leksahWindowsD')
          (updated allTreeD)
    -- "Hide Window" hid the currently-shown window, so wide0 would keep rendering
    -- it (the active tab still points there).  Activate the MRU-next flip item
    -- (⌘`-style, via the same synthetic-flip path) so it's replaced — excluding
    -- the just-hidden window; a no-op if the flipper is otherwise empty.  (Move is
    -- multi-pane only, so its source window keeps its remaining panes — no flip.)
    let hiddenNowE = fmapMaybe id hideWinE
        -- Next WIDE0 item only (skip side/bottom panes like Workspace — that's why
        -- this isn't plain ⌘`, which would include them), excluding the just-hidden
        -- window.  A no-op if nothing else is in wide0.
        nextFlipE  = attachWithMaybe
          (\flips (s, w) -> listToMaybe
              [ fi | (area, fi) <- flips, area == "wide0"
                   , case fi of FlipPane s' w' _ -> not (s' == s && w' == w); _ -> True ])
          (current flipLiveD) hiddenNowE
    performEvent_ $ ffor nextFlipE $ liftIO . fireNumFlip

    -- File ▸ Add Remote Project…: the native menu drops a token on the
    -- AddRemoteRequest bridge (drained here); the web menubar fires the command
    -- directly.  Either opens the modal (dyn/switchHold, like the save prompt);
    -- the dialog owns its own validate+add and fires when it should close.
    (addRemoteReqE, fireAddRemoteReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextAddRemoteRequest >>= fireAddRemoteReq
    let menuAddRemoteE = fmapMaybe (\case CommandProjectAddRemote -> Just (); _ -> Nothing)
                           (fmapMaybe (^? _MenubarCommand) menubarE)
        openAddRemoteE = leftmost [addRemoteReqE, menuAddRemoteE]
    addRemoteOpenD <- holdDyn False $ leftmost [ True <$ openAddRemoteE, False <$ addRemoteCloseE ]
    addRemoteCloseE <- switchHold never =<< dyn (ffor addRemoteOpenD $ \case
        False -> return never
        True  -> addRemoteDialog remoteHostsD)
    -- "New Claude Session in Worktree…" (tree context menus drop the clicked
    -- dir on the bridge): same modal pattern; the dialog owns the whole flow
    -- (create worktree → add project → start claude).
    (newWtReqE, fireNewWtReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextNewWorktreeRequest >>= fireNewWtReq
    newWtOpenD <- holdDyn Nothing $ leftmost
        [ Just <$> newWtReqE, Nothing <$ newWtCloseE ]
    newWtCloseE <- switchHold never =<< dyn (ffor newWtOpenD $ \case
        Nothing  -> return never
        Just dir -> newWorktreeDialog dir)
    -- File ▸ Add Server… / the Terminals-tree "Add Server…" row: same bridge
    -- pattern as Add Remote Project above ('commandAddServer' drops a token on
    -- the AddServerRequest bridge from any dispatch route).  The dialog owns
    -- its validate+add (appends to the remoteHosts pref); the debounced prefs
    -- writer below persists the change and remoteHostsD picks it up.
    (addServerReqE, fireAddServerReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextAddServerRequest >>= fireAddServerReq
    addServerOpenD <- holdDyn False $ leftmost
        [ True <$ addServerReqE, False <$ addServerCloseE ]
    addServerCloseE <- switchHold never =<< dyn (ffor addServerOpenD $ \case
        False -> return never
        True  -> addServerDialog)
    -- Project ▸ Remote Settings…: the context-menu item drops the project's
    -- 'ProjectKey' on the RemoteSettingsRequest bridge (drained here); a token
    -- opens the per-project prefix editor for that project, and the dialog
    -- fires when it should close (Save or Cancel).
    (remoteSettingsReqE, fireRemoteSettingsReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextRemoteSettings >>= fireRemoteSettingsReq
    remoteSettingsPkD <- holdDyn Nothing $ leftmost
        [ Just <$> remoteSettingsReqE, Nothing <$ remoteSettingsCloseE ]
    remoteSettingsCloseE <- switchHold never =<< dyn (ffor remoteSettingsPkD $ \case
        Nothing -> return never
        Just pk -> remoteSettingsDialog pk)
    let openInWide0 lws n =
          maybe M.empty (=: ("wide0", Just ())) (termTabKey lws n)
        lwsB = current leksahWindowsD'
        openTabsE = leftmost
          [ openFileE'   -- carries native opens too (see openFileSplitE)
          -- A new terminal opens by the key of the leksah window just minted
          -- for it, falling back to resolving the session id.  It must be ONE
          -- entry, not two: both fire in the same frame, and 'leftmost' would
          -- take whichever came first — the session-id one, which answers an
          -- empty map here (its snapshot predates the mint) and so silently
          -- cancelled the activation.
          , attachWith (\lws (sid, mlw) ->
                maybe (openInWide0 lws sid)
                      (\lw -> LeksahWinKey lw =: ("wide0", Just ()))
                      mlw)
              lwsB newTermE
          , attachWith openInWide0 lwsB editTermSidE
          , attachWith openInWide0 lwsB termRequestE
          , attachWith openInWide0 lwsB remoteOpenKeyE
          , attachWith openInWide0 lwsB selectAnyTermE
          , attachWith (\lws (s, _, _) -> openInWide0 lws s) lwsB flipPaneE
          , attachWith (\lws (s, _)    -> openInWide0 lws s) lwsB alertTargetE
          , ((\(d, b) -> GitLogKey d b =: ("wide0", Just ())) <$> gitLogReqE)
          , ((\d -> ReviewKey d =: ("wide0", Just ())) <$> reviewReqE)
          , (TasksKey =: ("wide0", Just ())) <$ tasksReqE
          , ((\(d, p) -> PlanKey d p =: ("wide0", Just ())) <$> planReqE)
          , ((\(d, p) -> CompareKey d p =: ("wide0", Just ())) <$> compareReqE)
          , (PreferencesKey =: ("wide0", Just ())) <$ showPrefsE
          , (ShortcutsKey =: ("wide0", Just ())) <$ showShortcutsE
          , (\k -> k =: ("wide0", Just ())) <$> newBrowserKeyE ]
        -- Killing a remote session server-side also drops its tab if open — under
        -- either identity it may be keyed by (its id, or its name from a
        -- cc-connect HOST#NAME tab); closeWide0 ignores whichever isn't present.
        closeRemoteSessTabE = ffor killRemoteSessE $ \(h, s, nm) ->
            [ TerminalKey (remoteKey h s), TerminalKey (remoteKey h nm) ]
        closeTabsE = leftmost [ attachWith (\lws n -> TerminalKey n
                                  : [ LeksahWinKey i
                                    | (i, lw) <- M.toAscList lws
                                    , lwSession lw == Just n ]) lwsB closeTermE
                              , detachCloseE
                              -- Dirty editor closed via the save prompt: after Save
                              -- (savedCloseE, one frame later) or Don't Save.
                              , savedCloseE, discardCloseE
                              -- ⌘D conversion replaces the tab with its pane.
                              , convertCloseE
                              , exitedTermE, closeRemoteSessTabE ]
        -- Running a grep brings the Grep pane to the front of its area; Preferences…
        -- opens and shows the Preferences pane in the editor area.  The flipper
        -- selects a tab directly, or brings up a pane's terminal in wide0.
        -- ⌥⌘N / ⌃⌘N: the Nth side- / bottom-bar pane (the strips' order —
        -- what the ⌘-held badges show).
        pickNth ks n = if n >= 1 && n <= length ks then Just (ks !! (n - 1)) else Nothing
        numSelTabE = leftmost
          -- ⌥⌘4 must not select a Metadata tab that metadataEnabled hid: its
          -- area would go blank.  (Metadata is LAST in the strip, so no other
          -- pane's number moves when it goes.)
          [ attachWithMaybe
                (\metaOn n -> do
                    k <- pickNth numberedTallTabs n
                    guard (metaOn || k /= MetadataKey)
                    pure ("tall" =: k))
                (current metaOnD) (numKeyE _CommandSelectSidePane)
          , fmapMaybe (fmap ("wide1" =:) . pickNth numberedWide1Tabs)
                      (numKeyE _CommandSelectBottomPane)
          -- C-b w (tmux prefix interceptor): show + focus the Terminals pane,
          -- exactly like the ⌥⌘N side-pane navigation it rides through here.
          , ("tall" =: TerminalsKey) <$ activateTerminalsE ]
        selectTabE = leftmost [flipTabE, restoreVisibleE, ("wide1" =: GrepKey) <$ grepReqE
                              , ("wide1" =: GrepKey) <$ lspRefsE
                              -- ⌘-drag cancel with a plain-tab source: a real
                              -- select of that tab (flips any peek back).
                              , leafSelectTabE
                              -- A flipped/alerted pane opens its OWNING leksah
                              -- window's tab (not the session's first lw).
                              , attachWith (\(lws, tree) (s, w, _) ->
                                  maybe M.empty ("wide0" =:)
                                    (maybe (termTabKey lws s) (Just . LeksahWinKey)
                                           (paneOwnerLw lws tree s w)))
                                  ((,) <$> lwsB <*> current allTreeD) flipPaneE
                              , attachWith (\(lws, tree) (s, w) ->
                                  maybe M.empty ("wide0" =:)
                                    (maybe (termTabKey lws s) (Just . LeksahWinKey)
                                           (paneOwnerLw lws tree s w)))
                                  ((,) <$> lwsB <*> current allTreeD) alertTargetE
                              , ("wide0" =: PreferencesKey) <$ showPrefsE
                              , ("wide0" =: ShortcutsKey) <$ showShortcutsE
                              , ("wide0" =:) <$> newBrowserKeyE
                              , (\(d, b) -> "wide0" =: GitLogKey d b) <$> gitLogReqE
                              , ("wide0" =:) . ReviewKey <$> reviewReqE
                              , ("wide0" =: TasksKey) <$ tasksReqE
                              , (\(d, p) -> "wide0" =: PlanKey d p) <$> planReqE
                              , (\(d, p) -> "wide0" =: CompareKey d p) <$> compareReqE
                              -- Escape closed the find bar: re-select the active
                              -- pane's tab (a re-select of the already-visible
                              -- tab fires each widget's selectedE), which hands
                              -- the keyboard back to it — editors focus their
                              -- view, leksah windows run the forced reconcile.
                              , attachWithMaybe (\(vis, mk) _ -> do
                                    k <- mk
                                    a <- listToMaybe [ a | (a, k') <- M.toList vis
                                                         , k' == k ]
                                    Just (a =: k))
                                  (current ((,) <$> visibleTabsD <*> activePaneD))
                                  findHideE
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
    -- Default the visible editor to the MRU-front of this window's wide0 list when
    -- no active tab is recorded (_wwActive == Nothing while _wwWide0 is non-empty).
    -- That state arises when a window is restored with editors but no saved active
    -- tab (session "wwsActive": null) — e.g. after a ghci :reload window-recreate —
    -- where the restorePb seed below would otherwise select nothing, leaving the
    -- whole editor tab-bar unrendered and every editor body visibility:hidden.
    -- Showing the seeded tab runs activateWide0, so _wwActive self-heals on save.
    wide0ActiveD <- holdUniqDyn
        ((\ww -> maybe (listToMaybe (_wwWide0 ww)) Just (_wwActive ww)) <$> myWinD)
    -- The Metadata tree is soft-deleted (the `metadata` cabal flag): the key
    -- stays parseable in old sessions, but the pane is always off.  The
    -- post-build close still fires once so a stale tab restored from an old
    -- session (or carried by 'initialTabs' historically) is cleaned up.
    let metaOnD = constDyn False :: Dynamic t Bool
    metaPb  <- getPostBuild
    let metaOnE    = leftmost [ updated metaOnD, tag (current metaOnD) metaPb ]
        metaOpenE  = (MetadataKey =: ("tall", Just ())) <$ ffilter id  metaOnE
        metaCloseE = [MetadataKey]                      <$ ffilter not metaOnE
    (recentTabs, tabE, visibleTabsD, activePaneD, tabCloseBtnE) <- tabsWidget
      initialTabs
      initialVisibleTabs
      wide0OrderD
      metaOpenE   -- wide0 opens go through the shared state (moveTabTo), not here
      metaCloseE  -- wide0 closes go through the shared state (closeWide0), not here
      selectTabE
      setRecentE
      focusTabE
      leafPeekE   -- ⌘-drag pane move: peek a hovered tab (visible, not active)
      mkTabButtons
      (\k selectedE _v -> do
        let toDM x = fmap (DM.singleton x . Identity)
        case k of
          WorkspaceKey   -> toDM WorkspaceTab <$> workspaceWidget ide treeHighlightD treeRevealD
          ErrorsKey      -> toDM ErrorsTab <$> errorsWidget ide allE (paneFind ErrorsKey) (paneMoveE "errors") (paneActivateE "errors")
          LogKey         -> toDM LogTab <$> logWidget ide (paneFind LogKey) (paneMoveE "log") (paneActivateE "log")
          GrepKey        -> toDM GrepTab <$> grepWidget grepResultsD (paneFind GrepKey)
          TerminalsKey   -> toDM TerminalsTab <$> terminalsWidget activeTermD attentionD remoteHostsD hostTreesD
          LeksahWinKey n -> toDM TerminalTab <$> do
              -- A leksah window: its native split tree of panes (whole tmux
              -- windows and/or native views).  Session-backed windows render
              -- through the CC widget (a control client on the session);
              -- SESSIONLESS ones (pure views, from tab materialization) get
              -- the lightweight renderer.  A window binds a session at most
              -- once (⌘D creating its first terminal), rebuilding the widget
              -- then — the ⌘D pipeline saves a dirty editor view first.
              lwSessD <- holdUniqDyn
                  ((\i -> M.lookup n (i ^. leksahWindows) >>= lwSession) <$> ide)
              evE <- dyn $ ffor lwSessD $ \case
                  Nothing  -> never <$ sessionlessLwWidget ide n selectedE leafViewW
                  Just sid -> terminalCCWidget ide n sid selectedE leafViewW
                                  closeMenuD renderCloseMenu
              switchHold never evE
          TerminalKey n  -> toDM TerminalTab <$> do
              -- RETAINED for remote (ssh://) sessions and the classic PTY /
              -- browser-demo paths: local control-mode terminals are
              -- 'LeksahWinKey' tabs now.  Control mode (-CC) vs classic PTY
              -- attach is decided when the tab is created.
              cm <- terminalControlMode . view prefs <$> sample (current ide)
              -- Control mode needs tmux, which has no Windows build; there the
              -- classic ConPTY-backed widget is the only option.
              let useCC = tmuxSupported && (cm || "ssh://" `T.isPrefixOf` n)
              if useCC
                then terminalCCWidget ide n n selectedE leafViewW
                         closeMenuD renderCloseMenu
                else terminalWidget ide n selectedE
          MetadataKey    ->
#ifdef LEKSAH_METADATA
            toDM MetadataTab <$> metadataWidget ide activeFileD revealMetaD (paneFind MetadataKey)
#else
            -- Tombstone: MetadataKey stays parseable in old sessions but the
            -- pane is soft-deleted (metadata cabal flag); renders nothing.
            toDM MetadataTab <$> (never <$ blank)
#endif
          AgentsKey      -> toDM AgentsTab <$> agentsWidget
          ChangesKey     -> toDM ChangesTab <$> changesWidget ide (paneFind ChangesKey)
          PreferencesKey -> toDM PreferencesTab <$> preferencesWidget ide
          ShortcutsKey   -> toDM ShortcutsTab <$> withConvertHint (shortcutsWidget ide)
          BrowserKey n   -> toDM BrowserTab <$>
              withConvertHint (browserWidget n selectedE (constDyn True))
          GitLogKey d b  -> toDM GitLogTab <$> do
              -- Same editor-backend pref as file tabs (decided at creation).
              mon <- monacoEditor . view prefs <$> sample (current ide)
              withConvertHint $ gitLogWidget mon d b
          ReviewKey d    -> toDM ReviewTab <$> do
              mon <- monacoEditor . view prefs <$> sample (current ide)
              withConvertHint $ reviewWidget mon d
          TasksKey       -> toDM TasksTab <$> do
              seed <- liftIO (readIORef tasksSeedRef)
              tasksWidget seed
          PlanKey d p    -> toDM PlanTab <$> planWidget d (T.unpack p) selectedE
          CompareKey d p -> toDM CompareTab <$> do
              mon <- monacoEditor . view prefs <$> sample (current ide)
              compareWidget mon d p selectedE
          EditorKey file -> toDM EditorTab <$>
              withConvertHint (makeEditor file selectedE (constDyn True)))
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
      , wbSave  = fireSaveReq
      , wbFind  = fireFindReq ()
      , wbPrefs = firePrefsReq ()
      , wbShortcuts = fireShortcutsReq ()
      , wbBrowser = fireBrowserReq ()
      , wbKeymap = fireKeymapCmd
      , wbOpenedFile = fireOpenedFile
      }
    -- Toolbar/menu Find toggles the bar; Cmd+F (keymap) always shows + focuses it.
    let findToggleE = leftmost
          [ fmapMaybe (\case CommandFind -> Just (); _ -> Nothing) panelCmdE
          , findBridgeE ]
        findShowE = fmapMaybe (\e -> case e ^? _KeymapCommand of
                                       Just CommandFind -> Just (); _ -> Nothing) keymapE
    findbarVisibleD <- foldDyn ($) False $ leftmost
        [ not <$ findToggleE, const True <$ findShowE
        -- Escape in the bar hides it (the keyboard handback is the re-select
        -- arm in selectTabE above).
        , const False <$ findHideE ]
    -- Focus the find input whenever the bar newly shows, and on every Cmd+F.
    performEvent_ $ ffor (leftmost
        [ () <$ findShowE
        , fmapMaybe (\v -> if v then Just () else Nothing) (updated findbarVisibleD) ]) $ \_ ->
      liftJSM . void $ jsg ("window" :: Text) ^. js0 ("leksahFocusFind" :: Text)
    findbarE   <- findbarWidget activePaneD findbarVisibleD
    let findHideE = fmapMaybe (\case FindHide -> Just (); _ -> Nothing) findbarE
    -- Remote-activity feed for the statusbar: every time a remote (ssh) op
    -- starts/finishes, RemoteExec signals on 'remoteInFlightChanged'; re-read
    -- the per-host in-flight counts and push them in.  Off the frame thread.
    (remoteActE, fireRemoteAct) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $
            readChan remoteInFlightChanged >> readTVarIO remoteInFlight >>= fireRemoteAct
    remoteActD <- holdDyn M.empty remoteActE
    statusbarE <- statusbarWidget ide remoteActD

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
        -- ⌘-drag pane move (leafDragJs): peek a hovered wide0 tab / the one
        -- drop-or-cancel payload.  Per-window globals — the gesture happens
        -- entirely inside one OS window.
        _ <- w ^. jss ("leksahLeafPeek" :: Text) (fun $ \_ _ args -> case args of
                (kV:_) -> valToText kV >>= liftIO . fireLeafPeekReq
                _ -> return ())
        _ <- w ^. jss ("leksahLeafDrop" :: Text) (fun $ \_ _ args -> case args of
                (jV:_) -> valToText jV >>= liftIO . fireLeafDrop
                _ -> return ())
        _ <- w ^. jss ("leksahPaneFocus" :: Text) (fun $ \_ _ args -> case args of
                (pV:_) -> do
                    pid <- valToText pV
                    focusLog ("[" <> show wid <> "] JS leksahPaneFocus " <> T.unpack pid)
                    liftIO (fireJsPaneFocus pid)
                _ -> return ())
        -- Open a file in an editor tab from page JS, via the same bridge the
        -- native Open dialogs use (routes to the frontmost window).  The
        -- browser demo's hosting page uses this (e.g. to open the sample
        -- project's Main.hs), and it works on every front end.
        _ <- w ^. jss ("leksahOpenFile" :: Text) (fun $ \_ _ args -> case args of
                (fpV:_) -> do
                    fp <- valToText fpV
                    liftIO $ deliverOpenedFile (T.unpack fp)
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
        -- Bridge proof hooks (UI-split stage 1, see IDE.Web.Bridge).
        -- leksahBridgeTest(name, argsJson) drives the FRONTEND end from page
        -- JS (Promise + window.leksahBridgeLast for `leksah-cmd js eval`);
        -- __leksahBridgeNotify(msg) pokes the BACKEND end into calling the
        -- frontend's showNotification — both directions testable from a shell.
        bridgeCtx <- askJSM
        let bridgeTestJs = T.unlines
              [ "window.__leksahBridgeToast = function(msg){"
              , "  var d = document.createElement('div');"
              , "  d.className = 'leksah-bridge-toast'; d.textContent = msg;"
              , "  d.style.cssText = 'position:fixed;bottom:20px;right:20px;background:var(--leksah-surface);color:var(--leksah-fg);padding:10px 16px;border-radius:6px;z-index:99999;font-family:sans-serif;box-shadow:0 2px 8px var(--leksah-shadow-drop)';"
              , "  document.body.appendChild(d); setTimeout(function(){ d.remove(); }, 4000);"
              , "};"
              , "window.__lbtPend = {}; window.__lbtNext = 1; window.leksahBridgeLast = null;"
              , "window.leksahBridgeTest = function(name, argsJson){"
              , "  return new Promise(function(res, rej){"
              , "    var id = window.__lbtNext++;"
              , "    window.__lbtPend[id] = { res: res, rej: rej };"
              , "    window.__leksahBridgeTestGo(id, name, argsJson === undefined ? null : argsJson);"
              , "  });"
              , "};"
              , "window.__leksahBridgeTestDone = function(id, ok, json){"
              , "  window.leksahBridgeLast = { id: id, ok: ok, result: json };"
              , "  var p = window.__lbtPend[id]; delete window.__lbtPend[id];"
              , "  if (p) { (ok ? p.res : p.rej)(json); }"
              , "};"
              ]
        _ <- eval bridgeTestJs
        liftIO $ getFrontendBridge >>= mapM_ (\feBr ->
            expose feBr "showNotification" $ \bargs -> do
                let msg = case bargs of
                        (BText t : _)          -> t
                        (BJson (A.String t):_) -> t
                        _                      -> "leksah bridge notification"
                (`runJSM` bridgeCtx) . void $
                    jsg ("window" :: Text) ^. js1 ("__leksahBridgeToast" :: Text) msg
                return BNull)
        _ <- w ^. jss ("__leksahBridgeTestGo" :: Text) (fun $ \_ _ args -> case args of
                (idV:nameV:argsJsonV:_) -> do
                    tid <- valToNumber idV
                    name <- valToText nameV
                    argsJson <- valToText argsJsonV
                    liftIO . void . forkIO $ do
                        mfe <- getFrontendBridge
                        (ok, out) <- case mfe of
                            Nothing -> return (False, "no frontend bridge end")
                            Just feBr -> do
                                -- one JSON value → the matching scalar
                                -- BridgeValue (so both exposeJSON and
                                -- BText-shaped endpoints accept it)
                                let vargs = case decodeStrict' (encodeUtf8 argsJson) of
                                        Nothing              -> []
                                        Just A.Null          -> []
                                        Just (A.String t)    -> [BText t]
                                        Just (A.Bool b)      -> [BBool b]
                                        Just v               -> [BJson v]
                                r <- call feBr name vargs
                                return $ case r of
                                    Right v -> (True, encodeValue1 v)
                                    Left e  -> (False, T.pack (show e))
                        (`runJSM` bridgeCtx) . void $
                            jsg ("window" :: Text) ^. js3 ("__leksahBridgeTestDone" :: Text)
                                tid ok out
                _ -> return ())
        _ <- w ^. jss ("__leksahBridgeNotify" :: Text) (fun $ \_ _ args -> case args of
                (msgV:_) -> do
                    msg <- valToText msgV
                    liftIO . void . forkIO $ getBackendBridge >>= mapM_ (\beBr ->
                        void $ call beBr "showNotification" [BText msg])
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
            AgentsKey    -> Just ("tall",  ".agents")
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
    -- The Preferences and Shortcuts panes are transient — never save/restore
    -- them as open tabs.
    let notPrefs k = k /= PreferencesKey && k /= ShortcutsKey
    leksahWindowsD <- holdUniqDyn ((^. leksahWindows) <$> ide)
    paneAID <- holdUniqDyn ((^. paneAISession) <$> ide)
    sessionD <- holdUniqDyn $
      (\wins vis recF lws mru paneAI ->
          WebSession 6
            [ WebWindowSession (filter notPrefs (_wwWide0 ww)) (_wwActive ww)
                               (_wwTall ww) (_wwWide1 ww)
            | (_, ww) <- M.toList wins ]
            (M.toList (M.filterWithKey (\a k -> a /= "wide0" && notPrefs k) vis))
            (Just recF)
            -- v6: SESSIONLESS leksah windows (pure native views) persist
            -- here; session-backed ones live on their tmux session as
            -- @leksah_layout v2.
            (Just [ (i, lw) | (i, lw) <- M.toAscList lws
                  , isNothing (lwSession lw) ])
            (Just mru)
            -- Explicit per-pane AI-session bindings; the derived ones are
            -- recomputed from the live sessions on demand, so only these need
            -- saving.
            (Just (M.toAscList paneAI)))
        <$> webWindowsD <*> visibleTabsD <*> recentFilesD <*> leksahWindowsD
        <*> flipMruD <*> paneAID
    let writeGateD = (&&) <$> restoredFlagD <*> isActiveD
    saveSessE <- debounce (1 :: NominalDiffTime) (gate (current writeGateD) (updated sessionD))
    -- Once this instance has initiated a handoff, stop writing the session — the
    -- successor has restored it and is now the authoritative writer (otherwise
    -- the retiring old instance races the successor on the same file).
    performEvent_ $ ffor saveSessE $ \s -> liftIO $
        handingOff >>= \ho -> when (not ho) (writeWebSession s)
    -- Handoff flush: 'requestHandoff' fires these (via registerSessionFlush) to
    -- force the CURRENT session to disk before the successor launches, so it
    -- restores the up-to-date flipper/tab/layout state.  Only the active window
    -- actually writes (the same single-writer rule as the debounced saver) and
    -- signals completion back to the waiting 'flushSessionAndWait'.
    (flushE, fireFlush) <- newTriggerEvent
    liftIO $ registerSessionFlush (fireFlush ())
    performEvent_ $ ffor (gate (current isActiveD) flushE) $ \_ -> do
        s   <- sample (current sessionD)
        lws <- sample (current leksahWindowsD)
        liftIO $ do
            writeWebSession s
            -- Zero-downtime handoff: the successor restores the leksah
            -- windows from the tmux sessions' @leksah_layout options, so any
            -- edit still sitting in the 1s write-back debounce must land NOW
            -- (saveSessionLayouts is idempotent; unchanged sessions just get
            -- the same option value again).
            sequence_ [ saveSessionLayouts sid grp
                      | (sid, grp) <- M.toList (lwsBySession lws) ]
            signalSessionFlushDone

    -- Leksah windows ('_leksahWindows'): reconcile against the live LOCAL
    -- sessions on every pane-tree poll — dead tmux windows drop out
    -- (collapsing their splits), emptied leksah windows disappear, stray
    -- tmux windows are wrapped in new leksah windows — and keep the wide0
    -- tab lists in step (a removed window's tab goes; a new window's tab is
    -- appended to the OS window already showing that session, else the
    -- active one).  Gated to the active window (the reconcile is idempotent,
    -- but N windows would race N identical 'modifyIDE_'s) and to a NON-EMPTY
    -- tree: an empty map means tmux didn't answer, never that every session
    -- died.  The changed check runs against a snapshot, but the mutation
    -- recomputes from the live IDE inside 'modifyIDE_', so a racing layout
    -- edit can't be clobbered.
    let applyReconcile tree i =
          let liveBySession = M.fromList
                [ (sid, map twId wins)
                | (sid, (_, wins)) <- M.toList tree
                , not ("ssh://" `T.isPrefixOf` sid) ]
              hiddenIds = S.fromList
                [ twId w
                | (sid, (_, wins)) <- M.toList tree
                , w <- wins
                , (sid, twIndex w) `S.member` (i ^. hiddenWindows) ]
              (lws', next') = reconcileWindows liveBySession hiddenIds mempty
                                (i ^. nextLeksahWin) (i ^. leksahWindows)
          in syncLwTabs (i & leksahWindows .~ lws' & nextLeksahWin .~ next')
        -- (syncLwTabs — the tab-list/window-map consistency pass — is a
        -- top-level function now: the ⌘-drag cross-window move composes it
        -- after 'moveLeafAcross' too.)
        needsReconcileE = attachWithMaybe
          (\i tree ->
              let i' = applyReconcile tree i
              in if i' ^. leksahWindows == i ^. leksahWindows
                    && i' ^. webWindows == i ^. webWindows
                   then Nothing else Just tree)
          (current ide)
          (gate (current isActiveD) (ffilter (not . M.null) (updated paneTreeD)))
    -- Skip the pass while a minimal-path conversion has tmux surgery in
    -- flight: its break-pane children must not be adopted as strays before
    -- the converting leksah window claims them.
    needsReconcileGatedE <- fmap (fmapMaybe id) . performEvent $
        ffor needsReconcileE $ \tree -> liftIO $ do
            busy <- conversionActive
            return (if busy then Nothing else Just tree)
    -- (The modifyIDE_ itself rides the IDEAction event stream with the other
    -- shared-state mutations — see the mconcat block below.)
    -- …and mirror changed session-backed windows back onto their tmux
    -- sessions (@leksah_layout v2), debounced like the web-session saver,
    -- with an encoded-value cache so only sessions whose windows actually
    -- changed get a set-option.  (Sessionless windows ride 'sessionD'.)
    layoutSaveE <- debounce (1 :: NominalDiffTime)
        (gate (current writeGateD) (updated leksahWindowsD))
    lastLayoutWrites <- liftIO $ newIORef M.empty
    performEvent_ $ ffor layoutSaveE $ \lws -> liftIO . void . forkIO $ do
        let bySess = lwsBySession lws
            enc = fmap encodeLayoutOption bySess
        prev <- atomicModifyIORef' lastLayoutWrites (\p -> (enc, p))
        sequence_ [ saveSessionLayouts sid grp
                  | (sid, e) <- M.toList enc
                  , M.lookup sid prev /= Just e
                  , Just grp <- [M.lookup sid bySess] ]

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
    -- The monospace-font prefs drive the --leksah-mono / --leksah-mono-size CSS
    -- variables the editor (CodeMirror) and monospace panes (git log, log)
    -- reference, so a change in Preferences reflows them live.
    fontCssD <- holdUniqDyn $
        (\p -> ":root{--leksah-mono:" <> monospaceFont p
            <> ";--leksah-mono-size:" <> T.pack (show (monospaceFontSize p)) <> "px}") <$> prefsD
    el "style" $ dynText fontCssD
    -- …and are published to window globals the terminals (xterm, fixed cell grid)
    -- read when they are created.  A terminal-font change takes effect for new
    -- terminals / on restart (open terminals keep their measured grid).
    monoPrefD <- holdUniqDyn ((\p -> (monospaceFont p, monospaceFontSize p)) <$> prefsD)
    monoPb <- getPostBuild
    performEvent_ $ ffor (leftmost [updated monoPrefD, tag (current monoPrefD) monoPb]) $ \(fam, sz) ->
        liftJSM $ do
            w <- jsg ("window" :: Text)
            _ <- w ^. jss ("__leksahMonoFamily" :: Text) fam
            void $ w ^. jss ("__leksahMonoSize" :: Text) sz
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
    -- The six editor/terminal theme names → window globals the re-theme engine
    -- (themeSwitchJs) reads, then re-theme now.  Fires on any theme-pref change
    -- and once at post-build; the matchMedia listener covers OS appearance flips.
    themePrefD <- holdUniqDyn ((\p -> ( monacoThemeDark p, monacoThemeLight p
                                      , codeMirrorThemeDark p, codeMirrorThemeLight p
                                      , xtermThemeDark p, xtermThemeLight p )) <$> prefsD)
    themePb <- getPostBuild
    performEvent_ $ ffor (leftmost [updated themePrefD, tag (current themePrefD) themePb]) $
        \(md, ml, cd, cl, xd, xl) -> liftJSM $ do
            w <- jsg ("window" :: Text)
            _ <- w ^. jss ("__leksahMonacoDark"  :: Text) md
            _ <- w ^. jss ("__leksahMonacoLight" :: Text) ml
            _ <- w ^. jss ("__leksahCmDark"      :: Text) cd
            _ <- w ^. jss ("__leksahCmLight"     :: Text) cl
            _ <- w ^. jss ("__leksahXtermDark"   :: Text) xd
            _ <- w ^. jss ("__leksahXtermLight"  :: Text) xl
            void $ w ^. js0 ("leksahRetheme" :: Text)

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
      -- Native view-leaf editors aren't in the tab fan above; their changes
      -- trigger the background build the same way.
      <> ((^.. (to $ \_ -> do
        tb <- readIDE triggerBuild
        void . liftIO $ tryPutMVar tb ())) <$> layoutEditorChangedE)
      -- ⌘W close menu: "Hide Window"/"Move Pane to Hidden Window" add a tmux
      -- window to 'hiddenWindows' (dropped from tab rows + flipper, kept alive);
      -- focusing a pane in a hidden window (tree/claude/editor reopen) un-hides it.
      <> ((\sw -> [modifyIDE_ (hiddenWindows %~ S.insert sw)]) <$> hideOrMoveWinE)
      <> ((\sw -> [modifyIDE_ (hiddenWindows %~ S.delete sw)]) <$> unhideWinE)
      -- Leksah windows: reconcile against the live pane tree (recomputed
      -- from the CURRENT ide inside the mutation, so nothing stale is
      -- written; 'applyReconcile' also keeps the wide0 tab lists in step).
      <> ((\tree -> [modifyIDE_ (applyReconcile tree)]) <$> needsReconcileGatedE)
      -- ⌥-open / ⌘D new content into an existing leksah window's split
      -- (see leafOpenE).
      <> ((\(lwi, pc, vert) ->
             [modifyIDE_ (leksahWindows %~ M.adjust (leafOpenPane pc vert) lwi)])
            <$> leafOpenE)
      -- ⌘W closed a native view pane (saved first if dirty — see leafCloseReqE).
      <> ((\(n, l) -> [modifyIDE_ (leksahWindows %~ M.adjust (closeLeaf l) n)])
            <$> delayedLeafCloseE)
      -- ⌘D conversion: record the pane→view overlay in the shared state (the
      -- CC widgets render it; every OS window sees it via the resync poll).
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
      -- Closing a tab also drops it from the shared flip MRU, so the flipper
      -- forgets it at once (the display filter would hide it anyway, but a
      -- stale entry would otherwise resurface a reopened tab mid-list).
      <> ((\ks -> [modifyIDE_ (flipMru %~ filter (\case
              FlipTab k -> k `notElem` ks
              _         -> True))]) <$> closeTabsE)
      -- …and a closed view leaf drops its FlipView entry the same way.
      <> ((\(n, LeafId l) -> [modifyIDE_ (flipMru %~ filter (/= FlipView n l))])
            <$> delayedLeafCloseE)
      -- A closed pane also forgets which AI session it aimed at.  Keyed by the
      -- PANE, so this is the only thing that drops a binding — a binding whose
      -- SESSION has exited is kept deliberately (it gets resumed on next use).
      -- 'PRTmux' entries need no sweep: tmux never reuses a @%N@ pane id, so a
      -- stale one can't be mistaken for a new pane.
      <> ((\ks -> [modifyIDE_ (paneAISession %~ M.filterWithKey (\r _ -> case r of
              PRTab k -> k `notElem` ks
              _       -> True))]) <$> closeTabsE)
      <> ((\(n, LeafId l) ->
              [modifyIDE_ (paneAISession %~ M.delete (PRLeaf n l))])
            <$> delayedLeafCloseE)
      -- …and picking a session in the AI picker makes it that pane's default.
      <> ((\(r, sid) -> [modifyIDE_ (paneAISession %~ M.insert r sid)])
            <$> aiBindE)
      <> ((\k -> [modifyIDE_ (webWindows %~ activateWide0 wid k)]) <$> wide0ActivateE)
      -- Cross-window flip: make the selected tab active in ITS window (which was
      -- just raised), without moving it here.  A pane's tab is resolved from
      -- the live leksah-window map inside the mutation.
      <> ((\(tree, (w, fi)) -> [modifyIDE_ (\i ->
              let lws = i ^. leksahWindows
                  mk = case fi of
                    FlipTab k       -> Just k
                    FlipView n _    -> Just (LeksahWinKey n)
                    FlipPane s wi _ ->
                      maybe (termTabKey lws s) (Just . LeksahWinKey)
                            (paneOwnerLw lws tree s wi)
                  -- A view-leaf selection also focuses that leaf in its window.
                  focusView i' = case fi of
                    FlipView n l -> i' & leksahWindows
                        %~ M.adjust (\lw -> lw { lwFocused = Just (LeafId l) }) n
                    _            -> i'
              in case mk of
                   Just k  -> focusView i & webWindows %~ activateWide0 w k
                   Nothing -> i)]) <$> attach (current allTreeD) crossFlipE)
      -- An in-window view-leaf flip commit focuses the leaf (its lw tab was
      -- selected via flipTabE).
      <> ((\(n, l) -> [modifyIDE_ (leksahWindows
              %~ M.adjust (\lw -> lw { lwFocused = Just (LeafId l) }) n)])
            <$> flipViewE)
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
