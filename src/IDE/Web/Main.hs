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
import Control.Lens (to, view, (^.), (^..), (^?), (?~), (.~), (%~), _Just)
import Control.Monad (forever, when, void)
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
import Data.IORef (newIORef)
import Data.Map (mapKeys)
import qualified Data.Map as M (keys, toList, fromList, union, findWithDefault, lookup, null)
import qualified Data.Set as S (fromList, delete, singleton)
import Data.Time.Clock (NominalDiffTime)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, unlines, isInfixOf, toLower, null)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import Text.Printf (printf)

import System.Directory
       (doesFileExist, doesDirectoryExist, getDirectoryContents, removeFile,
        getHomeDirectory)
import System.Process (readProcessWithExitCode)
import Data.List (nub, sort, isPrefixOf, isInfixOf)
import Data.Maybe (fromMaybe)
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
       (JSM, eval, syncPoint, jsg, js0, js1, js2, valToBool, liftJSM)
import Language.Javascript.JSaddle.Warp
       (jsaddleJs, jsaddleOr, debugWrapper)
import GHCJS.DOM.Types (askJSM)
import GHCJS.DOM.Debug (addDebugMenu)

import Reflex
       (switchDyn, mergeList, foldDyn, traceEventWith, constDyn, ffor,
        Dynamic, Event, holdDyn, merge, newTriggerEvent, leftmost, never,
        performEvent_, getPostBuild, performEvent, select, fan, fanMap,
        fmapMaybe, attachWith, attach, current, updated, holdUniqDyn, tag, gate,
        tagPromptlyDyn, debounce, delay)
import Reflex.Dom.Core
       (dynText, elAttr', elDynAttr', (=:), MonadWidget, mainWidgetWithCss)

import IDE.Core.State
       (triggerBuild, readIDE, IDEAction, wsFile, jsContexts, workspace,
        IDEState(..), Prefs(..), TallVisibility(..), IDE(..), IDERef, __,
        reflectIDE, getDataDir, catchIDE, modifyIDE_, prefs, currentState,
        wsProjects, pjPackages, ipdCabalFile, ipdPackageDir, wsActivePackFile)
import IDE.Metainfo.Provider (initInfo)
import IDE.Web.IDERefStore (setGlobalIDERef)
import IDE.Web.CmdServer (startCmdServer)
import IDE.Web.CloseRequest (nextCloseRequest)
import IDE.Web.OpenFileRequest (nextOpenedFile)
import IDE.Web.OpenPanel (runOpenFilePanel, runOpenProjectPanel)
import IDE.Web.SaveRequest (nextSaveRequest)
import IDE.Web.FindRequest (nextFindRequest)
import IDE.Web.RecentFiles (updateRecentFiles)
import IDE.Web.TerminalInput (setActiveTerminal)
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
        FindbarEvents(..),
        _ToolbarCommand, _MenubarCommand, _KeymapCommand, _PackageCommand,
        _ProjectPackageEvents, _ProjectCommand, _NewTerminal, _SelectTerminal,
        _CloseTerminal, _SelectTerminalWindow, _SelectTerminalPane)
import IDE.Web.Layout (layoutCss)
import IDE.Web.Widget.Changes (changesCss, changesWidget)
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
        selectTmuxWindow, selectTmuxPane)
import IDE.Web.Widget.Terminals (terminalsCss, terminalsWidget)
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
      -- Develop mode: rebuilding the leksah package in the IDE triggers
      -- QuitToRestart.  The Gtk front end handles that via its application; the
      -- web front ends have no such hook, so exit with code 2 and let the
      -- wrapper (leksah-nix.sh) rebuild and relaunch.
      when developLeksah $ do
          liftIO . (`reflectIDE` ideR) . void $
              registerEvent ideR "QuitToRestart" $ \e -> do
                  liftIO $ exitImmediately (ExitFailure 2)
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
  _ <- liftIO (readFile $ dataDir </> "xterm/addon-search.js") >>= eval

  -- Makes project-file paths in terminal output Ctrl-clickable (window.LeksahTermLinks).
  _ <- eval terminalLinksJs

  -- Defines window.LeksahTerm: a registry of live xterm.js terminals plus a
  -- write(id, base64) that decodes straight into a Uint8Array and hands the raw
  -- bytes to xterm.  xterm does its own (stateful) UTF-8 decoding, so this both
  -- avoids decoding shell output on the Haskell side and correctly handles
  -- multibyte sequences split across PTY reads.  base64 keeps the payload to
  -- printable ASCII, so jsaddle doesn't have to escape the control bytes that
  -- pervade terminal output.
  _ <- eval terminalWriteJs

  -- Focus the find bar's text input (called when Edit ▸ Find shows it); deferred
  -- to the next frame so the just-revealed input is laid out and focusable.
  _ <- eval focusFindJs

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
    flakeCss

-- Fallback label for a terminal that hasn't reported a window title yet.
defaultTermTitle :: Int -> Text
defaultTermTitle n = "Terminal " <> T.pack (show n)

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
  , "  return { setProjectFiles: setProjectFiles, attach: attach };"
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
  , "  return { register: register, unregister: unregister, write: write };"
  , "})();"
  ]

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

    let label k = dynText $ (\k' titles -> case k' of
            WorkspaceKey   -> "Workspace"
            ErrorsKey      -> "Errors"
            LogKey         -> "Log"
            GrepKey        -> "Grep"
            TerminalsKey   -> "Terminals"
            TerminalKey n  -> M.findWithDefault (defaultTermTitle n) n titles
            MetadataKey    -> "Metadata"
            ChangesKey     -> "Changes"
            EditorKey file -> T.pack $ takeFileName file) <$> k <*> terminalTitlesD

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
    (openFileE, makeEditor) <- editorWidget ide allE saveFileE
    -- File ▸ Open (the native NSOpenPanel on wkwebview) delivers chosen files via
    -- a background thread; open each one in the editor area like any other file.
    (nativeOpenedFileE, fireOpenedFile) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextOpenedFile >>= fireOpenedFile
    let nativeOpenE = (\fp -> EditorKey fp =: ("wide0", Just ())) <$> nativeOpenedFileE
    flipE <- flipperWidget
      recentTabs
      allE
      label
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
        terminalTitleE = fmapMaybe
          (\m -> case [ (n, t) | (TerminalKey n, dm) <- M.toList m
                               , Just (Identity (TerminalTitle t)) <- [DM.lookup TerminalTab dm] ] of
                   [] -> Nothing
                   ps -> Just (M.fromList ps))
          tabE
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
    performEvent_ $ ffor closeTermE $ liftIO . killTerminalSession
    performEvent_ $ ffor selectWinE  $ \(s, w)    -> liftIO (selectTmuxWindow s w)
    performEvent_ $ ffor selectPaneE $ \(s, w, p) -> liftIO (selectTmuxPane s w p)
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
                   terms = [ (TerminalKey n, ("wide0", Just ())) | TerminalKey n <- wsTabs s, n `elem` ids ]
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
    -- Last-used id: max of the restored sessions, then +1 per "New Terminal".
    idCounterD <- foldDyn ($) (0 :: Int) $ leftmost
      [ const . foldr max 0 <$> existingIdsE
      , (\() n -> n + 1) <$> newTermClickE ]
    let newTermIdE = attachWith (\n () -> n + 1) (current idCounterD) newTermClickE
    -- Open terminals: seed with the restored sessions, append new ones, drop
    -- closed ones.
    openTermIdsD <- foldDyn ($) [] $ leftmost
      [ const <$> existingIdsE
      , (\n xs -> xs <> [n]) <$> newTermIdE
      , (\n xs -> filter (/= n) xs) <$> closeTermE ]
    terminalTitlesD <- foldDyn M.union mempty terminalTitleE
    terminalListD <- holdUniqDyn $
      (\ids titles -> [ (n, M.findWithDefault (defaultTermTitle n) n titles) | n <- ids ])
        <$> openTermIdsD <*> terminalTitlesD
    -- The terminal currently shown in the editor area (for highlighting in the
    -- Terminals list).
    activeTermD <- holdUniqDyn $ (\vis -> case M.lookup "wide0" vis of
                                            Just (TerminalKey n) -> Just n
                                            _                    -> Nothing) <$> visibleTabsD
    -- Publish the active terminal so the Tmux menu can send C-b sequences to it.
    performEvent_ $ liftIO . setActiveTerminal <$> updated activeTermD
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
          , openInWide0 <$> newTermIdE
          , openInWide0 <$> selectAnyTermE ]
        closeTabsE = leftmost [ (\n -> [TerminalKey n]) <$> closeTermE, detachCloseE ]
        -- Running a grep brings the Grep pane to the front of its area.
        selectTabE = leftmost [flipE, restoreVisibleE, ("wide1" =: GrepKey) <$ grepReqE]
    (recentTabs, tabE, visibleTabsD, activePaneD, tabCloseBtnE) <- tabsWidget
      initialTabs
      initialVisibleTabs
      openTabsE
      closeTabsE
      selectTabE
      setRecentE
      (\case EditorKey f -> Just (T.pack f); _ -> Nothing)  -- tab tooltip: full path
      (\case EditorKey _ -> Just "Close"; TerminalKey _ -> Just "Detach"; _ -> Nothing)  -- close × tooltip
      (\k _ -> label $ constDyn k)
      (\k selectedE v -> do
        let toDM x = fmap (DM.singleton x . Identity)
        case k of
          WorkspaceKey   -> toDM WorkspaceTab <$> workspaceWidget ide treeHighlightD treeRevealD
          ErrorsKey      -> toDM ErrorsTab <$> errorsWidget ide allE (paneFind ErrorsKey)
          LogKey         -> toDM LogTab <$> logWidget ide (paneFind LogKey)
          GrepKey        -> toDM GrepTab <$> grepWidget grepResultsD (paneFind GrepKey)
          TerminalsKey   -> toDM TerminalsTab <$> terminalsWidget activeTermD terminalListD
          TerminalKey n  -> toDM TerminalTab <$> terminalWidget ide n selectedE
          MetadataKey    -> toDM MetadataTab <$> metadataWidget ide activeFileD revealMetaD (paneFind MetadataKey)
          ChangesKey     -> toDM ChangesTab <$> changesWidget ide (paneFind ChangesKey)
          EditorKey file -> toDM EditorTab <$> makeEditor file selectedE v)
    -- Edit ▸ Find (toolbar button / menu item) toggles the find bar; showing it
    -- focuses its text input.  It starts hidden.  The native macOS menu routes
    -- here via a background thread draining the find-toggle bridge.
    (findBridgeE, fireFindReq) <- newTriggerEvent
    _ <- liftIO . forkIO . forever $ nextFindRequest >> fireFindReq ()
    let findCmdE = leftmost
          [ fmapMaybe (\case CommandFind -> Just (); _ -> Nothing) panelCmdE
          , findBridgeE ]
    findbarVisibleD <- foldDyn (const not) False findCmdE
    performEvent_ $ ffor (fmapMaybe (\v -> if v then Just () else Nothing) (updated findbarVisibleD)) $ \_ ->
      liftJSM . void $ jsg ("window" :: Text) ^. js0 ("leksahFocusFind" :: Text)
    findbarE   <- findbarWidget activePaneD findbarVisibleD
    statusbarE <- statusbarWidget ide

    -- Persist the session (open files, open terminals, visible tabs) whenever it
    -- changes, but only once the saved session has been restored, so the initial
    -- (empty) state can't clobber the file before we've read it.  Debounced so a
    -- burst of restore/open events collapses into a single write.
    restoredFlagD <- holdDyn False (True <$ restoreE)
    tallD <- holdUniqDyn $ view (prefs . to tallVisibility) <$> ide
    wide1D <- holdUniqDyn $ view (prefs . to wide1Visibility) <$> ide
    sessionD <- holdUniqDyn $
      (\rt vis tall recF wide1 ->
          WebSession 2 (map snd rt) (M.toList vis) (Just tall) (Just recF) (Just wide1))
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
  return topEvents
