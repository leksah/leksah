{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A tiny control socket for a running leksah web UI, driven by the
-- @leksah-cmd@ command line tool (see @main/Cmd.hs@).
--
-- leksah listens on a Unix domain socket at @~/.leksah/cmd.sock@.  @leksah-cmd@
-- connects, sends its working directory followed by its argv (each field
-- NUL-separated, so arguments containing spaces/newlines survive), half-closes
-- the write side, and reads back a one-shot text reply.  The commands:
--
--   * @restart@ — exit with code 2, exactly like rebuilding the leksah package
--     in the IDE does, so @leksah-nix.sh@'s loop rebuilds and relaunches.  This
--     replaces the older @dev-relaunch.sh@ request-file mechanism.
--   * @rebuild-self [--no-restart]@ — rebuild leksah in place (the app stays up
--     so the build doesn't run while the window is gone), streaming the build
--     output back to the client; on success exit(2) for a quick relaunch into the
--     new binary, unless @--no-restart@ is given (then the build just lands on
--     disk and the app keeps running — restart later with @leksah-cmd restart@).
--     Long-running, which the one-shot streamed reply handles fine: the
--     client half-closes after sending, then prints whatever the server streams
--     until it closes.
--   * @editor open FILE…@ (alias @cm open@) — open each file in the editor
--     area, reusing the same bridge the native "Open File" dialog feeds.
--   * @project open FILE…@ — add each project file to the workspace, like the
--     GTK @projectOpen@ / the native open-project dialog.
--   * @js eval CODE@ — evaluate CODE in leksah's JS engine(s) and reply with the
--     result (handy for poking at the live page from a shell).
--
-- Relative paths are resolved against the *client's* working directory (sent as
-- the first field), not leksah's.
#if defined(ghcjs_HOST_OS)

-- Browser build: no unix sockets, so no control-socket server.  The restart
-- suppression flag is kept (the develop-mode code in IDE.Web.Main references
-- it), though nothing arms it in a browser.
module IDE.Web.CmdServer
  ( startCmdServer
  , cmdSocketPath
  , suppressNextRestart
  ) where

import Data.IORef (IORef, newIORef)
import System.IO.Unsafe (unsafePerformIO)
import IDE.App (App)

startCmdServer :: App -> IO ()
startCmdServer _ = return ()

cmdSocketPath :: IO FilePath
cmdSocketPath = return "/no-cmd-socket-in-the-browser"

{-# NOINLINE suppressNextRestart #-}
suppressNextRestart :: IORef Bool
suppressNextRestart = unsafePerformIO (newIORef False)

#else
module IDE.Web.CmdServer
  ( startCmdServer
  , cmdSocketPath
  , suppressNextRestart
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
       (MVar, newMVar, tryTakeMVar, putMVar, newEmptyMVar, takeMVar)
import Control.Exception (SomeException, catch, finally, try)
import GHC.Conc (threadStatus, ThreadStatus(..))
import GHC.Conc.Sync (listThreads, threadLabel)
import GHC.Stack.CloneStack (cloneThreadStack, decode, StackEntry(..))
import Data.List (isPrefixOf, isInfixOf)
import Control.Lens ((^.), (%~))
import Control.Monad (filterM, forM_, forever, void, when, (<=<))

import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import qualified Data.ByteString as BS
import qualified Data.Map as M (insert)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import System.Directory
       (getHomeDirectory, removeFile, doesFileExist, doesDirectoryExist,
        createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath (isRelative, (</>))
import System.IO (hSetBinaryMode)
import System.IO.Unsafe (unsafePerformIO)
import IDE.Utils.ExitImmediately (exitImmediately)
import IDE.Web.GhciMode
       (ghciMode, registerGhciCleanupNamed, stopForGhci, suspendForGhci)
import System.Process
       (createProcess, proc, shell, waitForProcess, CreateProcess(std_out, std_in),
        StdStream(CreatePipe, NoStream))

import Network.Socket
       (Socket, Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix),
        socket, bind, listen, accept, connect, close, defaultProtocol,
        withFdSocket, setCloseOnExecIfNeeded)
import Network.Socket.ByteString (recv, sendAll)

import Language.Javascript.JSaddle (eval, valToText)
import Text.Printf (printf)

import Control.Lens (view)
import qualified Data.Map as Map
import System.Log.Logger
       (Priority(..), updateGlobalLogger, setLevel)

import IDE.App
       (App, appBuilder, appJSMResults, appProblems, appUi, appWorkspace)
import IDE.Builder (buildActiveTarget, runVerbWait)
import IDE.Problems (problemsCell)
import IDE.Problems.Types
       (Pos(..), Problem(..), Range(..), Severity(..))
import IDE.Reactive (modifyCell, readCell)
import IDE.Web.Model
       (AIPaneRef(..), TabKey(..), paneAISession, webWindows)
import IDE.Workspace
       (activeProject, prDir, projectOpenPath, setProjectCmdPrefix,
        workspaceActivatePackage, wsCell, wsProjectKey, wsProjects, wsSpec)
import qualified IDE.Ws.File as WF
import IDE.Ws.Registry (detectProject)
import IDE.Ws.Types (Package(..), Project(..), ProjectKey(..), Verb(..))
import IDE.Web.FS (fsEffects)
import IDE.Utils.RemoteExec (resolveProjectInput)
import IDE.Utils.RemotePath (isRemotePath)
import IDE.LSP (requestTerminalHover)
import IDE.Web.Agent
       (ForkPlace(..), ForkRequest(..), agentList, agentRead, agentSend,
        agentStatus, emptyForkRequest, forkAgent)
import IDE.Web.AgentInfo (describeAgent)
import IDE.Web.WorktreeRegistry
       (ClaimRole, parseRole, recordBranchMove, registerWorktree)
import IDE.Web.Claude (sessionOwningPid, showLiveSession)

import IDE.Web.Instance (cmdSocketFileName)
import IDE.Web.Handoff (handoffEnabled, requestHandoff)
import IDE.Web.OpenFileRequest (deliverOpenedFile)
import IDE.Web.RegionGrabRequest (requestRegionGrab)
import IDE.Web.RemoteTermRequest (requestRemoteTerm)
import IDE.Web.ScreenshotRequest (requestScreenshot)
import IDE.Web.Heartbeat (lastBeatAge)
import IDE.Web.SnapRequest (requestSnapPane)

-- | The control socket both sides agree on: @~/.leksah/cmd.sock@ for the
-- default instance, @~/.leksah/cmd-\<port\>.sock@ under a non-default
-- @LEKSAH_PORT@ (see 'cmdSocketFileName').
cmdSocketPath :: IO FilePath
cmdSocketPath = do
  home <- getHomeDirectory
  return $ home </> ".leksah" </> cmdSocketFileName

-- | Start the control socket listener on a background thread and return.  Any
-- stale socket file from a previous run is removed first; failures to bind are
-- swallowed (a missing control socket just means @leksah-cmd@ won't work, which
-- mustn't take the IDE down).  On Windows the AF_UNIX socket call fails at
-- runtime and lands in the same catch — the control server is simply absent
-- there (leksah-cmd isn't built on Windows either).
startCmdServer :: App -> IO ()
startCmdServer app = void . forkIO $ serve `catch` \(_ :: SomeException) -> return ()
  where
    serve = do
      path <- cmdSocketPath
      createDirectoryIfMissing True =<< (</> ".leksah") <$> getHomeDirectory
      acquire path (240 :: Int)

    -- Acquire the control socket.  Only reclaim the file if nothing is listening
    -- on it.  A live listener normally means another instance on this same
    -- LEKSAH_PORT already owns it — don't steal it (that orphaned the older
    -- instance's leksah-cmd); abort, leaving this instance without a control
    -- socket.  A dead socket file (stale from a crash) has no listener, so we
    -- remove and rebind.  Under the handoff (see IDE.Web.Handoff) the live owner
    -- is our own predecessor, about to be retired — so retry instead of aborting
    -- until it releases the socket (bounded, ~120s).
    acquire path retriesLeft = do
      exists <- doesFileExist path
      live   <- if exists then socketInUse path else return False
      if live
        then if handoffEnabled && retriesLeft > 0
               then threadDelay 500000 >> acquire path (retriesLeft - 1)
               else ioError (userError ("cmd socket in use: " <> path))
        else do
          when exists $ removeFile path `catch` \(_ :: SomeException) -> return ()
          bindAndServe path

    bindAndServe path = do
      sock <- socket AF_UNIX Stream defaultProtocol
      -- Never let spawned children (tmux, git, language servers …) inherit the
      -- listener: an inheritor outliving this instance keeps the socket
      -- "connectable" after we exit, so the NEXT instance's in-use guard sees
      -- a live listener and silently declines to bind — leaving it without a
      -- control socket (and leksah-cmd hanging in the dead backlog).
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock (SockAddrUnix path)
      listen sock 5
      -- ghci mode: free the fd at teardown (the fresh :main unlinks + rebinds
      -- the path anyway, this just avoids leaking a listener per reload).
      when ghciMode $ registerGhciCleanupNamed "cmd-socket" (close sock)
      forever $ do
        (conn, _) <- accept sock
        void . forkIO $
          (handleConn app conn `catch` \(_ :: SomeException) -> return ())
            `finally` close conn

-- | Is a live listener answering on the AF_UNIX socket at @path@?  We just try
-- to connect: success means someone is listening (a running instance);
-- ECONNREFUSED / ENOENT (a stale file left by a crash, or none) throws and we
-- report it free.  Used to avoid stealing a control socket another instance
-- still owns (see 'startCmdServer').
socketInUse :: FilePath -> IO Bool
socketInUse path = (probe `catch` \(_ :: SomeException) -> return False)
  where
    probe = do
      s <- socket AF_UNIX Stream defaultProtocol
      (connect s (SockAddrUnix path) >> return True) `finally` close s

-- | Read the whole request (client half-closes after sending), dispatch it and
-- write the reply.  @restart@ never returns — it exits the process.
handleConn :: App -> Socket -> IO ()
handleConn app conn = do
  raw <- recvAll conn
  let fields = map (decodeUtf8With lenientDecode) (BS.split 0 raw)
  case fields of
    -- The client's pid arrives as a tagged @pid=N@ field after the cwd (see
    -- 'payloadFor' in main/Cmd.hs).  Optional: a leksah-cmd built before it
    -- exists simply sends the verb here, and no verb is ever @pid=…@.
    (cwdT : rest) ->
      let (mpid, argsT) = case rest of
            (f : more) | Just n <- T.stripPrefix "pid=" f
                       , Just p <- readMaybe (T.unpack n) -> (Just (p :: Int), more)
            _                                             -> (Nothing, rest)
      in dispatch mpid (T.unpack cwdT) argsT
    []             -> reply "leksah-cmd: empty request\n"
  where
    reply = sendAll conn . encodeUtf8

    -- Resolve a (possibly relative) client path against the client's cwd.
    -- ssh://host/… paths pass through untouched.
    resolve cwd p = let s = T.unpack p in
        if isRemotePath s || not (isRelative s) then s else cwd </> s

    -- Resolve user input that may also be scp-style (host:~/path etc.) —
    -- one cached ssh round trip expands a leading ~ (see RemoteExec).
    resolveInput cwd p = resolveProjectInput cwd p >>= \case
        Left err -> return (Left (err <> "\n"))
        Right fp -> return (Right fp)

    -- | Bind each opened file's editor tab to the Claude session that asked for
    -- it, when the request came from one.  The pane reference is derivable from
    -- the path alone ('PRTab' of its 'EditorKey'), so this needs no hook into
    -- pane creation — and because it goes into '_paneAISession' it is persisted
    -- like any explicit choice.  A request from a human shell binds nothing.
    bindOpenedTo mpid files = forM_ mpid $ \pid ->
        sessionOwningPid pid >>= \case
          Nothing  -> return ()
          Just sid -> modifyCell (appUi app) $ paneAISession %~ \m ->
              foldr (\fp -> M.insert (PRTab (EditorKey fp)) sid) m files

    dispatch mpid cwd = \case
      ("restart" : args) -> do
        -- @--no-rebuild@ exits 3 instead of 2; leksah-nix.sh's loop treats 3 as
        -- "relaunch but skip the cabal build" (safe after rebuild-self already
        -- built), avoiding the redundant build + its `nix develop`.  A loop that
        -- predates this only continues on 2, so plain restart stays 2.
        let noRebuild = "--no-rebuild" `elem` args
        if ghciMode
          then do
            -- ghci mode: there is no relaunch loop — tear down and return to
            -- the ghci prompt; the client (leksah-cmd) drives :reload/:main.
            reply "Stopping leksah (ghci mode: back to the prompt for :reload / :main).\n"
            threadDelay 100000
            stopForGhci
          else if handoffEnabled
            -- Zero-downtime handoff: stay up and let the supervisor loop start a
            -- successor, retiring us only once it's ready (see IDE.Web.Handoff).
            then do
              reply "Handing off to a fresh leksah (staying up until it's ready)…\n"
              requestHandoff noRebuild
            else do
              reply $ if noRebuild
                then "Restarting leksah (exit 3 → leksah-nix.sh relaunches without rebuilding).\n"
                else "Restarting leksah (exit 2 → leksah-nix.sh rebuilds and relaunches).\n"
              -- Give the reply a moment to flush over the socket before we exit.
              threadDelay 100000
              exitImmediately (ExitFailure (if noRebuild then 3 else 2))

      -- @editor open FILE…@ (was @cm open@, renamed once the editor stopped
      -- being CodeMirror-only — Monaco/nano/vim/emacs too); @cm@ kept as a
      -- silent back-compat alias.
      (verb : "open" : files) | verb `elem` ["editor", "cm"], not (null files) -> do
        results <- mapM (resolveInput cwd) files
        let opened = [ fp | Right fp <- results ]
        mapM_ deliverOpenedFile opened
        -- Opened BY a Claude session (its MCP server or its shell)?  Then its
        -- pane belongs to that session.
        bindOpenedTo mpid opened
        reply $ case [ e | Left e <- results ] of
          []   -> "Opened " <> T.pack (show (length files)) <> " file(s) in the editor.\n"
          errs -> mconcat errs

      ("project" : "open" : files) | not (null files) -> do
        results <- mapM (either return openProject <=< resolveInput cwd) files
        reply $ T.unlines (map T.strip results)

      -- Make the package with this .cabal file (and its project) active —
      -- what the workspace tree's Activate context-menu item does.
      ("package" : "activate" : file : _) ->
        resolveInput cwd file >>= \case
          Left e -> reply e
          Right fp -> do
            ws <- readCell (wsCell (appWorkspace app))
            case [ (project, p)
                 | project <- wsProjects ws
                 , p <- prPackages project
                 , pkgManifest p == fp ] of
              ((project, package):_) -> do
                workspaceActivatePackage (appWorkspace app)
                    (prKey project) (Just (pkgManifest package)) Nothing
                reply $ "Activated " <> T.pack fp <> "\n"
              [] -> reply $ "No package with cabal file " <> T.pack fp <> " in the workspace\n"

      -- Set (or clear, with no prefix argument) the per-project command
      -- prefix — the shell fragment remote tool runs are wrapped in, e.g.
      -- `leksah-cmd project set-prefix host:~/proj/cabal.project nix develop -c`.
      ("project" : "set-prefix" : file : prefixParts) ->
        resolveInput cwd file >>= \case
          Left e -> reply e
          Right fp -> do
            -- Prefer a project already in the workspace (matched by its
            -- file or root — works for remote roots too); otherwise detect.
            ws <- readCell (wsCell (appWorkspace app))
            let inWs = listToMaybe
                  [ wsProjectKey p
                  | p <- WF.wsProjects (view wsSpec ws)
                  , WF.wpFile p == Just fp || WF.wpRoot p == fp ]
            mbKey <- maybe (detectProject fsEffects fp) (return . Just) inWs
            case mbKey of
              Nothing -> reply $ "Not a project file: " <> T.pack fp <> "\n"
              Just pk -> do
                let prefix = T.strip (T.unwords prefixParts)
                setProjectCmdPrefix (appWorkspace app) pk
                    (if T.null prefix then Nothing else Just prefix)
                reply $ "Command prefix for " <> T.pack fp <> ": "
                        <> (if T.null prefix then "(cleared)" else prefix) <> "\n"

      -- Cheap liveness check for `leksah-cmd wait-ready` / `restart --wait`:
      -- answered as soon as the control socket is serving, so it marks the point
      -- the relaunched UI is back.
      -- The age of the last frame-thread heartbeat comes with it: the socket
      -- answering only proves the SERVER thread is alive, and a wedged UI
      -- answers just as promptly.  `wait-ready` requires a fresh beat.
      ("ping" : _) -> do
        age <- lastBeatAge
        reply $ case age of
            Nothing -> "ok building\n"
            Just a  -> "ok beat=" <> T.pack (show (round a :: Int)) <> "s\n"

      -- How this instance runs: "ghci" (leksah.sh --ghci, a cabal repl) or
      -- "binary".  leksah-cmd picks its rebuild/restart behaviour off this.
      ("mode" : _) -> reply $ if ghciMode then "ghci\n" else "binary\n"

      -- ghci mode only: hand control back to the ghci prompt.  With
      -- --keep-windows just the run loop stops (leksah-cmd hs eval's suspend;
      -- resumeApp at the prompt takes the UI straight back); without it the
      -- full teardown runs (close windows + listeners) so the prompt is ready
      -- for :reload + a fresh :main.
      ("ghci-stop" : args)
        | not ghciMode -> reply "not in ghci mode (start with leksah.sh --ghci)\n"
        | otherwise -> do
            let keepWindows = "--keep-windows" `elem` args
            reply "ok\n"
            threadDelay 100000  -- let the reply flush before the loop stops
            if keepWindows then suspendForGhci else stopForGhci

      -- threads: dump every RTS (green) thread's label + status.  The definitive
      -- view of a pure-Haskell freeze — which thread is blocked on what — that
      -- `sample` can't see (it only shows OS threads; blocked green threads are
      -- invisible).  Long-running threads are labelled at their fork sites
      -- (resync notifiers, bridge drains, per-window frame threads).
      ("threads" : _) -> do
        ts <- listThreads
        lns <- mapM (\t -> do
                 lbl <- threadLabel t
                 st  <- (try (threadStatus t) :: IO (Either SomeException ThreadStatus))
                 return $ T.pack (show t <> "  " <> maybe "-" id lbl
                                  <> "  " <> either (const "?") show st)) ts
        reply (T.unlines (lns <> [T.pack ("total=" <> show (length ts))]))

      -- stacks [SUBSTR]: decoded stack snapshots of the labelled long-running
      -- threads (or, with SUBSTR, of every labelled thread matching it) — shows
      -- the exact call chain a wedged thread is blocked in.  Needs the code
      -- compiled with -finfo-table-map for source locations.
      ("stacks" : rest) -> do
        let want = case rest of
              (s : _) | not (T.null s) -> T.unpack s
              _ -> ""   -- default: the freeze-forensics threads
            -- "-" dumps UNLABELLED threads instead (capped): a thread-spawn
            -- runaway is invisible to a label filter, since the flood is
            -- exactly the threads nothing bothered to name.
            unlabelled = want == "-"
            interesting l
              | null want = any (`isPrefixOf` l)
                              ["resync-notifier", "reflex-frames", "bridge-drain"]
              | otherwise = want `isInfixOf` l
        ts0 <- listThreads
        ts <- if not unlabelled then return ts0 else
                take 4 <$> filterM (\t -> do
                  mlbl <- threadLabel t
                  st <- (try (threadStatus t) :: IO (Either SomeException ThreadStatus))
                  let dead = case st of
                        Right ThreadFinished -> True
                        Right ThreadDied     -> True
                        Left _               -> True
                        _                    -> False
                  return (isNothing mlbl && not dead)) (reverse ts0)
        lns <- fmap concat $ mapM (\t -> do
                 mlbl <- (if unlabelled then const (Just "(unlabelled)") else id) <$> threadLabel t
                 case mlbl of
                   Just l | unlabelled || interesting l -> do
                     st <- (try (threadStatus t) :: IO (Either SomeException ThreadStatus))
                     entries <- (try (cloneThreadStack t >>= decode)
                                   :: IO (Either SomeException [StackEntry]))
                     return $ T.pack ("== " <> show t <> "  " <> l <> "  "
                                      <> either (const "?") show st)
                            : case entries of
                                Left e   -> [T.pack ("   <stack unavailable: " <> show e <> ">")]
                                Right es -> [ T.pack ("   " <> functionName e
                                                      <> "  (" <> moduleName e
                                                      <> " " <> srcLoc e <> ")")
                                            | e <- take 60 es ]
                   _ -> return []) ts
        reply (T.unlines lns)

      -- resync-state: the resync machinery is gone (state is push-per-cell
      -- now); the verb survives as a cell-world summary so old habits and
      -- scripts still get a useful answer.
      ("resync-state" : _) -> do
        ui <- readCell (appUi app)
        ws <- readCell (wsCell (appWorkspace app))
        probs <- readCell (problemsCell (appProblems app))
        reply . T.unlines $
          [ "cells (resync machinery removed: state is push-per-cell)"
          , T.pack ("windows=" <> show (Map.size (view webWindows ui)))
          , T.pack ("projects=" <> show (length (wsProjects ws)))
          , T.pack ("problemSources=" <> show (Map.size probs))
          ]

      -- screenshot FILE: capture the UI to a PNG (native WKWebView snapshot on
      -- macOS).  Relative paths resolve against the client's cwd.
      ("screenshot" : file : _) | not (T.null file) -> do
        let path = resolve cwd file
            -- Right after a relaunch the window/WKWebView may not be wired up yet
            -- (a screenshot then finds no view); retry a few times before giving up.
            tryShot 0 = requestScreenshot (T.pack path)
            tryShot n = requestScreenshot (T.pack path) >>= \case
              True  -> return True
              False -> threadDelay 500000 >> tryShot (n - 1 :: Int)
        ok <- tryShot 6
        reply $ if ok
          then "Wrote screenshot to " <> T.pack path <> "\n"
          else "screenshot: failed — no capture handler (the wkwebview and "
            <> "webkitgtk front ends support it) or the snapshot errored.\n"

      -- grab-region [TARGET]: interactively select a screen rectangle
      -- (`screencapture -i`) and type the resulting PNG's path into a terminal
      -- pane, so its program (e.g. a claude session) can pick the image up.
      -- TARGET is a session/window/pane path (default: the regionCaptureTarget
      -- preference); no argument uses the preference.
      ("grab-region" : rest) -> do
        let mbTarget = case rest of (t : _) | not (T.null t) -> Just t; _ -> Nothing
        requestRegionGrab mbTarget
        reply ("grab-region: select a rectangle. If Screen Recording permission is "
              <> "granted you'll get the system crosshair; otherwise drag inside the "
              <> "leksah window. The image path is typed into the target pane.\n")

      ("js" : "eval" : codeParts) | not (null codeParts) -> do
        let code = T.intercalate " " codeParts
        evalJs code >>= reply

      -- open-browser <pane_id> <url>: launch the default browser and (if run in a
      -- leksah tmux pane) snap its window over that pane.  The pane id comes from
      -- $TMUX_PANE, captured by leksah-cmd.
      ("open-browser" : pane : url : _) | not (T.null url) -> do
        _ <- (try (void $ createProcess (proc "open" [T.unpack url]))
                :: IO (Either SomeException ()))
        if T.null pane
          then reply $ "Opened " <> url <> " (not run in a leksah pane; not snapped).\n"
          else do
            requestSnapPane pane
            reply $ "Opened " <> url <> ", snapping the browser to pane " <> pane <> ".\n"

      -- rebuild-self: build leksah through the IDE's own build system, so
      -- errors and warnings land in the UI (Errors/Log panes) — and, with ghci
      -- mode on, go through ffcabal's cached repls.  On a successful build of
      -- the leksah package makePackage' triggers QuitToRestart (exit 2 →
      -- relaunch); --no-restart arms 'suppressNextRestart' so the handler
      -- swallows that one restart.  --use-cabal is the FAILSAFE: it bypasses
      -- leksah's build code entirely (in case we broke it) and runs
      -- ~/.leksah/rebuild.sh directly, streaming output back here — also the
      -- automatic fallback when no leksah package is open in the workspace.
      ("rebuild-self" : args) -> do
        let noRestart = "--no-restart" `elem` args
            useCabal  = "--use-cabal" `elem` args
        mbTarget <- if useCabal then return Nothing else findLeksahPackage
        case mbTarget of
          Nothing
            | useCabal  -> rebuildSelf noRestart
            | otherwise -> do
                reply ("rebuild-self: no leksah package in the workspace — using the "
                      <> "direct cabal build instead.\n")
                rebuildSelf noRestart
          Just (project, package) -> do
            reply $ "Rebuilding leksah via the IDE build system — output appears in "
                 <> "the IDE (Errors/Log panes)"
                 <> (if noRestart
                       then "; the app stays up (--no-restart).\n"
                       else "; on success it restarts.\n")
                 <> "(Failsafe if the IDE build is broken: rebuild-self --use-cabal)\n"
            void . forkIO $ do
                r <- runVerbWait (appBuilder app) (prKey project)
                        (Just package) Nothing VBuild
                case r of
                  Just ExitSuccess
                    | not noRestart ->
                        -- The self-build restart contract: relaunch into the
                        -- fresh build (same exits the old QuitToRestart used).
                        if ghciMode
                          then stopForGhci
                          else if handoffEnabled
                            then requestHandoff True
                            else exitImmediately (ExitFailure 2)
                  _ -> return ()

      -- Fired by tmux's after-select-window / after-select-pane hooks: poke the
      -- reflex network (reusing the JS trigger the ⌃B/mousedown listener uses) so
      -- the pane tree is re-read and the focused terminal's new active pane floats
      -- to the flipper/tab MRU front, without waiting for the 2 s poll.
      ("term-activity" : _) -> do
        let poke = "window.leksahTermActivity && window.leksahTermActivity()" :: Text
        _ <- (try (appJSMResults app (void (eval poke)))
                :: IO (Either SomeException [()]))
        -- No reply: this is fired by a tmux run-shell hook, which would surface
        -- any stdout as an "ok" view on every window/pane select.
        return ()

      -- cc-connect HOST[#TARGET]: open a terminal tab attached to HOST's tmux
      -- over ssh in control mode (native pane rendering; TerminalCC).  With
      -- #TARGET, attach that session (name or $id); without, attach-or-create
      -- the session named 'leksah'.  ssh runs without a PTY and with
      -- BatchMode, so key-based auth must be set up.
      ("cc-connect" : host : _) | not (T.null host) -> do
        requestRemoteTerm host
        reply $ "Opening a control-mode terminal for " <> host
                <> (if "#" `T.isInfixOf` host
                      then "\n"
                      else " (tmux session 'leksah' on the remote; created if"
                           <> " missing).\nUse HOST#TARGET for a specific"
                           <> " session, e.g. cc-connect '" <> host <> "#0'.\n")

      -- log LOGGER LEVEL: set an hslogger logger's level at runtime (no restart).
      -- e.g. `leksah-cmd log leksah.focus debug` turns on focus/activation
      -- diagnostics (→ ~/.leksah/focus-debug.log); `… off` silences them.
      ("log" : loggerName : levelT : _) | not (T.null loggerName) -> do
        let lvl = case T.toLower levelT of
              "debug"   -> Just DEBUG
              "info"    -> Just INFO
              "notice"  -> Just NOTICE
              "warning" -> Just WARNING
              "error"   -> Just ERROR
              "off"     -> Just EMERGENCY
              _         -> Nothing
        case lvl of
          Nothing -> reply $ "unknown level " <> levelT
                     <> " (debug|info|notice|warning|error|off)\n"
          Just l -> do
            updateGlobalLogger (T.unpack loggerName) (setLevel l)
            reply $ "logger " <> loggerName <> " -> " <> levelT <> "\n"

      -- diagnostics [FILE] [--all]: the current compiler/LSP errors and
      -- warnings (the Errors pane's model), one per line — the backend of the
      -- MCP `diagnostics` tool, so agents ask instead of grepping build logs.
      -- Scoped to the active project unless --all; FILE filters to one file.
      ("diagnostics" : rest) -> do
        let allScope = "--all" `elem` rest
            mfile = case filter (/= "--all") rest of
              (f : _) | not (T.null f) -> Just (resolve cwd f)
              _                        -> Nothing
        probs <- readCell (problemsCell (appProblems app))
        ws <- readCell (wsCell (appWorkspace app))
        let activeRoot = prDir <$> activeProject ws
            inScope src = allScope || case activeRoot of
              Nothing -> True
              Just r  -> src == "build:" <> T.pack r
                      || src == "lsp:" <> T.pack r
            -- a source key's root resolves its problems' relative paths
            srcRoot src = T.unpack . fromMaybe src $
                T.stripPrefix "build:" src `orElseT` T.stripPrefix "lsp:" src
            orElseT a b = maybe b Just a
            fullPath src p
              | isRelative (pPath p) = srcRoot src </> pPath p
              | otherwise            = pPath p
            refs = [ (fullPath src p, p)
                   | (src, ps) <- Map.toList probs
                   , inScope src
                   , p <- ps
                   , maybe True (\f -> fullPath src p == f) mfile ]
            sev p = case pSeverity p of
              SevError   -> "error"
              SevWarning -> "warning"
              SevHint    -> "lint"
              SevInfo    -> "note"
            one (fp, p) = let Pos l c = rFrom (pRange p) in
              sev p <> " " <> T.pack fp
                <> ":" <> T.pack (show (l + 1))
                <> ":" <> T.pack (show (c + 1))
                -- indent continuation lines so one ref = one visual block
                <> " " <> T.replace "\n" "\n    " (T.strip (pMessage p))
            scope = if allScope then " (all projects)" else " (active project)"
        reply $ if null refs
          then "no diagnostics" <> scope <> "\n"
          else T.pack (show (length refs)) <> " diagnostic(s)" <> scope <> ":\n"
               <> T.unlines (map one refs)

      -- active-selection: the focused editor's file plus the selection's
      -- 1-based start/end lines, tab-separated (the MCP `active_selection`
      -- backend).  Empty everywhere → no editor focused.
      ("active-selection" : _) -> do
        r <- evalJs activeSelectionJs
        reply $ case [ l | l <- T.lines r, not (T.null (T.strip l)) ] of
          (l : _) -> l <> "\n"
          []      -> "(no editor focused)\n"

      -- build: kick off the active target's build (Haskell package or custom
      -- project), exactly like the Build toolbar button.  Asynchronous — the
      -- output lands in the IDE; agents poll `diagnostics` for the result.
      ("build" : _) -> do
        reply ("build started — output lands in the IDE's Errors/Log panes; "
              <> "poll `diagnostics` for the result.\n")
        buildActiveTarget (appBuilder app)

      -- hover FILE LINE [COL]: the LSP hover (type/docs) at a position, plus
      -- the file's diagnostics summary — the same lookup the terminal file-link
      -- tooltips use.  LINE/COL are 1-based.
      ("hover" : file : lineT : rest)
        | Just ln <- readMaybe (T.unpack lineT) -> do
            let mcol = case rest of
                  (c : _) -> (\n -> max 0 (n - 1)) <$> readMaybe (T.unpack c)
                  _       -> Nothing
            v <- newEmptyMVar
            requestTerminalHover (resolve cwd file) (Just ln) mcol (putMVar v)
            r <- timeout 20000000 (takeMVar v)
            reply $ case r of
              Just (Just t) -> t <> "\n"
              Just Nothing  -> "no hover information at that position\n"
              Nothing       -> "hover: timed out (is the language server still starting?)\n"

      -- agent …: one Claude session driving another (see IDE.Web.Agent).  The
      -- caller identifies itself by pid, so `agent fork` needs no arguments —
      -- "fork me, beside me" is the whole common case.
      ("agent" : rest) -> agentVerb mpid cwd rest

      ("help" : _) -> reply usage
      []            -> reply usage
      other         -> reply $ "leksah-cmd: unknown command: "
                                  <> T.unwords other <> "\n\n" <> usage

    -- One @agent@ verb.  Every one of them starts by asking WHO is calling:
    -- the client's pid walked up to the @claude@ process above it, if any.  That
    -- is what lets `agent fork` take no arguments (fork me, beside me), `agent
    -- list` mark "(you)", and a child's report reach the right parent.
    agentVerb mpid cwd args = do
      me <- maybe (return Nothing) sessionOwningPid mpid
      case args of
        ("fork" : rest) -> case parseFork cwd me rest of
          Left err -> reply ("agent fork: " <> err <> "\n\n" <> agentUsage)
          Right fr -> forkAgent fr >>= \case
            Left err       -> reply ("agent fork: " <> err <> "\n")
            Right (_, msg) -> reply msg
        ("list" : _)  -> agentList me >>= reply
        ("me" : _)    -> reply $ case me of
            Just sid -> sid <> "\n"
            Nothing  -> "not called from a Claude Code session\n"
        ("status" : sid : _) | not (T.null sid) -> agentStatus sid >>= reply
        ("send" : sid : rest)
          | not (T.null sid), (submit, ps) <- sendFlags rest, not (null ps) ->
              agentSend sid submit (T.intercalate " " ps) >>= reply
        ("read" : sid : rest) | not (T.null sid) ->
            agentRead sid (fromMaybe 1 (intFlag ["--last", "-n"] rest)) >>= reply
        ("show" : sid : _) | not (T.null sid) -> showLiveSession sid >>= \ok ->
            reply $ if ok then "Showing " <> sid <> ".\n"
                          else "Could not find a pane for " <> sid <> ".\n"
        -- How an agent says what it is doing, for the Agents pane: a title and
        -- a small HTML description.  The session defaults to the CALLER, so the
        -- usual call names no session at all.
        ("describe" : rest) -> case parseDescribe me rest of
          Left err          -> reply ("agent describe: " <> err <> "\n\n" <> agentUsage)
          Right (sid, t, h) -> describeAgent sid t h >>= reply
        -- How a session (or a hook acting for one) records its relationship
        -- with a git worktree — the Worktrees tree node and the Agents pane
        -- read these back.  The worktree defaults to the CALLER's cwd, the
        -- session to the caller, so the usual call is just
        -- `agent register --role working --note '…'`.
        ("register" : rest) -> case parseRegister cwd me rest of
          Left err -> reply ("agent register: " <> err <> "\n\n" <> agentUsage)
          -- --branch-only (the git hook's switch/checkout path) records the
          -- branch move without touching claims; a full register upserts the
          -- caller's claim too.
          Right (path, mrole, mbranch, note, msid, branchOnly)
            | branchOnly ->
                recordBranchMove path mbranch msid "hook" >>= \r ->
                  reply (r <> "\n")
            | otherwise ->
                registerWorktree path mbranch msid mrole note "agent" >>= \r ->
                  reply (r <> "\n")
        _ -> reply agentUsage
      where
        -- Only leading flags are flags, so a message that starts with a dash
        -- still arrives intact.
        sendFlags = goSend False
        goSend _ ("--submit" : as)    = goSend True as
        goSend _ ("--no-submit" : as) = goSend False as
        goSend s as                   = (s, as)
        intFlag names as = listToMaybe
          [ n | (f, v) <- zip as (drop 1 as)
              , T.unpack f `elem` names
              , Just n <- [readMaybe (T.unpack v)] ]

    -- @agent fork@'s flags; anything left over is the child's first prompt (its
    -- fields joined, so both `fork 'a b'` and `fork a b` work).
    parseFork cwd me = go (emptyForkRequest cwd) { frParent = me }
      where
        go fr [] = Right fr
        go fr (a : as) = case a of
          "--beside"   -> go fr { frPlace = PlaceBeside } as
          "--below"    -> go fr { frPlace = PlaceBelow  } as
          "--vertical" -> go fr { frPlace = PlaceBelow  } as
          "--tab"      -> go fr { frPlace = PlaceTab    } as
          "--window"   -> go fr { frPlace = PlaceTab    } as
          "--fresh"    -> go fr { frFresh = True } as
          "--from"     -> arg as $ \v as' -> go fr { frFrom = Just v } as'
          "--dir"      -> arg as $ \v as' -> go fr { frDir = Just (resolve cwd v) } as'
          "--"         -> Right (withPrompt fr as)
          _ | "-" `T.isPrefixOf` a, T.length a > 1 ->
                Left ("unknown option " <> a)
            | otherwise -> Right (withPrompt fr (a : as))
        arg (v : as) k | not ("-" `T.isPrefixOf` v) = k v as
        arg _ _ = Left "missing value for an option"
        withPrompt fr ps =
          let p = T.strip (T.intercalate " " ps)
          in fr { frPrompt = if T.null p then Nothing else Just p }

    -- @agent describe@'s arguments: an optional leading session id (default:
    -- the caller), then --title / --html.  Values are taken verbatim — an HTML
    -- fragment is full of punctuation, so nothing in it may be read as a flag.
    parseDescribe me = go Nothing Nothing Nothing
      where
        go msid t h [] = case msid `orElse` me of
          -- (This module is compiled with CPP, which splices backslash-continued
          -- lines — so long strings here are concatenated, never string gaps.)
          Nothing  -> Left ("no session: run this from a Claude session, or name"
                              <> " one — `agent describe SID --title …`")
          Just sid
            | t == Nothing && h == Nothing ->
                Left "nothing to record: pass --title and/or --html"
            | otherwise -> Right (sid, t, h)
        go msid t h (a : as) = case a of
          "--title" -> val as $ \v as' -> go msid (Just v) h as'
          "--html"  -> val as $ \v as' -> go msid t (Just v) as'
          "--desc"  -> val as $ \v as' -> go msid t (Just v) as'
          _ | "-" `T.isPrefixOf` a -> Left ("unknown option " <> a)
            | msid == Nothing      -> go (Just a) t h as
            | otherwise            -> Left ("unexpected argument " <> a)
        val (v : as) k = k v as
        val []       _ = Left "missing value for an option"
        orElse x y = maybe y Just x

    -- @agent register@'s flags.  All optional: the worktree defaults to the
    -- caller's cwd, the session to the caller (or to no session at all — a
    -- human registering from a shell is fine, the claim just says so).
    parseRegister :: FilePath -> Maybe Text -> [Text]
                  -> Either Text (FilePath, Maybe ClaimRole, Maybe Text, Text, Maybe Text, Bool)
    parseRegister cwd me = go Nothing Nothing Nothing Nothing Nothing False
      where
        go mw mr mb mn ms bo [] =
          Right (fromMaybe cwd mw, mr, mb, fromMaybe "" mn, ms `orElse` me, bo)
        go mw mr mb mn ms bo (a : as) = case a of
          "--worktree" -> val as $ \v as' -> go (Just (resolve cwd v)) mr mb mn ms bo as'
          "--role"     -> val as $ \v as' -> case parseRole v of
            Just r  -> go mw (Just r) mb mn ms bo as'
            Nothing -> Left ("unknown role " <> v
                             <> " (created|working|reviewing|abandoned)")
          "--branch"   -> val as $ \v as' -> go mw mr (Just v) mn ms bo as'
          "--note"     -> val as $ \v as' -> go mw mr mb (Just v) ms bo as'
          "--session"  -> val as $ \v as' -> go mw mr mb mn (Just v) bo as'
          "--branch-only" -> go mw mr mb mn ms True as
          _ -> Left ("unexpected argument " <> a)
        val (v : as) k = k v as
        val []       _ = Left "missing value for an option"
        orElse x y = maybe y Just x

    agentUsage = T.unlines
      [ "leksah-cmd agent — Claude sessions starting and driving each other:"
      , "  fork [OPTS] [PROMPT]  start an agent in a pane beside you, forked from"
      , "                        your conversation (so it has your context)"
      , "      --below           split top/bottom instead of side by side"
      , "      --tab             give it its own leksah tab instead"
      , "      --fresh           no inherited context: it knows only PROMPT"
      , "      --from SID        fork that session instead of the calling one"
      , "      --dir DIR         work here (only with --fresh: see `agent fork`)"
      , "  list                  live sessions: id, state, pane, dir, title"
      , "  me                    the session id of the caller"
      , "  status SID            one line: state, detail, pane, dir"
      , "  wait SID [--timeout S]  block until SID is idle/waiting (client-side)"
      , "  read SID [--last N]   SID's last N answers, newest last"
      , "  send SID [--submit] TEXT   type TEXT into SID (--submit presses Enter)"
      , "  show SID              bring SID's pane to the front in the UI"
      , "  describe [SID] --title T --html H   how you appear in the Agents pane"
      , "                        (SID defaults to the calling session)"
      , "  register [--worktree DIR] [--role created|working|reviewing|abandoned]"
      , "           [--branch B] [--note TEXT] [--session SID]"
      , "                        record your relationship with a git worktree"
      , "                        (worktree defaults to your cwd, session to you;"
      , "                        re-register with --branch after re-pointing one)"
      ]

    -- The workspace's leksah package (project, package), if it's open.
    findLeksahPackage = do
        ws <- readCell (wsCell (appWorkspace app))
        return $ listToMaybe
            [ (project, p)
            | project <- wsProjects ws
            , p <- prPackages project
            , pkgName p == "leksah" ]

    -- A directory becomes a plain-directory project (no build file needed);
    -- otherwise the path is a project file (cabal.project / stack.yaml / …).
    -- Route through 'projectOpenPath' — the single recognition point shared with
    -- the Open Project / Open Folder panels — so Cargo.toml / pyproject.toml /
    -- setup.py (Rust/Python) are recognised here too.
    openProject fp = do
      projectOpenPath (appWorkspace app) fp
      return $ "Opened in workspace: " <> T.pack fp

    -- The user's CODE is evaluated inside a JS-side try/catch: a throwing
    -- expression must never raise into jsaddle itself.  An uncaught JS
    -- exception doesn't just fail this eval — it can poison jsaddle's
    -- async command stream and corrupt OTHER threads' in-flight DOM work
    -- (observed: a probe thrown against a still-building page aborted the
    -- whole initial render, leaving an empty window).
    evalJs code = do
      let wrapped = "(function () { try { return String(eval(" <> jsStringLit code
                    <> ")); } catch (e) { return 'JS error: ' + e; } })()"
      r <- try $ appJSMResults app (eval wrapped >>= valToText)
      return $ case r of
        Left (e :: SomeException) -> "JS error: " <> T.pack (show e) <> "\n"
        Right []                  -> "(no live JS context — is the page loaded?)\n"
        Right results             -> T.unlines results

    -- A JS string literal for arbitrary code (escapes quotes, backslashes,
    -- control characters and the U+2028/U+2029 line separators JS strings
    -- can't contain raw).
    jsStringLit t = "\"" <> T.concatMap esc t <> "\""
      where
        esc '"'      = "\\\""
        esc '\\'     = "\\\\"
        esc '\n'     = "\\n"
        esc '\r'     = "\\r"
        esc '\t'     = "\\t"
        esc '\x2028' = "\\u2028"
        esc '\x2029' = "\\u2029"
        esc c | c < ' '   = T.pack (printf "\\u%04x" (fromEnum c))
              | otherwise = T.singleton c

    -- Rebuild leksah in place (the app keeps running, so the slow build doesn't
    -- happen while the window is gone), streaming the build output back to the
    -- client.  On success, exit(2) so the wrapper relaunches the freshly-built
    -- binary (a quick restart, since the build is already done) — unless
    -- @--no-restart@ was passed, in which case the build lands on disk and the app
    -- keeps running, so you can rebuild repeatedly and only @leksah-cmd restart@
    -- when ready.  (Restarting on every rebuild is how duplicate instances pile
    -- up if a stray relaunch loop is around.)
    rebuildSelf noRestart = do
      home <- getHomeDirectory
      let script = home </> ".leksah" </> "rebuild.sh"
      configured <- doesFileExist script
      if not configured
        then reply ("rebuild-self: not configured — no ~/.leksah/rebuild.sh "
                   <> "(launch leksah via leksah-nix.sh).\n")
        else tryTakeMVar buildLock >>= \case
          Nothing -> reply "rebuild-self: a build is already in progress.\n"
          Just () -> do
            reply ("Rebuilding leksah (the app stays up; it restarts only if the "
                  <> "build succeeds)…\n\n")
            outcome <- try (streamBuild conn script) :: IO (Either SomeException Bool)
            case outcome of
              Right True
                | noRestart -> do
                    putMVar buildLock ()
                    reply ("\nBuild succeeded — app left running (--no-restart). "
                          <> "Run `leksah-cmd restart` to relaunch into it.\n")
                | otherwise -> do
                    threadDelay 150000  -- let the reply flush before we exit
                    -- ghci mode: the client normally reloads via the prompt and
                    -- never gets here, but if it does, stop instead of exiting.
                    if ghciMode then reply "\nBuild succeeded — restarting into the new build.\n" >> stopForGhci
                    -- Handoff: the build already produced the binary, so ask the
                    -- loop for a no-rebuild successor and stay up until it's ready.
                    else if handoffEnabled
                      then reply "\nBuild succeeded — handing off to the new build (staying up until it's ready)…\n" >> requestHandoff True
                      else reply "\nBuild succeeded — restarting into the new build.\n" >> exitImmediately (ExitFailure 2)
              Right False -> do
                putMVar buildLock ()
                reply ("\nBuild FAILED — leksah left running. Fix the errors and "
                      <> "run rebuild-self again.\n")
              Left e -> do
                putMVar buildLock ()
                reply $ "\nrebuild-self error: " <> T.pack (show e) <> "\n"

usage :: Text
usage = T.unlines
  [ "leksah-cmd commands:"
  , "  restart [--no-rebuild]  exit so the wrapper relaunches (--no-rebuild skips the build)"
  , "  rebuild-self [--no-restart] [--use-cabal]"
  , "                          rebuild leksah via the IDE build system (errors in the UI);"
  , "                          restart on success unless --no-restart; --use-cabal is the"
  , "                          failsafe: bypass the IDE build, run cabal directly (streamed)"
  , "  editor open FILE...     open files in the editor (alias: cm)"
  , "  project open FILE...    add project files to the workspace"
  , "  cc-connect HOST         terminal tab on HOST's tmux (ssh, control mode)"
  , "  open-browser URL        open the default browser snapped to this pane"
  , "  js eval CODE            evaluate JS in the running leksah"
  , "  ping                    reply \"ok\" (liveness check for wait-ready)"
  , "  screenshot FILE         capture the UI to a PNG (wkwebview)"
  , "  grab-region [TARGET]    select a screen region → its path into a terminal pane"
  , "  diagnostics [FILE|--all] current errors/warnings (active project; --all = every project)"
  , "  active-selection        focused editor's file + selected line range"
  , "  build                   build the active target (async; poll diagnostics)"
  , "  hover FILE LINE [COL]   LSP hover (type/docs) at a 1-based position"
  , "  agent SUBCOMMAND        Claude sessions starting/driving each other"
  , "                          (fork/list/status/wait/read/send/show — `agent` for details)"
  , "  log LOGGER LEVEL        set an hslogger logger's level live, e.g."
  , "                          `log leksah.focus debug` (→ ~/.leksah/focus-debug.log), `… off`"
  ]

-- | The focused editor's file + 1-based selection start/end lines as
-- @file\\tstart\\tend@ (empty string when no editor is focused) — a copy of
-- 'IDE.Web.Main' 'activeEditorSelectionJs' (that module sits above this one).
activeSelectionJs :: Text
activeSelectionJs = mconcat
  [ "(function(){var v=window.LeksahCM&&window.LeksahCM.activeView;if(!v)return '';"
  , "if(v.__leksahMonaco){"
  , "var md=v.getDomNode&&v.getDomNode();var me=md&&md.closest&&md.closest('.editor');"
  , "var mf=me&&me.getAttribute('data-file');if(!mf)return '';"
  , "var ms=v.getSelection();if(!ms)return '';"
  , "return mf+'\\t'+ms.startLineNumber+'\\t'+ms.endLineNumber;}"
  , "var ed=v.dom&&v.dom.closest&&v.dom.closest('.editor');"
  , "var f=ed&&ed.getAttribute('data-file');if(!f)return '';"
  , "var s=v.state.selection.main;"
  , "var a=v.state.doc.lineAt(s.from).number,b=v.state.doc.lineAt(s.to).number;"
  , "return f+'\\t'+a+'\\t'+b;})()" ]

-- | Held while a 'rebuild-self' build runs, so two clients can't build at once.
{-# NOINLINE buildLock #-}
buildLock :: MVar ()
buildLock = unsafePerformIO (newMVar ())

-- | Armed by @rebuild-self --no-restart@ (IDE-build path): the web front end's
-- @QuitToRestart@ handler (see 'IDE.Web.Main') checks-and-clears this and, when
-- set, swallows that one restart — the build lands on disk and the app stays
-- up, exactly like the old script path's @--no-restart@.
{-# NOINLINE suppressNextRestart #-}
suppressNextRestart :: IORef Bool
suppressNextRestart = unsafePerformIO (newIORef False)

-- | Run @~/.leksah/rebuild.sh@ (written by leksah-nix.sh with the same build
-- options leksah was launched with), streaming its combined stdout/stderr to the
-- client as it goes.  Returns whether the build exited successfully.  The script
-- runs in leksah's own (dev-shell) environment, so it can call @cabal@ directly.
streamBuild :: Socket -> FilePath -> IO Bool
streamBuild conn script = do
  (_, Just hout, _, ph) <-
    createProcess (shell ("sh '" <> script <> "' 2>&1"))
      { std_out = CreatePipe, std_in = NoStream }
  hSetBinaryMode hout True
  -- Read the build's output to EOF no matter what, so the build never blocks on a
  -- full/closed pipe and is always reaped — even if the client (leksah-cmd)
  -- disconnects mid-build (piped to `head`, Ctrl-C'd, …).  Once a send fails we
  -- stop sending but keep draining.  Abandoning the read here used to orphan the
  -- build: it kept running (holding cabal's builddir lock) while rebuildSelf
  -- released the build-lock MVar, so a later rebuild-self ran a *second*
  -- concurrent build on the same builddir.
  let drain sending = do
        chunk <- BS.hGetSome hout 4096
        if BS.null chunk
          then return ()
          else if not sending
            then drain False
            else (sendAll conn chunk >> drain True)
                   `catch` \(_ :: SomeException) -> drain False
  drain True
  (== ExitSuccess) <$> waitForProcess ph

-- | Read until the peer closes its write side (EOF).
recvAll :: Socket -> IO BS.ByteString
recvAll conn = go []
  where
    go acc = do
      chunk <- recv conn 65536
      if BS.null chunk
        then return (BS.concat (reverse acc))
        else go (chunk : acc)

#endif
