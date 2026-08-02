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
import IDE.Core.State (IDERef)

startCmdServer :: IDERef -> IO ()
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
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, putMVar)
import Control.Exception (SomeException, catch, finally, try)
import GHC.Conc (threadStatus, ThreadStatus(..))
import GHC.Conc.Sync (listThreads, threadLabel)
import GHC.Stack.CloneStack (cloneThreadStack, decode, StackEntry(..))
import Data.List (isPrefixOf, isInfixOf)
import Control.Lens ((^.))
import Control.Monad (forever, void, when, (<=<))

import Data.IORef (IORef, newIORef, writeIORef)
import Data.Maybe (listToMaybe)

import qualified Data.ByteString as BS
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

import IDE.Core.State
       (IDERef, reflectIDE, ideJSM, readIDE, workspace, runWorkspace,
        runProject, pjPackages, ipdPackageName, ipdCabalFile, wsProjects,
        setLoggerLevel)
import qualified IDE.Core.State as State (runPackage)
import IDE.Core.Types (filePathToProjectKey, ProjectSettings(..))
import IDE.Utils.RemoteExec (resolveProjectInput)
import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.Instance (cmdSocketFileName)
import IDE.Web.Handoff (handoffEnabled, requestHandoff)
import IDE.Web.OpenFileRequest (deliverOpenedFile)
import IDE.Web.RegionGrabRequest (requestRegionGrab)
import IDE.Web.RemoteTermRequest (requestRemoteTerm)
import IDE.Web.ScreenshotRequest (requestScreenshot)
import IDE.Web.WindowBridge (resyncStates)
import IDE.Web.SnapRequest (requestSnapPane)
import IDE.Workspaces
       (projectOpenThis, projectOpenPath, dirProjectKey, setProjectSettings,
        workspaceActivatePackage, workspaceTryQuiet, makePackage')

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
startCmdServer :: IDERef -> IO ()
startCmdServer ideR = void . forkIO $ serve `catch` \(_ :: SomeException) -> return ()
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
      -- Never let spawned children (leksah-server, tmux, git …) inherit the
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
          (handleConn ideR conn `catch` \(_ :: SomeException) -> return ())
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
handleConn :: IDERef -> Socket -> IO ()
handleConn ideR conn = do
  raw <- recvAll conn
  let fields = map (decodeUtf8With lenientDecode) (BS.split 0 raw)
  case fields of
    (cwdT : argsT) -> dispatch (T.unpack cwdT) argsT
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

    dispatch cwd = \case
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
        mapM_ deliverOpenedFile [ fp | Right fp <- results ]
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
          Right fp ->
            reflectIDE (readIDE workspace) ideR >>= \case
              Nothing -> reply "No workspace open\n"
              Just ws ->
                case [ (project, p)
                     | project <- ws ^. wsProjects
                     , p <- pjPackages project
                     , ipdCabalFile p == fp ] of
                  ((project, package):_) -> do
                    void $ reflectIDE (workspaceTryQuiet
                        (workspaceActivatePackage project (Just package) Nothing)) ideR
                    reply $ "Activated " <> T.pack fp <> "\n"
                  [] -> reply $ "No package with cabal file " <> T.pack fp <> " in the workspace\n"

      -- Set (or clear, with no prefix argument) the per-project command
      -- prefix — the shell fragment remote tool runs are wrapped in, e.g.
      -- `leksah-cmd project set-prefix host:~/proj/cabal.project nix develop -c`.
      ("project" : "set-prefix" : file : prefixParts) ->
        resolveInput cwd file >>= \case
          Left e -> reply e
          Right fp -> case filePathToProjectKey fp of
            Nothing -> reply $ "Not a project file: " <> T.pack fp <> "\n"
            Just pk -> do
              let prefix = T.strip (T.unwords prefixParts)
                  settings = ProjectSettings
                    { psCmdPrefix = if T.null prefix then Nothing else Just prefix }
              void $ reflectIDE (workspaceTryQuiet (setProjectSettings pk settings)) ideR
              reply $ "Command prefix for " <> T.pack fp <> ": "
                      <> (if T.null prefix then "(cleared)" else prefix) <> "\n"

      -- Cheap liveness check for `leksah-cmd wait-ready` / `restart --wait`:
      -- answered as soon as the control socket is serving, so it marks the point
      -- the relaunched UI is back.
      ("ping" : _) -> reply "ok\n"

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

      -- resync-state: each window's resync signal/ack MVar occupancy (see
      -- 'resyncStates') — pinpoints where a frozen window's resync stalled.
      ("resync-state" : _) -> do
        sts <- resyncStates
        reply . T.unlines $
          [ T.pack (show wid <> " sig=" <> (if s then "FULL" else "empty")
                             <> " ack=" <> (if a then "FULL" else "empty"))
          | (wid, s, a) <- sts ]

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
            when noRestart $ writeIORef suppressNextRestart True
            reply $ "Rebuilding leksah via the IDE build system — output appears in "
                 <> "the IDE (Errors/Log panes)"
                 <> (if noRestart
                       then "; the app stays up (--no-restart).\n"
                       else "; on success it restarts.\n")
                 <> "(Failsafe if the IDE build is broken: rebuild-self --use-cabal)\n"
            void . forkIO . void $ reflectIDE
                (readIDE workspace >>=
                   mapM_ (runWorkspace $ runProject (State.runPackage makePackage' package) project))
                ideR

      -- Fired by tmux's after-select-window / after-select-pane hooks: poke the
      -- reflex network (reusing the JS trigger the ⌃B/mousedown listener uses) so
      -- the pane tree is re-read and the focused terminal's new active pane floats
      -- to the flipper/tab MRU front, without waiting for the 2 s poll.
      ("term-activity" : _) -> do
        let poke = "window.leksahTermActivity && window.leksahTermActivity()" :: Text
        _ <- (try (reflectIDE (ideJSM (void (eval poke))) ideR)
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
      ("log" : loggerName : levelT : _) | not (T.null loggerName) ->
        setLoggerLevel (T.unpack loggerName) (T.unpack levelT) >>= reply

      ("help" : _) -> reply usage
      []            -> reply usage
      other         -> reply $ "leksah-cmd: unknown command: "
                                  <> T.unwords other <> "\n\n" <> usage

    -- The workspace's leksah package (project, package), if it's open.
    findLeksahPackage = (`reflectIDE` ideR) $
        readIDE workspace >>= \case
            Nothing -> return Nothing
            Just ws -> return $ listToMaybe
                [ (project, p)
                | project <- ws ^. wsProjects
                , p <- pjPackages project
                , ipdPackageName p == "leksah" ]

    -- A directory becomes a plain-directory project (no build file needed);
    -- otherwise the path is a project file (cabal.project / stack.yaml / …).
    -- Route through 'projectOpenPath' — the single recognition point shared with
    -- the Open Project / Open Folder panels — so Cargo.toml / pyproject.toml /
    -- setup.py (Rust/Python) are recognised here too.
    openProject fp = do
      void $ reflectIDE (workspaceTryQuiet (projectOpenPath fp)) ideR
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
      r <- try $ reflectIDE (ideJSM (eval wrapped >>= valToText)) ideR
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
  , "  log LOGGER LEVEL        set an hslogger logger's level live, e.g."
  , "                          `log leksah.focus debug` (→ ~/.leksah/focus-debug.log), `… off`"
  ]

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
