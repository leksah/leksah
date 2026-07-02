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
--   * @cm open FILE…@ — open each file in the editor (CodeMirror) area, reusing
--     the same bridge the native "Open File" dialog feeds.
--   * @project open FILE…@ — add each project file to the workspace, like the
--     GTK @projectOpen@ / the native open-project dialog.
--   * @js eval CODE@ — evaluate CODE in leksah's JS engine(s) and reply with the
--     result (handy for poking at the live page from a shell).
--
-- Relative paths are resolved against the *client's* working directory (sent as
-- the first field), not leksah's.
module IDE.Web.CmdServer
  ( startCmdServer
  , cmdSocketPath
  , suppressNextRestart
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, putMVar)
import Control.Exception (SomeException, catch, finally, try)
import Control.Lens ((^.))
import Control.Monad (forever, void, when)

import Data.IORef (IORef, newIORef, writeIORef)
import Data.Maybe (listToMaybe)

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import System.Directory
       (getHomeDirectory, removeFile, doesFileExist,
        createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath (isRelative, (</>))
import System.IO (hSetBinaryMode)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (exitImmediately)
import System.Process
       (createProcess, proc, shell, waitForProcess, CreateProcess(std_out, std_in),
        StdStream(CreatePipe, NoStream))

import Network.Socket
       (Socket, Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix),
        socket, bind, listen, accept, close, defaultProtocol)
import Network.Socket.ByteString (recv, sendAll)

import Language.Javascript.JSaddle (eval, valToText)

import IDE.Core.State
       (IDERef, reflectIDE, ideJSM, readIDE, workspace, runWorkspace,
        runProject, pjPackages, ipdPackageName, wsProjects)
import qualified IDE.Core.State as State (runPackage)
import IDE.Core.Types (filePathToProjectKey)
import IDE.Web.OpenFileRequest (deliverOpenedFile)
import IDE.Web.RemoteTermRequest (requestRemoteTerm)
import IDE.Web.SnapRequest (requestSnapPane)
import IDE.Workspaces (projectOpenThis, workspaceTryQuiet, makePackage')

-- | @~/.leksah/cmd.sock@ — the control socket both sides agree on.
cmdSocketPath :: IO FilePath
cmdSocketPath = do
  home <- getHomeDirectory
  return $ home </> ".leksah" </> "cmd.sock"

-- | Start the control socket listener on a background thread and return.  Any
-- stale socket file from a previous run is removed first; failures to bind are
-- swallowed (a missing control socket just means @leksah-cmd@ won't work, which
-- mustn't take the IDE down).
startCmdServer :: IDERef -> IO ()
startCmdServer ideR = void . forkIO $ serve `catch` \(_ :: SomeException) -> return ()
  where
    serve = do
      path <- cmdSocketPath
      createDirectoryIfMissing True =<< (</> ".leksah") <$> getHomeDirectory
      exists <- doesFileExist path
      when exists $ removeFile path `catch` \(_ :: SomeException) -> return ()
      sock <- socket AF_UNIX Stream defaultProtocol
      bind sock (SockAddrUnix path)
      listen sock 5
      forever $ do
        (conn, _) <- accept sock
        void . forkIO $
          (handleConn ideR conn `catch` \(_ :: SomeException) -> return ())
            `finally` close conn

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
    resolve cwd p = let s = T.unpack p in if isRelative s then cwd </> s else s

    dispatch cwd = \case
      ("restart" : args) -> do
        -- @--no-rebuild@ exits 3 instead of 2; leksah-nix.sh's loop treats 3 as
        -- "relaunch but skip the cabal build" (safe after rebuild-self already
        -- built), avoiding the redundant build + its `nix develop`.  A loop that
        -- predates this only continues on 2, so plain restart stays 2.
        let noRebuild = "--no-rebuild" `elem` args
        reply $ if noRebuild
          then "Restarting leksah (exit 3 → leksah-nix.sh relaunches without rebuilding).\n"
          else "Restarting leksah (exit 2 → leksah-nix.sh rebuilds and relaunches).\n"
        -- Give the reply a moment to flush over the socket before we exit.
        threadDelay 100000
        exitImmediately (ExitFailure (if noRebuild then 3 else 2))

      ("cm" : "open" : files) | not (null files) -> do
        mapM_ (deliverOpenedFile . resolve cwd) files
        reply $ "Opened " <> T.pack (show (length files)) <> " file(s) in the editor.\n"

      ("project" : "open" : files) | not (null files) -> do
        results <- mapM (openProject . resolve cwd) files
        reply $ T.unlines results

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
                reply "rebuild-self: no leksah package in the workspace — using the \
                      \direct cabal build instead.\n"
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

    openProject fp = case filePathToProjectKey fp of
      Nothing -> return $ "Not a project file: " <> T.pack fp
      Just pk -> do
        void $ reflectIDE (workspaceTryQuiet (projectOpenThis pk)) ideR
        return $ "Added project to workspace: " <> T.pack fp

    evalJs code = do
      r <- try $ reflectIDE (ideJSM (eval code >>= valToText)) ideR
      return $ case r of
        Left (e :: SomeException) -> "JS error: " <> T.pack (show e) <> "\n"
        Right []                  -> "(no live JS context — is the page loaded?)\n"
        Right results             -> T.unlines results

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
        then reply "rebuild-self: not configured — no ~/.leksah/rebuild.sh \
                   \(launch leksah via leksah-nix.sh).\n"
        else tryTakeMVar buildLock >>= \case
          Nothing -> reply "rebuild-self: a build is already in progress.\n"
          Just () -> do
            reply "Rebuilding leksah (the app stays up; it restarts only if the \
                  \build succeeds)…\n\n"
            outcome <- try (streamBuild conn script) :: IO (Either SomeException Bool)
            case outcome of
              Right True
                | noRestart -> do
                    putMVar buildLock ()
                    reply "\nBuild succeeded — app left running (--no-restart). \
                          \Run `leksah-cmd restart` to relaunch into it.\n"
                | otherwise -> do
                    reply "\nBuild succeeded — restarting into the new build.\n"
                    threadDelay 150000  -- let the reply flush before we exit
                    exitImmediately (ExitFailure 2)
              Right False -> do
                putMVar buildLock ()
                reply "\nBuild FAILED — leksah left running. Fix the errors and \
                      \run rebuild-self again.\n"
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
  , "  cm open FILE...         open files in the editor"
  , "  project open FILE...    add project files to the workspace"
  , "  cc-connect HOST         terminal tab on HOST's tmux (ssh, control mode)"
  , "  open-browser URL        open the default browser snapped to this pane"
  , "  js eval CODE            evaluate JS in the running leksah"
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
