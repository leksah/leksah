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
--   * @rebuild-self@ — rebuild leksah in place (the app stays up so the build
--     doesn't run while the window is gone), streaming the build output back to
--     the client; only on success exit(2) for a quick relaunch into the new
--     binary.  Long-running, which the one-shot streamed reply handles fine: the
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
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, putMVar)
import Control.Exception (SomeException, catch, finally, try)
import Control.Monad (forever, void, when)

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
       (createProcess, shell, waitForProcess, CreateProcess(std_out, std_in),
        StdStream(CreatePipe, NoStream))

import Network.Socket
       (Socket, Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix),
        socket, bind, listen, accept, close, defaultProtocol)
import Network.Socket.ByteString (recv, sendAll)

import Language.Javascript.JSaddle (eval, valToText)

import IDE.Core.State (IDERef, reflectIDE, ideJSM)
import IDE.Core.Types (filePathToProjectKey)
import IDE.Web.OpenFileRequest (deliverOpenedFile)
import IDE.Workspaces (projectOpenThis, workspaceTryQuiet)

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
      ["restart"] -> do
        reply "Restarting leksah (exit 2 → leksah-nix.sh rebuilds and relaunches).\n"
        -- Give the reply a moment to flush over the socket before we exit.
        threadDelay 100000
        exitImmediately (ExitFailure 2)

      ("cm" : "open" : files) | not (null files) -> do
        mapM_ (deliverOpenedFile . resolve cwd) files
        reply $ "Opened " <> T.pack (show (length files)) <> " file(s) in the editor.\n"

      ("project" : "open" : files) | not (null files) -> do
        results <- mapM (openProject . resolve cwd) files
        reply $ T.unlines results

      ("js" : "eval" : codeParts) | not (null codeParts) -> do
        let code = T.intercalate " " codeParts
        evalJs code >>= reply

      ("rebuild-self" : _) -> rebuildSelf

      ("help" : _) -> reply usage
      []            -> reply usage
      other         -> reply $ "leksah-cmd: unknown command: "
                                  <> T.unwords other <> "\n\n" <> usage

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
    -- client.  Only on success do we exit(2) so the wrapper relaunches the
    -- freshly-built binary — a quick restart, since the build is already done.
    rebuildSelf = do
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
              Right True -> do
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
  , "  restart                 exit (code 2) so the wrapper rebuilds + relaunches"
  , "  rebuild-self            rebuild in place; restart only if the build succeeds"
  , "  cm open FILE...         open files in the editor"
  , "  project open FILE...    add project files to the workspace"
  , "  js eval CODE            evaluate JS in the running leksah"
  ]

-- | Held while a 'rebuild-self' build runs, so two clients can't build at once.
{-# NOINLINE buildLock #-}
buildLock :: MVar ()
buildLock = unsafePerformIO (newMVar ())

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
  let pump = do
        chunk <- BS.hGetSome hout 4096
        if BS.null chunk then return () else sendAll conn chunk >> pump
  pump
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
