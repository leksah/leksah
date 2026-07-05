{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | @leksah-cmd@: a tiny command line client that talks to a running leksah web
-- UI over the Unix domain socket at @~/.leksah/cmd.sock@ (server side in
-- 'IDE.Web.CmdServer').
--
-- Usage:
--   leksah-cmd restart [--no-rebuild] [--wait]  relaunch via the wrapper
--   leksah-cmd rebuild-self [--no-restart] [--use-cabal]  rebuild in place
--   leksah-cmd wait-ready [SECONDS]    block until the UI answers (default 180)
--   leksah-cmd cm open FILE...         open files in the editor (CodeMirror)
--   leksah-cmd project open FILE...    add project files to the workspace
--   leksah-cmd js eval CODE            evaluate JS in the running leksah
--   leksah-cmd js eval -f FILE         evaluate JS read from FILE
--   leksah-cmd js eval -               evaluate JS read from stdin
--   leksah-cmd ping                    print "ok" if the UI is up
--   leksah-cmd help                    show this help
--
-- The wire format: the client's working directory followed by its argv, each
-- field NUL-separated; the client then half-closes its write side and prints the
-- single text reply.  (Sending the cwd lets the server resolve relative paths
-- against the shell that ran @leksah-cmd@, not against leksah's own directory.)
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch, try)
import Control.Monad (unless)

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import System.Directory (getCurrentDirectory, getHomeDirectory, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, stdout, hFlush)
import Text.Read (readMaybe)

import Network.Socket
       (Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix),
        socket, connect, close, defaultProtocol,
        ShutdownCmd(ShutdownSend), shutdown)
import Network.Socket.ByteString (recv, sendAll)

cmdSocketPath :: IO FilePath
cmdSocketPath = (</> ".leksah" </> "cmd.sock") <$> getHomeDirectory

usage :: Text
usage = T.unlines
  [ "leksah-cmd — control a running leksah web UI"
  , ""
  , "Usage:"
  , "  leksah-cmd restart [--no-rebuild] [--wait]"
  , "                                     relaunch via the wrapper (--no-rebuild skips the"
  , "                                     build; --wait blocks until the new UI answers)"
  , "  leksah-cmd rebuild-self [--no-restart] [--use-cabal]"
  , "                                     rebuild via the IDE build system (errors in the UI);"
  , "                                     --use-cabal = failsafe: direct cabal, output streamed"
  , "  leksah-cmd wait-ready [SECONDS]    block until the UI answers a ping (default 180)"
  , "  leksah-cmd cm open FILE...         open files in the editor (CodeMirror)"
  , "  leksah-cmd project open FILE...    add project files to the workspace"
  , "  leksah-cmd cc-connect HOST         terminal tab on HOST's tmux (ssh, control mode)"
  , "  leksah-cmd open-browser URL        open the default browser snapped to this pane"
  , "  leksah-cmd js eval CODE            evaluate JS in the running leksah"
  , "  leksah-cmd js eval -f FILE         evaluate JS read from FILE (no shell escaping)"
  , "  leksah-cmd js eval -               evaluate JS read from stdin"
  , "  leksah-cmd ping                    print \"ok\" if the UI is up (silent-ish, exit 0/1)"
  , "  leksah-cmd screenshot FILE         capture the UI to a PNG (wkwebview only)"
  , "  leksah-cmd grab-region [TARGET]    select a screen region; type its PNG path into"
  , "                                     a terminal pane (default: regionCaptureTarget pref;"
  , "                                     TARGET is a session/window/pane path)"
  , "  leksah-cmd help                    show this help"
  ]

main :: IO ()
main = getArgs >>= \case
  []            -> T.putStr usage
  ("help":_)    -> T.putStr usage
  ("--help":_)  -> T.putStr usage
  ("-h":_)      -> T.putStr usage
  -- open-browser also needs the tmux pane it was run in ($TMUX_PANE); pass it
  -- along so leksah can snap the browser to that pane.
  ("open-browser":rest) -> do
    pane <- maybe "" id <$> lookupEnv "TMUX_PANE"
    send ("open-browser" : pane : rest)

  -- ping: exit 0 if the UI answered, 1 otherwise (for scripting).
  ("ping":_) -> pingOnce >>= \ok ->
    if ok then putStrLn "ok" else hPutStrLn stderr "leksah: not responding" >> exitFailure

  -- wait-ready [SECONDS]: poll until the UI answers (used after a relaunch).
  ("wait-ready":rest) -> do
    let secs = maybe 180 id (readMaybe =<< listToMaybeStr rest)
    ok <- waitUp secs
    if ok then putStrLn "leksah is ready."
          else hPutStrLn stderr "leksah-cmd: timed out waiting for the UI" >> exitFailure

  -- restart [--wait]: relaunch; with --wait, block until the NEW instance is up
  -- (first wait for the old one to go down, so we don't match it before it exits).
  ("restart":rest)
    | "--wait" `elem` rest -> do
        send ("restart" : filter (/= "--wait") rest)
        putStr "Waiting for leksah to relaunch… "; hFlush stdout
        ok <- waitDownThenUp 180
        putStrLn (if ok then "ready." else "TIMED OUT.")
        unless ok exitFailure
    | otherwise -> send ("restart" : rest)

  -- js eval CODE | -f FILE | -   (the last two avoid shell-escaping the JS).
  ("js":"eval":rest) | not (null rest) -> do
    code <- case rest of
      ["-f", file] -> T.readFile file
      ["-"]        -> T.getContents
      parts        -> return (T.unwords (map T.pack parts))
    send' ["js", "eval", T.unpack code]

  args          -> send args

-- | The first element of a list, if any (as a String), for optional args.
listToMaybeStr :: [String] -> Maybe String
listToMaybeStr (x:_) = Just x
listToMaybeStr []    = Nothing

-- | @cwd@ + argv, NUL-separated — the request wire format.
payloadFor :: [String] -> IO BS.ByteString
payloadFor args = do
  cwd <- getCurrentDirectory
  return $ BS.intercalate (BS.singleton 0)
             (map (encodeUtf8 . T.pack) (cwd : args))

-- | Send a command whose argv fields are already exactly as intended (no extra
-- packing), streaming the reply to stdout.  Used for @js eval@ where the code is
-- one field (possibly multi-line).
send' :: [String] -> IO ()
send' args = do
  path <- cmdSocketPath
  exists <- doesFileExist path
  unless exists (noSocket path)
  payload <- payloadFor args
  runClient path payload `catch` \(e :: IOException) -> do
    hPutStrLn stderr $ "leksah-cmd: could not talk to leksah: " <> show e
    exitFailure

send :: [String] -> IO ()
send = send'

noSocket :: FilePath -> IO ()
noSocket path = do
  hPutStrLn stderr $ "leksah-cmd: no control socket at " <> path
                     <> "\n(is leksah running? start it with ./leksah-nix.sh ghc914 wkwebview)"
  exitFailure

runClient :: FilePath -> BS.ByteString -> IO ()
runClient path payload = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix path)
  sendAll sock payload
  shutdown sock ShutdownSend   -- signal EOF so the server starts handling
  let drain = do
        chunk <- recv sock 65536
        if BS.null chunk then return () else BS.hPut stdout chunk >> drain
  drain
  close sock

-- | Try one command and collect its reply; 'Nothing' if we couldn't connect
-- (socket missing or no listener) — used for readiness polling.
tryReply :: [String] -> IO (Maybe Text)
tryReply args = do
  path <- cmdSocketPath
  exists <- doesFileExist path
  if not exists then return Nothing else do
    r <- try (collect path) :: IO (Either IOException BS.ByteString)
    return $ either (const Nothing) (Just . decodeUtf8With lenientDecode) r
  where
    collect path = do
      payload <- payloadFor args
      sock <- socket AF_UNIX Stream defaultProtocol
      connect sock (SockAddrUnix path)
      sendAll sock payload
      shutdown sock ShutdownSend
      let go acc = do
            chunk <- recv sock 65536
            if BS.null chunk then return (BS.concat (reverse acc)) else go (chunk : acc)
      out <- go []
      close sock
      return out

-- | Is the UI up right now?  (A @ping@ that returns "ok".)
pingOnce :: IO Bool
pingOnce = maybe False (T.isInfixOf "ok") <$> tryReply ["ping"]

-- | Poll @ping@ every 0.5 s until it succeeds or @secs@ elapse.
waitUp :: Int -> IO Bool
waitUp secs = go (max 1 (secs * 2))
  where
    go 0 = pingOnce
    go n = pingOnce >>= \case
      True  -> return True
      False -> threadDelay 500000 >> go (n - 1)

-- | For @restart --wait@: first wait for the running instance to go DOWN (up to
-- ~20 s), so a ping can't match the old instance before it exits, then wait for
-- the new one to come UP.
waitDownThenUp :: Int -> IO Bool
waitDownThenUp secs = downThen (40 :: Int)
  where
    downThen 0 = waitUp secs   -- never saw it go down; just wait for up
    downThen n = pingOnce >>= \case
      False -> waitUp secs
      True  -> threadDelay 500000 >> downThen (n - 1)
