{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | @leksah-cmd@: a tiny command line client that talks to a running leksah web
-- UI over the Unix domain socket at @~/.leksah/cmd.sock@ (server side in
-- 'IDE.Web.CmdServer').
--
-- Usage:
--   leksah-cmd restart [--no-rebuild]  relaunch via the wrapper (--no-rebuild skips the build)
--   leksah-cmd rebuild-self [--no-restart] [--use-cabal]  rebuild via the IDE build (errors in the UI); --use-cabal = failsafe direct cabal (streamed)
--   leksah-cmd cm open FILE...         open files in the editor (CodeMirror)
--   leksah-cmd project open FILE...    add project files to the workspace
--   leksah-cmd js eval CODE            evaluate JS in the running leksah
--   leksah-cmd help                    show this help
--
-- The wire format: the client's working directory followed by its argv, each
-- field NUL-separated; the client then half-closes its write side and prints the
-- single text reply.  (Sending the cwd lets the server resolve relative paths
-- against the shell that ran @leksah-cmd@, not against leksah's own directory.)
module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (unless)

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (encodeUtf8)

import System.Directory (getCurrentDirectory, getHomeDirectory, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, stdout)

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
  , "  leksah-cmd restart [--no-rebuild]  relaunch via the wrapper (--no-rebuild skips the build)"
  , "  leksah-cmd rebuild-self [--no-restart] [--use-cabal]"
  , "                                     rebuild via the IDE build system (errors in the UI);"
  , "                                     --use-cabal = failsafe: direct cabal, output streamed"
  , "  leksah-cmd cm open FILE...         open files in the editor (CodeMirror)"
  , "  leksah-cmd project open FILE...    add project files to the workspace"
  , "  leksah-cmd cc-connect HOST         terminal tab on HOST's tmux (ssh, control mode)"
  , "  leksah-cmd open-browser URL        open the default browser snapped to this pane"
  , "  leksah-cmd js eval CODE            evaluate JS in the running leksah"
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
  args          -> send args

send :: [String] -> IO ()
send args = do
  path <- cmdSocketPath
  exists <- doesFileExist path
  unless exists $ do
    hPutStrLn stderr $ "leksah-cmd: no control socket at " <> path
                       <> "\n(is leksah running? start it with ./leksah-nix.sh ghc914 wkwebview)"
    exitFailure
  cwd <- getCurrentDirectory
  -- cwd first, then argv; NUL-separated.
  let payload = BS.intercalate (BS.singleton 0)
                  (map (encodeUtf8 . T.pack) (cwd : args))
  runClient path payload `catch` \(e :: IOException) -> do
    hPutStrLn stderr $ "leksah-cmd: could not talk to leksah: " <> show e
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
