{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | WHICH tmux server a command runs on, and the one place that runs it.
--
-- Leksah's terminals live on its private local server (@tmux -L leksah@, see
-- 'IDE.Web.Instance.tmuxServerSocket'); a remote host's terminals live on that
-- host's own (default-socket) tmux, reached over ssh.  Everything that runs a
-- one-shot tmux command goes through 'tmuxRunOn' with the server it means, so
-- a pane, a session or a leksah window can name its server and the commands
-- that act on it follow — rather than every call site hard-wiring
-- @["-L", tmuxSocket]@ and remote work needing a parallel code path.
--
-- Not here: STARTING the local server (the @-f conf@ calls that apply
-- leksah's tmux config), the PTY attach of a classic terminal, and the
-- control-mode connections ("IDE.Web.TmuxCC") — those are long-lived
-- processes, not commands.
--
-- Process-level only and low in the import graph (nothing from reflex, and
-- only "IDE.Web.Instance" from leksah).
module IDE.Web.TmuxServer
  ( TmuxServer(..)
  , serverOfTabKey
  , serverLabel
  , rawTmuxId
  , tmuxRunOn
  , tmuxCmdOn
  , tmuxOkOn
  , tmuxOutOn
  , tmuxStdoutOn
  , tmuxLinesOn
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson
       (FromJSON(..), ToJSON(..), Value(..), object, withObject, (.:), (.=))
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import IDE.Web.Instance (tmuxServerSocket)

-- | A tmux server leksah can run commands on.
data TmuxServer
  = LocalServer        -- ^ leksah's private server on this machine
  | SshServer Text     -- ^ the default tmux server of an ssh destination
                       --   (an ssh-config alias or @user\@host@)
  deriving (Eq, Ord, Show)

-- | @"local"@, or @{"ssh": host}@.
instance ToJSON TmuxServer where
  toJSON LocalServer     = String "local"
  toJSON (SshServer h)   = object [ "ssh" .= h ]

instance FromJSON TmuxServer where
  parseJSON (String "local") = return LocalServer
  parseJSON v = withObject "TmuxServer" (\o -> SshServer <$> o .: "ssh") v

-- | The server behind a terminal tab key: @ssh://host[#target]@ is that host's,
-- anything else (a local @$id@ / session name) is leksah's own.
serverOfTabKey :: Text -> TmuxServer
serverOfTabKey k = case T.stripPrefix "ssh://" k of
  Just rest -> SshServer (T.takeWhile (/= '#') rest)
  Nothing   -> LocalServer

-- | A tmux id as its own server knows it: a server-qualified remote id
-- (@ssh://host#\@3@) loses its @ssh://host#@ prefix; a local id is unchanged.
rawTmuxId :: Text -> Text
rawTmuxId x
  | "ssh://" `T.isPrefixOf` x, (_, rest) <- T.breakOn "#" x, not (T.null rest) = T.drop 1 rest
  | otherwise = x

-- | How the Terminals tree names it.
serverLabel :: TmuxServer -> Text
serverLabel LocalServer   = "Local"
serverLabel (SshServer h) = h

-- | Run one tmux command on @srv@ with @input@ on stdin: the exit code, stdout
-- and stderr, or 'Nothing' when tmux (or ssh) could not be run at all.
--
-- Remotely the arguments are single-quoted FOR THE REMOTE SHELL: ssh joins the
-- remote command's words with spaces and hands them to the login shell, so an
-- unquoted tmux format like @#{session_id}@ would start a shell comment, and a
-- session id like @$0@ would be expanded.  No PTY, key auth only (BatchMode),
-- so a host that wants a password fails instead of hanging.
tmuxRunOn :: TmuxServer -> [String] -> String -> IO (Maybe (ExitCode, String, String))
tmuxRunOn srv args input = (`catch` \(_ :: SomeException) -> return Nothing) $
  case srv of
    -- A server-qualified id (@ssh://host#\@3@) names an object on ANOTHER
    -- server; handed to the local one as a target it would at best fail and at
    -- worst resolve to some local object.  Refuse it outright.
    LocalServer | foreignTarget args -> return Nothing
    LocalServer -> findExecutable "tmux" >>= maybe (return Nothing) (\tmux ->
        Just <$> readProcessWithExitCode tmux
                   (["-L", tmuxServerSocket] <> args) input)
    SshServer host -> Just <$> readProcessWithExitCode "ssh"
        [ "-o", "BatchMode=yes", "-o", "ConnectTimeout=5", T.unpack host
        , unwords ("tmux" : map shellQuote args) ] input
  where
    shellQuote s = "'" <> concatMap esc s <> "'"
    esc '\'' = "'\\''"
    esc c    = [c]

-- | Does a target-ish flag (@-t@, @-s@) carry a server-qualified id?
foreignTarget :: [String] -> Bool
foreignTarget (f : v : rest)
  | f `elem` ["-t", "-s"], "ssh://" `isPrefixOf` v = True
  | otherwise = foreignTarget (v : rest)
foreignTarget _ = False

-- | Run a command for its effect, ignoring the outcome.
tmuxCmdOn :: TmuxServer -> [String] -> IO ()
tmuxCmdOn srv args = () <$ tmuxRunOn srv args ""

-- | Run a command; did it succeed?
tmuxOkOn :: TmuxServer -> [String] -> String -> IO Bool
tmuxOkOn srv args input = maybe False (\(ec, _, _) -> ec == ExitSuccess)
                            <$> tmuxRunOn srv args input

-- | The first non-empty (stripped) line of a command's output, whatever its
-- exit code; 'Nothing' when there is none or tmux could not be run.
tmuxOutOn :: TmuxServer -> [String] -> IO (Maybe Text)
tmuxOutOn srv args = (>>= listToMaybe . nonEmptyLines) <$> stdoutOf srv args

-- | All output lines of a SUCCESSFUL command, unstripped (tab-separated
-- formats keep their empty trailing fields); 'Nothing' on a non-zero exit or
-- when tmux could not be run.
tmuxLinesOn :: TmuxServer -> [String] -> IO (Maybe [Text])
tmuxLinesOn srv args = tmuxRunOn srv args "" >>= \r -> return $ case r of
  Just (ExitSuccess, out, _) -> Just (T.lines (T.pack out))
  _                          -> Nothing

-- | A command's whole stdout, whatever its exit code; empty when tmux could
-- not be run.  (What most callers parse: tmux prints nothing useful on
-- failure, so an empty result already means "none".)
tmuxStdoutOn :: TmuxServer -> [String] -> IO Text
tmuxStdoutOn srv args = maybe "" id <$> stdoutOf srv args

stdoutOf :: TmuxServer -> [String] -> IO (Maybe Text)
stdoutOf srv args = fmap (\(_, out, _) -> T.pack out) <$> tmuxRunOn srv args ""

nonEmptyLines :: Text -> [Text]
nonEmptyLines = filter (not . T.null) . map T.strip . T.lines
