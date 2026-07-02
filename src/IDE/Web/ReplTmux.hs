{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- | Widget-free tmux plumbing for the shared repl session.
--
-- Lives outside "IDE.Web.Widget.Terminal" so that "IDE.Package" (which the
-- widget modules transitively import — Menu → Command → Workspaces → Build)
-- can use it without an import cycle.
module IDE.Web.ReplTmux
  ( tmuxSocket
  , tmuxCmd
  , replSessionName
  , ffcabalTmuxEnv
  , findReplWindow
  , selectTmuxWindowById
  ) where

import Control.Exception (catch, SomeException)
import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Process (readProcessWithExitCode)

-- | The private tmux socket leksah's terminals live on (so they don't mix with
-- the user's own tmux sessions, and so its options don't touch their config).
tmuxSocket :: String
tmuxSocket = "leksah"

-- | Run a tmux command on leksah's private socket, ignoring failures.
tmuxCmd :: [String] -> IO ()
tmuxCmd args = (`catch` \(_ :: SomeException) -> return ()) $
    findExecutable "tmux" >>= \case
        Nothing -> return ()
        Just tmux -> void $ readProcessWithExitCode tmux (["-L", tmuxSocket] <> args) ""

-- | The shared repl session: ffcabal's cached component repls live here
-- (windows named @pkg:comp@), and the workspace-tree run buttons add their
-- @nix repl@ / @nix develop@ windows to the same session — one "repls" tab.
replSessionName :: Text
replSessionName = "ffcabal"

-- | Environment entry pinning ffcabal's tmux server to leksah's own
-- (@-L leksah@), passed to every ffcabal leksah runs.  Without it the repl
-- windows would land on whatever server leksah's own environment implies
-- ($TMUX when launched from inside tmux, the default server otherwise) —
-- somewhere the terminal widgets and 'findReplWindow' can't reach.
ffcabalTmuxEnv :: (String, String)
ffcabalTmuxEnv = ("FFCABAL_TMUX_ARGS", "-L " <> tmuxSocket)

-- | Find a window by exact name in the shared repl session: (session id,
-- window id).  'Nothing' when the session or window doesn't exist.
findReplWindow :: Text -> IO (Maybe (Text, Text))
findReplWindow name = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            (_, out, _) <- readProcessWithExitCode tmux
                [ "-L", tmuxSocket, "list-windows", "-t", "=" <> T.unpack replSessionName
                , "-F", "#{session_id}\t#{window_id}\t#{window_name}" ] ""
            return $ listToMaybe
                [ (sid, wid) | l <- T.lines (T.pack out)
                , (sid : wid : wname) <- [T.splitOn "\t" l]
                , T.intercalate "\t" wname == name ]

-- | Select a tmux window by its unique window id (@\@N@) — repl window names
-- contain ':' (@pkg:lib:name@), so id targeting is the only unambiguous form.
selectTmuxWindowById :: Text -> IO ()
selectTmuxWindowById wid = tmuxCmd ["select-window", "-t", T.unpack wid]
