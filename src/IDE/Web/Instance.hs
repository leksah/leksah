-- | Per-instance identity, so more than one leksah can run at once.
--
-- Three resources are otherwise single, fixed, and HOST-global — the
-- jsaddle\/warp UI port, the @leksah-cmd@ control socket, and leksah's private
-- tmux server — so two instances would collide on all three (a second UI
-- fails to bind the port, the newer control server steals the socket from the
-- older, and both drive the same tmux sessions).  They are keyed off one
-- anchor: the UI port, overridable with the @LEKSAH_PORT@ environment
-- variable.  A second instance launched on a different port thus gets its own
-- port, its own control socket, and its own tmux server.
--
-- The env var is read once (it can't change over the life of a process), so
-- these are exposed as pure constants.
module IDE.Web.Instance
  ( leksahPort
  , defaultLeksahPort
  , cmdSocketFileName
  , tmuxServerSocket
  ) where

import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- | The UI port used when @LEKSAH_PORT@ is unset — the historical default,
-- kept so the normal single-instance dev loop (and everything that assumes
-- @127.0.0.1:3367@, @~\/.leksah\/cmd.sock@, tmux @leksah@) is unchanged.
defaultLeksahPort :: Int
defaultLeksahPort = 3367

-- | The jsaddle\/warp port this instance serves its UI on: @LEKSAH_PORT@ when
-- it parses to a positive int, else 'defaultLeksahPort'.  The anchor the other
-- per-instance names derive from.
leksahPort :: Int
leksahPort = case unsafePerformIO (lookupEnv "LEKSAH_PORT") >>= readMaybe of
    Just p | p > 0 -> p
    _              -> defaultLeksahPort
{-# NOINLINE leksahPort #-}

-- | A per-instance name suffix: empty for the default port (so the primary
-- instance keeps the historical unsuffixed names the dev tooling and docs
-- assume), otherwise @-\<port\>@.
instanceTag :: String
instanceTag
  | leksahPort == defaultLeksahPort = ""
  | otherwise                       = '-' : show leksahPort

-- | The control-socket filename under @~\/.leksah@ (see "IDE.Web.CmdServer"):
-- @cmd.sock@ for the default instance, @cmd-\<port\>.sock@ otherwise, so two
-- instances' control servers never share — hence never steal — a socket.  Kept
-- in sync with the standalone @leksah-cmd@ client (@main\/Cmd.hs@), which
-- can't depend on this library and so replicates the rule.
cmdSocketFileName :: FilePath
cmdSocketFileName = "cmd" <> instanceTag <> ".sock"

-- | The @tmux -L@ server-socket name leksah's terminals live on: @leksah@ for
-- the default instance, @leksah-\<port\>@ otherwise.  Using a distinct server
-- (not just distinct session names) puts a second instance's terminals on a
-- wholly separate tmux server, so neither can see or resize the other's panes.
tmuxServerSocket :: String
tmuxServerSocket = "leksah" <> instanceTag
