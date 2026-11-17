{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
  , qualifyTmuxId
  , tmuxOnIds
  , tmuxRunOn
  , remoteTmux
  , remoteSocketArgs
  , tmuxCmdOn
  , tmuxOkOn
  , tmuxOutOn
  , tmuxStdoutOn
  , tmuxLinesOn
  , viewerId
  , notViewerFilter
  , viewerFilterArg
  , remoteViewerAttach
  , selectWindowAsViewer
  , selectWindowAsViewerSoon
  , fitWindowScript
  , installFitScript
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson
       (FromJSON(..), ToJSON(..), Value(..), object, withObject, (.:), (.=))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum)
import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, findExecutable, getHomeDirectory)
import System.FilePath ((</>))
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import IDE.App (appConfig, getGlobalApp)
import IDE.Config (Config(..), RemoteC(..), currentConfig)
import IDE.Web.Instance (leksahPort, tmuxServerSocket)
import IDE.Utils.RemoteExec (runSsh)
import Control.Concurrent (threadDelay)

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

-- | A raw id from server @srv@, written as the model keeps it: unchanged for
-- the local server, @ssh://host#\<id\>@ for a remote one.
qualifyTmuxId :: TmuxServer -> Text -> Text
qualifyTmuxId LocalServer   x = x
qualifyTmuxId (SshServer h) x = "ssh://" <> h <> "#" <> x

-- | Act on tmux objects named by (possibly server-qualified) ids: run @k@ on
-- the ONE server they all live on, handing it @raw@ to turn each id back into
-- the form that server knows.  Ids on different servers can't meet in one
-- tmux command (a pane can't move between machines), so that answers @dflt@.
tmuxOnIds :: [Text] -> a -> (TmuxServer -> (Text -> String) -> IO a) -> IO a
tmuxOnIds ids dflt k = case ids of
  []      -> k LocalServer raw
  (i : rest)
    | all ((== serverOfTabKey i) . serverOfTabKey) rest -> k (serverOfTabKey i) raw
    | otherwise -> return dflt
  where raw = T.unpack . rawTmuxId

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
    SshServer host -> remoteTmux host >>= \case
      -- (no tmux there: the reason, as the command's error)
      Left err       -> return (Just (ExitFailure 127, "", T.unpack err))
      Right (bin, _) -> do
        sock <- remoteSocketArgs host
        Just <$> readProcessWithExitCode "ssh"
          [ "-o", "BatchMode=yes", "-o", "ConnectTimeout=5", T.unpack host
          , unwords (shellQuote bin : map shellQuote (sock <> args)) ] input

-- | The @-L NAME@ arguments for leksah's tmux server on remote @host@: the
-- host's entry in @remote.tmuxSockets@ (Add Server, or Preferences ▸
-- Remote), or none — the host's default tmux server — when it has no entry.
-- Read per call, so a changed setting applies to the next command.
remoteSocketArgs :: Text -> IO [String]
remoteSocketArgs host = getGlobalApp >>= \case
  Nothing  -> return []
  Just app -> do
    cfg <- currentConfig (appConfig app)
    return $ case T.strip <$> M.lookup host (rcTmuxSockets (cfgRemote cfg)) of
      Just name | not (T.null name) -> ["-L", T.unpack name]
      _                             -> []


--------------------------------------------------------------------------------
-- Sharing a server with other leksahs
--
-- Two leksahs can show the same tmux server: the one running on a host, and
-- another reaching that host over ssh (its @remote.tmuxSockets@ entry names the
-- host leksah's socket).  They share the WINDOWS — that is the point — but a
-- session's current window belongs to the session, so two viewers attached to
-- one session would move each other.  A remote viewer therefore attaches
-- through its own GROUPED session (@NAME~VIEWER@: the same windows, its own
-- current window), marked @\@leksah_viewer@ and destroyed by tmux once it has
-- no clients.  Every leksah leaves such sessions out of what it lists
-- ('notViewerFilter').  (A window's active PANE and its size stay shared: tmux
-- keeps them on the window.)

-- | Who this leksah is to the servers it shares ('RemoteC.rcViewerId', else
-- this machine's name, lower case — plus the port when it is not the default
-- one): letters, digits, @-@ and @_@ only, as it is part of a tmux session
-- name.  (Two leksahs on one machine under different accounts would need
-- @remote.viewerId@ to tell them apart.)
viewerId :: IO Text
viewerId = readIORef viewerIdRef >>= \case
  Just v  -> return v
  Nothing -> do
    configured <- getGlobalApp >>= \case
      Nothing  -> return ""
      Just app -> T.strip . rcViewerId . cfgRemote <$> currentConfig (appConfig app)
    v <- if not (T.null configured) then return configured else do
      user <- maybe "" T.pack <$> lookupEnv "USER"
      host <- (`catch` \(_ :: SomeException) -> return "") $
        (\(_, o, _) -> T.takeWhile (/= '.') (T.strip (T.pack o)))
          <$> readProcessWithExitCode "uname" ["-n"] ""
      return $ T.toLower (if T.null host then user else host)
                 <> (if leksahPort == 3367 then "" else "-" <> T.pack (show leksahPort))
    let clean = T.map (\c -> if isAlphaNum c || c == '-' || c == '_' then c else '-') v
        v' = if T.null clean then "leksah" else clean
    atomicModifyIORef' viewerIdRef (const (Just v', ()))
    return v'

{-# NOINLINE viewerIdRef #-}
viewerIdRef :: IORef (Maybe Text)
viewerIdRef = unsafePerformIO (newIORef Nothing)

-- | A @-f@ filter for @list-sessions@ \/ @list-windows@ \/ @list-panes@ that
-- leaves out other leksahs' viewer sessions (and this one's): in a session
-- group every window is listed once PER SESSION, so without it a viewer
-- session reads as a second session holding the same windows.
notViewerFilter :: [String]
notViewerFilter = ["-f", viewerFilterArg]

-- | The format 'notViewerFilter' filters on.
viewerFilterArg :: String
viewerFilterArg = "#{==:#{@leksah_viewer},}"

-- | The remote command (one string, quoted for the remote shell) that attaches
-- a control-mode client to session @target@ on @host@ — or, with no target,
-- to its @leksah@ session, created if need be — THROUGH this viewer's grouped
-- session for it ('viewerId'), creating that first.  'Left' when there is no
-- tmux on the host.
remoteViewerAttach :: Text -> Maybe Text -> IO (Either Text String)
remoteViewerAttach host target = remoteTmux host >>= \case
  Left err -> return (Left err)
  Right (bin, _) -> do
    sock   <- remoteSocketArgs host
    viewer <- viewerId
    let sockName = case sock of ["-L", n] -> n; _ -> ""
    return . Right . unwords . map shellQuote $
      [ "sh", "-c", viewerAttachScript, "sh", bin, sockName
      , maybe "" T.unpack target, T.unpack viewer
      , reverse (dropWhile (== '\n') (reverse fitWindowScript)) ]

-- | @$1@ tmux, @$2@ socket name (or empty), @$3@ target session (empty: the
-- @leksah@ session), @$4@ the viewer.  The group is named after its leader,
-- so attaching through someone else's viewer session still lands in the
-- shared one's group.  (tmux 3.x: a pane-target command — set-option,
-- display-message — needs @=NAME:@, with the colon, for an exact name.)
viewerAttachScript :: String
viewerAttachScript = concat
  [ "T=\"$1\"; S=\"$2\"; G=\"$3\"; V=\"$4\"; "
  -- (the window-sizing script, for this host's tmux to run: 'fitWindowScript')
  , "mkdir -p \"$HOME/.leksah\" && { [ \"$(cat \"$HOME/.leksah/fit-window.sh\" 2>/dev/null)\" = \"$5\" ] "
  , "|| printf '%s\\n' \"$5\" > \"$HOME/.leksah/fit-window.sh\"; "
  , "printf '%s' \"$T\" > \"$HOME/.leksah/fit-window.tmux\"; }; "
  , "tx() { if [ -n \"$S\" ]; then \"$T\" -L \"$S\" \"$@\"; else \"$T\" \"$@\"; fi; }; "
  , "if [ -z \"$G\" ]; then tx has-session -t =leksah 2>/dev/null || tx new-session -d -s leksah || exit 1; G==leksah:; fi; "
  , "n=$(tx display-message -p -t \"$G\" '#{session_group}' 2>/dev/null); "
  , "[ -n \"$n\" ] || n=$(tx display-message -p -t \"$G\" '#{session_name}' 2>/dev/null); "
  , "[ -n \"$n\" ] || { echo \"no tmux session $G\" >&2; exit 1; }; "
  , "v=\"$n~$V\"; "
  , "if ! tx has-session -t \"=$v\" 2>/dev/null; then "
  ,   "tx new-session -d -t \"=$n\" -s \"$v\" || exit 1; "
  ,   "tx set-option -t \"=$v:\" @leksah_viewer \"$V\"; "
  -- Gone with its last client — but set only once attached: tmux destroys
  -- a never-attached session the moment the option is set.  (@keep-group@:
  -- never the group's last session, whose windows would go with it.)
  ,   "tx set-hook -t \"=$v:\" client-attached 'set-option destroy-unattached keep-group'; "
  , "fi; "
  , "if [ -n \"$S\" ]; then exec \"$T\" -L \"$S\" -C attach-session -t \"=$v\"; "
  , "else exec \"$T\" -C attach-session -t \"=$v\"; fi" ]

-- | The script that sizes a window several leksahs show to the smallest size
-- any of them wants: each records its wanted size on the window as
-- @\@leksah_size_VIEWER = "WxH SESSION"@ ('IDE.Web.TmuxCC.ccResizeWindow'),
-- and an entry whose session has no client any more is dropped.  So no
-- leksah's view of a shared window is ever cut off; the larger one has unused
-- space.  Kept at @~\/.leksah\/fit-window.sh@ on the machine the tmux server
-- runs on ('installFitScript' here, 'remoteViewerAttach' there), run there by
-- tmux's @run-shell@ — whose @tmux@ is that same server.  No argument: every
-- window (after a viewer leaves).
fitWindowScript :: String
fitWindowScript = unlines
  [ "# leksah: size each shared tmux window to the smallest size wanted by the"
  , "# leksahs showing it (@leksah_size_VIEWER = \"WxH SESSION\", dropped once that"
  , "# session has no client).  $1: a window id; none: every window."
  , "# (The tmux to run: the one leksah found on this machine, which may be a"
  , "# nix-built one off PATH - see fit-window.tmux.)"
  , "T=$(cat \"$HOME/.leksah/fit-window.tmux\" 2>/dev/null); [ -n \"$T\" ] || T=tmux"
  , "tmux() { command \"$T\" \"$@\"; }"
  , "fit() {"
  , "  w=$1; mw=; mh="
  , "  opts=$(tmux show-options -w -t \"$w\" 2>/dev/null | grep '^@leksah_size_')"
  , "  [ -n \"$opts\" ] || return 0"
  , "  while read -r k v; do"
  , "    v=${v#\\\"}; v=${v%\\\"}; sz=${v%% *}; s=${v#* }"
  , "    a=$(tmux display-message -p -t \"=$s:\" '#{session_attached}' 2>/dev/null)"
  , "    if [ \"${a:-0}\" -gt 0 ]; then"
  , "      W=${sz%x*}; H=${sz#*x}"
  , "      if [ -z \"$mw\" ] || [ \"$W\" -lt \"$mw\" ]; then mw=$W; fi"
  , "      if [ -z \"$mh\" ] || [ \"$H\" -lt \"$mh\" ]; then mh=$H; fi"
  , "    else"
  , "      tmux set-option -w -t \"$w\" -u \"$k\""
  , "    fi"
  , "  done <<OPTS"
  , "$opts"
  , "OPTS"
  , "  [ -n \"$mw\" ] && tmux resize-window -t \"$w\" -x \"$mw\" -y \"$mh\""
  , "}"
  , "if [ -n \"$1\" ]; then fit \"$1\"; else"
  , "  for w in $(tmux list-windows -a -F '#{window_id}' | sort -u); do fit \"$w\"; done"
  , "fi" ]

-- | Write 'fitWindowScript' to this machine's @~\/.leksah@ (when it differs).
installFitScript :: IO ()
installFitScript = (`catch` \(_ :: SomeException) -> return ()) $ do
  home <- getHomeDirectory
  let dir = home </> ".leksah"
      f   = dir </> "fit-window.sh"
  createDirectoryIfMissing True dir
  old <- (`catch` \(_ :: SomeException) -> return "") (readFile' f)
  when (old /= fitWindowScript) $ writeFile f fitWindowScript
  -- (which tmux the script runs: this one, wherever it is)
  findExecutable "tmux" >>= mapM_ (writeFile (dir </> "fit-window.tmux"))
  where readFile' f = readFile f >>= \c -> length c `seq` return c

-- | Make the window @target@ names (a window or pane id, or @session:index@)
-- the current window of THIS viewer's session(s) on @srv@ — not the shared
-- session's, which is another leksah's view.  Locally, plain @select-window@.
-- 'True' when some session took it (remotely: this viewer has a session there
-- holding the window — none yet while its tab is still attaching).
selectWindowAsViewer :: TmuxServer -> String -> IO Bool
selectWindowAsViewer LocalServer t =
  maybe False (\(c, _, _) -> c == ExitSuccess)
    <$> tmuxRunOn LocalServer ["select-window", "-t", t] ""
selectWindowAsViewer (SshServer host) t = remoteTmux host >>= \case
  Left _ -> return False
  Right (bin, _) -> (`catch` \(_ :: SomeException) -> return False) $ do
    sock   <- remoteSocketArgs host
    viewer <- viewerId
    let sockName = case sock of ["-L", n] -> n; _ -> ""
        script = T.pack $ concat
          [ "T=\"$0\"; S=\"$1\"; V=\"$2\"; t=\"$3\"; "
          , "tx() { if [ -n \"$S\" ]; then \"$T\" -L \"$S\" \"$@\"; else \"$T\" \"$@\"; fi; }; "
          , "w=$(tx display-message -p -t \"$t\" '#{window_id}') || exit 1; n=0; "
          , "for v in $(tx list-sessions -F '#{session_name}' -f \"#{==:#{@leksah_viewer},$V}\"); do "
          , "tx select-window -t \"=$v:$w\" 2>/dev/null && n=1; done; "
          , "[ \"$n\" = 1 ]" ]
    (code, _, _) <- runSsh host script
      [T.pack bin, T.pack sockName, viewer, T.pack t] mempty
    return (code == ExitSuccess)

-- | 'selectWindowAsViewer', retried for a while until this viewer has a
-- session there to take it: a tab just asked for (a launch, a show) attaches
-- over ssh, which can take a while (a handshake, waking the host, building
-- tmux there).  Run it off the caller's thread.
selectWindowAsViewerSoon :: TmuxServer -> String -> IO ()
selectWindowAsViewerSoon srv t = go (30 :: Int)
  where
    go 0 = return ()
    go n = selectWindowAsViewer srv t >>= \ok ->
      if ok then return () else threadDelay 500000 >> go (n - 1)

-- | Single-quote one word for a remote (ssh) shell.
shellQuote :: String -> String
shellQuote s = "'" <> concatMap esc s <> "'"
  where esc '\'' = "'\\''"
        esc c    = [c]

-- | Does a target-ish flag (@-t@, @-s@) carry a server-qualified id?
foreignTarget :: [String] -> Bool
foreignTarget (f : v : rest)
  | f `elem` ["-t", "-s"], "ssh://" `isPrefixOf` v = True
  | otherwise = foreignTarget (v : rest)
foreignTarget _ = False

-- | The tmux to run on remote @host@ (cached per host once found): its own
-- @tmux@ if it has one; otherwise — when it has nix, as NixOS does — nixpkgs'
-- tmux, built once into @~\/.cache\/leksah-tmux@ on that host (an out-link,
-- so the garbage collector keeps it); otherwise a 'Left' saying tmux is not
-- installed there.  The 'Bool' is True for the nix fallback.
remoteTmux :: Text -> IO (Either Text (String, Bool))
remoteTmux host = M.lookup host <$> readIORef remoteTmuxRef >>= \case
  Just bin -> return (Right (bin, bin /= "tmux"))
  -- Serialised per host: every tmux command for it waits on this, and the
  -- first nix build of tmux can take a minute or more — concurrent callers
  -- must not each start one.  (Re-checks the cache once it has the lock.)
  Nothing -> hostLock host >>= \lock -> withMVar lock $ \_ ->
   M.lookup host <$> readIORef remoteTmuxRef >>= \case
   Just bin -> return (Right (bin, bin /= "tmux"))
   Nothing  -> (`catch` \(e :: SomeException) -> return (Left (T.pack (show e)))) $ do
    (c, out, err) <- readProcessWithExitCode "ssh"
      [ "-o", "BatchMode=yes", "-o", "ConnectTimeout=10", T.unpack host
      , "sh -c " <> shellQuote script ] ""
    let bin = lastLine out
    case c of
      ExitSuccess | not (null bin) -> do
        atomicModifyIORef' remoteTmuxRef (\m -> (M.insert host bin m, ()))
        return (Right (bin, bin /= "tmux"))
      ExitFailure 3 -> return . Left $
        "tmux is not installed on " <> host
        <> " (and it has no nix to run it with) — install tmux there."
      ExitFailure 255 -> return . Left $ "Cannot reach " <> host <> firstErr err
      _ -> return . Left $
        "tmux is not installed on " <> host
        <> ", and building it with nix failed" <> firstErr err
  where
    lastLine = reverse . takeWhile (/= '\n') . dropWhile (== '\n') . reverse
    firstErr e = case filter (not . null) (lines e) of
      (l : _) -> ": " <> T.pack l
      []      -> "."
    script = unwords
      [ "if command -v tmux >/dev/null 2>&1; then echo tmux;"
      , "elif [ -x \"$HOME/.cache/leksah-tmux/bin/tmux\" ]; then echo \"$HOME/.cache/leksah-tmux/bin/tmux\";"
      , "elif command -v nix >/dev/null 2>&1; then mkdir -p \"$HOME/.cache\" &&"
      , "nix --extra-experimental-features 'nix-command flakes' build nixpkgs#tmux"
      , "--out-link \"$HOME/.cache/leksah-tmux\" >&2 && echo \"$HOME/.cache/leksah-tmux/bin/tmux\";"
      , "else exit 3; fi" ]

hostLock :: Text -> IO (MVar ())
hostLock host = do
  fresh <- newMVar ()
  atomicModifyIORef' remoteTmuxLocks $ \m -> case M.lookup host m of
    Just l  -> (m, l)
    Nothing -> (M.insert host fresh m, fresh)

{-# NOINLINE remoteTmuxLocks #-}
remoteTmuxLocks :: IORef (M.Map Text (MVar ()))
remoteTmuxLocks = unsafePerformIO (newIORef M.empty)

{-# NOINLINE remoteTmuxRef #-}
remoteTmuxRef :: IORef (M.Map Text String)
remoteTmuxRef = unsafePerformIO (newIORef M.empty)

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
