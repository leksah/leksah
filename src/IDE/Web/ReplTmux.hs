{-# LANGUAGE CPP #-}
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
  , activePaneIdOfSession
  , replSessionName
  , ffcabalTmuxEnv
  , findReplWindow
  , findRunPane
  , liveRunKeys
  , liveRunPanes
  , selectTmuxWindowById
  , ensureCommandWindow
  , ensureRemoteWindow
  , openTerminalInDir
  , runInTerminal
  , cmdPrefixForDir
  , buildSplitWindowCommand
  , getLoginShell
  , interactiveShellArgs
  , tmuxSupported
  , writeTmuxConf
  , clipboardCopyCmd
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (catch, SomeException)
import Control.Lens ((^.))
import Control.Monad (void, mfilter)
import Control.Monad.IO.Class (MonadIO(..))
import Data.List (find, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath
       ((</>), takeFileName, dropTrailingPathSeparator, addTrailingPathSeparator)
import System.Info (os)
#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
import System.Posix.User (getRealUserID, getUserEntryForID, userShell)
#endif
import System.Log.Logger (debugM)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

import IDE.Core.State
       (reflectIDE, readIDE, workspace, wsProjects, wsSettingsFor,
        ProjectSettings(..), pjKey, pjDir)
import IDE.Utils.RemoteExec (runSsh)
import IDE.Utils.RemotePath (parseRemotePath)
import IDE.Web.IDERefStore (getGlobalIDERef)
import IDE.Web.Instance (tmuxServerSocket)
import IDE.Web.RemoteTermRequest (requestRemoteTerm, requestLocalTerm)

-- | The private tmux server socket leksah's terminals live on (so they don't
-- mix with the user's own tmux sessions, and so its options don't touch their
-- config).  Per-instance (see 'IDE.Web.Instance.tmuxServerSocket'): @leksah@
-- for the default instance, @leksah-\<port\>@ under a non-default
-- @LEKSAH_PORT@, so a second instance's terminals get a wholly separate server.
tmuxSocket :: String
tmuxSocket = tmuxServerSocket

-- | Run a tmux command on leksah's private socket, ignoring failures.
tmuxCmd :: [String] -> IO ()
tmuxCmd args = (`catch` \(_ :: SomeException) -> return ()) $
    findExecutable "tmux" >>= \case
        Nothing -> return ()
        Just tmux -> void $ readProcessWithExitCode tmux (["-L", tmuxSocket] <> args) ""

-- | The active pane of @sess@'s current window (the one shown in that session's
-- terminal tab), or 'Nothing'.  'display-message -t \<session\>' proved
-- unreliable here (empty for detached / CC-client sessions), so scan
-- 'list-panes' and pick the active one.  Used by the split-open pipeline to
-- find the pane to split against.
activePaneIdOfSession :: Text -> IO (Maybe Text)
activePaneIdOfSession sess = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            (_, out, _) <- readProcessWithExitCode tmux
                [ "-L", tmuxSocket, "list-panes", "-t", T.unpack sess
                , "-F", "#{pane_active}\t#{pane_id}" ] ""
            return $ listToMaybe
                [ pid | l <- T.lines (T.pack out)
                      , let (act, rest) = T.breakOn "\t" l
                      , act == "1"
                      , let pid = T.drop 1 rest
                      , not (T.null pid) ]

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

-- | Every @\@leksah_run@ key currently live on leksah's private tmux server
-- (empty on any failure / no tmux).  Used to detect which command panes —
-- e.g. a running @claude@ session — are currently open.  Scans *panes*: the
-- key is a pane option (so it follows the pane through the Terminals-tree
-- drag-and-drop / break-pane), and pane→window option inheritance keeps
-- legacy window-tagged windows visible too.  May contain duplicates (all
-- panes of a legacy-tagged window report its key); callers do membership
-- tests, so that's harmless.
liveRunKeys :: IO [Text]
liveRunKeys = (`catch` \(_ :: SomeException) -> return []) $
    findExecutable "tmux" >>= \case
        Nothing   -> return []
        Just tmux -> do
            (_, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-panes", "-a", "-F", "#{@leksah_run}"] ""
            return $ filter (not . T.null) (T.lines (T.pack out))

-- | The first pane whose @\@leksah_run@ resolves to @key@, as
-- @(session id, window id, pane id)@ — a pane option set by
-- 'ensureCommandWindow' \/ 'IDE.Web.Widget.Terminal.openFileInEditor' (which
-- travels with the pane when it's dragged to another tmux window\/session),
-- or a legacy *window* option reaching the pane through tmux's option
-- inheritance.  'Nothing' when no pane matches / tmux is unavailable.
findRunPane :: Text -> IO (Maybe (Text, Text, Text))
findRunPane key = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing   -> return Nothing
        Just tmux -> do
            (_, out, _) <- readProcessWithExitCode tmux
                [ "-L", tmuxSocket, "list-panes", "-a", "-F"
                , "#{session_id}\t#{window_id}\t#{pane_id}\t#{@leksah_run}" ] ""
            return $ listToMaybe
                [ (sid, wid, pid) | l <- T.lines (T.pack out)
                , (sid : wid : pid : k) <- [T.splitOn "\t" l]
                , T.intercalate "\t" k == key ]

-- | Every live pane carrying a @\@leksah_run@ key, most-recently-active first
-- (tmux @window_activity@, so \"recently used\" reflects actual output\/use):
-- @(key, session id, window id, pane id)@.  Empty on any failure / no tmux.
liveRunPanes :: IO [(Text, Text, Text, Text)]
liveRunPanes = (`catch` \(_ :: SomeException) -> return []) $
    findExecutable "tmux" >>= \case
        Nothing   -> return []
        Just tmux -> do
            (_, out, _) <- readProcessWithExitCode tmux
                [ "-L", tmuxSocket, "list-panes", "-a", "-F"
                , "#{window_activity}\t#{session_id}\t#{window_id}\t#{pane_id}\t#{@leksah_run}" ] ""
            return $ map snd $ sortOn (Down . fst)
                [ (act, (key, sid, wid, pid))
                | l <- T.lines (T.pack out)
                , (actT : sid : wid : pid : k) <- [T.splitOn "\t" l]
                , let key = T.intercalate "\t" k
                , not (T.null key)
                , Just act <- [readMaybe (T.unpack actT) :: Maybe Integer] ]

-- | Select a tmux window by its unique window id (@\@N@) — repl window names
-- contain ':' (@pkg:lib:name@), so id targeting is the only unambiguous form.
selectTmuxWindowById :: Text -> IO ()
selectTmuxWindowById wid = tmuxCmd ["select-window", "-t", T.unpack wid]

-- | The login shell to run in terminals.  Prefer the password-database login
-- shell (what terminal emulators use).  $SHELL is unreliable here: launched from
-- a `nix develop` shell it points at the scripting bash, built without readline,
-- so it has no line editor and arrow keys echo as `^[[A`.
getLoginShell :: IO String
#ifdef mingw32_HOST_OS
-- No password database on Windows; use the command processor.
getLoginShell = fromMaybe "powershell.exe" <$> lookupEnv "COMSPEC"
#elif defined(ghcjs_HOST_OS)
-- No password database in the browser — geteuid has no JS-RTS shim, and a
-- missing shim raises a JS ReferenceError that Haskell `catch` cannot see.
-- The value only labels the (unavailable) terminal there.
getLoginShell = return "/bin/sh"
#else
getLoginShell = do
    loginShell <- (userShell <$> (getRealUserID >>= getUserEntryForID))
                    `catch` \(_ :: SomeException) -> return ""
    envShell <- fromMaybe "" <$> lookupEnv "SHELL"
    return $ fromMaybe "/bin/bash" . listToMaybe $ filter (not . null) [loginShell, envShell]
#endif

-- | Arguments for running 'getLoginShell' directly as an interactive terminal
-- (the no-tmux path).  POSIX shells want @-i@ for the line editor; the Windows
-- command processors (cmd.exe/PowerShell) take no such flag.
interactiveShellArgs :: [String]
#ifdef mingw32_HOST_OS
interactiveShellArgs = []
#else
interactiveShellArgs = ["-i"]
#endif

-- | Whether tmux (and thus persistent sessions and control mode) is usable on
-- this platform.  There is no Windows tmux, so terminals there are plain,
-- non-persistent ConPTY shells and the control-mode path is never taken.
tmuxSupported :: Bool
#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
tmuxSupported = False
#else
tmuxSupported = True
#endif


-- | Write (idempotently) the minimal tmux config used for leksah's terminals:
-- no status bar, pass window titles through to xterm, and use the login shell.
-- Only applies when the tmux server first starts (i.e. for the first terminal).
writeTmuxConf :: FilePath -> IO FilePath
writeTmuxConf loginShell = do
    dir <- getTemporaryDirectory
    let path = dir </> "leksah.tmux.conf"
    cb <- clipboardCopyCmd
    writeFile path $ unlines $
        [ "set -g status off"
        , "set -g set-titles on"
        , "set -g set-titles-string \"#T\""
        , "set -g default-shell \"" <> loginShell <> "\""
        -- Report focus in/out: tmux requests focus events from the outer terminal
        -- (xterm.js, which sends CSI I / CSI O on focus/blur) and forwards them to
        -- the program in the active pane.  This is what lets a Claude Code running
        -- in a pane know when its pane gains/loses focus (needed for teammate-mode
        -- tmux) — both on pane/tab switches and when the whole app is deactivated.
        , "set -g focus-events on"
        -- True colour (24-bit) and OSC 8 hyperlinks out to xterm.js (which
        -- supports both): the outer terminal reports TERM=xterm-256color, and
        -- tmux only forwards these when the feature is advertised for it.  Without
        -- RGB, Claude Code / TUIs are stuck at 256 colours and look washed out.
        , "set -sa terminal-features \",xterm-256color:RGB:hyperlinks\""
        -- Let programs in a pane emit DCS passthrough sequences through tmux (OSC
        -- 52 clipboard, progress, image/hyperlink protocols); off by default.
        , "set -g allow-passthrough on"
        -- Flag windows with new output (activity) or that have gone quiet
        -- (silence) — surfaced as badges in the Terminals tree so you can see at a
        -- glance which teammate is working vs idle.  (Bell is already flagged by
        -- default via bell-action; Claude Code rings it for permission prompts.)
        , "set -g monitor-activity on"
        , "set -g monitor-silence 15"
        -- Don't also print tmux's own \"activity in window N\" status message /
        -- visual bell — the tree badges are the surface we want.
        , "set -g visual-activity off"
        , "set -g visual-silence off"
        , "set -g visual-bell off"
        -- Mouse on so the wheel scrolls tmux's scrollback in the xterm.js pane;
        -- a generous history so there's plenty to scroll back through.
        , "set -g mouse on"
        , "set -g history-limit 50000"
        -- vim-style pane navigation (lower-case) and resizing (upper-case), as an
        -- alternative to the prefix+arrow keys.  These rebind prefix `l` (default
        -- last-window) and `L` (default last-session), so move those to Tab and
        -- Shift-Tab — the common convention when hjkl/HJKL take l/L.
        , "bind h select-pane -L"
        , "bind j select-pane -D"
        , "bind k select-pane -U"
        , "bind l select-pane -R"
        , "bind -r H resize-pane -L 5"
        , "bind -r J resize-pane -D 5"
        , "bind -r K resize-pane -U 5"
        , "bind -r L resize-pane -R 5"
        -- Fine (1-cell) resize on Ctrl+hjkl, the common home-row companion to the
        -- coarse (5-cell) HJKL above; both repeatable (-r).
        , "bind -r C-h resize-pane -L 1"
        , "bind -r C-j resize-pane -D 1"
        , "bind -r C-k resize-pane -U 1"
        , "bind -r C-l resize-pane -R 1"
        , "bind Tab last-window"
        , "bind BTab switch-client -l"
        ]
        -- Pipe copy-mode selections to the system clipboard so copying in a
        -- terminal (mouse drag / yank) is shared with ⌘C/⌘V instead of living only
        -- in tmux's own paste buffer.  The default copy bindings use copy-pipe
        -- without a command, so they pick this up.
        <> maybe [] (\c -> ["set -s copy-command " <> show c]) cb
    return path


-- | Shell command tmux should pipe a copy-mode selection to so it lands on the
-- *system* clipboard (shared with the terminal's ⌘C/⌘V), not just tmux's own
-- paste buffer.  macOS uses @pbcopy@; elsewhere the first available Wayland/X
-- clipboard tool.  'Nothing' (leave @copy-command@ unset) if none is found.
clipboardCopyCmd :: IO (Maybe String)
clipboardCopyCmd
  | os == "darwin" = return (Just "pbcopy")
  | otherwise = firstAvailable
      [ ("wl-copy", "wl-copy")
      , ("xclip",   "xclip -selection clipboard -in")
      , ("xsel",    "xsel -ib") ]
  where
    firstAvailable [] = return Nothing
    firstAvailable ((exe, cmd):rest) =
        findExecutable exe >>= maybe (firstAvailable rest) (const (return (Just cmd)))

-- | Ensure a window running @cmd@ exists in the shared repl session, reusing
-- the window previously created for the same @key@ (recorded in the
-- @\@leksah_run@ window option) rather than piling up duplicates.  The window
-- is selected either way; returns the session id (the terminal tab key).
--
-- With @keepShell@ the window drops to the login shell when @cmd@ ends, so
-- its output stays readable (run/test/bench windows); without it a clean
-- exit closes the window and only a FAILING @cmd@ keeps a shell (repl
-- windows — exiting the repl should exit right out).
-- | Remote analogue of 'ensureCommandWindow': ensure the REMOTE host's
-- default-socket tmux has a @leksah@ session (the one @ssh:\/\/HOST@ tabs
-- attach — see IDE.Web.Widget.TerminalCC) with a window named @name@ whose
-- cwd is @rdir@, optionally running @cmd@ (dropping to a login shell when
-- it ends), and select it.  ONE pooled ssh round trip.
ensureRemoteWindow :: Text      -- ^ host
                   -> FilePath  -- ^ remote directory (host-local, absolute)
                   -> Text      -- ^ window name
                   -> Maybe Text -- ^ optional command to run in the window
                   -> IO Bool
ensureRemoteWindow host rdir name mbCmd = do
    (code, _, _) <- runSsh host script
        [T.pack rdir, name, fromMaybe "" mbCmd] mempty
    return (code == ExitSuccess)
  where
    -- Like the local 'ensureCommandWindow', the identity lives on the *pane*
    -- (@leksah_run pane option = the window name) so it survives the
    -- Terminals-tree drag-and-drop; the window-name grep remains as the
    -- fallback for panes created before tagging (or a remote tmux too old
    -- for pane options — set-option -p failures are ignored).
    script =
      "d=\"$0\"; n=\"$1\"; c=\"$2\"; "
      <> "if [ -n \"$c\" ]; then set -- sh -lc \"$c; exec \\\"${SHELL:-sh}\\\" -l\"; else set --; fi; "
      <> "if tmux has-session -t =leksah 2>/dev/null; then "
      <> "p=$(tmux list-panes -s -t =leksah -F '#{pane_id}\t#{@leksah_run}' 2>/dev/null"
      <> " | awk -F '\t' -v k=\"$n\" '$2==k{print $1; exit}'); "
      <> "if [ -n \"$p\" ]; then tmux select-window -t \"$p\"; exec tmux select-pane -t \"$p\"; fi; "
      <> "if tmux list-windows -t =leksah -F '#{window_name}' 2>/dev/null | grep -Fqx \"$n\"; then "
      <> "exec tmux select-window -t \"=leksah:$n\"; fi; "
      <> "p=$(tmux new-window -t =leksah -c \"$d\" -n \"$n\" -P -F '#{pane_id}' \"$@\"); "
      <> "else "
      <> "p=$(tmux new-session -d -s leksah -c \"$d\" -n \"$n\" -P -F '#{pane_id}' \"$@\"); "
      <> "fi; "
      <> "[ -n \"$p\" ] || exit 1; "
      <> "tmux set-option -p -t \"$p\" @leksah_run \"$n\" 2>/dev/null; exit 0"

-- | Open (or focus) a terminal whose working directory is @dir@ — local or
-- @ssh:\/\/HOST\/…@.  Shared by "Open Terminal Here" (projectOpenTerminal) and
-- by double-clicking a project or directory in the workspace tree.  The tmux
-- window is named after the directory's last path segment; a remote dir lands
-- in the host's shared @leksah@ session (one pooled ssh round trip), a local
-- dir in leksah's own tmux server keyed by the directory (each directory its
-- own window).
--
-- If the project that owns @dir@ has a stored command prefix (@psCmdPrefix@,
-- e.g. @nix develop -c@ / @nix shell … -c@ — for both local and remote
-- projects), the terminal opens *inside* that environment: the window @exec@s
-- the prefixed login shell, so the tools the prefix puts on @PATH@ are there,
-- and exiting the shell closes the window.  No prefix → a plain login shell.
--
-- Fire-and-forget on a background thread, so it never blocks the reflex frame
-- thread / the calling IDEAction.
openTerminalInDir :: MonadIO m => FilePath -> m ()
openTerminalInDir dir0 = liftIO . void . forkIO $ do
    -- 'pjDir' (and hence @dir0@) usually ends in a path separator, which would
    -- leave the shell's cwd — and its prompt — with a trailing "//"; strip it.
    let dir = dropTrailingPathSeparator dir0
    mbPrefix <- mfilter (not . T.null) <$> cmdPrefixForDir dir
    let shellUnder p = "exec " <> p <> " \"${SHELL:-bash}\" -l"
    case parseRemotePath dir of
        Just (host, rdir0) -> do
            let rdir = dropTrailingPathSeparator rdir0
            _ <- ensureRemoteWindow host rdir (winName rdir) (shellUnder <$> mbPrefix)
            requestRemoteTerm (host <> "#leksah")
        Nothing ->
            ensureCommandWindow True (T.pack dir <> "#shell") dir (winName dir)
                (maybe "true" shellUnder mbPrefix)
                >>= mapM_ requestLocalTerm
  where
    winName p = case T.pack (takeFileName p) of
                  "" -> "shell"
                  n  -> n

-- | Open (or focus) a terminal in @dir@ (local or @ssh:\/\/@) that runs @cmd@
-- inside the owning project's command prefix.  One reusable window per
-- @(dir, keySuffix)@ — e.g. a single @"git"@ window per checkout.  Fire-and-forget;
-- for a plain shell (no command) use 'openTerminalInDir'.
--
-- With @keepOpen@ the window always drops to a login shell when @cmd@ ends, so
-- its output stays readable.  Without it the window CLOSES on success and only a
-- FAILING @cmd@ (non-zero exit) keeps a shell — so you see errors\/conflicts but a
-- clean run tidies up after itself.
runInTerminal :: MonadIO m => Bool -> FilePath -> Text -> Text -> Text -> m ()
runInTerminal keepOpen dir0 keySuffix name cmd = liftIO . void . forkIO $ do
    let dir = dropTrailingPathSeparator dir0
    mbPrefix <- mfilter (not . T.null) <$> cmdPrefixForDir dir
    let full = maybe cmd (\p -> p <> " " <> cmd) mbPrefix
    case parseRemotePath dir of
        Just (host, rdir0) -> do
            -- ensureRemoteWindow wraps a non-empty command as
            -- @sh -lc "<cmd>; exec ${SHELL} -l"@, keeping the shell afterwards; to
            -- close on success instead, make the command exit itself when it wins,
            -- so the trailing shell only runs on a non-zero exit.
            let rcmd = if keepOpen then full else full <> " && exit"
            _ <- ensureRemoteWindow host (dropTrailingPathSeparator rdir0) name (Just rcmd)
            requestRemoteTerm (host <> "#leksah")
        Nothing ->
            ensureCommandWindow keepOpen (T.pack dir <> "#" <> keySuffix) dir name full
                >>= mapM_ requestLocalTerm

-- | The stored command prefix (@psCmdPrefix@) of the workspace project that
-- contains @dir@, read from the live IDE — 'Nothing' when there's no IDE yet,
-- no matching project, or no prefix set.  Used to open a project's terminals
-- inside its environment (local or remote).  Mirrors @IDE.LSP.remotePrefixFor@.
cmdPrefixForDir :: FilePath -> IO (Maybe Text)
cmdPrefixForDir dir = getGlobalIDERef >>= \case
    Nothing   -> return Nothing
    Just ideR -> do
        mbWs <- reflectIDE (readIDE workspace) ideR
        return $ do
            ws      <- mbWs
            project <- find (\p -> pjDir (pjKey p) `dirContains` dir) (ws ^. wsProjects)
            psCmdPrefix (wsSettingsFor (pjKey project) ws)
  where
    -- Does directory @parent@ contain (or equal) @child@?  A textual test
    -- rather than 'IDE.Utils.FileUtils.isSubPath', which runs 'normalise' —
    -- that both mangles @ssh:\/\/@ paths and, via 'splitPath', trips on a
    -- trailing-slash mismatch between 'pjDir' (keeps one) and a stripped dir.
    -- Normalising both to exactly one trailing separator keeps the prefix test
    -- on segment boundaries (so @…/foo@ doesn't match @…/foobar@).
    dirContains parent child =
        let norm p = addTrailingPathSeparator (dropTrailingPathSeparator p)
        in norm parent `isPrefixOf` norm child

-- | Build the tmux @split-window@ command for splitting the active terminal's
-- current pane.  A bare split starts a login shell in the pane's current
-- directory; this reproduces the *window's* setup instead.  A directory window
-- — one opened by double-clicking a directory, whose @\@leksah_run@ key ends in
-- @#shell@ — re-enters that project's command prefix (@nix develop -c@ …), so
-- the new pane lands in the same environment.  Any other window (a component
-- repl, a run\/test\/bench window, a nix window, or one with no marker) just
-- inherits the directory with a plain shell — \"just set the directory\".
-- Falls back to a plain @split-window@ when the pane can't be inspected.
buildSplitWindowCommand :: Bool     -- ^ horizontal split (Split Right)?
                        -> Text     -- ^ the active (local) tmux session id
                        -> IO Text
buildSplitWindowCommand horizontal session = do
    (path, runKey) <- queryActivePane session
    result <-
      if null path
        then return bare
        else do
          -- Only a directory window re-applies its project's prefix; every
          -- other window just gets a plain shell in the inherited directory.
          mbPrefix <- if "#shell" `T.isSuffixOf` runKey
                        then mfilter (not . T.null) <$> cmdPrefixForDir path
                        else return Nothing
          let cwd = " -c " <> shq (T.pack path)
              cmd = case mbPrefix of
                      Just p  -> " " <> shq ("exec " <> p <> " \"${SHELL:-bash}\" -l")
                      Nothing -> ""
              -- A directory window's key lives on its pane (see
              -- 'ensureCommandWindow'); copy it to the new pane — split-window
              -- leaves it active, so a target-less @set-option -p@ hits it —
              -- keeping the directory findable from the tree even if the
              -- original shell pane later exits.  Command panes (claude/git/
              -- repl) deliberately don't propagate: their key must stay on
              -- the one pane running the command, so re-opening focuses it.
              tag = if "#shell" `T.isSuffixOf` runKey
                      then " ; set-option -p @leksah_run " <> shq runKey
                      else ""
          return (bare <> cwd <> cmd <> tag)
    debugM "leksah" $ "buildSplitWindowCommand: session=" <> T.unpack session
        <> " path=" <> show path <> " runKey=" <> show runKey
        <> " -> " <> T.unpack result
    return result
  where
    bare = "split-window " <> (if horizontal then "-h" else "-v")
    -- The active pane's current directory and its window's @leksah_run marker,
    -- in one round trip on leksah's private tmux server.
    queryActivePane sess = (`catch` \(_ :: SomeException) -> return ("", "")) $
        findExecutable "tmux" >>= \case
          Nothing   -> return ("", "")
          Just tmux -> do
            (_, out, _) <- readProcessWithExitCode tmux
                [ "-L", tmuxSocket, "display-message", "-p", "-t", T.unpack sess
                , "-F", "#{pane_current_path}\t#{@leksah_run}" ] ""
            case T.splitOn "\t" (T.strip (T.pack out)) of
              (p : rest) -> return (T.unpack p, T.intercalate "\t" rest)
              _          -> return ("", "")
    -- tmux command-line single-quoting (tmux unquotes '…' literally and
    -- concatenates adjacent quotes, like the shell): keeps @${SHELL}@ out of
    -- tmux's own format expansion, passing it through to the pane's /bin/sh.
    shq t = "'" <> T.replace "'" "'\\''" t <> "'"

ensureCommandWindow :: Bool -> Text -> FilePath -> Text -> Text -> IO (Maybe Text)
ensureCommandWindow keepShell key dir name cmd = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            shell <- getLoginShell
            conf  <- writeTmuxConf shell
            let base = ["-L", tmuxSocket, "-f", conf]
                run as = readProcessWithExitCode tmux (base ++ as) ""
            -- The key lives on the *pane* (it travels with the Terminals-tree
            -- drag-and-drop / break-pane), so find the pane wherever it is
            -- now, make it the shown pane of its (possibly new) session, and
            -- return THAT session's id — the terminal-tab key — so the tab
            -- that opens is the one the pane actually lives in.
            findRunPane key >>= \case
              Just (sid, wid, pid) -> do
                _ <- run ["select-window", "-t", T.unpack wid]
                _ <- run ["select-pane", "-t", T.unpack pid]
                return (Just sid)
              Nothing -> do
                (hasRc, _, _) <- run ["has-session", "-t", "=" <> T.unpack replSessionName]
                let mk = if hasRc == ExitSuccess
                           then ["new-window", "-t", "=" <> T.unpack replSessionName]
                           else ["new-session", "-d", "-s", T.unpack replSessionName]
                (_, out, _) <- run $ mk ++
                    [ "-c", dir, "-n", T.unpack name, "-P", "-F"
                    , "#{session_id}\t#{window_id}\t#{pane_id}"
                    , T.unpack cmd <> (if keepShell then " ; exec " else " || exec ") <> shell ]
                case T.splitOn "\t" (T.strip (T.pack out)) of
                  (sid : wid : pid : _) | not (T.null pid) -> do
                    -- Tag the pane, not the window: a window option would
                    -- stay behind when the pane is dragged away (and, through
                    -- option inheritance, would keep matching any sibling
                    -- panes left in the source window — the wrong pane).
                    _ <- run ["set-option", "-p", "-t", T.unpack pid, "@leksah_run", T.unpack key]
                    _ <- run ["select-window", "-t", T.unpack wid]
                    return (Just sid)
                  _ -> return Nothing
