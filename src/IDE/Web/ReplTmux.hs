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
  , replSessionName
  , ffcabalTmuxEnv
  , findReplWindow
  , selectTmuxWindowById
  , ensureCommandWindow
  , getLoginShell
  , writeTmuxConf
  , clipboardCopyCmd
  ) where

import Control.Exception (catch, SomeException)
import Control.Monad (void)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Info (os)
#ifndef mingw32_HOST_OS
import System.Posix.User (getRealUserID, getUserEntryForID, userShell)
#endif
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

-- | The login shell to run in terminals.  Prefer the password-database login
-- shell (what terminal emulators use).  $SHELL is unreliable here: launched from
-- a `nix develop` shell it points at the scripting bash, built without readline,
-- so it has no line editor and arrow keys echo as `^[[A`.
getLoginShell :: IO String
#ifdef mingw32_HOST_OS
-- No password database on Windows; use the command processor.
getLoginShell = fromMaybe "powershell.exe" <$> lookupEnv "COMSPEC"
#else
getLoginShell = do
    loginShell <- (userShell <$> (getRealUserID >>= getUserEntryForID))
                    `catch` \(_ :: SomeException) -> return ""
    envShell <- fromMaybe "" <$> lookupEnv "SHELL"
    return $ fromMaybe "/bin/bash" . listToMaybe $ filter (not . null) [loginShell, envShell]
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
ensureCommandWindow :: Bool -> Text -> FilePath -> Text -> Text -> IO (Maybe Text)
ensureCommandWindow keepShell key dir name cmd = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            shell <- getLoginShell
            conf  <- writeTmuxConf shell
            let base = ["-L", tmuxSocket, "-f", conf]
                run as = readProcessWithExitCode tmux (base ++ as) ""
            (_, existing, _) <- run
                [ "list-windows", "-a", "-F"
                , "#{session_id}\t#{window_id}\t#{@leksah_run}" ]
            case [ (sid, wid) | l <- T.lines (T.pack existing)
                 , (sid : wid : k) <- [T.splitOn "\t" l]
                 , T.intercalate "\t" k == key ] of
              ((sid, wid) : _) -> do
                _ <- run ["select-window", "-t", T.unpack wid]
                return (Just sid)
              [] -> do
                (hasRc, _, _) <- run ["has-session", "-t", "=" <> T.unpack replSessionName]
                let mk = if hasRc == ExitSuccess
                           then ["new-window", "-t", "=" <> T.unpack replSessionName]
                           else ["new-session", "-d", "-s", T.unpack replSessionName]
                (_, out, _) <- run $ mk ++
                    [ "-c", dir, "-n", T.unpack name, "-P", "-F"
                    , "#{session_id}\t#{window_id}"
                    , T.unpack cmd <> (if keepShell then " ; exec " else " || exec ") <> shell ]
                case T.splitOn "\t" (T.strip (T.pack out)) of
                  (sid : wid : _) | not (T.null wid) -> do
                    _ <- run ["set-option", "-w", "-t", T.unpack wid, "@leksah_run", T.unpack key]
                    _ <- run ["select-window", "-t", T.unpack wid]
                    return (Just sid)
                  _ -> return Nothing
