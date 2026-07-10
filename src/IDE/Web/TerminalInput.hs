{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A process-global bridge for injecting input into the active terminal.
--
-- Each 'IDE.Web.Widget.Terminal.terminalWidget' owns a real PTY but doesn't
-- otherwise expose it.  The Tmux menu ('IDE.Web.MenuModel') needs to send the
-- @C-b X@ prefix sequences to whichever terminal is currently on screen, exactly
-- as if the user had typed them — but a menu 'Command' runs as an 'IDEAction',
-- outside the reflex network and any particular terminal widget.  So terminals
-- register their PTY here by id, 'IDE.Web.Main' publishes which terminal is
-- active, and the menu command writes the bytes to that PTY's input (the same
-- place xterm's @onData@ keystrokes go).
--
-- Control-mode (CC) terminals have no PTY — keystrokes are @send-keys@'d
-- straight into a pane, so a @C-b@ prefix chord never reaches tmux's key
-- handling and would just type @^B@ into the shell.  They instead register a
-- COMMAND RUNNER ('registerTerminalCC') that executes a tmux command verbatim
-- on their control channel (where \"current window/pane\" resolves to what the
-- widget displays); 'tmuxCommandActiveTerminal' prefers that path and reports
-- whether it ran, so callers can fall back to the chord for PTY terminals.
module IDE.Web.TerminalInput
  ( registerTerminalPty
  , unregisterTerminalPty
  , registerTerminalCC
  , unregisterTerminalCC
  , registerCCStop
  , unregisterCCStop
  , registerTerminalSplits
  , unregisterTerminalSplits
  , registerTerminalFocus
  , unregisterTerminalFocus
  , focusTerminalPane
  , selectSplitActiveTerminal
  , setActiveTerminal
  , isActiveTerminal
  , setActiveTerminalNotifier
  , sendToActiveTerminal
  , tmuxCommandActiveTerminal
  , dispatchTmuxPrefix
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, void, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (cons)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.IO.Unsafe (unsafePerformIO)
#ifdef mingw32_HOST_OS
import IDE.Web.ConPty (Pty, writePty)
#else
import System.Posix.Pty (Pty, writePty)
#endif

{-# NOINLINE ptyRegistry #-}
ptyRegistry :: IORef (M.Map Text Pty)
ptyRegistry = unsafePerformIO (newIORef M.empty)

-- Command runners of the control-mode terminals, by terminal (tab) id.
{-# NOINLINE ccRegistry #-}
ccRegistry :: IORef (M.Map Text (Text -> IO ()))
ccRegistry = unsafePerformIO (newIORef M.empty)

-- Teardown actions for the *control clients* (the @tmux -C@ processes), keyed
-- by session id.  A session's wide0 tab lives in exactly one OS window at a
-- time, so at most one leksah control client should be attached to it; when the
-- tab moves to another window that window builds a fresh client and the old
-- window's client would otherwise linger — attached, and (with @window-size
-- latest@) clamping the tmux window to its stale size.  reflex-dom gives the
-- widget no destructor to detach on, so instead each client registers its stop
-- action here and 'registerCCStop' reaps the prior client for the same session
-- on connect.  The 'Integer' tags each registration so a torn-down window's
-- late 'unregisterCCStop' can't drop the *replacement* client (id mismatch).
{-# NOINLINE ccStopRegistry #-}
ccStopRegistry :: IORef (M.Map Text (Integer, IO ()))
ccStopRegistry = unsafePerformIO (newIORef M.empty)

{-# NOINLINE ccStopCounter #-}
ccStopCounter :: IORef Integer
ccStopCounter = unsafePerformIO (newIORef 0)

-- Numbered split selectors of the control-mode terminals, by terminal (tab)
-- id: given N (1-based), select the displayed window's Nth pane in layout
-- (reading) order — the numbering the ⌘-held badges show.
{-# NOINLINE splitRegistry #-}
splitRegistry :: IORef (M.Map Text (Int -> IO ()))
splitRegistry = unsafePerformIO (newIORef M.empty)

-- Focus callbacks of the control-mode terminals, by terminal (tab) id: bring
-- the session's current window's active pane's xterm to keyboard focus (and the
-- active-pane highlight), used when a repl is launched into the session so it
-- becomes THE active pane even though the launch came from the tree/a button.
{-# NOINLINE focusRegistry #-}
focusRegistry :: IORef (M.Map Text (IO ()))
focusRegistry = unsafePerformIO (newIORef M.empty)

{-# NOINLINE activeRef #-}
activeRef :: IORef (Maybe Text)
activeRef = unsafePerformIO (newIORef Nothing)

-- Told whether a terminal is active whenever that changes; the native macOS
-- menu uses it to enable/disable the Terminal menu's key equivalents (so ⌘D
-- etc. pass through to the editor when no terminal is on screen).
{-# NOINLINE notifierRef #-}
notifierRef :: IORef (Bool -> IO ())
notifierRef = unsafePerformIO (newIORef (const (return ())))

-- | Record the PTY backing terminal @n@ (its tmux session id; called as the
-- terminal is created).
registerTerminalPty :: Text -> Pty -> IO ()
registerTerminalPty n pty = atomicModifyIORef' ptyRegistry $ \m -> (M.insert n pty m, ())

-- | Forget terminal @n@'s PTY (e.g. when its session is killed).
unregisterTerminalPty :: Text -> IO ()
unregisterTerminalPty n = atomicModifyIORef' ptyRegistry $ \m -> (M.delete n m, ())

-- | Record the control-channel command runner of CC terminal @n@.  The runner
-- gets tmux command text (no target rewriting — the control client's notion of
-- current window/pane is the displayed one) and must not block the caller.
registerTerminalCC :: Text -> (Text -> IO ()) -> IO ()
registerTerminalCC n run = atomicModifyIORef' ccRegistry $ \m -> (M.insert n run m, ())

-- | Forget CC terminal @n@'s runner (its client exited or the tab closed).
unregisterTerminalCC :: Text -> IO ()
unregisterTerminalCC n = atomicModifyIORef' ccRegistry $ \m -> (M.delete n m, ())

-- | Install session @n@'s control-client teardown, first running (on a fresh
-- thread, so a slow detach never blocks the new client's setup) any prior one
-- for the same session — the stale client left behind by the window this
-- session's tab just moved away from.  Returns an id identifying this
-- registration, to be passed back to 'unregisterCCStop'.
registerCCStop :: Text -> IO () -> IO Integer
registerCCStop n stop = do
    myId  <- atomicModifyIORef' ccStopCounter $ \i -> (i + 1, i + 1)
    prior <- atomicModifyIORef' ccStopRegistry $ \m ->
                (M.insert n (myId, stop) m, M.lookup n m)
    forM_ prior $ \(_, s) -> runStop s
    return myId

-- | Drop and run session @n@'s teardown — but only if the registered client is
-- still the one that got id @myId@ (a later window may already own the slot).
unregisterCCStop :: Text -> Integer -> IO ()
unregisterCCStop n myId = do
    mine <- atomicModifyIORef' ccStopRegistry $ \m ->
                case M.lookup n m of
                    Just (i, s) | i == myId -> (M.delete n m, Just s)
                    _                       -> (m, Nothing)
    forM_ mine runStop

runStop :: IO () -> IO ()
runStop s = void . forkIO $ s `catch` \(_ :: SomeException) -> return ()

-- | Record CC terminal @n@'s numbered split selector (see 'splitRegistry').
registerTerminalSplits :: Text -> (Int -> IO ()) -> IO ()
registerTerminalSplits n sel = atomicModifyIORef' splitRegistry $ \m -> (M.insert n sel m, ())

unregisterTerminalSplits :: Text -> IO ()
unregisterTerminalSplits n = atomicModifyIORef' splitRegistry $ \m -> (M.delete n m, ())

-- | Record CC terminal @n@'s focus callback (see 'focusRegistry').
registerTerminalFocus :: Text -> IO () -> IO ()
registerTerminalFocus n act = atomicModifyIORef' focusRegistry $ \m -> (M.insert n act m, ())

unregisterTerminalFocus :: Text -> IO ()
unregisterTerminalFocus n = atomicModifyIORef' focusRegistry $ \m -> (M.delete n m, ())

-- | Ask CC terminal @n@ to focus its current active pane, if it is registered.
-- A no-op when the session has no CC terminal (not open, or PTY-backed).
focusTerminalPane :: Text -> IO ()
focusTerminalPane n = do
  reg <- readIORef focusRegistry
  case M.lookup n reg of
    Just act -> act `catch` \(_ :: SomeException) -> return ()
    Nothing  -> return ()

-- | Select the active terminal's Nth split (1-based, layout order) through
-- its registered selector.  'False' = the active terminal has none (classic
-- PTY tab, or no terminal active) and the caller should fall back to tmux's
-- own pane indexes.
selectSplitActiveTerminal :: Int -> IO Bool
selectSplitActiveTerminal n = do
  mActive <- readIORef activeRef
  reg <- readIORef splitRegistry
  case (`M.lookup` reg) =<< mActive of
    Just sel -> do
      sel n `catch` \(_ :: SomeException) -> return ()
      return True
    Nothing -> return False

-- | Publish which terminal is currently on screen (the editor-area @wide0@ tab),
-- or 'Nothing' when the visible pane isn't a terminal.
setActiveTerminal :: Maybe Text -> IO ()
setActiveTerminal mb = do
  writeIORef activeRef mb
  notify <- readIORef notifierRef
  notify (isJust mb) `catch` \(_ :: SomeException) -> return ()

-- | Is terminal @n@ the one currently on screen (the editor-area @wide0@ tab)?
-- Used by the CC widget's focus retry to check a slow (remote) connection is
-- STILL the tab the user is looking at before it grabs the keyboard — a
-- connection that only completes after they have moved on must not steal focus.
isActiveTerminal :: Text -> IO Bool
isActiveTerminal n = (== Just n) <$> readIORef activeRef

-- | Register the callback told whether a terminal is active (native menu
-- enabling).  Called once at startup by the front end.
setActiveTerminalNotifier :: (Bool -> IO ()) -> IO ()
setActiveTerminalNotifier = writeIORef notifierRef

-- | Write @bytes@ to the active terminal's PTY, as if typed.  A no-op (rather
-- than an error) when no terminal is active or its PTY has gone away.
sendToActiveTerminal :: ByteString -> IO ()
sendToActiveTerminal bytes = do
  mActive <- readIORef activeRef
  reg <- readIORef ptyRegistry
  case (`M.lookup` reg) =<< mActive of
    Just pty -> writePty pty bytes `catch` \(_ :: SomeException) -> return ()
    Nothing  -> return ()

-- | Run a tmux command on the active terminal's control channel, if it is a
-- CC terminal.  Returns whether it was dispatched — 'False' means the active
-- terminal is PTY-backed (or absent) and the caller should fall back to the
-- @C-b@ chord via 'sendToActiveTerminal'.
tmuxCommandActiveTerminal :: Text -> IO Bool
tmuxCommandActiveTerminal cmd = do
  mActive <- readIORef activeRef
  reg <- readIORef ccRegistry
  case (`M.lookup` reg) =<< mActive of
    Just run -> do
      run cmd `catch` \(_ :: SomeException) -> return ()
      return True
    Nothing -> return False

-- | A tmux prefix key (the key that followed a @C-b@ chord, intercepted in JS
-- by @window.LeksahTmux@ when the "Intercept Ctrl+B" pref is on) — dispatch it
-- to the active terminal.  Mapped keys run the equivalent tmux command on a
-- control-mode (CC) tab's channel (where a raw @C-b@ chord can't work — it is
-- @send-keys@'d into the pane, bypassing tmux's prefix handling) and fall back
-- to typing the @C-b@ chord on a classic PTY tab, exactly like the Terminal
-- menu's 'IDE.Web.Command.paneCmd'.  Unmapped single characters are forwarded
-- as the raw @C-b@ chord (works on PTY tabs; a no-op on CC tabs, which register
-- no PTY).  The special key @w@ is handled reflex-side (activate the Terminals
-- pane) and never reaches here.  Tokens may carry @C-@\/@M-@ modifier prefixes.
dispatchTmuxPrefix :: Text -> IO ()
dispatchTmuxPrefix tok
  | Just (ccCmd, chord) <- M.lookup tok tmuxPrefixMap = run ccCmd chord
  | [d] <- T.unpack tok, d `elem` ['0' .. '9'] =
      run ("select-window -t " <> tok) (encodeUtf8 tok)
  | [_] <- T.unpack tok = sendToActiveTerminal (BS.cons 2 (encodeUtf8 tok))
  | otherwise = return ()
  where
    run ccCmd chord = do
      done <- tmuxCommandActiveTerminal ccCmd
      unless done $ sendToActiveTerminal (BS.cons 2 chord)

-- | Prefix keys we map to a tmux command (control channel) or @C-b@ chord
-- (classic PTY).  Mirrors the command/chord pairs of the Terminal menu.
tmuxPrefixMap :: M.Map Text (Text, ByteString)
tmuxPrefixMap = M.fromList
  [ ("c",     ("new-window",          "c"))
  , ("n",     ("next-window",         "n"))
  , ("p",     ("previous-window",     "p"))
  , ("&",     ("kill-window",         "&"))
  , ("%",     ("split-window -h",     "%"))
  , ("\"",    ("split-window -v",     "\""))
  , ("x",     ("kill-pane",           "x"))
  , ("z",     ("resize-pane -Z",      "z"))
  , ("o",     ("select-pane -t :.+",  "o"))
  , (";",     ("select-pane -t :.-",  ";"))
  , ("Up",    ("select-pane -U",      "\ESC[A"))
  , ("Down",  ("select-pane -D",      "\ESC[B"))
  , ("Left",  ("select-pane -L",      "\ESC[D"))
  , ("Right", ("select-pane -R",      "\ESC[C"))
  ]
