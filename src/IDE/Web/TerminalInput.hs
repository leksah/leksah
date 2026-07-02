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
  , setActiveTerminal
  , setActiveTerminalNotifier
  , sendToActiveTerminal
  , tmuxCommandActiveTerminal
  ) where

import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Pty (Pty, writePty)

{-# NOINLINE ptyRegistry #-}
ptyRegistry :: IORef (M.Map Text Pty)
ptyRegistry = unsafePerformIO (newIORef M.empty)

-- Command runners of the control-mode terminals, by terminal (tab) id.
{-# NOINLINE ccRegistry #-}
ccRegistry :: IORef (M.Map Text (Text -> IO ()))
ccRegistry = unsafePerformIO (newIORef M.empty)

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

-- | Publish which terminal is currently on screen (the editor-area @wide0@ tab),
-- or 'Nothing' when the visible pane isn't a terminal.
setActiveTerminal :: Maybe Text -> IO ()
setActiveTerminal mb = do
  writeIORef activeRef mb
  notify <- readIORef notifierRef
  notify (isJust mb) `catch` \(_ :: SomeException) -> return ()

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
