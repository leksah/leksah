{-# LANGUAGE ScopedTypeVariables #-}
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
module IDE.Web.TerminalInput
  ( registerTerminalPty
  , unregisterTerminalPty
  , setActiveTerminal
  , sendToActiveTerminal
  ) where

import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import qualified Data.Map as M
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Pty (Pty, writePty)

{-# NOINLINE ptyRegistry #-}
ptyRegistry :: IORef (M.Map Text Pty)
ptyRegistry = unsafePerformIO (newIORef M.empty)

{-# NOINLINE activeRef #-}
activeRef :: IORef (Maybe Text)
activeRef = unsafePerformIO (newIORef Nothing)

-- | Record the PTY backing terminal @n@ (its tmux session id; called as the
-- terminal is created).
registerTerminalPty :: Text -> Pty -> IO ()
registerTerminalPty n pty = atomicModifyIORef' ptyRegistry $ \m -> (M.insert n pty m, ())

-- | Forget terminal @n@'s PTY (e.g. when its session is killed).
unregisterTerminalPty :: Text -> IO ()
unregisterTerminalPty n = atomicModifyIORef' ptyRegistry $ \m -> (M.delete n m, ())

-- | Publish which terminal is currently on screen (the editor-area @wide0@ tab),
-- or 'Nothing' when the visible pane isn't a terminal.
setActiveTerminal :: Maybe Text -> IO ()
setActiveTerminal = writeIORef activeRef

-- | Write @bytes@ to the active terminal's PTY, as if typed.  A no-op (rather
-- than an error) when no terminal is active or its PTY has gone away.
sendToActiveTerminal :: ByteString -> IO ()
sendToActiveTerminal bytes = do
  mActive <- readIORef activeRef
  reg <- readIORef ptyRegistry
  case (`M.lookup` reg) =<< mActive of
    Just pty -> writePty pty bytes `catch` \(_ :: SomeException) -> return ()
    Nothing  -> return ()
