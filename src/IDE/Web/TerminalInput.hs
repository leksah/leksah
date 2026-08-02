{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
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
  , getActiveTerminal
  , getActiveConvertible
  , setActiveTerminalNotifier
  , setActiveConvertible
  , setActiveViewSplit
  , setActiveSplitWindow
  , setSplitActiveNotifier
  , sendToActiveTerminal
  , tmuxCommandActiveTerminal
  , splitActiveTerminal
  , dispatchTmuxPrefix
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, void, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (cons)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.IO.Unsafe (unsafePerformIO)
#if defined(mingw32_HOST_OS)
import IDE.Web.ConPty (Pty, writePty)
#elif defined(ghcjs_HOST_OS)
import IDE.Web.NoPty (Pty, writePty)
#else
import System.Posix.Pty (Pty, writePty)
#endif

import IDE.Core.Types (TabKey)
import IDE.Web.ConvertRequest (requestConvert)
import IDE.Web.ReplTmux (buildSplitWindowCommand, activePaneIdOfWindow)

{-# NOINLINE ptyRegistry #-}
ptyRegistry :: IORef (M.Map Text Pty)
ptyRegistry = unsafePerformIO (newIORef M.empty)

-- Command runners of the control-mode terminals, by terminal (tab) id.
-- Entries are tagged with a registration id (see 'registryCounter'): several
-- leksah windows can share one tmux session, and an old widget's teardown
-- (its client's EvExit) must never unregister the REPLACEMENT widget's entry
-- for the same key — the id makes every unregister self-identifying.
{-# NOINLINE ccRegistry #-}
ccRegistry :: IORef (M.Map Text (Integer, Text -> IO ()))
ccRegistry = unsafePerformIO (newIORef M.empty)

-- One counter for all the tagged registries in this module.
{-# NOINLINE registryCounter #-}
registryCounter :: IORef Integer
registryCounter = unsafePerformIO (newIORef 0)

-- | Insert a tagged entry; returns the registration id for 'unregisterKeyed'.
registerKeyed :: IORef (M.Map Text (Integer, a)) -> Text -> a -> IO Integer
registerKeyed ref n v = do
  myId <- atomicModifyIORef' registryCounter $ \i -> (i + 1, i + 1)
  atomicModifyIORef' ref $ \m -> (M.insert n (myId, v) m, ())
  return myId

-- | Drop an entry — but only if it is still the one registered under @myId@
-- (a later widget may already own the slot).
unregisterKeyed :: IORef (M.Map Text (Integer, a)) -> Text -> Integer -> IO ()
unregisterKeyed ref n myId = atomicModifyIORef' ref $ \m ->
  case M.lookup n m of
    Just (i, _) | i == myId -> (M.delete n m, ())
    _                       -> (m, ())

lookupKeyed :: IORef (M.Map Text (Integer, a)) -> Text -> IO (Maybe a)
lookupKeyed ref n = fmap snd . M.lookup n <$> readIORef ref

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
splitRegistry :: IORef (M.Map Text (Integer, Int -> IO ()))
splitRegistry = unsafePerformIO (newIORef M.empty)

-- Focus callbacks of the terminal / leksah-window widgets, by tmux session id
-- AND by leksah-window id: bring the widget's focused pane/leaf to keyboard
-- focus, used when a repl is launched into the session (or an item is opened
-- into the window) so it becomes THE active pane even though the action came
-- from the tree/a button.
{-# NOINLINE focusRegistry #-}
focusRegistry :: IORef (M.Map Text (Integer, IO ()))
focusRegistry = unsafePerformIO (newIORef M.empty)

-- Focus requests that arrived before their widget registered (the tab is
-- still mounting — the first repl into a fresh session, a ⌘D that binds a
-- session and rebuilds the tab).  'registerTerminalFocus' consumes a pending
-- request for its key at registration, so the request is STICKY instead of
-- being retried on timers.
{-# NOINLINE pendingFocusKeys #-}
pendingFocusKeys :: IORef (M.Map Text ())
pendingFocusKeys = unsafePerformIO (newIORef M.empty)

-- (The backing-pane registry lived here until the native split layouts
-- replaced the ⌘D pane overlays — an editor converts to a PaneView now, no
-- tmux twin needed.)

{-# NOINLINE activeRef #-}
activeRef :: IORef (Maybe Text)
activeRef = unsafePerformIO (newIORef Nothing)

-- Told whether a terminal is active whenever that changes; the native macOS
-- menu uses it to enable/disable the Terminal menu's key equivalents (so ⌘D
-- etc. pass through to the editor when no terminal is on screen).
{-# NOINLINE notifierRef #-}
notifierRef :: IORef (Bool -> IO ())
notifierRef = unsafePerformIO (newIORef (const (return ())))

-- The active wide0 tab when it is not a terminal but CAN convert to a tmux
-- pane (an editor / git-log tab).  ⌘D on such a tab materializes it into a
-- leksah window (see 'splitActiveTerminal'); the paired notifier drives
-- the native Split items' enablement (leksah_set_split_active).
{-# NOINLINE activeConvertibleRef #-}
activeConvertibleRef :: IORef (Maybe TabKey)
activeConvertibleRef = unsafePerformIO (newIORef Nothing)

-- The ⌘D override when the active leksah window's FOCUSED pane is a native
-- VIEW (an editor / git log in the split): instead of splitting a tmux pane,
-- run the stored action (Main keeps it current — it opens a terminal in the
-- view's directory as a new native sibling pane).  The Bool is ⌘D's
-- horizontal flag.
{-# NOINLINE activeViewSplitRef #-}
activeViewSplitRef :: IORef (Maybe (Bool -> IO ()))
activeViewSplitRef = unsafePerformIO (newIORef Nothing)

-- | Publish (or clear) the view-pane ⌘D action; called from Main whenever
-- the active tab / focused pane changes.
setActiveViewSplit :: Maybe (Bool -> IO ()) -> IO ()
setActiveViewSplit = writeIORef activeViewSplitRef

-- The tmux WINDOW the active leksah window's focused pane shows (when it is
-- a tmux pane): ⌘D / C-b % target ITS active pane rather than the session's
-- current one, which — with several windows visible at once — may be a
-- different window entirely.
{-# NOINLINE activeSplitWindowRef #-}
activeSplitWindowRef :: IORef (Maybe Text)
activeSplitWindowRef = unsafePerformIO (newIORef Nothing)

-- | Publish (or clear) the focused tmux window; paired with
-- 'setActiveViewSplit' in Main's focused-pane tracking.
setActiveSplitWindow :: Maybe Text -> IO ()
setActiveSplitWindow = writeIORef activeSplitWindowRef

{-# NOINLINE splitNotifierRef #-}
splitNotifierRef :: IORef (Bool -> IO ())
splitNotifierRef = unsafePerformIO (newIORef (const (return ())))

-- | Publish whether the active wide0 tab is a convertible editor/git-log tab
-- (and which); called from Main whenever the active tab changes.
setActiveConvertible :: Maybe TabKey -> IO ()
setActiveConvertible mb = do
  writeIORef activeConvertibleRef mb
  notify <- readIORef splitNotifierRef
  notify (isJust mb) `catch` \(_ :: SomeException) -> return ()

-- | Install the native "split enabled" notifier (the wkwebview front end's
-- 'c_setSplitActive'); mirrors 'setActiveTerminalNotifier'.  Replays the
-- CURRENT state at once: registration can race the restore-time publications
-- (they write the ref regardless), so a late-registering notifier syncs
-- itself here rather than callers delaying their pushes.
setSplitActiveNotifier :: (Bool -> IO ()) -> IO ()
setSplitActiveNotifier notify = do
  writeIORef splitNotifierRef notify
  mb <- readIORef activeConvertibleRef
  notify (isJust mb) `catch` \(_ :: SomeException) -> return ()

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
-- Returns the registration id for 'unregisterTerminalCC'.
registerTerminalCC :: Text -> (Text -> IO ()) -> IO Integer
registerTerminalCC = registerKeyed ccRegistry

-- | Forget CC terminal @n@'s runner (its client exited or the tab closed) —
-- id-guarded, so a stale teardown never drops a replacement's registration.
unregisterTerminalCC :: Text -> Integer -> IO ()
unregisterTerminalCC = unregisterKeyed ccRegistry

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
registerTerminalSplits :: Text -> (Int -> IO ()) -> IO Integer
registerTerminalSplits = registerKeyed splitRegistry

unregisterTerminalSplits :: Text -> Integer -> IO ()
unregisterTerminalSplits = unregisterKeyed splitRegistry

-- | Record widget @n@'s focus callback (see 'focusRegistry'), consuming any
-- focus request that arrived before this widget existed ('pendingFocusKeys')
-- — the sticky-request half of 'focusTerminalPane'.
registerTerminalFocus :: Text -> IO () -> IO Integer
registerTerminalFocus n act = do
  myId <- registerKeyed focusRegistry n act
  pending <- atomicModifyIORef' pendingFocusKeys $ \m ->
    (M.delete n m, M.member n m)
  when pending $ act `catch` \(_ :: SomeException) -> return ()
  return myId

unregisterTerminalFocus :: Text -> Integer -> IO ()
unregisterTerminalFocus = unregisterKeyed focusRegistry

-- | Ask widget @n@ (a tmux session id or a leksah-window id) to focus its
-- current pane/leaf.  If nothing is registered yet — the tab is still
-- mounting — the request is remembered and delivered by
-- 'registerTerminalFocus' when the widget appears, so callers need no
-- retry timers.
focusTerminalPane :: Text -> IO ()
focusTerminalPane n =
  lookupKeyed focusRegistry n >>= \case
    Just act -> act `catch` \(_ :: SomeException) -> return ()
    Nothing  -> atomicModifyIORef' pendingFocusKeys $ \m -> (M.insert n () m, ())

-- | Select the active terminal's Nth split (1-based, layout order) through
-- its registered selector.  'False' = the active terminal has none (classic
-- PTY tab, or no terminal active) and the caller should fall back to tmux's
-- own pane indexes.
selectSplitActiveTerminal :: Int -> IO Bool
selectSplitActiveTerminal n = do
  mActive <- readIORef activeRef
  reg <- readIORef splitRegistry
  case fmap snd . (`M.lookup` reg) =<< mActive of
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

-- | The session id of the terminal currently on screen ('Nothing' if the
-- visible tab isn't a terminal) — the read side of 'setActiveTerminal', used by
-- the split-open pipeline to find the pane to split.
getActiveTerminal :: IO (Maybe Text)
getActiveTerminal = readIORef activeRef

-- | The active tab when it is a convertible editor/git-log ('Nothing'
-- otherwise) — the read side of 'setActiveConvertible'.
getActiveConvertible :: IO (Maybe TabKey)
getActiveConvertible = readIORef activeConvertibleRef

-- | Register the callback told whether a terminal is active (native menu
-- enabling).  Called once at startup by the front end.  Replays the current
-- state at once (see 'setSplitActiveNotifier').
setActiveTerminalNotifier :: (Bool -> IO ()) -> IO ()
setActiveTerminalNotifier notify = do
  writeIORef notifierRef notify
  mb <- readIORef activeRef
  notify (isJust mb) `catch` \(_ :: SomeException) -> return ()

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
  case fmap snd . (`M.lookup` reg) =<< mActive of
    Just run -> do
      run cmd `catch` \(_ :: SomeException) -> return ()
      return True
    Nothing -> return False

-- | Split the active terminal.  On a control-mode (CC) tab we build a
-- @split-window@ that reproduces the window's setup — a directory window
-- re-enters its project's command prefix, other windows just inherit the
-- directory (see 'IDE.Web.ReplTmux.buildSplitWindowCommand').  Remote (@ssh:\/\/@)
-- tabs get a plain split (the local socket can't inspect the remote pane), and
-- a classic PTY tab (no CC runner) falls back to the @C-b@ chord.
splitActiveTerminal :: Bool -> ByteString -> IO ()
splitActiveTerminal horizontal chord = do
  mViewSplit <- readIORef activeViewSplitRef
  mActive <- readIORef activeRef
  reg <- readIORef ccRegistry
  case mViewSplit of
   -- The focused pane is a native VIEW in a leksah window: ⌘D opens a
   -- terminal beside it (in the view's directory) rather than splitting
   -- some tmux pane.
   Just act -> act horizontal
   Nothing -> case mActive of
    Just sid | Just (_, run) <- M.lookup sid reg -> do
      cmd <- if "ssh://" `T.isPrefixOf` sid
               then return ("split-window " <> if horizontal then "-h" else "-v")
               else do
                 -- Split the FOCUSED leksah pane's window (its active tmux
                 -- pane), not whatever window happens to be tmux-current.
                 mw <- readIORef activeSplitWindowRef
                 mtgt <- maybe (return Nothing) activePaneIdOfWindow mw
                 buildSplitWindowCommand horizontal sid mtgt
      run cmd `catch` \(_ :: SomeException) -> return ()
    Just _ -> sendToActiveTerminal (BS.cons 2 chord)
    Nothing -> readIORef activeConvertibleRef >>= \case
      -- ⌘D on a convertible editor/git-log tab: convert it to its backing
      -- tmux pane, then split (the pipeline lives in IDE.Web.Main).
      Just k  -> requestConvert (k, horizontal)
      Nothing -> sendToActiveTerminal (BS.cons 2 chord)

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
  | tok == "%"  = splitActiveTerminal True  "%"
  | tok == "\"" = splitActiveTerminal False "\""
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
  -- "%" (Split Right) and "\"" (Split Down) are handled specially in
  -- 'dispatchTmuxPrefix' via 'splitActiveTerminal' (they reproduce a directory
  -- window's environment rather than issuing a bare split).
  , ("x",     ("kill-pane",           "x"))
  , ("z",     ("resize-pane -Z",      "z"))
  , ("o",     ("select-pane -t :.+",  "o"))
  , (";",     ("select-pane -t :.-",  ";"))
  , ("Up",    ("select-pane -U",      "\ESC[A"))
  , ("Down",  ("select-pane -D",      "\ESC[B"))
  , ("Left",  ("select-pane -L",      "\ESC[D"))
  , ("Right", ("select-pane -R",      "\ESC[C"))
  -- The remaining Terminal-menu 'paneCmd's: without these a C-b chord for one
  -- of them fell through to the raw-chord path, a no-op on CC (control-mode)
  -- tabs — so it worked from the menu but not the keyboard.  The token is what
  -- 'window.LeksahTmux' hands us: the produced character ('{' etc.), a 'C-'/'M-'
  -- prefix for ctrl/alt, and 'Tab'/'Space' spelled out.
  , ("{",     ("swap-pane -U",        "{"))
  , ("}",     ("swap-pane -D",        "}"))
  , ("C-o",   ("rotate-window",       "\SI"))
  , ("!",     ("break-pane",          "!"))
  , ("m",     ("select-pane -m",      "m"))
  , ("M",     ("select-pane -M",      "M"))
  , ("[",     ("copy-mode",           "["))
  , ("]",     ("paste-buffer",        "]"))
  , ("r",     ("refresh-client",      "r"))
  , ("Tab",   ("last-window",         "\t"))
  , ("K",     ("resize-pane -U 5",    "K"))
  , ("J",     ("resize-pane -D 5",    "J"))
  , ("L",     ("resize-pane -R 5",    "L"))
  , ("H",     ("resize-pane -L 5",    "H"))
  , ("C-k",   ("resize-pane -U 1",    "\v"))
  , ("C-j",   ("resize-pane -D 1",    "\n"))
  , ("C-l",   ("resize-pane -R 1",    "\f"))
  , ("C-h",   ("resize-pane -L 1",    "\b"))
  ]
