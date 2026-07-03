{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
-- | A terminal pane for the web UI.
--
-- The front end is xterm.js (loaded as a global by 'IDE.Web.Main', the same
-- way codemirror is).  The back end is a real pseudo-terminal running the
-- user's shell, spawned locally with @posix-pty@: the web executables run on
-- the same host as the browser, so we can bridge the PTY to xterm.js over
-- jsaddle:
--
--   * xterm @onData@ (keystrokes)  -> 'writePty'  (to the shell)
--   * a reader thread 'readPty'    -> reflex event -> @term.write@ (to screen)
--
-- All JS calls stay on the reflex event loop; only the blocking PTY read runs
-- on its own thread, handing bytes back through a trigger event.
module IDE.Web.Widget.Terminal
  ( terminalCss
  , terminalWidget
  , listTerminalSessions
  , killTerminalSession
  , TmuxWindow(..)
  , TmuxPane(..)
  , listTerminalTree
  , listRemoteTerminalTree
  , remoteTabTree
  , remoteTabHostTarget
  , createRemoteSession
  , selectRemoteTmuxWindow
  , selectRemoteTmuxPane
  , reapControlClients
  , createTerminalSession
  , openFileInEditor
  , replSessionName
  , ffcabalTmuxEnv
  , findReplWindow
  , selectTmuxWindowById
  , selectTmuxWindow
  , selectTmuxPane
  , killTmuxWindow
  , killTmuxPane
  , newTmuxWindow
  , zoomTmuxPane
  , breakTmuxPane
  , renameTmuxSession
  , renameTmuxWindow
  , activePaneId
  , paneGeometry
  , sessionOfPane
  , notifyTerminalBell
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (try, catch, SomeException)
import Control.Lens ((^.))
import Control.Monad (void, forM_, when)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.List (find, intercalate, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M
       (empty, singleton, fromListWith, unionWith, toAscList, toList, map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
       (unpack, pack, splitOn, stripPrefix, intercalate, strip, words, lines,
        null, breakOn, drop)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.Read (readMaybe)

import Clay (height, width, pct, (?), (-:), Css)

import Language.Javascript.JSaddle
       (jsg, js, jss, js0, js1, js2, js3, fun, new, valToText, valToNumber,
        valToBool, liftJSM)

import IDE.Core.CTypes (SrcSpan(..))
import IDE.Core.State (IDE)
import IDE.Web.Widget.Menu (menu)
import IDE.Web.Widget.Metadata (lookupIdentLocations)

import Reflex
       (attach, attachWith, current, ffor, getPostBuild, holdDyn, never,
        leftmost, constDyn, fmapMaybe, switchHold, performEvent, performEvent_,
        newTriggerEvent, delay, Dynamic, Event)
import Reflex.Dom.Core
       (elAttr, elAttr', dyn, resizeDetectorWithAttrs, MonadWidget, (=:),
        _element_raw)

import System.Directory
       (findExecutable, getHomeDirectory,
        createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv, getEnvironment)
import System.FilePath ((</>), takeDirectory)
import System.Posix.Pty
       (spawnWithPty, readPty, writePty, resizePty, threadWaitReadPty)
import System.Posix.Signals (signalProcess, sigKILL)
import System.Process (readProcessWithExitCode, createProcess, proc)
import System.Exit (ExitCode(ExitSuccess))

import IDE.Web.Events (TerminalEvents(..))
import IDE.Web.ReplTmux
       (tmuxSocket, tmuxCmd, replSessionName, ffcabalTmuxEnv, findReplWindow,
        selectTmuxWindowById, getLoginShell, writeTmuxConf, clipboardCopyCmd)
import IDE.Web.TerminalInput (registerTerminalPty, unregisterTerminalPty)
import IDE.Web.SnapRequest (requestSnapPane)

terminalCss :: Css
terminalCss = do
    ".terminal" ? do
        height (pct 100)
        width (pct 100)
    -- tmux turns on xterm's mouse mode, which makes xterm switch the cursor to
    -- the default arrow (.xterm.enable-mouse-events).  We want the usual text
    -- (I-beam) cursor over the terminal, so override it back (our stylesheet is
    -- concatenated after xterm.css, so this equal-specificity rule wins).
    ".xterm.enable-mouse-events" ? ("cursor" -: "text")
    -- ...but keep the hand cursor when hovering a clickable link (the terminal
    -- file links): xterm toggles .xterm-cursor-pointer on the same element, so
    -- re-assert it *after* the rule above or our text cursor would mask it.
    ".xterm.xterm-cursor-pointer" ? ("cursor" -: "pointer")
    ".xterm .xterm-cursor-pointer" ? ("cursor" -: "pointer")
    -- CC pane dividers: the whole tmux separator gutter is the grab strip for
    -- drag-to-resize; the visible 1px line (.divider-line) sits centered in it
    -- and brightens on hover/drag.  Geometry is inline (per-layout).
    ".terminal-cc-divider.vert" ? ("cursor" -: "col-resize")
    ".terminal-cc-divider.horiz" ? ("cursor" -: "row-resize")
    ".terminal-cc-divider .divider-line" ?
        ("background" -: "rgb(128,128,128)")
    ".terminal-cc-divider:hover .divider-line" ?
        ("background" -: "rgba(190,190,190,0.9)")
    ".terminal-cc-divider.dragging .divider-line" ?
        ("background" -: "rgba(190,190,190,0.9)")
    -- The active pane's marker: a transparent box exactly over the pane with
    -- a mid-grey shadow around it (shown/hidden by applyPaneHighlight).
    ".terminal-cc-hl" ?
        ("box-shadow" -: "0 0 8px 2px rgba(128,128,128,0.9)")

-- | A terminal pane.  The 'Int' is the terminal's id; it maps to a tmux
-- session named @leksah-N@ so the shell survives a leksah restart (see
-- 'listTerminalSessions').  The 'Event' fires whenever this terminal's tab is
-- selected; the terminal grabs keyboard focus then (and on creation), so it
-- takes input without an extra click.
terminalWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE          -- ^ for Ctrl/Cmd-click identifier lookup in metadata
  -> Text -> Event t () -> m (Event t TerminalEvents)
terminalWidget ide termId selectedE = do
  -- A real PTY running the user's shell.  Created up front so the xterm
  -- `onData` callback (wired below) can write keystrokes to it.
  pty <- liftIO $ do
      shell <- getLoginShell
      -- Inherit the environment but force a sensible TERM (without it the shell
      -- can't bind the arrow-key sequences).
      baseEnv <- getEnvironment
      let env = ("TERM", "xterm-256color") : filter ((/= "TERM") . fst) baseEnv
      -- Attach to the tmux session by its stable id (@termId@, e.g. "$3"); the
      -- session was created up front (see 'createTerminalSession') or already
      -- existed.  Attaching by id (not name) means a rename doesn't break the
      -- attach.  The reader thread sees tmux's redraw on attach.  Without tmux on
      -- PATH, run the shell directly (`-i` for the line editor) — no persistence.
      mbTmux <- findExecutable "tmux"
      (cmd, args) <- case mbTmux of
          Just tmux -> return (tmux, ["-L", tmuxSocket, "attach-session", "-t", T.unpack termId])
          Nothing -> return (shell, ["-i"])
      (pty, _ph) <- spawnWithPty (Just env) True cmd args (80, 24)
      -- Expose this PTY so the Tmux menu can inject `C-b X` prefix sequences into
      -- it when this terminal is the active one (see IDE.Web.TerminalInput).
      registerTerminalPty termId pty
      -- The vim-style pane bindings live in the tmux config, but `-f` only takes
      -- effect when the server first starts; a server left running by a previous
      -- leksah session keeps the old bindings.  Re-assert them here (idempotent)
      -- so they work without having to kill every terminal first.
      forM_ mbTmux $ \_ -> do
          mapM_ (\(k, d) -> tmuxCmd ["bind-key", k, "select-pane", d])
              [("h", "-L"), ("j", "-D"), ("k", "-U"), ("l", "-R")]
          mapM_ (\(k, d) -> tmuxCmd ["bind-key", "-r", k, "resize-pane", d, "5"])
              [("H", "-L"), ("J", "-D"), ("K", "-U"), ("L", "-R")]
          mapM_ (\(k, d) -> tmuxCmd ["bind-key", "-r", k, "resize-pane", d, "1"])
              [("C-h", "-L"), ("C-j", "-D"), ("C-k", "-U"), ("C-l", "-R")]
          tmuxCmd ["bind-key", "Tab", "last-window"]
          tmuxCmd ["bind-key", "BTab", "switch-client", "-l"]
          clipboardCopyCmd >>= mapM_ (\c -> tmuxCmd ["set", "-s", "copy-command", c])
          -- Re-assert focus reporting on the running server too (the -f config
          -- above only takes effect when the server first starts, so a server
          -- left over from before this setting existed wouldn't have it).
          tmuxCmd ["set", "-g", "focus-events", "on"]
          tmuxCmd ["set", "-g", "allow-passthrough", "on"]
          tmuxCmd ["set", "-g", "monitor-activity", "on"]
          tmuxCmd ["set", "-g", "monitor-silence", "15"]
          tmuxCmd ["set", "-g", "visual-activity", "off"]
          tmuxCmd ["set", "-g", "visual-silence", "off"]
          tmuxCmd ["set", "-g", "visual-bell", "off"]
          -- terminal-features is read when a client attaches; setting it here
          -- takes effect on the next attach (relaunch), not this one.
          tmuxCmd ["set", "-sa", "terminal-features", ",xterm-256color:RGB:hyperlinks"]
          -- Post a macOS notification when any window rings the bell (Claude Code
          -- does this when a teammate wants input / finishes).  The hook passes
          -- the belling window's ids to the helper script (run in the background
          -- so it never blocks tmux).
          notifyPath <- writeNotifyScript
          tmuxCmd [ "set-hook", "-g", "alert-bell"
                  , "run-shell -b \"sh " <> notifyPath <> " '#{session_id}' '#{window_index}'\"" ]
          -- Poke leksah whenever the current window/pane changes by *any* route
          -- (⌃B n/p, ⌃B w chooser, mouse, scripts), so the flipper/tab MRU and the
          -- highlight update at once instead of waiting for the 2 s poll.  The poke
          -- re-reads the tree; leksah then floats the focused terminal's new active
          -- pane to the MRU front (via activeFlipD).  Uses leksah-cmd's absolute
          -- path (it's on PATH in the dev shell); skipped if not found.
          -- Redirect the poke's output: tmux's run-shell surfaces a command's
          -- stdout (as an "ok" view on every select otherwise).
          findExecutable "leksah-cmd" >>= mapM_ (\lc ->
            mapM_ (\ev -> tmuxCmd ["set-hook", "-g", ev, "run-shell -b \"" <> lc <> " term-activity >/dev/null 2>&1\""])
                  ["after-select-window", "after-select-pane"])
      return pty

  -- Output from the shell arrives on this trigger event from the reader
  -- thread (started once the terminal exists, so the prompt isn't dropped).
  (outputE, triggerOutput) <- newTriggerEvent
  -- The terminal's window title (set by the shell/programs via OSC sequences),
  -- surfaced so the Terminals list pane can label this terminal.
  (titleE, triggerTitle) <- newTriggerEvent
  -- Ctrl+clicking a project-file path in the terminal output asks to open it at
  -- the given line/column (the link provider, wired below, fires this).
  (linkE, triggerLink) <- newTriggerEvent
  -- Ctrl/Cmd+clicking any identifier-like token instead asks to look it up in
  -- the metadata; this fires (token, clientX, clientY) for the click.
  (lookupE, triggerLookup) <- newTriggerEvent
  -- The shell rang the bell (xterm's onBell).  Since a client is attached
  -- viewing this session's current window, tmux's alert-bell hook won't fire for
  -- a bell there — so we catch it here and let leksah surface the attention.
  (bellE, triggerBell) <- newTriggerEvent
  -- The attached tmux client exited (its PTY hit EOF): the session ended — e.g.
  -- the last window's shell was `exit`ed — so the tab should close instead of
  -- lingering with a dead "[exited]" screen.
  (exitedE, triggerExited) <- newTriggerEvent

  (resizeE, el) <- resizeDetectorWithAttrs ("style" =: "height:100%;width:100%") $
      fst <$> elAttr' "div" ("class" =: "terminal") (pure ())
  let rawEl = _element_raw el

  postBuild <- getPostBuild
  -- Build the xterm.js terminal, attach it to our div, wire input, then start
  -- streaming the shell's output into it.
  termE <- performEvent $ ffor postBuild $ \_ -> liftJSM $ do
      term <- new (jsg ("Terminal" :: Text)) ()
      -- Register this terminal so the reader thread can route output to it by id
      -- (see window.LeksahTerm in IDE.Web.Main).
      _ <- jsg ("LeksahTerm" :: Text) ^. js2 ("register" :: Text) termId term
      -- Pin an explicit monospace font/size *before* opening: xterm measures
      -- the character cell from the configured font, and without this it
      -- inherits the page's proportional `body` font, making cells wider than
      -- the glyphs (visible gaps between characters).
      opts <- term ^. js ("options" :: Text)
      _ <- opts ^. jss ("fontFamily" :: Text) ("Menlo, Monaco, \"Courier New\", monospace" :: Text)
      _ <- opts ^. jss ("fontSize" :: Text) (13 :: Int)
      -- The SearchAddon highlights matches via xterm's *proposed* decorations
      -- API, which throws ("allowProposedApi") unless this is enabled.  Must be
      -- set before the find bar drives a search (it is — before loadAddon below).
      _ <- opts ^. jss ("allowProposedApi" :: Text) True
      -- Unicode 11 widths (xterm defaults to Unicode 6, where emoji are
      -- width 1 — tmux and modern apps assume 2, so ✅ etc. misalign).
      uni <- new (jsg ("Unicode11Addon" :: Text) ^. js ("Unicode11Addon" :: Text)) ()
      _ <- term ^. js1 ("loadAddon" :: Text) uni
      unicodeApi <- term ^. js ("unicode" :: Text)
      _ <- unicodeApi ^. jss ("activeVersion" :: Text) ("11" :: Text)
      -- Handle OSC 8 hyperlinks (forwarded by tmux): hover shows the URL.  Clicking
      -- an http(s) link opens it in the browser — snapping the browser over this
      -- terminal's active pane only when Command was held; a file:// link opens in
      -- a CodeMirror editor instead (via the same path as a clicked file token).
      handler <- jsg ("LeksahOscLinks" :: Text) ^. js2 ("makeHandler" :: Text)
          (fun $ \_ _ as -> case as of
              (u:snapV:_) -> do
                  url  <- valToText u
                  snap <- valToBool snapV
                  liftIO $ do
                      _ <- (try (void $ createProcess (proc "open" [T.unpack url]))
                              :: IO (Either SomeException ()))
                      when snap $ activePaneId termId >>= mapM_ requestSnapPane
              _ -> return ())
          (fun $ \_ _ as -> case as of
              (pV:lV:cV:_) -> do
                  path <- valToText pV
                  ln   <- valToNumber lV
                  col  <- valToNumber cV
                  liftIO $ triggerLink (T.unpack path, max 1 (round ln), max 1 (round col))
              _ -> return ())
      _ <- opts ^. jss ("linkHandler" :: Text) handler
      fit  <- new (jsg ("FitAddon" :: Text) ^. js ("FitAddon" :: Text)) ()
      _ <- term ^. js1 ("loadAddon" :: Text) fit
      _ <- term ^. js1 ("open" :: Text) rawEl
      -- GPU renderer: xterm's default DOM renderer rounds the character cell up
      -- to whole CSS pixels, so on HiDPI (retina) displays glyphs don't fill the
      -- cell and look too widely spaced.  The WebGL renderer draws from a texture
      -- atlas with correct device-pixel scaling, fixing the spacing.  It must be
      -- loaded after open() (it needs the terminal's screen element).
      webgl <- new (jsg ("WebglAddon" :: Text) ^. js ("WebglAddon" :: Text)) ()
      _ <- term ^. js1 ("loadAddon" :: Text) webgl
      -- xterm's SearchAddon, registered on the terminal element so the find bar
      -- can search this pane (terminals render to a canvas, so no DOM find).
      _ <- jsg ("LeksahCM" :: Text) ^. js2 ("loadTerminalSearch" :: Text) term rawEl
      -- Make tokens in the output clickable.  Without a modifier, project-file
      -- paths (validated against the workspace file set kept in JS via
      -- LeksahTermLinks.setProjectFiles) call back with the resolved absolute
      -- path + line/column.  With Ctrl/Cmd held, any identifier is clickable and
      -- calls back with the token + click position for a metadata lookup.
      _ <- jsg ("LeksahTermLinks" :: Text) ^. js3 ("attach" :: Text) term
              (fun $ \_ _ as -> case as of
                  (p:l:c:_) -> do
                      path <- valToText p
                      ln   <- valToNumber l
                      col  <- valToNumber c
                      liftIO $ triggerLink (T.unpack path, round ln :: Int, round col :: Int)
                  _ -> return ())
              (fun $ \_ _ as -> case as of
                  (t:x:y:_) -> do
                      tok <- valToText t
                      cx  <- valToNumber x
                      cy  <- valToNumber y
                      liftIO $ triggerLookup (tok, round cx :: Int, round cy :: Int)
                  _ -> return ())
      _ <- fit ^. js0 ("fit" :: Text)
      -- Re-fit on *any* size change of the terminal element: window resize and
      -- layout changes alike (e.g. showing/hiding the side pane, which resizes
      -- the editor column).  The scroll-based resize detector misses grid track
      -- changes; ResizeObserver catches them.
      ro <- new (jsg ("ResizeObserver" :: Text)) (fun $ \_ _ _ -> do
          _ <- fit ^. js0 ("fit" :: Text)
          c <- valToNumber =<< term ^. js ("cols" :: Text)
          r <- valToNumber =<< term ^. js ("rows" :: Text)
          liftIO $ ignorePtyError (resizePty pty (round c, round r)))
      _ <- ro ^. js1 ("observe" :: Text) rawEl
      -- keystrokes -> shell
      _ <- term ^. js1 ("onData" :: Text) (fun $ \_ _ args -> case args of
              (d:_) -> do
                  s <- valToText d
                  liftIO $ ignorePtyError (writePty pty (encodeUtf8 s))
              _ -> return ())
      -- title changes -> Terminals list
      _ <- term ^. js1 ("onTitleChange" :: Text) (fun $ \_ _ args -> case args of
              (titleVal:_) -> valToText titleVal >>= liftIO . triggerTitle
              _ -> return ())
      -- bell (Claude Code's needs-input signal) -> leksah attention
      _ <- term ^. js1 ("onBell" :: Text) (fun $ \_ _ _ -> liftIO (triggerBell ()))
      syncPtySize term fit pty
      -- shell -> screen: blocking reads on their own thread.  Each chunk is
      -- handed to xterm as raw bytes (see the output write below) rather than
      -- decoded here.  We don't coalesce reads: tmux (which backs these
      -- terminals) is a screen-diff multiplexer that already rate-limits its
      -- output, so the PTY delivers a thin, paced stream with little to batch —
      -- measured coalescing through tmux was near-neutral, so it isn't worth the
      -- added end-of-burst latency.
      _ <- liftIO . forkIO $
          let loop = (try (threadWaitReadPty pty >> readPty pty) :: IO (Either SomeException ByteString)) >>= \case
                  Right bs -> triggerOutput bs >> loop
                  -- EOF/read error: the attached tmux client is gone (the session
                  -- ended).  Tell reflex so the tab closes rather than lingering.
                  Left _   -> triggerExited ()
          in loop
      return (term, fit)

  termFitD <- holdDyn Nothing (Just <$> termE)

  -- Grab keyboard focus when the terminal is created (it's opened visible) and
  -- whenever its tab is later selected.  `delay 0` lets `termFitD` catch the
  -- freshly-created terminal; the focus itself is done inside
  -- `requestAnimationFrame` so it runs after the tab's `visibility` has been
  -- applied/laid out — focusing a still-hidden element is a silent no-op, which
  -- is why selecting an already-open (previously hidden) terminal didn't work.
  focusE <- delay 0 $ leftmost [ () <$ termE, selectedE ]
  performEvent_ $ ffor (attach (current termFitD) focusE) $ \case
      (Just (term, _), ()) -> liftJSM . void $
          jsg ("window" :: Text) ^. js1 ("requestAnimationFrame" :: Text)
              (fun $ \_ _ _ -> void $ term ^. js0 ("focus" :: Text))
      _ -> return ()

  -- Write each batch of shell output to the terminal.  Gated on the terminal
  -- existing (termFitD becomes Just only once the build action — including the
  -- LeksahTerm.register above — has run), then routed to it by id as raw bytes.
  performEvent_ $ ffor (attach (current termFitD) outputE) $ \case
      (Just _, bs) ->
          liftJSM . void $ jsg ("LeksahTerm" :: Text)
              ^. js2 ("write" :: Text) termId (decodeUtf8 (B64.encode bs))
      _ -> return ()

  -- Keep xterm and the PTY in step with the pane size.
  performEvent_ $ ffor (attach (current termFitD) (() <$ resizeE)) $ \case
      (Just (term, fit), ()) -> liftJSM $ do
          _ <- fit ^. js0 ("fit" :: Text)
          syncPtySize term fit pty
      _ -> return ()

  -- Re-fit shortly after creation (and whenever the tab is re-shown), so the
  -- terminal corrects to the pane's final width even if its first fit ran
  -- before the layout settled.  leksah-wkwebview applies its full-size title bar
  -- asynchronously at start-up, which relays out the panes after this terminal's
  -- initial fit — without this the screen stays stuck at that early width.
  refitE <- delay 0.3 $ leftmost [ () <$ termE, selectedE ]
  performEvent_ $ ffor (attach (current termFitD) refitE) $ \case
      (Just (term, fit), ()) -> liftJSM $ do
          _ <- fit ^. js0 ("fit" :: Text)
          syncPtySize term fit pty
      _ -> return ()

  -- Navigation from a clicked file path.
  let fileGotoE = (\(f, l, c) -> SrcSpan f l c l c) <$> linkE
  -- Navigation from a Ctrl/Cmd-clicked identifier: look it up in the metadata.
  -- No match -> nothing; one match -> jump straight there; several -> pop up a
  -- chooser of module names at the click position and jump to the picked one.
  let optsE = attachWith (\i (tok, x, y) -> (lookupIdentLocations tok i, x, y))
                (current ide) lookupE
      singleGotoE = fmapMaybe (\(opts, _, _) -> case opts of [(_, sp)] -> Just sp; _ -> Nothing) optsE
      multiE      = fmapMaybe (\(opts, x, y) -> if length opts > 1 then Just (x, y, opts) else Nothing) optsE
  rec chooserD <- holdDyn Nothing $ leftmost [ Just <$> multiE, Nothing <$ chosenE ]
      chosenE <- switchHold never =<< dyn (ffor chooserD $ \case
        Nothing           -> return never
        Just (x, y, opts) ->
          elAttr "div" ("class" =: "context-menu"
              <> "style" =: T.pack ("position:fixed;left:" <> show x <> "px;top:" <> show y <> "px")) $
            menu [ constDyn (lbl, sp) | (lbl, sp) <- opts ])
  let gotoE = TerminalGoto <$> leftmost [ fileGotoE, singleGotoE, chosenE ]
  return $ leftmost [ TerminalTitle <$> titleE, gotoE, TerminalBell <$ bellE
                    , TerminalExited <$ exitedE ]
  where
    -- Match the PTY's window size to xterm's current cols/rows so full-screen
    -- programs (vim, htop, …) lay out correctly.
    syncPtySize term _fit pty = do
        cols <- valToNumber =<< term ^. js ("cols" :: Text)
        rows <- valToNumber =<< term ^. js ("rows" :: Text)
        liftIO $ ignorePtyError (resizePty pty (round cols, round rows))

-- | Run a PTY write/resize, swallowing errors.  Once a terminal's shell exits
-- (e.g. the user typed @exit@) its tmux session/window can be gone and the PTY
-- dead, so a write/resize raises @fdWriteBuf: Input/output error@.  That's just
-- one dead terminal — it must never take down the whole UI (which it did: the
-- exception was uncaught and locked up leksah).
ignorePtyError :: IO () -> IO ()
ignorePtyError act = act `catch` \(_ :: SomeException) -> return ()

-- | Create a fresh leksah tmux session (detached) named @name@, applying the
-- leksah tmux config, and return its stable tmux session id (e.g. "$3").  The
-- terminal widget then attaches to that id.  'Nothing' if tmux is absent or the
-- command fails.
createTerminalSession :: Text -> IO (Maybe Text)
createTerminalSession name = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            shell <- getLoginShell
            conf <- writeTmuxConf shell
            (_rc, out, _) <- readProcessWithExitCode tmux
                [ "-L", tmuxSocket, "-f", conf, "new-session", "-d"
                , "-s", T.unpack name, "-P", "-F", "#{session_id}" ] ""
            return . listToMaybe . filter (not . T.null) . map T.strip . T.lines $ T.pack out

-- | Open a file in the external editor: run @argv@ (e.g. @["vim","+12","/f.hs"]@)
-- as a new *window* in the shared @leksah-editor@ tmux session — created on the
-- first open, reused after — so every externally-opened file is a window-tab
-- (named @winName@, the file's basename) under one Terminals-tree node.  Returns
-- the @leksah-editor@ session id (the terminal tab is keyed by it).  When the last
-- window is @:q@'d the session ends; the next open recreates it.
openFileInEditor :: String -> [String] -> IO (Maybe Text)
openFileInEditor winName argv = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            shell <- getLoginShell
            conf  <- writeTmuxConf shell
            let base = ["-L", tmuxSocket, "-f", conf]
            -- Add a window to leksah-editor if it exists, else create the session.
            (hasRc, _, _) <- readProcessWithExitCode tmux
                (base ++ ["has-session", "-t", "=leksah-editor"]) ""
            let mk = if hasRc == ExitSuccess
                       then ["new-window", "-t", "=leksah-editor"]
                       else ["new-session", "-d", "-s", "leksah-editor"]
            (_rc, out, _) <- readProcessWithExitCode tmux
                (base ++ mk ++ ["-n", winName, "-P", "-F", "#{session_id}"] ++ argv) ""
            return . listToMaybe . filter (not . T.null) . map T.strip . T.lines $ T.pack out

-- | Write (idempotently) a tiny helper that posts a macOS Notification Center
-- notification for a tmux bell alert, and return its path.  Driven by the
-- @alert-bell@ hook (set in 'terminalWidget'): the hook passes the belling
-- window's session id + window index; the script looks their names up and shows
-- the notification via @osascript@ — which works from any process (leksah runs
-- as a bare binary, not a .app bundle, so the native UNUserNotification API
-- isn't available).  Bell is Claude Code's "needs input / done" signal, so this
-- tells you a teammate wants you without watching the window.
-- | The notify script lives at a *stable* path under @~/.leksah@ — not in
-- @$TMPDIR@, which under nix-shell is a per-invocation dir that gets cleaned up
-- (leaving the @alert-bell@ hook pointing at a vanished file, and littering
-- @/tmp@ with one script per launch).
notifyScriptPath :: IO FilePath
notifyScriptPath = (</> ".leksah" </> "leksah-notify.sh") <$> getHomeDirectory

writeNotifyScript :: IO FilePath
writeNotifyScript = do
    path <- notifyScriptPath
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path $ unlines
        [ "#!/bin/sh"
        , "# $1 = tmux session id (e.g. $3), $2 = window index (optional — empty"
        , "# means the session's current window).  Written by leksah."
        , "target=\"$1\"; [ -n \"$2\" ] && target=\"$1:$2\""
        , "label=$(tmux -L " <> tmuxSocket <> " display-message -p -t \"$target\" '#{session_name}: #{window_name}' 2>/dev/null)"
        , "[ -z \"$label\" ] && label=\"$target\""
        -- Pass the label as an argv item (not string-interpolated) so names with
        -- quotes can't break the AppleScript.
        , "osascript - \"$label\" >/dev/null 2>&1 <<'OSA' || true"
        , "on run argv"
        , "  display notification (item 1 of argv) with title \"leksah terminal\""
        , "end run"
        , "OSA"
        ]
    return path

-- | Post the bell notification for session @sid@'s current window from leksah
-- (via the same script the tmux hook uses).  Needed because tmux's alert-bell
-- hook does NOT fire for a bell in the window a client is viewing — which is
-- every open terminal's current window — so leksah catches those bells itself
-- (xterm @onBell@) and calls this.
notifyTerminalBell :: Text -> IO ()
notifyTerminalBell sid = (`catch` \(_ :: SomeException) -> return ()) $ do
    path <- notifyScriptPath
    exists <- doesFileExist path
    ensured <- if exists then return path else writeNotifyScript
    void $ readProcessWithExitCode "sh" [ensured, T.unpack sid, ""] ""


-- | All currently-live tmux sessions on leksah's socket as @(session id, session
-- name)@ pairs — every session, not just leksah's own, so the Terminals list can
-- show them all.  Empty if tmux is absent or no server is running.
listTerminalSessions :: IO [(Text, Text)]
listTerminalSessions = (`catch` \(_ :: SomeException) -> return []) $
    findExecutable "tmux" >>= \case
        Nothing -> return []
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-sessions", "-F", "#{session_id}\t#{session_name}"] ""
            return [ (sid, T.intercalate "\t" rest)
                   | line <- lines out
                   , (sid:rest) <- [T.splitOn "\t" (T.pack line)]
                   , not (T.null sid) ]

-- | Kill the tmux session with id @n@ (so it no longer persists).
killTerminalSession :: Text -> IO ()
killTerminalSession n = (`catch` \(_ :: SomeException) -> return ()) $ do
    unregisterTerminalPty n
    findExecutable "tmux" >>= \case
        Nothing -> return ()
        Just tmux -> void $ readProcessWithExitCode tmux
            ["-L", tmuxSocket, "kill-session", "-t", T.unpack n] ""

-- | A tmux pane within a window: its index, a display label (its index and the
-- command running in it), and whether it is the window's active pane.
data TmuxPane = TmuxPane
  { tpIndex  :: Int
  , tpLabel  :: Text
  , tpActive :: Bool
  } deriving (Eq, Show)

-- | A tmux window within a session: its index, a display label (its index and
-- name), whether it is the session's active window, its tmux alert flags (bell /
-- activity / silence — surfaced in the Terminals tree so you can see which
-- teammate rang the bell, is producing output, or has gone quiet), and its panes.
data TmuxWindow = TmuxWindow
  { twIndex    :: Int
  , twLabel    :: Text
  , twActive   :: Bool
  , twBell     :: Bool
  , twActivity :: Bool
  , twSilence  :: Bool
  , twPanes    :: [TmuxPane]
  } deriving (Eq, Show)

-- | The tmux window/pane hierarchy of every live session, keyed by tmux session
-- id (e.g. "$3") and carrying that session's current name — the whole Terminals
-- tree in one @list-panes -a@ call.  Keying by the stable id (not the name) means
-- a rename just changes the carried name on the next poll, not the key.  Includes
-- all sessions, not only leksah's own.  Empty if tmux is absent / no server.
listTerminalTree :: IO (Map Text (Text, [TmuxWindow]))
listTerminalTree = (`catch` \(_ :: SomeException) -> return M.empty) $
    findExecutable "tmux" >>= \case
        Nothing -> return M.empty
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-panes", "-a", "-F", paneTreeFormat] ""
            return (parsePaneTree out)

-- | Tab-separated so names / commands / titles (which won't contain tabs) stay
-- intact: session id/name, window index/name/active, pane
-- index/active/command/title.  pane_title is the per-pane title (what ⌃B w
-- shows) — used as the pane's display name so panes don't all share the
-- terminal's (active-pane) OSC title; command is the fallback when it's empty.
paneTreeFormat :: String
paneTreeFormat = intercalate "\t"
    [ "#{session_id}", "#{session_name}", "#{window_index}", "#{window_name}"
    , "#{window_active}", "#{window_bell_flag}", "#{window_activity_flag}"
    , "#{window_silence_flag}", "#{pane_index}", "#{pane_active}"
    , "#{pane_current_command}", "#{pane_title}" ]

-- | Run tmux on a remote host over ssh (no PTY, BatchMode — key auth only).
-- 'Nothing' when ssh or the remote tmux fails (host down, no server, …).
--
-- Each argument is single-quoted FOR THE REMOTE SHELL: ssh joins the remote
-- command's words with spaces and hands them to the login shell, so an
-- unquoted tmux format like @#{session_id}@ starts a shell COMMENT at the
-- @#@ — tmux saw @-F@ with no value, failed, and the whole host showed as
-- \"(unreachable)\" even though ssh was fine.
sshTmux :: Text -> [String] -> IO (Maybe String)
sshTmux host args = (`catch` \(_ :: SomeException) -> return Nothing) $ do
    (c, out, _) <- readProcessWithExitCode "ssh"
        ([ "-o", "BatchMode=yes", "-o", "ConnectTimeout=5", T.unpack host
         , unwords ("tmux" : map shellQuote args) ]) ""
    return $ if c == ExitSuccess then Just out else Nothing
  where
    shellQuote s = "'" <> concatMap esc s <> "'"
    esc '\'' = "'\\''"
    esc c    = [c]

-- | The session/window/pane tree of a remote host's (default-socket) tmux —
-- same shape as 'listTerminalTree'.  'Nothing' = unreachable / no server.
listRemoteTerminalTree :: Text -> IO (Maybe (Map Text (Text, [TmuxWindow])))
listRemoteTerminalTree host =
    fmap parsePaneTree <$> sshTmux host ["list-panes", "-a", "-F", paneTreeFormat]

-- | The @(host, target)@ behind an @ssh://host[#target]@ terminal tab key
-- ('Nothing' for local keys).  An empty target means the default session
-- name @leksah@ (what a plain @cc-connect HOST@ attaches).
remoteTabHostTarget :: Text -> Maybe (Text, Text)
remoteTabHostTarget n = do
    rest <- T.stripPrefix "ssh://" n
    let (host, hash) = T.breakOn "#" rest
        target0 = T.drop 1 hash
    return (host, if T.null target0 then "leksah" else target0)

-- | The display label and windows of the remote session behind an
-- @ssh://host#target@ tab key — the remote analogue of one
-- 'listTerminalTree' entry, so remote windows/panes can join the flipper.
remoteTabTree :: Text -> IO (Maybe (Text, [TmuxWindow]))
remoteTabTree n = case remoteTabHostTarget n of
    Nothing -> return Nothing
    Just (host, target) -> do
        mb <- listRemoteTerminalTree host
        return $ do
            tree <- mb
            (_, (nm, ws)) <- find (\(sid, (nm', _)) -> nm' == target || sid == target)
                                  (M.toList tree)
            return (host <> " · " <> nm, ws)

-- | Create a detached session on a remote host's tmux; returns its session id.
createRemoteSession :: Text -> IO (Maybe Text)
createRemoteSession host =
    (>>= (listToMaybe . filter (not . T.null) . map T.strip . T.lines . T.pack))
        <$> sshTmux host ["new-session", "-d", "-P", "-F", "#{session_id}"]

selectRemoteTmuxWindow :: Text -> Text -> Int -> IO ()
selectRemoteTmuxWindow host s w =
    void $ sshTmux host ["select-window", "-t", T.unpack s <> ":" <> show w]

selectRemoteTmuxPane :: Text -> Text -> Int -> Int -> IO ()
selectRemoteTmuxPane host s w p = do
    selectRemoteTmuxWindow host s w
    void $ sshTmux host ["select-pane", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Detach tmux control-mode clients left over from previous leksah runs.
-- leksah's own @tmux -C@ child processes don't die when leksah exits — they
-- wedge writing to the closed pipe — and tmux keeps counting them as attached
-- clients; the never-resized ones clamp their sessions' windows to 80x24,
-- wrecking rendering for the live clients.  Run at startup, before any
-- terminal widget attaches (leksah has no control clients of its own yet, so
-- everything control-mode on the socket is stale).
reapControlClients :: IO ()
reapControlClients = (`catch` \(_ :: SomeException) -> return ()) $
    findExecutable "tmux" >>= \case
        Nothing -> return ()
        Just tmux -> do
            (_, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-clients", "-F"
                , "#{client_name}\t#{client_pid}\t#{client_control_mode}"] ""
            forM_ (lines out) $ \l -> case splitOn '\t' l of
                [name, pid, "1"] -> do
                    -- KILL the client process, don't just detach: a wedged
                    -- client's pipe is full, so tmux can't flush its buffer
                    -- and a plain detach-client silently never completes —
                    -- the zombie stays "attached", and once IT falls behind,
                    -- tmux stops reading the pane's pty, periodically
                    -- freezing the program inside (seen as the whole TUI
                    -- pausing every few seconds while it streams).
                    forM_ (readMaybe pid :: Maybe Int) $ \p ->
                        signalProcess sigKILL (fromIntegral p)
                            `catch` \(_ :: SomeException) -> return ()
                    void $ readProcessWithExitCode tmux
                        ["-L", tmuxSocket, "detach-client", "-t", name] ""
                _ -> return ()
  where
    splitOn c s = case break (== c) s of
        (a, _ : rest) -> a : splitOn c rest
        (a, [])       -> [a]

-- | Parse the @list-panes -a@ output into the per-session (id -> (name, windows))
-- tree, grouping by session then window (ascending by index within each level).
parsePaneTree :: String -> Map Text (Text, [TmuxWindow])
parsePaneTree out = M.map toSession grouped
  where
    rows =
      [ (sid, sname, wi, wn, wa == "1", wb == "1", wac == "1", ws == "1", pidx, pa == "1", paneName)
      | line <- lines out
      , (sid:sname:wiT:wn:wa:wb:wac:ws:piT:pa:cmd:rest) <- [T.splitOn "\t" (T.pack line)]
      , not (T.null sid)
      , Just wi   <- [readMaybe (T.unpack wiT)]
      , Just pidx <- [readMaybe (T.unpack piT)]
      , let title    = T.intercalate "\t" rest
            paneName = if T.null title then cmd else title ]
    -- session id -> (name, window index -> (name, active, bell, activity, silence, pane idx -> (paneName, active)))
    grouped :: Map Text (Text, Map Int (Text, Bool, Bool, Bool, Bool, Map Int (Text, Bool)))
    grouped = M.fromListWith mergeSess
      [ (sid, (sname, M.singleton wi (wn, wa, wb, wac, ws, M.singleton pidx (paneName, pa))))
      | (sid, sname, wi, wn, wa, wb, wac, ws, pidx, pa, paneName) <- rows ]
    mergeSess (sname, w1) (_, w2) = (sname, M.unionWith mergeWin w1 w2)
    mergeWin (wn, wa, wb, wac, ws, ps1) (_, _, _, _, _, ps2) = (wn, wa, wb, wac, ws, ps1 <> ps2)
    toSession (sname, wm) =
      ( sname
      , [ TmuxWindow wi (T.pack (show wi) <> ": " <> wn) wa wb wac ws
            [ TmuxPane pidx (T.pack (show pidx) <> ": " <> paneName) pa
            | (pidx, (paneName, pa)) <- M.toAscList ps ]
        | (wi, (wn, wa, wb, wac, ws, ps)) <- M.toAscList wm ] )

-- | Make window @w@ of session @s@ (a tmux session id) the current window.
selectTmuxWindow :: Text -> Int -> IO ()
selectTmuxWindow s w =
    tmuxCmd ["select-window", "-t", T.unpack s <> ":" <> show w]

-- | Make pane @p@ of window @w@ in session @s@ the active pane.  Also switch the
-- session to that window first: @select-pane@ only moves focus within a window,
-- so without this a pane in a non-current window wouldn't actually be shown.
selectTmuxPane :: Text -> Int -> Int -> IO ()
selectTmuxPane s w p = do
    selectTmuxWindow s w
    tmuxCmd ["select-pane", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Kill window @w@ of session @s@ (tmux closes the session if it was its last
-- window).
killTmuxWindow :: Text -> Int -> IO ()
killTmuxWindow s w =
    tmuxCmd ["kill-window", "-t", T.unpack s <> ":" <> show w]

-- | Kill pane @p@ of window @w@ in session @s@ (tmux closes the window if it was
-- its last pane).
killTmuxPane :: Text -> Int -> Int -> IO ()
killTmuxPane s w p =
    tmuxCmd ["kill-pane", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Create a new window in session @s@ (becomes that session's current window).
newTmuxWindow :: Text -> IO ()
newTmuxWindow s = tmuxCmd ["new-window", "-t", T.unpack s]

-- | Toggle zoom (fullscreen-within-its-window) for pane @p@ of window @w@.
zoomTmuxPane :: Text -> Int -> Int -> IO ()
zoomTmuxPane s w p =
    tmuxCmd ["resize-pane", "-Z", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Break pane @p@ of window @w@ out into its own new window (so it gets its own
-- activity/bell tracking and more room).
breakTmuxPane :: Text -> Int -> Int -> IO ()
breakTmuxPane s w p =
    tmuxCmd ["break-pane", "-t", T.unpack s <> ":" <> show w <> "." <> show p]

-- | Rename session @s@ (a session id) to @name@.
renameTmuxSession :: Text -> Text -> IO ()
renameTmuxSession s name = tmuxCmd ["rename-session", "-t", T.unpack s, T.unpack name]

-- | Rename window @w@ of session @s@ to @name@.
renameTmuxWindow :: Text -> Int -> Text -> IO ()
renameTmuxWindow s w name =
    tmuxCmd ["rename-window", "-t", T.unpack s <> ":" <> show w, T.unpack name]

-- | The id (e.g. @%3@) of the active pane of session @n@ (a tmux session id),
-- used to pick which pane to make transparent (see "IDE.Web.Main").
activePaneId :: Text -> IO (Maybe Text)
activePaneId n = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "display-message", "-p", "-t", T.unpack n, "-F", "#{pane_id}"] ""
            return $ case filter (not . T.null) (map T.strip (T.lines (T.pack out))) of
                (p:_) -> Just p
                _     -> Nothing

-- | The cell rectangle @(left, top, width, height)@ of pane @pid@ in session @n@
-- (a tmux session id) — but only while that pane's window is the session's
-- current one, so a transparency hole is hidden when its pane isn't on screen.
paneGeometry :: Text -> Text -> IO (Maybe (Int, Int, Int, Int))
paneGeometry n pid = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-panes", "-t", T.unpack n, "-F",
                 "#{pane_id} #{pane_left} #{pane_top} #{pane_width} #{pane_height} #{window_active}"] ""
            return $ listToMaybe
                [ (l, t, w, h)
                | line <- T.lines (T.pack out)
                , (pid':lT:tT:wT:hT:waT:_) <- [T.words line]
                , pid' == pid, waT == "1"
                , Just l <- [readMaybe (T.unpack lT)]
                , Just t <- [readMaybe (T.unpack tT)]
                , Just w <- [readMaybe (T.unpack wT)]
                , Just h <- [readMaybe (T.unpack hT)] ]

-- | Which tmux session (by session id) the pane @pid@ (e.g. @%20@) belongs to.
-- Used by @open-browser@ to map @$TMUX_PANE@ to a terminal so its pane can be
-- snapped.
sessionOfPane :: Text -> IO (Maybe Text)
sessionOfPane pid = (`catch` \(_ :: SomeException) -> return Nothing) $
    findExecutable "tmux" >>= \case
        Nothing -> return Nothing
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-panes", "-a", "-F", "#{session_id} #{pane_id}"] ""
            return $ listToMaybe
                [ sid
                | line <- T.lines (T.pack out)
                , (sid:pid':_) <- [T.words line]
                , pid' == pid ]

