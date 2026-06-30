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
  , selectTmuxWindow
  , selectTmuxPane
  , killTmuxWindow
  , killTmuxPane
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (try, catch, SomeException)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.List (intercalate, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M (empty, singleton, fromListWith, unionWith, toAscList, map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack, splitOn, stripPrefix, intercalate)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.Read (readMaybe)

import Clay (height, width, pct, (?), (-:), Css)

import Language.Javascript.JSaddle
       (jsg, js, jss, js0, js1, js2, js3, fun, new, valToText, valToNumber, liftJSM)

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

import System.Directory (findExecutable, getTemporaryDirectory)
import System.Environment (lookupEnv, getEnvironment)
import System.FilePath ((</>))
import System.Posix.Pty
       (spawnWithPty, readPty, writePty, resizePty, threadWaitReadPty)
import System.Posix.User (getRealUserID, getUserEntryForID, userShell)
import System.Process (readProcessWithExitCode)

import IDE.Web.Events (TerminalEvents(..))
import IDE.Web.TerminalInput (registerTerminalPty, unregisterTerminalPty)

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

-- | A terminal pane.  The 'Int' is the terminal's id; it maps to a tmux
-- session named @leksah-N@ so the shell survives a leksah restart (see
-- 'listTerminalSessions').  The 'Event' fires whenever this terminal's tab is
-- selected; the terminal grabs keyboard focus then (and on creation), so it
-- takes input without an extra click.
terminalWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE          -- ^ for Ctrl/Cmd-click identifier lookup in metadata
  -> Int -> Event t () -> m (Event t TerminalEvents)
terminalWidget ide termId selectedE = do
  -- A real PTY running the user's shell.  Created up front so the xterm
  -- `onData` callback (wired below) can write keystrokes to it.
  pty <- liftIO $ do
      -- Prefer the login shell from the password database (what terminal
      -- emulators use).  $SHELL is unreliable here: launched from a `nix
      -- develop` shell it points at the scripting bash, which is built without
      -- readline, so it has no line editor and arrow keys echo as `^[[A`.
      loginShell <- (userShell <$> (getRealUserID >>= getUserEntryForID))
                      `catch` \(_ :: SomeException) -> return ""
      envShell <- fromMaybe "" <$> lookupEnv "SHELL"
      let shell = fromMaybe "/bin/bash" . listToMaybe $ filter (not . null) [loginShell, envShell]
      -- Inherit the environment but force a sensible TERM (without it the shell
      -- can't bind the arrow-key sequences).
      baseEnv <- getEnvironment
      let env = ("TERM", "xterm-256color") : filter ((/= "TERM") . fst) baseEnv
      -- Prefer tmux: attach-or-create a named session on a private socket, so
      -- the shell (and anything running in it) persists across leksah restarts.
      -- The reader thread sees tmux's redraw on (re)attach.  Without tmux on
      -- PATH, run the shell directly (`-i` for the line editor) — no persistence.
      mbTmux <- findExecutable "tmux"
      (cmd, args) <- case mbTmux of
          Just tmux -> do
              conf <- writeTmuxConf shell
              return (tmux, ["-L", tmuxSocket, "-f", conf, "new-session", "-A", "-s", sessionName termId])
          Nothing -> return (shell, ["-i"])
      (pty, _ph) <- spawnWithPty (Just env) True cmd args (80, 24)
      -- Expose this PTY so the Tmux menu can inject `C-b X` prefix sequences into
      -- it when this terminal is the active one (see IDE.Web.TerminalInput).
      registerTerminalPty termId pty
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
          liftIO $ resizePty pty (round c, round r))
      _ <- ro ^. js1 ("observe" :: Text) rawEl
      -- keystrokes -> shell
      _ <- term ^. js1 ("onData" :: Text) (fun $ \_ _ args -> case args of
              (d:_) -> do
                  s <- valToText d
                  liftIO $ writePty pty (encodeUtf8 s)
              _ -> return ())
      -- title changes -> Terminals list
      _ <- term ^. js1 ("onTitleChange" :: Text) (fun $ \_ _ args -> case args of
              (titleVal:_) -> valToText titleVal >>= liftIO . triggerTitle
              _ -> return ())
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
                  Left _   -> return ()
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
  return $ leftmost [ TerminalTitle <$> titleE, gotoE ]
  where
    -- Match the PTY's window size to xterm's current cols/rows so full-screen
    -- programs (vim, htop, …) lay out correctly.
    syncPtySize term _fit pty = do
        cols <- valToNumber =<< term ^. js ("cols" :: Text)
        rows <- valToNumber =<< term ^. js ("rows" :: Text)
        liftIO $ resizePty pty (round cols, round rows)

-- | The private tmux socket leksah's terminals live on (so they don't mix with
-- the user's own tmux sessions, and so its options don't touch their config).
tmuxSocket :: String
tmuxSocket = "leksah"

sessionName :: Int -> String
sessionName n = "leksah-" <> show n

-- | Write (idempotently) the minimal tmux config used for leksah's terminals:
-- no status bar, pass window titles through to xterm, and use the login shell.
-- Only applies when the tmux server first starts (i.e. for the first terminal).
writeTmuxConf :: FilePath -> IO FilePath
writeTmuxConf loginShell = do
    dir <- getTemporaryDirectory
    let path = dir </> "leksah.tmux.conf"
    writeFile path $ unlines
        [ "set -g status off"
        , "set -g set-titles on"
        , "set -g set-titles-string \"#T\""
        , "set -g default-shell \"" <> loginShell <> "\""
        -- Mouse on so the wheel scrolls tmux's scrollback in the xterm.js pane;
        -- a generous history so there's plenty to scroll back through.
        , "set -g mouse on"
        , "set -g history-limit 50000"
        ]
    return path

-- | Ids of leksah's currently-live tmux sessions (named @leksah-N@), so the
-- Terminals list can be repopulated after a restart.  Empty if tmux is absent
-- or no leksah tmux server is running.
listTerminalSessions :: IO [Int]
listTerminalSessions = (`catch` \(_ :: SomeException) -> return []) $
    findExecutable "tmux" >>= \case
        Nothing -> return []
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-sessions", "-F", "#{session_name}"] ""
            return [ n | line <- lines out
                       , Just rest <- [stripPrefix "leksah-" line]
                       , [(n, "")] <- [reads rest] ]

-- | Kill the tmux session backing terminal @n@ (so it no longer persists).
killTerminalSession :: Int -> IO ()
killTerminalSession n = (`catch` \(_ :: SomeException) -> return ()) $ do
    unregisterTerminalPty n
    findExecutable "tmux" >>= \case
        Nothing -> return ()
        Just tmux -> void $ readProcessWithExitCode tmux
            ["-L", tmuxSocket, "kill-session", "-t", sessionName n] ""

-- | A tmux pane within a window: its index, a display label (its index and the
-- command running in it), and whether it is the window's active pane.
data TmuxPane = TmuxPane
  { tpIndex  :: Int
  , tpLabel  :: Text
  , tpActive :: Bool
  } deriving (Eq, Show)

-- | A tmux window within a session: its index, a display label (its index and
-- name), whether it is the session's active window, and its panes (by index).
data TmuxWindow = TmuxWindow
  { twIndex  :: Int
  , twLabel  :: Text
  , twActive :: Bool
  , twPanes  :: [TmuxPane]
  } deriving (Eq, Show)

-- | The tmux window/pane hierarchy of every live @leksah-N@ session, keyed by
-- session id @N@ — the deeper levels of the Terminals tree (the session level
-- itself is tracked by 'listTerminalSessions').  Gathered in one
-- @list-panes -a@ call.  Empty if tmux is absent or no leksah server is running.
listTerminalTree :: IO (Map Int [TmuxWindow])
listTerminalTree = (`catch` \(_ :: SomeException) -> return M.empty) $
    findExecutable "tmux" >>= \case
        Nothing -> return M.empty
        Just tmux -> do
            (_rc, out, _) <- readProcessWithExitCode tmux
                ["-L", tmuxSocket, "list-panes", "-a", "-F", paneFormat] ""
            return (parsePaneTree out)
  where
    -- Tab-separated so window names / commands (which won't contain tabs) stay
    -- intact: session, window index/name/active, pane index/active/command.
    paneFormat = intercalate "\t"
        [ "#{session_name}", "#{window_index}", "#{window_name}", "#{window_active}"
        , "#{pane_index}", "#{pane_active}", "#{pane_current_command}" ]

-- | Parse the @list-panes -a@ output into the per-session window/pane tree,
-- grouping by session then window (ascending by index within each level).
parsePaneTree :: String -> Map Int [TmuxWindow]
parsePaneTree out = M.map toWindows grouped
  where
    rows =
      [ (sid, wi, wn, wa == "1", pidx, pa == "1", cmd)
      | line <- lines out
      , (sname:wiT:wn:wa:piT:pa:rest) <- [T.splitOn "\t" (T.pack line)]
      , Just sidT <- [T.stripPrefix "leksah-" sname]
      , Just sid  <- [readMaybe (T.unpack sidT)]
      , Just wi   <- [readMaybe (T.unpack wiT)]
      , Just pidx <- [readMaybe (T.unpack piT)]
      , let cmd = T.intercalate "\t" rest ]
    -- session -> (window index -> (name, active, pane index -> (command, active)))
    grouped :: Map Int (Map Int (Text, Bool, Map Int (Text, Bool)))
    grouped = M.fromListWith (M.unionWith mergeWin)
      [ (sid, M.singleton wi (wn, wa, M.singleton pidx (cmd, pa)))
      | (sid, wi, wn, wa, pidx, pa, cmd) <- rows ]
    mergeWin (wn, wa, ps1) (_, _, ps2) = (wn, wa, ps1 <> ps2)
    toWindows wm =
      [ TmuxWindow wi (T.pack (show wi) <> ": " <> wn) wa
          [ TmuxPane pidx (T.pack (show pidx) <> ": " <> cmd) pa
          | (pidx, (cmd, pa)) <- M.toAscList ps ]
      | (wi, (wn, wa, ps)) <- M.toAscList wm ]

-- | Make window @w@ of session @s@ the session's current window.
selectTmuxWindow :: Int -> Int -> IO ()
selectTmuxWindow s w =
    tmuxCmd ["select-window", "-t", sessionName s <> ":" <> show w]

-- | Make pane @p@ of window @w@ in session @s@ the active pane.  Also switch the
-- session to that window first: @select-pane@ only moves focus within a window,
-- so without this a pane in a non-current window wouldn't actually be shown.
selectTmuxPane :: Int -> Int -> Int -> IO ()
selectTmuxPane s w p = do
    selectTmuxWindow s w
    tmuxCmd ["select-pane", "-t", sessionName s <> ":" <> show w <> "." <> show p]

-- | Kill window @w@ of session @s@ (tmux closes the session if it was its last
-- window).
killTmuxWindow :: Int -> Int -> IO ()
killTmuxWindow s w =
    tmuxCmd ["kill-window", "-t", sessionName s <> ":" <> show w]

-- | Kill pane @p@ of window @w@ in session @s@ (tmux closes the window if it was
-- its last pane).
killTmuxPane :: Int -> Int -> Int -> IO ()
killTmuxPane s w p =
    tmuxCmd ["kill-pane", "-t", sessionName s <> ":" <> show w <> "." <> show p]

-- | Run a tmux command on leksah's private socket, ignoring failures.
tmuxCmd :: [String] -> IO ()
tmuxCmd args = (`catch` \(_ :: SomeException) -> return ()) $
    findExecutable "tmux" >>= \case
        Nothing -> return ()
        Just tmux -> void $ readProcessWithExitCode tmux (["-L", tmuxSocket] <> args) ""
