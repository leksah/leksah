{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- SPDX-License-Identifier: Apache-2.0

-- | The Welcome pane: what an OS window shows when it has nothing else in it.
--
-- Two jobs, and the second is the reason it exists at all:
--
--   * introduce leksah and give a first-run user somewhere to start —
--     open a project, open a folder, pick up a recent file, find the
--     keyboard cheat sheet;
--   * make \"an empty OS window\" unrepresentable.  A window with no tabs
--     has no tab row, no content and no way back — you can't even close it
--     from inside.  "IDE.Web.Main" therefore drops a fresh 'WelcomeKey' tab
--     into any window that would otherwise be empty, and closes the OS
--     window instead when there is another one to fall back to.
--
-- Every action here is a process-global request hook ('runOpenProjectPanel',
-- 'requestNewWindow', …) — the same ones the native menu bar drives.  That
-- keeps the pane free of reflex plumbing: it reports nothing outward
-- ('WelcomeEvents' is @()@) and needs no wiring beyond its tab dispatch.
--
-- The chords are read from the LIVE keymap ('registerKeymapListener'), like
-- the Shortcuts sheet, so a user's @keybindings.json@ shows here exactly as
-- it binds and the text can't drift from reality.  Ids that no longer exist
-- simply render no key cap.
module IDE.Web.Widget.Welcome
  ( welcomeWidget
  , welcomeCss
  , nextWelcomeId
  ) where

import Control.Monad (forM_, void, unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import System.Directory (getHomeDirectory)
import System.FilePath (takeFileName, takeDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Clay ((?), (-:), Css)

import Control.Lens ((^.))

import Language.Javascript.JSaddle (eval, js0, liftJSM)

import Reflex
       (Dynamic, Event, ffor, getPostBuild, holdDyn, holdUniqDyn,
        leftmost, never, newTriggerEvent, zipDyn)
import Reflex.Dom.Core
       (EventName(..), _element_raw, blank, divClass, dyn_, elAttr, elAttr',
        elClass, elClass', domEvent, text, (=:))

import IDE.Web.Chord (toGlyphs)
import IDE.Web.Ctx (Ctx)
import IDE.Web.Events (WelcomeEvents)
import IDE.Web.Frame (MonadWidget, performEvent_)
import IDE.Web.Keybindings
       (Binding(..), CommandSpec(..), Keymap, registerKeymapListener)
import IDE.Web.NewWindowRequest (requestNewWindow)
import IDE.Web.OpenFileRequest (deliverOpenedFile)
import IDE.Web.OpenPanel
       (runOpenFilePanel, runOpenFolderPanel, runOpenProjectPanel)
import IDE.Web.PreferencesRequest (requestShowPreferences)
import IDE.Web.ShortcutsRequest (requestShowShortcuts)

import qualified Paths_leksah as P (version)

-- | Welcome panes are minted, never reused: a 'TabKey' is a pane's global
-- identity, and two OS windows can each be showing one.  The counter is
-- process-global and monotonic (same rule as
-- 'IDE.Web.Widget.Browser.nextBrowserId'); it needs no persistence because
-- Welcome tabs are transient — they are dropped at save and re-minted on
-- demand by the empty-window rule.
{-# NOINLINE welcomeCounter #-}
welcomeCounter :: IORef Int
welcomeCounter = unsafePerformIO (newIORef 1)

nextWelcomeId :: IO Int
nextWelcomeId = atomicModifyIORef' welcomeCounter (\n -> (n + 1, n))

--
-- Content.  Kept as data so the renderer stays small and the prose is
-- editable in one place.
--

-- | One clickable row in the \"Start\" column: label, the command id whose
-- chord to show beside it (@\"\"@ = none), the one-line explanation, and what
-- clicking it does.
data Action = Action Text Text Text (IO ())

-- | A run of tip prose: literal text, the live chord of a command id
-- rendered as a key cap, or a literal (file name, setting) in code type.
data Frag = Txt Text | Key Text | Code Text

startActions :: [Action]
startActions =
  [ Action "Open Project…" "workspace.openProject"
      "A .cabal, cabal.project, stack.yaml or flake to work in."
      runOpenProjectPanel
  , Action "Open Folder…" "workspace.openFolder"
      "Any directory — leksah treats it as a plain project."
      runOpenFolderPanel
  , Action "Open File…" "workspace.openFile"
      "Straight into the editor, no project needed."
      runOpenFilePanel
  , Action "New Window" "workspace.newWindow"
      "Another OS window, sharing this one's panes and state."
      requestNewWindow
  , Action "Keyboard Shortcuts" "edit.showShortcuts"
      "The whole cheat sheet, generated from the live keymap."
      requestShowShortcuts
  , Action "Preferences…" "edit.showPreferences"
      "Theme, fonts, editor backend, build behaviour."
      requestShowPreferences
  ]

-- | The orientation cards.  Written so that nothing here is a claim the
-- keymap can contradict: every chord is looked up live, and a binding the
-- user has removed just renders as prose.
tips :: [(Text, [Frag])]
tips =
  [ ( "Panes, not windows"
    , [ Txt "Any tab can become a split pane: "
      , Key "terminal.splitRight"
      , Txt " splits the active one. Drag a pane by its chrome to re-split \
            \the layout, onto another OS window to move it there, or onto the \
            \middle of an empty editor area to give it a window of its own." ] )
  , ( "Terminals are real tmux"
    , [ Txt "Every terminal pane is a tmux pane, so your shells, builds and \
            \repls outlive a leksah restart — and you can attach to the same \
            \session from a plain terminal. The Tmux side pane lists every \
            \session, window and pane; double-click one to bring it here." ] )
  , ( "Claude Code lives in the layout"
    , [ Txt "Agent sessions are panes like any other, and the Agents side \
            \pane shows them as a tree of who forked whom. The status light \
            \by the toolbar turns amber while a session is working and red \
            \when one is waiting on you." ] )
  , ( "Two kinds of bigger"
    , [ Key "view.zoomIn"
      , Txt " and "
      , Key "view.zoomOut"
      , Txt " zoom this whole OS window, layout and all, and the level sticks \
            \across restarts. "
      , Key "view.fontBigger"
      , Txt " and "
      , Key "view.fontSmaller"
      , Txt " resize just the pane you are in." ] )
  , ( "Find things fast"
    , [ Key "edit.find"
      , Txt " searches the active editor; the Grep pane searches the project. \
            \Errors and warnings from every language server land in the \
            \Errors pane, scoped to the project you are working in." ] )
  , ( "Make it yours"
    , [ Txt "Keybindings live in "
      , Code "keybindings.json"
      , Txt " in your leksah config directory, and reload without a restart — \
            \every chord on this page is read back out of that table, so it \
            \always tells you the truth." ] )
  ]

-- | The leksah mark — the double lambda from the application icon.
--
-- The real artwork, not a redraw: the 512×512 element out of
-- @osx\/leksah.icns@, trimmed to the glyph and saved as @pics\/leksah.png@
-- (@nix\/macos-app.nix@ and @nix\/windows-installer.nix@ copy the whole
-- @pics@ tree, so it packages itself).  There is no vector to use instead:
-- the icon began life as a losing entry in the 2008\/09 Haskell logo
-- competition, drawn in Inkscape but only ever published as PNGs, and every
-- copy that reached this repo — the two @.icns@, the 128px @leksah.png@s, the
-- 48px @.ico@s — is a raster.
--
-- A PNG rather than an SVG for a second reason: both of the theme mechanisms
-- that sweep @\/pics@ key on the @.svg@ extension and so leave this alone —
-- the colour-icon swapper ('IDE.Web.Main.colorIconsJs'), which would rewrite
-- the src to a @\/pics\/color@ twin that does not exist, and the light-mode
-- @filter:invert(1)@ in 'IDE.Web.Theme.contrastCss', which would turn the blue
-- orange.
markSrc :: Text
markSrc = "/pics/leksah.png"

links :: [(Text, Text)]
links =
  [ ("Documentation",   "https://leksah.org/")
  , ("Source on GitHub", "https://github.com/leksah/leksah")
  , ("Report an issue",  "https://github.com/leksah/leksah/issues")
  ]

--
-- The widget.
--

welcomeWidget
  :: forall t m. MonadWidget t m
  => Ctx t
  -> Event t ()             -- ^ \"take keyboard focus now\" (the tab's select
                            --   pulse), as every other pane widget gets
  -> Dynamic t [FilePath]   -- ^ recently opened files, most recent first
  -> m (Event t WelcomeEvents)
welcomeWidget _ctx selectedE recentD = do
  -- Read once: the recent-file rows show @~/…@ rather than the full home path.
  home <- liftIO getHomeDirectory
  (kmE, fireKm) <- newTriggerEvent
  -- Fires immediately with the current table, then on every reload.
  liftIO (registerKeymapListener fireKm)
  kmD     <- holdDyn [] kmE
  recentU <- holdUniqDyn recentD
  -- The root is FOCUSABLE, and takes focus when the tab is selected.  Nothing
  -- in here wants the keyboard, but a pane that can't hold focus can't be the
  -- ACTIVE pane (@focusTabJs@ resolves that from the focused element's tab),
  -- and commands that act on the active pane — ⌘W above all — then skip it.
  -- Without this the Welcome pane is the one pane you cannot close, which is
  -- absurd for the pane a user most often wants gone.  @tabindex=-1@ keeps it
  -- out of the tab order: focusable by click and by us, not by ⇥.
  (rootEl, _) <- elAttr' "div" ("class" =: "welcome" <> "tabindex" =: "-1") $
    -- Rebuilt on a keymap reload or a change to the recent list — both rare,
    -- and the pane holds no state of its own worth preserving across one.
    dyn_ (uncurry (render home) <$> zipDyn kmD recentU)
  -- postBuild as well as the pulse: a Welcome pane is only ever created into a
  -- window that has nothing else in it, so it IS the visible tab and cannot be
  -- stealing focus from a background one.  (They are never restored, so the
  -- session-restore case this would otherwise trip over cannot arise.)
  pb <- getPostBuild
  performEvent_ $ ffor (leftmost [selectedE, pb]) $ \_ ->
      liftJSM . void $ _element_raw rootEl ^. js0 ("focus" :: Text)
  return never

render
  :: forall t m. MonadWidget t m
  => FilePath -> Keymap -> [FilePath] -> m ()
render home km recent = divClass "wc-inner" $ do
    divClass "wc-hero" $ do
      elAttr "img" ("class" =: "wc-mark" <> "src" =: markSrc
                     <> "alt" =: "" <> "aria-hidden" =: "true") blank
      divClass "wc-hero-text" $ do
        divClass "wc-title" $ text "Welcome to Leksah"
        divClass "wc-sub" $
          text ("The Haskell IDE · version " <> T.pack (showVersion P.version))
        divClass "wc-blurb" $ text
          "An IDE built out of panes you can split, tab and drag between \
          \windows — with tmux terminals, language-server diagnostics and \
          \Claude Code sessions all living in the same layout."

    divClass "wc-cols" $ do
      divClass "wc-card" $ do
        divClass "wc-card-title" $ text "Start"
        divClass "wc-actions" $ mapM_ action startActions
      divClass "wc-card" $ do
        divClass "wc-card-title" $ text "Recent files"
        if null recent
          then divClass "wc-empty" $ text
                 "Nothing yet. Open a project or a file and it will show up \
                 \here, and in File ▸ Open Recent."
          else divClass "wc-recent" $ mapM_ recentRow (take 8 recent)

    divClass "wc-card wc-wide" $ do
      divClass "wc-card-title" $ text "Finding your way around"
      divClass "wc-tips" $ forM_ tips $ \(title, frags) ->
        divClass "wc-tip" $ do
          divClass "wc-tip-title" $ text title
          divClass "wc-tip-body" $ mapM_ frag frags

    divClass "wc-links" $ do
      forM_ (zip [(0 :: Int) ..] links) $ \(i, (label, url)) -> do
        unless (i == 0) $ elClass "span" "wc-dot" $ text "·"
        (e, _) <- elClass' "a" "wc-link" $ text label
        performEvent_ $ ffor (domEvent Click e) $ \_ ->
          liftJSM . void . eval $
            "if (window.__leksahOpenUrl) window.__leksahOpenUrl('"
              <> url <> "',false,false)"
  where
    -- A start row: a button, its chord, its explanation.
    action (Action label cid blurb act) = do
      (e, _) <- elClass' "div" "wc-action" $ do
        divClass "wc-action-main" $ do
          elClass "span" "wc-action-label" $ text label
          keyCap km cid
        divClass "wc-action-blurb" $ text blurb
      performEvent_ $ liftIO act <$ domEvent Click e

    -- A recent file: name in full, directory dimmed beside it.  Clicking
    -- routes through the same hook the native Open Recent menu uses, so it
    -- opens in whichever window is frontmost.
    recentRow fp = do
      (e, _) <- elAttr' "div" ("class" =: "wc-recent-row"
                               <> "title" =: T.pack fp) $ do
        elClass "span" "wc-recent-name" $ text (T.pack (takeFileName fp))
        elClass "span" "wc-recent-dir"  $ text (prettyDir home fp)
      performEvent_ $ liftIO (deliverOpenedFile fp) <$ domEvent Click e

    frag (Txt t)  = text t
    frag (Key c)  = keyCap km c
    frag (Code t) = elClass "code" "wc-code" $ text t

-- | A recent file's directory for display, with the user's home replaced by
-- @~@ — the part of the path that carries no information and, left in, pushed
-- everything interesting past the ellipsis.  The row's @title@ still carries
-- the full path.
prettyDir :: FilePath -> FilePath -> Text
prettyDir home fp
  | not (null home), home `isPrefixOf` d = "~" <> T.pack (drop (length home) d)
  | otherwise                            = T.pack d
  where d = takeDirectory fp

-- | The chord bound to a command id, as a key cap — or nothing at all if the
-- command has no binding (a user may have removed it).  The LAST matching
-- rule wins, which is the same one the menus display.
keyCap :: MonadWidget t m => Keymap -> Text -> m ()
keyCap km cid =
    case [ toGlyphs (bChord b) | b <- km, csId (bSpec b) == cid ] of
      [] -> return ()
      gs -> elClass "kbd" "wc-key" $ text (last gs)

welcomeCss :: Css
welcomeCss = do
  ".welcome" ? do
    "height" -: "100%"
    "box-sizing" -: "border-box"
    "overflow" -: "auto"
    "background" -: "var(--leksah-bg-sunken)"
    "color" -: "var(--leksah-fg-muted)"
    -- Sized off the pane font so ⌥⌘=/⌥⌘− work here like anywhere else, and
    -- so window zoom scales it through the same cascade.
    "font-size" -: "calc(var(--leksah-mono-size, 13px) * 1.0)"
    "line-height" -: "1.5"
  -- It is focusable (see 'welcomeWidget'), but the pane chrome already shows
  -- which pane is active — a focus ring round the whole page would just be noise.
  ".welcome:focus" ? ("outline" -: "none")
  ".welcome .wc-inner" ? do
    "max-width" -: "62em"
    "margin" -: "0 auto"
    -- Fixed px, not vh: viewport units are NOT divided by the CSS `zoom` on
    -- <html>, so a vh padding would grow by the zoom factor on top of the
    -- zoom itself (see the window-zoom notes in "IDE.Web.Layout").
    "padding" -: "30px 24px 40px 24px"

  -- Hero.
  ".welcome .wc-hero" ? do
    "display" -: "flex"
    "align-items" -: "flex-start"
    "gap" -: "18px"
    "margin-bottom" -: "26px"
  -- The app icon itself ('markSrc'), sized off the pane font like everything
  -- else here so ⌥⌘=/⌥⌘− and window zoom both carry it.  Width only — the
  -- height follows the PNG's own aspect, so the glyph can never be squashed —
  -- and nudged down a hair so its optical centre lines up with the first line
  -- of text rather than with the top of the text box.
  ".welcome .wc-mark" ? do
    "flex" -: "0 0 auto"
    "display" -: "block"
    "width" -: "5em"
    "height" -: "auto"
    "margin-top" -: "0.15em"
  ".welcome .wc-title" ? do
    "font-size" -: "2.0em"
    "font-weight" -: "600"
    "line-height" -: "1.15"
    "color" -: "var(--leksah-fg)"
  ".welcome .wc-sub" ? do
    "color" -: "var(--leksah-fg-dim)"
    "margin" -: "2px 0 8px 0"
  ".welcome .wc-blurb" ? do
    "max-width" -: "44em"

  -- Cards.
  ".welcome .wc-cols" ? do
    "display" -: "grid"
    "grid-template-columns" -: "repeat(auto-fit, minmax(19em, 1fr))"
    "gap" -: "16px"
  ".welcome .wc-card" ? do
    "background" -: "var(--leksah-surface)"
    "border" -: "1px solid var(--leksah-border)"
    "border-radius" -: "10px"
    "padding" -: "12px 14px 14px 14px"
    "min-width" -: "0"
  ".welcome .wc-wide" ? do
    "margin-top" -: "16px"
  ".welcome .wc-card-title" ? do
    "font-size" -: "1.08em"
    "font-weight" -: "600"
    "color" -: "var(--leksah-fg)"
    "margin-bottom" -: "8px"

  -- Start actions.
  ".welcome .wc-action" ? do
    "padding" -: "6px 8px"
    "border-radius" -: "7px"
    "cursor" -: "pointer"
    "border" -: "1px solid transparent"
  ".welcome .wc-action:hover" ? do
    "background" -: "var(--leksah-hover)"
    "border-color" -: "var(--leksah-border-control)"
  ".welcome .wc-action-main" ? do
    "display" -: "flex"
    "align-items" -: "baseline"
    "justify-content" -: "space-between"
    "gap" -: "10px"
  ".welcome .wc-action-label" ? do
    "color" -: "var(--leksah-accent-text)"
    "font-weight" -: "500"
  ".welcome .wc-action-blurb" ? do
    "color" -: "var(--leksah-fg-dim)"
    "font-size" -: "0.92em"

  -- Recent files.
  ".welcome .wc-recent-row" ? do
    "display" -: "flex"
    "align-items" -: "baseline"
    "gap" -: "8px"
    "padding" -: "4px 8px"
    "border-radius" -: "7px"
    "cursor" -: "pointer"
    "min-width" -: "0"
  ".welcome .wc-recent-row:hover" ? ("background" -: "var(--leksah-hover)")
  ".welcome .wc-recent-name" ? do
    "color" -: "var(--leksah-accent-text)"
    "white-space" -: "nowrap"
  ".welcome .wc-recent-dir" ? do
    "color" -: "var(--leksah-fg-dim)"
    "font-size" -: "0.88em"
    "overflow" -: "hidden"
    "text-overflow" -: "ellipsis"
    "white-space" -: "nowrap"
    "min-width" -: "0"
  ".welcome .wc-empty" ? do
    "color" -: "var(--leksah-fg-dim)"

  -- Tips.
  ".welcome .wc-tips" ? do
    "display" -: "grid"
    "grid-template-columns" -: "repeat(auto-fit, minmax(17em, 1fr))"
    "gap" -: "12px 22px"
  ".welcome .wc-tip-title" ? do
    "color" -: "var(--leksah-fg)"
    "font-weight" -: "600"
    "margin-bottom" -: "2px"
  ".welcome .wc-code" ? do
    "font-family" -: "Hasklig, Menlo, monospace"
    "font-size" -: "0.92em"
    "color" -: "var(--leksah-fg)"
    "word-break" -: "break-all"
  ".welcome .wc-tip-body" ? do
    "color" -: "var(--leksah-fg-muted)"
    "font-size" -: "0.95em"

  -- Key caps (same look as the Shortcuts sheet).
  ".welcome .wc-key" ? do
    "font-family" -: "Hasklig, Menlo, monospace"
    "font-size" -: "0.88em"
    "white-space" -: "nowrap"
    "color" -: "var(--leksah-fg-muted)"
    "background" -: "var(--leksah-surface-alt)"
    "border" -: "1px solid var(--leksah-border-control)"
    "border-radius" -: "4px"
    "padding" -: "1px 5px"
    "margin" -: "0 1px"

  -- Footer links.
  ".welcome .wc-links" ? do
    "margin-top" -: "20px"
    "color" -: "var(--leksah-fg-dim)"
  ".welcome .wc-link" ? do
    "color" -: "var(--leksah-accent-text)"
    "cursor" -: "pointer"
  ".welcome .wc-link:hover" ? ("text-decoration" -: "underline")
  ".welcome .wc-dot" ? ("margin" -: "0 8px")
