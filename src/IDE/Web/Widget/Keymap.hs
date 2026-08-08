{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | The DOM keydown handler: matches the resolved keybindings table
-- ('IDE.Web.Keybindings') against each keydown on the document and emits the
-- bound 'Command'.
--
-- On the NATIVE front ends only 'WhenAlways' bindings live here: the
-- terminal-gated chords (⌘D split, ⌘⌥arrows, …) are native menu key
-- equivalents, AppKit\/Gtk consumes them before the web view ever sees a
-- keydown, and the gating is the menu item's enabled state.  A BROWSER-HOSTED
-- front end (leksah-warp, the in-browser demo) has no native menu — its
-- menubar is HTML and, as 'IDE.Web.Widget.Menu' notes, cannot intercept key
-- equivalents at all — so every one of those chords was simply dead there.
-- They are bound here instead, with the same gate the menu item's enabled
-- state expresses, read at keydown time from 'getActiveTerminal' \/
-- 'getActiveConvertible'.  Scoping that to browser-hosted keeps the native
-- front ends single-dispatch: there the menu owns the chord, and a disabled
-- item must stay inert rather than fire through this path.
--
-- The lookup map lives in an 'IORef' the handler reads per keydown, kept
-- current by a keymap listener — a @keybindings.json@ reload applies to the
-- next keystroke with no reflex replumbing.
module IDE.Web.Widget.Keymap where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Data.Bool (bool)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M (fromList, lookup)
import Data.Maybe (isJust)
import qualified Data.Set as S (Set, fromList)

import Reflex (Event, ffilter, leftmost)
import Reflex.Dom.Core
       (DomBuilderSpace, Element, EventResult, Key(..),
        keyCodeLookup, wrapDomEvent, wrapDomEventMaybe)

import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.EventM (event, onSync, preventDefault)
import GHCJS.DOM.GlobalEventHandlers (keyDown, keyUp)
import GHCJS.DOM.KeyboardEvent
       (getKeyCode, getCtrlKey, getShiftKey, getAltKey, getMetaKey)

import IDE.Web.Chord (toReflexKeys)
import IDE.Web.Command (Command(..))
import IDE.Web.Events (KeymapEvents(..))
import IDE.Web.Keybindings
       (Binding(..), CommandSpec(..), Keymap, When(..),
        registerKeymapListener)
import IDE.Web.Frame (MonadWidget)
import IDE.Web.TerminalInput (getActiveTerminal, getActiveConvertible)

-- | The keydown lookup table for one front end: exact modifier set + trigger
-- key → the bound command and the gate it is live under.
keymapLookup :: Bool -> Keymap -> Map (S.Set Key, Key) (When, Command)
keymapLookup browserHosted km = M.fromList
    -- Later bindings win a chord (M.fromList keeps the last occurrence),
    -- so user rules override the defaults.
    [ (rk, (csWhen spec, cmd))
    | Binding ch args spec <- km
    , browserHosted || csWhen spec == WhenAlways
    , rk       <- toReflexKeys browserHosted ch
    , Just cmd <- [csMake spec args]
    ]

-- | Is a gated binding live right now?  The same question the native menu
-- item's enabled state answers, asked of the same two published refs.
gateOpen :: When -> IO Bool
gateOpen WhenAlways                = return True
gateOpen WhenTerminal              = isJust <$> getActiveTerminal
gateOpen WhenTerminalOrConvertible =
    (||) <$> (isJust <$> getActiveTerminal)
         <*> (isJust <$> getActiveConvertible)

keymapWidget
  :: forall t m . MonadWidget t m
  => Bool -- ^ browser-hosted (warp/web demo)?  Resolves the @mod@ alias
          --   (Ctrl is the primary there, with ⌘ accepted too — see
          --   'toReflexKeys') and decides whether the gated chords are bound
          --   here at all.
  -> Element EventResult (DomBuilderSpace m) t
  -> m (Event t KeymapEvents)
keymapWidget browserHosted _top = do
  -- Listen on the document (events bubble up to it) so we don't need the raw
  -- root element to satisfy IsGlobalEventHandlers.
  doc <- currentDocumentUnchecked
  mapRef <- liftIO $ newIORef mempty
  -- Fires immediately with the current table, then on every reload.
  liftIO . registerKeymapListener $
    writeIORef mapRef . keymapLookup browserHosted
  -- The flipper's commit modifier — whichever one stepped it.  Browser-hosted
  -- accepts ⌘\` as well as Ctrl+\`, so it must accept either release.
  let flipMods = if browserHosted then [Control, Command] else [Command]
  -- Read the modifier state straight off each keydown event (rather than
  -- tracking key up/down separately, which could desync and miss a shortcut).
  -- preventDefault on a recognised shortcut so the browser/host doesn't also act
  -- on it (and so macOS doesn't beep about an "unhandled" key).
  cmdE <- wrapDomEventMaybe doc (`onSync` keyDown) $ do
    ke    <- event
    code  <- getKeyCode ke
    ctrl  <- getCtrlKey ke
    shift <- getShiftKey ke
    alt   <- getAltKey ke
    meta  <- getMetaKey ke
    keyToCommandMap <- liftIO (readIORef mapRef)
    let key  = keyCodeLookup (fromIntegral code)
        mods = S.fromList $
                 [Control | ctrl] ++ [Shift | shift] ++ [Alt | alt] ++ [Command | meta]
    -- A chord whose gate is shut is NOT ours: leave it unhandled (and not
    -- prevented) so the host still gets it, exactly as a disabled menu item
    -- would.
    mbCmd <- case M.lookup (mods, key) keyToCommandMap of
      Nothing         -> return Nothing
      Just (whn, cmd) -> bool Nothing (Just cmd) <$> liftIO (gateOpen whn)
    when (maybe False (const True) mbCmd) preventDefault
    return mbCmd
  -- The flipper commits when its modifier (Command, or Control when
  -- browser-hosted) is released.
  upE <- wrapDomEvent doc (`onSync` keyUp) $ do
    ke   <- event
    code <- getKeyCode ke
    return (keyCodeLookup (fromIntegral code))
  let flipdone = CommandFlipDone <$ ffilter (`elem` flipMods) upE
  return $ KeymapCommand <$> leftmost [cmdE, flipdone]
