{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | The DOM keydown handler: matches the resolved keybindings table
-- ('IDE.Web.Keybindings') against each keydown on the document and emits the
-- bound 'Command'.  Only 'WhenAlways' bindings live here — the
-- terminal-gated chords (⌘D split, ⌘⌥arrows, …) exist as native menu key
-- equivalents, where the gating is the menu item's enabled state.
--
-- The lookup map lives in an 'IORef' the handler reads per keydown, kept
-- current by a keymap listener — a @keybindings.json@ reload applies to the
-- next keystroke with no reflex replumbing.
module IDE.Web.Widget.Keymap where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M (fromList, lookup)
import qualified Data.Set as S (Set, fromList)

import Reflex (Event, ffilter, leftmost)
import Reflex.Dom.Core
       (DomBuilderSpace, Element, EventResult, Key(..), MonadWidget,
        keyCodeLookup, wrapDomEvent, wrapDomEventMaybe)

import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.EventM (event, onSync, preventDefault)
import GHCJS.DOM.GlobalEventHandlers (keyDown, keyUp)
import GHCJS.DOM.KeyboardEvent
       (getKeyCode, getCtrlKey, getShiftKey, getAltKey, getMetaKey)

import IDE.Web.Chord (toReflexKey)
import IDE.Web.Command (Command(..))
import IDE.Web.Events (KeymapEvents(..))
import IDE.Web.Keybindings
       (Binding(..), CommandSpec(..), Keymap, When(..),
        registerKeymapListener)

-- | The keydown lookup table for one front end: exact modifier set + trigger
-- key → the bound command.
keymapLookup :: Bool -> Keymap -> Map (S.Set Key, Key) Command
keymapLookup browserHosted km = M.fromList
    -- Later bindings win a chord (M.fromList keeps the last occurrence),
    -- so user rules override the defaults.
    [ (rk, cmd)
    | Binding ch args spec <- km
    , csWhen spec == WhenAlways
    , Just rk  <- [toReflexKey browserHosted ch]
    , Just cmd <- [csMake spec args]
    ]

keymapWidget
  :: forall t m . MonadWidget t m
  => Bool -- ^ browser-hosted (warp/web demo)?  Resolves the @mod@ alias:
          --   Cmd+` belongs to the OS/browser there, so Ctrl is the primary.
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
  let flipMod = if browserHosted then Control else Command
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
        mbCmd = M.lookup (mods, key) keyToCommandMap
    when (maybe False (const True) mbCmd) preventDefault
    return mbCmd
  -- The flipper commits when its modifier (Command, or Control when
  -- browser-hosted) is released.
  upE <- wrapDomEvent doc (`onSync` keyUp) $ do
    ke   <- event
    code <- getKeyCode ke
    return (keyCodeLookup (fromIntegral code))
  let flipdone = CommandFlipDone <$ ffilter (== flipMod) upE
  return $ KeymapCommand <$> leftmost [cmdE, flipdone]
