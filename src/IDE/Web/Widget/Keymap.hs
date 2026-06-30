{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Keymap where

import Control.Monad (when)

import qualified Data.Set as S (fromList)
import qualified Data.Map as M (lookup, fromList)

import Reflex (Reflex(..), ffilter, leftmost)
import Reflex.Dom.Core
       (DomBuilderSpace, EventResult, Element, MonadWidget, Key(..),
        keyCodeLookup, wrapDomEvent, wrapDomEventMaybe)

import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.EventM (event, onSync, preventDefault)
import GHCJS.DOM.GlobalEventHandlers (keyDown, keyUp)
import GHCJS.DOM.KeyboardEvent
       (getKeyCode, getCtrlKey, getShiftKey, getAltKey, getMetaKey)

import IDE.Web.Events (KeymapEvents(..))
import IDE.Web.Command (commandPackageBuild, Command(..))

keymapWidget
  :: forall t m . MonadWidget t m
  => Element EventResult (DomBuilderSpace m) t
  -> m (Event t KeymapEvents)
keymapWidget _top = do
  -- Listen on the document (events bubble up to it) so we don't need the raw
  -- root element to satisfy IsGlobalEventHandlers.
  doc <- currentDocumentUnchecked
  let keyToCommandMap = M.fromList $
        map (\(mods, key, command) -> ((S.fromList mods, key), command))
        [ ([Control]        , Backquote, CommandFlipDown)
        , ([Control, Shift] , Backquote, CommandFlipUp)
        , ([Control]        , KeyB,      commandPackageBuild)
        , ([Control]        , KeyJ,      CommandNextError)
        , ([Control, Shift] , KeyJ,      CommandPreviousError)
        ]
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
    let key  = keyCodeLookup (fromIntegral code)
        mods = S.fromList $
                 [Control | ctrl] ++ [Shift | shift] ++ [Alt | alt] ++ [Command | meta]
        mbCmd = M.lookup (mods, key) keyToCommandMap
    when (maybe False (const True) mbCmd) preventDefault
    return mbCmd
  -- The flipper commits when Control is released.
  upE <- wrapDomEvent doc (`onSync` keyUp) $ do
    ke   <- event
    code <- getKeyCode ke
    return (keyCodeLookup (fromIntegral code))
  let flipdone = CommandFlipDone <$ ffilter (== Control) upE
  return $ KeymapCommand <$> leftmost [cmdE, flipdone]
