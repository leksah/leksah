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
import IDE.Web.Command
       (commandPackageBuild, snapWindowCmd, toggleTransparencyCmd, Command(..))

-- | The fixed global keyboard chords, shared by 'keymapWidget' (which turns
-- them into the live lookup table) and the Shortcuts cheat-sheet pane (which
-- renders them) so the two can never drift apart.  The flipper's modifier
-- ('flipMod', Command vs Control) and the numbered-navigation chords (⌘/⌥⌘/⌃⌘
-- 1-9) are added in 'keymapWidget' itself — they depend on host/runtime — and
-- the cheat sheet represents those separately.
globalBindings :: [([Key], Key, Command)]
globalBindings =
    -- Build: Ctrl+Shift+B (Cmd+Shift+B on macOS), matching VS Code.  Plain
    -- Ctrl+B is avoided — it's the tmux prefix.
    [ ([Control, Shift] , KeyB,      commandPackageBuild)
    , ([Command, Shift] , KeyB,      commandPackageBuild)
    , ([Control]        , KeyJ,      CommandNextError)
    , ([Control, Shift] , KeyJ,      CommandPreviousError)
    , ([Command]        , KeyF,      CommandFind)
    , ([Command]        , Comma,     CommandShowPreferences)
    , ([Command]        , ForwardSlash, CommandShowShortcuts)
    -- Underlay (macOS): ⌘⌥U snap/unsnap a window on the active pane, ⌘⌥Y
    -- toggle the active pane's transparency.  Reuse the menu commands.
    , ([Command, Alt]   , KeyU,      snapWindowCmd)
    , ([Command, Alt]   , KeyY,      toggleTransparencyCmd)
    -- Jump to the next terminal window wanting attention (bell, then activity).
    , ([Control, Alt]   , KeyA,      CommandFocusAlert)
    ]

keymapWidget
  :: forall t m . MonadWidget t m
  => Bool -- ^ browser-hosted (warp/web demo)?  Cmd+` belongs to the OS/browser
          --   there, so Ctrl drives the tab flipper instead.
  -> Element EventResult (DomBuilderSpace m) t
  -> m (Event t KeymapEvents)
keymapWidget browserHosted _top = do
  -- Listen on the document (events bubble up to it) so we don't need the raw
  -- root element to satisfy IsGlobalEventHandlers.
  doc <- currentDocumentUnchecked
  let flipMod = if browserHosted then Control else Command
      keyToCommandMap = M.fromList $
        map (\(mods, key, command) -> ((S.fromList mods, key), command)) $
        -- The flipper's modifier is host-dependent, so it stays here rather
        -- than in the shared 'globalBindings'.
        [ ([flipMod]        , Backquote, CommandFlipDown)
        , ([flipMod, Shift] , Backquote, CommandFlipUp)
        ]
        ++ globalBindings
        -- Numbered navigation (terminal-app style): ⌘1…9 the active
        -- terminal's Nth split (layout order), ⌥⌘1…9 the Nth side-bar pane,
        -- ⌃⌘1…9 the Nth bottom-bar pane.  Hold ⌘ to see the numbers as
        -- badges (a preference).
        ++ concat
        [ [ ([Command]          , d, CommandSelectSplit n)
          , ([Command, Alt]     , d, CommandSelectSidePane n)
          , ([Command, Control] , d, CommandSelectBottomPane n) ]
        | (n, d) <- zip [1 ..]
            [ Digit1, Digit2, Digit3, Digit4, Digit5
            , Digit6, Digit7, Digit8, Digit9 ] ]
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
  -- The flipper commits when its modifier (Command, or Control when
  -- browser-hosted) is released.
  upE <- wrapDomEvent doc (`onSync` keyUp) $ do
    ke   <- event
    code <- getKeyCode ke
    return (keyCodeLookup (fromIntegral code))
  let flipdone = CommandFlipDone <$ ffilter (== flipMod) upE
  return $ KeymapCommand <$> leftmost [cmdE, flipdone]
