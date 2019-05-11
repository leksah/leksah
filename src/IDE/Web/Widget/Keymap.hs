{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Keymap where

import Data.Bool (bool)
import Data.Maybe (catMaybes)
import qualified Data.Set as S (fromList)

import Reflex (Reflex(..), MonadHold(..), ffilter, attach, leftmost, FunctorMaybe(..), fmapMaybe)
import Reflex.Dom.Core
       (DomBuilderSpace, EventResult, Element, MonadWidget, Key(..), keyCodeLookup,
        HasDomEvent(..), EventName(..))
import IDE.Web.Events (KeymapEvents(..))
import IDE.Web.Command (commandPackageBuild, Command(..))
import qualified Data.Map as M (lookup, fromList)

keymapWidget
  :: forall t m . MonadWidget t m
  => Element EventResult (DomBuilderSpace m) t
  -> m (Event t KeymapEvents)
keymapWidget top = do
  let keydown' = keyCodeLookup . fromIntegral <$> domEvent Keydown top
      keyup' = keyCodeLookup . fromIntegral <$> domEvent Keyup top
      keyIsPressed k = holdDyn Nothing $ leftmost
        [ fmapMaybe (bool Nothing (Just Nothing) . (==k)) keyup'
        , fmapMaybe (bool Nothing (Just (Just k)) . (==k)) keydown'
        ]
  modifiers <- fmap (S.fromList . catMaybes) . sequence <$> mapM keyIsPressed
    [Control, Shift, Alt, Command]

  let keyToCommandMap = M.fromList $
        map (\(mods, key, command) -> ((S.fromList mods, key), command))
        [ ([Control]        , Backquote, CommandFlipDown)
        , ([Control, Shift] , Backquote, CommandFlipUp)
        , ([Control]        , KeyB,      commandPackageBuild)
        , ([Control]        , KeyJ,      CommandNextError)
        , ([Control, Shift] , KeyJ,      CommandPreviousError)
        ]
      keydown = attach (current modifiers) keydown'
--      keyup = attach (current modifiers) keyup'
      keydownCommand = fmapMaybe (`M.lookup` keyToCommandMap) keydown
      flipdone = CommandFlipDone <$ ffilter (==Control) keyup'
  return $ KeymapCommand <$> leftmost [ keydownCommand, flipdone ]
