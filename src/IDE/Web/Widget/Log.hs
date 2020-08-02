{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module IDE.Web.Widget.Log
  ( logCss
  , logWidget
  ) where

import Control.Lens (view)
import qualified Data.Map as M (lookup, size, fromList)

import Clay
       (lightblue, grey, green, red, color, fontFamily, overflowX,
        px, width, cursorDefault, whiteSpace, pct, vGradient,
        backgroundImage, height, (?), Css, Color(..), Cursor(..), Auto(..))
import Clay.Text (pre)

import GHCJS.DOM.Types (Element(..), HTMLElement(..), uncheckedCastTo)
import GHCJS.DOM.HTMLElement (getOffsetHeight)

import Reflex
       (attachWithMaybe, attachWith, zipDynWith, updated,
        leftmost, delay, holdUniqDyn, Dynamic, holdDyn, never, current,
        getPostBuild, performEvent, fmapMaybe)
import Reflex.Dom.Core
       (elDynAttr', divClass, virtualList, elAttr',
        resizeDetectorWithAttrs, dynText, MonadWidget, (=:), Event,
        _element_raw)

import IDE.Core.State
       (IDE, logLineMap)
import IDE.Web.Events (LogEvents)
import qualified Data.Text as T (pack)

logCss :: Css
logCss = do
    ".log" ? do
        backgroundImage (vGradient (Rgba 32 32 32 1.0) (Rgba 16 16 16 1.0))
        fontFamily ["Hasklig"] []
        height (pct 100)
        overflowX auto
    ".log-child" ? do
        width (px 2000)
        height (pct 100)
    ".log .log-item" ? do
        whiteSpace pre
        cursor cursorDefault
    ".log .ErrorTag" ? do
        color red
    ".log .FrameTag" ? do
        color green
    ".log .InputTag" ? do
        color lightblue
    ".log .InfoTag" ? do
        color grey

logWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> m (Event t LogEvents)
logWidget ide =
  divClass "log" $ mdo
    (resizeE, result) <- resizeDetectorWithAttrs ("class" =: "log-child") $ mdo
      let p = uncheckedCastTo HTMLElement $ _element_raw parent
      postPostBuild <- delay 0 =<< getPostBuild
      initialHeightE <- performEvent (getOffsetHeight p <$ postPostBuild)
      let sizeE = leftmost [ initialHeightE, fmapMaybe snd resizeE]
      (parent, result) <- elAttr' "div" ("style" =: "height: 100%") $ mdo
        logLines <- -- fmap (M.fromList . zip [0..] . toList) <$>
          holdUniqDyn (view logLineMap <$> ide)
        heightD <- holdDyn 80 $ round <$> sizeE
        let expandWindow (idx, num) = (max 0 (idx - 20), num + 40)
            itemsInWindow = zipDynWith (\(idx,num) is ->
                M.fromList $ map (\ix -> (ix, M.lookup ix is)) [idx .. idx + num]) (expandWindow <$> windowD) logLines
--        let itemsInWindow = zipDynWith (\(idx,num) refs' ->
--                M.fromList . zip [idx..] . toList . Seq.take num $ Seq.drop idx refs') windowD logLines
----                M.fromList . zip [idx..idx+num] . (<> repeat Nothing) . fmap Just . toList . Seq.take num $ Seq.drop idx refs') windowD refs
----        refs <- fmap (M.fromList . zip [0..] . toList) <$> holdUniqDyn (view allLogRefs <$> ide)
            updateMap old new = (Just <$> new) <> (Nothing <$ old)
            itemsUpdate = attachWith updateMap (current itemsInWindow) (updated itemsInWindow)
        let logSize = M.size <$> logLines
        unlockE <- delay 0.1 $ attachWithMaybe
              (\((oldIdx, num), l) (newIdx, _) ->
                if | oldIdx > newIdx && newIdx + num < l -> Just False
                   | oldIdx < newIdx && newIdx + num + 1 >= l -> Just True
                   | otherwise -> Nothing) (current $ (,) <$> windowD <*> logSize) (updated windowD)
        unlockedD <- holdUniqDyn =<< holdDyn True unlockE
        currentScrollD <- holdUniqDyn $ (,) <$> windowD <*> unlockedD
        scrollToE <- delay 0.1 $ attachWithMaybe
              (\((_, num), unlocked) newLength -> if unlocked then Just (newLength - num + 1) else Nothing) (current currentScrollD) (updated logSize)
        -- elAttr "div" ("style" =: "position: relative;") $ display windowD
        (windowD, _eventsD) <- virtualList
          heightD
          20
          logSize
          0
          scrollToE
          id
          mempty
          itemsUpdate
          (\_k iv u -> do
            v <- holdDyn iv u
            (_e, _) <- elDynAttr' "div" (("class" =:) . ("log-item "<>)
                  . maybe "" ((" "<>) . T.pack . show . snd) <$> v) $
              dynText $ maybe "" fst <$> v
            return ()) ---- $ tag (current $ logRefSrcSpan <$> v) (domEvent Dblclick e))
        return never -- . fmapMaybe (fmap ErrorsGoto . listToMaybe . M.elems) $ switchDyn (mergeMap <$> eventsD)
      return result
    return result

