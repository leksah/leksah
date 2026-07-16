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
import qualified Data.Map as M (lookup, size, fromList, toList)

import Clay
       (lightblue, grey, green, red, color, (-:), overflowX,
        px, width, cursorDefault, whiteSpace, pct, vGradient,
        backgroundImage, height, (?), Css, Cursor(..), Auto(..))
import Clay.Text (pre)

import GHCJS.DOM.Types (Element(..), HTMLElement(..), uncheckedCastTo)
import GHCJS.DOM.HTMLElement (getOffsetHeight)

import Reflex
       (attachWithMaybe, attachWith, zipDynWith, updated,
        leftmost, delay, holdUniqDyn, Dynamic, holdDyn, never, current,
        getPostBuild, performEvent, fmapMaybe, foldDyn, tag, ffilter)
import Reflex.Dom.Core
       (elDynAttr', virtualList, elAttr, elAttr',
        resizeDetectorWithAttrs, dynText, MonadWidget, (=:), Event,
        _element_raw)

import IDE.Web.Theme (selectionColor)
import IDE.Core.State
       (IDE, logLineMap)
import IDE.Web.Events (LogEvents, FindbarEvents)
import IDE.Web.Widget.Findbar (findSelection)
import qualified Data.Text as T (pack)

logCss :: Css
logCss = do
    ".log" ? do
        -- Flat black (via the bottom-bar pane's black background), like the Changes pane.
        "font-family" -: "var(--leksah-mono, Hasklig, Menlo, monospace)"
        height (pct 100)
        overflowX auto
    ".log-child" ? do
        width (px 2000)
        height (pct 100)
    ".log .log-item" ? do
        whiteSpace pre
        cursor cursorDefault
    ".log .log-item.selected" ?
        backgroundImage (vGradient selectionColor selectionColor)
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
  -> Event t FindbarEvents
  -> Event t Bool          -- ^ keyboard list move: 'True' = down, 'False' = up
  -> Event t ()            -- ^ keyboard activate (Enter); a no-op for the log
  -> m (Event t LogEvents)
logWidget ide findE moveE _activateE =
  elAttr "div" ("class" =: "log leksah-vlist" <> "data-pane" =: "log" <> "tabindex" =: "-1") $ mdo
    (resizeE, result) <- resizeDetectorWithAttrs ("class" =: "log-child") $ mdo
      let p = uncheckedCastTo HTMLElement $ _element_raw parent
      postPostBuild <- delay 0 =<< getPostBuild
      initialHeightE <- performEvent (getOffsetHeight p <$ postPostBuild)
      let sizeE = leftmost [ initialHeightE, fmapMaybe snd resizeE]
      (parent, result) <- elAttr' "div" ("style" =: "height: 100%") $ mdo
        logLines <- -- fmap (M.fromList . zip [0..] . toList) <$>
          holdUniqDyn (view logLineMap <$> ide)
        -- Find selects a log line: matching index highlights it and scrolls to it.
        findSelD <- findSelection findE (map (\(i, (t, _)) -> (i, t)) . M.toList <$> logLines)
        -- Up/Down arrows and find share one selection index (highlight + scroll).
        let numD = M.size <$> logLines
        selectionIndexD <- holdUniqDyn =<< foldDyn ($) (-1::Int) (leftmost
          [ (\n x -> let x' = succ x in if x' >= n then 0     else x') <$> tag (current numD) (ffilter id moveE)
          , (\n x -> let x' = pred x in if x' < 0  then n - 1 else x') <$> tag (current numD) (ffilter not moveE)
          , const <$> fmapMaybe id (updated findSelD)
          ])
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
          (leftmost [scrollToE, fmapMaybe (\i -> if i >= 0 then Just i else Nothing) (updated selectionIndexD)])
          id
          mempty
          itemsUpdate
          (\k iv u -> do
            v <- holdDyn iv u
            (_e, _) <- elDynAttr' "div"
                  ((\sel v' -> "class" =: ("log-item"
                        <> (if sel == k then " selected" else "")
                        <> maybe "" ((" "<>) . T.pack . show . snd) v'))
                     <$> selectionIndexD <*> v) $
              dynText $ maybe "" fst <$> v
            return ()) ---- $ tag (current $ logRefSrcSpan <$> v) (domEvent Dblclick e))
        return never -- . fmapMaybe (fmap ErrorsGoto . listToMaybe . M.elems) $ switchDyn (mergeMap <$> eventsD)
      return result
    return result

