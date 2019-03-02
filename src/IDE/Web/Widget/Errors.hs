{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IDE.Web.Widget.Errors
  ( errorsCss
  , errorsWidget
  ) where

import Control.Lens (view, (^?))
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Dependent.Map (DMap)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import qualified Data.Map as M (elems, fromList, lookup, size)
import Data.Maybe (listToMaybe)
import Data.Sequence ((!?))
import Data.Text (Text)
import qualified Data.Text as T
       (pack, dropWhileEnd, takeWhile, length, drop, intercalate, lines)

import Clay
       (overflowX, px, width, cursorDefault,
        nowrap, whiteSpace, pct, vGradient, backgroundImage, height, (?),
        Css, Color(..), Cursor(..), Auto(..), Background(..))

import GHCJS.DOM.Types (Element(..), HTMLElement(..), uncheckedCastTo)
import GHCJS.DOM.HTMLElement (getOffsetHeight)

import Reflex
       (attachWithMaybe, select,
        attachWith, mergeMap, switchDyn, zipDynWith, updated, leftmost,
        delay, holdUniqDyn, Dynamic, holdDyn, never, fmapMaybe, tag,
        current, getPostBuild, performEvent, fan, foldDyn)
import Reflex.Dom.Core
       (elDynClass', divClass, virtualList, elAttr',
        resizeDetectorWithAttrs, dynText, elDynAttr, text, MonadWidget,
        (=:), Event, domEvent, EventName(..), _element_raw)

import IDE.Core.State
       (logRefFilePath, IDE, allLogRefs, LogRef(..), LogRefType(..))
import IDE.Web.Command (_CommandNextError, _CommandPreviousError)
import IDE.Web.Events (IDEWidget(..), ErrorsEvents(..), _KeymapCommand)

errorsCss :: Css
errorsCss = do
  ".errors" ? do
    backgroundImage (vGradient (Rgba 32 32 32 1.0) (Rgba 16 16 16 1.0))
    height (pct 100)
    overflowX auto
  ".errors-child" ? do
    width (px 2000)
    height (pct 100)
  ".errors .error-item" ? do
    whiteSpace nowrap
    cursor cursorDefault
  ".errors .error-item.selected" ?
    background (Rgba 30 88 209 1.0)

errorsWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> Event t (DMap IDEWidget Identity)
  -> m (Event t ErrorsEvents)
errorsWidget ide allEvents = do
  let keyEvents = select (fan allEvents) KeymapWidget
      commandE x = fmapMaybe (^? _KeymapCommand . x) keyEvents
      nextError = commandE _CommandNextError
      prevError = commandE _CommandPreviousError
  allRefs <- holdUniqDyn (view allLogRefs <$> ide)
  numberOfRefsD <- holdUniqDyn $ length <$> allRefs
  selectionIndexD <- holdUniqDyn =<< foldDyn ($) (-1::Int) (leftmost
    [ (\n x -> let x' = succ x in if x' >= n then 0     else x') <$> tag (current numberOfRefsD) nextError
    , (\n x -> let x' = pred x in if x' < 0  then n - 1 else x') <$> tag (current numberOfRefsD) prevError
--    , const -1 <$ reset
    ])
  let selE = attachWithMaybe (!?) (current allRefs) $ updated selectionIndexD
  goE <- delay 0 $ ErrorsGoto <$> selE
  divClass "errors" $ mdo
    (resizeE, result) <- resizeDetectorWithAttrs ("class" =: "errors-child") $ mdo
      let p = uncheckedCastTo HTMLElement $ _element_raw parent
      postPostBuild <- delay 0 =<< getPostBuild
      sizeE <- performEvent (getOffsetHeight p <$ leftmost [postPostBuild, resizeE])
      (parent, result) <- elAttr' "div" ("style" =: "height: 100%") $ mdo
        let refs = fmap (M.fromList . zip [0..] . toList) allRefs
        heightD <- holdDyn 80 $ round <$> sizeE
        let expandWindow (idx, num) = (max 0 (idx - 20), num + 40)
            itemsInWindow = zipDynWith (\(idx,num) is ->
                M.fromList $ map (\ix -> (ix, M.lookup ix is)) [idx .. idx + num]) (expandWindow <$> windowD) refs
            updateMap old new = (Just <$> new) <> (Nothing <$ old)
            itemsUpdate = attachWith updateMap (current itemsInWindow) (updated itemsInWindow)
        (windowD, eventsD) <- virtualList
          heightD
          20
          (M.size <$> refs)
          0
          never -- scrollTo
          id
          mempty
          itemsUpdate
          (\k iv u -> do
            v <- holdDyn iv u
            (e, _) <- elDynClass' "div" (("error-item" <>) . bool "" " selected" . (==k) <$> selectionIndexD) $ do
              let imgSrc l = case logRefType <$> l of
                    Just LintRef -> "/pics/ide_suggestion.png"
                    Just WarningRef -> "/pics/ide_warning.png"
                    Just TestFailureRef -> "/pics/tango/status/software-update-urgent.svg"
                    _ -> "/pics/ide_error.png"
                  singleLine l = T.pack (logRefFilePath l) <> ": " <> refDescription l
                                  & removeIndentation
                                  & T.lines
                                  & map removeTrailingWhiteSpace
                                  & T.intercalate " "
              elDynAttr "img" (("src" =:) . imgSrc <$> v) $ return ()
              text " "
              dynText $ maybe "" singleLine <$> v
            return $ fmapMaybe id $ tag (current v) (domEvent Dblclick e))
        return . fmapMaybe (fmap ErrorsGoto . listToMaybe . M.elems) $ switchDyn (mergeMap <$> eventsD)
      return result
    return $ leftmost [ result, goE ]

-- | Removes the unnecessary indentation
removeIndentation :: Text -> Text
removeIndentation t = T.intercalate "\n" $ map (T.drop minIndent) l
  where
    l = T.lines t
    minIndent = minimum $ map (T.length . T.takeWhile (== ' ')) l

removeTrailingWhiteSpace :: Text -> Text
removeTrailingWhiteSpace = T.dropWhileEnd isSpace
