{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IDE.Web.Widget.Errors
  ( errorsCss
  , errorsWidget
  ) where

import Control.Lens ((^?))
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Dependent.Map (DMap)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as M
       (elems, findWithDefault, fromList, lookup, size)
import Data.Maybe (listToMaybe)
import Data.Sequence ((!?))
import qualified Data.Sequence as Seq (fromList)
import Data.Text (Text)
import qualified Data.Text as T
       (pack, dropWhileEnd, takeWhile, length, drop, intercalate, lines)
import System.FilePath (isAbsolute, makeRelative, (</>))

import Clay
       (overflowX, px, width, cursorDefault,
        nowrap, whiteSpace, pct, height, (?), (-:),
        Css, Cursor(..), Auto(..), Background(..))

import Reflex
       (attachWithMaybe, select,
        attachWith, mergeMap, switchDyn, zipDynWith, updated, leftmost,
        delay, holdUniqDyn, holdDyn, fmapMaybe, tag,
        current, fan, foldDyn, ffilter)
import Reflex.Dom.Core
       (elDynClass', virtualList, elAttr, elClass',
        dynText, elDynAttr, text, MonadWidget,
        (=:), Event, domEvent, EventName(..), _element_raw)

import IDE.Web.Widget.ResizeObserver (resizeObserver)

import IDE.Web.Theme (selectionColor)
import IDE.Problems.Types (Problem(..), Severity(..))
import IDE.Web.Ctx (Ctx(..))
import IDE.Workspace (Ws, activeProject, prDir)
import IDE.Web.Command (_CommandNextError, _CommandPreviousError)
import IDE.Web.Events
       (IDEWidget(..), ErrorsEvents(..), FindbarEvents, _KeymapCommand)
import IDE.Web.Widget.Findbar (findSelection)

errorsCss :: Css
errorsCss = do
  ".errors" ? do
    -- Flat black (via the bottom-bar pane's black background), like the Changes pane.
    height (pct 100)
    overflowX auto
  ".errors-child" ? do
    width (px 2000)
    height (pct 100)
  ".errors .error-item" ? do
    whiteSpace nowrap
    cursor cursorDefault
  -- The severity SVGs carry only a viewBox (no intrinsic px size), so pin them
  -- to the row height explicitly — otherwise they render at the default size.
  ".errors .error-item img" ? do
    height (px 14)
    width (px 14)
    "vertical-align" -: "text-bottom"
  ".errors .error-item.selected" ?
    background selectionColor

errorsWidget
  :: forall t m . MonadWidget t m
  => Ctx t
  -> Event t (DMap IDEWidget Identity)
  -> Event t FindbarEvents
  -> Event t Bool          -- ^ keyboard list move: 'True' = down, 'False' = up
  -> Event t ()            -- ^ keyboard activate (Enter) of the selected row
  -> m (Event t ErrorsEvents)
errorsWidget ctx allEvents findE moveE activateE = do
  let keyEvents = select (fan allEvents) KeymapWidget
      commandE x = fmapMaybe (^? _KeymapCommand . x) keyEvents
      -- Next-error command and arrow-down move the selection the same way.
      nextError = leftmost [() <$ commandE _CommandNextError, () <$ ffilter id moveE]
      prevError = leftmost [() <$ commandE _CommandPreviousError, () <$ ffilter not moveE]
  allRefs <- holdUniqDyn
      (Seq.fromList <$> (activeProjectProblems <$> cWs ctx <*> cProblems ctx))
  numberOfRefsD <- holdUniqDyn $ length <$> allRefs
  -- Find selects an error: matching index drives the selection (and scroll).
  findSelD <- findSelection findE (zip [0..] . map errorLine . toList <$> allRefs)
  selectionIndexD <- holdUniqDyn =<< foldDyn ($) (-1::Int) (leftmost
    [ (\n x -> let x' = succ x in if x' >= n then 0     else x') <$> tag (current numberOfRefsD) nextError
    , (\n x -> let x' = pred x in if x' < 0  then n - 1 else x') <$> tag (current numberOfRefsD) prevError
    , const <$> fmapMaybe id (updated findSelD)
    ])
  -- Selection change navigates; Enter re-navigates to the current selection.
  let selChangeE = attachWithMaybe (!?) (current allRefs) $ updated selectionIndexD
      activateSelE = attachWithMaybe (!?) (current allRefs) (tag (current selectionIndexD) activateE)
  goE <- delay 0 $ ErrorsGoto . snd <$> leftmost [selChangeE, activateSelE]
  elAttr "div" ("class" =: "errors leksah-vlist" <> "data-pane" =: "errors" <> "tabindex" =: "-1") $ mdo
    -- The virtual list needs a pixel viewport height; a ResizeObserver on the
    -- pane feeds it (the scroll-based reflex resize detector is dead in
    -- wkwebview — see IDE.Web.Widget.ResizeObserver).
    (childEl, result) <- elClass' "div" "errors-child" $ mdo
      let refs = fmap (M.fromList . zip [0..] . toList) allRefs
      heightD <- holdDyn 80 $ round . snd <$> resizeE
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
        (fmapMaybe (\i -> if i >= 0 then Just i else Nothing) (updated selectionIndexD))
        id
        mempty
        itemsUpdate
        (\k iv u -> do
          v <- holdDyn iv u
          (e, _) <- elDynClass' "div" (("error-item" <>) . bool "" " selected" . (==k) <$> selectionIndexD) $ do
            let imgSrc l = case pSeverity . snd <$> l of
                  Just SevHint -> "/pics/sev-hint.svg"
                  Just SevInfo -> "/pics/sev-hint.svg"
                  Just SevWarning -> "/pics/sev-warning.svg"
                  _ -> "/pics/sev-error.svg"
            elDynAttr "img" (("src" =:) . imgSrc <$> v) $ return ()
            text " "
            dynText $ maybe "" errorLine <$> v
          return $ fmapMaybe id $ tag (current v) (domEvent Dblclick e))
      return . fmapMaybe (fmap (ErrorsGoto . snd) . listToMaybe . M.elems) $ switchDyn (mergeMap <$> eventsD)
    resizeE <- resizeObserver (_element_raw childEl)
    return $ leftmost [ result, goE ]

-- | The active project's problems: the @build:@\/@lsp:@ slices of the
-- problems map keyed by the project's root — the map key after the prefix
-- IS the producing root (nothing active = no rows, like the status bar's
-- counts).  Each problem is paired with that root (for relative display)
-- and its path resolved against it, so the click-to-jump target is
-- absolute.
activeProjectProblems :: Ws -> Map Text [Problem] -> [(FilePath, Problem)]
activeProjectProblems ws probs = case prDir <$> activeProject ws of
    Nothing   -> []
    Just root ->
      let resolve p
            | isAbsolute (pPath p) = p
            | otherwise            = p { pPath = root </> pPath p }
      in [ (root, resolve p)
         | pfx <- ["build:", "lsp:"]
         , p <- M.findWithDefault [] (pfx <> T.pack root) probs ]

-- | One-line description of an error/warning (file + message), used for both
-- the row label and find matching.
errorLine :: (FilePath, Problem) -> Text
errorLine (root, p) = T.pack (makeRelative root (pPath p)) <> ": " <> pMessage p
            & removeIndentation
            & T.lines
            & map removeTrailingWhiteSpace
            & T.intercalate " "

-- | Removes the unnecessary indentation
removeIndentation :: Text -> Text
removeIndentation t = T.intercalate "\n" $ map (T.drop minIndent) l
  where
    l = T.lines t
    minIndent = minimum $ map (T.length . T.takeWhile (== ' ')) l

removeTrailingWhiteSpace :: Text -> Text
removeTrailingWhiteSpace = T.dropWhileEnd isSpace
