{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Tabs
  ( tabsCss
  , tabsWidget
  ) where

import Control.Arrow (Arrow(..))
import Control.Lens ((^..))

import Data.Bool (bool)
import Data.Foldable (foldr')
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as M
       (toList, fromList, elems, filter, delete, lookup, union)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S (member, fromList)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Tuple (swap)

import Clay
       (nowrap, whiteSpace, marginTop, scroll, overflow, white,
        color, fontSize, borderStyle, textDecoration, middle,
        borderRadius, padding, hover, (#), background, opacity,
        margin, px, height, cursor, cursorDefault, (?), (-:), Css,
        Color(..), VerticalAlign(..), Auto(..), Hidden(..), None(..), Cursor(..))

import Reflex
       (foldDyn, holdDyn, holdUniqDyn, listViewWithKey, listWithKey, switchDyn,
        mergeMap, leftmost, attachWith, current, ffilter, fmapMaybe,
        constDyn, Dynamic)
import Reflex.Dom.Core
       (elDynAttr', elAttr, blank, MonadWidget, (=:),
        divClass, Event, domEvent, EventName(..))

import IDE.Web.Theme (selectionColor, dimColor, dimOpacity)

tabsCss :: Css
tabsCss = do
    ".tab-buttons" ? do
        background (Rgba 0 0 0 1.0)
        height (px 20)
        overflow scroll
        -- A flex row so tabs can be ordered (via the CSS `order` property) by
        -- flipper/MRU position — the active tab is order 0, i.e. leftmost.
        -- align-items:flex-start keeps the buttons at the top (as the previous
        -- inline layout did); the default (stretch/center) sat them too low.
        "display" -: "flex"
        "align-items" -: "flex-start"
        "position" -: "relative"
    -- The bottom bar's tab row also gets a line on TOP, separating it from the
    -- editor above.  (Side/editor rows sit under the toolbar and don't need one;
    -- and the wide1 top divider is hidden in bottom-bar auto-hide, so the tab
    -- row carries its own top line here.)
    ".tab-buttons.area-wide1::before" ? do
        "content" -: "''"
        "position" -: "absolute"
        "left" -: "0"
        "right" -: "0"
        "top" -: "0"
        "height" -: "1px"
        "background" -: "rgb(128,128,128)"
        "pointer-events" -: "none"
    -- A blank item the width of the side pane, kept last, so when the auto-hide
    -- side pane slides over the content the part that's covered is this spacer
    -- rather than a real tab.
    ".tab-buttons .tab-spacer" ? do
        "order" -: "99999"
        "flex" -: "0 0 300px"
    -- The wrapper is the visual tab (close × on the left + the label button); it
    -- carries the hover and selected highlight so the × is inside the blue.
    ".tab-buttons .tab-wrap" ? do
        verticalAlign middle
        borderRadius (px 3) (px 3) (px 3) (px 3)
        -- A little bottom padding extends the highlight box back down to where it
        -- was before the close button moved the highlight onto this wrapper.
        padding (px 0) (px 0) (px 2) (px 0)
        whiteSpace nowrap
        cursor cursorDefault
        "position" -: "relative"
    ".tab-buttons .tab-wrap" # hover ?
        background (Rgba 61 96 150 1.0)
    ".tab-buttons .tab-wrap.selected" ?
        background selectionColor
    -- The flipper's live selection tints its tab button with the hover colour
    -- (set by leksahSetFlipSel while the flipper is open).
    ".tab-buttons .tab-wrap.leksah-flip-sel" ?
        background (Rgba 61 96 150 1.0)
    ".tab-buttons button" ? do
        verticalAlign middle
        padding (px 0) (px 10) (px 0) (px 2)
        margin (px 0) (px 0) (px 0) (px 0)
        textDecoration none
        borderStyle none
        fontSize (px 13)
        background (Rgba 0 0 0 0.0)
        -- De-emphasis instead of emphasis: non-selected tabs' labels + icons are
        -- dimmed to light grey; the selected (and hovered / flipper-preview) tab
        -- is restored to full-brightness white below.
        color dimColor
        cursor cursorDefault
    -- Leading tree icon inside a side-pane tab button (Workspace/Terminals/…):
    -- white B&W SVG sized to the 20px row, nudged to sit centred with the label.
    ".tab-buttons button img.tab-icon" ? do
        height (px 14)
        verticalAlign middle
        "margin" -: "0 4px 2px 2px"
        opacity dimOpacity
    -- Full brightness for the selected / hovered / flipper-preview tab.
    ".tab-buttons .tab-wrap.selected button" ? color white
    ".tab-buttons .tab-wrap.leksah-flip-sel button" ? color white
    ".tab-buttons .tab-wrap:hover button" ? color white
    ".tab-buttons .tab-wrap.selected button img.tab-icon" ? opacity 1
    ".tab-buttons .tab-wrap.leksah-flip-sel button img.tab-icon" ? opacity 1
    ".tab-buttons .tab-wrap:hover button img.tab-icon" ? opacity 1
    -- A bell (needs-input) terminal tab is pulled to full brightness — name AND
    -- icon — even when it is neither selected nor hovered, so the alert stands
    -- out (matching the moused-over look).
    ".tab-buttons .tab-wrap:has(img[src*='tree-window-bell']) button" ? color white
    ".tab-buttons .tab-wrap:has(img[src*='tree-window-bell']) button img.tab-icon" ? opacity 1
    ".tab-buttons .tab-close" ? do
        verticalAlign middle
        color white
        padding (px 0) (px 4) (px 0) (px 8)
        fontSize (px 13)
        cursor cursorDefault
    ".tab" ? do
        marginTop (px 20)
        height auto
        overflow hidden

tabsWidget
  :: (MonadWidget t m, Ord k, Show k, Eq v)
  => Map k (Text, v)
  -> Map Text k
  -- | The wide0 (editor/terminal) tabs owned by THIS OS window, in MRU/flip
  -- order, injected from shared per-window state.  wide0 membership and order
  -- come entirely from here (not the open/close events, which now carry only the
  -- fixed side/bottom-bar tabs) so a tab can belong to exactly one window and be
  -- moved between windows by mutating the shared state.
  -> Dynamic t [(k, v)]
  -> Event t (Map k (Text, v))
  -> Event t [k]                  -- ^ tabs to close (removed from the bar)
  -> Event t (Map Text k)
  -> Event t [k]                  -- ^ restore the recent (MRU/flipper) order
  -> Event t Text                 -- ^ @show@-key of a tab that just received focus (moved to MRU front)
  -- | Render the button(s) for one tab key in the bar, given: its grid area, the
  -- key, its value, whether it is the visible tab in its area, and its (wide0) MRU
  -- slot index (@Nothing@ off the wide0 row, where no CSS @order@ is applied).
  -- Returns the combined @(area := key selections, keys to close)@ over however
  -- many buttons it draws.  Most keys draw a single button, but a terminal draws
  -- one per tmux window (all selecting the one session body) — so buttons and
  -- bodies need not be 1:1, and the callback can sub-order within a slot.
  -> (Text -> k -> Dynamic t v -> Dynamic t Bool -> Dynamic t (Maybe Int)
        -> m (Event t (Map Text k, [k])))
  -> (k -> Event t () -> Dynamic t v -> m (Event t e))  -- ^ tab body (2nd arg fires when selected)
  -> m ( Dynamic t [(Text, k)], Event t (Map k e), Dynamic t (Map Text k)
       , Dynamic t (Maybe k)    -- ^ the most-recently focused pane (active pane)
       , Event t [k])           -- ^ close (×) button clicks
tabsWidget initialTabs initialVisibleTabs wide0OrderD openTabE closeTabE selectTabE setRecentE focusedTabE mkButtons mkTab = mdo
  let selectOrOpenTab = selectTabE' <> selectTabE <> reselectE
                          <> (M.fromList . map (swap . second fst) . M.toList <$> openTabE)
      -- When a *visible* tab is closed, point its area at a sibling tab (if any)
      -- so the area doesn't go blank.  Closing a background tab changes nothing.
      -- The sibling is chosen in MRU order (recentTabs) — the closed tab is at the
      -- MRU front, so the first surviving sibling in its area is the *second* entry
      -- in that area's tab-button list (the ⌘1 button), i.e. the one used most
      -- recently before it.  (Ordering by the tab Map's keys instead would jump to
      -- an arbitrary sibling.)
      reselectE = attachWith
        (\(vis, tabs, rt) ks -> M.fromList
            [ (gridArea, sib)
            | (gridArea, k) <- M.toList vis
            , k `elem` ks
            , sib <- take 1 [ k' | (_, k') <- rt
                                 , Just (a, _) <- [M.lookup k' tabs]
                                 , a == gridArea, k' `notElem` ks ] ])
        (current ((,,) <$> visibleTabs <*> tabsD <*> recentTabs)) closeTabE
  -- wide0 membership + order come from the injected shared per-window state.
  let wide0MapD  = M.fromList . map (\(k, v) -> (k, ("wide0", v))) <$> wide0OrderD
      wide0KeysD = map fst <$> wide0OrderD
  visibleTabs <- foldDyn (<>) initialVisibleTabs selectOrOpenTab
  -- The fixed side/bottom-bar tabs (open/close carry only these now); wide0 is
  -- unioned in from the injected shared state.
  barTabsD <- foldDyn ($) initialTabs $ leftmost
    [ (<>) <$> openTabE
    , (\ks m -> foldr' M.delete m ks) <$> closeTabE ]
  let tabsD = M.union <$> wide0MapD <*> barTabsD
  allVisibleTabs <- holdUniqDyn $ S.fromList . M.elems <$> visibleTabs
  -- Recent (MRU) tabs for the flipper.  This must add *every* opened tab, not
  -- the area-keyed `selectOrOpenTab` map: restoring a session opens many tabs in
  -- one area (wide0) at once, and keying by area would collapse them to one.
  -- (Visibility still dedups by area — only one tab is visible per area.)
  let openedPairsE = leftmost
        [ map (\(k, (a, _)) -> (a, k)) . M.toList <$> openTabE
        , M.toList <$> (selectTabE' <> selectTabE <> reselectE) ]
      -- Restore a saved MRU order: order the currently-open tabs by the saved
      -- key list, appending any tabs not mentioned (e.g. always-present panes).
      reorderE = attachWith
        (\tabs order ->
            [ (a, k) | k <- order, Just (a, _) <- [M.lookup k tabs] ]
            ++ [ (a, k) | (k, (a, _)) <- M.toList tabs, k `notElem` order ])
        (current tabsD) setRecentE
  -- MRU of the fixed side/bottom-bar tabs only (wide0's MRU lives in the shared
  -- per-window order, prepended below).
  barRecent <- foldDyn ($) (map swap . M.toList $ fst <$> initialTabs) $ leftmost
    [ const <$> reorderE
    , (\new old -> new <> filter ((`notElem` map snd new) . snd) old) <$> openedPairsE
    , (\ks old -> filter ((`notElem` ks) . snd) old) <$> closeTabE
    -- Focusing a pane/editor/terminal (by any means — click, the keyboard, or the
    -- initial focus at start-up) moves it to the front of the MRU/flipper order.
    , (\str old -> case [ ak | ak@(_, k') <- old, T.pack (show k') == str ] of
                     (ak@(_, k):_) -> ak : filter ((/= k) . snd) old
                     []            -> old) <$> focusedTabE ]
  -- The full MRU/flipper order: this window's wide0 tabs (already MRU-ordered in
  -- the shared state) first, then the side/bottom-bar tabs.  The wide0 CSS
  -- `order` (below) indexes into this, so the active wide0 tab is slot 0.
  let recentTabs = (\ks rest -> map (\k -> ("wide0", k)) ks
                                ++ filter ((`notElem` ks) . snd) rest)
                     <$> wide0KeysD <*> barRecent
  tabBtnE <- fmap (fmap (mconcat . (^.. traverse . traverse))) $
        listViewWithKey visibleTabs $ \gridArea visibleTab -> do
    let tabs = M.filter ((gridArea ==) . fst) <$> tabsD
        -- The editor/terminal row (where opening a file or terminal puts it) is
        -- the only one that reorders by flipper/MRU order and gets the end spacer.
        isEditorArea = gridArea == "wide0"
    divClass ("tab-buttons area-" <> gridArea) $ do
      r <- listViewWithKey tabs $ \k v -> do
        let isVisibleD = (== k) <$> visibleTab
            -- Only the wide0 (editor/terminal) row reorders by flipper/MRU order
            -- via the CSS `order` property (DOM order stays keyed by k): the
            -- active tab is most-recent, so it gets slot 0 and sits leftmost.
            -- Other rows keep their source order (Nothing → no `order` style).
            baseOrderD
              | isEditorArea = (\rt -> Just (fromMaybe 9998 (elemIndex k (map snd rt)))) <$> recentTabs
              | otherwise    = constDyn Nothing
        mkButtons gridArea k (snd <$> v) isVisibleD baseOrderD
      -- A blank spacer the width of the side pane, kept last, only on the wide0
      -- row (so the auto-hide side pane covers blank space, not a real tab).
      if isEditorArea
        then elAttr "div" ("class" =: "tab-spacer") blank
        else blank
      return r
  let selectTabE'  = fst <$> tabBtnE
      closeBtnE    = fmapMaybe (\(_, ks) -> if null ks then Nothing else Just ks) tabBtnE
  -- Render the tab bodies, capturing each one's mouse-down so we know which pane
  -- is active (the find bar routes to it; this replaces a JS focus callback).
  tabResultsD <- listWithKey tabsD $ \k v -> do
    gridAreaD <- holdUniqDyn $ fst <$> v
    visibleD <- holdUniqDyn $ S.member k <$> allVisibleTabs
    -- Fires whenever this tab is explicitly selected/opened (tab button, list,
    -- flipper, or re-select of the already-visible tab) — used to (re)focus.
    let selectedE = () <$ ffilter (k `elem`) (M.elems <$> selectOrOpenTab)
    let attrD = do
            visible <- visibleD
            gridArea <- gridAreaD
            return $
                 ("class" =: ("tab area-" <> gridArea))
              <> ("data-tabkey" =: T.pack (show k))   -- focusin → MRU reorder (see focusTabJs)
              <> bool ("style" =: "visibility:hidden;") mempty visible
    (el, ev) <- elDynAttr' "div" attrD $
      mkTab k selectedE (snd <$> v)
    return (ev, k <$ domEvent Mousedown el)
  let tabEvents = switchDyn $ mergeMap . fmap fst <$> tabResultsD
      activeE   = switchDyn $ leftmost . map snd . M.elems <$> tabResultsD
  -- The active pane changes both when a tab *body* is pressed (activeE) and when
  -- a tab *button* in the bar is clicked (selectTabE') — the latter so clicking a
  -- side/bottom-bar tab (Terminals, Metadata, …) also focuses it and promotes it
  -- in the flipper MRU, not just clicking inside its body.
  activePane <- holdDyn Nothing $ Just <$>
    leftmost [ activeE, fmapMaybe (listToMaybe . M.elems) selectTabE' ]
  return (recentTabs, tabEvents, visibleTabs, activePane, closeBtnE)
