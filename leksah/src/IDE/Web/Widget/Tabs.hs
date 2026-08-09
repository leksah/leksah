{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Tabs
  ( tabsCss
  , tabsWidget
  ) where

import Control.Arrow (Arrow(..))
import Control.Lens ((^..), (^.))
import Control.Monad (void)

import Data.Bool (bool)
import Data.Foldable (foldr')
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as M
       (toList, fromList, elems, filter, delete, lookup, union, member)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S (member, fromList)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Tuple (swap)

import Clay
       (nowrap, whiteSpace, marginTop, scroll, overflow,
        color, fontSize, borderStyle, textDecoration, middle,
        borderRadius, padding, hover, (#), background, opacity,
        margin, px, height, cursor, cursorDefault, (?), (-:), Css,
        Color(..), VerticalAlign(..), Auto(..), Hidden(..), None(..), Cursor(..))

import Language.Javascript.JSaddle (jsg, js1, liftJSM)

import Reflex
       (foldDyn, holdDyn, holdUniqDyn, listViewWithKey, listWithKey, switchDyn,
        mergeMap, mergeWith, leftmost, attachWith, attachWithMaybe, current,
        ffilter, fmapMaybe, constDyn, Dynamic, sample, updated, never,
        switchHold, ffor)
import Reflex.Dom.Core
       (elDynAttr', elAttr, blank, (=:),
        divClass, Event, domEvent, EventName(..), dyn, _element_raw)

import IDE.Web.Theme (selectionColor, dimColor, dimOpacity, bgColor, accentHoverColor, fgColor, onAccentColor)
import IDE.Web.Frame (MonadWidget, performEvent_)

tabsCss :: Css
tabsCss = do
    -- A hidden (inactive) tab body uses `visibility:hidden` — not `display:none`
    -- — so its editors keep their layout/scroll instead of collapsing.  But
    -- Monaco and CodeMirror set an inline `visibility:visible` on their own
    -- layers, which overrides the inherited hidden and leaves e.g. the git-log
    -- side-by-side diff painted on top of whatever tab is now active.  Force
    -- those layers hidden; the editors' inline style isn't !important, so a
    -- stylesheet !important wins.
    ".tab-hidden .monaco-editor" ? ("visibility" -: "hidden !important")
    ".tab-hidden .editor"        ? ("visibility" -: "hidden !important")
    ".tab-hidden .cm-editor"     ? ("visibility" -: "hidden !important")
    ".tab-hidden .cm-mergeView"  ? ("visibility" -: "hidden !important")
    ".tab-buttons" ? do
        background bgColor
        height (px 20)
        overflow scroll
        -- A flex row so tabs can be ordered (via the CSS `order` property) by
        -- flipper/MRU position — the active tab is order 0, i.e. leftmost.
        -- align-items:flex-start keeps the buttons at the top (as the previous
        -- inline layout did); the default (stretch/center) sat them too low.
        "display" -: "flex"
        "align-items" -: "flex-start"
        "position" -: "relative"
    -- (The bottom bar's 1px top line, separating it from the editor above, is
    -- drawn by the .wide1-divider's border-top in Layout.hs — shown docked and
    -- floated up to the bar's top edge when revealed in auto-hide — so the tab
    -- row needs no ::before line of its own.)
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
        background accentHoverColor
    ".tab-buttons .tab-wrap.selected" ?
        background selectionColor
    -- The flipper's live selection tints its tab button with the hover colour
    -- (set by leksahSetFlipSel while the flipper is open).
    ".tab-buttons .tab-wrap.leksah-flip-sel" ?
        background accentHoverColor
    -- The ⌘-drag pane move's hovered (peeked) tab button (leafDragJs).
    ".tab-buttons .tab-wrap.leksah-drag-peek" ?
        background accentHoverColor
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
    -- Full brightness for the selected / hovered / flipper-preview tab.  The
    -- SELECTED tab sits on the dark-blue selection background in both themes, so
    -- its label stays light ('onAccentColor') rather than 'fgColor' (which flips
    -- to near-black in light mode).  Hover / flip-preview use the PALE accent in
    -- light mode, where 'fgColor' (dark) is the readable choice — so keep it.
    ".tab-buttons .tab-wrap.selected button" ? color onAccentColor
    ".tab-buttons .tab-wrap.leksah-flip-sel button" ? color fgColor
    ".tab-buttons .tab-wrap:hover button" ? color fgColor
    ".tab-buttons .tab-wrap.selected button img.tab-icon" ? opacity 1
    ".tab-buttons .tab-wrap.leksah-flip-sel button img.tab-icon" ? opacity 1
    ".tab-buttons .tab-wrap:hover button img.tab-icon" ? opacity 1
    -- A bell (needs-input) terminal tab is pulled to full brightness — name AND
    -- icon — even when it is neither selected nor hovered, so the alert stands
    -- out (matching the moused-over look).
    ".tab-buttons .tab-wrap:has(img[src*='tree-window-bell']) button" ? color fgColor
    ".tab-buttons .tab-wrap:has(img[src*='tree-window-bell']) button img.tab-icon" ? opacity 1
    ".tab-buttons .tab-close" ? do
        verticalAlign middle
        color fgColor
        padding (px 0) (px 4) (px 0) (px 8)
        fontSize (px 13)
        cursor cursorDefault
    -- The × is inside the tab-wrap, so on a selected tab it's over the blue —
    -- keep it light there too (matches the label).
    ".tab-buttons .tab-wrap.selected .tab-close" ? color onAccentColor
    ".tab" ? do
        marginTop (px 20)
        height auto
        overflow hidden
    -- Tabs are programmatically focusable (tabindex -1, see tabsWidget) so a
    -- widget-less panel can hold DOM focus; never draw a focus ring for it —
    -- the active-pane highlight is the anchored overlay.
    ".tab:focus" ? ("outline" -: "none")
    -- The active-pane glow+ring overlay (terminalCss ".leksah-pane-glow")
    -- anchors to the TAB body itself for tabs without per-pane markers
    -- (Preferences, Workspace, Tasks, …); LW/CC tabs anchor at their
    -- internal pane markers / leaf chrome instead.  The tab draws nothing —
    -- the ring lives on the overlay.
    ".tab.tab-active:not(:has(.terminal-cc))" ?
        ("anchor-name" -: "--leksah-active-pane")
    -- Side (tall) tabs sit on the screen's left edge, where no line may be
    -- drawn: they anchor the border-left-free overlay variant instead (see
    -- terminalCss ".leksah-pane-glow.glow-tall").
    ".tab.area-tall.tab-active:not(:has(.terminal-cc))" ?
        ("anchor-name" -: "--leksah-active-pane-tall")
    -- Bottom-bar tabs anchor the wide1 variant, which rides the bar's
    -- parked/revealed transforms in auto-hide mode (Layout.hs).
    ".tab.area-wide1.tab-active:not(:has(.terminal-cc))" ?
        ("anchor-name" -: "--leksah-active-pane-wide1")

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
  -- | PEEK a tab: make it the visible tab of its area WITHOUT activating it —
  -- no 'activePane' change, no MRU promotion, no @selectedE@ pulse (which
  -- would steal DOM focus).  Used by the ⌘-drag pane move to show the
  -- hovered tab's window as a drop target; the drop/cancel always ends in a
  -- real select, which puts visibility back under normal control.
  -> Event t (Map Text k)
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
tabsWidget initialTabs initialVisibleTabs wide0OrderD openTabE closeTabE selectTabE setRecentE focusedTabE peekTabE mkButtons mkTab = mdo
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
  -- peekTabE joins the visibility fold ONLY — never selectOrOpenTab (that
  -- would drive activePane / the MRU / each tab's selectedE focus pulse).
  visibleTabs <- foldDyn (<>) initialVisibleTabs
                   (mergeWith (<>) [selectOrOpenTab, peekTabE])
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
  -- barRecent is also filtered to tabs that still EXIST: selecting a wide0 tab
  -- adds it to barRecent (openedPairsE), but a wide0 close goes through the
  -- shared state (closeWide0), never closeTabE — without the filter a closed
  -- editor/git-log/Preferences tab would linger in the flipper forever.
  let recentTabs = (\ks rest tabs ->
                       map (\k -> ("wide0", k)) ks
                       ++ filter (\(_, k) -> k `notElem` ks && k `M.member` tabs) rest)
                     <$> wide0KeysD <*> barRecent <*> tabsD
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
    -- Is this the window's active (most-recently-focused) tab?  Drives the
    -- 'tab-active' class, which is the per-window gate for the pane chrome's
    -- active ring (exactly one lit ring per OS window; see terminalCss
    -- ".terminal-cc-hl" / ".pane-chrome" and the '.tab.tab-active' outline).
    isActiveD <- holdUniqDyn $ (== Just k) <$> activePane
    -- Fires whenever this tab is explicitly selected/opened (tab button, list,
    -- flipper, or re-select of the already-visible tab) — used to (re)focus.
    let selectedE = () <$ ffilter (k `elem`) (M.elems <$> selectOrOpenTab)
    let attrD = do
            visible <- visibleD
            gridArea <- gridAreaD
            act <- isActiveD
            return $
                 -- 'tab-hidden' (see 'tabsCss') force-hides editor layers that set
                 -- their own inline visibility, so a hidden tab's Monaco/CM diff
                 -- doesn't paint over the active one.
                 ("class" =: ("tab area-" <> gridArea <> bool " tab-hidden" "" visible
                              <> bool "" " tab-active" act))
              <> ("data-tabkey" =: T.pack (show k))   -- focusin → MRU reorder (see focusTabJs)
              -- Programmatically focusable: a selected tab whose body grabs no
              -- focus of its own (Preferences, Review, …) receives DOM focus
              -- directly (leksahFocusTabBody below), so the active pane / MRU
              -- / highlight follow the select like any other pane.
              <> ("tabindex" =: "-1")
              <> bool ("style" =: "visibility:hidden;") mempty visible
    (el, ev) <- elDynAttr' "div" attrD $ do
      -- LAZY wide0 bodies: an editor/terminal/browser tab that is not visible
      -- builds NOTHING until the first time it is shown (then stays built, so
      -- switching back is instant).  Restoring a session used to mount every
      -- saved tab's editors/xterms in one giant frame cascade — measured
      -- 42-50s to first terminal content in ghci mode, ~all of it this.
      -- Deferred bodies are safe because the state they render lives OUTSIDE
      -- the widget (leksahWindows / files / tmux); the widgets' own
      -- create-time paths (postBuild reconciler, focus-on-create,
      -- requestReplay-at-mount, sticky pending-focus) already handle
      -- select-then-mount, since that is exactly what ⌘D/open always did.
      -- The fixed side/bottom-bar tabs stay EAGER: they are few, cheap, and
      -- some carry side effects that must run from boot (bell announcements,
      -- error routing).
      area0 <- sample (current gridAreaD)
      vis0  <- sample (current visibleD)
      if area0 /= "wide0" || vis0
        then mkTab k selectedE (snd <$> v)
        else do
          mountedD <- holdUniqDyn =<< foldDyn (||) False (updated visibleD)
          evE <- dyn $ ffor mountedD $ \m ->
              if m then mkTab k selectedE (snd <$> v) else return never
          switchHold never evE
    -- On every explicit select, hand the tab body DOM focus unless a widget
    -- inside grabs it itself (see leksahFocusTabBody in Main.hs) — panels
    -- like Preferences otherwise leave focus (and thus the active pane, MRU
    -- and highlight) stuck on the previously-focused pane.
    performEvent_ $ ffor selectedE $ \_ -> liftJSM . void $
        jsg ("window" :: Text) ^. js1 ("leksahFocusTabBody" :: Text)
            (_element_raw el)
    return (ev, k <$ domEvent Mousedown el)
  let tabEvents = switchDyn $ mergeMap . fmap fst <$> tabResultsD
      activeE   = switchDyn $ leftmost . map snd . M.elems <$> tabResultsD
  -- The active pane changes when a tab *body* is pressed (activeE); when a tab
  -- *button* in the bar is clicked (selectTabE') — so clicking a side/bottom-bar
  -- tab (Terminals, Metadata, …) also focuses it and promotes it in the flipper
  -- MRU, not just clicking inside its body; and when focus lands in a tab body by
  -- ANY route (focusedKeyE) — the programmatic focus a pane takes when opened
  -- from the workspace (Open Terminal Here, opening a file / Claude session).
  -- Without the last, a workspace-opened pane had keyboard focus but was not the
  -- active pane, so ⌘W / Find ignored it until a redundant click (mousedown).
  -- focusedTabE carries the tab's show-key (focusTabJs, a document focusin
  -- listener); map it back to the live TabKey.
  let focusedKeyE = attachWithMaybe
        (\tabs str -> listToMaybe [ k | (k, _) <- M.toList tabs, T.pack (show k) == str ])
        (current tabsD) focusedTabE
  -- holdUniqDyn: focus can report the same pane twice (a synthetic focus, then
  -- the real focusin it triggers), which would otherwise double the wide0
  -- activate / resync fan-out on every focus.  Consumers read `current` or act
  -- idempotently on `updated`, so collapsing identical values is safe.
  -- selectOrOpenTab (not just the bar clicks): a tab selected by the native
  -- menu, the flipper, close-succession, or an open request becomes the
  -- active pane too — it is the tab the user is now looking at, and the
  -- pane-chrome ring follows 'activePane'.  Only SINGLE-target selects count:
  -- a multi-area map is a session restore repopulating every area at once,
  -- not the user turning to a tab (it would crown an arbitrary area's tab).
  -- Focus-driven arms still win when they fire in the same frame.
  activePane <- holdUniqDyn =<< holdDyn Nothing (Just <$>
    leftmost [ activeE, focusedKeyE
             , fmapMaybe (\m -> case M.elems m of [k] -> Just k; _ -> Nothing)
                         selectOrOpenTab ])
  return (recentTabs, tabEvents, visibleTabs, activePane, closeBtnE)
