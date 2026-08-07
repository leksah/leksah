{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | The SESSIONLESS leksah-window renderer: a leksah window whose leaves are
-- all native views (editors, browser panes, git logs) — no backing tmux
-- session, no control client — drawn as the same absolutely-positioned leaf
-- rects, view layers, focus reconciler and draggable native dividers as the
-- full control-mode widget ('IDE.Web.Widget.TerminalCC').  It lives in its
-- own module, with no CPP, because it is exactly the part of the leksah
-- window system that works everywhere: the in-browser demo (GHC JS backend)
-- renders its showcase editor-beside-browser window with this, while the
-- session-backed renderer stays native-only.
module IDE.Web.Widget.LwView
  ( sessionlessLwWidget
  , publishLwGeom
  , modifyLeksahWindow
  ) where

import Control.Concurrent (forkIO)
import Control.Lens ((^.), over)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', newIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Reflex
       (Dynamic, Event, current, delay, ffilter, ffor, fmapMaybe,
        getPostBuild, holdUniqDyn, leftmost, newTriggerEvent, performEvent_,
        tag, updated)
import Reflex.Dom.Core
       (MonadWidget, blank, dyn_, elAttr', elDynAttr, elDynAttr',
        listWithKey, _element_raw, (=:))
import Language.Javascript.JSaddle
       (JSM, eval, fun, js, js0, js1, js2, jsg, jss, liftJSM, valIsNull,
        valToBool, valToNumber, valToText)

import IDE.App (appUi, withApp)
import IDE.DebugLog (focusLog)
import IDE.Reactive (modifyCell)
import IDE.Web.Ctx (Ctx(..))
import IDE.Web.Model
       (TabKey, leksahWindows,
        LeksahWindow(..), PaneContent(..), PaneKind(..), LeafId(..),
        SplitTree)
import IDE.Web.WindowBridge (setFocusedLeaf)
import IDE.Web.SplitLayout
       (leafRects, LeafRect(..), treeDividers, NativeDivider(..), resizeNode,
        subtreeRects, treeLeafIds)
import IDE.Web.TerminalInput (registerTerminalFocus)

-- | Renderer for a SESSIONLESS leksah window (native views only — no tmux
-- windows yet, so no control client): the same absolutely-positioned leaf
-- rects, view layers, focus tracking and draggable native dividers as the
-- full widget.  Main swaps it for 'terminalCCWidget' the moment the window
-- binds a backing session (⌘D creating a terminal beside the view).
sessionlessLwWidget
  :: forall t m . MonadWidget t m
  => Ctx t
  -> Text                            -- ^ leksah window id
  -> Event t ()                      -- ^ tab selected (refocus pulse)
  -> (TabKey -> Event t () -> Dynamic t Bool -> m ())
                                     -- ^ view pane builder (per-leaf focus
                                     --   pulse + \"is the focused leaf\")
  -> m ()
sessionlessLwWidget ctx lwId selectedE leafViewW = do
    lwD <- holdUniqDyn $ (\i -> M.lookup lwId (i ^. leksahWindows)) <$> cUi ctx
    -- \"Take keyboard focus\" pulses for the leaves, fired by the reconciler
    -- below for the focused leaf only.
    (viewFocusE, fireViewFocus) <- newTriggerEvent
    -- Explicit focus requests, via the sticky registry (an ⌥-open/⌘D into
    -- this window while its tab is still mounting).  The CC widget
    -- re-registers the same key when the window later binds a session.
    (focusReqE, fireFocusReq) <- newTriggerEvent
    pbS <- getPostBuild
    performEvent_ $ ffor pbS $ \_ ->
        liftIO . void $ registerTerminalFocus lwId (fireFocusReq ())
    -- Split-tree geometry for the ⌘-drag pane-move preview (see the
    -- session-backed sibling in TerminalCC).
    geomD <- holdUniqDyn $
        (fmap (\lw -> (lwTree lw, lwZoomed lw))) <$> lwD
    pbGeom <- getPostBuild
    performEvent_ $
        ffor (leftmost [updated geomD, tag (current geomD) pbGeom]) $
            liftJSM . publishLwGeom lwId
    let rectsD = maybe M.empty (\lw -> leafRects (lwZoomed lw) (lwTree lw))
                   <$> lwD
        pct v = T.pack (show (v * (100 :: Double))) <> "%"
        leafStyle :: LeafId -> LeafRect -> M.Map Text Text
        leafStyle (LeafId n) r =
              -- edge-left: see the session-backed sibling in TerminalCC.
              "class" =: ("terminal-cc-leaf"
                          <> (if lrX r == 0 then " edge-left" else ""))
           <> "data-leaf" =: T.pack (show n)
           <> "style" =: ("position:absolute;box-sizing:border-box"
                <> ";left:"   <> pct (lrX r)
                <> ";top:"    <> pct (lrY r)
                <> ";width:"  <> pct (lrW r)
                <> ";height:" <> pct (lrH r)
                <> ";display:" <> (if lrVisible r then "block" else "none"))
    -- Same -3px pull as the session-backed container: the leaf grid's origin
    -- sits on the editor column's boundary-line pixel (the tall divider's
    -- overlay line), so leaf boxes — and the active ring anchored to them —
    -- reach the line exactly.
    (containerEl, _) <- elAttr' "div" ("class" =: "terminal terminal-cc"
                  <> "data-lw" =: lwId
                  <> "style" =: ("position:relative;overflow:hidden"
                                 <> ";margin-left:-3px;margin-top:-3px"
                                 <> ";width:calc(100% + 3px);height:calc(100% + 3px)")) $ do
        _ <- listWithKey rectsD $ \lid rectD0 -> do
            rectD <- holdUniqDyn rectD0
            paneD <- holdUniqDyn $ (>>= (M.lookup lid . lwPanes)) <$> lwD
            viewD <- holdUniqDyn $
                (\case Just (PaneContent (PaneView k) _) -> Just k
                       _                                 -> Nothing) <$> paneD
            fontD <- holdUniqDyn $ (>>= pcFontSize) <$> paneD
            amFocusedD <- holdUniqDyn $
                (\mlw -> (lwFocused =<< mlw) == Just lid) <$> lwD
            (leafEl, _) <- elDynAttr' "div" (leafStyle lid <$> rectD) $
                dyn_ $ ffor viewD $ \case
                    Nothing -> return ()
                    Just k  -> do
                        (vEl, _) <- elDynAttr' "div"
                            ((\mf -> "class" =: "terminal-cc-view-leaf"
                                 <> maybe mempty
                                      (\n -> "style" =:
                                         ("--leksah-mono-size:"
                                          <> T.pack (show n) <> "px")) mf)
                              <$> fontD) $ leafViewW k
                                    (void (ffilter (== lid) viewFocusE))
                                    amFocusedD
                        -- Leaf chrome — see the session-backed sibling.
                        elDynAttr "div"
                            ((\f -> "class" =: ("pane-chrome"
                                <> (if f then " active" else "")))
                              <$> amFocusedD)
                            blank
                        performEvent_ $ ffor (updated fontD) $ \mf ->
                            liftJSM . void $ jsg ("window" :: Text)
                                ^. js2 ("leksahSetLeafFont" :: Text)
                                    (_element_raw vEl)
                                    (maybe (0 :: Int) id mf)
            -- focusin AND mousedown, for the reason spelled out at the other
            -- leaf renderer: a click need not move DOM focus at all.
            pbFoc <- getPostBuild
            performEvent_ $ ffor pbFoc $ \_ -> liftJSM $ do
                let el  = _element_raw leafEl
                    hit = fun $ \_ _ _ -> liftIO . void . forkIO $
                              setFocusedLeaf lwId lid
                void $ el ^. js2 ("addEventListener" :: Text) ("focusin" :: Text) hit
                void $ el ^. js2 ("addEventListener" :: Text) ("mousedown" :: Text) hit
        -- Native dividers (the px→fraction conversion measures the
        -- divider's offsetParent — the positioned container above).
        dividersUniqD <- holdUniqDyn $ maybe [] (treeDividers . lwTree) <$> lwD
        dyn_ $ ffor dividersUniqD $ mapM_ $ \nd -> do
            let styleND =
                    "position:absolute;z-index:6"
                    <> (if ndVertical nd
                          then ";cursor:col-resize;width:7px"
                            <> ";left:calc(" <> pct (ndX nd) <> " - 3px)"
                            <> ";top:" <> pct (ndY nd)
                            <> ";height:" <> pct (ndLen nd)
                          else ";cursor:row-resize;height:7px"
                            <> ";top:calc(" <> pct (ndY nd) <> " - 3px)"
                            <> ";left:" <> pct (ndX nd)
                            <> ";width:" <> pct (ndLen nd))
            (dEl, _) <- elAttr' "div"
                ("class" =: "terminal-cc-native-divider"
                 <> "style" =: styleND) blank
            pbD <- getPostBuild
            performEvent_ $ ffor pbD $ \_ -> liftJSM $ do
                let raw = _element_raw dEl
                raw ^. jss ("__leksahNativeResize" :: Text)
                    (fun $ \_ _ args -> case args of
                      (dv : _) -> do
                        d <- valToNumber dv
                        parent <- raw ^. js ("offsetParent" :: Text)
                        isNull <- valIsNull parent
                        unless isNull $ do
                          rect <- parent ^. js0 ("getBoundingClientRect" :: Text)
                          ext <- valToNumber =<< rect ^. js
                              (if ndVertical nd then "width" :: Text
                                                else "height")
                          let deltaFrac = d / max 1 (ext * ndAxis nd)
                          liftIO . void . forkIO $
                              modifyLeksahWindow lwId $ \lw ->
                                  lw { lwTree = resizeNode (ndPath nd)
                                                 (ndIndex nd) deltaFrac
                                                 (lwTree lw) }
                      _ -> return ())
                void $ jsg ("LeksahNativeDrag" :: Text)
                    ^. js2 ("arm" :: Text) raw (ndVertical nd)
        return ()
    -- ══ MODEL-DRIVEN FOCUS RECONCILER (views only — no tmux panes) ══════
    -- The sessionless sibling of the CC widget's reconciler: turn
    -- 'lwFocused' into DOM focus on model changes / tab select / explicit
    -- requests / build, guarded so a hidden or unfocused window never
    -- steals the keyboard (focus() in a hidden tab is a no-op anyway).
    focusedViewD <- holdUniqDyn $ (\mlw -> do
            lw <- mlw
            l  <- lwFocused lw
            pc <- M.lookup l (lwPanes lw)
            case pcKind pc of
              PaneView _ -> Just l
              _          -> Nothing) <$> lwD
    selectedSettledE <- delay 0 selectedE
    focusReqSettledE <- delay 0 focusReqE
    forcePendRef <- liftIO $ newIORef False
    performEvent_ $ ffor focusReqE $ \_ -> liftIO $ writeIORef forcePendRef True
    let reconcileE = leftmost
          [ fmap ((,) False) (fmapMaybe id (updated focusedViewD))
          -- Tab select = explicit navigation: focus unconditionally.
          , fmap ((,) True)  (fmapMaybe id
              (tag (current focusedViewD) selectedSettledE))
          , fmap ((,) False) (fmapMaybe id (tag (current focusedViewD)
              (leftmost [focusReqSettledE, pbS])))
          ]
    performEvent_ $ ffor reconcileE $ \(forced, l) -> do
        (had, tagName) <- liftJSM $ do
            ae <- jsg ("document" :: Text) ^. js ("activeElement" :: Text)
            h  <- valToBool =<< _element_raw containerEl
                                  ^. js1 ("contains" :: Text) ae
            t  <- valToText =<< ae ^. js ("tagName" :: Text)
            return (h, t)
        pend <- liftIO $ atomicModifyIORef' forcePendRef (\p -> (False, p))
        focusLog $ "[" <> T.unpack lwId <> "] sessionless reconcileFocus had="
            <> show had <> " activeEl=" <> T.unpack tagName
            <> " pend=" <> show pend <> " forced=" <> show forced
        when (forced || had || tagName == "BODY" || pend) . liftIO $
            fireViewFocus l

-- | Publish a leksah window's split-tree geometry to the front end, for the
-- ⌘-drag pane-move preview (leafDragJs reads @window.__leksahLwGeom[lwId]@ on
-- every mousemove — the tracking loop must never round-trip through jsaddle).
-- The candidate universe mirrors 'IDE.Web.SplitLayout.pickDropTarget'
-- exactly: every subtree's fraction rect, or — zoomed — just the zoomed leaf
-- covering the whole container.  @Nothing@ (window gone / remote tab)
-- publishes @null@ so the drag shows its no-op state there.
publishLwGeom :: Text -> Maybe (SplitTree, Maybe LeafId) -> JSM ()
publishLwGeom i mb = void . eval $
    "(window.__leksahLwGeom = window.__leksahLwGeom || {})['" <> i <> "'] = "
    <> payload <> ";"
  where
    payload = case mb of
      Nothing -> "null"
      Just (tree, zoomed) ->
        "{subs:[" <> T.intercalate "," (map sub (universe tree zoomed)) <> "]}"
    universe tree zoomed = case zoomed of
      Just z | z `elem` treeLeafIds tree -> [ ([], Just z, (0, 0, 1, 1)) ]
      _ -> subtreeRects tree
    sub (p, ml, (x, y, w, h)) =
      "{p:[" <> T.intercalate "," (map (T.pack . show) p) <> "]"
      <> ",leaf:" <> maybe "null" (\(LeafId n) -> T.pack (show n)) ml
      <> ",x:" <> num x <> ",y:" <> num y
      <> ",w:" <> num w <> ",h:" <> num h <> "}"
    num = T.pack . show

-- | Mutate a leksah window's shared layout through the global app (the
-- widget has no reflex path back to Main's mutation stream).  No-op before
-- boot completes.
modifyLeksahWindow :: Text -> (LeksahWindow -> LeksahWindow) -> IO ()
modifyLeksahWindow i f = withApp $ \app ->
    modifyCell (appUi app) (over leksahWindows (M.adjust f i))
