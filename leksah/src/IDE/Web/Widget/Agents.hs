{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- | The \"Agents\" tree pane (lives on the side, with the Workspace, Terminals
-- and Metadata trees).  It answers "what are my agents doing?" — the question
-- the status light can only answer with a count.
--
--   * one row per Claude Code session, nested under the agent that FORKED it
--     (the lineage "IDE.Web.AgentInfo" records at fork time), with a status
--     glyph: ▲ needs you, ◆ working, ● idle, ◎ starting, ○ exited;
--   * expand a row and it shows what that agent says it is doing — a small HTML
--     description it wrote itself, links to its PRs and CI builds included;
--   * ⟳ on the right asks that agent to refresh its title and description
--     (a real turn in its conversation — see 'agentRefreshPrompt'); on an exited
--     row it becomes ✕, which forgets it;
--   * clicking a row brings that agent's pane to the front, or — if it has
--     exited — resumes the session where it left off.
--
-- Links in a description open in the OS browser; hold ⌥ (⌥⇧ for the other
-- direction) to open one in a leksah browser pane split instead.  The click is
-- routed in JS ('agentLinksJs' → @window.__leksahOpenExtUrl@, defined in
-- "IDE.Web.Main"), because a description's anchors are deliberately inert: the
-- sanitizer moves @href@ to @data-href@, so nothing can navigate the page.
module IDE.Web.Widget.Agents
  ( agentsCss
  , agentsWidget
  , agentLinksJs
  ) where

import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)

import Data.Map (Map)
import qualified Data.Map as M (fromList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Clay
       (Css, (?), (#), (|>), (-:), auto, backgroundImage, bold, borderRadius,
        borderStyle, color, cursor, cursorDefault, display, flex, fontSize,
        fontStyle, italic,
        fontWeight, height, hidden, hover, none, opacity, overflow, padding,
        pct, pointer, px, textDecoration, underline, vGradient,
        None(..), Cursor(..))
import Clay.Stylesheet (key)

import Reflex
       (Dynamic, Event, ffor, getPostBuild, holdDyn, holdUniqDyn, leftmost,
        listViewWithKey, never, newTriggerEvent, switchHold,
        tagPromptlyDyn, tickLossyFromPostBuildTime)
import Reflex.Dom.Core
       (blank, divClass, domEvent, dyn, dyn_, dynText, el, elAttr,
        elAttr', elClass, elClass', elDynAttr, elDynAttr', elDynHtmlAttr',
        text, EventName(..), (=:), _element_raw)
import Language.Javascript.JSaddle (liftJSM, jsg, js1)

import IDE.Web.Agent (agentSend, showAgentPane)
import IDE.Web.AgentInfo
       (AgentNode(..), agentForest, agentRefreshPrompt, dismissAgent)
import IDE.Web.Claude (ClaudeCmd(..), runClaudeCmd)
import IDE.Web.Theme
       (btnBottomColor, btnHoverBottomColor, btnHoverTopColor, btnTopColor,
        dimColor, dimOpacity, fgColor, fgMutedColor, hoverColor, selectionColor)
import IDE.Web.Widget.Tree (treeItem)
import IDE.Web.Frame (MonadWidget, performEvent_)

-- | The pane.  Polls 'agentForest' (cheap — it reads the status poll's cache),
-- and renders it with keyed lists so a poll that changes one agent's state
-- leaves every other row — and every expanded description — exactly as it was.
agentsWidget :: forall t m . MonadWidget t m => m (Event t ())
agentsWidget = do
  (root, _) <- elClass' "div" "agents leksah-nav" $ do
    pb   <- getPostBuild
    tick <- tickLossyFromPostBuildTime 2
    (forestE, fireForest) <- newTriggerEvent
    -- The read runs OFF the reflex thread: it stats transcripts of exited
    -- agents, and a synchronous performEvent would hitch the whole UI.
    let refresh = void . forkIO $ agentForest >>= fireForest
    forestD <- holdUniqDyn =<< holdDyn [] forestE
    -- A click that finds no pane used to do nothing at all, which reads as a
    -- broken row rather than as the fact it is: the session is running, but not
    -- in a terminal of leksah's tmux (another terminal, another machine, or —
    -- like a `claude` background job — no terminal at all).  Say so instead.
    -- Fired from the click's own thread, shown on the frame thread.
    (missE, fireMiss) <- newTriggerEvent
    performEvent_ $ ffor missE $ \msg -> liftJSM . void $
      jsg ("window" :: Text) ^. js1 ("__leksahBridgeToast" :: Text) (msg :: Text)
    hdrE <- divClass "agents-head" $ do
      elClass "span" "agents-head-label" . dynText $ ffor forestD $ \f ->
        case count f of
          0 -> "No agents running"
          1 -> "1 agent"
          n -> T.pack (show n) <> " agents"
      actionBtn "⟳" "Re-read the agent list"
    performEvent_ $ liftIO refresh <$ leftmost [() <$ pb, () <$ tick, hdrE]
    -- Nothing running: say how one starts, rather than showing an empty box.
    dyn_ $ ffor (null <$> forestD) $ \empty' -> if not empty' then blank else
      divClass "agents-empty" $ text
        "A Claude session in a terminal here can start one with \
        \`leksah-cmd agent fork 'do this'`, and the Tasks pane starts one per \
        \queued task."
    -- Top-level agents come up EXPANDED: a root is usually the session you are
    -- talking to, and what it says about itself is the reason to look here.
    -- Anything it forked stays collapsed, so a deep tree still opens small.
    void . el "ul" $ agentLevel True refresh fireMiss (byKey <$> forestD)
  -- Arm the description links on THIS pane's root element.  The handle, never
  -- document.querySelector: at postBuild the div isn't attached in wkwebview's
  -- batched DOM, and setting a property on null aborts the build batch (which
  -- shows up as a blank UI, not as an error).
  pb <- getPostBuild
  performEvent_ $ ffor pb $ \_ -> liftJSM . void $
    jsg ("LeksahAgentLinks" :: Text) ^. js1 ("arm" :: Text) (_element_raw root)
  return never
  where
    count ns = sum [ 1 + count (anChildren n) | n <- ns ]

byKey :: [AgentNode] -> Map Text AgentNode
byKey ns = M.fromList [ (anSession n, n) | n <- ns ]

-- | One level of the tree: a keyed list, so rows persist across polls (an
-- unkeyed rebuild would collapse whatever the user had expanded).  @open@ is
-- how a row of this level STARTS (see 'agentsWidget'); a row created later by
-- the poll starts the same way, and a collapse the user made survives, because
-- the keyed list never rebuilds the rows it already has.
agentLevel
  :: MonadWidget t m
  => Bool -> IO () -> (Text -> IO ()) -> Dynamic t (Map Text AgentNode)
  -> m (Event t ())
agentLevel open refresh miss mD = do
  e <- listViewWithKey mD (\_ nD -> agentNodeW open refresh miss nD)
  return (() <$ e)

-- | One agent: its row, and — only while expanded, which is the point — its
-- description and the agents it forked.
agentNodeW
  :: forall t m . MonadWidget t m
  => Bool -> IO () -> (Text -> IO ()) -> Dynamic t AgentNode -> m (Event t ())
agentNodeW open refresh miss nD = treeItem "agents-node" open item children
  where
    item = do
      (lbl, _) <- elDynAttr' "span" (rowAttrs <$> nD) $ do
        elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-claude.svg") blank
        elDynAttr "span" (glyphAttrs <$> nD) . dynText $ glyphOf <$> nD
        elClass "span" "agents-title" . dynText $ anTitle <$> nD
        elDynAttr "span" (ageAttrs <$> nD) . dynText $ anAge <$> nD
      -- Click: show the agent's pane (including one that is still starting, and
      -- may be parked on a first-run question), or bring an exited one back
      -- where it left off — its transcript is still there, so --resume picks the
      -- thread up rather than starting over.
      performEvent_ $ ffor (tagPromptlyDyn nD (domEvent Click lbl)) $ \n ->
        liftIO . void . forkIO $
          if anState n == "gone"
            then runClaudeCmd (ClaudeResume (anDir n) (anSession n))
            else showAgentPane (anSession n) >>= \found -> if found
              then return ()
              else miss $ anTitle n <> " is running, but not in a terminal \
                          \here — there is no pane to bring up."
      -- ⟳ while it is running, ✕ once it has gone.
      liveD <- holdUniqDyn (anLive <$> nD)
      btnE  <- switchHold never =<< dyn (ffor liveD $ \l ->
        if l then actionBtn "\8635" "Ask this agent to refresh its title and \
                                    \description"
             else actionBtn "\10005" "Forget this agent (its children stay)")
      performEvent_ $ ffor (tagPromptlyDyn nD btnE) $ \n ->
        liftIO . void . forkIO $
          if anLive n
            then void (agentSend (anSession n) True agentRefreshPrompt)
            else dismissAgent (anSession n) >> refresh
      return (never :: Event t ())

    children = do
      -- The line leksah writes itself (as against the agent's description):
      -- which checkout it is in, its open PR — clickable, same routing as a
      -- link in the description — and the branch it is on.  For worktree agents
      -- that trio is the whole story of what the agent produced.  It comes
      -- FIRST, directly under the title: it is one short line of leksah's own
      -- facts, and a reader wants "which checkout / which branch" before the
      -- agent's multi-line prose.
      divClass "agents-where" $ do
        dynText $ (T.pack . baseName . anDir) <$> nD
        elDynAttr "span" (sepAttrs <$> nD) $ text "  ·  "
        elDynAttr "a" (prAttrs <$> nD) . dynText $
          ffor nD $ \n -> maybe "" (\(k, _) -> "PR #" <> T.pack (show k)) (anPr n)
        dynText $ ffor nD $ maybe "" ("  ·  " <>) . anBranch
      -- The worktree relationships the agent REGISTERED (`agent register` /
      -- register_worktree / the git hook) — leksah's other own-facts lines:
      -- one per claim, plain text, empty collapses away.
      dyn_ $ ffor nD $ \n ->
        forM_ (anWorktrees n) $ \l -> divClass "agents-worktrees" (text l)
      -- The description, straight into innerHTML — sanitized once, where it was
      -- stored ('IDE.Web.AgentInfo.sanitizeAgentHtml').  Empty collapses away
      -- (see the :empty rule in 'agentsCss') rather than leaving a gap.
      void $ elDynHtmlAttr' "div" ("class" =: "agent-desc")
        (fromMaybe "" . anDesc <$> nD)
      el "ul" $ agentLevel False refresh miss (byKey . anChildren <$> nD)

    -- Signatures because '=:' is polymorphic in its container: without them the
    -- inferred type is an over-general 'At' constraint that won't generalize.
    rowAttrs, glyphAttrs, ageAttrs, prAttrs, sepAttrs :: AgentNode -> Map Text Text
    rowAttrs n = "class" =: ("agents-label leksah-nav-item"
                              <> if anLive n then "" else " agents-gone")
              <> "title" =: (anTitle n <> "\n" <> anDetail n
                              <> "\n" <> T.pack (anDir n)
                              <> "\n" <> anSession n)

    -- Shape AND colour per state, like the workspace tree's badges, so the
    -- states stay tellable apart without colour vision.
    glyphAttrs n = "class" =: "agents-badge"
                <> "style" =: ("color:" <> colourOf (anState n))
    glyphOf :: AgentNode -> Text
    glyphOf n = case anState n of
      "waiting" -> "\9650"   -- ▲ blocked on an approval prompt: it needs you
      "busy"    -> "\9670"   -- ◆ working
      "shell"   -> "\9670"
      "idle"    -> "\9679"   -- ● ready for input
      "starting" -> "\9678"  -- ◎ a pane exists, no session registered yet
      _         -> "\9675"   -- ○ exited
    colourOf :: Text -> Text
    colourOf = \case
      "waiting"  -> "#f85149"
      "busy"     -> "#d29922"
      "shell"    -> "#d29922"
      "idle"     -> "#3fb950"
      "starting" -> "#d29922"
      _          -> "#8b949e"
    ageAttrs n | T.null (anAge n) = "style" =: "display:none"
               | otherwise        = "class" =: "agents-age"
    -- No PR: the anchor disappears, separator and all.  'data-href' (not 'href')
    -- for the same reason the sanitizer rewrites description links — the pane's
    -- own handler decides where a URL opens.
    prAttrs n = case anPr n of
      Nothing       -> "style" =: "display:none"
      Just (_, url) -> "class" =: "agent-link agents-pr" <> "data-href" =: url
                    <> "title" =: url
    -- The separator belongs to the PR, not to the link itself (it would be
    -- clickable inside the anchor), so it hides with it.
    sepAttrs n | Nothing <- anPr n = "style" =: "display:none"
               | otherwise         = mempty
    baseName = reverse . takeWhile (/= '/') . dropWhile (== '/') . reverse

-- | A compact glyph button in a row (⟳ refresh / ✕ forget), matching the
-- Terminals tree's management glyphs.
actionBtn :: MonadWidget t m => Text -> Text -> m (Event t ())
actionBtn glyph tip = do
  (e, _) <- elAttr' "button" ("class" =: "agents-action" <> "title" =: tip) $
    text glyph
  return (domEvent Click e)

--------------------------------------------------------------------------------

-- | Route a click on a description link.  The anchors carry @data-href@, not
-- @href@ (the sanitizer's doing), so there is no default navigation to race —
-- which matters because jsaddle-wkwebview dispatches events asynchronously and a
-- @preventDefault@ from Haskell would arrive too late.  Armed per pane by
-- 'agentsWidget'; @__leksahOpenExtUrl@ is installed per OS window by
-- "IDE.Web.Main".
agentLinksJs :: Text
agentLinksJs = T.unlines
  [ "window.LeksahAgentLinks = (function(){"
  , "  function arm(root){"
  , "    if (!root || root.__lkAgentLinks) return;"
  , "    root.__lkAgentLinks = true;"
  , "    root.addEventListener('click', function(e){"
  , "      var a = e.target && e.target.closest"
  , "                ? e.target.closest('a.agent-link[data-href]') : null;"
  , "      if (!a) return;"
  , "      e.preventDefault(); e.stopPropagation();"
  , "      var u = a.getAttribute('data-href');"
  , "      try { if (window.__leksahOpenExtUrl)"
  , "              window.__leksahOpenExtUrl(u, !!e.altKey, !!e.shiftKey); } catch(_) {}"
  , "    }, true);"
  , "  }"
  , "  return { arm: arm };"
  , "})();"
  ]

agentsCss :: Css
agentsCss = do
    -- Top-level uls sit flush with the pane's left edge (like .terminals > ul);
    -- only nested uls take the global 20px indent.
    ".agents > ul" ? ("margin-left" -: "0px")
    ".agents" ? do
        height (pct 100)
        overflow auto
        -- The expand/collapse triangles are SVG and need an explicit fill here
        -- (the shared tree rules only set it under .workspace / .metadata).
        key "fill" dimColor
        cursor cursorDefault
        "padding-right" -: "8px"
    ".agents button" ? do
        color fgColor
        borderStyle none
        borderRadius (px 3) (px 3) (px 3) (px 3)
        backgroundImage (vGradient btnTopColor btnBottomColor)
        fontSize (px 13)
        cursor cursorDefault
    ".agents button" # hover ?
        backgroundImage (vGradient btnHoverTopColor btnHoverBottomColor)
    -- The per-row ⟳ / ✕ is a QUIET glyph, not a button: one boxed control per
    -- row stacked up a column of chrome that competed with the titles.  Faint
    -- at rest (still discoverable — hiding it entirely would make it a secret),
    -- clearer on the row, full on itself and on keyboard focus.
    ".agents .agents-action" ? do
        padding (px 0) (px 3) (px 0) (px 3)
        fontSize (px 11)
        "background" -: "none"
        color dimColor
        "opacity" -: "0.35"
        "flex" -: "0 0 auto"
    ".agents li" # hover |> ".agents-action" ? ("opacity" -: "0.75")
    ".agents .agents-action" # hover ? do
        "opacity" -: "1"
        color fgColor
        backgroundImage (vGradient hoverColor hoverColor)
    ".agents .agents-action:focus-visible" ? ("opacity" -: "1")
    -- The header's own re-read glyph has no row to hover, so it always shows.
    ".agents .agents-head .agents-action" ? ("opacity" -: "0.6")
    -- The header: agent count on the left, the re-read glyph on the right.
    ".agents .agents-head" ? do
        display flex
        "align-items" -: "center"
        padding (px 3) (px 0) (px 4) (px 2)
    ".agents .agents-head-label" ? do
        fontWeight bold
        color dimColor
        fontSize (px 12)
        "flex" -: "1"
        "min-width" -: "0"
    ".agents .agents-empty" ? do
        color dimColor
        fontSize (px 12)
        padding (px 2) (px 2) (px 6) (px 2)
        "overflow-wrap" -: "anywhere"
    ".agents li" ? do
        padding (px 1) (px 0) (px 1) (px 0)
        display flex
        "flex-wrap" -: "wrap"
        "align-items" -: "center"
    -- One agent is one block: a hairline above each sibling after the first
    -- separates a tall expanded agent from the next row, which otherwise ran
    -- into it as one wall of text.  Only at the top level — nested children
    -- are already set apart by the tree indent.
    ".agents > ul > li.agents-node + li.agents-node" ? do
        "border-top" -: "1px solid var(--leksah-border-faint)"
        "margin-top" -: "3px"
        "padding-top" -: "4px"
    -- The row is a flex line so the title can take the slack and ellipsize;
    -- `gap` spaces the glyphs (each used to carry its own margin).
    ".agents .agents-label" ? do
        cursor cursorDefault
        color dimColor
        display flex
        "align-items" -: "center"
        "gap" -: "4px"
        "flex" -: "1"
        "min-width" -: "0"
        "line-height" -: "18px"
        "padding-left" -: "2px"
    ".agents .agents-label img.tree-icon" ? do
        opacity dimOpacity
        "flex" -: "0 0 auto"
    -- A live agent's title is lit; an exited one stays dim (and its whole row
    -- reads as history).  ONE line, ellipsized: a wrapped title made every row
    -- a different height and buried the rest of the block.  The full text is a
    -- hover away (the row's title attribute).
    ".agents .agents-label .agents-title" ? do
        color fgColor
        fontSize (px 12)
        "font-weight" -: "500"
        "flex" -: "1"
        "min-width" -: "0"
        "white-space" -: "nowrap"
        overflow hidden
        "text-overflow" -: "ellipsis"
    ".agents .agents-gone" ? ("opacity" -: "0.55")
    -- The state glyph carries meaning in its colour, so it is never dimmed.
    ".agents .agents-badge" ? do
        fontSize (px 9)
        "flex" -: "0 0 auto"
        opacity 1
    ".agents .agents-age" ? do
        fontSize (px 11)
        "flex" -: "0 0 auto"
        color dimColor
    ".agents .tree-expand" ? ("flex" -: "0 0 auto")
    ".agents .tree-children" ? ("flex-basis" -: "100%")
    -- The description an agent wrote about itself: about four lines, clamped so
    -- one that runs on can't push the tree off screen, and wrapping anywhere so
    -- a long URL doesn't stretch the pane.
    -- A node's own content (description, checkout line) is indented past the
    -- expand triangle so it starts under the robot icon rather than under the
    -- triangle: 12px is the triangle SVG's width ('simpleSvgPath' in
    -- "IDE.Web.Widget.Tree"), plus the 2px inset .agents-label carries.  Nested
    -- child agents keep the shared tree indent (their own <ul>'s 20px).
    ".agents .agent-desc" ? do
        -- Muted, not full foreground: the title is the thing you scan for, and
        -- three or four lines of body text at the same weight drowned it.
        color fgMutedColor
        fontSize (px 12)
        "line-height" -: "1.45"
        padding (px 1) (px 4) (px 3) (px 14)
        "overflow-wrap" -: "anywhere"
        "display" -: "-webkit-box"
        "-webkit-line-clamp" -: "3"
        "-webkit-box-orient" -: "vertical"
        overflow hidden
    ".agents .agent-desc:empty" ? ("display" -: "none")
    ".agents .agent-desc p" ? ("margin" -: "0 0 2px 0")
    ".agents .agent-desc ul" ? ("margin" -: "0")
    -- Inline code as a small chip rather than same-size text that only differs
    -- by family (`ghc914-sh`, `emcc`, … are frequent in these descriptions).
    ".agents .agent-desc code" ? do
        "font-size" -: "11px"
        "background" -: "var(--leksah-surface-alt)"
        "border-radius" -: "3px"
        "padding" -: "0 3px"
    -- Links look like links and are clickable (the pane's handler decides where
    -- they open: OS browser, or ⌥ for a leksah browser pane split) — both the
    -- ones inside an agent's description and the PR on the checkout line.
    ".agents a.agent-link" ? do
        color selectionColor
        cursor pointer
        textDecoration underline
    ".agents .agents-where" ? do
        color dimColor
        fontSize (px 11)
        padding (px 0) (px 4) (px 1) (px 14)
        "overflow-wrap" -: "anywhere"
    ".agents .agents-worktrees" ? do
        color dimColor
        fontSize (px 11)
        fontStyle italic
        padding (px 0) (px 4) (px 1) (px 14)
        "overflow-wrap" -: "anywhere"
    -- Hovering a row's own button highlights that row's line, like the
    -- Terminals tree (clipped to the first line so it can't bleed over an
    -- expanded subtree).
    ".agents li:has(> button:hover)" ? do
        backgroundImage (vGradient hoverColor hoverColor)
        "background-size" -: "100% 20px"
        "background-repeat" -: "no-repeat"
    -- The keyboard-nav cursor is a blue outline, as in the other trees.
    ".agents .leksah-nav-item.leksah-nav-current" ? do
        "background" -: "transparent"
        "box-shadow" -: "inset 0 0 0 1px var(--leksah-selection)"
