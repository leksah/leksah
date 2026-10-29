{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
-- | The \"Agents\" pane (lives on the side, with the Workspace and Terminals
-- trees).  It answers "what are my agents doing?" — the question the status
-- light can only answer with a count — as a single column of CARDS
-- ("IDE.Web.Widget.AgentCard"), the same cards the Team pane lays out as a
-- grid of bios:
--
--   * the TOP card is the agent you are working with — the one in the active
--     pane, else the most recently used one — and is always shown in full:
--     its bio, its facts, what it last said, its actions;
--   * the manager comes next (or, without one, the header offers to start it);
--   * every other agent is a collapsed card — face, name, state, title, what
--     it is doing — with a ⋯ that opens it.
--
-- Above the cards: a count per state that doubles as a filter, a search box
-- (@/@ focuses it), a toggle between the lineage TREE (children indented
-- under the agent that started them) and a flat needs-you-first list, ＋ for a
-- new agent and 👥 for the Team pane.  There is no message box: clicking a
-- card brings the agent's own pane up, and that is where you talk to it.
--
-- Links in a description or bio open in the OS browser; hold ⌥ (⌥⇧ for the
-- other direction) to open one in a leksah browser pane split instead.  The
-- click is routed in JS ('agentLinksJs' → @window.__leksahOpenExtUrl@, defined
-- in "IDE.Web.Main"), because the anchors are deliberately inert: the
-- sanitizer moves @href@ to @data-href@, so nothing can navigate the page.
module IDE.Web.Widget.Agents
  ( agentsCss
  , agentsWidget
  , agentLinksJs
  , agentCardActions
  , activeProjectDir
  ) where

import Control.Concurrent (forkIO)
import Control.Lens ((^.), (.~), (&))
import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)

import Data.Bool (bool)
import Data.List (find, sortOn)

import Data.Map (Map)
import qualified Data.Map as M (fromList, fromListWith, findWithDefault)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Clay
       (Css, (?), (#), (-:), backgroundImage, borderRadius,
        borderStyle, color, cursor, cursorDefault, display, flex, fontSize,
        fontWeight, bold, height, hidden, hover, none, overflow, padding,
        pct, px, vGradient, None(..), Cursor(..))
import Clay.Stylesheet (key)

import Reflex
       (Dynamic, Event, ffor, foldDyn, getPostBuild, holdDyn, holdUniqDyn,
        leftmost, listViewWithKey, never, newTriggerEvent,
        tickLossyFromPostBuildTime, updated)
import Reflex.Dom.Core
       (blank, def, divClass, domEvent, dyn_, dynText, elAttr, elAttr', elClass,
        elClass', elDynAttr, elDynAttr', elementConfig_initialAttributes,
        inputElement, inputElementConfig_elementConfig, text,
        EventName(..), (=:), _element_raw, _inputElement_value)
import Language.Javascript.JSaddle (liftJSM, jsg, js1, eval, valToBool)

import IDE.App (App(..), getGlobalApp)
import IDE.Reactive (readCell)
import IDE.Workspace (WorkspaceService(..), activeProject, prDir)
import IDE.Web.Agent
       (ForkRequest(..), agentLastSaid, agentManagerStart, agentRefresh,
        agentResume, agentSuspend, emptyForkRequest, forkAgent,
        renameAgentEverywhere, showAgentPane)
import IDE.Web.AgentInfo (AgentNode(..), agentForest, dismissAgent)
import IDE.Web.Claude (ClaudeCmd(..), runClaudeCmd)
import IDE.Web.Theme
       (btnBottomColor, btnHoverBottomColor, btnHoverTopColor, btnTopColor,
        dimColor, fgColor, hoverColor)
import IDE.Web.Widget.AgentCard
       (CardActions(..), agentCard, agentCardCss, colourOf, displayName)
import IDE.Web.Command (Command(CommandShowTeam))
import IDE.Web.KeymapRequest (requestKeymapCommand)
import IDE.Web.Frame (MonadWidget, performEvent_)

-- | What the cards' buttons do, for a pane that shows notices with @toast@ and
-- re-reads the agents with @refresh@.  Shared with the Team pane.
agentCardActions :: (Text -> IO ()) -> IO () -> CardActions
agentCardActions toast refresh = CardActions
  { caOpen = \n ->
      if anLive n || anState n == "starting"
        then showAgentPane (anSession n) >>= \found -> if found then return ()
          else toast $ displayName n <> " is running, but not in a terminal \
                                        \here — there is no pane to bring up."
        -- Resumed as it was launched (its parent, role, manager brief); an
        -- agent leksah has no record of falls back to a plain resume.
        else agentResume (anSession n) >>= \case
          Right _ -> return ()
          Left _  -> runClaudeCmd (ClaudeResume (anDir n) (anSession n))
  , caRefresh = \n -> do
      _ <- agentRefresh (anSession n)
      toast ("Asked " <> displayName n <> " to refresh how it appears")
  , caSuspend = \n -> agentSuspend (anSession n) False >>= toast . T.strip >> refresh
  , caResume  = \n -> agentResume (anSession n) >>= either toast (toast . T.strip) >> refresh
  , caForget  = \n -> dismissAgent (anSession n) >> refresh
  , caRename  = \n new -> renameAgentEverywhere (anSession n) new >>= \case
      Left err -> toast err
      Right _  -> refresh
    -- A copy of its conversation, beside it — the user asked, so (unlike an
    -- agent's own fork) the new pane is brought up to type into.
  , caBranch = \n -> forkAgent (emptyForkRequest (anDir n)) { frFrom = Just (anSession n) }
      >>= \case
        Left err       -> toast err
        Right (sid, _) -> void (showAgentPane sid) >> refresh
  , caLastSaid = agentLastSaid . anSession
  }

-- | The pane.  @mruNamesD@ are the agents' names in flip-MRU order (the
-- flipper's order: the active pane first) — the first one's card leads, in
-- full, and the rest of the order breaks ties within a state group.
--
-- Polls 'agentForest' (cheap — it reads the status poll's cache), and renders
-- it with a keyed list so a poll that changes one agent's state leaves every
-- other card — and whichever the user opened — exactly as it was.
agentsWidget :: forall t m . MonadWidget t m => Dynamic t [Text] -> m (Event t ())
agentsWidget mruNamesD = do
  (root, _) <- elClass' "div" "agents leksah-nav" $ mdo
    pb   <- getPostBuild
    tick <- tickLossyFromPostBuildTime 2
    (forestE, fireForest) <- newTriggerEvent
    -- The read runs OFF the reflex thread: it stats transcripts of exited
    -- agents, and a synchronous performEvent would hitch the whole UI.
    let refresh = void . forkIO $ agentForest >>= fireForest
    forestD <- holdUniqDyn =<< holdDyn [] forestE
    -- One-line notices (a click that finds no pane, an agent that started or
    -- agent or failed): fired from whichever thread knows, shown as a toast on
    -- the frame thread.
    (toastE, toast) <- newTriggerEvent
    performEvent_ $ ffor toastE $ \msg -> liftJSM . void $
      jsg ("window" :: Text) ^. js1 ("__leksahBridgeToast" :: Text) (msg :: Text)
    let acts = agentCardActions toast refresh

    (hdrE, stateE, searchD, flatD) <- divClass "agents-head" $ do
      (hdr', stE, flat') <- divClass "agents-head-row" $ do
        elClass "span" "agents-head-label" . dynText $ ffor forestD $ \f ->
          case length (flatten f) of
            0 -> "No agents running"
            1 -> "1 agent"
            n -> T.pack (show n) <> " agents"
        chipEs <- forM chips $ \(st, glyph, word, colour) -> do
          let countD = ffor forestD $ \f ->
                length [ () | n <- flatten f, inState st n ]
              attrs c cur = "class" =: ("agents-chip" <> if cur == Just st
                                                          then " agents-chip-on" else "")
                         <> "title" =: ("Show only the agents that are " <> word
                                        <> " (click again for all)")
                         <> (if c == (0 :: Int) && cur /= Just st
                               then "style" =: "display:none" else mempty)
          (b, _) <- elDynAttr' "button" (attrs <$> countD <*> stateD) $ do
            elAttr "span" ("style" =: ("color:" <> colour)) (text glyph)
            dynText $ (\c -> " " <> T.pack (show c)) <$> countD
          return (st <$ domEvent Click b)
        -- The layout toggle; remembered per viewer.
        flatInit <- liftJSM $ valToBool =<< eval
          ("(function(){try{return localStorage.getItem('leksahAgentsFlat')==='1'}\
           \catch(e){return false}})()" :: Text)
        rec flatD' <- foldDyn (const not) flatInit modeE
            (modeB, _) <- elDynAttr' "button"
              (ffor flatD' $ \fl -> "class" =: "agents-action agents-mode"
                 <> "title" =: (if fl then "Showing needs-you first — click for \
                                           \the tree of who started whom"
                                      else "Showing the tree of who started whom — \
                                           \click for needs-you first"))
              (dynText (bool "⌥ tree" "≡ list" <$> flatD'))
            let modeE = domEvent Click modeB
        performEvent_ $ ffor (updated flatD') $ \fl -> liftJSM . void . eval $
          ("try{localStorage.setItem('leksahAgentsFlat','" <> (if fl then "1" else "0")
             <> "')}catch(e){}" :: Text)
        -- A new agent: its own tab in the active project, where you then tell
        -- it what to do.
        newE <- actionBtn "＋" "Start a new agent in the active project, in its own tab"
        performEvent_ $ ffor newE $ \_ -> liftIO . void . forkIO $
          activeProjectDir >>= \case
            Nothing -> toast "Open a project first: a new agent starts in the active project"
            Just d  -> runClaudeCmd (ClaudeNew d) >> refresh
        teamE <- actionBtn "👥" "Open the Team pane: a bio card for every agent"
        performEvent_ $ liftIO (requestKeymapCommand CommandShowTeam) <$ teamE
        h <- actionBtn "⟳" "Re-read the agent list"
        return (h, leftmost chipEs, flatD')
      inp <- inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("class" =: "agents-search" <> "placeholder" =: "Search agents  (/)"
             <> "spellcheck" =: "false")
      return (hdr', stE, _inputElement_value inp, flat')
    -- The state filter: click a chip to show only that state, click it again
    -- (or another) to change.
    stateD <- foldDyn (\st cur -> if cur == Just st then Nothing else Just st)
                      Nothing stateE

    -- No manager yet: offer one.  (One that exists has its own card, second.)
    let noManagerD = not . any anManager . flatten <$> forestD
    dyn_ $ ffor noManagerD $ \noManager -> if not noManager then blank else do
      (b, _) <- elAttr' "button"
        ("class" =: "agents-manager-start"
         <> "title" =: "Start a manager agent: it routes your tasks to the right \
                       \agent (or starts one), keeps their descriptions current, \
                       \and suspends and resumes them") $
        text "Start a manager"
      performEvent_ $ ffor (domEvent Click b) $ \_ -> liftIO . void . forkIO $
        activeProjectDir >>= \case
          Nothing -> toast "Open a project first: the manager starts in the active project"
          Just d  -> agentManagerStart d >>= either toast (const refresh)

    performEvent_ $ liftIO refresh <$ leftmost [() <$ pb, () <$ tick, hdrE]
    -- Nothing running: say how one starts, rather than showing an empty box.
    dyn_ $ ffor (null <$> forestD) $ \empty' -> if not empty' then blank else
      divClass "agents-empty" $ text
        "Start one with ＋ above, or from a Claude session with \
        \`leksah-cmd agent fork 'do this'`."
    let shownD = shape <$> forestD <*> stateD <*> searchD <*> flatD <*> mruNamesD
    divClass "agents-list" . void . listViewWithKey shownD $ \_ cD -> do
      elDynAttr "div" (slotAttrs <$> cD) $
        agentCard acts (slotTop <$> cD) False (slotNode <$> cD)
      return (never :: Event t ())
    return ()
  -- Arm the pane's JS (description links, / to search) on
  -- THIS pane's root element.  The handle, never document.querySelector: at
  -- postBuild the div isn't attached in wkwebview's batched DOM, and setting a
  -- property on null aborts the build batch (which shows up as a blank UI, not
  -- as an error).
  pb <- getPostBuild
  performEvent_ $ ffor pb $ \_ -> liftJSM . void $
    jsg ("LeksahAgentLinks" :: Text) ^. js1 ("arm" :: Text) (_element_raw root)
  return never
  where
    chips :: [(Text, Text, Text, Text)]
    chips = [ ("waiting",   "▲", "waiting for you", colourOf "waiting")
            , ("busy",      "◆", "working",         colourOf "busy")
            , ("idle",      "●", "idle",            colourOf "idle")
            , ("done",      "✓", "done",            colourOf "done")
            , ("suspended", "❚", "suspended",       colourOf "suspended") ]

    -- What the list shows, as session → slot.  The top agent first (always in
    -- full), then the manager, then the rest.  The top and manager cards are
    -- shown whatever the filters say: they are the ones you are working with.
    --
    -- Both layouts order agents the same way ('order'): by state group —
    -- needs you, working, idle, done, suspended, exited — then most recently
    -- used first (the flipper's order), then by name.  The list is every
    -- matching agent, flat, in that order.  The tree applies it at every level,
    -- pruned to the agents that match (an agent stays when anything under it
    -- does, so the match keeps its context), children indented under the agent
    -- that started them — a pinned card's children come up with it.
    shape forest mst q flat mru =
      M.fromList [ (anSession n, Slot i d (Just (anSession n) == topSid) n)
                 | (i, (d, n)) <- zip [0 ..] (dedupe (pinned <> rest)) ]
      where
        everyone = flatten forest
        mruIx    = M.fromListWith min (zip mru [0 :: Int ..])
        order n  = ( rankOf (anState n)
                   , M.findWithDefault maxBound (anName n) mruIx
                   , T.toLower (anName n), T.toLower (anTitle n) )
        topNode  = listToMaybe
          [ n | nm <- mru
              , Just n <- [ find (\x -> anName x == nm && anLive x) everyone
                              `orElse` find ((== nm) . anName) everyone ] ]
        topSid   = anSession <$> topNode
        manager  = find anManager everyone
        pinnedNodes = [ n | Just n <- [topNode] ]
                   <> [ n | Just n <- [manager], Just (anSession n) /= topSid ]
        pinnedIds = map anSession pinnedNodes
        -- A pinned card, and (in the tree) the matching agents it started.
        pinned = concat
          [ (0 :: Int, p) : (if flat then [] else withDepth 1 (sortTree (prune (anChildren p))))
          | p <- pinnedNodes ]
        rest
          | flat      = [ (0, n) | n <- sortOn order everyone, ok n
                                 , anSession n `notElem` pinnedIds ]
          | otherwise = withDepth 0 (sortTree (prune (without pinnedIds forest)))
        -- Pinned agents (and so their subtrees) come out of the tree: they
        -- are shown at the top, children and all.
        without ids ns = [ n { anChildren = without ids (anChildren n) }
                         | n <- ns, anSession n `notElem` ids ]
        sortTree ns = sortOn order [ n { anChildren = sortTree (anChildren n) } | n <- ns ]
        prune ns = [ n { anChildren = ks } | n <- ns
                   , let ks = prune (anChildren n), ok n || not (null ks) ]
        withDepth d = concatMap (\n -> (d, n) : withDepth (d + 1) (anChildren n))
        -- An agent appears once (the manager may be in the top card's tree).
        dedupe = go []
          where go _ [] = []
                go seen (x@(_, n) : xs)
                  | anSession n `elem` seen = go seen xs
                  | otherwise               = x : go (anSession n : seen) xs
        ok n = maybe True (`inState` n) mst && all (`T.isInfixOf` haystack n) terms
        terms = T.words (T.toLower q)
        haystack n = T.toLower $ T.unwords
          [ anName n, anTitle n, stripTags (fromMaybe "" (anDesc n))
          , stripTags (fromMaybe "" (anBio n))
          , fromMaybe "" (anBranch n), maybe "" (\(k, _) -> "#" <> T.pack (show k)) (anPr n)
          , T.pack (anDir n), fromMaybe "" (anActivity n)
          , fromMaybe "" (anRole n), fromMaybe "" (anDone n), T.unwords (anWorktrees n) ]
        orElse a b = maybe b Just a

    -- The chips' groups: "working" takes the shell and starting states too.
    inState st n = case st of
      "busy" -> anState n `elem` ["busy", "shell", "starting"]
      _      -> anState n == st

    stripTags t = case T.breakOn "<" t of
      (a, rest) | T.null rest -> a
                | otherwise   -> a <> " " <> stripTags (T.drop 1 (T.dropWhile (/= '>') rest))

-- | Where one card goes: its position (a CSS @order@ — the keyed list renders
-- in key order, and re-keying by position would rebuild, and collapse, every
-- card that moved), its depth in the tree (an indent), whether it is the top
-- card, and the agent.
data Slot = Slot
  { slotOrder :: Int
  , slotDepth :: Int
  , slotTop   :: Bool
  , slotNode  :: AgentNode
  } deriving (Eq, Show)

slotAttrs :: Slot -> Map Text Text
slotAttrs s = "class" =: "agents-slot"
           <> "style" =: ("order:" <> T.pack (show (slotOrder s))
                          <> ";margin-left:" <> T.pack (show (12 * min 4 (slotDepth s))) <> "px")

flatten :: [AgentNode] -> [AgentNode]
flatten = concatMap (\n -> n : flatten (anChildren n))

rankOf :: Text -> Int
rankOf = \case
  "waiting"   -> 0
  "busy"      -> 1
  "shell"     -> 1
  "starting"  -> 1
  "idle"      -> 2
  "done"      -> 3
  "suspended" -> 4
  _           -> 5

-- | The active project's directory, if there is one.
activeProjectDir :: IO (Maybe FilePath)
activeProjectDir = getGlobalApp >>= \case
  Nothing  -> return Nothing
  Just app -> fmap prDir . activeProject <$> readCell (wsCell (appWorkspace app))

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
    -- / anywhere in the pane (outside a text field) jumps to the search box.
    -- In JS because preventDefault from a Haskell handler arrives too late
    -- under wkwebview's asynchronous event dispatch.
  , "    root.addEventListener('keydown', function(e){"
  , "      var t = e.target, tag = t && t.tagName;"
  , "      if (e.key === '/' && tag !== 'INPUT' && tag !== 'TEXTAREA') {"
  , "        var s = root.querySelector('.agents-search');"
  , "        if (s) { e.preventDefault(); s.focus(); }"
  , "      }"
  , "    }, true);"
  , "  }"
  , "  return { arm: arm };"
  , "})();"
  ]

agentsCss :: Css
agentsCss = do
    -- The cards themselves (shared with the Team pane).
    agentCardCss
    -- Head and cards: only the cards scroll.
    ".agents" ? do
        height (pct 100)
        overflow hidden
        display flex
        "flex-direction" -: "column"
        -- The search/expand triangles etc. are SVG and need an explicit fill.
        key "fill" dimColor
        cursor cursorDefault
        "padding-right" -: "8px"
    ".agents button" ? do
        color fgColor
        borderStyle none
        borderRadius (px 3) (px 3) (px 3) (px 3)
        backgroundImage (vGradient btnTopColor btnBottomColor)
        fontSize (px 12)
        cursor cursorDefault
    ".agents button" # hover ?
        backgroundImage (vGradient btnHoverTopColor btnHoverBottomColor)
    -- The header's quiet glyph buttons (⟳, the layout toggle).
    ".agents .agents-action" ? do
        padding (px 0) (px 3) (px 0) (px 3)
        fontSize (px 11)
        "background" -: "none"
        color dimColor
        "opacity" -: "0.6"
        "flex" -: "0 0 auto"
    ".agents .agents-action" # hover ? do
        "opacity" -: "1"
        color fgColor
        backgroundImage (vGradient hoverColor hoverColor)
    -- The header: agent count, the state chips, the layout toggle and the
    -- re-read glyph; the search box under them.
    ".agents .agents-head" ? do
        "flex" -: "0 0 auto"
        padding (px 3) (px 0) (px 4) (px 2)
    ".agents .agents-head-row" ? do
        display flex
        "align-items" -: "center"
        "gap" -: "3px"
    ".agents .agents-head-label" ? do
        fontWeight bold
        color dimColor
        fontSize (px 12)
        "flex" -: "1"
        "min-width" -: "0"
    ".agents .agents-chip" ? do
        "background" -: "none"
        fontSize (px 11)
        padding (px 0) (px 4) (px 0) (px 4)
        color dimColor
        "border" -: "1px solid transparent"
        "flex" -: "0 0 auto"
    ".agents .agents-chip.agents-chip-on" ? do
        color fgColor
        "border" -: "1px solid var(--leksah-selection)"
    ".agents .agents-search" ? do
        "width" -: "calc(100% - 8px)"
        "margin" -: "4px 0 0 0"
        fontSize (px 12)
        padding (px 2) (px 4) (px 2) (px 4)
        color fgColor
        "background" -: "var(--leksah-surface-alt)"
        "border" -: "1px solid var(--leksah-border-faint)"
        borderRadius (px 3) (px 3) (px 3) (px 3)
        "outline" -: "none"
    ".agents .agents-search:focus" ?
        ("border-color" -: "var(--leksah-selection)")
    ".agents .agents-manager-start" ? do
        "margin" -: "0 0 6px 2px"
        "align-self" -: "flex-start"
        padding (px 2) (px 8) (px 2) (px 8)
    ".agents .agents-empty" ? do
        color dimColor
        fontSize (px 12)
        padding (px 2) (px 2) (px 6) (px 2)
        "overflow-wrap" -: "anywhere"
    -- The cards: one column, ordered by CSS (see 'Slot').
    ".agents .agents-list" ? do
        "flex" -: "1 1 auto"
        "min-height" -: "0"
        overflow'
        display flex
        "flex-direction" -: "column"
        "gap" -: "5px"
        "padding" -: "1px 1px 6px 1px"
    ".agents .agents-slot" ? ("flex" -: "0 0 auto")
    -- An agent's face in the workspace tree's session rows.
    ".tree-face" ? do
        "display" -: "inline-flex"
        "vertical-align" -: "middle"
        "margin-right" -: "3px"
    ".tree-face svg" ? do
        "width" -: "16px"
        "height" -: "16px"
  where
    overflow' = "overflow" -: "auto"
