{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
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
import Control.Lens ((^.), (.~), (&))
import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (liftIO)

import Data.Bool (bool)
import Data.List (sortOn)

import Data.Map (Map)
import qualified Data.Map as M (fromList, elems)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Clay
       (Css, (?), (#), (|>), (-:), auto, backgroundImage, bold, borderRadius,
        borderStyle, color, cursor, cursorDefault, display, flex, fontSize,
        fontStyle, italic,
        fontWeight, height, hidden, hover, none, opacity, overflow, padding,
        pct, pointer, px, textDecoration, underline, vGradient, width,
        fontFamily, monospace, None(..), Cursor(..), Color(Other))
import Clay.Stylesheet (key)

import Reflex
       (Dynamic, Event, attach, constDyn, current, ffilter, fmapMaybe, ffor,
        foldDyn, gate, getPostBuild, holdDyn, holdUniqDyn, leftmost,
        listViewWithKey, never, newTriggerEvent, sample, switchHold, tag,
        tagPromptlyDyn, tickLossyFromPostBuildTime, updated)
import Reflex.Dom.Core
       (Key(Enter, Escape), blank, def, divClass, domEvent, dyn, dyn_, dynText,
        el, elAttr, elAttr', elClass, elClass', elDynAttr, elDynAttr',
        elDynHtmlAttr', elementConfig_initialAttributes, inputElement,
        inputElementConfig_elementConfig, inputElementConfig_initialValue,
        keydown, keypress, text, textAreaElement,
        textAreaElementConfig_elementConfig, textAreaElementConfig_setValue,
        EventName(..), (=:), _element_raw, _inputElement_element,
        _inputElement_hasFocus, _inputElement_value, _textAreaElement_value)
import Language.Javascript.JSaddle (liftJSM, jsg, js0, js1, eval, valToBool)

import System.FilePath (takeFileName)

import IDE.App (App(..), getGlobalApp)
import IDE.Reactive (readCell)
import IDE.Workspace (WorkspaceService(..), activeProject, prDir)
import IDE.Web.Agent
       (ForkPlace(..), ForkRequest(..), agentLastSaid, agentSend, emptyForkRequest,
        forkAgent, renameAgentEverywhere, showAgentPane)
import IDE.Web.ClaudeQueue (queueAdd)
import IDE.Web.AgentFace (agentFaceSvg)
import IDE.Web.AgentInfo
       (AgentNode(..), agentForest, agentRefreshPrompt, dismissAgent)
import IDE.Web.Claude (ClaudeCmd(..), copySessionId, runClaudeCmd)
import IDE.Web.Theme
       (btnBottomColor, btnHoverBottomColor, btnHoverTopColor, btnTopColor,
        dimColor, dimOpacity, fgColor, fgMutedColor, hoverColor, selectionColor)
import IDE.Web.Widget.Tree (treeItemDynAttr')
import IDE.Web.Widget.Menu (rowMenu)
import IDE.Web.Frame (MonadWidget, performEvent_)

-- | The pane.  Polls 'agentForest' (cheap — it reads the status poll's cache),
-- and renders it with keyed lists so a poll that changes one agent's state
-- leaves every other row — and every expanded description — exactly as it was.
--
-- Above the list: a count per state that doubles as a filter (click ▲ to see
-- only the agents that need you), a search box (@/@ focuses it), and a toggle
-- between the lineage TREE and a flat list sorted needs-you-first.  Below it:
-- the dispatch box, which starts a new agent or sends the selected one a
-- message.
agentsWidget :: forall t m . MonadWidget t m => m (Event t ())
agentsWidget = do
  (root, _) <- elClass' "div" "agents leksah-nav" $ mdo
    pb   <- getPostBuild
    tick <- tickLossyFromPostBuildTime 2
    (forestE, fireForest) <- newTriggerEvent
    -- The read runs OFF the reflex thread: it stats transcripts of exited
    -- agents, and a synchronous performEvent would hitch the whole UI.
    let refresh = void . forkIO $ agentForest >>= fireForest
    forestD <- holdUniqDyn =<< holdDyn [] forestE
    -- One-line notices (a click that finds no pane, a dispatch that started an
    -- agent or failed): fired from whichever thread knows, shown as a toast on
    -- the frame thread.
    (toastE, toast) <- newTriggerEvent
    performEvent_ $ ffor toastE $ \msg -> liftJSM . void $
      jsg ("window" :: Text) ^. js1 ("__leksahBridgeToast" :: Text) (msg :: Text)

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

    performEvent_ $ liftIO refresh <$ leftmost [() <$ pb, () <$ tick, hdrE]
    -- Nothing running: say how one starts, rather than showing an empty box.
    dyn_ $ ffor (null <$> forestD) $ \empty' -> if not empty' then blank else
      divClass "agents-empty" $ text
        "Type a task below to start an agent, or start one from a Claude \
        \session with `leksah-cmd agent fork 'do this'`."
    let shownD = shape <$> forestD <*> stateD <*> searchD <*> flatD
    -- Top-level agents come up EXPANDED in the tree: a root is usually the
    -- session you are talking to, and what it says about itself is the reason
    -- to look here.  Anything it forked stays collapsed, so a deep tree still
    -- opens small.  In the flat list every row starts collapsed.
    pickE <- divClass "agents-list" . el "ul" $
      agentLevel True refresh toast selectedD (byKey <$> shownD)
    -- The selection: the agent the dispatch box talks to.  Clicking a row
    -- selects it (and, as before, brings its pane up); ✕ in the box clears it.
    selectedD <- holdDyn Nothing (leftmost [Just <$> pickE, Nothing <$ clearE])
    clearE <- dispatchW toast refresh selectedD forestD
    return ()
  -- Arm the pane's JS (description links, Enter-to-dispatch, / to search) on
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
    chips = [ ("waiting", "▲", "waiting for you", colourOf "waiting")
            , ("busy",    "◆", "working",         colourOf "busy")
            , ("idle",    "●", "idle",            colourOf "idle")
            , ("done",    "✓", "done",            colourOf "done") ]

    -- What the list shows: the tree pruned to the agents that match (an agent
    -- stays when anything under it does, so the match keeps its context), or —
    -- in list mode — every matching agent, flat, needs-you first.
    shape forest mst q flat
      | flat      = sortOn listOrder [ n { anChildren = [] } | n <- flatten forest, ok n ]
      | otherwise = prune forest
      where
        prune ns = [ n { anChildren = ks } | n <- ns
                   , let ks = prune (anChildren n), ok n || not (null ks) ]
        ok n = maybe True (`inState` n) mst && all (`T.isInfixOf` haystack n) terms
        terms = T.words (T.toLower q)
        haystack n = T.toLower $ T.unwords
          [ anName n, anTitle n, stripTags (fromMaybe "" (anDesc n))
          , fromMaybe "" (anBranch n), maybe "" (\(k, _) -> "#" <> T.pack (show k)) (anPr n)
          , T.pack (anDir n), fromMaybe "" (anActivity n)
          , fromMaybe "" (anRole n), fromMaybe "" (anDone n), T.unwords (anWorktrees n) ]
        listOrder n = (rankOf (anState n), T.toLower (anName n <> anTitle n))

    -- The chips' groups: "working" takes the shell and starting states too.
    inState st n = case st of
      "busy" -> anState n `elem` ["busy", "shell", "starting"]
      _      -> anState n == st

    stripTags t = case T.breakOn "<" t of
      (a, rest) | T.null rest -> a
                | otherwise   -> a <> " " <> stripTags (T.drop 1 (T.dropWhile (/= '>') rest))

flatten :: [AgentNode] -> [AgentNode]
flatten = concatMap (\n -> n : flatten (anChildren n))

rankOf :: Text -> Int
rankOf = \case
  "waiting"  -> 0
  "busy"     -> 1
  "shell"    -> 1
  "starting" -> 1
  "idle"     -> 2
  "done"     -> 3
  _          -> 4

-- | Each agent keyed by session (so a row persists across polls) and carrying
-- its POSITION, which the row turns into a CSS @order@: the keyed list itself
-- renders in key order, and re-keying by position would rebuild — and collapse
-- — every row that moved.
byKey :: [AgentNode] -> Map Text (Int, AgentNode)
byKey ns = M.fromList [ (anSession n, (i, n)) | (i, n) <- zip [0 ..] ns ]

-- | One level of the tree: a keyed list, so rows persist across polls (an
-- unkeyed rebuild would collapse whatever the user had expanded).  @open@ is
-- how a row of this level STARTS (see 'agentsWidget'); a row created later by
-- the poll starts the same way, and a collapse the user made survives, because
-- the keyed list never rebuilds the rows it already has.  Answers the session
-- of a row the user clicked.
agentLevel
  :: MonadWidget t m
  => Bool -> IO () -> (Text -> IO ()) -> Dynamic t (Maybe Text)
  -> Dynamic t (Map Text (Int, AgentNode))
  -> m (Event t Text)
agentLevel open refresh toast selectedD mD = do
  e <- listViewWithKey mD (\_ nD -> agentNodeW open refresh toast selectedD nD)
  return (fmapMaybe (listToMaybe . M.elems) e)

-- | One agent: its row, and — only while expanded, which is the point — its
-- description, what it last said, and the agents it forked.
agentNodeW
  :: forall t m . MonadWidget t m
  => Bool -> IO () -> (Text -> IO ()) -> Dynamic t (Maybe Text)
  -> Dynamic t (Int, AgentNode) -> m (Event t Text)
agentNodeW open refresh toast selectedD inD =
    treeItemDynAttr' (constDyn False) (liAttrs <$> inD) open item children
  where
    nD = snd <$> inD
    liAttrs (i, _) = "class" =: "agents-node" <> "style" =: ("order:" <> T.pack (show i))

    item = mdo
      -- Inline rename: the name becomes a text field (Enter keeps it, Esc or
      -- leaving it gives up).  No modal prompt: a browser dialog would block
      -- every other event in the window.
      renamingD <- holdDyn False (leftmost [True <$ renameE, False <$ doneRenameE])
      (lbl, doneRenameE) <- elDynAttr' "span" (rowAttrs <$> nD <*> selectedD) $ do
        -- Its face, with the state glyph as a badge on its corner — WHO it is
        -- and how it is, in one place — then its name and what it says it is
        -- doing.  (No name yet, e.g. a pane still starting: the robot.)
        elClass "span" "agents-avatar" $ do
          void $ elDynHtmlAttr' "span" ("class" =: "agents-face") (faceOf <$> nD)
          elDynAttr "span" (glyphAttrs <$> nD) . dynText $ glyphOf <$> nD
        doneE <- switchHold never =<< dyn (ffor renamingD $ \case
          False -> do
            elDynAttr "span" (nameAttrs <$> nD) . dynText $ anName <$> nD
            return never
          True -> do
            n0 <- sample (current nD)
            inp <- inputElement $ def
              & inputElementConfig_initialValue .~ anName n0
              & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
                  ("class" =: "agents-rename" <> "spellcheck" =: "false")
            pbR <- getPostBuild
            performEvent_ $ ffor pbR $ \_ -> liftJSM . void $
              _element_raw (_inputElement_element inp) ^. js0 ("select" :: Text)
            let commitE = tag (current (_inputElement_value inp)) (keypress Enter inp)
                cancelE = leftmost [ () <$ keydown Escape inp
                                   , () <$ ffilter not (updated (_inputElement_hasFocus inp)) ]
            performEvent_ $ ffor (attach (current nD) commitE) $ \(n, new) ->
              liftIO . void . forkIO $ renameAgentEverywhere (anSession n) new >>= \case
                Left err -> toast err
                Right _  -> refresh
            return (leftmost [() <$ commitE, cancelE]))
        elClass "span" "agents-title" . dynText $ anTitle <$> nD
        elDynAttr "span" (ageAttrs <$> nD) . dynText $ anAge <$> nD
        return doneE
      -- Click: show the agent's pane (including one that is still starting, and
      -- may be parked on a first-run question), or bring an exited one back
      -- where it left off — its transcript is still there, so --resume picks the
      -- thread up rather than starting over.  Either way the row is selected.
      let open' n =
            if anState n == "gone"
              then runClaudeCmd (ClaudeResume (anDir n) (anSession n))
              else showAgentPane (anSession n) >>= \found -> if found
                then return ()
                else toast $ displayName n <> " is running, but not in a terminal \
                              \here — there is no pane to bring up."
          -- ⟳ while it is running, ✕ once it has gone.
          manage n =
            if anLive n
              then void (agentSend (anSession n) True agentRefreshPrompt)
              else dismissAgent (anSession n) >> refresh
          clickE = gate (not <$> current renamingD) (domEvent Click lbl)
      performEvent_ $ ffor (tagPromptlyDyn nD clickE) $ \n ->
        liftIO . void . forkIO $ open' n
      liveD <- holdUniqDyn (anLive <$> nD)
      btnE  <- switchHold never =<< dyn (ffor liveD $ \l ->
        if l then actionBtn "\8635" "Ask this agent to refresh its title and \
                                    \description"
             else actionBtn "\10005" "Forget this agent (its children stay)")
      performEvent_ $ ffor (tagPromptlyDyn nD btnE) $ \n ->
        liftIO . void . forkIO $ manage n
      -- The same actions on right-click, plus renaming it and copying the
      -- session id (what `leksah-cmd agent send/read/wait` take).
      (renameE, fireRename) <- newTriggerEvent
      menuE <- rowMenu lbl
        [ ffor nD $ \n -> ( if anState n == "gone" then "Resume Agent" else "Show Pane"
                         , open' n )
        , ffor nD $ \n -> ( if anLive n then "Refresh Title and Description"
                                        else "Forget Agent"
                         , manage n )
        , constDyn ("Rename…", fireRename ())
        , ffor nD $ \n -> ("Copy Session Id", copySessionId (anSession n)) ]
      performEvent_ $ ffor menuE $ liftIO . void . forkIO
      -- What it is doing this minute — its last tool call, or the approval it
      -- is waiting on — on a line of its own under the title.  Gone at rest.
      elDynAttr "div" (activityAttrs <$> nD) . dynText $ fromMaybe "" . anActivity <$> nD
      return (anSession <$> tagPromptlyDyn nD clickE)

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
        -- The checkout's FULL path, dimmest — the same touch as the toolbar
        -- strip's editor path, and what tells two same-named worktrees apart.
        elClass "span" "agents-where-path" . dynText $ (T.pack . anDir) <$> nD
      -- The worktree relationships the agent REGISTERED (`agent register` /
      -- register_worktree / the git hook) — leksah's other own-facts lines:
      -- one per claim, plain text, empty collapses away.
      dyn_ $ ffor nD $ \n ->
        forM_ (anWorktrees n) $ \l -> divClass "agents-worktrees" (text l)
      -- Its own summary, once it has said it is done (`agent done`), and the
      -- role it was started in.
      dyn_ $ ffor nD $ \n -> do
        forM_ (anRole n) $ \ro -> divClass "agents-role" (text ("Role: " <> ro))
        forM_ (anDone n) $ \d -> divClass "agents-done" $ do
          elClass "span" "agents-said-label" (text "Done ")
          text d
      -- The description, straight into innerHTML — sanitized once, where it was
      -- stored ('IDE.Web.AgentInfo.sanitizeAgentHtml').  Empty collapses away
      -- (see the :empty rule in 'agentsCss') rather than leaving a gap.
      void $ elDynHtmlAttr' "div" ("class" =: "agent-desc")
        (fromMaybe "" . anDesc <$> nD)
      -- What it last said — the answer you'd otherwise open its pane to read.
      -- Fetched when the row opens and again whenever its state changes (a
      -- turn that ends is a new answer).
      pbC <- getPostBuild
      stateC <- holdUniqDyn (anState <$> nD)
      (saidE, fireSaid) <- newTriggerEvent
      performEvent_ $ ffor (tag (current nD) (leftmost [pbC, () <$ updated stateC])) $ \n ->
        liftIO . void . forkIO $ agentLastSaid (anSession n) >>= fireSaid
      saidD <- holdDyn Nothing saidE
      dyn_ $ ffor saidD $ \case
        Nothing -> blank
        Just t  -> divClass "agents-said" $ do
          elClass "span" "agents-said-label" (text "Last said ")
          text (T.take 600 t)
      el "ul" $ agentLevel False refresh toast selectedD (byKey . anChildren <$> nD)

    -- Signatures because '=:' is polymorphic in its container: without them the
    -- inferred type is an over-general 'At' constraint that won't generalize.
    glyphAttrs, nameAttrs, ageAttrs, prAttrs, sepAttrs, activityAttrs
      :: AgentNode -> Map Text Text
    activityAttrs n = case anActivity n of
      Just a | not (T.null a) -> "class" =: ("agents-activity agents-activity-" <> anState n)
                              <> "title" =: a
      _                       -> "style" =: "display:none"
    rowAttrs :: AgentNode -> Maybe Text -> Map Text Text
    rowAttrs n sel = "class" =: ("agents-label leksah-nav-item"
                              <> (if anLive n then "" else " agents-gone")
                              <> (if sel == Just (anSession n) then " agents-selected" else ""))
              <> "title" =: (nameLine n <> anTitle n <> "\n" <> anDetail n
                              <> "\n" <> T.pack (anDir n)
                              <> "\n" <> anSession n)
    nameLine n | T.null (anName n) = ""
               | otherwise         = anName n <> " — "
    faceOf n | T.null (anName n) = "<img class=\"tree-icon\" src=\"/pics/tree-claude.svg\">"
             | otherwise         = agentFaceSvg (anName n)
    nameAttrs n | T.null (anName n) = "style" =: "display:none"
                | otherwise         = "class" =: "agents-name"
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
      "done"    -> "\10003"  -- ✓ said it is done (`agent done`)
      "starting" -> "\9678"  -- ◎ a pane exists, no session registered yet
      _         -> "\9675"   -- ○ exited
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

-- | The state colours — the same literal colours as every other Claude status
-- surface (they must match, and are not theme-dependent).
colourOf :: Text -> Text
colourOf = \case
  "waiting"  -> "#f85149"
  "busy"     -> "#d29922"
  "shell"    -> "#d29922"
  "idle"     -> "#3fb950"
  "done"     -> "#3fb950"
  "starting" -> "#d29922"
  _          -> "#8b949e"

-- | How a row is called in a message: its name, else its title.
displayName :: AgentNode -> Text
displayName n | T.null (anName n) = anTitle n
              | otherwise         = anName n

-- | The dispatch box under the list: say what you want, then send it to the
-- selected agent, or start a new one with it (here, in a fresh worktree, or
-- branched off the selected agent's conversation).  Enter does the
-- highlighted action — "Send" while an agent is selected, "New agent"
-- otherwise; Shift+Enter is a newline.  Answers the "clear the selection"
-- clicks.
dispatchW
  :: forall t m . MonadWidget t m
  => (Text -> IO ()) -> IO () -> Dynamic t (Maybe Text) -> Dynamic t [AgentNode]
  -> m (Event t ())
dispatchW toast refresh selectedD forestD = divClass "agents-dispatch" $ mdo
  let selNodeD = (\ms f -> ms >>= \s -> listToMaybe [ n | n <- flatten f, anSession n == s ])
                   <$> selectedD <*> forestD
  clearE <- divClass "agents-dispatch-target" $ do
    dynText $ ffor selNodeD $ \case
      Just n  -> "To " <> displayName n
      Nothing -> "New agent"
    (x, _) <- elDynAttr' "button"
      (ffor selNodeD $ \m -> "class" =: "agents-action"
                          <> "title" =: "Talk to nobody in particular (start a new agent)"
                          <> maybe ("style" =: "display:none") (const mempty) m)
      (text "✕")
    return (domEvent Click x)
  ta <- textAreaElement $ def
    & textAreaElementConfig_setValue .~ ("" <$ sentE)
    & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~
        ( "class" =: "agents-dispatch-text" <> "rows" =: "2" <> "spellcheck" =: "false"
       <> "placeholder" =: "Describe a task, or reply to the selected agent…" )
  let textB = T.strip <$> current (_textAreaElement_value ta)
  sentE <- divClass "agents-dispatch-buttons" $ do
    -- The default (Enter) button carries .agents-dispatch-go, which the pane's
    -- JS clicks.
    let btn cls label tip visible = do
          (b, _) <- elDynAttr' "button"
            (ffor selNodeD $ \m -> "class" =: ("agents-dispatch-btn" <> cls m)
                                <> "title" =: tip
                                <> (if visible m then mempty else "style" =: "display:none"))
            (dynText (label <$> selNodeD))
          return (domEvent Click b)
        go m = if isJust m then "" else " agents-dispatch-go"
        goSel m = if isJust m then " agents-dispatch-go" else ""
        liveSel = maybe False anLive
    sendE <- btn goSel (\m -> "Send to " <> maybe "" displayName m)
               "Type this into the selected agent's prompt and submit it" liveSel
    newE  <- btn go (const "New agent")
               "Start a new agent in the active project with this as its task" (const True)
    wtE   <- btn (const "") (const "New in worktree")
               "Queue this task: it gets its own git worktree and agent (see the Tasks pane)"
               (const True)
    forkE <- btn (const "") (\m -> "Branch off " <> maybe "" displayName m)
               "Start an agent that continues the selected agent's conversation, beside it"
               liveSel
    let act k e = fmapMaybe (\(t, m) -> if T.null t then Nothing else Just (k, m, t))
                    (attach textB (tag (current selNodeD) e))
        actionsE = leftmost [ act Send sendE, act New newE, act Worktree wtE, act Branch forkE ]
    performEvent_ $ ffor actionsE $ \(k, m, t) -> liftIO . void . forkIO $
      dispatch k m t >> refresh
    return (() <$ actionsE)
  return clearE
  where
    dispatch k m t = case (k, m) of
      (Send, Just n) -> do
        _ <- agentSend (anSession n) True t
        toast ("Sent to " <> displayName n)
      (Branch, Just n) -> do
        r <- forkAgent (emptyForkRequest (anDir n))
               { frFrom = Just (anSession n), frPrompt = Just t }
        report r
      (Worktree, _) -> withDir m $ \d -> do
        queueAdd d t 1
        toast ("Queued: a new worktree of " <> T.pack (takeFileName d)
               <> " and an agent for it (see the Tasks pane)")
      _ -> withDir m $ \d -> do
        r <- forkAgent (emptyForkRequest d)
               { frFresh = True, frDir = Just d, frPlace = PlaceTab, frPrompt = Just t }
        report r
    report = \case
      Left err      -> toast err
      Right (_, ln) -> toast (T.takeWhile (/= '(') (T.takeWhile (/= '\n') ln))
    -- Where a new agent goes: the active project, else the selected agent's
    -- checkout.
    withDir m k = activeProjectDir >>= \case
      Just d  -> k d
      Nothing -> case m of
        Just n  -> k (anDir n)
        Nothing -> toast "Open a project first: a new agent starts in the active project"

data DispatchKind = Send | New | Worktree | Branch
  deriving (Eq, Show)

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
    -- Enter in the dispatch box does the highlighted action (Shift+Enter is a
    -- newline), and / anywhere else in the pane jumps to the search box.  In
    -- JS because preventDefault from a Haskell handler arrives too late under
    -- wkwebview's asynchronous event dispatch.
  , "    root.addEventListener('keydown', function(e){"
  , "      var t = e.target, tag = t && t.tagName;"
  , "      if (t && t.classList && t.classList.contains('agents-dispatch-text')"
  , "          && e.key === 'Enter' && !e.shiftKey && !e.isComposing) {"
  , "        e.preventDefault();"
  , "        var b = root.querySelector('.agents-dispatch-go');"
  , "        if (b) setTimeout(function(){ b.click(); }, 0);"
  , "        return;"
  , "      }"
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
    -- Top-level uls sit flush with the pane's left edge (like .terminals > ul);
    -- only nested uls take the global 20px indent.
    ".agents .agents-list > ul" ? ("margin-left" -: "0px")
    -- Head, list, dispatch box: only the list scrolls.
    ".agents" ? do
        height (pct 100)
        overflow hidden
        display flex
        "flex-direction" -: "column"
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
    ".agents .agents-list" ? do
        "flex" -: "1 1 auto"
        "min-height" -: "0"
        overflow auto
    -- Rows are ordered by CSS (see 'byKey'), so every list is a flex column.
    ".agents ul" ? do
        display flex
        "flex-direction" -: "column"
    -- The header: agent count, the state chips, the layout toggle and the
    -- re-read glyph; the search box under them.
    ".agents .agents-head" ? do
        "flex" -: "0 0 auto"
        padding (px 3) (px 0) (px 4) (px 2)
    ".agents .agents-head-row" ? do
        display flex
        "align-items" -: "center"
        "gap" -: "3px"
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
    ".agents .agents-mode" ? ("opacity" -: "0.6")
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
    -- An agent's face (see "IDE.Web.AgentFace") with its state glyph as a
    -- badge on the lower-right corner, and its name beside it.
    ".agents .agents-avatar" ? do
        "position" -: "relative"
        display flex
        "flex" -: "0 0 auto"
        width (px 20)
        height (px 20)
    ".agents .agents-face" ? display flex
    ".agents .agents-face svg" ? do
        width (px 20)
        height (px 20)
    ".agents .agents-avatar .agents-badge" ? do
        "position" -: "absolute"
        "right" -: "-3px"
        "bottom" -: "-3px"
        "line-height" -: "9px"
        "padding" -: "1px"
        "border-radius" -: "6px"
        "background" -: "var(--leksah-bg)"
    ".agents .agents-selected" ?
        ("box-shadow" -: "inset 2px 0 0 var(--leksah-selection)")
    ".agents .agents-rename" ? do
        fontSize (px 12)
        width (px 90)
        padding (px 0) (px 3) (px 0) (px 3)
    -- The activity line: dim, one line, indented under the title.
    ".agents .agents-activity" ? do
        "flex-basis" -: "100%"
        fontSize (px 11)
        color dimColor
        padding (px 0) (px 4) (px 1) (px 40)
        "white-space" -: "nowrap"
        overflow hidden
        "text-overflow" -: "ellipsis"
        fontFamily ["SF Mono", "Menlo"] [monospace]
    ".agents .agents-activity-waiting" ? color (Other "#f85149")
    ".agents .agents-role" ? do
        color dimColor
        fontSize (px 11)
        padding (px 0) (px 4) (px 1) (px 14)
    ".agents .agents-done" ? do
        color fgColor
        fontSize (px 12)
        padding (px 1) (px 4) (px 2) (px 14)
        "overflow-wrap" -: "anywhere"
        "border-left" -: "2px solid #3fb950"
        "margin-left" -: "14px"
    -- What the agent last said, under its description.
    ".agents .agents-said" ? do
        color fgMutedColor
        fontSize (px 12)
        "line-height" -: "1.4"
        padding (px 2) (px 4) (px 3) (px 14)
        "overflow-wrap" -: "anywhere"
        "white-space" -: "pre-wrap"
        "display" -: "-webkit-box"
        "-webkit-line-clamp" -: "4"
        "-webkit-box-orient" -: "vertical"
        overflow hidden
    ".agents .agents-said-label" ? do
        color dimColor
        fontStyle italic
    -- The dispatch box, pinned under the list.
    ".agents .agents-dispatch" ? do
        "flex" -: "0 0 auto"
        "border-top" -: "1px solid var(--leksah-border-faint)"
        padding (px 4) (px 4) (px 6) (px 2)
    ".agents .agents-dispatch-target" ? do
        display flex
        "align-items" -: "center"
        "gap" -: "4px"
        fontSize (px 11)
        color dimColor
        "margin-bottom" -: "3px"
    ".agents .agents-dispatch-text" ? do
        "width" -: "calc(100% - 10px)"
        "resize" -: "vertical"
        fontSize (px 12)
        padding (px 3) (px 4) (px 3) (px 4)
        color fgColor
        "background" -: "var(--leksah-surface-alt)"
        "border" -: "1px solid var(--leksah-border-faint)"
        borderRadius (px 3) (px 3) (px 3) (px 3)
        "outline" -: "none"
        "font-family" -: "inherit"
    ".agents .agents-dispatch-text:focus" ?
        ("border-color" -: "var(--leksah-selection)")
    ".agents .agents-dispatch-buttons" ? do
        display flex
        "flex-wrap" -: "wrap"
        "gap" -: "4px"
        "margin-top" -: "4px"
    ".agents .agents-dispatch-btn" ? do
        fontSize (px 11)
        padding (px 1) (px 6) (px 1) (px 6)
    ".agents .agents-dispatch-go" ?
        ("box-shadow" -: "0 0 0 1px var(--leksah-selection)")
    ".agents .agents-name" ? do
        fontWeight bold
        color fgColor
        fontSize (px 12)
        "flex" -: "0 0 auto"
    -- The same face in the workspace tree's session rows.
    ".tree-face" ? do
        "display" -: "inline-flex"
        "vertical-align" -: "middle"
        "margin-right" -: "3px"
    ".tree-face svg" ? do
        width (px 16)
        height (px 16)
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
    ".agents .agents-where-path" ? do
        opacity 0.7
        "margin-left" -: "6px"
        fontSize (px 10)
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
