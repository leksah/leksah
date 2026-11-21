{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
-- | One agent as a CARD — the unit both the Agents pane (a single column) and
-- the Team pane (a grid of bios) are made of.
--
-- A card is either FULL or COLLAPSED:
--
--   * collapsed: the face (three lines of text tall, its state glyph as a
--     badge), and three lines beside it — name · state · age, its title, and
--     what it is doing (else the first line of its description) — with a ⋯
--     button that opens it;
--   * full: the same head, then its bio (else its description), the facts
--     leksah knows (checkout, branch, PR, who started it, whom it started, how
--     long it has been going), what it last said, its done summary, and the
--     actions (branch off, refresh, suspend\/resume, forget, rename).
--
-- A click anywhere on a card brings its agent's pane up (or resumes it) —
-- except on its own controls (buttons, links, the rename field) and except a
-- click that ends a text selection, so the bio and what it said stay
-- selectable.
--
-- Whether a card is full is up to the caller ('agentCard' takes a "must be
-- full" 'Dynamic' — the Agents pane's top card) and to the user (⋯).
module IDE.Web.Widget.AgentCard
  ( CardActions(..)
  , agentCard
  , agentCardCss
  , colourOf
  , glyphOf
  , displayName
  ) where

import Control.Concurrent (forkIO)
import Control.Lens ((.~), (&))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)

import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (TimeZone, getCurrentTimeZone)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)

import Clay (Css, (?), (-:))

import Reflex
       (Dynamic, Event, attach, current, delay, ffilter, ffor, gate, getPostBuild,
        holdDyn, holdUniqDyn, leftmost, never, newTriggerEvent, sample,
        tickLossyFromPostBuildTime, _tickInfo_lastUTC,
        switchHold, tag, tagPromptlyDyn, updated)
import Reflex.Dom.Core
       (Key(Enter, Escape), blank, def, divClass, domEvent, dyn, dyn_, dynText,
        elAttr, elAttr', elClass, elDynAttr, elDynAttr', elDynHtmlAttr',
        elementConfig_initialAttributes, inputElement,
        inputElementConfig_elementConfig, inputElementConfig_initialValue,
        keydown, keypress, text, wrapDomEvent, EventName(..), (=:), _element_raw,
        _inputElement_element, _inputElement_hasFocus, _inputElement_value)
import Language.Javascript.JSaddle (liftJSM)

import IDE.Web.AgentFace (agentFaceSvg)
import IDE.Web.AgentInfo (AgentNode(..), stripTags)
import IDE.Web.AgentUsage
       (usageChip, usageContextPct, usageLevel, usageLines)
import IDE.Web.Frame (MonadWidget, performEvent_)
import IDE.Web.Dom (focusElement)
import GHCJS.DOM.HTMLInputElement (select)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Element (closest)
import GHCJS.DOM.GlobalEventHandlers (click)
import GHCJS.DOM.EventM (EventM, on, target)
import GHCJS.DOM.Selection (getIsCollapsed)
import GHCJS.DOM.Types
       (Element(..), EventTarget(..), HTMLElement(..), HTMLInputElement(..),
        PointerEvent, uncheckedCastTo)
import GHCJS.DOM.Window (getSelection)
import IDE.Web.TextAssist (noTextAssist)

-- | What a card's buttons do.  Each runs off the reflex thread.
data CardActions = CardActions
  { caOpen    :: AgentNode -> IO ()          -- ^ show its pane (or resume it)
  , caRefresh :: AgentNode -> IO ()          -- ^ ask it to refresh itself
  , caSuspend :: AgentNode -> IO ()
  , caResume  :: AgentNode -> IO ()
  , caForget  :: AgentNode -> IO ()          -- ^ drop an exited agent
  , caRename  :: AgentNode -> Text -> IO ()
  , caBranch  :: AgentNode -> IO ()          -- ^ start a copy of its
                                             --   conversation beside it
  , caLastSaid :: AgentNode -> IO (Maybe Text)
  , caAnswer  :: AgentNode -> Bool -> IO ()  -- ^ answer its permission prompt:
                                             --   approve (True) or deny
  }

-- | One card.  @fullD@: this card must be shown in full (the user's ⋯ opens
-- any other).  @startOpen@: how a card the user has not touched starts.
-- Everything a card does, it does through 'CardActions': clicking it brings
-- the agent's pane up, which is where you talk to it.
agentCard
  :: forall t m . MonadWidget t m
  => CardActions -> Dynamic t Bool -> Bool -> Dynamic t AgentNode -> m ()
agentCard acts mustD startOpen nD = mdo
  tz <- liftIO getCurrentTimeZone
  -- The time now, to the half minute: a usage report says when its
  -- rate-limit windows reset and its prompt cache expires, and an idle
  -- session sends no new report when they do.
  now0 <- liftIO (realToFrac <$> getPOSIXTime)
  tickE <- tickLossyFromPostBuildTime 30
  nowD <- holdDyn now0 (realToFrac . utcTimeToPOSIXSeconds . _tickInfo_lastUTC <$> tickE)
  openD <- holdDyn startOpen (not <$> tag (current openD) moreE)
  fullD <- holdUniqDyn ((||) <$> mustD <*> openD)
  renamingD <- holdDyn False (leftmost [True <$ renameE, False <$ doneRenameE])
  (cardEl, (moreE, doneRenameE, bodyE)) <-
    elDynAttr' "div" (cardAttrs <$> nD <*> fullD) $ do
      (mE, dE) <- divClass "agent-card-top" $ do
        (_, d) <- elAttr' "div" ("class" =: "agent-card-handle") $ do
          -- The face, three text lines tall, with its state glyph as a badge.
          elClass "div" "agent-card-avatar" $ do
            void $ elDynHtmlAttr' "span" ("class" =: "agent-card-face") (faceOf <$> nD)
            elDynAttr "span" (badgeAttrs <$> nD) . dynText $ glyphOf . anState <$> nD
            -- And which CLI it is, on the other side.
            void $ elDynHtmlAttr' "span" ("class" =: "agent-card-agent") (agentGlyph . anAgent <$> nD)
          divClass "agent-card-head" $ do
            -- 1: name (or the rename field), badges, state, age
            d' <- divClass "agent-card-line agent-card-line1" $ do
              r <- switchHold never =<< dyn (ffor renamingD $ \case
                False -> do
                  elClass "span" "agent-card-name" . dynText $ displayName <$> nD
                  return never
                True -> renameField)
              elDynAttr "span" (managerAttrs <$> nD) (text "Manager")
              elDynAttr "span" (roleAttrs <$> nD) . dynText $ fromMaybe "" . anRole <$> nD
              elDynAttr "span" (stateAttrs <$> nD) . dynText $ stateWord . anState <$> nD
              elDynAttr "span" (ageAttrs <$> nD) . dynText $ anAge <$> nD
              -- How full its context window is, and what it has cost (from
              -- its status line); the details are the tooltip.
              elDynAttr "span" (usageAttrs tz <$> nowD <*> nD) $ do
                elDynAttr "span" (meterAttrs <$> nD) $
                  elDynAttr "span" (meterFillAttrs <$> nD) blank
                dynText $ maybe "" usageChip . anUsage <$> nD
              return r
            -- 2: its title
            elClass "div" "agent-card-line agent-card-title" . dynText $ anTitle <$> nD
            -- 3: what it is doing now, else the gist of its description
            elDynAttr "div" (line3Attrs <$> nD) . dynText $ line3 <$> nD
            -- Blocked on a permission prompt: answer it from here (the
            -- details are line 3, "needs approval: Bash: rm -rf …").  On the
            -- compact card too — a card that needs you is the one to act on.
            approvalD <- holdUniqDyn (anApproval <$> nD)
            answerE <- switchHold never =<< dyn (ffor approvalD $ \case
              Nothing -> return never
              Just what -> elAttr "div" ("class" =: "agent-card-approval"
                                         <> "title" =: what) $ do
                (y, _) <- elAttr' "button" ("class" =: "agent-card-approve"
                                            <> "title" =: ("Approve: " <> what)) (text "Approve")
                (n, _) <- elAttr' "button" ("class" =: "agent-card-deny"
                                            <> "title" =: "Deny (Esc in its pane)") (text "Deny")
                return (leftmost [True <$ domEvent Click y, False <$ domEvent Click n]))
            performEvent_ $ ffor (attach (current nD) answerE) $ \(n, yes) ->
              liftIO . void . forkIO $ caAnswer acts n yes
            return d'
        (b, _) <- elDynAttr' "button" (moreAttrs <$> fullD <*> mustD) $
          dynText ((\f -> if f then "▴" else "⋯") <$> fullD)
        return (domEvent Click b, d)
      -- The rest only while full.
      bE <- switchHold never =<< dyn (ffor fullD $ \full ->
        if full then fullBody else return never)
      return (mE, dE, bE)
  -- A click anywhere on the card that is not on one of its controls (the ⋯,
  -- the action buttons, a link, the rename field) and does not end a text
  -- selection.
  cardClickE <- fmap (ffilter id) . wrapDomEvent (uncheckedCastTo HTMLElement (_element_raw cardEl)) (`on` click) $ do
    t <- target :: EventM HTMLElement PointerEvent (Maybe EventTarget)
    onControl <- case t of
      Nothing -> return False
      Just el -> isJust <$> closest (uncheckedCastTo Element el)
                   ("button, a, input, textarea, .agent-link" :: Text)
    selected <- getSelection =<< currentWindowUnchecked
    collapsed <- maybe (return True) getIsCollapsed selected
    return (not onControl && collapsed)
  let renameE = bodyE
      clickE  = gate (not <$> current renamingD) (() <$ cardClickE)
  performEvent_ $ ffor (tagPromptlyDyn nD clickE) $ \n ->
    liftIO . void . forkIO $ caOpen acts n
  where
    -- The inline rename field: Enter keeps the name, Esc or leaving it gives
    -- up.  (No modal prompt — a browser dialog blocks every event in the
    -- window.)
    renameField :: m (Event t ())
    renameField = do
      n0 <- sample (current nD)
      inp <- inputElement $ def
        & inputElementConfig_initialValue .~ anName n0
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("class" =: "agent-card-rename" <> noTextAssist)
      -- Focus it with the current name selected, so typing replaces it.  Not
      -- at postBuild itself: the input is not attached to the page yet then
      -- (wkwebview batches DOM changes), and an unattached element cannot take
      -- focus — a moment later it is.
      readyE <- delay 0.05 =<< getPostBuild
      performEvent_ $ ffor readyE $ \_ -> liftJSM $ do
        let raw = _element_raw (_inputElement_element inp)
        focusElement raw
        select (uncheckedCastTo HTMLInputElement raw)
      let commitE = tag (current (_inputElement_value inp)) (keypress Enter inp)
          cancelE = leftmost [ () <$ keydown Escape inp
                             , () <$ ffilter not (updated (_inputElement_hasFocus inp)) ]
      performEvent_ $ ffor (attach (current nD) commitE) $ \(n, new) ->
        liftIO . void . forkIO $ caRename acts n new
      return (leftmost [() <$ commitE, cancelE])

    -- The full card's body; answers its Rename button.
    fullBody :: m (Event t ())
    fullBody = do
      -- The bio (else the description): sanitized HTML, straight into
      -- innerHTML — sanitized once, where it was stored.
      void $ elDynHtmlAttr' "div" ("class" =: "agent-card-bio")
        (bioOf <$> nD)
      -- The facts leksah knows itself.
      now <- liftIO (realToFrac <$> getPOSIXTime)
      divClass "agent-card-facts" $ do
        elClass "span" "agent-card-fact" . dynText $ (T.pack . baseName . anDir) <$> nD
        elDynAttr "span" (branchAttrs <$> nD) . dynText $ fromMaybe "" . anBranch <$> nD
        elDynAttr "a" (prAttrs <$> nD) . dynText $
          ffor nD $ \n -> maybe "" (\(k, _) -> "PR #" <> T.pack (show k)) (anPr n)
        elDynAttr "span" (parentAttrs <$> nD) . dynText $
          ffor nD $ \n -> maybe "" ("started by " <>) (anParentName n)
        elDynAttr "span" (childrenAttrs <$> nD) . dynText $
          ffor nD $ \n -> "started " <> T.intercalate ", " (anChildNames n)
        elDynAttr "span" (sinceAttrs <$> nD) . dynText $
          ffor nD $ \n -> maybe "" (\b -> "since " <> ago now b) (anBorn n)
        -- Where its shell has gone, and what it has edited (from the hooks).
        elDynAttr "span" (cwdAttrs <$> nD) . dynText $
          ffor nD $ \n -> maybe "" (\c -> "now in " <> T.pack (baseName c)) (anCwd n)
        elDynAttr "span" (editsAttrs <$> nD) . dynText $ editsText <$> nD
      dyn_ $ ffor nD $ \n ->
        forM_ (anWorktrees n) $ \l -> divClass "agent-card-worktree" (text l)
      dyn_ $ ffor nD $ \n -> forM_ (anDone n) $ \d ->
        divClass "agent-card-done" $ do
          elClass "span" "agent-card-label" (text "Done ")
          text d
      -- What it last said — fetched when the card opens, and again whenever
      -- its state changes (a turn that ends is a new answer).
      pbC <- getPostBuild
      stateC <- holdUniqDyn (anState <$> nD)
      (saidE, fireSaid) <- newTriggerEvent
      performEvent_ $ ffor (tag (current nD) (leftmost [pbC, () <$ updated stateC])) $ \n ->
        liftIO . void . forkIO $ caLastSaid acts n >>= fireSaid
      saidD <- holdDyn Nothing saidE
      dyn_ $ ffor saidD $ \case
        Nothing -> blank
        Just t  -> divClass "agent-card-said" $ do
          elClass "span" "agent-card-label" (text "Last said ")
          text (T.take 600 t)
      -- The actions.
      divClass "agent-card-actions" $ do
        branchE  <- btn "Branch off" anLive
        refreshE <- btn "Refresh" anLive
        suspendE <- btn "Suspend" anLive
        resumeE  <- btn "Resume" (not . anLive)
        forgetE  <- btn "Forget" (\n -> not (anLive n) && anState n /= "suspended")
        renE     <- btn "Rename" (const True)
        let run f e = performEvent_ $ ffor (tagPromptlyDyn nD e) $ \n ->
              liftIO . void . forkIO $ f n
        run (caBranch acts) branchE
        run (caRefresh acts) refreshE
        run (caSuspend acts) suspendE
        run (caResume acts) resumeE
        run (caForget acts) forgetE
        return renE

    btn :: Text -> (AgentNode -> Bool) -> m (Event t ())
    btn label visible = do
      (b, _) <- elDynAttr' "button"
        (ffor nD $ \n -> "class" =: "agent-card-btn"
                      <> (if visible n then mempty else "style" =: "display:none"))
        (text label)
      return (domEvent Click b)

-- | The card's own attributes: its state and mode as classes.
cardAttrs :: AgentNode -> Bool -> Map Text Text
cardAttrs n full =
  "class" =: T.unwords
    ([ "agent-card", if full then "agent-card-full" else "agent-card-compact"
     , "agent-card-" <> anState n ]
     <> [ "agent-card-gone" | not (anLive n) ]
     <> [ "agent-card-manager" | anManager n ])
  <> "data-session" =: anSession n
  -- The agent's pane, for dragging the card (see 'leafDragJs' in IDE.Web.Main,
  -- which looks the name up in @window.__leksahAgentPanes@).
  <> "data-agent-name" =: anName n
  <> "title" =: (anDetail n <> "\n" <> T.pack (anDir n) <> "\n" <> anSession n)

faceOf :: AgentNode -> Text
faceOf n | T.null (anName n) = "<img class=\"agent-card-robot\" src=\"/pics/tree-claude.svg\">"
         | otherwise         = agentFaceSvg (anName n)

-- | How a card is called: its name, else its title.
displayName :: AgentNode -> Text
displayName n | T.null (anName n) = T.take 8 (anSession n)
              | otherwise         = anName n

badgeAttrs, managerAttrs, roleAttrs, stateAttrs, ageAttrs, line3Attrs, branchAttrs,
  prAttrs, parentAttrs, childrenAttrs, sinceAttrs, cwdAttrs, editsAttrs
  :: AgentNode -> Map Text Text
badgeAttrs n = "class" =: "agent-card-badge" <> "style" =: ("color:" <> colourOf (anState n))
-- | The badge on the left of the face: which CLI the agent runs in — Codex's
-- logo (its rounded cloud with a @>_@ prompt cut out), Antigravity's arch, or
-- Claude Code's robot.
-- Drawn, not typed: a font glyph sits on its baseline, so it never lines up
-- with the state glyph opposite; these are centred in their box by
-- construction.  The Codex mark is its mid-blue rather than its gradient: the
-- same SVG on every card would repeat the gradient's id, and a gradient
-- referenced from a hidden card need not paint.
agentGlyph :: Text -> Text
agentGlyph "codex" =
  "<svg class=\"agent-glyph-codex\" viewBox=\"0 0 24 24\" fill=\"#7a9dff\" fill-rule=\"evenodd\">\
  \<title>Codex agent</title><path clip-rule=\"evenodd\" d=\"\
  \M8.086.457a6.105 6.105 0 013.046-.415c1.333.153 2.521.72 3.564 1.7\
  \a.117.117 0 00.107.029c1.408-.346 2.762-.224 4.061.366l.063.03.154\
  \.076c1.357.703 2.33 1.77 2.918 3.198.278.679.418 1.388.421 2.126a5\
  \.655 5.655 0 01-.18 1.631.167.167 0 00.04.155 5.982 5.982 0 011.57\
  \8 2.891c.385 1.901-.01 3.615-1.183 5.14l-.182.22a6.063 6.063 0 01-\
  \2.934 1.851.162.162 0 00-.108.102c-.255.736-.511 1.364-.987 1.992-\
  \1.199 1.582-2.962 2.462-4.948 2.451-1.583-.008-2.986-.587-4.21-1.7\
  \36a.145.145 0 00-.14-.032c-.518.167-1.04.191-1.604.185a5.924 5.924\
  \ 0 01-2.595-.622 6.058 6.058 0 01-2.146-1.781c-.203-.269-.404-.522\
  \-.551-.821a7.74 7.74 0 01-.495-1.283 6.11 6.11 0 01-.017-3.064.166\
  \.166 0 00.008-.074.115.115 0 00-.037-.064 5.958 5.958 0 01-1.38-2.\
  \202 5.196 5.196 0 01-.333-1.589 6.915 6.915 0 01.188-2.132c.45-1.4\
  \84 1.309-2.648 2.577-3.493.282-.188.55-.334.802-.438.286-.12.573-.\
  \22.861-.304a.129.129 0 00.087-.087A6.016 6.016 0 015.635 2.31C6.31\
  \5 1.464 7.132.846 8.086.457zm-.804 7.85a.848.848 0 00-1.473.842l1.\
  \694 2.965-1.688 2.848a.849.849 0 001.46.864l1.94-3.272a.849.849 0 \
  \00.007-.854l-1.94-3.393zm5.446 6.24a.849.849 0 000 1.695h4.848a.84\
  \9.849 0 000-1.696h-4.848z\"/></svg>"
-- Claude Code's: its little orange robot, exactly as its banner draws it in
-- quadrant blocks:
--
-- >  ▐▛███▛█
-- > ▝▜██████▀
-- >  ▝▝   ▝▝
--
-- one unit per quadrant, two tall because a terminal cell is twice as tall as
-- it is wide.  The eyes are the gaps in the second row: the badge shows
-- through them.
-- Antigravity's arch, as the agy CLI's banner draws it in half blocks:
--
-- >     ▄▀▀▄
-- >    ▀▀▀▀▀▀
-- >   ▀▀▀▀▀▀▀▀
-- >  ▄▀▀    ▀▀▄
-- > ▄▀▀      ▀▀▄
--
-- (each @▀@ there has a background colour too, so it is a whole cell; a @▄@
-- is the lower half) — one unit wide, two tall per cell, like the robot.  In
-- the middle blue of the banner's green-to-blue gradient.
agentGlyph "agy" =
  "<svg class=\"agent-glyph-agy\" viewBox=\"1 0 12 10\" fill=\"#5fafd7\" \
  \shape-rendering=\"crispEdges\"><title>Antigravity agent</title><path d=\"\
  \M5 1h1v1H5zM6 0h2v2H6zM8 1h1v1H8zM4 2h6v2H4zM3 4h8v2H3z\
  \M2 7h1v1H2zM3 6h2v2H3zM9 6h2v2H9zM11 7h1v1h-1z\
  \M1 9h1v1H1zM2 8h2v2H2zM10 8h2v2h-2zM12 9h1v1h-1z\"/></svg>"
agentGlyph _ =
  "<svg class=\"agent-glyph-claude\" viewBox=\"1 0 17 10\" fill=\"#d97757\" \
  \shape-rendering=\"crispEdges\"><title>Claude Code agent</title><path d=\"\
  \M3 0h13v2H3zM3 2h2v2H3zM6 2h7v2H6zM14 2h2v2h-2zM1 4h17v2H1zM3 6h13v2H3z\
  \M3 8h1v2H3zM5 8h1v2H5zM13 8h1v2h-1zM15 8h1v2h-1z\"/></svg>"
managerAttrs n = if anManager n then "class" =: "agent-card-tag agent-card-tag-manager"
                                else hidden
roleAttrs n = maybe hidden (const ("class" =: "agent-card-tag")) (anRole n)
stateAttrs n = "class" =: "agent-card-state" <> "style" =: ("color:" <> colourOf (anState n))
ageAttrs n = if T.null (anAge n) then hidden else "class" =: "agent-card-age"
line3Attrs n = if T.null (line3 n) then hidden
               else "class" =: ("agent-card-line agent-card-activity"
                                <> (if isJust (anError n) then " agent-card-error"
                                    else if isJust (anActivity n) then " agent-card-now" else ""))
                 <> "title" =: line3 n
branchAttrs n = maybe hidden (const ("class" =: "agent-card-fact")) (anBranch n)
prAttrs n = case anPr n of
  Nothing       -> hidden
  Just (_, url) -> "class" =: "agent-card-fact agent-link" <> "data-href" =: url
                <> "title" =: url
parentAttrs n = maybe hidden (const ("class" =: "agent-card-fact")) (anParentName n)
childrenAttrs n = if null (anChildNames n) then hidden else "class" =: "agent-card-fact"
sinceAttrs n = maybe hidden (const ("class" =: "agent-card-fact")) (anBorn n)
cwdAttrs n = maybe hidden (\c -> "class" =: "agent-card-fact" <> "title" =: T.pack c) (anCwd n)
editsAttrs n
  | null (anEdits n) = hidden
  | otherwise = "class" =: "agent-card-fact agent-card-edits"
             <> "title" =: T.intercalate "\n" (map T.pack (anEdits n))

-- | "edited: Main.hs, Agents.hs +3" — the latest few files it edited.
editsText :: AgentNode -> Text
editsText n = case anEdits n of
  [] -> ""
  fs -> "edited: " <> T.intercalate ", " (map (T.pack . baseName) (take 3 fs))
     <> (if length fs > 3 then " +" <> T.pack (show (length fs - 3)) else "")

-- | The usage figure on the first line: hidden until its status line has
-- reported, coloured as its context window fills ('usageLevel').  The
-- tooltip's reset times and cache expiry are judged against @now@.
usageAttrs :: TimeZone -> Double -> AgentNode -> Map Text Text
usageAttrs tz now n = case anUsage n of
  Just u | not (T.null (usageChip u)) ->
    "class" =: T.unwords ([ "agent-card-usage" ]
                          <> [ "agent-card-usage-" <> l | let l = usageLevel u, not (T.null l) ])
    <> "title" =: T.intercalate "\n" (usageLines tz now u)
  _ -> hidden

meterAttrs, meterFillAttrs :: AgentNode -> Map Text Text
meterAttrs n = maybe hidden (const ("class" =: "agent-card-meter"))
                 (anUsage n >>= usageContextPct)
meterFillAttrs n = case anUsage n >>= usageContextPct of
  Nothing -> hidden
  Just p  -> "class" =: "agent-card-meter-fill"
          <> "style" =: ("width:" <> T.pack (show (max 0 (min 100 p))) <> "%")

moreAttrs :: Bool -> Bool -> Map Text Text
moreAttrs full must
  | must      = hidden          -- the top card is always full: nothing to toggle
  | otherwise = "class" =: "agent-card-more"
             <> "title" =: (if full then "Show less" else "Show more")

hidden :: Map Text Text
hidden = "style" =: "display:none"

-- | The third line: the API error its last turn ended on, else what it is
-- doing right now, else the first line of what it says about itself, else the
-- first line of the last thing it said (most agents never describe
-- themselves: without this an idle one's card is its title alone).
line3 :: AgentNode -> Text
line3 n | Just err <- anError n = "⚠ API error: " <> err
line3 n = case anActivity n of
  Just a | not (T.null a) -> a
  _ -> case T.take 160 . T.strip . T.takeWhile (/= '\n') . stripTags
              $ fromMaybe "" (anDesc n) of
         "" -> maybe "" (T.take 160) (anLastSaid n)
         d  -> d

bioOf :: AgentNode -> Text
bioOf n = fromMaybe "" (anBio n `orElse` anDesc n)
  where orElse a b = maybe b Just a

baseName :: FilePath -> FilePath
baseName = reverse . takeWhile (/= '/') . dropWhile (== '/') . reverse

ago :: Double -> Double -> Text
ago now t
  | s < 90    = "just now"
  | s < 5400  = T.pack (show (s `div` 60)) <> "m"
  | s < 129600 = T.pack (show (s `div` 3600)) <> "h"
  | otherwise = T.pack (show (s `div` 86400)) <> "d"
  where s = round (now - t) :: Integer

-- | The state word a card shows.
stateWord :: Text -> Text
stateWord = \case
  "waiting"   -> "needs you"
  "busy"      -> "working"
  "shell"     -> "working"
  "idle"      -> "idle"
  "done"      -> "done"
  "starting"  -> "starting"
  "suspended" -> "suspended"
  "interrupted" -> "interrupted"
  _           -> "exited"

-- | Shape AND colour per state, like the workspace tree's badges, so the
-- states stay tellable apart without colour vision.
glyphOf :: Text -> Text
glyphOf = \case
  "waiting"   -> "\9650"   -- ▲ blocked on an approval prompt: it needs you
  "busy"      -> "\9670"   -- ◆ working
  "shell"     -> "\9670"
  "idle"      -> "\9679"   -- ● ready for input
  "done"      -> "\10003"  -- ✓ said it is done (`agent done`)
  "starting"  -> "\9678"   -- ◎ a pane exists, no session registered yet
  "suspended" -> "\10074"  -- ❚ suspended: its process ended on purpose
  "interrupted" -> "\8635" -- ↻ its host went down under it: resume it
  _           -> "\9675"   -- ○ exited

-- | The state colours — the same literal colours as every other Claude status
-- surface (they must match, and are not theme-dependent).
colourOf :: Text -> Text
colourOf = \case
  "waiting"   -> "#f85149"
  "busy"      -> "#d29922"
  "shell"     -> "#d29922"
  "idle"      -> "#3fb950"
  "done"      -> "#3fb950"
  "starting"  -> "#d29922"
  "suspended" -> "#58a6ff"
  "interrupted" -> "#db6d28"
  _           -> "#8b949e"

-- | Card styling, shared by both panes.  The face is three lines of the card's
-- text tall: the head's three lines are 12px at line-height 1.35.
agentCardCss :: Css
agentCardCss = do
  ".agent-card" ? do
    "border" -: "1px solid var(--leksah-border-faint)"
    "border-radius" -: "6px"
    "padding" -: "6px 8px"
    "background" -: "var(--leksah-bg)"
    "font-size" -: "12px"
    "line-height" -: "1.35"
    "min-width" -: "0"
    -- The whole card is the click target that brings its agent up.
    "cursor" -: "pointer"
  ".agent-card button, .agent-card input" ? ("cursor" -: "auto")
  ".agent-card .agent-card-bio, .agent-card .agent-card-said" ? ("cursor" -: "text")
  ".agent-card.agent-card-manager" ?
    ("border-color" -: "#a371f7")
  ".agent-card.agent-card-gone" ? ("opacity" -: "0.6")
  ".agent-card .agent-card-top" ? do
    "display" -: "flex"
    "align-items" -: "flex-start"
    "gap" -: "4px"
  ".agent-card .agent-card-handle" ? do
    "display" -: "flex"
    "align-items" -: "flex-start"
    "gap" -: "8px"
    "flex" -: "1 1 auto"
    "min-width" -: "0"
  ".agent-card .agent-card-avatar" ? do
    "position" -: "relative"
    "flex" -: "0 0 auto"
    "width" -: "calc(3 * 1.35em)"
    "height" -: "calc(3 * 1.35em)"
  ".agent-card .agent-card-face, .agent-card .agent-card-face svg, .agent-card .agent-card-robot" ? do
    "display" -: "block"
    "width" -: "100%"
    "height" -: "100%"
  ".agent-card .agent-card-badge" ? do
    "position" -: "absolute"
    "right" -: "-3px"
    "bottom" -: "-3px"
    "font-size" -: "10px"
    "line-height" -: "11px"
    "padding" -: "1px 2px"
    "border-radius" -: "7px"
    "background" -: "var(--leksah-bg)"
  -- The same box as the state badge opposite (13px tall: 11px line + 1px
  -- padding each way), its drawing centred in it.
  ".agent-card .agent-card-agent" ? do
    "position" -: "absolute"
    "left" -: "-3px"
    "bottom" -: "-3px"
    "box-sizing" -: "border-box"
    "width" -: "13px"
    "height" -: "13px"
    "display" -: "flex"
    "align-items" -: "center"
    "justify-content" -: "center"
    "border-radius" -: "7px"
    "background" -: "var(--leksah-bg)"
  ".agent-card .agent-card-agent svg" ? do
    "display" -: "block"
    "width" -: "9px"
    "height" -: "9px"
  -- The Codex cloud fills its box where the spark is open strokes: a size
  -- up keeps its cut-out prompt legible.
  ".agent-card .agent-card-agent svg.agent-glyph-codex" ? do
    "width" -: "11px"
    "height" -: "11px"
  -- The arch is 12:10.
  ".agent-card .agent-card-agent svg.agent-glyph-agy" ? do
    "width" -: "11px"
    "height" -: "9px"
  -- The robot is wide and short (17:10): as wide as the badge allows.
  ".agent-card .agent-card-agent svg.agent-glyph-claude" ? do
    "width" -: "12px"
    "height" -: "8px"
  ".agent-card .agent-card-head" ? do
    "flex" -: "1 1 auto"
    "min-width" -: "0"
  ".agent-card .agent-card-line" ? do
    "white-space" -: "nowrap"
    "overflow" -: "hidden"
    "text-overflow" -: "ellipsis"
    "height" -: "1.35em"
  ".agent-card .agent-card-line1" ? do
    "display" -: "flex"
    "align-items" -: "baseline"
    "gap" -: "6px"
  -- In a narrow pane the first line's spans give way (with an ellipsis) so
  -- that the usage figure at its end stays whole; the name last of all.
  ".agent-card .agent-card-line1 > span:not(.agent-card-usage)" ? do
    "flex" -: "0 3 auto"
    "min-width" -: "0"
    "overflow" -: "hidden"
    "text-overflow" -: "ellipsis"
    "white-space" -: "nowrap"
  ".agent-card .agent-card-name" ? do
    "font-weight" -: "bold"
    "color" -: "var(--leksah-fg)"
  ".agent-card .agent-card-line1 > span.agent-card-name" ? ("flex-shrink" -: "1")
  ".agent-card .agent-card-tag" ? do
    "font-size" -: "10px"
    "padding" -: "0 4px"
    "border-radius" -: "3px"
    "background" -: "var(--leksah-surface-alt)"
    "color" -: "var(--leksah-fg-muted)"
  ".agent-card .agent-card-tag-manager" ? do
    "background" -: "#a371f7"
    "color" -: "#fff"
  ".agent-card .agent-card-state" ? ("font-size" -: "11px")
  ".agent-card .agent-card-age" ? do
    "font-size" -: "11px"
    "color" -: "var(--leksah-dim)"
  ".agent-card .agent-card-title" ? ("color" -: "var(--leksah-fg)")
  -- The usage figure sits at the right of the first line: a small context
  -- meter and "43% · $1.20", dim until the context window is nearly full.
  ".agent-card .agent-card-usage" ? do
    "margin-left" -: "auto"
    "flex" -: "0 0 auto"
    "font-size" -: "11px"
    "color" -: "var(--leksah-dim)"
    "font-variant-numeric" -: "tabular-nums"
  ".agent-card .agent-card-usage-high" ? ("color" -: "#db6d28")
  ".agent-card .agent-card-usage-full" ? ("color" -: "#f85149")
  ".agent-card .agent-card-meter" ? do
    "display" -: "inline-block"
    "width" -: "22px"
    "height" -: "4px"
    "margin-right" -: "4px"
    "border-radius" -: "2px"
    "background" -: "var(--leksah-surface-alt)"
    "overflow" -: "hidden"
    "vertical-align" -: "middle"
  ".agent-card .agent-card-meter-fill" ? do
    "display" -: "block"
    "height" -: "100%"
    "background" -: "currentColor"
  ".agent-card .agent-card-activity" ? ("color" -: "var(--leksah-dim)")
  ".agent-card .agent-card-activity.agent-card-now" ? do
    "font-family" -: "'SF Mono', Menlo, monospace"
    "font-size" -: "11px"
  ".agent-card-waiting .agent-card-activity" ? ("color" -: "#f85149")
  ".agent-card .agent-card-activity.agent-card-error" ? ("color" -: "#f85149")
  ".agent-card .agent-card-more" ? do
    "flex" -: "0 0 auto"
    "background" -: "none"
    "border" -: "none"
    "color" -: "var(--leksah-dim)"
    "font-size" -: "14px"
    "line-height" -: "1"
    "padding" -: "0 2px"
    "cursor" -: "pointer"
  ".agent-card .agent-card-more:hover" ? ("color" -: "var(--leksah-fg)")
  -- The rename field takes the name's place exactly: one line tall (the line
  -- clips anything taller), border-box so its padding and border fit inside
  -- that, and in the name's own bold face so nothing shifts when it appears.
  ".agent-card .agent-card-rename" ? do
    "box-sizing" -: "border-box"
    "height" -: "1.35em"
    "margin" -: "0"
    "padding" -: "0 3px"
    "font" -: "inherit"
    "font-weight" -: "bold"
    "line-height" -: "normal"
    "color" -: "var(--leksah-fg)"
    "background" -: "var(--leksah-surface-alt)"
    "border" -: "1px solid var(--leksah-selection)"
    "border-radius" -: "3px"
    "outline" -: "none"
    "align-self" -: "center"
    "flex" -: "0 1 9em"
    "min-width" -: "4em"
  ".agent-card .agent-card-bio" ? do
    "margin" -: "6px 0 2px 0"
    "color" -: "var(--leksah-fg-muted)"
    "overflow-wrap" -: "anywhere"
  ".agent-card .agent-card-bio:empty" ? ("display" -: "none")
  ".agent-card .agent-card-bio p" ? ("margin" -: "0 0 3px 0")
  ".agent-card .agent-card-bio code" ? do
    "font-size" -: "11px"
    "background" -: "var(--leksah-surface-alt)"
    "border-radius" -: "3px"
    "padding" -: "0 3px"
  ".agent-card a.agent-link" ? do
    "color" -: "var(--leksah-selection)"
    "cursor" -: "pointer"
    "text-decoration" -: "underline"
  ".agent-card .agent-card-facts" ? do
    "display" -: "flex"
    "flex-wrap" -: "wrap"
    "gap" -: "2px 10px"
    "margin" -: "4px 0"
    "font-size" -: "11px"
    "color" -: "var(--leksah-dim)"
  ".agent-card .agent-card-worktree" ? do
    "font-size" -: "11px"
    "font-style" -: "italic"
    "color" -: "var(--leksah-dim)"
    "overflow-wrap" -: "anywhere"
  ".agent-card .agent-card-done" ? do
    "margin" -: "4px 0"
    "padding-left" -: "6px"
    "border-left" -: "2px solid #3fb950"
    "overflow-wrap" -: "anywhere"
  ".agent-card .agent-card-said" ? do
    "margin" -: "4px 0"
    "color" -: "var(--leksah-fg-muted)"
    "white-space" -: "pre-wrap"
    "overflow-wrap" -: "anywhere"
    "display" -: "-webkit-box"
    "-webkit-line-clamp" -: "5"
    "-webkit-box-orient" -: "vertical"
    "overflow" -: "hidden"
  ".agent-card .agent-card-label" ? do
    "color" -: "var(--leksah-dim)"
    "font-style" -: "italic"
  -- Approve / Deny under line 3 while it waits on a permission prompt.
  ".agent-card .agent-card-approval" ? do
    "display" -: "flex"
    "gap" -: "6px"
    "margin-top" -: "3px"
  ".agent-card .agent-card-approval button" ? do
    "font-size" -: "11px"
    "padding" -: "1px 9px"
    "border-radius" -: "4px"
    "cursor" -: "pointer"
    "border" -: "1px solid currentColor"
    "background" -: "transparent"
  ".agent-card .agent-card-approve" ? do
    "color" -: "#fff"
    "background" -: "#3fb950 !important"
    "border-color" -: "#3fb950 !important"
  ".agent-card .agent-card-deny" ? ("color" -: "#f85149")
  ".agent-card .agent-card-actions" ? do
    "display" -: "flex"
    "flex-wrap" -: "wrap"
    "gap" -: "4px"
    "margin-top" -: "6px"
  ".agent-card .agent-card-btn" ? do
    "font-size" -: "11px"
    "padding" -: "1px 7px"
