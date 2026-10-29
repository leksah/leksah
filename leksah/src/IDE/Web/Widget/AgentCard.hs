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
--     actions (show, message, refresh, suspend\/resume, rename).
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
import Control.Lens ((^.), (.~), (&))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)

import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import Clay (Css, (?), (-:))

import Reflex
       (Dynamic, Event, attach, current, delay, ffilter, ffor, gate, getPostBuild,
        holdDyn, holdUniqDyn, leftmost, never, newTriggerEvent, sample,
        switchHold, tag, tagPromptlyDyn, updated)
import Reflex.Dom.Core
       (Key(Enter, Escape), blank, def, divClass, domEvent, dyn, dyn_, dynText,
        elAttr', elClass, elDynAttr, elDynAttr', elDynHtmlAttr',
        elementConfig_initialAttributes, inputElement,
        inputElementConfig_elementConfig, inputElementConfig_initialValue,
        keydown, keypress, text, EventName(..), (=:), _element_raw,
        _inputElement_element, _inputElement_hasFocus, _inputElement_value)
import Language.Javascript.JSaddle (liftJSM, js0)

import IDE.Web.AgentFace (agentFaceSvg)
import IDE.Web.AgentInfo (AgentNode(..))
import IDE.Web.Frame (MonadWidget, performEvent_)

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
  }

-- | One card.  @fullD@: this card must be shown in full (the user's ⋯ opens
-- any other).  @startOpen@: how a card the user has not touched starts.
-- Everything a card does, it does through 'CardActions': clicking it brings
-- the agent's pane up, which is where you talk to it.
agentCard
  :: forall t m . MonadWidget t m
  => CardActions -> Dynamic t Bool -> Bool -> Dynamic t AgentNode -> m ()
agentCard acts mustD startOpen nD = mdo
  openD <- holdDyn startOpen (not <$> tag (current openD) moreE)
  fullD <- holdUniqDyn ((||) <$> mustD <*> openD)
  renamingD <- holdDyn False (leftmost [True <$ renameE, False <$ doneRenameE])
  (moreE, headE, doneRenameE, bodyE) <-
    elDynAttr "div" (cardAttrs <$> nD <*> fullD) $ do
      (hd, mE, dE) <- divClass "agent-card-top" $ do
        -- The face and the three lines are the card's handle: a click there
        -- brings its agent up.  The ⋯ sits OUTSIDE it, so
        -- opening a card is not also a click on it.
        (h, d) <- elAttr' "div" ("class" =: "agent-card-handle") $ do
          -- The face, three text lines tall, with its state glyph as a badge.
          elClass "div" "agent-card-avatar" $ do
            void $ elDynHtmlAttr' "span" ("class" =: "agent-card-face") (faceOf <$> nD)
            elDynAttr "span" (badgeAttrs <$> nD) . dynText $ glyphOf . anState <$> nD
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
              return r
            -- 2: its title
            elClass "div" "agent-card-line agent-card-title" . dynText $ anTitle <$> nD
            -- 3: what it is doing now, else the gist of its description
            elDynAttr "div" (line3Attrs <$> nD) . dynText $ line3 <$> nD
            return d'
        (b, _) <- elDynAttr' "button" (moreAttrs <$> fullD <*> mustD) $
          dynText ((\f -> if f then "▴" else "⋯") <$> fullD)
        return (domEvent Click h, domEvent Click b, d)
      -- The rest only while full.
      bE <- switchHold never =<< dyn (ffor fullD $ \full ->
        if full then fullBody else return never)
      return (mE, hd, dE, bE)
  let renameE = bodyE
      clickE  = gate (not <$> current renamingD) headE
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
            ("class" =: "agent-card-rename" <> "spellcheck" =: "false")
      -- Focus it with the current name selected, so typing replaces it.  Not
      -- at postBuild itself: the input is not attached to the page yet then
      -- (wkwebview batches DOM changes), and an unattached element cannot take
      -- focus — a moment later it is.
      readyE <- delay 0.05 =<< getPostBuild
      performEvent_ $ ffor readyE $ \_ -> liftJSM $ do
        let raw = _element_raw (_inputElement_element inp)
        void $ raw ^. js0 ("focus" :: Text)
        void $ raw ^. js0 ("select" :: Text)
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
        showE    <- btn "Show" (const True)
        branchE  <- btn "Branch off" anLive
        refreshE <- btn "Refresh" anLive
        suspendE <- btn "Suspend" anLive
        resumeE  <- btn "Resume" (not . anLive)
        forgetE  <- btn "Forget" (\n -> not (anLive n) && anState n /= "suspended")
        renE     <- btn "Rename" (const True)
        let run f e = performEvent_ $ ffor (tagPromptlyDyn nD e) $ \n ->
              liftIO . void . forkIO $ f n
        run (caOpen acts) showE
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

moreAttrs :: Bool -> Bool -> Map Text Text
moreAttrs full must
  | must      = hidden          -- the top card is always full: nothing to toggle
  | otherwise = "class" =: "agent-card-more"
             <> "title" =: (if full then "Show less" else "Show more")

hidden :: Map Text Text
hidden = "style" =: "display:none"

-- | The third line: the API error its last turn ended on, else what it is
-- doing right now, else the first line of what it says about itself.
line3 :: AgentNode -> Text
line3 n | Just err <- anError n = "⚠ API error: " <> err
line3 n = case anActivity n of
  Just a | not (T.null a) -> a
  _ -> T.take 160 . T.strip . T.takeWhile (/= '\n') . stripTags
         $ fromMaybe "" (anDesc n)

bioOf :: AgentNode -> Text
bioOf n = fromMaybe "" (anBio n `orElse` anDesc n)
  where orElse a b = maybe b Just a

stripTags :: Text -> Text
stripTags t = case T.breakOn "<" t of
  (a, rest) | T.null rest -> a
            | otherwise   -> a <> " " <> stripTags (T.drop 1 (T.dropWhile (/= '>') rest))

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
    "cursor" -: "default"
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
  ".agent-card .agent-card-name" ? do
    "font-weight" -: "bold"
    "color" -: "var(--leksah-fg)"
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
  ".agent-card .agent-card-actions" ? do
    "display" -: "flex"
    "flex-wrap" -: "wrap"
    "gap" -: "4px"
    "margin-top" -: "6px"
  ".agent-card .agent-card-btn" ? do
    "font-size" -: "11px"
    "padding" -: "1px 7px"
