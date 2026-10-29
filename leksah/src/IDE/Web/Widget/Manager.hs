{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
-- | The Manager pane, in the bottom bar: the team's manager agent as a chat.
-- Its recent conversation above (read from its transcript), a line to talk to
-- it below — so you can hand it work from wherever you are, and see what it
-- says back, without going to its pane.
--
-- Not its terminal: the bottom bar is built once per OS window, which a tmux
-- control client cannot be.  Typing goes to the manager's own pane (as if you
-- had typed there), so the line refuses while the manager is waiting on an
-- approval prompt — the text would land in the prompt — and offers its pane
-- instead.  The same for a manager that is not running (resume it) or that
-- does not exist yet (start one).
module IDE.Web.Widget.Manager
  ( managerCss
  , managerWidget
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.), (.~), (&))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

import Clay (Css, (?), (-:))

import Reflex
       (Dynamic, Event, attach, current, delay, ffor, getPostBuild, holdDyn, holdUniqDyn,
        leftmost, never, newTriggerEvent, tag, tickLossyFromPostBuildTime,
        updated)
import Reflex.Dom.Core
       (Key(Enter), blank, def, divClass, domEvent, dyn_, elAttr, elAttr', elClass,
        elClass', elDynHtmlAttr', elementConfig_initialAttributes, inputElement,
        inputElementConfig_elementConfig, inputElementConfig_setValue, keypress,
        text, EventName(..), (=:), _element_raw, _inputElement_value)
import Language.Javascript.JSaddle (liftJSM, jsg, js, js1, jss)

import IDE.Web.Agent
       (agentConversation, agentManagerStart, agentResume, agentSend,
        showAgentPane)
import IDE.Web.AgentFace (agentFaceSvg)
import IDE.Web.AgentInfo (AgentNode(..), agentForest)
import IDE.Web.Widget.AgentCard (colourOf, displayName)
import IDE.Web.Widget.Agents (activeProjectDir)
import IDE.Web.Frame (MonadWidget, performEvent_)

-- | What the pane shows: the manager (if there is one) and its last turns.
data View = View
  { vManager :: Maybe AgentNode
  , vTurns   :: [(Bool, Text)]   -- ^ @(fromUser, text)@, oldest first
  } deriving (Eq, Show)

managerWidget :: forall t m . MonadWidget t m => m (Event t ())
managerWidget = do
  _ <- elClass' "div" "manager" $ do
    pb   <- getPostBuild
    tick <- tickLossyFromPostBuildTime 3
    (viewE, fireView) <- newTriggerEvent
    -- Read off the reflex thread: the forest and a transcript tail.
    let refresh = void . forkIO $ do
          forest <- agentForest
          let mgr = find anManager (concatMap everyone forest)
          turns <- maybe (return []) (\n -> agentConversation (anSession n) 30) mgr
          fireView (View mgr turns)
        everyone n = n : concatMap everyone (anChildren n)
    (againE, again) <- newTriggerEvent
    performEvent_ $ liftIO refresh <$ leftmost [() <$ pb, () <$ tick, againE]
    viewD <- holdUniqDyn =<< holdDyn (View Nothing []) viewE
    mgrD  <- holdUniqDyn (vManager <$> viewD)
    (toastE, toast) <- newTriggerEvent
    performEvent_ $ ffor toastE $ \msg -> liftJSM . void $
      jsg ("window" :: Text) ^. js1 ("__leksahBridgeToast" :: Text) (msg :: Text)
    -- Soon after an action, look again (the manager's answer takes a moment).
    let soon = void . forkIO $ threadDelay 1500000 >> again ()

    -- The head: who the manager is and how it is, and what you can do with it.
    divClass "manager-head" . dyn_ $ ffor mgrD $ \case
      Nothing -> do
        elClass "span" "manager-none" $ text
          "No manager yet: it routes your tasks to the right agent (or starts \
          \one), keeps the team's descriptions current, and suspends and \
          \resumes agents."
        startE <- button "Start a manager"
        performEvent_ $ ffor startE $ \_ -> liftIO . void . forkIO $
          activeProjectDir >>= \case
            Nothing -> toast "Open a project first: the manager starts in the active project"
            Just d  -> agentManagerStart d >>= either toast (const soon)
      Just n -> do
        void $ elDynHtmlAttr' "span" ("class" =: "manager-face") (pure (agentFaceSvg (anName n)))
        elClass "span" "manager-name" (text (displayName n))
        elClass "span" "manager-tag" (text "Manager")
        elAttr "span" ("class" =: "manager-state"
                       <> "style" =: ("color:" <> colourOf (anState n))) (text (stateLine n))
        elClass "span" "manager-spacer" blank
        if anLive n
          then do
            openE <- button "Open pane"
            performEvent_ $ ffor openE $ \_ -> liftIO . void . forkIO . void $
              showAgentPane (anSession n)
          else do
            resumeE <- button "Resume"
            performEvent_ $ ffor resumeE $ \_ -> liftIO . void . forkIO $
              agentResume (anSession n) >>= either toast (const soon)

    -- The conversation, newest at the bottom.
    (chat, _) <- elClass' "div" "manager-chat" . dyn_ $ ffor viewD $ \v ->
      case (vManager v, vTurns v) of
        (Nothing, _) -> blank
        (Just _, []) -> elClass "div" "manager-empty" $ text "Nothing said yet."
        (Just n, ts) -> forM_ ts $ \(fromUser, t) ->
          elClass "div" ("manager-turn " <> if fromUser then "manager-you" else "manager-it") $ do
            elClass "span" "manager-who" (text (if fromUser then "You" else displayName n))
            elClass "div" "manager-text" (text (T.strip t))
    -- Keep the newest turn in view.
    scrollE <- delay 0.05 (updated viewD)
    performEvent_ $ ffor scrollE $ \_ -> liftJSM $ do
      let raw = _element_raw chat
      h <- raw ^. js ("scrollHeight" :: Text)
      void $ raw ^. jss ("scrollTop" :: Text) h

    -- The line: Enter (or Send) types it into the manager's pane and submits.
    divClass "manager-input" $ mdoInput toast soon mgrD
  return never
  where
    stateLine n = case anState n of
      "waiting"   -> "waiting for your approval — open its pane"
      "busy"      -> "working"
      "shell"     -> "working"
      "idle"      -> "idle"
      "done"      -> "idle"
      "suspended" -> "suspended"
      "starting"  -> "starting"
      _           -> "not running"

    button :: Text -> m (Event t ())
    button label = do
      (b, _) <- elAttr' "button" ("class" =: "manager-btn") (text label)
      return (domEvent Click b)

-- | The input line and its Send button.  What happens to a message depends on
-- the manager's state when it is sent.
mdoInput
  :: forall t m . MonadWidget t m
  => (Text -> IO ()) -> IO () -> Dynamic t (Maybe AgentNode) -> m ()
mdoInput toast soon mgrD = do
  rec inp <- inputElement $ def
        & inputElementConfig_setValue .~ ("" <$ sentE)
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ( "class" =: "manager-line" <> "spellcheck" =: "false"
           <> "placeholder" =: "Tell the manager what you need…  (Enter to send)" )
      (b, _) <- elAttr' "button" ("class" =: "manager-btn") (text "Send")
      let goE = leftmost [keypress Enter inp, domEvent Click b]
          msgE = attach (current mgrD) (tag (current (_inputElement_value inp)) goE)
          sentE = ffor msgE (const ())
  performEvent_ $ ffor msgE $ \(mm, raw) -> liftIO . void . forkIO $
    let t = T.strip raw in
    if T.null t then return () else case mm of
      Nothing -> toast "There is no manager yet — start one first"
      Just n
        | anState n == "waiting" ->
            toast (displayName n <> " is waiting for an approval: answer it in its pane first")
        | not (anLive n) ->
            toast (displayName n <> " is not running — Resume it first")
        | otherwise -> do
            _ <- agentSend (anSession n) True t
            soon

managerCss :: Css
managerCss = do
  ".manager" ? do
    "height" -: "100%"
    "display" -: "flex"
    "flex-direction" -: "column"
    "font-size" -: "12px"
    "background" -: "var(--leksah-bg)"
    "color" -: "var(--leksah-fg)"
  ".manager .manager-head" ? do
    "flex" -: "0 0 auto"
    "display" -: "flex"
    "align-items" -: "center"
    "gap" -: "6px"
    "padding" -: "2px 6px"
    "border-bottom" -: "1px solid var(--leksah-border-faint)"
  ".manager .manager-head > div, .manager .manager-head" ? ("min-width" -: "0")
  ".manager .manager-face svg" ? do
    "width" -: "18px"
    "height" -: "18px"
    "display" -: "block"
  ".manager .manager-name" ? ("font-weight" -: "bold")
  ".manager .manager-tag" ? do
    "font-size" -: "10px"
    "padding" -: "0 4px"
    "border-radius" -: "3px"
    "background" -: "#a371f7"
    "color" -: "#fff"
  ".manager .manager-state" ? ("font-size" -: "11px")
  ".manager .manager-none" ? do
    "color" -: "var(--leksah-dim)"
    "flex" -: "1"
  ".manager .manager-spacer" ? ("flex" -: "1")
  ".manager .manager-btn" ? do
    "font-size" -: "11px"
    "padding" -: "1px 8px"
    "flex" -: "0 0 auto"
  ".manager .manager-chat" ? do
    "flex" -: "1 1 auto"
    "min-height" -: "0"
    "overflow" -: "auto"
    "padding" -: "4px 8px"
  ".manager .manager-empty" ? ("color" -: "var(--leksah-dim)")
  ".manager .manager-turn" ? do
    "margin" -: "0 0 6px 0"
    "display" -: "flex"
    "gap" -: "8px"
  ".manager .manager-who" ? do
    "flex" -: "0 0 auto"
    "width" -: "60px"
    "color" -: "var(--leksah-dim)"
    "text-align" -: "right"
  ".manager .manager-you .manager-who" ? ("color" -: "var(--leksah-selection)")
  ".manager .manager-text" ? do
    "white-space" -: "pre-wrap"
    "overflow-wrap" -: "anywhere"
    "min-width" -: "0"
  ".manager .manager-input" ? do
    "flex" -: "0 0 auto"
    "display" -: "flex"
    "gap" -: "6px"
    "padding" -: "4px 6px"
    "border-top" -: "1px solid var(--leksah-border-faint)"
  ".manager .manager-line" ? do
    "flex" -: "1 1 auto"
    "min-width" -: "0"
    "font" -: "inherit"
    "padding" -: "2px 6px"
    "color" -: "var(--leksah-fg)"
    "background" -: "var(--leksah-surface-alt)"
    "border" -: "1px solid var(--leksah-border-faint)"
    "border-radius" -: "3px"
    "outline" -: "none"
  ".manager .manager-line:focus" ? ("border-color" -: "var(--leksah-selection)")
