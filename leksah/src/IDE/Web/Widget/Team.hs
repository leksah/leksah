{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- | The Team pane: the agents as a "who we are" page — a grid of bio cards
-- ("IDE.Web.Widget.AgentCard", all shown in full), the manager first, then
-- everyone else grouped by the project they work in, then those on leave
-- (suspended) and alumni (exited).  The Agents pane shows the same cards as a
-- single column; this is the page you read to get to know the team.
module IDE.Web.Widget.Team
  ( teamCss
  , teamWidget
  ) where

import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import Clay (Css, (?), (-:))

import Reflex
       (Event, constDyn, ffor, getPostBuild, holdDyn,
        holdUniqDyn, leftmost, listViewWithKey, never, newTriggerEvent,
        tickLossyFromPostBuildTime)
import Reflex.Dom.Core
       (blank, divClass, dyn_, dynText, el, elClass, elClass', text, _element_raw)
import Language.Javascript.JSaddle (liftJSM, jsg, js1)

import qualified Data.Set as S
import System.FilePath ((</>), takeDirectory, takeFileName, dropTrailingPathSeparator)

import IDE.Web.AgentInfo (AgentNode(..), agentForest)
import IDE.Web.Widget.AgentCard (agentCard)
import IDE.Web.Widget.Agents (agentCardActions)
import IDE.Web.Frame (MonadWidget, performEvent_)

teamWidget :: forall t m . MonadWidget t m => m (Event t ())
teamWidget = do
  (root, _) <- elClass' "div" "team" $ do
    pb   <- getPostBuild
    tick <- tickLossyFromPostBuildTime 3
    (forestE, fireForest) <- newTriggerEvent
    let refresh = void . forkIO $ agentForest >>= fireForest
    performEvent_ $ liftIO refresh <$ leftmost [() <$ pb, () <$ tick]
    forestD <- holdUniqDyn =<< holdDyn [] forestE
    (toastE, toast) <- newTriggerEvent
    performEvent_ $ ffor toastE $ \msg -> liftJSM . void $
      jsg ("window" :: Text) ^. js1 ("__leksahBridgeToast" :: Text) (msg :: Text)
    let acts = agentCardActions toast refresh

    divClass "team-intro" $ do
      el "h1" (text "Team")
      el "p" $ text
        "The agents working in this IDE, in their own words. Each keeps its \
        \bio current when asked (Refresh); the manager does the asking."
    dyn_ $ ffor (null <$> forestD) $ \case
      True -> divClass "team-empty" $ text
        "Nobody here yet. Start an agent from the Agents pane, or a manager to \
        \run the team."
      False -> blank
    -- One section per group, keyed so it sorts in display order, each a keyed
    -- grid of cards (so a poll leaves every card as it was).
    void . listViewWithKey (sections <$> forestD) $ \_ secD -> do
      divClass "team-section" $ do
        elClass "h2" "team-section-title" . dynText $ fst <$> secD
        void . divClass "team-grid" . listViewWithKey (snd <$> secD) $ \_ nD -> do
          agentCard acts (constDyn True) True nD
          return (never :: Event t ())
        return (never :: Event t ())
    return ()
  pb <- getPostBuild
  performEvent_ $ ffor pb $ \_ -> liftJSM . void $
    jsg ("LeksahAgentLinks" :: Text) ^. js1 ("arm" :: Text) (_element_raw root)
  return never

-- | The page's sections, keyed so they sort in display order: the manager,
-- then one per project directory (alphabetical), then on leave, then alumni.
sections :: [AgentNode] -> Map Text (Text, Map Text AgentNode)
sections forest = M.filter (not . M.null . snd) $ M.fromListWith merge
  [ (key n, (title n, M.singleton (sortKey n) n)) | n <- everyone ]
  where
    everyone = concatMap (\n -> n : flatten (anChildren n)) forest
    flatten = concatMap (\n -> n : flatten (anChildren n))
    merge (t, a) (_, b) = (t, M.union a b)
    key n
      | anManager n                  = "0"
      | anState n == "suspended"     = "8"
      | not (anLive n)               = "9"
      -- (a tab before the path, so "haskell.nix" sorts before "haskell.nix-2")
      | otherwise                    = "1:" <> T.toLower (project n) <> "\t" <> T.pack (anDir n)
    title n
      | anManager n                  = "Manager"
      | anState n == "suspended"     = "On leave"
      | not (anLive n)               = "Alumni"
      | otherwise                    = project n
    -- A project is shown by its folder name — and, when two different
    -- folders share that name (~/infra and ~/work/infra), with its parent too.
    project n =
      let d = dropTrailingPathSeparator (anDir n)
          base = takeFileName d
          clash = length (nubOrd [ dropTrailingPathSeparator (anDir m)
                                 | m <- everyone, anLive m, not (anManager m)
                                 , takeFileName (dropTrailingPathSeparator (anDir m)) == base ]) > 1
      in T.pack (if clash then takeFileName (takeDirectory d) </> base else base)
    nubOrd = S.toList . S.fromList
    -- Within a section: alphabetical by name (then session, for the unnamed).
    sortKey n = T.toLower (anName n) <> ":" <> anSession n

-- | The page: a readable column of sections, each a responsive grid of cards.
teamCss :: Css
teamCss = do
  ".team" ? do
    "height" -: "100%"
    "overflow" -: "auto"
    "padding" -: "18px 22px 40px 22px"
    "box-sizing" -: "border-box"
    "background" -: "var(--leksah-bg)"
    "color" -: "var(--leksah-fg)"
  ".team .team-intro h1" ? do
    "font-size" -: "22px"
    "margin" -: "0 0 4px 0"
  ".team .team-intro p" ? do
    "margin" -: "0 0 14px 0"
    "color" -: "var(--leksah-dim)"
    "font-size" -: "13px"
  ".team .team-empty" ? ("color" -: "var(--leksah-dim)")
  ".team .team-section" ? ("margin-bottom" -: "18px")
  ".team .team-section-title" ? do
    "font-size" -: "14px"
    "font-weight" -: "600"
    "margin" -: "0 0 8px 0"
    "padding-bottom" -: "3px"
    "border-bottom" -: "1px solid var(--leksah-border-faint)"
  ".team .team-grid" ? do
    "display" -: "grid"
    "grid-template-columns" -: "repeat(auto-fill, minmax(300px, 1fr))"
    "gap" -: "12px"
    "align-items" -: "start"
  ".team .agent-card" ? ("padding" -: "10px 12px")
