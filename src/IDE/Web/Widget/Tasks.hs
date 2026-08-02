{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
-- reflex-dom deprecates 'textInput'; leksah still uses it deliberately.
{-# OPTIONS_GHC -Wno-deprecations #-}
-- | The Claude task queue pane (plan Stage 4).  Queue a prompt against a
-- project directory; the scheduler ("IDE.Web.ClaudeQueue") starts each task —
-- when one of the concurrency slots frees — as a seeded Claude session in a
-- fresh git worktree, and marks it done when the session ends, ready for the
-- Review pane (diff → merge → archive).  This pane is just the queue's face:
-- an add form and the task cards with their actions.
module IDE.Web.Widget.Tasks
  ( tasksCss
  , tasksWidget
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void, unless)
import Control.Monad.IO.Class (liftIO)

import Data.Default (def)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Clay ((?), (-:), Css)

import Reflex
       (Event, holdDyn, getPostBuild, performEvent_, newTriggerEvent,
        ffor, leftmost, tag, current, never, constDyn, simpleList,
        tickLossyFromPostBuildTime)
import Reflex.Dom.Core
       (MonadWidget, divClass, elClass, elAttr', dynText, dyn_, text,
        domEvent, EventName(..), (=:), textInput, _textInput_value,
        TextInputConfig, _textInputConfig_initialValue,
        _textInputConfig_attributes)

import IDE.Web.Claude (activateMruClaude)
import IDE.Web.ClaudeQueue
       (QueueTask(..), queueSlots, queueList, queueAdd, queueDelete,
        queueStartNow, requestCompare)
import IDE.Web.Worktree (requestReview)

tasksWidget
  :: forall t m . MonadWidget t m
  => FilePath          -- ^ the dir the pane was opened from (seeds the form)
  -> m (Event t ())
tasksWidget seedDir = divClass "tasks" $ do
  -- Poll the queue (cheap MVar read) so scheduler progress shows on its own.
  pb <- getPostBuild
  tick <- tickLossyFromPostBuildTime 2
  (tasksE, fireTasks) <- newTriggerEvent
  let refresh = void . forkIO $ queueList >>= fireTasks
  performEvent_ $ liftIO refresh <$ leftmost [pb, () <$ tick]
  tasksD <- holdDyn [] tasksE

  -- Add form: project dir + prompt.
  divClass "tasks-add" $ do
    dirTi <- textInput $ (def :: TextInputConfig t)
      { _textInputConfig_initialValue = T.pack seedDir
      , _textInputConfig_attributes   = constDyn
          ("placeholder" =: "project directory" <> "class" =: "tasks-dir") }
    promptTi <- textInput $ (def :: TextInputConfig t)
      { _textInputConfig_attributes = constDyn
          ("placeholder" =: "task prompt — e.g. add property tests for the parser"
           <> "class" =: "tasks-prompt") }
    -- Compare-N-approaches: ×N runs the same prompt in N separate worktree
    -- sessions (tagged "approach k/N"), each reviewed independently.
    countTi <- textInput $ (def :: TextInputConfig t)
      { _textInputConfig_initialValue = "1"
      , _textInputConfig_attributes   = constDyn
          ("class" =: "tasks-count" <> "type" =: "number"
           <> "min" =: "1" <> "max" =: "8"
           <> "title" =: "approaches: run the same prompt in N worktrees to compare") }
    (addEl, _) <- elAttr' "button" ("class" =: "review-btn") $ text "Queue Task"
    let valsB = (,,) <$> current (_textInput_value dirTi)
                     <*> current (_textInput_value promptTi)
                     <*> current (_textInput_value countTi)
    performEvent_ $ ffor (tag valsB (domEvent Click addEl)) $ \(d, p, c) ->
      liftIO . void . forkIO $ do
        let d' = T.strip d; p' = T.strip p
            n  = maybe 1 (max 1 . min 8) (readMaybe (T.unpack (T.strip c)))
        if T.null d' || T.null p' then return () else do
          queueAdd (T.unpack d') p' n
          refresh
    return ()

  -- Slot usage.
  divClass "tasks-slots" . dynText $ ffor tasksD $ \ts ->
    let running = length [ () | t <- ts, qtStatus t == "running" ]
    in T.pack (show running) <> " of " <> T.pack (show queueSlots)
       <> " agent slots busy — queued tasks start as worktree sessions when one frees"

  -- Task cards (newest last), each with its status-appropriate actions.
  divClass "tasks-list" . void . simpleList tasksD $ \tD ->
    dyn_ (taskRow refresh <$> tD)
  return never

-- | One task card.
taskRow :: MonadWidget t m => IO () -> QueueTask -> m ()
taskRow refresh t = divClass "tasks-row" $ do
    elClass "span" ("tasks-status tasks-" <> qtStatus t) . text $
      case qtStatus t of
        "queued"  -> "◷ queued"
        "running" -> "● running"
        "done"    -> "✓ done"
        _         -> "✗ error"
    divClass "tasks-main" $ do
      elClass "span" "tasks-prompt-text" $ do
        unless (T.null (qtTag t)) $
          elClass "span" "tasks-tag" (text (qtTag t))
        text (qtPrompt t)
      elClass "span" "tasks-where" . text $
        T.pack (qtDir t)
          <> maybe "" (("  →  " <>) . T.pack) (qtWorktree t)
      unless (T.null (qtNote t)) $
        elClass "span" "tasks-note" (text (qtNote t))
    divClass "tasks-actions" $ do
      case qtStatus t of
        "queued"  -> act "Start Now" (queueStartNow (qtId t))
        "running" -> forMb (qtWorktree t) $ \wt ->
                       act "Show Session" (void (activateMruClaude wt))
        "done"    -> forMb (qtWorktree t) $ \wt ->
                       act "Review" (requestReview wt)
        _         -> return ()
      -- A compare-N card: the side-by-side view of all its group's approaches.
      unless (T.null (qtTag t)) $
        act "Compare" (requestCompare (qtDir t, qtPrompt t))
      act "Delete" (queueDelete (qtId t))
  where
    forMb = flip (maybe (return ()))
    act name io = do
      (e, _) <- elAttr' "button" ("class" =: "review-btn") $ text name
      performEvent_ $ ffor (domEvent Click e) $ \_ ->
        liftIO (io >> refresh)

tasksCss :: Css
tasksCss = do
    ".tasks" ? do
        "display"        -: "flex"
        "flex-direction" -: "column"
        "height"         -: "100%"
        "overflow"       -: "hidden"
    ".tasks-add" ? do
        "display"     -: "flex"
        "gap"         -: "6px"
        "padding"     -: "6px 8px"
        "align-items" -: "center"
        "border-bottom" -: "1px solid var(--leksah-border, #333)"
    ".tasks-add .tasks-dir"    ? ("flex" -: "0 0 34%")
    ".tasks-add .tasks-prompt" ? do
        "flex"      -: "1 1 auto"
        "min-width" -: "0"
    -- The compare-N "approaches" spinner: just wide enough for two digits.
    ".tasks-add .tasks-count"  ? ("flex" -: "0 0 3.5em")
    ".tasks-slots" ? do
        "padding"   -: "4px 8px"
        "color"     -: "var(--leksah-fg-dim)"
        "font-size" -: "12px"
    ".tasks-list" ? do
        "flex"     -: "1 1 0"
        "overflow" -: "auto"
    ".tasks-row" ? do
        "display"     -: "flex"
        "gap"         -: "10px"
        "align-items" -: "center"
        "padding"     -: "6px 8px"
        "border-bottom" -: "1px solid var(--leksah-border, #333)"
    ".tasks-status" ? do
        "flex"        -: "0 0 6.5em"
        "white-space" -: "nowrap"
    ".tasks-queued"  ? ("color" -: "var(--leksah-fg-dim)")
    ".tasks-running" ? ("color" -: "#d29922")
    ".tasks-done"    ? ("color" -: "#3fb950")
    ".tasks-error"   ? ("color" -: "#f85149")
    ".tasks-main" ? do
        "flex"           -: "1 1 auto"
        "display"        -: "flex"
        "flex-direction" -: "column"
        "min-width"      -: "0"
    ".tasks-prompt-text" ? do
        "overflow"      -: "hidden"
        "text-overflow" -: "ellipsis"
        "white-space"   -: "nowrap"
    ".tasks-where" ? do
        "color"         -: "var(--leksah-fg-dim)"
        "font-size"     -: "11px"
        "overflow"      -: "hidden"
        "text-overflow" -: "ellipsis"
        "white-space"   -: "nowrap"
    ".tasks-note" ? do
        "color"     -: "#f85149"
        "font-size" -: "11px"
    -- The permanent "approach k/N" chip in front of a compare-N card's prompt.
    ".tasks-tag" ? do
        "color"         -: "var(--leksah-accent-text)"
        "font-size"     -: "11px"
        "margin-right"  -: "8px"
        "padding"       -: "0 5px"
        "border"        -: "1px solid var(--leksah-border-control)"
        "border-radius" -: "6px"
        "white-space"   -: "nowrap"
    ".tasks-actions" ? do
        "display" -: "flex"
        "gap"     -: "6px"
        "flex"    -: "0 0 auto"
