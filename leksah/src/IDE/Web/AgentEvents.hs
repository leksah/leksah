{-# LANGUAGE OverloadedStrings #-}
-- | What Claude Code's hooks tell leksah about each session, as it happens
-- (see 'IDE.Web.Agent.agentHook'): the tool it is running right now, the
-- approval it is waiting for, the API error its last turn ended on, the files
-- it has edited, and the directory it is working in.
--
-- In memory only: these describe the present (a restart forgets them, and the
-- next hook brings them back).  The Agents pane, `agent list` and `agent
-- status` read them in preference to what they would otherwise piece together
-- from the transcript and the CLI's session file.
module IDE.Web.AgentEvents
  ( SessionEvents(..)
  , seTool
  , seSubagentsAt
  , seCompactingAt
  , sePromptPending
  , perToolHookEvents
  , sessionEvents
  , eventsFor
  , recordToolStart
  , recordToolEnd
  , recordPermission
  , recordFailure
  , recordTurnEnd
  , recordPromptSubmit
  , recordSubagentStart
  , recordSubagentStop
  , recordCompacting
  , recordSessionEnd
  , recordEdit
  , recordCwd
  , editedRecently
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (nub)
import Data.Maybe (isNothing, listToMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.IO.Unsafe (unsafePerformIO)

-- | One session's live facts.  Times are POSIX seconds.
data SessionEvents = SessionEvents
  { seRunning    :: [(Maybe Text, (Text, Double))]
                                          -- ^ the tool calls running now, newest
                                          --   first, by tool_use_id (parallel
                                          --   calls overlap)
  , sePermission :: Maybe (Text, Double)  -- ^ the approval it is waiting for
  , seError      :: Maybe (Text, Double)  -- ^ the API error its turn ended on
  , seEdits      :: [(FilePath, Double)]  -- ^ files it edited, newest first
  , seCwd        :: Maybe FilePath        -- ^ where its shell is now
  , seEnded      :: [Text]                -- ^ tool_use_ids that have finished
                                          --   (the last few): see 'recordToolStart'
  , seTurnEnd    :: Maybe Double          -- ^ when its last turn ended (Stop /
                                          --   StopFailure)
  , seSubagents  :: [(Text, (Text, Double))]
                                          -- ^ the subagents running now, oldest
                                          --   first: agent_id → (agent type,
                                          --   when it started)
  , seCompacting :: Maybe Double          -- ^ since when it is compacting its
                                          --   conversation (PreCompact)
  , seSubEnded   :: [Text]                -- ^ agent_ids that have finished (the
                                          --   last few): see 'recordSubagentStart'
  , sePromptAt   :: Maybe Double          -- ^ when the last prompt was submitted
  } deriving (Eq, Show)

none :: SessionEvents
none = SessionEvents [] Nothing Nothing [] Nothing [] Nothing [] Nothing [] Nothing

-- | The tool call running now: the newest of those still running.
seTool :: SessionEvents -> Maybe (Text, Double)
seTool = fmap snd . listToMaybe . seRunning

-- | The subagents running at @now@ (dropping any whose stop never came).
seSubagentsAt :: Double -> SessionEvents -> [(Text, (Text, Double))]
seSubagentsAt now e = [ x | x@(_, (_, t)) <- seSubagents e, now - t < subagentMaxAge ]

-- | Is it compacting at @now@ (a compaction whose end never came is over)?
seCompactingAt :: Double -> SessionEvents -> Bool
seCompactingAt now e = maybe False (\t -> now - t < compactMaxAge) (seCompacting e)

-- | A prompt submitted within the last @window@ seconds whose turn has not
-- ended: the session is working, whatever its session file says yet.
sePromptPending :: Double -> Double -> SessionEvents -> Bool
sePromptPending window now e = case sePromptAt e of
  Just p -> now - p < window && maybe True (< p) (seTurnEnd e)
  Nothing -> False

-- | The hook events that come once per tool call — several times a turn.
-- They are registered async (they must never hold a tool call up) and
-- handled on the cheap path ("IDE.Web.Agent".'agentHook').  One list, so
-- adding a per-tool event can't quietly put it on the slow path.
perToolHookEvents :: [Text]
perToolHookEvents = ["PreToolUse", "PostToolUse", "PostToolUseFailure"]

-- | How long after a turn ends a tool START is taken to be a late report
-- from that turn rather than the next one's first call: the next turn needs a
-- new prompt and a model response first, which takes longer than this.
lateStartGrace :: Double
lateStartGrace = 1.5

-- | The least time from a prompt to its turn's first tool call (see
-- 'recordToolStart').
firstCallGap :: Double
firstCallGap = 0.7

-- | How long a subagent with no SubagentStop is believed to be running (it
-- was interrupted, or its stop never arrived), and how long a compaction with
-- no PostCompact is (it failed).
subagentMaxAge, compactMaxAge :: Double
subagentMaxAge = 2 * 3600
compactMaxAge  = 600

{-# NOINLINE eventsRef #-}
eventsRef :: IORef (Map Text SessionEvents)
eventsRef = unsafePerformIO (newIORef M.empty)

sessionEvents :: IO (Map Text SessionEvents)
sessionEvents = readIORef eventsRef

eventsFor :: Text -> IO SessionEvents
eventsFor sid = M.findWithDefault none sid <$> readIORef eventsRef

update :: Text -> (Double -> SessionEvents -> SessionEvents) -> IO ()
update sid f = do
  now <- realToFrac <$> getPOSIXTime
  atomicModifyIORef' eventsRef $ \m ->
    (M.insert sid (f now (M.findWithDefault none sid m)) m, ())

-- | A tool call starting (PreToolUse).  A new call also means the turn is
-- going again: no error stands.
--
-- The hooks are ASYNC (they never hold a tool call up), so they can arrive in
-- any order.  A start is ignored when it is a late report: its @tool_use_id@
-- has already finished (a quick tool's PostToolUse overtook it), or the turn
-- ended moments ago (a call that got no PostToolUse at all — interrupted,
-- denied).  Otherwise the row would say "running …" until the next event.
-- It leaves the approval alone: PermissionRequest for this very call may have
-- overtaken it.
--
-- "The turn ended moments ago" holds even when a prompt has been submitted
-- since, as long as that prompt is newer still than 'firstCallGap': its own
-- first tool call can't come that soon (the model has to answer first), so a
-- start that soon is the old turn's.  A tool call also means any compaction is
-- over.
recordToolStart :: Text -> Maybe Text -> Text -> IO ()
recordToolStart sid uid what = update sid $ \now e ->
  let finished = maybe False (`elem` seEnded e) uid
      newTurn = case (seTurnEnd e, sePromptAt e) of
        (Just t, Just p) -> p >= t && now - p >= firstCallGap
        _                -> True
      afterTurn = maybe False (\t -> now - t < lateStartGrace) (seTurnEnd e) && not newTurn
  in if finished || afterTurn then e
     else e { seRunning = (uid, (what, now))
                          : filter (\(u, _) -> isNothing u || u /= uid) (seRunning e)
            , seError = Nothing, seCompacting = Nothing }

-- | A tool call finished, one way or the other (PostToolUse /
-- PostToolUseFailure); @what@ is its approval text ('recordPermission''s
-- key).  Only that call stops running — with parallel calls the others carry
-- on — and the approval clears only if it was for this call.  A report with
-- no id (older Claude Code) clears every running call, as before.
recordToolEnd :: Text -> Maybe Text -> Text -> IO ()
recordToolEnd sid uid what = update sid $ \_ e ->
  e { seRunning = case uid of
        Just u  -> filter ((/= Just u) . fst) (seRunning e)
        Nothing -> []
    , sePermission = if (fst <$> sePermission e) == Just what then Nothing
                     else sePermission e
    , seEnded = take 32 (maybe id (:) uid (seEnded e)) }

-- | Waiting for the user to approve this (PermissionRequest).
recordPermission :: Text -> Text -> IO ()
recordPermission sid what = update sid $ \now e -> e { sePermission = Just (what, now) }

-- | The turn ended on an API error (StopFailure).
recordFailure :: Text -> Text -> IO ()
recordFailure sid err = update sid $ \now e ->
  e { seError = Just (err, now), seRunning = [], sePermission = Nothing
    , seTurnEnd = Just now, seCompacting = Nothing }

-- | The turn ended normally (Stop): nothing is running, nothing is waiting.
recordTurnEnd :: Text -> IO ()
recordTurnEnd sid = update sid $ \now e ->
  e { seRunning = [], sePermission = Nothing, seError = Nothing, seTurnEnd = Just now
    , seCompacting = Nothing }

-- | The user (or `agent send --submit`) submitted a prompt (UserPromptSubmit):
-- a new turn starts — no error stands.  When (see 'recordToolStart', and the
-- status poll, which counts a session as working from this moment).
recordPromptSubmit :: Text -> IO ()
recordPromptSubmit sid = update sid $ \now e -> e { seError = Nothing, sePromptAt = Just now }

-- | A subagent started (SubagentStart).  Called again for one that is
-- resumed, which keeps its place.  The hooks are async, so a quick
-- subagent's SubagentStop can arrive first: a start for one that has already
-- finished is ignored.  The newest 20 are kept, and any older than
-- 'subagentMaxAge' (its stop never came) are dropped.
recordSubagentStart :: Text -> Text -> Text -> IO ()
recordSubagentStart sid aid kind = update sid $ \now e ->
  let current = [ x | x@(_, (_, t)) <- seSubagents e, now - t < subagentMaxAge ]
  in if aid `elem` seSubEnded e || aid `elem` map fst current then e { seSubagents = current }
     else e { seSubagents = lastN 20 (current <> [(aid, (kind, now))]) }
  where lastN n xs = drop (length xs - n) xs

-- | A subagent finished (SubagentStop).
recordSubagentStop :: Text -> Text -> IO ()
recordSubagentStop sid aid = update sid $ \_ e ->
  e { seSubagents = filter ((/= aid) . fst) (seSubagents e)
    , seSubEnded = take 32 (aid : filter (/= aid) (seSubEnded e)) }

-- | Compaction started (True, PreCompact) or finished (False, PostCompact or
-- the SessionStart that follows it).
recordCompacting :: Text -> Bool -> IO ()
recordCompacting sid on = update sid $ \now e ->
  e { seCompacting = if on then Just now else Nothing }

-- | The session ended (SessionEnd): nothing of it is running any more.
recordSessionEnd :: Text -> IO ()
recordSessionEnd sid = update sid $ \_ e ->
  e { seRunning = [], sePermission = Nothing, seSubagents = [], seCompacting = Nothing }

-- | It edited these files (kept to the 100 most recent).
recordEdit :: Text -> [FilePath] -> IO ()
recordEdit sid fs = update sid $ \now e ->
  e { seEdits = take 100 ([ (f, now) | f <- nub fs ]
                           <> filter ((`notElem` fs) . fst) (seEdits e)) }

-- | Its shell is now in @dir@ (CwdChanged).
recordCwd :: Text -> FilePath -> IO ()
recordCwd sid dir = update sid $ \_ e -> e { seCwd = Just dir }

-- | The OTHER sessions that edited @file@ within the last @window@ seconds —
-- who to coordinate with before two agents' edits collide.
editedRecently :: Double -> Text -> FilePath -> IO [Text]
editedRecently window sid file = do
  now <- realToFrac <$> getPOSIXTime
  m <- readIORef eventsRef
  return [ s | (s, e) <- M.toList m, s /= sid
             , any (\(f, t) -> f == file && now - t < window) (seEdits e) ]
