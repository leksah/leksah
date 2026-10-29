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
  , sessionEvents
  , eventsFor
  , recordToolStart
  , recordToolEnd
  , recordPermission
  , recordFailure
  , recordTurnEnd
  , recordEdit
  , recordCwd
  , editedRecently
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.IO.Unsafe (unsafePerformIO)

-- | One session's live facts.  Times are POSIX seconds.
data SessionEvents = SessionEvents
  { seTool       :: Maybe (Text, Double)  -- ^ the tool call running now
  , sePermission :: Maybe (Text, Double)  -- ^ the approval it is waiting for
  , seError      :: Maybe (Text, Double)  -- ^ the API error its turn ended on
  , seEdits      :: [(FilePath, Double)]  -- ^ files it edited, newest first
  , seCwd        :: Maybe FilePath        -- ^ where its shell is now
  } deriving (Eq, Show)

none :: SessionEvents
none = SessionEvents Nothing Nothing Nothing [] Nothing

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
-- going again: no error stands, and any approval was answered.
recordToolStart :: Text -> Text -> IO ()
recordToolStart sid what = update sid $ \now e ->
  e { seTool = Just (what, now), seError = Nothing, sePermission = Nothing }

-- | A tool call finished, one way or the other (PostToolUse /
-- PostToolUseFailure).
recordToolEnd :: Text -> IO ()
recordToolEnd sid = update sid $ \_ e -> e { seTool = Nothing, sePermission = Nothing }

-- | Waiting for the user to approve this (PermissionRequest).
recordPermission :: Text -> Text -> IO ()
recordPermission sid what = update sid $ \now e -> e { sePermission = Just (what, now) }

-- | The turn ended on an API error (StopFailure).
recordFailure :: Text -> Text -> IO ()
recordFailure sid err = update sid $ \now e ->
  e { seError = Just (err, now), seTool = Nothing, sePermission = Nothing }

-- | The turn ended normally (Stop): nothing is running, nothing is waiting.
recordTurnEnd :: Text -> IO ()
recordTurnEnd sid = update sid $ \_ e ->
  e { seTool = Nothing, sePermission = Nothing, seError = Nothing }

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
