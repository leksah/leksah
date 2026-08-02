{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  IDE.Web.ClaudeQueue
--
-- The Claude task queue (plan Stage 4, kanban-lite): queued prompts per
-- project, each started — when one of the 'queueSlots' concurrency slots
-- frees up — as a Claude session in its own fresh git worktree (the Stage-2
-- lifecycle: worktree + branch + workspace project; when the session ends
-- the task is ready for the Review pane's diff → merge → archive flow).
--
-- The queue lives in @~\/.leksah\/claude-queue.json@ behind a process MVar
-- (the "IDE.Web.Widget.Browser" sidecar pattern), so it survives restarts;
-- the scheduler is one background loop ('startQueueScheduler', armed from
-- "IDE.Web.Main" once per boot — a ghci reload's thread ratchet kills the
-- old one and the fresh @:main@ re-arms it).
--
-- Also hosts the Stage-4 request bridges: open the Tasks pane, open the
-- plan-review pane for a session.
-----------------------------------------------------------------------------
module IDE.Web.ClaudeQueue
  ( QueueTask(..)
  , queueSlots
  , queueList
  , queueAdd
  , queueDelete
  , queueStartNow
  , armQueueScheduler
    -- * Request bridges
  , requestTaskQueue
  , nextTaskQueueRequest
  , requestPlanReview
  , nextPlanReviewRequest
  , requestCompare
  , nextCompareRequest
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (catch, SomeException)
import Control.Monad (void, forever, forM_, when)
import Data.Aeson
       (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject,
        encode, decode')
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import Data.List (find)
import qualified Data.Map as M (Map, empty, toList, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

import IDE.Core.State (reflectIDE)
import IDE.Utils.FileUtils (getConfigFilePathForSave)
import IDE.Web.Claude
       (ClaudeCmd(..), runClaudeCmd, claudeRunning, claudeLiveBySession,
        ClaudeLive(..), mruClaudePane, claudeTranscriptPath)
import IDE.Web.IDERefStore (getGlobalIDERef)
import IDE.Web.Worktree (newClaudeWorktreeUnique)
import IDE.Workspaces (projectOpenPath, workspaceTryQuiet)

-- | How many queue-started sessions may run at once.  A small fixed number
-- for now (agents saturate a machine quickly); a preference if demand shows.
queueSlots :: Int
queueSlots = 2

-- | One queued (or started, or finished) agent task.
data QueueTask = QueueTask
  { qtId       :: Int
  , qtDir      :: FilePath        -- ^ the project/repo the task belongs to
  , qtPrompt   :: Text            -- ^ the seeded prompt
  , qtStatus   :: Text            -- ^ queued | running | done | error
  , qtWorktree :: Maybe FilePath  -- ^ set once started (the session's checkout)
  , qtNote     :: Text            -- ^ error text / short status detail
  , qtTag      :: Text            -- ^ permanent label, e.g. @approach 2\/3@
                                  --   (compare-N; "" for an ordinary task)
  } deriving (Eq, Show)

instance ToJSON QueueTask where
  toJSON t = object
    [ "id" .= qtId t, "dir" .= qtDir t, "prompt" .= qtPrompt t
    , "status" .= qtStatus t, "worktree" .= qtWorktree t, "note" .= qtNote t
    , "tag" .= qtTag t ]

instance FromJSON QueueTask where
  parseJSON = withObject "QueueTask" $ \o -> QueueTask
    <$> o .: "id" <*> o .: "dir" <*> o .: "prompt" <*> o .: "status"
    <*> o .:? "worktree" <*> (fromMaybe "" <$> o .:? "note")
    <*> (fromMaybe "" <$> o .:? "tag")

data Queue = Queue { qNext :: Int, qTasks :: [QueueTask] }

instance ToJSON Queue where
  toJSON q = object [ "next" .= qNext q, "tasks" .= qTasks q ]
instance FromJSON Queue where
  parseJSON = withObject "Queue" $ \o -> Queue <$> o .: "next" <*> o .: "tasks"

{-# NOINLINE queueVar #-}
queueVar :: MVar (Maybe Queue)
queueVar = unsafePerformIO (newMVar Nothing)

queuePath :: IO FilePath
queuePath = getConfigFilePathForSave "claude-queue.json"

loadQueue :: IO Queue
loadQueue = (`catch` \(_ :: SomeException) -> return (Queue 1 [])) $ do
  p <- queuePath
  fromMaybe (Queue 1 []) . decode' <$> LBS.readFile p

saveQueue :: Queue -> IO ()
saveQueue q = (`catch` \(_ :: SomeException) -> return ()) $ do
  p <- queuePath
  LBS.writeFile p (encode q)

-- | Run an update against the (lazily loaded) queue, persisting the result.
withQueue :: (Queue -> (Queue, a)) -> IO a
withQueue f = modifyMVar queueVar $ \m -> do
  q0 <- maybe loadQueue return m
  let (q, a) = f q0
  saveQueue q
  return (Just q, a)

-- | The tasks, newest last (the display/scheduling order).
queueList :: IO [QueueTask]
queueList = withQueue (\q -> (q, qTasks q))

-- | Queue a prompt for a project directory — @n@ times.  @n > 1@ is
-- compare-N-approaches (Crystal-style): the same prompt runs in N separate
-- worktree sessions (the scheduler's unique-slug creation gives them
-- @slug@, @slug-2@, …), each card tagged @approach k\/N@, each reviewed —
-- and merged or archived — independently.
queueAdd :: FilePath -> Text -> Int -> IO ()
queueAdd dir prompt n0 = withQueue $ \q ->
  let n = max 1 n0
      tag k = if n == 1 then ""
              else "approach " <> T.pack (show k) <> "/" <> T.pack (show n)
  in ( q { qNext = qNext q + n
         , qTasks = qTasks q
             <> [ QueueTask (qNext q + k - 1) dir (T.strip prompt) "queued"
                            Nothing "" (tag k)
                | k <- [1 .. n] ] }
     , () )

-- | Drop a task from the list (any status — a running one keeps its session
-- and worktree; this only forgets the card).
queueDelete :: Int -> IO ()
queueDelete i = withQueue $ \q ->
  (q { qTasks = filter ((/= i) . qtId) (qTasks q) }, ())

-- | Start a queued task immediately, ignoring the slot limit.
queueStartNow :: Int -> IO ()
queueStartNow i = void . forkIO $ do
  mt <- find ((== i) . qtId) <$> queueList
  forM_ mt $ \t -> when (qtStatus t == "queued") (startTask t)

-- | Create the worktree, add it to the workspace, start the seeded session,
-- and mark the task running (or error, with the reason on the card).
startTask :: QueueTask -> IO ()
startTask t = do
  -- Claim the task first so two scheduler passes can't double-start it.
  claimed <- withQueue $ \q ->
    case find ((== qtId t) . qtId) (qTasks q) of
      Just cur | qtStatus cur == "queued" ->
        (patch q (qtId t) (\x -> x { qtStatus = "running", qtNote = "starting…" }), True)
      _ -> (q, False)
  when claimed $
    -- Unique-slug creation: N queued copies of one prompt (compare-N) get
    -- slug, slug-2, … instead of the second one failing on a taken branch.
    newClaudeWorktreeUnique (qtDir t) (qtPrompt t) >>= \case
      Left err -> void . withQueue $ \q ->
        (patch q (qtId t) (\x -> x { qtStatus = "error", qtNote = err }), ())
      Right (wt, _branch) -> do
        getGlobalIDERef >>= mapM_
          (reflectIDE (workspaceTryQuiet (projectOpenPath wt)))
        runClaudeCmd (ClaudePrompt wt (qtPrompt t))
        void . withQueue $ \q ->
          (patch q (qtId t) (\x ->
             x { qtWorktree = Just wt, qtNote = "" }), ())

patch :: Queue -> Int -> (QueueTask -> QueueTask) -> Queue
patch q i f = q { qTasks = map (\x -> if qtId x == i then f x else x) (qTasks q) }

{-# NOINLINE schedulerArmed #-}
schedulerArmed :: IORef Bool
schedulerArmed = unsafePerformIO (newIORef False)

-- | Start the scheduler once per process (each OS window's build calls this;
-- only the first arms it — and a ghci @:reload@ wipes the CAF, so the fresh
-- @:main@ re-arms after the thread ratchet killed the old loop).
armQueueScheduler :: IO ()
armQueueScheduler = do
  first <- atomicModifyIORef' schedulerArmed (\b -> (True, not b))
  when first (void startQueueScheduler)

-- | The scheduler: every 5s, retire running tasks whose session is gone
-- (→ done, ready for review) and start queued ones while slots are free.
startQueueScheduler :: IO ()
startQueueScheduler = void . forkIO . forever $ do
  (`catch` \(_ :: SomeException) -> return ()) $ do
    ts <- queueList
    -- running → done when the worktree's claude session has ended.
    forM_ [ t | t <- ts, qtStatus t == "running", Just _ <- [qtWorktree t] ] $ \t ->
      forM_ (qtWorktree t) $ \wt -> do
        live <- claudeRunning wt
        when (not live) . void . withQueue $ \q ->
          (patch q (qtId t) (\x ->
             x { qtStatus = "done", qtNote = "session ended — review the worktree" }), ())
    ts' <- queueList
    let running = length [ () | t <- ts', qtStatus t == "running" ]
        free    = queueSlots - running
    forM_ (take (max 0 free) [ t | t <- ts', qtStatus t == "queued" ]) startTask
    surfacePlanPrompts
  threadDelay 5000000

-- | The last status seen per live session, so a plan prompt fires the pane
-- open exactly once per waiting episode (edge-triggered).
{-# NOINLINE planPromptSeen #-}
planPromptSeen :: IORef (M.Map Text Text)
planPromptSeen = unsafePerformIO (newIORef M.empty)

-- | When a session flips to @waiting@ AND its pane is showing the
-- ExitPlanMode approval prompt (the CLI's @waitingFor@ is just "permission
-- prompt" — the pane text is what tells a plan approval from an ordinary
-- edit permission), open its plan-review pane so the approval happens on the
-- rendered HTML plan instead of the terminal text.
surfacePlanPrompts :: IO ()
surfacePlanPrompts = do
  live <- claudeLiveBySession
  prev <- readIORef planPromptSeen
  let statusOf = fromMaybe "" . clStatus
  forM_ (M.toList live) $ \(sid, l) ->
    when (statusOf l == "waiting" && M.lookup sid prev /= Just "waiting") $
      mruClaudePane (clDir l) >>= mapM_ (\pane -> do
        out <- paneText pane
        when ("is ready to execute" `T.isInfixOf` out) $ do
          tr <- claudeTranscriptPath (clDir l) sid
          ex <- doesFileExist tr
          when ex $ requestPlanReview (clDir l, T.pack tr))
  writeIORef planPromptSeen (fmap statusOf live)

-- | The visible content of a tmux pane (empty on any failure).
paneText :: Text -> IO Text
paneText pane = do
  r <- (`catch` \(_ :: SomeException) -> return (ExitFailure 1, "", "")) $
    readProcessWithExitCode "tmux"
      ["-L", "leksah", "capture-pane", "-p", "-t", T.unpack pane] ""
  return $ case r of
    (ExitSuccess, out, _) -> T.pack out
    _                     -> ""

--------------------------------------------------------------------------------
-- Request bridges (tree menus → Main)

{-# NOINLINE taskQueueChan #-}
taskQueueChan :: Chan FilePath
taskQueueChan = unsafePerformIO newChan

-- | Open the Tasks pane, seeding its add-form with @dir@.
requestTaskQueue :: FilePath -> IO ()
requestTaskQueue = writeChan taskQueueChan

nextTaskQueueRequest :: IO FilePath
nextTaskQueueRequest = readChan taskQueueChan

{-# NOINLINE planReviewChan #-}
planReviewChan :: Chan (FilePath, Text)
planReviewChan = unsafePerformIO newChan

-- | Open the plan-review pane for a session: @(dir, transcript path)@.
requestPlanReview :: (FilePath, Text) -> IO ()
requestPlanReview = writeChan planReviewChan

nextPlanReviewRequest :: IO (FilePath, Text)
nextPlanReviewRequest = readChan planReviewChan

{-# NOINLINE compareChan #-}
compareChan :: Chan (FilePath, Text)
compareChan = unsafePerformIO newChan

-- | Open the side-by-side compare pane for a compare-N group:
-- @(project dir, the shared prompt)@.
requestCompare :: (FilePath, Text) -> IO ()
requestCompare = writeChan compareChan

nextCompareRequest :: IO (FilePath, Text)
nextCompareRequest = readChan compareChan
