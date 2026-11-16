{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | What the Claude Code sessions running right now are doing, in the one shape
-- every status surface needs it.
--
-- Two consumers, ONE poll: the macOS menu-bar status item (pushed, see
-- 'registerClaudeStatusPush' — it lives outside any reflex network, in
-- @IDE.Web.MacMenu@) and the in-page traffic light (pulled per window with
-- 'claudeStatusNow' from the reflex tick, so cross-window fan-out needs no JS
-- broadcast — see @statusLightJs@).  Both show the same states, so they can't
-- disagree.
--
-- POLLED, not pushed by the CLI: Claude Code writes
-- @~\/.claude\/sessions\/\<pid\>.json@ behind leksah's back, so there is nothing
-- to hook.  Same 3s cadence as the workspace tree's badges, and just as cheap —
-- a directory listing plus a few hundred bytes per live session, and not even a
-- @ps@ when nothing is running.
module IDE.Web.ClaudeStatus
  ( ClaudeStatus(..)
  , ClaudeStatusRow(..)
  , emptyClaudeStatus
  , startClaudeStatusPoll
  , claudeStatusNow
  , registerClaudeStatusPush
  , claudeStatusTooltip
  , claudeStatusRowsJson
  , claudeStatusGlyph
  , claudeRowLabel
  , pokeClaudeStatus
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import System.Timeout (timeout)
import Control.Exception (catch, SomeException)
import Control.Monad (foldM, void, when)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.IORef
       (IORef, newIORef, readIORef, atomicModifyIORef', atomicWriteIORef)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Web.AgentFace (agentFaceDataUri)
import IDE.Web.AgentNames (agentNames, touchAgentNames)
import IDE.Web.AgentEvents (sePromptPending, sessionEvents)
import Data.Time.Clock.POSIX (getPOSIXTime)
import IDE.Web.Claude
       (ClaudeLive(..), ClaudeBackground(..), backgroundNeedsYou,
        claudeBackgroundSessions, claudeLiveBySession, claudeSessionLabel)

-- | One live session, as a status surface shows it.
data ClaudeStatusRow = ClaudeStatusRow
  { csrState   :: Text  -- ^ @waiting@ (blocked on an approval prompt) \/ @busy@
                        --   (the agent or a shell command is working) \/ @idle@
  , csrName    :: Text  -- ^ the name leksah gave it ("Wren" — see
                        --   "IDE.Web.AgentNames"): WHO it is, as against
                        --   'csrTitle', which says what it is doing
  , csrTitle   :: Text  -- ^ what the agent calls itself (@agent describe@), else
                        --   the name @\/rename@ gave it, else its first prompt
  , csrDir     :: Text  -- ^ its working directory (absolute)
  , csrDetail  :: Text  -- ^ what it is doing, spelled out
  , csrSession :: Text  -- ^ session id — the handle for showing this session
  , csrBackground :: Bool -- ^ a Claude Code BACKGROUND session with no process
                          --   ('backgroundNeedsYou': blocked, recently) — it
                          --   counts on every status surface, but is not a live
                          --   session (no pane; showing it resumes it)
  } deriving (Eq, Show)

-- | The live sessions, worst state first.
data ClaudeStatus = ClaudeStatus
  { csState   :: Text              -- ^ worst of the rows: @waiting@ \/ @busy@ \/
                                   --   @idle@, or @none@ with nothing running
  , csCount   :: Int               -- ^ how many sessions are in 'csState' — what
                                   --   the status surfaces draw beside the glyph.
                                   --   0 unless the state needs you (@waiting@ \/
                                   --   @busy@): all-idle and nothing-running are
                                   --   not situations to put a number on.
  , csSummary :: Text              -- ^ one line, e.g. @"3 agents — 1 waiting"@
  , csRows    :: [ClaudeStatusRow] -- ^ attention first, then working, then idle
  } deriving (Eq, Show)

emptyClaudeStatus :: ClaudeStatus
emptyClaudeStatus = ClaudeStatus "none" 0 "No agents running" []

-- The latest poll result, and the push handlers to notify when it changes.
-- (Interpreted-module CAFs: a ghci :reload gives the next instance empty ones,
-- which is right — its own poll refills them within a tick and the old
-- instance's handlers are gone with it.)
{-# NOINLINE statusRef #-}
statusRef :: IORef ClaudeStatus
statusRef = unsafePerformIO (newIORef emptyClaudeStatus)

{-# NOINLINE pushRef #-}
pushRef :: IORef [ClaudeStatus -> IO ()]
pushRef = unsafePerformIO (newIORef [])

-- | A poke for the poll: look again now rather than at the next 3s tick.
{-# NOINLINE pokeVar #-}
pokeVar :: MVar ()
pokeVar = unsafePerformIO newEmptyMVar

-- | Re-read the sessions now — what a Claude Code hook calls when a session
-- has just finished a turn, asked for approval, or ended, so every status
-- surface shows it at once instead of within 3s.  Pokes again a moment later
-- too: the hook can run a beat before the CLI rewrites its session file.
pokeClaudeStatus :: IO ()
pokeClaudeStatus = do
  _ <- tryPutMVar pokeVar ()
  void . forkIO $ threadDelay 400000 >> void (tryPutMVar pokeVar ())

-- | The most recent poll result.  A plain 'IORef' read — safe to call from a
-- reflex tick on the frame thread.
claudeStatusNow :: IO ClaudeStatus
claudeStatusNow = readIORef statusRef

-- | Be told whenever the status CHANGES (and once, immediately, with the current
-- value so the caller starts in sync).  For surfaces that can't poll — the
-- native menu-bar item.  Handlers run on the poll thread: keep them quick, and
-- don't touch a reflex network from one.
registerClaudeStatusPush :: (ClaudeStatus -> IO ()) -> IO ()
registerClaudeStatusPush h = do
  atomicModifyIORef' pushRef (\hs -> (hs <> [h], ()))
  claudeStatusNow >>= \s -> h s `catch` \(_ :: SomeException) -> return ()

-- | Start the 3s poll.  Returns its 'ThreadId' so ghci mode can kill it at
-- teardown — otherwise every @:reload@ would leave another poll running.
--
-- Session labels are read once each and cached: the first prompt of a
-- transcript can't change (a @\/rename@ name can, and comes free with the poll).
--
-- @titlesIO@ supplies the names agents gave THEMSELVES
-- ('IDE.Web.AgentInfo.agentTitles' — passed in rather than imported, since that
-- module reads this one's rows).  It is read every cycle, so a session that
-- describes itself is renamed in every status surface within a tick.
--
-- @heldIO@ gives the sessions leksah itself holds as suspended or interrupted
-- ('IDE.Web.AgentInfo.agentsHeld'): the Agents pane shows those as such, so a
-- blocked background session among them is not counted as needing you here
-- either — the two would disagree.
startClaudeStatusPoll :: IO (Map Text Text) -> IO [Text] -> IO ThreadId
startClaudeStatusPoll titlesIO heldIO = forkIO (loop emptyClaudeStatus M.empty)
  where
    loop prev labels = do
      (st, labels') <- (`catch` \(_ :: SomeException) -> return (prev, labels)) $ do
        live0 <- claudeLiveBySession
        -- A prompt was just submitted (the UserPromptSubmit hook): working
        -- from this moment, though its session file may still say idle for a
        -- little while.
        events <- sessionEvents
        nowP <- realToFrac <$> getPOSIXTime
        let pending l = maybe False (sePromptPending 5 nowP) (M.lookup (clSession l) events)
            live = M.map (\l -> if clStatus l == Just "idle" && pending l
                                   then l { clStatus = Just "busy" } else l) live0
        -- Label the unnamed ones: one transcript head-read each, first time only.
        labels' <- foldM addLabel labels
          [ l | l <- M.elems live, clName l == Nothing
              , not (clSession l `M.member` labels) ]
        titles <- titlesIO
        -- A background session blocked on a question has no pane, so nothing
        -- else says it is waiting: it counts as needing you here too, while
        -- it blocked recently ('backgroundNeedsYou').
        now <- getCurrentTime
        held <- heldIO
        blocked <- filter (\b -> backgroundNeedsYou now b && not (cbSession b `M.member` live)
                                 && cbSession b `notElem` held)
                     <$> claudeBackgroundSessions
        -- The poll is where a session leksah did NOT launch (claude -c, the
        -- resume picker, one typed into a terminal) first gets its name.
        -- (Only the live ones are "seen running": that is what
        -- touchAgentNames records, and what the interrupted-agent logic
        -- reads.  A blocked background session's name is only looked up.)
        names0 <- touchAgentNames (M.keys live)
        known  <- agentNames
        let names = names0 <> M.filterWithKey (\k _ -> k `elem` map cbSession blocked) known
        return (summarize names titles labels' (M.elems live) blocked, labels')
      when (st /= prev) $ do
        atomicWriteIORef statusRef st
        readIORef pushRef >>= mapM_ (\h ->
          h st `catch` \(_ :: SomeException) -> return ())
      -- Every 3s — or at once, when a hook says something changed.
      _ <- timeout 3000000 (takeMVar pokeVar)
      loop st labels'
    addLabel m l = claudeSessionLabel (clDir l) (clSession l) >>= \case
      Just lbl -> return (M.insert (clSession l) lbl m)
      Nothing  -> return m

-- | Fold the live sessions into a 'ClaudeStatus': attention first (blocked on an
-- approval prompt), then working, then idle, each group by title.  Takes the
-- agent-written titles (session → title) and the cached first-prompt labels.
summarize :: Map Text Text -> Map Text Text -> Map Text Text -> [ClaudeLive]
          -> [ClaudeBackground] -> ClaudeStatus
summarize names titles labels ls bgs = ClaudeStatus
  { csState   = if null ls && null bgs then "none"
                else if nWaiting > 0 then "waiting"
                else if nWorking > 0 then "busy" else "idle"
    -- The count belongs to whichever state won above, so it always answers "how
    -- many of THAT?" — and falls out as 0 for idle/none, which is what the
    -- surfaces want (no number on green or grey).
  , csCount   = if nWaiting > 0 then nWaiting else nWorking
  , csSummary = summary
  , csRows    = map bgRow (sortOn (T.toLower . bgTitle) bgs)
                <> map row (sortOn (\l -> (rank l, T.toLower (title l))) ls)
  }
  where
    rank l = case clStatus l of
      Just "waiting" -> 0 :: Int
      Just "busy"    -> 1
      Just "shell"   -> 1
      _              -> 2
    nWaiting = length (filter ((== 0) . rank) ls) + length bgs
    nWorking = length (filter ((== 1) . rank) ls)
    nAll = length ls + length bgs
    summary
      | nAll == 0 = csSummary emptyClaudeStatus
      | otherwise = num nAll
                      <> (if nAll == 1 then " agent" else " agents") <> detail
      where detail = case [ num n <> w | (n, w) <- [ (nWaiting, " waiting")
                                                   , (nWorking, " working") ], n > 0 ] of
              [] -> ""
              ds -> " — " <> T.intercalate ", " ds
    num n = T.pack (show n)
    -- What the agent calls ITSELF wins ('IDE.Web.AgentInfo.describeAgent' — the
    -- Agents pane's title): it says what the session is FOR, where a /rename
    -- name says what it was called and a first prompt says how it opened.  So
    -- every surface that lists sessions reads the same, and an agent renames
    -- itself everywhere at once.  Then the name, the cached first prompt, the
    -- bare id.
    title l = fromMaybe (T.take 8 (clSession l)) $
      M.lookup (clSession l) titles
        <|> clName l
        <|> M.lookup (clSession l) labels
    row l = ClaudeStatusRow
      { csrState   = case rank l of
          0 -> "waiting"
          1 -> "busy"
          _ -> "idle"
      , csrName    = M.findWithDefault "" (clSession l) names
      , csrTitle   = title l
      , csrDir     = T.pack (clDir l)
      , csrDetail  = case clStatus l of
          Just "waiting" -> "Waiting for approval" <> maybe "" (": " <>) (clWaitingFor l)
          Just "busy"    -> "Working"
          Just "shell"   -> "Running a shell command"
          Just "idle"    -> "Idle — ready for input"
          _              -> "Running"
      , csrSession = clSession l
      , csrBackground = False
      }
    bgTitle b = fromMaybe (T.take 8 (cbSession b)) $
      M.lookup (cbSession b) titles <|> cbName b
    bgRow b = ClaudeStatusRow
      { csrState   = "waiting"
      , csrName    = M.findWithDefault "" (cbSession b) names
      , csrTitle   = bgTitle b
      , csrDir     = T.pack (cbDir b)
      , csrDetail  = "Background session, blocked — waiting for you (open it to answer)"
      , csrSession = cbSession b
      , csrBackground = True
      }

-- | One session in one line, as every list of them prints it: its name, then
-- what it is doing — @Wren — fix the CI failure@.
claudeRowLabel :: ClaudeStatusRow -> Text
claudeRowLabel r
  | T.null (csrName r)  = csrTitle r
  | T.null (csrTitle r) = csrName r
  | otherwise           = csrName r <> " — " <> csrTitle r

-- | The status-surface glyph for a state: the same three shapes the macOS
-- menu-bar item and the in-page traffic light draw, as text (for tooltips and
-- tree rows) — triangle = needs you, diamond = working, circle = idle.
claudeStatusGlyph :: Text -> Text
claudeStatusGlyph = \case
  "waiting" -> "▲"
  "busy"    -> "◆"
  "idle"    -> "●"
  _         -> "○"

-- | The hover text for a status surface: the summary, then one line per session
-- (glyph, title, directory), so the same information the menu-bar item's menu
-- lists is reachable in the page too.
claudeStatusTooltip :: ClaudeStatus -> Text
claudeStatusTooltip st = T.intercalate "\n" (csSummary st : map line (csRows st))
  where
    line r = claudeStatusGlyph (csrState r) <> " " <> claudeRowLabel r
               <> (if T.null (csrDir r) then "" else "  ·  " <> baseName (csrDir r))
    baseName = T.takeWhileEnd (/= '/') . T.dropWhileEnd (== '/')

-- | The rows as the in-page status light's hover list draws them (see
-- @statusLightJs@ in "IDE.Web.Main"): a JSON array, one object per session,
-- in 'csRows' order — the same order and the same 'claudeRowLabel' the
-- menu-bar item's menu uses.  @face@ is the animal's face as a @data:@ URI
-- ('agentFaceDataUri'), empty for a session with no name yet; @name@ and
-- @title@ are 'claudeRowLabel''s two halves (so the page can set the name in
-- bold); @tip@ is the row's hover text (full directory, what it is doing).
claudeStatusRowsJson :: ClaudeStatus -> Text
claudeStatusRowsJson st =
    decodeUtf8 . BL.toStrict . A.encode $ map row (csRows st)
  where
    row r = A.object
      [ "state" A..= csrState r
      , "label" A..= claudeRowLabel r
      , "name"  A..= csrName r
      , "title" A..= csrTitle r
      , "dir"   A..= baseName (csrDir r)
      , "tip"   A..= T.intercalate "\n" (filter (not . T.null) [csrDir r, csrDetail r])
      , "sid"   A..= csrSession r
      , "face"  A..= (if T.null (csrName r) then "" else agentFaceDataUri (csrName r))
      ]
    baseName = T.takeWhileEnd (/= '/') . T.dropWhileEnd (== '/')
