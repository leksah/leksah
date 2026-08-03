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
  , claudeStatusGlyph
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad (foldM, when)

import Data.IORef
       (IORef, newIORef, readIORef, atomicModifyIORef', atomicWriteIORef)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

import IDE.Web.Claude
       (ClaudeLive(..), claudeLiveBySession, claudeSessionLabel)

-- | One live session, as a status surface shows it.
data ClaudeStatusRow = ClaudeStatusRow
  { csrState   :: Text  -- ^ @waiting@ (blocked on an approval prompt) \/ @busy@
                        --   (the agent or a shell command is working) \/ @idle@
  , csrTitle   :: Text  -- ^ the name @\/rename@ gave it, else its first prompt
  , csrDir     :: Text  -- ^ its working directory (absolute)
  , csrDetail  :: Text  -- ^ what it is doing, spelled out
  , csrSession :: Text  -- ^ session id — the handle for showing this session
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
  , csSummary :: Text              -- ^ one line, e.g. @"Claude: 3 sessions — 1 waiting"@
  , csRows    :: [ClaudeStatusRow] -- ^ attention first, then working, then idle
  } deriving (Eq, Show)

emptyClaudeStatus :: ClaudeStatus
emptyClaudeStatus = ClaudeStatus "none" 0 "Claude: no sessions running" []

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
startClaudeStatusPoll :: IO ThreadId
startClaudeStatusPoll = forkIO (loop emptyClaudeStatus M.empty)
  where
    loop prev labels = do
      (st, labels') <- (`catch` \(_ :: SomeException) -> return (prev, labels)) $ do
        live <- claudeLiveBySession
        -- Label the unnamed ones: one transcript head-read each, first time only.
        labels' <- foldM addLabel labels
          [ l | l <- M.elems live, clName l == Nothing
              , not (clSession l `M.member` labels) ]
        return (summarize labels' (M.elems live), labels')
      when (st /= prev) $ do
        atomicWriteIORef statusRef st
        readIORef pushRef >>= mapM_ (\h ->
          h st `catch` \(_ :: SomeException) -> return ())
      threadDelay 3000000
      loop st labels'
    addLabel m l = claudeSessionLabel (clDir l) (clSession l) >>= \case
      Just lbl -> return (M.insert (clSession l) lbl m)
      Nothing  -> return m

-- | Fold the live sessions into a 'ClaudeStatus': attention first (blocked on an
-- approval prompt), then working, then idle, each group by title.
summarize :: Map Text Text -> [ClaudeLive] -> ClaudeStatus
summarize labels ls = ClaudeStatus
  { csState   = if null ls then "none"
                else if nWaiting > 0 then "waiting"
                else if nWorking > 0 then "busy" else "idle"
    -- The count belongs to whichever state won above, so it always answers "how
    -- many of THAT?" — and falls out as 0 for idle/none, which is what the
    -- surfaces want (no number on green or grey).
  , csCount   = if nWaiting > 0 then nWaiting else nWorking
  , csSummary = summary
  , csRows    = map row (sortOn (\l -> (rank l, T.toLower (title l))) ls)
  }
  where
    rank l = case clStatus l of
      Just "waiting" -> 0 :: Int
      Just "busy"    -> 1
      Just "shell"   -> 1
      _              -> 2
    nWaiting = length (filter ((== 0) . rank) ls)
    nWorking = length (filter ((== 1) . rank) ls)
    summary
      | null ls = csSummary emptyClaudeStatus
      | otherwise = "Claude: " <> num (length ls)
                      <> (if length ls == 1 then " session" else " sessions") <> detail
      where detail = case [ num n <> w | (n, w) <- [ (nWaiting, " waiting")
                                                   , (nWorking, " working") ], n > 0 ] of
              [] -> ""
              ds -> " — " <> T.intercalate ", " ds
    num n = T.pack (show n)
    -- The name /rename gave it, else its cached first prompt, else the bare id.
    title l = fromMaybe (M.findWithDefault (T.take 8 (clSession l)) (clSession l) labels)
                        (clName l)
    row l = ClaudeStatusRow
      { csrState   = case rank l of
          0 -> "waiting"
          1 -> "busy"
          _ -> "idle"
      , csrTitle   = title l
      , csrDir     = T.pack (clDir l)
      , csrDetail  = case clStatus l of
          Just "waiting" -> "Waiting for approval" <> maybe "" (": " <>) (clWaitingFor l)
          Just "busy"    -> "Working"
          Just "shell"   -> "Running a shell command"
          Just "idle"    -> "Idle — ready for input"
          _              -> "Running"
      , csrSession = clSession l
      }

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
    line r = claudeStatusGlyph (csrState r) <> " " <> csrTitle r
               <> (if T.null (csrDir r) then "" else "  ·  " <> baseName (csrDir r))
    baseName = T.takeWhileEnd (/= '/') . T.dropWhileEnd (== '/')
