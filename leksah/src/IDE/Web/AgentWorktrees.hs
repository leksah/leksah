{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  IDE.Web.AgentWorktrees
--
-- The worktree registry ("IDE.Web.WorktreeRegistry") turned round: per AGENT,
-- the worktrees it registered — for the Agents pane (the top agent's
-- worktrees, under its card) and the Workspace pane (an "Agents" node with
-- one expandable row per agent).
--
-- Claims are recorded per Claude SESSION, but an agent keeps its name across
-- sessions (a backgrounded or compacted agent carries on under a new id —
-- 'agentContinuations'), so claims are grouped by the session each one's
-- chain ends in.  An agent's claims on one path collapse to the newest.
--
-- What counts as "its worktrees now" ('awCurrent'): a created\/working claim
-- on a path that still exists, made within the last 'staleDays' — or made at
-- any time, while the agent is live.  The rest that still exist (reviewing,
-- abandoned, stale) are kept apart for an "Earlier" group; paths that are gone
-- from disk are not shown at all.  The registry itself is never pruned: it is
-- history, and a re-created path picks its old story back up.
-----------------------------------------------------------------------------
module IDE.Web.AgentWorktrees
  ( AgentWorktree(..)
  , AgentWorktrees(..)
  , agentSessions
  , worktreesOfAgent
  , allAgentWorktrees
  , worktreesBySession
  , currentClaimants
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (filterM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import Data.List (sortOn)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.Directory (doesDirectoryExist)

import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.AgentNames (agentContinuations, agentNames)
import IDE.Web.ClaudeStatus (ClaudeStatus(..), ClaudeStatusRow(..), claudeStatusNow)
import IDE.Web.WorktreeRegistry
       (ClaimRole(..), SessionClaim(..), WorktreeInfo(..), allWorktrees)

-- | One worktree an agent has a claim on (its newest claim there).
data AgentWorktree = AgentWorktree
  { awPath     :: FilePath
  , awMainRoot :: FilePath   -- ^ the main checkout ('awPath' when unknown)
  , awRole     :: ClaimRole
  , awNote     :: Text
  , awBranch   :: Text       -- ^ newest recorded branch (a fallback label:
                             --   the rows read the live one from git)
  , awWhen     :: Double
  , awCurrent  :: Bool       -- ^ one of its worktrees NOW (see the module doc)
  } deriving (Eq, Show)

-- | One agent and its worktrees, newest claim first.
data AgentWorktrees = AgentWorktrees
  { agSession   :: Text      -- ^ the agent's current session id
  , agName      :: Text      -- ^ its name ('' when it never had one)
  , agLive      :: Bool
  , agWorktrees :: [AgentWorktree]
  } deriving (Eq, Show)

-- | Claims older than this (days) stop counting as current once the agent
-- has exited.
staleDays :: Double
staleDays = 14

-- | Every session id that is (or was) the agent whose current session is
-- @sid@: itself and every session that carried on into it.
agentSessions :: Map Text Text -> Text -> S.Set Text
agentSessions conts sid =
  S.fromList (sid : [ old | (old, new) <- M.toList conts, new == sid ])

-- | The worktrees of the agent whose current session is @sid@.
worktreesOfAgent :: Text -> IO [AgentWorktree]
worktreesOfAgent sid = do
  groups <- allAgentWorktrees
  return $ maybe [] agWorktrees (lookup sid [ (agSession g, g) | g <- groups ])

-- | Every agent with a claim on a worktree that still exists: live agents
-- first, then the rest by their newest claim.
--
-- Cached for a few seconds: every Agents node in the trees polls this (and
-- 'worktreesBySession'), and one read per period is plenty.
allAgentWorktrees :: IO [AgentWorktrees]
allAgentWorktrees = fst <$> snapshot

-- | Every session id — an agent's current one and each earlier one that
-- carried on into it — to that agent's worktrees (agents with any).  What a
-- session row under an Agents node expands to.
worktreesBySession :: IO (Map Text [AgentWorktree])
worktreesBySession = snd <$> snapshot

-- | The agents with a CURRENT claim ('awCurrent') on the worktree at @path@:
-- (session, name — the short id when it has none), live ones first.
currentClaimants :: FilePath -> IO [(Text, Text)]
currentClaimants path = do
  gs <- allAgentWorktrees
  return [ (agSession g, if T.null (agName g) then T.take 8 (agSession g) else agName g)
         | g <- gs, any (\w -> awPath w == path && awCurrent w) (agWorktrees g) ]

{-# NOINLINE snapshotRef #-}
snapshotRef :: IORef (Double, ([AgentWorktrees], Map Text [AgentWorktree]))
snapshotRef = unsafePerformIO (newIORef (0, ([], M.empty)))

snapshot :: IO ([AgentWorktrees], Map Text [AgentWorktree])
snapshot = do
  now <- realToFrac <$> getPOSIXTime
  (at, v) <- readIORef snapshotRef
  if now - at < 4 then return v else do
    gs    <- computeAgentWorktrees
    conts <- agentContinuations
    let bySid = M.fromList
          [ (s, agWorktrees g) | g <- gs, s <- S.toList (agentSessions conts (agSession g)) ]
        v' = (gs, bySid)
    writeIORef snapshotRef (now, v')
    return v'

computeAgentWorktrees :: IO [AgentWorktrees]
computeAgentWorktrees = do
  wis   <- allWorktrees
  conts <- agentContinuations
  names <- agentNames
  live  <- (S.fromList . map csrSession . csRows <$> claudeStatusNow)
             `catch` \(_ :: SomeException) -> return S.empty
  now   <- realToFrac <$> getPOSIXTime
  -- Which paths still exist (remote ones are taken on trust: a stat would
  -- be an ssh round trip per row).
  present <- S.fromList <$> filterM exists (map wiPath wis)
  let final s = M.findWithDefault s s conts
      -- (agent, path) -> newest claim
      newest = M.fromListWith pick
        [ ((final s, wiPath w), (w, c))
        | w <- wis, wiPath w `S.member` present
        , c <- wiClaims w, Just s <- [scSession c] ]
      pick a b = if scWhen (snd a) >= scWhen (snd b) then a else b
      byAgent = M.fromListWith (<>)
        [ (a, [entry a w c]) | ((a, _), (w, c)) <- M.toList newest ]
      entry a w c =
        let isLive = a `S.member` live
            role   = scRole c
            fresh  = now - scWhen c < staleDays * 86400
        in AgentWorktree
             { awPath     = wiPath w
             , awMainRoot = if null (wiMainRoot w) then wiPath w else wiMainRoot w
             , awRole     = role
             , awNote     = scNote c
             , awBranch   = fromMaybe "" (scBranch c)
             , awWhen     = scWhen c
             , awCurrent  = role `elem` [RoleCreated, RoleWorking] && (isLive || fresh) }
      groups =
        [ AgentWorktrees a (M.findWithDefault "" a names) (a `S.member` live)
                         (sortOn (Down . awWhen) ws)
        | (a, ws) <- M.toList byAgent ]
  return $ sortOn (\g -> ( not (agLive g)
                         , Down (maximum (0 : map awWhen (agWorktrees g))) ))
                  groups
  where
    exists p | isRemotePath p = return True
             | otherwise      = doesDirectoryExist p
                                  `catch` \(_ :: SomeException) -> return False
