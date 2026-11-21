{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | The part of a directory's right-click menu that starts agents: one
-- "New … Session" entry per agent CLI on PATH, each with that CLI's badge,
-- and "Continue Recent Session ▸", which lists the sessions that ran there —
-- the agent's face, its CLI's badge, its name, what it was asked, how long
-- ago.  The same entries in every menu that offers them (the workspace tree's
-- project, package and worktree rows; the file tree's directories and Agents
-- nodes), so starting an agent reads the same everywhere.
module IDE.Web.Widget.AgentMenu
  ( availableClis
  , agentMenuEntries
  , agentMenuEntriesD
  , newSessionEntries
  , continueRecentEntry
  ) where

import Control.Monad (filterM)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import Reflex (Dynamic, Reflex, ffor)

import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.AgentCli (AgentCli(..), allClis, cliLabel)
import IDE.Web.AgentFace (agentFaceSvg)
import IDE.Web.AgentGlyph (cliGlyph)
import IDE.Web.Agy (agyAvailable)
import IDE.Web.Claude (ClaudeCmd(..), claudeAvailable, runClaudeCmd)
import IDE.Web.Codex (codexAvailable)
import IDE.Web.RecentSessions (RecentSession(..), recentSessions)
import IDE.Web.SplitOpenRequest (SplitTarget(..))
import IDE.Web.Widget.Menu (MenuEntry(..))

-- | The agent CLIs on PATH, in menu order.
availableClis :: IO [AgentCli]
availableClis = filterM avail allClis
  where avail = \case
          CliClaude -> claudeAvailable
          CliCodex  -> codexAvailable
          CliAgy    -> agyAvailable

-- | 'newSessionEntries', then 'continueRecentEntry' — the usual pair.
agentMenuEntries :: (IO () -> a) -> Text -> [AgentCli] -> FilePath
                 -> [MenuEntry (Maybe SplitTarget, a)]
agentMenuEntries wrap suffix clis dir =
  newSessionEntries wrap suffix clis dir <> continueRecentEntry wrap suffix clis dir

-- | 'agentMenuEntries' for a directory that can change (a package's): one
-- entry per CLI, and one for Continue, each hidden while it does not apply.
agentMenuEntriesD :: Reflex t => (IO () -> a) -> Text -> [AgentCli] -> Dynamic t FilePath
                  -> [Dynamic t (MenuEntry (Maybe SplitTarget, a))]
agentMenuEntriesD wrap suffix clis dirD =
  [ ffor dirD $ \d -> firstOr (newSessionEntries wrap suffix [c] d) | c <- clis ]
  <> [ ffor dirD $ \d -> firstOr (continueRecentEntry wrap suffix clis d) | not (null clis) ]
  where
    -- (an entry with no label is not shown: 'MenuEntry')
    firstOr (e : _) = e
    firstOr []      = MenuEntry [] "" (Nothing, wrap (return ()))

-- | "New Claude Session", "New Codex Session", "New Antigravity Session" (as
-- many as are on PATH, each with its badge; @suffix@: " Here", say).  ⌥-click
-- opens one in a split of the active pane — on this machine: a directory on
-- another one runs Claude there, and the other CLIs not at all yet.
newSessionEntries :: (IO () -> a) -> Text -> [AgentCli] -> FilePath
                  -> [MenuEntry (Maybe SplitTarget, a)]
newSessionEntries wrap suffix clis dir =
  [ MenuEntry [cliGlyph c] ("New " <> cliLabel c <> " Session" <> suffix)
      (splitTarget dir (STCliNew c dir), wrap (runClaudeCmd (CliNew c dir)))
  | c <- clisFor dir clis ]

-- | "Continue Recent Session ▸": the sessions that ran in @dir@ and are not
-- running, newest first, whichever CLI ran them; choosing one resumes it.
-- Looked for when the menu opens ('recentSessions').  Nothing when no agent
-- CLI is on PATH.
continueRecentEntry :: (IO () -> a) -> Text -> [AgentCli] -> FilePath
                    -> [MenuEntry (Maybe SplitTarget, a)]
continueRecentEntry wrap suffix clis dir =
  [ MenuSubmenu [] ("Continue Recent Session" <> suffix) "No recent sessions here" $
      map entry . filter ((`elem` clis) . rsCli) <$> recentSessions 12 dir
  | not (null (clisFor dir clis)) ]
  where
    entry r = MenuEntry
      -- (an unnamed one keeps the face's place, so the badges line up)
      [ maybe "" agentFaceSvg (rsName r), cliGlyph (rsCli r) ]
      (labelOf r)
      ( splitTarget dir (STCliResume (rsCli r) dir (rsId r))
      , wrap (runClaudeCmd (CliResume (rsCli r) dir (rsId r))) )
    labelOf r = T.intercalate " · " $ filter (not . T.null)
      [ maybe "" id (rsName r)
      , if T.null (rsLabel r)
          then (if isNothing (rsName r) then T.take 8 (rsId r) else "")
          else clip 48 (rsLabel r)
      , rsAge r ]
    clip k t = if T.length t > k then T.stripEnd (T.take (k - 1) t) <> "…" else t

-- | The CLIs that can start in @dir@: on another machine only Claude can
-- (Codex and agy sessions don't run remotely yet).
clisFor :: FilePath -> [AgentCli] -> [AgentCli]
clisFor dir clis | isRemotePath dir = filter (== CliClaude) clis
                 | otherwise        = clis

-- | A split target, for a directory on this machine (a split runs locally).
splitTarget :: FilePath -> SplitTarget -> Maybe SplitTarget
splitTarget dir t = if isRemotePath dir then Nothing else Just t
