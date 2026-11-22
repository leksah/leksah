{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The sessions that ran in a directory, whichever CLI ran them — what the
-- "Continue Recent Session" menus offer ("IDE.Web.Widget.AgentMenu").
module IDE.Web.RecentSessions
  ( RecentSession(..)
  , recentSessions
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (forM)
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import System.Directory (getModificationTime)

import IDE.Web.AgentCli (AgentCli(..))
import IDE.Web.AgentNames (agentContinuations, agentNames)
import IDE.Web.Agy (agyFirstPrompt, agySessionsIn)
import IDE.Web.AgentInfo (agentTitles)
import IDE.Web.Claude
       (ClaudeSession(..), TranscriptGist(..), cachedTranscriptGist,
        claudeLiveBySession, claudeSessionsFor, humanAge)
import IDE.Web.Codex (codexSessionsIn, rolloutFirstPrompt)

-- | A session that ran in a directory and is not running now.
data RecentSession = RecentSession
  { rsCli      :: AgentCli
  , rsId       :: Text
  , rsModified :: UTCTime     -- ^ its transcript's last write
  , rsAge      :: Text        -- ^ e.g. @3h ago@
  , rsLabel    :: Text        -- ^ what it is about (one line), or @""@:
                              --   the title the agent gave itself, else
                              --   Claude Code's title for the conversation,
                              --   else its first prompt
  , rsName     :: Maybe Text  -- ^ the agent name leksah gave it, if any
  } deriving (Eq, Show)

-- | The @n@ most recent sessions of these CLIs that ran in @dir@ and are not
-- running now, newest first: Claude Code's saved transcripts there, and the
-- Codex and Antigravity sessions leksah's hooks saw there.  Running ones are
-- left out (they are in the tree already, and continuing one would only show
-- it), and so are ones that carried on under another id (a backgrounded
-- session: resuming the old id would fork a conversation still running).  A
-- conversation an agent left with @/clear@ stays: it can still be resumed.
recentSessions :: [AgentCli] -> Int -> FilePath -> IO [RecentSession]
recentSessions clis n dir = (`catch` \(_ :: SomeException) -> return []) $ do
  live  <- claudeLiveBySession
  conts <- agentContinuations
  names <- agentNames
  titles <- agentTitles
  now   <- getCurrentTime
  let keep sid = sid `M.notMember` live && sid `M.notMember` conts
      age t = humanAge (diffUTCTime now t)
  claude <- if CliClaude `elem` clis then claudeSessionsFor dir else return []
  -- (only the newest n are labelled: a gist is a read of the transcript's tail)
  fromClaude <- forM (take n (sortOn (Down . csModified) [ s | s <- claude, keep (csId s) ])) $ \s -> do
    gist <- (Just <$> cachedTranscriptGist (csPath s))
              `catch` \(_ :: SomeException) -> return Nothing
    let lbl = labelFrom (M.lookup (csId s) titles) (gist >>= tgTitle) (csLabel s)
    return (RecentSession CliClaude (csId s) (csModified s) (csAge s) lbl (M.lookup (csId s) names))
  hooked <- fmap concat . forM [ x | x@(cli, _, _) <- [ (CliCodex, codexSessionsIn, rolloutFirstPrompt)
                                                         , (CliAgy, agySessionsIn, agyFirstPrompt) ]
                                  , cli `elem` clis ] $
    \(cli, sessionsIn, firstPrompt) -> do
      ss <- sessionsIn dir
      -- (only the newest n get their first prompt read: a read of each one)
      dated <- fmap catMaybes . forM [ s | s@(sid, _) <- ss, keep sid ] $ \(sid, tr) ->
        (`catch` \(_ :: SomeException) -> return Nothing) $
          (\t -> Just (sid, tr, t)) <$> getModificationTime tr
      forM (take n (sortOn (\(_, _, t) -> Down t) dated)) $ \(sid, tr, t) -> do
        prompt <- fromMaybe "" <$> (firstPrompt tr `catch` \(_ :: SomeException) -> return Nothing)
        return $ RecentSession cli sid t (age t)
          (labelFrom (M.lookup sid titles) Nothing prompt) (M.lookup sid names)
  return . take n . sortOn (Down . rsModified) $ fromClaude <> hooked
  where
    oneLine = T.strip . T.takeWhile (/= '\n')
    -- The first of these that says something.  A "prompt" that opens with a
    -- tag is the CLI's own wrapping (a slash command's caveat), not words.
    labelFrom title gistTitle prompt =
      case filter (not . T.null) (map (maybe "" oneLine) [title, gistTitle]) of
        (t : _) -> t
        []      -> let p = oneLine prompt in if "<" `T.isPrefixOf` p then "" else p
