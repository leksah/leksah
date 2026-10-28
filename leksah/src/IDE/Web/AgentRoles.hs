{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Agent ROLES: kinds of agent you can start by name — @leksah-cmd agent fork
-- --as reviewer 'look at my diff'@ — so picking the right agent for a task is
-- picking from a list rather than writing a briefing.
--
-- Leksah has no role format of its own.  It reads the subagent definitions
-- Claude Code already uses — Markdown files with a front-matter header:
--
-- > ---
-- > name: reviewer
-- > description: Reviews a diff for correctness bugs before it is pushed
-- > ---
-- > You are a careful code reviewer. …
--
-- from @.claude\/agents\/@ in the project (the directory and each one above it)
-- and from @~\/.claude\/agents\/@.  The body becomes part of the started
-- agent's system prompt; the description is what @agent find@ and the Agents
-- pane show and match against.  A project's role shadows a user one of the
-- same name, as in Claude Code.
module IDE.Web.AgentRoles
  ( AgentRole(..)
  , agentRoles
  , findAgentRole
  , roleNote
  ) where

import Control.Exception (catch, SomeException)
import Control.Monad (forM)

import Data.Char (isSpace)
import Data.List (nubBy, sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory
       (doesDirectoryExist, getHomeDirectory, listDirectory, makeAbsolute)
import System.FilePath ((</>), takeDirectory, takeExtension, dropTrailingPathSeparator)

-- | One role, as its file describes it.
data AgentRole = AgentRole
  { roleName        :: Text      -- ^ how it is asked for (@--as NAME@)
  , roleDescription :: Text      -- ^ one line: what it is for
  , rolePrompt      :: Text      -- ^ the file's body — its instructions
  , roleFile        :: FilePath  -- ^ where it was defined
  } deriving (Eq, Show)

-- | Every role visible from @dir@: the project's (nearest directory first),
-- then the user's, by name, without duplicates.
agentRoles :: FilePath -> IO [AgentRole]
agentRoles dir0 = (`catch` \(_ :: SomeException) -> return []) $ do
  dir  <- makeAbsolute dir0
  home <- getHomeDirectory
  let ups = takeWhile (/= takeDirectory home) (ancestors (dropTrailingPathSeparator dir))
      dirs = [ d </> ".claude" </> "agents" | d <- ups ]
             <> [ home </> ".claude" </> "agents" ]
  rs <- concat <$> mapM rolesIn dirs
  return . sortOn roleName $ nubBy (\a b -> roleName a == roleName b) rs
  where
    ancestors d = d : let p = takeDirectory d in if p == d then [] else ancestors p

rolesIn :: FilePath -> IO [AgentRole]
rolesIn d = (`catch` \(_ :: SomeException) -> return []) $ do
  ok <- doesDirectoryExist d
  if not ok then return [] else do
    fs <- filter ((== ".md") . takeExtension) <$> listDirectory d
    fmap catMaybes . forM fs $ \f ->
      (parseRole (d </> f) <$> T.readFile (d </> f))
        `catch` \(_ :: SomeException) -> return Nothing

-- | A role file: front matter between @---@ lines (simple @key: value@ lines;
-- quotes stripped), then the body.  A file without a name in its front matter
-- isn't a role.
parseRole :: FilePath -> Text -> Maybe AgentRole
parseRole file txt = case T.lines txt of
  (l0 : rest) | T.strip l0 == "---" ->
    let (hdr, body) = break ((== "---") . T.strip) rest
        fields = [ (T.toLower (T.strip k), unquote (T.strip (T.drop 1 v)))
                 | l <- hdr, let (k, v) = T.breakOn ":" l, not (T.null v) ]
    in case lookup "name" fields of
         Just n | not (T.null n) -> Just AgentRole
           { roleName        = n
           , roleDescription = maybe "" id (lookup "description" fields)
           , rolePrompt      = T.strip (T.unlines (drop 1 body))
           , roleFile        = file }
         _ -> Nothing
  _ -> Nothing
  where
    unquote t = case T.uncons t of
      Just (q, r) | q `elem` ['"', '\''], T.takeEnd 1 r == T.singleton q -> T.dropEnd 1 r
      _ -> T.dropWhileEnd isSpace t

-- | The role called @name@ (case-insensitive), as seen from @dir@.
findAgentRole :: FilePath -> Text -> IO (Either Text AgentRole)
findAgentRole dir name = do
  rs <- agentRoles dir
  return $ case [ r | r <- rs, T.toLower (roleName r) == T.toLower (T.strip name) ] of
    (r : _) -> Right r
    [] | null rs   -> Left ("no role called " <> name <> " — define one in "
                            <> ".claude/agents/" <> name <> ".md (Claude Code's "
                            <> "subagent format)")
       | otherwise -> Left ("no role called " <> name <> "; the roles here are: "
                            <> T.intercalate ", " (map roleName rs))

-- | What a role agent is told at launch: its role, then the role's own
-- instructions.
roleNote :: AgentRole -> Text
roleNote r = T.unlines
  [ "You were started in the role \"" <> roleName r <> "\""
      <> (if T.null (roleDescription r) then "." else ": " <> roleDescription r)
  , ""
  , rolePrompt r ]
