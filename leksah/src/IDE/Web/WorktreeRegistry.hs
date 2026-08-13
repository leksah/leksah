{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  IDE.Web.WorktreeRegistry
--
-- What leksah remembers ABOUT git worktrees: which sessions created them,
-- work in them or reviewed them (a claim carries a free-text note saying
-- what the relationship is), and a small audit log of branch moves — a
-- worktree keeps its path for its whole life, but its branch is re-pointed
-- freely, and "who moved it, when" is exactly what a `git worktree list`
-- cannot answer.
--
-- The worktree PATH is the key.  The current branch is never read from here
-- for display (widgets read it live from git); 'wiBranches' is history.
--
-- Everything is FS\/JSON only (no reflex), because the writers are usually
-- control-socket connections (@leksah-cmd agent register@ / the
-- @register_worktree@ MCP tool land in "IDE.Web.CmdServer") or plain-IO
-- creation paths ("IDE.Web.Worktree", "IDE.Web.ClaudeQueue").  Readers are
-- pane polls, which must stay cheap — hence 'peekRegistry' next to
-- 'withRegistry', the "IDE.Web.AgentInfo" store pattern.
--
-- This module must not import "IDE.Web.Worktree" or "IDE.Web.Claude": both
-- of those import it.
-----------------------------------------------------------------------------
module IDE.Web.WorktreeRegistry
  ( -- * Types
    ClaimRole(..)
  , roleText
  , parseRole
  , BranchEvent(..)
  , SessionClaim(..)
  , WorktreeInfo(..)
    -- * Resolution
  , worktreeMainRoot
    -- * Writing
  , registerWorktree
  , recordBranchMove
    -- * Reading
  , worktreesForRoot
  , worktreeInfo
  , claimsBySession
  , owningSession
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (catch, try, SomeException)

import Data.Aeson
       (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject,
        encode, decode')
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath
       ((</>), dropTrailingPathSeparator, equalFilePath, takeDirectory,
        takeFileName)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Git (runGit)
import IDE.Paths (sidecarPath)
import IDE.Utils.RemotePath (isRemotePath)

--------------------------------------------------------------------------------
-- Types

-- | What a session says its relationship with a worktree is.
data ClaimRole
  = RoleCreated    -- ^ it made the worktree (or leksah made it on its behalf)
  | RoleWorking    -- ^ it is (or was) doing the work in it
  | RoleReviewing  -- ^ it is looking at someone else's work in it
  | RoleAbandoned  -- ^ it is done with it (archived / removed / handed off)
  deriving (Eq, Show)

roleText :: ClaimRole -> Text
roleText RoleCreated   = "created"
roleText RoleWorking   = "working"
roleText RoleReviewing = "reviewing"
roleText RoleAbandoned = "abandoned"

parseRole :: Text -> Maybe ClaimRole
parseRole "created"   = Just RoleCreated
parseRole "working"   = Just RoleWorking
parseRole "reviewing" = Just RoleReviewing
parseRole "abandoned" = Just RoleAbandoned
parseRole _           = Nothing

-- | One branch move: the worktree was (re-)pointed at @beBranch@.
data BranchEvent = BranchEvent
  { beBranch  :: Text        -- ^ branch name (or short sha when detached)
  , beSession :: Maybe Text  -- ^ who moved it ('Nothing' = human \/ leksah \/ unknown)
  , beVia     :: Text        -- ^ @leksah@ | @agent@ | @hook@ | @scan@
  , beWhen    :: Double      -- ^ POSIX seconds
  } deriving (Eq, Show)

instance ToJSON BranchEvent where
  toJSON e = object
    [ "branch" .= beBranch e, "session" .= beSession e
    , "via" .= beVia e, "when" .= beWhen e ]

instance FromJSON BranchEvent where
  parseJSON = withObject "BranchEvent" $ \o -> BranchEvent
    <$> o .: "branch" <*> o .:? "session"
    <*> (fromMaybe "" <$> o .:? "via") <*> (fromMaybe 0 <$> o .:? "when")

-- | One session's (current) relationship with a worktree.  A session has at
-- most one claim per worktree — registering again replaces it.
data SessionClaim = SessionClaim
  { scSession :: Maybe Text  -- ^ 'Nothing' for claims leksah makes before a
                             --   session exists (the queue's launch moment)
  , scRole    :: ClaimRole
  , scNote    :: Text        -- ^ the relationship, in the claimant's words
  , scBranch  :: Maybe Text  -- ^ the branch at claim time (context, not truth)
  , scWhen    :: Double      -- ^ POSIX seconds
  } deriving (Eq, Show)

instance ToJSON SessionClaim where
  toJSON c = object
    [ "session" .= scSession c, "role" .= roleText (scRole c)
    , "note" .= scNote c, "branch" .= scBranch c, "when" .= scWhen c ]

instance FromJSON SessionClaim where
  parseJSON = withObject "SessionClaim" $ \o -> SessionClaim
    <$> o .:? "session"
    <*> (fromMaybe RoleWorking . (>>= parseRole) <$> o .:? "role")
    <*> (fromMaybe "" <$> o .:? "note")
    <*> o .:? "branch" <*> (fromMaybe 0 <$> o .:? "when")

-- | Everything remembered about one worktree.
data WorktreeInfo = WorktreeInfo
  { wiMainRoot :: FilePath        -- ^ the main checkout's root
  , wiPath     :: FilePath        -- ^ the worktree's path — the key
  , wiBranches :: [BranchEvent]   -- ^ newest first, capped
  , wiClaims   :: [SessionClaim]  -- ^ newest first, one per session
  } deriving (Eq, Show)

instance ToJSON WorktreeInfo where
  toJSON w = object
    [ "mainRoot" .= wiMainRoot w, "path" .= wiPath w
    , "branches" .= wiBranches w, "claims" .= wiClaims w ]

instance FromJSON WorktreeInfo where
  parseJSON = withObject "WorktreeInfo" $ \o -> WorktreeInfo
    <$> (fromMaybe "" <$> o .:? "mainRoot") <*> o .: "path"
    <*> (fromMaybe [] <$> o .:? "branches") <*> (fromMaybe [] <$> o .:? "claims")

-- | The audit log is history, not a transcript — keep it bounded.
branchLogCap :: Int
branchLogCap = 50

--------------------------------------------------------------------------------
-- The store (the agents.json pattern: one MVar, lazily loaded, save-on-write)

{-# NOINLINE regVar #-}
regVar :: MVar (Maybe (Map FilePath WorktreeInfo))
regVar = unsafePerformIO (newMVar Nothing)

regPath :: IO FilePath
regPath = sidecarPath "worktrees.json"

loadRegistry :: IO (Map FilePath WorktreeInfo)
loadRegistry = (`catch` \(_ :: SomeException) -> return M.empty) $ do
  p  <- regPath
  ex <- doesFileExist p
  if not ex then return M.empty else do
    ws <- fromMaybe [] . decode' <$> LBS.readFile p
    return $ M.fromList [ (wiPath w, w) | w <- ws ]

saveRegistry :: Map FilePath WorktreeInfo -> IO ()
saveRegistry m = (`catch` \(_ :: SomeException) -> return ()) $ do
  p <- regPath
  LBS.writeFile p (encode (M.elems m))

-- | Update the (lazily loaded) store and persist it.  Vanished local worktrees
-- are pruned here, on the write path — never on a poll's peek.
withRegistry :: (Map FilePath WorktreeInfo -> (Map FilePath WorktreeInfo, a)) -> IO a
withRegistry f = modifyMVar regVar $ \m0 -> do
  m  <- maybe loadRegistry return m0
  m1 <- prune m
  let (m', a) = f m1
  saveRegistry m'
  return (Just m', a)
  where
    prune m = do
      keep <- mapM exists (M.keys m)
      return $ M.fromList [ kv | (kv, True) <- zip (M.toList m) keep ]
    exists p
      | isRemotePath p = return True  -- can't cheaply check; never prune remote
      | otherwise      = doesDirectoryExist p

-- | The store WITHOUT writing it back (polls only look).
peekRegistry :: IO (Map FilePath WorktreeInfo)
peekRegistry = modifyMVar regVar $ \m0 -> do
  m <- maybe loadRegistry return m0
  return (Just m, m)

--------------------------------------------------------------------------------
-- Resolution

-- | Trimmed stdout on success.
gitOut :: FilePath -> [Text] -> IO (Maybe Text)
gitOut dir args = do
  r <- try (runGit dir args)
  return $ case r :: Either SomeException (ExitCode, Text, Text) of
    Right (ExitSuccess, out, _) | not (T.null (T.strip out)) -> Just (T.strip out)
    _ -> Nothing

-- | The MAIN checkout's root, when @dir@ is inside a linked worktree —
-- 'Nothing' for the main checkout itself or a non-checkout.  Same derivation
-- as 'IDE.Web.Worktree.scanReview': a linked worktree's
-- @--git-common-dir@ is @\<main\>\/.git@, not @\<its own root\>\/.git@.
worktreeMainRoot :: FilePath -> IO (Maybe FilePath)
worktreeMainRoot dir = gitOut dir ["rev-parse", "--show-toplevel"] >>= \case
  Nothing   -> return Nothing
  Just root -> gitOut dir ["rev-parse", "--path-format=absolute", "--git-common-dir"] >>= \case
    Just common
      | not (equalFilePath (T.unpack common) (T.unpack root </> ".git"))
      -> return (Just (takeDirectory (dropTrailingPathSeparator (T.unpack common))))
    _ -> return Nothing

-- | The checkout's current branch (short sha when detached).
currentBranch :: FilePath -> IO (Maybe Text)
currentBranch dir = gitOut dir ["rev-parse", "--abbrev-ref", "HEAD"] >>= \case
  Just "HEAD" -> gitOut dir ["rev-parse", "--short", "HEAD"]
  r           -> return r

--------------------------------------------------------------------------------
-- Writing

-- | Register a relationship with the worktree at @path@: upsert the caller's
-- claim, and append a 'BranchEvent' when the branch differs from the last one
-- recorded.  Branch and main root are resolved from git when not supplied.
-- A path that is not a linked worktree is refused (with a readable reply, not
-- an exception) — callers like the PostToolUse hook stay dumb and just try.
registerWorktree
  :: FilePath        -- ^ the worktree
  -> Maybe Text      -- ^ branch (resolved live when 'Nothing')
  -> Maybe Text      -- ^ session id ('Nothing' = leksah itself)
  -> Maybe ClaimRole -- ^ default 'RoleWorking'
  -> Text            -- ^ the relationship, in the claimant's words
  -> Text            -- ^ via: @leksah@ | @agent@ | @hook@ | @scan@
  -> IO Text
registerWorktree path0 mbranch msession mrole note via = do
  let path = dropTrailingPathSeparator path0
  worktreeMainRoot path >>= \case
    Nothing -> return $ "not a linked git worktree: " <> T.pack path
               <> " (nothing registered)"
    Just mainRoot -> do
      branch <- maybe (fromMaybe "" <$> currentBranch path) (return . T.strip) mbranch
      now    <- realToFrac <$> getPOSIXTime
      let role  = fromMaybe RoleWorking mrole
          claim = SessionClaim msession role note
                    (if T.null branch then Nothing else Just branch) now
      withRegistry $ \m ->
        let w0 = fromMaybe (WorktreeInfo mainRoot path [] []) (M.lookup path m)
            -- Repair the main root if an older entry recorded "" (or the
            -- checkout moved); harmless otherwise.
            w1 = w0 { wiMainRoot = mainRoot }
            w2 = if T.null branch || (beBranch <$> listToMaybe (wiBranches w1)) == Just branch
                   then w1
                   else w1 { wiBranches = take branchLogCap
                               (BranchEvent branch msession via now : wiBranches w1) }
            w3 = w2 { wiClaims = claim : filter ((/= msession) . scSession) (wiClaims w2) }
            reply = "registered " <> T.pack (takeFileName path)
                 <> (if T.null branch then "" else " [" <> branch <> "]")
                 <> " as " <> roleText role
                 <> maybe " (no session)" (\s -> " for session " <> T.take 8 s) msession
        in (M.insert path w3 m, reply)

-- | Append a branch move WITHOUT touching claims — the PostToolUse hook's
-- @git switch@\/@git checkout@ path, where clobbering a session's carefully
-- written claim note with \"branch switched (hook)\" would be a loss.  The
-- branch is read live from git when not supplied (the hook runs after the
-- switch, so live is correct); already-latest and non-worktree paths no-op.
recordBranchMove :: FilePath -> Maybe Text -> Maybe Text -> Text -> IO Text
recordBranchMove path0 mbranch msession via = do
  let path = dropTrailingPathSeparator path0
  worktreeMainRoot path >>= \case
    Nothing       -> return $ "not a linked git worktree: " <> T.pack path
                     <> " (nothing recorded)"
    Just mainRoot -> do
      branch <- maybe (fromMaybe "" <$> currentBranch path) (return . T.strip) mbranch
      if T.null branch then return "no branch to record" else do
        now <- realToFrac <$> getPOSIXTime
        withRegistry $ \m ->
          let w0 = fromMaybe (WorktreeInfo mainRoot path [] []) (M.lookup path m)
              same = (beBranch <$> listToMaybe (wiBranches w0)) == Just branch
              w1 = if same then w0
                   else w0 { wiBranches = take branchLogCap
                               (BranchEvent branch msession via now : wiBranches w0) }
              reply | same = "branch unchanged (" <> branch <> ")"
                    | otherwise = "recorded " <> T.pack (takeFileName path)
                                  <> " -> " <> branch
          in (M.insert path w1 m, reply)

--------------------------------------------------------------------------------
-- Reading

-- | Everything registered under one main checkout, keyed by worktree path.
worktreesForRoot :: FilePath -> IO (Map FilePath WorktreeInfo)
worktreesForRoot root0 = do
  let root = dropTrailingPathSeparator root0
  M.filter (equalFilePath root . wiMainRoot) <$> peekRegistry

-- | One worktree's record, if any.
worktreeInfo :: FilePath -> IO (Maybe WorktreeInfo)
worktreeInfo path = M.lookup (dropTrailingPathSeparator path) <$> peekRegistry

-- | Every claim a session holds, newest first — the Agents pane's view.
claimsBySession :: Text -> IO [(WorktreeInfo, SessionClaim)]
claimsBySession sid = do
  m <- peekRegistry
  return $ sortOn (Down . scWhen . snd)
    [ (w, c) | w <- M.elems m, c <- wiClaims w, scSession c == Just sid ]

-- | The session that most plausibly "owns" a worktree right now: the newest
-- created\/working claim that names a session.
owningSession :: FilePath -> IO (Maybe Text)
owningSession path = do
  mw <- worktreeInfo path
  return $ do
    w <- mw
    listToMaybe $ mapMaybe scSession
      [ c | c <- sortOn (Down . scWhen) (wiClaims w)
          , scRole c == RoleCreated || scRole c == RoleWorking ]
