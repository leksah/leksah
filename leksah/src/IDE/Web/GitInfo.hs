{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  IDE.Web.GitInfo
--
-- Shared git / GitHub summary helpers used by both the workspace tree
-- (branch + PR next to a git node, "Open PR" menu items, collapsed-project
-- summaries) and the status bar (active project's branch / PR / change totals).
--
-- Self-contained (depends only on 'IDE.Git' and the refresh buses) so the
-- status bar and the tree can both use it without either depending on the other.
-- A few small leaf helpers here intentionally mirror ones in
-- 'IDE.Web.Widget.Workspace' (which predates this module and keeps its own for
-- the rest of the tree); the shared entry points new code should use are
-- 'prForBranch', 'scanActiveGitInfo' and 'openUrl'.
--
-----------------------------------------------------------------------------
module IDE.Web.GitInfo (
    openUrl
,   registerGitRefresh
,   gitCurrentBranch
,   gitOriginUrl
,   parseGitHub
,   gitUpstreamBranch
,   ghPullForHead
,   GhPull(..)
,   prForBranch
,   gitNumstat
,   ActiveGitInfo(..)
,   emptyGitInfo
,   scanActiveGitInfo
) where

import Control.Concurrent.MVar
       (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception (catch, try, SomeException)
import Control.Monad (void, when)
import Data.Aeson (FromJSON(..), withObject, (.:), eitherDecodeStrict)
import Data.List (isPrefixOf, foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import qualified Data.Map as M
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath (dropTrailingPathSeparator)
import System.Info (os)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode, createProcess, proc)
import Text.Read (readMaybe)

import IDE.Git (runGit)
import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.LocalRefresh (registerLocalRefresh)
import IDE.Web.RemoteRefresh (registerRemoteRefresh)

-- | Open a URL in the system browser (macOS @open@ / else @xdg-open@).
openUrl :: Text -> IO ()
openUrl url = do
    _ <- (try (void $ createProcess (proc opener [T.unpack url]))
            :: IO (Either SomeException ()))
    return ()
  where opener = if os == "darwin" then "open" else "xdg-open"

-- | Subscribe to git refreshes for @dir@: for a local checkout, filesystem
-- events under @dir@; for a remote one, the remote-refresh bus.
registerGitRefresh :: FilePath -> IO () -> IO ()
registerGitRefresh dir act
    | isRemotePath dir = void $ registerRemoteRefresh (const act)
    | otherwise        = void $ registerLocalRefresh $ \p ->
          when ((dropTrailingPathSeparator dir <> "/") `isPrefixOf` p) act

-- | The current branch name (@Nothing@ on a detached HEAD or non-repo).
gitCurrentBranch :: FilePath -> IO (Maybe Text)
gitCurrentBranch dir = do
    r <- try (runGit dir ["rev-parse", "--abbrev-ref", "HEAD"])
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _) | b <- T.strip out, not (T.null b) -> Just b
        _ -> Nothing

-- | The checkout's @remote.origin.url@ (if any).
gitOriginUrl :: FilePath -> IO (Maybe Text)
gitOriginUrl dir = do
    r <- try (runGit dir ["config", "--get", "remote.origin.url"])
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _) | u <- T.strip out, not (T.null u) -> Just u
        _ -> Nothing

-- | Parse a github.com remote URL into @(owner, repo)@ — handling the
-- @git\@github.com:owner\/repo(.git)@, @https:\/\/github.com\/owner\/repo(.git)@
-- and @ssh:\/\/git\@github.com\/owner\/repo(.git)@ forms.
parseGitHub :: Text -> Maybe (Text, Text)
parseGitHub raw =
    case T.breakOn "github.com" (fromMaybe u (T.stripSuffix ".git" u)) of
        (_, rest)
          | not (T.null rest)
          , path <- T.dropWhile (`elem` (":/" :: String)) (T.drop (T.length "github.com") rest)
          , (owner : repo : _) <- T.splitOn "/" path
          , not (T.null owner), not (T.null repo) -> Just (owner, repo)
        _ -> Nothing
  where u = T.strip raw

-- | The upstream (tracking) branch's head ref for the current branch — e.g.
-- @origin/foo@ → @foo@.  'Nothing' when there is no upstream configured.
gitUpstreamBranch :: FilePath -> IO (Maybe Text)
gitUpstreamBranch dir = do
    r <- try (runGit dir ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"])
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _)
          | u <- T.strip out, not (T.null u)
          , let (_remote, rest) = T.breakOn "/" u
          , h <- T.drop 1 rest, not (T.null h) -> Just h
        _ -> Nothing

newtype GhPull = GhPull { ghpNumber :: Int }
instance FromJSON GhPull where
    parseJSON = withObject "GhPull" $ \o -> GhPull <$> o .: "number"

-- | The number of the open PR whose head branch is @headRef@ (same-repo PRs),
-- via the @pulls@ REST endpoint filtered by @head=owner:branch@.  Uses
-- @$GITHUB_TOKEN@ when set.  Any failure → 'Nothing'.
ghPullForHead :: (Text, Text) -> Text -> IO (Maybe Int)
ghPullForHead (owner, repo) headRef = do
    tok <- lookupEnv "GITHUB_TOKEN"
    let url  = "https://api.github.com/repos/" <> owner <> "/" <> repo
                 <> "/pulls?state=open&head=" <> owner <> ":" <> headRef
        auth = maybe [] (\t -> ["-H", "Authorization: Bearer " <> t]) tok
        args = [ "-s", "-H", "Accept: application/vnd.github+json"
               , "-H", "User-Agent: leksah" ] <> auth <> [T.unpack url]
    r <- try (readProcessWithExitCode "curl" args "")
    return $ case r :: Either SomeException (ExitCode, String, String) of
        Right (ExitSuccess, out, _) ->
            case eitherDecodeStrict (encodeUtf8 (T.pack out)) of
                Right (p : _) -> Just (ghpNumber p)
                _             -> Nothing
        _ -> Nothing

-- gh CLI JSON for @gh pr view … --json number,url@.
data GhCli = GhCli { gcNumber :: Int, gcUrl :: Text }
instance FromJSON GhCli where
    parseJSON = withObject "GhCli" $ \o -> GhCli <$> o .: "number" <*> o .: "url"

-- | Resolve the open PR for @dir@'s current/upstream branch as @(number, url)@.
-- Prefers the @gh@ CLI when it is on PATH (and authenticated); otherwise falls
-- back to the curl + @$GITHUB_TOKEN@ path ('ghPullForHead'), constructing the
-- URL.  Non-github origins, no PR, or any failure → 'Nothing'.  Results are
-- cached briefly (per repo+branch) so the tree, project node and status bar
-- don't each issue their own network call on every refresh.
prForBranch :: FilePath -> IO (Maybe (Int, Text))
prForBranch dir = gitOriginUrl dir >>= \mu -> case mu >>= parseGitHub of
    Nothing   -> return Nothing
    Just repo -> do
        mhead <- gitUpstreamBranch dir >>= \case
            Just h  -> return (Just h)
            Nothing -> gitCurrentBranch dir
        case mhead of
            Nothing -> return Nothing
            Just br -> cachedPr repo br (resolve repo br)
  where
    resolve repo@(owner, name) br = ghPrViaCli repo br >>= \case
        Just x  -> return (Just x)
        Nothing -> fmap (\n -> (n, prUrl owner name n)) <$> ghPullForHead repo br
    prUrl o r n = "https://github.com/" <> o <> "/" <> r <> "/pull/" <> T.pack (show n)

-- | Try @gh pr view <branch> --repo owner/name --json number,url@.  Requires the
-- @gh@ binary on PATH; any failure (absent, unauthenticated, no PR) → 'Nothing'.
ghPrViaCli :: (Text, Text) -> Text -> IO (Maybe (Int, Text))
ghPrViaCli (owner, name) br = findExecutable "gh" >>= \case
    Nothing -> return Nothing
    Just _  -> do
        let args = [ "pr", "view", T.unpack br
                   , "--repo", T.unpack (owner <> "/" <> name)
                   , "--json", "number,url" ]
        r <- try (readProcessWithExitCode "gh" args "")
        return $ case r :: Either SomeException (ExitCode, String, String) of
            Right (ExitSuccess, out, _) ->
                case eitherDecodeStrict (encodeUtf8 (T.pack out)) of
                    Right c -> Just (gcNumber c, gcUrl c)
                    _       -> Nothing
            _ -> Nothing

-- A short-TTL cache keyed by (owner, repo, branch).  A PR lookup is a network
-- round trip and several widgets ask for the same repo on every refresh, so
-- an entry is either a fresh ANSWER or the FETCH that is currently getting
-- one: callers that arrive while a request is in flight wait for it instead
-- of firing their own (which used to send four identical GitHub requests in
-- the same second, for nothing but rate limit).
data PrEntry
    = PrFresh POSIXTime (Maybe (Int, Text))
    | PrFetching (MVar (Maybe (Int, Text)))

{-# NOINLINE prCache #-}
prCache :: IORef (M.Map (Text, Text, Text) PrEntry)
prCache = unsafePerformIO (newIORef M.empty)

cachedPr :: (Text, Text) -> Text -> IO (Maybe (Int, Text)) -> IO (Maybe (Int, Text))
cachedPr (owner, repo) br fetch = do
    now <- getPOSIXTime
    slot <- newEmptyMVar
    let key = (owner, repo, br)
    -- One atomic decision per caller: use the fresh answer, wait on the
    -- in-flight fetch, or become the fetcher.
    action <- atomicModifyIORef' prCache $ \m -> case M.lookup key m of
        Just (PrFresh t v) | now - t < ttl -> (m, Right v)
        Just (PrFetching wait)             -> (m, Left (Left wait))
        _ -> (M.insert key (PrFetching slot) m, Left (Right slot))
    case action of
        Right v            -> return v
        Left (Left wait)   -> readMVar wait
        Left (Right mine)  -> do
            v <- fetch `catch` \(_ :: SomeException) -> return Nothing
            atomicModifyIORef' prCache $ \m -> (M.insert key (PrFresh now v) m, ())
            -- Release everyone who queued behind this fetch.
            putMVar mine v
            return v
  where ttl = 30  -- seconds

-- | Sum a @git diff --numstat@ into @(insertions, deletions)@ (binary rows,
-- shown as @-@, are skipped).  @extra@ selects the range, e.g. @["HEAD"]@ for
-- all uncommitted changes or @["\@{upstream}..HEAD"]@ for commits ahead.
gitNumstat :: FilePath -> [Text] -> IO (Int, Int)
gitNumstat dir extra = do
    r <- try (runGit dir (["diff", "--numstat"] <> extra))
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _) -> foldl' acc (0, 0)
            [ (a, d)
            | l <- T.lines out
            , (af : df : _) <- [T.splitOn "\t" l]
            , Just a <- [rd af], Just d <- [rd df] ]
        _ -> (0, 0)
  where
    rd t = readMaybe (T.unpack (T.strip t)) :: Maybe Int
    acc (a, d) (a', d') = (a + a', d + d')

-- | A one-shot git summary of a checkout for the status bar.
data ActiveGitInfo = ActiveGitInfo
    { agiBranch :: Maybe Text            -- ^ current branch
    , agiPr     :: Maybe (Int, Text)     -- ^ open PR (number, url)
    , agiWork   :: (Int, Int)            -- ^ uncommitted working-tree +/-
    , agiAhead  :: (Int, Int)            -- ^ committed-ahead-of-upstream +/-
    } deriving (Eq)

emptyGitInfo :: ActiveGitInfo
emptyGitInfo = ActiveGitInfo Nothing Nothing (0, 0) (0, 0)

-- | Scan @dir@ for the status-bar summary.  Not a git checkout ⇒ branch
-- 'Nothing' and zero counts (so the section hides).
scanActiveGitInfo :: FilePath -> IO ActiveGitInfo
scanActiveGitInfo dir = do
    isGit <- gitCurrentBranch dir
    case isGit of
        Nothing -> return emptyGitInfo
        Just _  -> ActiveGitInfo isGit
            <$> prForBranch dir
            <*> gitNumstat dir ["HEAD"]
            <*> gitNumstat dir ["@{upstream}..HEAD"]
