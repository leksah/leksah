{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  IDE.Web.Worktree
--
-- The worktree-per-Claude-session lifecycle (Claude plan Stage 2): create an
-- isolated git worktree + branch for an agent task, resolve what a "review"
-- of such a checkout should diff against, and the merge \/ push-PR \/ archive
-- actions the Review pane offers.  All plain 'IO', run off the reflex frame
-- thread by callers.
--
-- Also hosts the two request bridges ("drop a token, Main drains it" — the
-- 'IDE.Web.GitLogRequest' pattern): tree context menus ask for the
-- new-worktree dialog or a Review tab from plain 'IO' click handlers.
--
-- Layout: worktrees live in @\<repo root\>\/.worktrees\/\<slug\>@ on branch
-- @claude\/\<slug\>@; the branch it forked from is recorded in
-- @branch.claude\/\<slug\>.leksah-base@ (the repo's local git config) so the
-- review diff has a stable base even after the main checkout moves on.
-----------------------------------------------------------------------------
module IDE.Web.Worktree
  ( -- * Create
    slugify
  , newClaudeWorktree
  , newClaudeWorktreeUnique
    -- * Review target resolution
  , ReviewInfo(..)
  , scanReview
    -- * Actions
  , worktreeMerge
  , worktreePushPR
  , worktreeArchive
    -- * Request bridges
  , requestNewWorktree
  , nextNewWorktreeRequest
  , requestReview
  , nextReviewRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Exception (try, SomeException)
import Control.Monad (unless)
import Data.Char (isAlphaNum, isAscii, toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), dropTrailingPathSeparator, takeDirectory,
                        equalFilePath)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readCreateProcessWithExitCode, proc, cwd)

import IDE.Git (runGit)
import IDE.Web.WorktreeRegistry (ClaimRole(..), registerWorktree)

--------------------------------------------------------------------------------
-- Small git helpers (Either-returning wrappers over 'runGit')

-- | Run git and return the trimmed stdout, or the (trimmed) stderr as the error.
git :: FilePath -> [Text] -> IO (Either Text Text)
git dir args = do
  r <- try (runGit dir args)
  return $ case r :: Either SomeException (ExitCode, Text, Text) of
    Right (ExitSuccess, out, _)  -> Right (T.strip out)
    Right (_, _, err)            -> Left (T.strip err)
    Left e                       -> Left (T.pack (show e))

-- | Like 'git' but the caller only cares about success.
git_ :: FilePath -> [Text] -> IO (Either Text ())
git_ dir args = fmap (() <$) (git dir args)

-- | The repo root containing @dir@ ('Left' when it isn't a git checkout).
gitRoot :: FilePath -> IO (Either Text FilePath)
gitRoot dir = fmap T.unpack <$> git dir ["rev-parse", "--show-toplevel"]

-- | The current branch name; 'Left' on a detached HEAD.
gitBranch :: FilePath -> IO (Either Text Text)
gitBranch dir = git dir ["rev-parse", "--abbrev-ref", "HEAD"] >>= \case
  Right "HEAD" -> return (Left "detached HEAD")
  r            -> return r

--------------------------------------------------------------------------------
-- Create

-- | Turn a free-form task name into a branch\/directory slug: lowercased,
-- non-alphanumerics collapsed to single dashes.
slugify :: Text -> Text
slugify = T.dropAround (== '-')
        . T.intercalate "-" . filter (not . T.null) . T.split (== '-')
        . T.map (\c -> if isAscii c && isAlphaNum c then toLower c else '-')

-- | Create @\<root\>\/.worktrees\/\<slug\>@ on a fresh branch @claude\/\<slug\>@
-- forked from the current HEAD, recording the current branch as the review
-- base.  Returns the new worktree's path and branch.
newClaudeWorktree :: FilePath -> Text -> IO (Either Text (FilePath, Text))
newClaudeWorktree dir taskName = do
  let slug = slugify taskName
  if T.null slug then return (Left "please give the task a name") else
    gitRoot dir >>= \case
      Left _     -> return (Left ("not a git checkout: " <> T.pack dir))
      Right root -> do
        let branch = "claude/" <> slug
            wtPath = root </> ".worktrees" </> T.unpack slug
        exists <- doesDirectoryExist wtPath
        branchTaken <- either (const False) (const True)
            <$> git root ["rev-parse", "--verify", "--quiet", "refs/heads/" <> branch]
        if exists || branchTaken
          then return (Left ("a worktree or branch for “" <> slug <> "” already exists"))
          else do
            -- The review base: the branch we fork from (detached HEAD → its sha).
            base <- gitBranch root >>= \case
              Right b -> return b
              Left _  -> fromMaybe "HEAD" . rightToMaybe
                           <$> git root ["rev-parse", "--short", "HEAD"]
            excludeWorktreesDir root
            git root ["worktree", "add", T.pack wtPath, "-b", branch] >>= \case
              Left err -> return (Left err)
              Right _  -> do
                -- Best-effort: a failed config write only loses the recorded
                -- base (scanReview falls back to origin/HEAD).
                _ <- git_ root ["config", "branch." <> branch <> ".leksah-base", base]
                _ <- registerWorktree wtPath (Just branch) Nothing (Just RoleCreated)
                       ("created by leksah, forked from " <> base) "leksah"
                return (Right (wtPath, branch))
  where rightToMaybe = either (const Nothing) Just

-- | Like 'newClaudeWorktree', but a taken name is not an error: try the task
-- name itself, then @name-2@, @name-3@, … until one is free.  This is the
-- queue's (and compare-N-approaches') path — N tasks queued from the same
-- prompt land in @slug@, @slug-2@, … instead of failing after the first.
-- Detection keys on "already exists", which covers both our own pre-check
-- message above and git's own "a branch named '…' already exists" (two
-- concurrent creations racing past the pre-check).
newClaudeWorktreeUnique :: FilePath -> Text -> IO (Either Text (FilePath, Text))
newClaudeWorktreeUnique dir taskName = go (1 :: Int)
  where
    -- Queued tasks arrive as whole PROMPTS, not names — cap the slug so the
    -- worktree/branch stays readable.  Capped BEFORE the -N suffix goes on,
    -- so the suffix can never be truncated back into a collision.
    base = T.dropWhileEnd (== '-') (T.take 40 (slugify taskName))
    go n
      | n > 99 = return (Left ("no free worktree name for “" <> base <> "”"))
      | otherwise = do
          let name = if n == 1 then base
                               else base <> "-" <> T.pack (show n)
          newClaudeWorktree dir name >>= \case
            Left err | "already exists" `T.isInfixOf` err -> go (n + 1)
            r -> return r

-- | Make sure @.worktrees/@ never shows up as untracked: append it to the
-- repo's local (untracked, per-clone) @info\/exclude@.  Best-effort.
excludeWorktreesDir :: FilePath -> IO ()
excludeWorktreesDir root = do
  r <- git root ["rev-parse", "--path-format=absolute", "--git-common-dir"]
  case r of
    Left _ -> return ()
    Right common -> do
      let f = T.unpack common </> "info" </> "exclude"
      old <- either (\(_ :: SomeException) -> "") id <$> try (readFile f)
      unless (".worktrees/" `elem` lines old) $ do
        _ <- try (appendFile f "\n.worktrees/\n")
               :: IO (Either SomeException ())
        return ()

--------------------------------------------------------------------------------
-- Review target resolution

-- | What the Review pane needs to know about a checkout.
data ReviewInfo = ReviewInfo
  { riRoot      :: FilePath        -- ^ the checkout's root
  , riBranch    :: Text            -- ^ its current branch (or short sha)
  , riBase      :: Text            -- ^ the ref reviewed against
  , riMergeBase :: Text            -- ^ @merge-base riBase HEAD@ (the diff's left side)
  , riMainRoot  :: Maybe FilePath  -- ^ the main checkout, when this is a linked worktree
  } deriving (Eq, Show)

-- | Resolve the review target for the checkout at @dir@.  The base is, in
-- order: the recorded @branch.\<branch\>.leksah-base@; the branch's upstream;
-- @origin\/HEAD@ (the default branch); a local @master@\/@main@.
scanReview :: FilePath -> IO (Either Text ReviewInfo)
scanReview dir = gitRoot dir >>= \case
  Left _     -> return (Left ("not a git checkout: " <> T.pack dir))
  Right root -> do
    branch <- either (const "") id <$> gitBranch root
    branch' <- if T.null branch
                 then either (const "HEAD") id <$> git root ["rev-parse", "--short", "HEAD"]
                 else return branch
    mbase <- firstRight
      [ git root ["config", "branch." <> branch' <> ".leksah-base"]
      , git root ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"]
      , fmap (T.replace "refs/remotes/" "")
          <$> git root ["symbolic-ref", "refs/remotes/origin/HEAD"]
      , fmap (const "master")
          <$> git root ["rev-parse", "--verify", "--quiet", "refs/heads/master"]
      , fmap (const "main")
          <$> git root ["rev-parse", "--verify", "--quiet", "refs/heads/main"]
      ]
    case mbase of
      Nothing   -> return (Left "no review base found (no recorded base, upstream, origin/HEAD, master or main)")
      Just base -> git root ["merge-base", base, "HEAD"] >>= \case
        Left err -> return (Left ("merge-base " <> base <> " failed: " <> err))
        Right mb -> do
          common <- git root ["rev-parse", "--path-format=absolute", "--git-common-dir"]
          let mainRoot = case common of
                Right c | not (equalFilePath (T.unpack c) (root </> ".git"))
                  -> Just (takeDirectory (dropTrailingPathSeparator (T.unpack c)))
                _ -> Nothing
          return (Right (ReviewInfo root branch' base mb mainRoot))
  where
    firstRight [] = return Nothing
    firstRight (act : rest) = act >>= \case
      Right v | not (T.null v) -> return (Just v)
      _                        -> firstRight rest

--------------------------------------------------------------------------------
-- Actions

-- | Merge the worktree's branch into its base, in the MAIN checkout — which
-- must currently be ON the base branch (we refuse to guess otherwise).
-- Returns git's message on success.
worktreeMerge :: ReviewInfo -> IO (Either Text Text)
worktreeMerge ri = case riMainRoot ri of
  Nothing   -> return (Left "not a linked worktree — merge it with git directly")
  Just main -> gitBranch main >>= \case
    Left err -> return (Left ("main checkout: " <> err))
    Right cur
      | cur /= riBase ri ->
          return (Left ("main checkout is on “" <> cur <> "”, not “" <> riBase ri
                        <> "” — check it out first"))
      | otherwise -> do
          r <- git main ["merge", "--no-ff", riBranch ri,
                         "-m", "Merge " <> riBranch ri]
          return $ case r of
            Right out -> Right (if T.null out then "merged " <> riBranch ri else out)
            Left err  -> Left err

-- | Push the branch to @origin@ and open a PR via the @gh@ CLI.  Returns the
-- PR URL (also handed to the caller to open).
worktreePushPR :: ReviewInfo -> IO (Either Text Text)
worktreePushPR ri = git_ (riRoot ri) ["push", "-u", "origin", riBranch ri] >>= \case
  Left err -> return (Left ("push failed: " <> err))
  Right () -> do
    r <- try (readCreateProcessWithExitCode
                (proc "gh" ["pr", "create", "--fill", "--head", T.unpack (riBranch ri)])
                  { cwd = Just (riRoot ri) } "")
    return $ case r :: Either SomeException (ExitCode, String, String) of
      Right (ExitSuccess, out, _) ->
        case [ l | l <- T.lines (T.pack out), "https://" `T.isInfixOf` l ] of
          (u : _) -> Right (T.strip u)
          []      -> Right ("pushed " <> riBranch ri <> " (no PR URL from gh)")
      Right (_, out, err) -> Left (T.strip (T.pack err <> "\n" <> T.pack out))
      Left e              -> Left (T.pack (show e))

-- | Remove the worktree and delete its branch (forced — the caller confirms).
worktreeArchive :: ReviewInfo -> IO (Either Text ())
worktreeArchive ri = case riMainRoot ri of
  Nothing   -> return (Left "not a linked worktree")
  Just main -> git_ main ["worktree", "remove", "--force", T.pack (riRoot ri)] >>= \case
    Left err -> return (Left err)
    Right () -> do
      -- Branch deletion is best-effort (it may be checked out elsewhere or
      -- already gone); the worktree removal is the destructive part.
      _ <- git_ main ["branch", "-D", riBranch ri]
      return (Right ())

--------------------------------------------------------------------------------
-- Request bridges

{-# NOINLINE newWorktreeChan #-}
newWorktreeChan :: Chan FilePath
newWorktreeChan = unsafePerformIO newChan

-- | Ask "IDE.Web.Main" to show the new-worktree-session dialog for a directory
-- (any directory inside the repo; the dialog resolves the root).
requestNewWorktree :: FilePath -> IO ()
requestNewWorktree = writeChan newWorktreeChan

-- | Block until the next request (drained by the reflex bridge in Main).
nextNewWorktreeRequest :: IO FilePath
nextNewWorktreeRequest = readChan newWorktreeChan

{-# NOINLINE reviewChan #-}
reviewChan :: Chan FilePath
reviewChan = unsafePerformIO newChan

-- | Ask "IDE.Web.Main" to open a Review tab for the checkout at a directory.
requestReview :: FilePath -> IO ()
requestReview = writeChan reviewChan

-- | Block until the next request (drained by the reflex bridge in Main).
nextReviewRequest :: IO FilePath
nextReviewRequest = readChan reviewChan
