{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Claude Code integration: discover the CLI, enumerate a directory's saved
-- sessions, and launch @claude@ in a terminal.
--
-- Claude Code stores each conversation as a JSONL transcript under
-- @~\/.claude\/projects\/<encoded-cwd>\/<session-id>.jsonl@, where the encoding
-- replaces every non-alphanumeric character of the absolute working directory
-- with @-@ (so @\/Users\/me\/haskell.nix@ → @-Users-me-haskell-nix@).  The
-- encoding is lossy, so after locating the folder we re-read each transcript's
-- @cwd@ (cheap — it's on the first user line) to drop any collisions.  The
-- filename (a UUID) is the session id passed to @claude --resume@.
--
-- This module is FS/process only (no reflex); the tree widget lives in
-- "IDE.Web.Widget.FileTree".
module IDE.Web.Claude
  ( claudeAvailable
  , ClaudeSession(..)
  , claudeSessionsFor
  , ClaudeCmd(..)
  , runClaudeCmd
  , claudeRunning
  , copySessionId
  , revealSession
  , deleteSession
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (catch, SomeException)
import Control.Monad (void, forM, mfilter)

import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Ord (Down(..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
       (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)

import Data.Aeson (Value(..), decodeStrict', withObject, (.:))
import Data.Aeson.Types (parseMaybe, FromJSON)
import qualified Data.ByteString.Char8 as BS

import System.Directory
       (findExecutable, getHomeDirectory, doesDirectoryExist,
        listDirectory, getModificationTime, removeFile)
import System.FilePath
       ((</>), takeExtension, takeBaseName, takeDirectory, takeFileName,
        dropTrailingPathSeparator)
import System.Info (os)
import System.IO (withFile, IOMode(ReadMode), hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (createProcess, proc, readProcess)

import IDE.Web.ReplTmux
       (ensureCommandWindow, clipboardCopyCmd, cmdPrefixForDir, liveRunKeys)
import IDE.Web.RemoteTermRequest (requestLocalTerm)

-- | A saved Claude Code session for some directory.
data ClaudeSession = ClaudeSession
  { csId       :: Text      -- ^ session id (the transcript's UUID filename)
  , csPath     :: FilePath  -- ^ absolute path to the @.jsonl@ transcript
  , csModified :: UTCTime   -- ^ transcript mtime (used for MRU ordering)
  , csAge      :: Text      -- ^ human-friendly age, e.g. @"3h ago"@
  , csLabel    :: Text      -- ^ first user prompt (best-effort), for the row
  }

-- Whether @claude@ is on PATH, resolved once and cached (a benign race just
-- re-checks).  The whole feature is gated on this.
{-# NOINLINE claudeAvailableRef #-}
claudeAvailableRef :: IORef (Maybe Bool)
claudeAvailableRef = unsafePerformIO (newIORef Nothing)

claudeAvailable :: IO Bool
claudeAvailable = readIORef claudeAvailableRef >>= \case
  Just b  -> return b
  Nothing -> do
    b <- maybe False (const True) <$> findExecutable "claude"
    writeIORef claudeAvailableRef (Just b)
    return b

-- | Encode an absolute directory the way Claude Code names its project folder:
-- every non-alphanumeric character becomes @-@.
encodeClaudeDir :: FilePath -> FilePath
encodeClaudeDir = map (\c -> if isAlphaNum c then c else '-') . dropTrailingPathSeparator

-- | The saved sessions whose working directory is @dir@, most-recently-used
-- first.  @[]@ on any error (no folder, unreadable, …) so the node just hides.
claudeSessionsFor :: FilePath -> IO [ClaudeSession]
claudeSessionsFor dir = (`catch` \(_ :: SomeException) -> return []) $ do
  home <- getHomeDirectory
  let target = dropTrailingPathSeparator dir
      pdir   = home </> ".claude" </> "projects" </> encodeClaudeDir target
  ex <- doesDirectoryExist pdir
  if not ex then return [] else do
    names <- listDirectory pdir
    now   <- getCurrentTime
    let transcripts = [ pdir </> n | n <- names, takeExtension n == ".jsonl" ]
    fmap (sortOn (Down . csModified) . mapMaybe id) . forM transcripts $ \f ->
      (`catch` \(_ :: SomeException) -> return Nothing) $ do
        mt <- getModificationTime f
        (mcwd, mlabel) <- scanHead f
        -- The encoded folder is lossy; keep only transcripts that actually ran
        -- in this directory (or whose cwd we couldn't read).
        let matches = maybe True ((== target) . dropTrailingPathSeparator . T.unpack) mcwd
        return $ if not matches then Nothing else Just ClaudeSession
          { csId       = T.pack (takeBaseName f)
          , csPath     = f
          , csModified = mt
          , csAge      = humanAge (diffUTCTime now mt)
          , csLabel    = maybe "(untitled session)" id mlabel
          }

-- | Read a transcript's head for @(cwd, first-user-prompt)@, stopping as soon as
-- both are found (or after a small line budget) so we never read a huge file.
scanHead :: FilePath -> IO (Maybe Text, Maybe Text)
scanHead f = withFile f ReadMode $ \h -> go h (120 :: Int) Nothing Nothing
  where
    go _ 0 cwd lbl = return (cwd, lbl)
    go h n cwd lbl
      | Just _ <- cwd, Just _ <- lbl = return (cwd, lbl)
      | otherwise = hIsEOF h >>= \case
          True  -> return (cwd, lbl)
          False -> do
            l <- BS.hGetLine h
            let v    = decodeStrict' l :: Maybe Value
                cwd' = cwd `orElse` (v >>= objField "cwd")
                lbl' = lbl `orElse` (v >>= userPrompt)
            go h (n - 1) cwd' lbl'
    orElse a b = maybe b Just a

-- | The user prompt text of a @type:"user"@ entry (message content is a bare
-- string or a list of blocks); 'Nothing' for any other entry.
userPrompt :: Value -> Maybe Text
userPrompt v = do
  ty <- objField "type" v :: Maybe Text
  if ty /= "user" then Nothing else do
    msg     <- objField "message" v :: Maybe Value
    content <- objField "content" msg :: Maybe Value
    firstText content >>= cleanLabel

firstText :: Value -> Maybe Text
firstText (String s) = Just s
firstText (Array a)  = listToMaybe (mapMaybe blockText (toList a))
  where blockText b = case objField "type" b :: Maybe Text of
          Just "text" -> objField "text" b
          _           -> Nothing
firstText _ = Nothing

-- | First meaningful line of a prompt: skip blank / tag-ish lines (slash-command
-- wrappers, @<system-reminder>@ …), then truncate for the tree row.
cleanLabel :: Text -> Maybe Text
cleanLabel raw =
  case filter good (map T.strip (T.lines raw)) of
    (l : _) -> Just (ellipsize 72 l)
    []      -> case T.strip <$> listToMaybe (T.lines raw) of
                 Just l | not (T.null l) -> Just (ellipsize 72 l)
                 _                       -> Nothing
  where
    good l = not (T.null l) && not ("<" `T.isPrefixOf` l)
    ellipsize k t | T.length t > k = T.take (k - 1) t <> "…"
                  | otherwise      = t

-- | Look up @key@ in a JSON object and decode it, version-agnostically across
-- aeson 1/2 (the string literal becomes @Text@ or @Key@ as needed).
objField :: FromJSON a => String -> Value -> Maybe a
objField k = parseMaybe (withObject "o" (\o -> o .: fromString k))

humanAge :: NominalDiffTime -> Text
humanAge d
  | s < 60        = "just now"
  | s < 3600      = tshow (s `div` 60)          <> "m ago"
  | s < 86400     = tshow (s `div` 3600)        <> "h ago"
  | s < 7 * 86400 = tshow (s `div` 86400)       <> "d ago"
  | otherwise     = tshow (s `div` (7 * 86400)) <> "w ago"
  where s = max 0 (round d) :: Int
        tshow = T.pack . show

-- | A @claude@ invocation to run in a terminal for some directory.
data ClaudeCmd
  = ClaudeNew          FilePath        -- ^ @claude@ — a fresh session
  | ClaudeContinue     FilePath        -- ^ @claude -c@ — continue the most recent
  | ClaudeResumePicker FilePath        -- ^ @claude --resume@ — interactive picker
  | ClaudeResume       FilePath Text   -- ^ @claude --resume <id>@
  | ClaudeResumeFork   FilePath Text   -- ^ @claude --resume <id> --fork-session@
  | ClaudeAsk          FilePath        -- ^ @claude "<prompt>"@ — new session
                                       --   seeded to explain the given file

-- | Open (or focus) a terminal in the right directory running the command.
-- Interactive/new sessions share one @claude@ window per directory; a specific
-- resumed session gets its own window keyed by id.  Fire-and-forget.
runClaudeCmd :: ClaudeCmd -> IO ()
runClaudeCmd cmd = void . forkIO $ do
  let d = dropTrailingPathSeparator dir
  -- Launch inside the owning project's command prefix (e.g. @nix develop -c@)
  -- so the tools claude spawns (cabal/ghc/hls) inherit the project environment;
  -- no prefix set → a plain launch.
  mbPrefix <- mfilter (not . T.null) <$> cmdPrefixForDir d
  let line' = maybe line (\p -> p <> " " <> line) mbPrefix
  void $ ensureCommandWindow True key d "claude" line'
           >>= mapM_ requestLocalTerm
  where
    (dir, keyTag, line) = case cmd of
      ClaudeNew d          -> (d, "claude",            "claude")
      ClaudeContinue d     -> (d, "claude",            "claude -c")
      ClaudeResumePicker d -> (d, "claude",            "claude --resume")
      ClaudeResume d i     -> (d, "claude#" <> i,      "claude --resume " <> i)
      ClaudeResumeFork d i -> (d, "claude#fork#" <> i, "claude --resume " <> i <> " --fork-session")
      ClaudeAsk f          -> let b = T.pack (takeFileName f)
                              in ( takeDirectory f
                                 , "claude#ask#" <> b
                                 , "claude " <> shq ("Please explain the file @" <> b
                                     <> " — what it does and how it fits into this project.") )
    key = T.pack (dropTrailingPathSeparator dir) <> "#" <> keyTag
    -- Single-quote for the shell tmux runs the window command through, so a
    -- multi-word seeded prompt reaches @claude@ as one argument.
    shq t = "'" <> T.replace "'" "'\\''" t <> "'"

-- | Is a @claude@ terminal window currently live for @dir@ — the shared
-- interactive window (@dir#claude@) or any resumed/ask window (@dir#claude#…@)?
-- Used to show a "running" marker on the tree node.
claudeRunning :: FilePath -> IO Bool
claudeRunning dir = do
  let base = T.pack (dropTrailingPathSeparator dir) <> "#claude"
  any (\k -> k == base || (base <> "#") `T.isPrefixOf` k) <$> liveRunKeys

-- | Copy a session id to the system clipboard (best-effort).
copySessionId :: Text -> IO ()
copySessionId sid = clipboardCopyCmd >>= \case
  Nothing  -> return ()
  Just ccmd -> void . (`catch` \(_ :: SomeException) -> return "") $
      readProcess "sh" ["-c", ccmd] (T.unpack sid)

-- | Reveal a transcript in the OS file manager.
revealSession :: FilePath -> IO ()
revealSession f = void . (`catch` \(_ :: SomeException) -> return ()) $
  if os == "darwin"
    then void (createProcess (proc "open" ["-R", f]))
    else void (createProcess (proc "xdg-open" [takeDirectory f]))

-- | Delete a session's transcript (the caller should rescan afterwards).
deleteSession :: FilePath -> IO ()
deleteSession f = removeFile f `catch` \(_ :: SomeException) -> return ()
