{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | OpenAI Codex CLI sessions, as a second kind of agent beside Claude Code.
--
-- Codex has hooks shaped like Claude Code's (SessionStart, UserPromptSubmit,
-- PreToolUse, PermissionRequest, PostToolUse, Stop, SessionEnd …, the same
-- JSON on stdin), but nothing like Claude Code's @~\/.claude\/sessions\/\<pid\>.json@:
-- no file says which sessions are running, or what they are doing.  So
-- leksah's hooks keep one for it — @~\/.leksah\/codex-sessions\/\<pid\>.json@, in
-- the same shape — and "IDE.Web.Claude" reads those beside Claude Code's, so a
-- Codex session is one more live session to everything downstream (the status
-- surfaces, the Agents pane, the flipper, pane lookup by process ancestry).
--
-- The hooks are installed once, into Codex's own @hooks.json@
-- ('codexInstall'), and Codex runs them only after the user has trusted them in
-- its @\/hooks@ screen.  The trust is tied to each hook's definition, so the
-- command never changes: it is a wrapper script (@~\/.leksah\/codex-hook@) that
-- leksah rewrites to point at the current @leksah-cmd@.
--
-- This module is FS/process only, and must not import "IDE.Web.Claude" (which
-- imports it).
module IDE.Web.Codex
  ( codexAvailable
  , codexHome
    -- * Live sessions
  , CodexLive
  , HookLive(..)
  , codexLiveRecords
  , codexProcessOf
  , isCodexProgram
  , codexInteractive
  , codexKnownPid
  , codexSessionStart
  , codexClaimLaunch
  , codexRememberLaunch
  , codexSetStatus
  , codexSessionEnd
  , codexPrune
  , codexRecordFor
    -- * Sessions leksah has seen
  , isCodexSession
  , codexTranscriptOf
  , codexSessionDir
  , codexSessionsIn
  , codexStarted
    -- * Installing the hooks
  , codexHookEvents
  , codexInstall
  , codexUninstall
  , codexStatus
  , codexInstalled
  , codexEnsureInstalled
  , codexWriteScripts
  , codexScripts
  , codexHooksWith
  , codexRulesText
    -- * Transcripts (rollouts)
  , isRollout
  , rolloutTurns
  , rolloutTurnsOf
  , rolloutFirstPrompt
    -- * Tool calls
  , codexPatchFiles
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (forM_, when)
import Data.Aeson
       (Value(..), (.=), (.:?), decodeStrict', encode, object, toJSON, withObject)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf, nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
       (createDirectoryIfMissing, doesFileExist, findExecutable,
        getHomeDirectory, getPermissions, removeFile, setPermissions,
        setOwnerExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO
       (IOMode(ReadMode), SeekMode(SeekFromEnd), hFileSize, hIsEOF, hSeek, withFile)
import System.IO.Unsafe (unsafePerformIO)
import IDE.Utils.Spawn (readProcessWithExitCode)
import IDE.Web.HookSessions

-- | Whether @codex@ is on PATH — cached once seen, like 'claudeAvailable'
-- (it can be installed after leksah starts).
{-# NOINLINE codexAvailableRef #-}
codexAvailableRef :: IORef Bool
codexAvailableRef = unsafePerformIO (newIORef False)

codexAvailable :: IO Bool
codexAvailable = readIORef codexAvailableRef >>= \case
  True  -> return True
  False -> do
    found <- isJust <$> findExecutable "codex"
    when found $ writeIORef codexAvailableRef True
    return found

-- | Codex's own directory: @$CODEX_HOME@, else @~\/.codex@.
codexHome :: IO FilePath
codexHome = lookupEnv "CODEX_HOME" >>= \case
  Just d | not (null d) -> return d
  _ -> (</> ".codex") <$> getHomeDirectory

-- | Codex's live records and the sessions its hooks have seen
-- ("IDE.Web.HookSessions": @~\/.leksah\/codex-sessions@, @codex-index.json@).
{-# NOINLINE codexStore #-}
codexStore :: HookStore
codexStore = unsafePerformIO (newHookStore "codex")

--------------------------------------------------------------------------------
-- Live sessions

-- | One running Codex session, as leksah's hooks recorded it.
type CodexLive = HookLive

-- | Every hook-written live record (pid liveness is the reader's business —
-- 'IDE.Web.Claude.claudeLiveBySession' checks it with one @ps@).
codexLiveRecords :: IO [CodexLive]
codexLiveRecords = hookLiveRecords codexStore

-- | The live record for session @sid@, if one is open.
codexRecordFor :: Text -> IO (Maybe CodexLive)
codexRecordFor = hookRecordFor codexStore

-- | The codex process a hook runs under: the nearest ancestor of @pid@ (the
-- hook client) whose program is Codex's ('isCodexProgram').
codexProcessOf :: Int -> IO (Maybe (Int, String))
codexProcessOf = hookProcessOf isCodexProgram

-- | Is this codex command line an interactive session — not @codex exec@,
-- nor one of the other subcommands (which run the same hooks, being Codex)?
codexInteractive :: String -> Bool
codexInteractive args = case drop 1 (words args) of
  (w : _) -> w `notElem`
    [ "exec", "e", "review", "login", "logout", "mcp", "plugin", "mcp-server"
    , "app-server", "remote-control", "app", "completion", "update", "doctor"
    , "sandbox", "debug", "apply", "a", "archive", "delete", "unarchive", "cloud"
    , "exec-server", "features", "help", "execpolicy" ]
  [] -> True

-- | Is this program (a process's first argument) Codex's?  (@codex@;
-- nixpkgs' wrapper execs @.codex-wrapped@; npm's launcher runs
-- @codex-\<target\>@.)
isCodexProgram :: FilePath -> Bool
isCodexProgram exe = let b = takeFileName exe
                     in "codex" `isPrefixOf` b || ".codex" `isPrefixOf` b

-- | A session started in codex process @pid@ — see 'hookSessionStart'.
codexSessionStart :: Int -> Text -> FilePath -> Maybe FilePath -> IO (Maybe Text)
codexSessionStart pid sid cwd transcript = hookSessionStart codexStore pid sid cwd transcript "idle"

-- | Has leksah recorded a session for codex process @pid@ before (in this
-- run)?  See 'hookKnownPid'.
codexKnownPid :: Int -> IO Bool
codexKnownPid = hookKnownPid codexStore

-- | A leksah launch's codex process has appeared (see
-- 'IDE.Web.Claude.codexDiscover'): record it under its launch token.
codexClaimLaunch :: Int -> Text -> FilePath -> IO ()
codexClaimLaunch = hookClaimLaunch codexStore

-- | A leksah launch of a NEW Codex agent under token @tok@ — see
-- 'hookRememberLaunch'.
codexRememberLaunch :: Text -> FilePath -> IO ()
codexRememberLaunch = hookRememberLaunch codexStore

-- | Has Codex session @sid@ got a conversation of its own to resume?  'False'
-- for a launch token that never reached its first prompt.
codexStarted :: Text -> IO Bool
codexStarted = hookStarted codexStore

-- | Session @sid@ is now @status@ (idle | busy | waiting), waiting for @what@.
codexSetStatus :: Text -> Text -> Maybe Text -> IO Bool
codexSetStatus = hookSetStatus codexStore

-- | Session @sid@ ended: its record goes.
codexSessionEnd :: Text -> IO ()
codexSessionEnd = hookSessionEnd codexStore

-- | Drop the records of codex processes that have gone without a SessionEnd
-- (a crash, a kill) or whose pid now runs something else — see 'hookPrune'.
codexPrune :: IO ()
codexPrune = hookPrune codexStore isCodexProgram

--------------------------------------------------------------------------------
-- Sessions leksah has seen

-- | Is @sid@ a Codex session (running now, or seen by leksah's hooks before)?
isCodexSession :: Text -> IO Bool
isCodexSession = hookIsSession codexStore

-- | Where a Codex session ran, if leksah knows.
codexSessionDir :: Text -> IO (Maybe FilePath)
codexSessionDir = hookSessionDir codexStore

-- | The Codex sessions that ran in @dir@, with their rollouts.
codexSessionsIn :: FilePath -> IO [(Text, FilePath)]
codexSessionsIn = hookSessionsIn codexStore

-- | A Codex session's rollout (its transcript), if leksah knows it.
codexTranscriptOf :: Text -> IO (Maybe FilePath)
codexTranscriptOf = hookTranscriptOf codexStore

--------------------------------------------------------------------------------
-- Installing the hooks

-- | The events leksah listens to.  All synchronous: Codex 0.146 skips a hook
-- marked @async@ ("async hooks are not supported yet"), whatever its docs
-- say, and it has no Interrupt event.  @leksah-cmd agent hook@ answers in
-- milliseconds, and gives up after 1.5s when leksah is not answering, so a
-- turn is never held up for long.
codexHookEvents :: [Text]
codexHookEvents =
  [ "SessionStart", "UserPromptSubmit", "Stop", "SessionEnd"
  , "PreToolUse", "PostToolUse", "PermissionRequest"
  , "PreCompact", "PostCompact", "SubagentStart", "SubagentStop" ]

hookScript, mcpScript :: IO FilePath
hookScript = (</> "codex-hook") <$> leksahDir
mcpScript  = (</> "codex-mcp") <$> leksahDir

-- | (Re)write the two wrapper scripts so they run the current @leksah-cmd@.
-- 'False' without @leksah-cmd@ on PATH.  Called at every leksah launch of
-- Codex, and by the install.
codexWriteScripts :: IO Bool
codexWriteScripts = findExecutable "leksah-cmd" >>= \case
  Nothing  -> return False
  Just exe -> do
    d <- leksahDir
    createDirectoryIfMissing True d
    forM_ (codexScripts exe) $ \(name, body) -> do
      let path = d </> name
      atomicWrite path (BL.fromStrict (TE.encodeUtf8 body))
      perms <- getPermissions path
      setPermissions path (setOwnerExecutable True perms)
    return True

-- | leksah's wrapper scripts for Codex — their names in @~\/.leksah@, and what
-- they say — running leksah-cmd @exe@ (on a shared host: "IDE.Web.RemoteCli").
-- The token a leksah launch set ('IDE.Web.Claude.codexCommandLine') rides
-- along, so leksah can tie the session to the agent it started, and the
-- hook's parent process, to find the codex it runs under.
codexScripts :: FilePath -> [(FilePath, Text)]
codexScripts exe =
  [ ("codex-hook", T.unlines
      [ "#!/bin/sh"
      , "# Written by leksah: Codex hooks report to the running leksah."
      , "exec " <> cmd <> " agent hook \"$1\" codex \"${LEKSAH_AGENT_TOKEN:-}\" \"$PPID\"" ])
  , ("codex-mcp", T.unlines
      [ "#!/bin/sh"
      , "# Written by leksah: its MCP server (IDE tools) for Codex sessions."
      , "exec " <> cmd <> " mcp" ]) ]
  where
    cmd = "'" <> T.replace "'" "'\\''" (T.pack exe) <> "'"

hooksJson :: IO FilePath
hooksJson = (</> "hooks.json") <$> codexHome

rulesFile :: IO FilePath
rulesFile = (</> "rules" </> "leksah.rules") <$> codexHome

-- | Is a hook group one of leksah's (its command runs our wrapper)?
ourGroup :: FilePath -> Value -> Bool
ourGroup script = \case
  Object g | Just (Array hs) <- KM.lookup "hooks" g ->
    any (\case Object h | Just (String c) <- KM.lookup "command" h ->
                            T.pack script `T.isPrefixOf` c
               _ -> False) (toList hs)
  _ -> False

-- | Codex's @hooks.json@ — empty when there is none.  'Left' when there is
-- one leksah cannot read as a JSON object (a trailing comma, a comment …):
-- then it must not be written at all, or the user's own hooks would be lost.
readHooks :: IO (Either Text (KM.KeyMap Value))
readHooks = (`catch` \(e :: SomeException) -> return (Left (T.pack (show e)))) $ do
  p <- hooksJson
  ex <- doesFileExist p
  if not ex then return (Right mempty) else do
    v <- decodeStrict' <$> BS.readFile p
    return $ case v of
      Just (Object o) -> Right o
      _ -> Left (T.pack p <> " is not a JSON object leksah can read — fix it \
                \(or move it aside), and try again; leksah leaves it alone")

-- | Merge leksah's hook groups into Codex's @hooks.json@ (the user's own
-- entries stay), point the MCP server at leksah (through Codex's own
-- @codex mcp add@ — leksah never edits @config.toml@ itself), and pre-approve
-- the few @leksah-cmd@ commands leksah's notes ask an agent to run.
codexInstall :: IO Text
codexInstall = (`catch` \(e :: SomeException) -> return ("codex install failed: " <> T.pack (show e) <> "\n")) $ do
  ok <- codexWriteScripts
  if not ok then return "leksah-cmd is not on PATH, so there is nothing for Codex to run.\n" else do
    script <- hookScript
    readHooks >>= \case
     Left err -> return ("Not installed: " <> err <> "\n")
     Right top -> do
      clearUninstalled
      installInto script top

-- | 'codexInstall', once @hooks.json@ has been read.
installInto :: FilePath -> KM.KeyMap Value -> IO Text
installInto script top = do
    p <- hooksJson
    home <- codexHome
    createDirectoryIfMissing True home
    atomicWrite p (encode (Object (codexHooksWith script top)))
    rules <- rulesFile
    createDirectoryIfMissing True (home </> "rules")
    atomicWrite rules . BL.fromStrict . TE.encodeUtf8 $ codexRulesText
    mcp <- mcpScript
    mcpMsg <- codexCli ["mcp", "add", "leksah", "--", mcp] >>= \case
      Right _  -> return "the leksah MCP server is registered (codex mcp add leksah)"
      Left err -> return ("the MCP server could not be registered: " <> err)
    return $ T.unlines
      [ "Installed leksah's Codex hooks into " <> T.pack p <> ","
      , mcpMsg <> ","
      , "and allowed `leksah-cmd agent describe|register|done|send` (" <> T.pack rules <> ")."
      , ""
      , "Codex runs new hooks only once you trust them: start codex, type /hooks,"
      , "and trust the leksah ones (" <> T.pack script <> ")." ]

-- | Codex's hooks.json with leksah's hook groups merged in, running hook
-- script @script@ (the user's own entries stay).
codexHooksWith :: FilePath -> KM.KeyMap Value -> KM.KeyMap Value
codexHooksWith script top = KM.insert "hooks" (Object merged) top
  where
    existing = case KM.lookup "hooks" top of
      Just (Object o) -> o
      _               -> mempty
    groupFor ev = object
      [ "hooks" .= [ object
          [ "type" .= ("command" :: Text)
          , "command" .= (T.pack script <> " " <> ev)
          , "timeout" .= (if ev == "SessionEnd" then 3 else 5 :: Int) ] ] ]
    merged = foldr (\ev acc ->
               let k = Key.fromText ev
                   others = case KM.lookup k acc of
                     Just (Array gs) -> filter (not . ourGroup script) (toList gs)
                     _               -> []
               in KM.insert k (toJSON (others <> [groupFor ev])) acc)
             existing codexHookEvents

-- | leksah's rules file for Codex (@rules\/leksah.rules@): the leksah-cmd
-- commands its notes ask an agent to run, allowed.
codexRulesText :: Text
codexRulesText = T.unlines
  [ "# Written by leksah: the leksah-cmd commands its notes ask an agent to"
  , "# run (they only tell the IDE what the agent is doing)."
  , "# (`agent send` too: how a forked agent reports back to the one that"
  , "# started it — a prompt there would stall that loop.)"
  , "prefix_rule(pattern = [\"leksah-cmd\", \"agent\", [\"describe\", \"register\", \"done\", \"send\"]], decision = \"allow\")" ]

-- | Take leksah's hooks, MCP server and rules out of Codex again.
codexUninstall :: IO Text
codexUninstall = (`catch` \(e :: SomeException) -> return ("codex uninstall failed: " <> T.pack (show e) <> "\n")) $ do
  script <- hookScript
  readHooks >>= \case
    Left err -> return ("Not uninstalled: " <> err <> "\n")
    Right top -> do
      let existing = case KM.lookup "hooks" top of
            Just (Object o) -> o
            _               -> mempty
          kept = KM.mapMaybe (\case
                   Array gs -> let gs' = filter (not . ourGroup script) (toList gs)
                               in if null gs' then Nothing else Just (toJSON gs')
                   v -> Just v) existing
      p <- hooksJson
      ex <- doesFileExist p
      when ex $ atomicWrite p (encode (Object (KM.insert "hooks" (Object kept) top)))
      rules <- rulesFile
      removeFile rules `catch` \(_ :: SomeException) -> return ()
      _ <- codexCli ["mcp", "remove", "leksah"]
      -- Remembered, so the next Codex launch does not quietly put it all back
      -- ('codexEnsureInstalled'); `codex install` forgets it again.
      markUninstalled
      return "Removed leksah's hooks, MCP server and rules from Codex.\n"

-- | Are leksah's hooks in Codex's @hooks.json@ (all of them)?
codexInstalled :: IO Bool
codexInstalled = do
  script <- hookScript
  readHooks >>= \case
    Left _ -> return False
    Right top -> do
      let have ev = case KM.lookup "hooks" top >>= \case Object o -> KM.lookup (Key.fromText ev) o
                                                         _        -> Nothing of
            Just (Array gs) -> any (ourGroup script) (toList gs)
            _               -> False
      return (all have codexHookEvents)

-- | Install the hooks if they are missing — the first time leksah starts a
-- Codex session — unless the user took them out (`leksah-cmd codex
-- uninstall`).  'Just' the install's report when it did.
codexEnsureInstalled :: IO (Maybe Text)
codexEnsureInstalled = do
  out <- uninstalledMarker >>= doesFileExist
  if out then return Nothing else codexInstalled >>= \case
    True  -> return Nothing
    False -> Just <$> codexInstall

uninstalledMarker :: IO FilePath
uninstalledMarker = (</> "codex-uninstalled") <$> leksahDir

markUninstalled, clearUninstalled :: IO ()
markUninstalled = do
  m <- uninstalledMarker
  createDirectoryIfMissing True (takeDirectory m)
  writeFile m "leksah-cmd codex uninstall: leksah does not reinstall its Codex hooks by itself\n"
clearUninstalled = do
  m <- uninstalledMarker
  removeFile m `catch` \(_ :: SomeException) -> return ()

-- | What is installed, and whether Codex has been told to trust it.
codexStatus :: IO Text
codexStatus = do
  inst <- codexInstalled
  script <- hookScript
  cfg <- (</> "config.toml") <$> codexHome
  trusted <- (`catch` \(_ :: SomeException) -> return 0) $ do
    ex <- doesFileExist cfg
    if not ex then return (0 :: Int) else do
      t <- TE.decodeUtf8 <$> BS.readFile cfg
      -- Codex keeps one [hooks.state."<source>:<event>:…"] table per trusted
      -- hook, naming the file it came from; count the ones for hooks.json
      -- that carry a trusted_hash.
      let tables = T.splitOn "\n[" t
      return (length [ () | tb <- tables, "hooks.state" `T.isPrefixOf` tb
                          , "hooks.json" `T.isInfixOf` T.takeWhile (/= '\n') tb
                          , "trusted_hash" `T.isInfixOf` tb ])
  mcp <- either (const False) (const True) <$> codexCli ["mcp", "get", "leksah"]
  avail <- codexAvailable
  return $ T.unlines
    [ "codex on PATH:     " <> yn avail
    , "hooks installed:   " <> yn inst <> "  (" <> T.pack script <> ")"
    , "hooks trusted:     " <> (if trusted >= length codexHookEvents then "yes"
                                else if trusted == 0 then "no — run /hooks in codex and trust them"
                                else T.pack (show trusted) <> " of " <> T.pack (show (length codexHookEvents))
                                       <> " — run /hooks in codex")
    , "MCP server:        " <> yn mcp ]
  where yn b = if b then "yes" else "no"

codexCli :: [String] -> IO (Either Text Text)
codexCli args = (`catch` \(e :: SomeException) -> return (Left (T.pack (show e)))) $
  findExecutable "codex" >>= \case
    Nothing -> return (Left "codex is not on PATH")
    Just c  -> do
      (ec, out, err) <- readProcessWithExitCode c args ""
      return $ case ec of
        ExitSuccess -> Right (T.pack out)
        _           -> Left (T.strip (T.pack (if null err then out else err)))

--------------------------------------------------------------------------------
-- Transcripts (rollouts)

-- | Is this a Codex rollout (@rollout-\<time\>-\<id\>.jsonl@), rather than a
-- Claude Code transcript?
isRollout :: FilePath -> Bool
isRollout = ("rollout-" `isPrefixOf`) . takeFileName

-- | The conversation in the tail of a rollout, oldest first, as
-- @(fromUser, text)@: what was typed into it (@user_message@ events — not
-- the developer messages leksah's own briefing and Codex's instructions
-- arrive as) and what the agent said (@agent_message@).
rolloutTurns :: FilePath -> IO [(Bool, Text)]
rolloutTurns path = (`catch` \(_ :: SomeException) -> return []) $ do
  ls <- withFile path ReadMode $ \h -> do
    size <- hFileSize h
    let back = min size (512 * 1024)
    hSeek h SeekFromEnd (negate back)
    BS.lines <$> BS.hGet h (fromIntegral back)
  return (rolloutTurnsOf ls)

-- | 'rolloutTurns' of a rollout's lines (its tail, read here or on a shared
-- host: "IDE.Web.RemoteAgents").
rolloutTurnsOf :: [BS.ByteString] -> [(Bool, Text)]
rolloutTurnsOf ls = filter (not . T.null . T.strip . snd) $
  -- (Codex 0.146 writes the conversation as @user_message@ \/ @agent_message@
  -- events; 0.170 does not, and its turns are the @response_item@ messages —
  -- which 0.146 writes too, so they are read only when there are no events.)
  case [ t | l <- ls, "\"event_msg\"" `BS.isInfixOf` l
           , Just t <- [decodeStrict' l >>= parseMaybe eventP] ] of
    [] -> [ t | l <- ls, "\"response_item\"" `BS.isInfixOf` l
              , Just t <- [decodeStrict' l >>= parseMaybe itemP] ]
    ts -> ts
  where
    eventP = withObject "line" $ \o -> do
      p  <- o .:? "payload"
      case p of
        Just (Object po) -> do
          ty <- parseField po "type"
          msg <- parseField po "message"
          case (ty, msg) of
            (Just ("user_message" :: Text), Just m)  -> return (True, m)
            (Just "agent_message", Just m)           -> return (False, m)
            _ -> fail "not a turn"
        _ -> fail "no payload"
    -- A message the user typed, or the agent said (not the developer
    -- messages, nor the context Codex wraps in tags and sends as the user's).
    itemP = withObject "line" $ \o -> do
      p  <- o .:? "payload"
      case p of
        Just (Object po) | Just (String "message") <- KM.lookup "type" po
                         , Just (String role) <- KM.lookup "role" po
                         , role `elem` ["user", "assistant"]
                         , Just (Array cs) <- KM.lookup "content" po -> do
          let txt = T.intercalate "\n" [ x | Object c <- toList cs
                                          , Just (String ty) <- [KM.lookup "type" c]
                                          , ty `elem` ["input_text", "output_text"]
                                          , Just (String x) <- [KM.lookup "text" c] ]
          if role == "user" && "<" `T.isPrefixOf` T.stripStart txt then fail "context"
            else return (role == "user", txt)
        _ -> fail "not a message"
    parseField po k = return (KM.lookup k po >>= \case String t -> Just t; _ -> Nothing)

-- | The first thing typed into a rollout's session — its label until it says
-- what it is.  Reads from the front, a bounded number of lines (the first
-- line alone carries Codex's whole system prompt).
rolloutFirstPrompt :: FilePath -> IO (Maybe Text)
rolloutFirstPrompt path = (`catch` \(_ :: SomeException) -> return Nothing) $
  withFile path ReadMode $ \h -> do
    ls <- takeLines h (60 :: Int)
    return $ case [ t | (True, t) <- rolloutTurnsOf ls ] of
      (t : _) -> Just (T.strip (T.takeWhile (/= '\n') t))
      []      -> Nothing
  where
    takeLines _ 0 = return []
    takeLines h n = hIsEOF h >>= \case
      True  -> return []
      False -> (:) <$> BS.hGetLine h <*> takeLines h (n - 1)

--------------------------------------------------------------------------------
-- Tool calls

-- | The files a Codex @apply_patch@ call touches: its patch names each one on
-- an @*** Add File:@ \/ @*** Update File:@ \/ @*** Delete File:@ \/ @*** Move to:@
-- line.  The patch is wherever it is in the tool input (a string field).
codexPatchFiles :: Value -> [FilePath]
codexPatchFiles v = nub
  [ T.unpack (T.strip p)
  | s <- strings v, l <- T.lines s
  , pre <- ["*** Add File:", "*** Update File:", "*** Delete File:", "*** Move to:"]
  , Just p <- [T.stripPrefix pre l], not (T.null (T.strip p)) ]
  where
    strings = \case
      String t -> [t]
      Array xs -> concatMap strings (toList xs)
      Object o -> concatMap strings (KM.elems o)
      _        -> []

