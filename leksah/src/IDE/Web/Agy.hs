{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Google's Antigravity CLI (@agy@) sessions, as a third kind of agent beside
-- Claude Code and Codex.
--
-- Like Codex, agy has hooks but keeps no file saying which sessions are
-- running or what they are doing, so leksah keeps one for it
-- ("IDE.Web.HookSessions": @~\/.leksah\/agy-sessions\/\<pid\>.json@), which
-- "IDE.Web.Claude" reads beside Claude Code's.  What feeds it is not quite
-- Codex's, because agy's hooks are not (verified against agy 1.5.13 — its
-- docs leave most of this out):
--
--   * Its hooks are SessionStart, PreInvocation, PostInvocation, Stop,
--     PreToolUse and PostToolUse — no SessionEnd, no PermissionRequest,
--     nothing at all on @\/exit@.  SessionStart comes with a new
--     conversation's FIRST PROMPT (and after @\/clear@), and never for
--     @agy --conversation=\<id\>@.
--   * Its /title command/ is what says what a session is doing: agy runs it
--     with a JSON state on stdin at every change — @agent_state@ (@idle@,
--     @working@, and @tool_use@ exactly while an approval prompt is up — the
--     only sign of that anywhere) and @conversation_id@ (from launch on a
--     resume, and straight after a @\/clear@).  So leksah installs a title
--     command ('titleScript') as well as hooks, and its hook handler
--     ('IDE.Web.Agent.agyHook') treats each report as an event.
--   * A PreToolUse hook that prints @{}@, or FAILS, stops the tool call
--     (empty output is the "no opinion" answer).  leksah therefore installs
--     no PreToolUse hook at all — a slow @leksah-cmd@ start would block every
--     tool — and its wrapper scripts always exit 0.
--
-- The hooks go into agy's global @~\/.gemini\/config\/hooks.json@ under
-- leksah's own key; the title command and the allow-list for leksah's few
-- @leksah-cmd agent …@ commands into @~\/.gemini\/antigravity-cli\/settings.json@
-- (only when the user has no title command of their own — leksah's launches
-- pass theirs with @--settings@ anyway); the MCP server through
-- @agy mcp add@.  agy needs no trust step for any of them.
--
-- This module is FS/process only, and must not import "IDE.Web.Claude".
module IDE.Web.Agy
  ( agyAvailable
  , HookLive(..)
    -- * Live sessions
  , agyLiveRecords
  , agyRecordFor
  , agyProcessOf
  , isAgyProgram
  , agyInteractive
  , agySessionStart
  , agyClaimLaunch
  , agyRememberLaunch
  , agySetStatus
  , agyKnownPid
  , agyPidAlive
  , agyPrune
    -- * Sessions leksah has seen
  , isAgySession
  , agySessionDir
  , agyTranscriptOf
  , agyTranscriptFor
  , agyStarted
    -- * Launching
  , agyLaunchSettings
    -- * Installing the hooks
  , agyInstall
  , agyUninstall
  , agyStatus
  , agyInstalled
  , agyEnsureInstalled
  , agyWriteScripts
    -- * Hook payloads
  , agyClaudeShaped
  , agyStateStatus
    -- * Transcripts
  , isAgyTranscript
  , agyTurns
  , agyFirstPrompt
  , agyPendingTool
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Aeson
       (Object, Value(..), (.=), (.:?), decodeStrict', encode, object, withObject)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
       (createDirectoryIfMissing, doesFileExist, findExecutable,
        getHomeDirectory, getPermissions, removeFile, setPermissions,
        setOwnerExecutable)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO
       (IOMode(ReadMode), SeekMode(SeekFromEnd), hFileSize, hIsEOF, hSeek, withFile)
import System.IO.Unsafe (unsafePerformIO)
import IDE.Utils.Spawn (readProcessWithExitCode)
import IDE.Web.HookSessions
#if !defined(mingw32_HOST_OS)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
#endif
#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
import System.Posix.Signals (nullSignal, signalProcess)
#endif

-- | Whether @agy@ is on PATH — cached once seen (it can be installed after
-- leksah starts).
{-# NOINLINE agyAvailableRef #-}
agyAvailableRef :: IORef Bool
agyAvailableRef = unsafePerformIO (newIORef False)

agyAvailable :: IO Bool
agyAvailable = readIORef agyAvailableRef >>= \case
  True  -> return True
  False -> do
    found <- isJust <$> findExecutable "agy"
    when found $ writeIORef agyAvailableRef True
    return found

-- | agy's configuration (hooks.json, mcp_config.json) and its own data
-- (settings.json, the conversations' @brain\/@).
agyConfigDir, agyDataDir :: IO FilePath
agyConfigDir = (</> ".gemini" </> "config") <$> getHomeDirectory
agyDataDir   = (</> ".gemini" </> "antigravity-cli") <$> getHomeDirectory

{-# NOINLINE agyStore #-}
agyStore :: HookStore
agyStore = unsafePerformIO (newHookStore "agy")

--------------------------------------------------------------------------------
-- Live sessions

agyLiveRecords :: IO [HookLive]
agyLiveRecords = hookLiveRecords agyStore

agyRecordFor :: Text -> IO (Maybe HookLive)
agyRecordFor = hookRecordFor agyStore

-- | The agy process a hook (or the title command) runs under.
agyProcessOf :: Int -> IO (Maybe (Int, String))
agyProcessOf = hookProcessOf isAgyProgram

-- | Is this program (a process's first argument) agy?  The installer copies
-- the @antigravity@ binary to @~\/.local\/bin\/agy@.
isAgyProgram :: FilePath -> Bool
isAgyProgram exe = takeFileName exe `elem` ["agy", "antigravity"]

-- | Is this agy command line an interactive session — not @agy -p@, nor one
-- of its subcommands (@agy mcp@, @agy acp@ …)?
--
-- Only agy's own flags count: everything after @-i@ (the prompt a leksah
-- launch passes) or @--@ is text, which may well say @-p@ itself.
agyInteractive :: String -> Bool
agyInteractive args = case drop 1 (words args) of
  ws@(w : _) -> w `notElem`
      [ "acp", "agent", "agents", "changelog", "help", "install", "mcp"
      , "mic-serve", "models", "plugin", "plugins", "remote-control", "update" ]
    && not (any printFlag (takeWhile (not . promptStart) ws))
  [] -> True
  where
    printFlag w = w `elem` ["-p", "--print", "--prompt"]
               || any (`isPrefixOfS` w) ["--print=", "--prompt="]
    promptStart w = w `elem` ["-i", "--prompt-interactive", "--"]
                 || "--prompt-interactive=" `isPrefixOfS` w
    isPrefixOfS a b = take (length a) b == a

-- | Conversation @sid@ is running in agy process @pid@ (a new one, a resume,
-- or the fresh id a @\/clear@ gives the same process) — see
-- 'hookSessionStart'.  Its transcript is where agy keeps it.
agySessionStart :: Int -> Text -> FilePath -> Text -> IO (Maybe Text)
agySessionStart pid sid cwd status = do
  tr <- agyTranscriptFor sid
  hookSessionStart agyStore pid sid cwd (Just tr) status

agyClaimLaunch :: Int -> Text -> FilePath -> IO ()
agyClaimLaunch = hookClaimLaunch agyStore

agyRememberLaunch :: Text -> FilePath -> IO ()
agyRememberLaunch = hookRememberLaunch agyStore

agySetStatus :: Text -> Text -> Maybe Text -> IO Bool
agySetStatus = hookSetStatus agyStore

agyKnownPid :: Int -> IO Bool
agyKnownPid = hookKnownPid agyStore

-- | Is process @pid@ still running?  (A signal-0 probe: no @ps@.)  A record
-- outlives its agy process — agy reports nothing when it exits — so a
-- record is only taken at its word while this holds.
agyPidAlive :: Int -> IO Bool
#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
agyPidAlive pid = (True <$ signalProcess nullSignal (fromIntegral pid))
                    `catch` \(_ :: SomeException) -> return False
#else
agyPidAlive _ = return True
#endif

-- | Drop the records of agy processes that have gone (or whose pid now runs
-- something else) — see 'hookPrune'.  agy says nothing when it exits.
agyPrune :: IO ()
agyPrune = hookPrune agyStore isAgyProgram

--------------------------------------------------------------------------------
-- Sessions leksah has seen

isAgySession :: Text -> IO Bool
isAgySession = hookIsSession agyStore

agySessionDir :: Text -> IO (Maybe FilePath)
agySessionDir = hookSessionDir agyStore

agyTranscriptOf :: Text -> IO (Maybe FilePath)
agyTranscriptOf = hookTranscriptOf agyStore

agyStarted :: Text -> IO Bool
agyStarted = hookStarted agyStore

-- | Where agy keeps conversation @sid@'s transcript: the full one (the plain
-- @transcript.jsonl@ beside it has each tool call's arguments as strings).
-- The title command's own @transcript_path@ names the Antigravity APP's
-- folder, not the CLI's, so it is not used.
agyTranscriptFor :: Text -> IO FilePath
agyTranscriptFor sid = do
  d <- agyDataDir
  return (d </> "brain" </> T.unpack sid </> ".system_generated" </> "logs"
            </> "transcript_full.jsonl")

--------------------------------------------------------------------------------
-- Launching

-- | The @--settings@ a leksah launch passes (merged over settings.json, and
-- never saved): leksah's title command, so the session reports what it is
-- doing even when the user's settings name a title command of their own.
agyLaunchSettings :: IO Text
agyLaunchSettings = do
  t <- titleScript
  return . TE.decodeUtf8 . BL.toStrict . encode $ object
    [ "title" .= object [ "type" .= ("command" :: Text), "command" .= t ] ]

--------------------------------------------------------------------------------
-- Installing the hooks

-- | The hook events leksah listens to.  Not PreToolUse: see the module notes.
hookEvents :: [Text]
hookEvents = ["SessionStart", "PostToolUse", "Stop"]

hookScript, titleScript, mcpScript :: IO FilePath
hookScript  = (</> "agy-hook") <$> leksahDir
titleScript = (</> "agy-title") <$> leksahDir
mcpScript   = (</> "agy-mcp") <$> leksahDir

-- | (Re)write the wrapper scripts so they run the current @leksah-cmd@.
-- 'False' without @leksah-cmd@ on PATH.  Called at every leksah launch of
-- agy, and by the install.  The hook and title scripts NEVER fail and print
-- nothing but leksah's answer: agy stops a tool call over a failing hook.
agyWriteScripts :: IO Bool
agyWriteScripts = findExecutable "leksah-cmd" >>= \case
  Nothing  -> return False
  Just exe -> do
    d <- leksahDir
    createDirectoryIfMissing True d
    hs <- hookScript
    ts <- titleScript
    ms <- mcpScript
    let cmd = shq (T.pack exe)
    -- The token a leksah launch set (see 'IDE.Web.Claude.agyCommandLine')
    -- rides along, so leksah can tie the session to the agent it started.
    writeScript hs $ T.unlines
      [ "#!/bin/sh"
      , "# Written by leksah: Antigravity (agy) hooks report to the running leksah."
      , "# It must never fail: agy can stop the agent over a failing hook."
      , cmd <> " agent hook \"$1\" agy \"${LEKSAH_AGENT_TOKEN:-}\" 2>/dev/null"
      , "exit 0" ]
    writeScript ts $ T.unlines
      [ "#!/bin/sh"
      , "# Written by leksah: Antigravity's title command.  agy hands it the session's"
      , "# state at every change; leksah records it and answers with the title."
      , "# ($PPID is the agy process itself: it names who is reporting.)"
      , "t=$(" <> cmd <> " agent hook Title agy \"${LEKSAH_AGENT_TOKEN:-}\" \"$PPID\" 2>/dev/null)"
      , "if [ -n \"$t\" ]; then printf '%s\\n' \"$t\"; else echo Antigravity; fi"
      , "exit 0" ]
    writeScript ms $ T.unlines
      [ "#!/bin/sh"
      , "# Written by leksah: its MCP server (IDE tools) for Antigravity sessions."
      , "exec " <> cmd <> " mcp" ]
    return True
  where
    writeScript p body = do
      atomicWrite p (BL.fromStrict (TE.encodeUtf8 body))
      perms <- getPermissions p
      setPermissions p (setOwnerExecutable True perms)
    shq t = "'" <> T.replace "'" "'\\''" t <> "'"

hooksJson, settingsJson, mcpJson :: IO FilePath
hooksJson    = (</> "hooks.json") <$> agyConfigDir
mcpJson      = (</> "mcp_config.json") <$> agyConfigDir
settingsJson = (</> "settings.json") <$> agyDataDir

-- | leksah's key in hooks.json (each top-level key is one named hook set).
hooksKey :: Key.Key
hooksKey = "leksah"

-- | The @leksah-cmd@ commands leksah's notes ask an agent to run (they only
-- tell the IDE what the agent is doing; @agent send@ is how a forked agent
-- reports back — a prompt there would stall that loop).  agy matches a
-- @command(…)@ rule word by word, as a prefix.
allowRules :: [Text]
allowRules = [ "command(leksah-cmd agent " <> v <> ")" | v <- ["describe", "register", "done", "send"] ]

-- | A JSON object file — empty when there is none.  'Left' when there is one
-- leksah cannot read as an object: then it must not be written at all, or
-- what the user keeps there would be lost.
readObject :: FilePath -> IO (Either Text (KM.KeyMap Value))
readObject p = (`catch` \(e :: SomeException) -> return (Left (T.pack (show e)))) $ do
  ex <- doesFileExist p
  if not ex then return (Right mempty) else do
    v <- decodeStrict' . stripBom <$> BS.readFile p
    return $ case v of
      Just (Object o) -> Right o
      _ -> Left (T.pack p <> " is not a JSON object leksah can read — fix it "
                 <> "(or move it aside), and try again; leksah leaves it alone")
  where stripBom b = maybe b id (BS.stripPrefix "\xef\xbb\xbf" b)

-- | Write a JSON object file, keeping its permissions (agy's settings.json is
-- private to its owner, and an atomic write makes a new file).
writeObject :: FilePath -> KM.KeyMap Value -> IO ()
writeObject p o = do
  createDirectoryIfMissing True (takeDirectory p)
#if !defined(mingw32_HOST_OS)
  ex <- doesFileExist p
  mode <- if ex then Just . fileMode <$> getFileStatus p else return Nothing
  atomicWriteWith (\tmp -> mapM_ (setFileMode tmp) mode) p (encode (Object o))
#else
  atomicWrite p (encode (Object o))
#endif

-- | Is this the title command leksah installs?
isOurTitle :: FilePath -> Value -> Bool
isOurTitle script = \case
  Object t | Just (String c) <- KM.lookup "command" t -> T.pack script `T.isPrefixOf` c
  _ -> False

-- | Put leksah's hooks into agy's hooks.json (the user's own sets stay), its
-- title command and allow-list into settings.json, and its MCP server in
-- through @agy mcp add@.
agyInstall :: IO Text
agyInstall = (`catch` \(e :: SomeException) -> return ("agy install failed: " <> T.pack (show e) <> "\n")) $ do
  ok <- agyWriteScripts
  if not ok then return "leksah-cmd is not on PATH, so there is nothing for agy to run.\n" else do
    hp <- hooksJson
    sp <- settingsJson
    hooks <- readObject hp
    settings <- readObject sp
    case (hooks, settings) of
      (Left err, _) -> return ("Not installed: " <> err <> "\n")
      (_, Left err) -> return ("Not installed: " <> err <> "\n")
      (Right h, Right s) -> do
        clearUninstalled
        script <- hookScript
        title  <- titleScript
        let handler ev = object [ "type" .= ("command" :: Text)
                                , "command" .= (T.pack script <> " " <> ev)
                                , "timeout" .= (5 :: Int) ]
            entry ev | ev == "PostToolUse" =
                         object [ "matcher" .= ("*" :: Text), "hooks" .= [handler ev] ]
                     | otherwise = handler ev
            ours = Object (KM.fromList [ (Key.fromText ev, toJSONList [entry ev]) | ev <- hookEvents ])
        writeObject hp (KM.insert hooksKey ours h)
        -- The title command: leksah's, unless the user has their own.
        let (s1, titleMsg) = case KM.lookup "title" s of
              Just t | not (isOurTitle title t) ->
                (s, "kept your own title command (leksah's launches pass theirs "
                    <> "with --settings; others won't show what they are doing)")
              _ -> ( KM.insert "title" (object [ "type" .= ("command" :: Text)
                                              , "command" .= title ]) s
                   , "set the title command (" <> T.pack title <> ")" )
            s2 = KM.insert "permissions" (Object (addAllow (permissionsOf s1))) s1
        writeObject sp s2
        mcp <- mcpScript
        registered <- mcpRegistered
        mcpMsg <- if registered then return "the leksah MCP server was already registered" else
          agyCli ["mcp", "add", "leksah", mcp] >>= \case
            Right _  -> return "the leksah MCP server is registered (agy mcp add leksah)"
            Left err -> return ("the MCP server could not be registered: " <> err)
        return $ T.unlines
          [ "Installed leksah's Antigravity hooks into " <> T.pack hp <> ","
          , titleMsg <> ","
          , "allowed `leksah-cmd agent describe|register|done|send` (" <> T.pack sp <> "),"
          , "and " <> mcpMsg <> "." ]
  where
    toJSONList = Array . foldr (\x acc -> pure x <> acc) mempty
    addAllow perms =
      let have = case KM.lookup "allow" perms of
            Just (Array xs) -> [ t | String t <- toList xs ]
            _               -> []
      in KM.insert "allow" (toJSONList (map String (have <> filter (`notElem` have) allowRules))) perms

permissionsOf :: KM.KeyMap Value -> KM.KeyMap Value
permissionsOf s = case KM.lookup "permissions" s of
  Just (Object p) -> p
  _               -> mempty

-- | Take leksah's hooks, title command, allow-list and MCP server out of agy
-- again.
agyUninstall :: IO Text
agyUninstall = (`catch` \(e :: SomeException) -> return ("agy uninstall failed: " <> T.pack (show e) <> "\n")) $ do
  hp <- hooksJson
  sp <- settingsJson
  readObject hp >>= \case
    Right h | KM.member hooksKey h -> writeObject hp (KM.delete hooksKey h)
    _ -> return ()
  title <- titleScript
  readObject sp >>= \case
    Right s -> do
      let s1 = case KM.lookup "title" s of
                 Just t | isOurTitle title t -> KM.delete "title" s
                 _                           -> s
          perms = permissionsOf s1
          perms' = case KM.lookup "allow" perms of
            Just (Array xs) -> KM.insert "allow"
              (Array (foldr (\x acc -> if x `elem` map String allowRules then acc else pure x <> acc)
                            mempty xs)) perms
            _ -> perms
          s2 = if KM.null perms' then KM.delete "permissions" s1
                                 else KM.insert "permissions" (Object perms') s1
      when (s2 /= s) $ writeObject sp s2
    Left _ -> return ()
  _ <- agyCli ["mcp", "remove", "leksah"]
  -- Remembered, so the next agy launch does not quietly put it all back
  -- ('agyEnsureInstalled'); `agy install` forgets it again.
  markUninstalled
  return "Removed leksah's hooks, title command, allow-list and MCP server from agy.\n"

-- | Are leksah's hooks in agy's hooks.json?
agyInstalled :: IO Bool
agyInstalled = do
  script <- hookScript
  hooksJson >>= readObject >>= \case
    Right h | Just (Object ours) <- KM.lookup hooksKey h ->
      return (all (\ev -> case KM.lookup (Key.fromText ev) ours of
                            Just v -> T.pack script `T.isInfixOf` TE.decodeUtf8 (BL.toStrict (encode v))
                            Nothing -> False) hookEvents)
    _ -> return False

-- | Install the hooks if they are missing — the first time leksah starts an
-- agy session — unless the user took them out (`leksah-cmd agy uninstall`).
-- 'Just' the install's report when it did.
agyEnsureInstalled :: IO (Maybe Text)
agyEnsureInstalled = do
  out <- uninstalledMarker >>= doesFileExist
  if out then return Nothing else agyInstalled >>= \case
    True  -> return Nothing
    False -> Just <$> agyInstall

uninstalledMarker :: IO FilePath
uninstalledMarker = (</> "agy-uninstalled") <$> leksahDir

markUninstalled, clearUninstalled :: IO ()
markUninstalled = do
  m <- uninstalledMarker
  createDirectoryIfMissing True (takeDirectory m)
  writeFile m "leksah-cmd agy uninstall: leksah does not reinstall its Antigravity hooks by itself\n"
clearUninstalled = do
  m <- uninstalledMarker
  removeFile m `catch` \(_ :: SomeException) -> return ()

-- | What is installed.
agyStatus :: IO Text
agyStatus = do
  avail <- agyAvailable
  inst <- agyInstalled
  script <- hookScript
  title <- titleScript
  settings <- settingsJson >>= readObject
  let titleState = case settings of
        Left err -> "unreadable — " <> err
        Right s -> case KM.lookup "title" s of
          Just t | isOurTitle title t -> "leksah's"
                 | otherwise -> "your own (agy sessions leksah did not start won't show what they are doing)"
          Nothing -> "none — run `leksah-cmd agy install`"
      allowed = case settings of
        Right s | Just (Array xs) <- KM.lookup "allow" (permissionsOf s) ->
          all (\r -> String r `elem` toList xs) allowRules
        _ -> False
  mcp <- mcpRegistered
  return $ T.unlines
    [ "agy on PATH:       " <> yn avail
    , "hooks installed:   " <> yn inst <> "  (" <> T.pack script <> ")"
    , "title command:     " <> titleState
    , "allow-list:        " <> yn allowed
    , "MCP server:        " <> yn mcp ]
  where yn b = if b then "yes" else "no"

-- | Is the leksah MCP server in agy's mcp_config.json?
mcpRegistered :: IO Bool
mcpRegistered = (`catch` \(_ :: SomeException) -> return False) $ do
  p <- mcpJson
  ex <- doesFileExist p
  if not ex then return False else do
    v <- decodeStrict' <$> BS.readFile p
    return $ case v of
      Just (Object o) | Just (Object ss) <- KM.lookup "mcpServers" o -> KM.member "leksah" ss
      _ -> False

agyCli :: [String] -> IO (Either Text Text)
agyCli args = (`catch` \(e :: SomeException) -> return (Left (T.pack (show e)))) $
  findExecutable "agy" >>= \case
    Nothing -> return (Left "agy is not on PATH")
    Just c  -> do
      (ec, out, err) <- readProcessWithExitCode c args ""
      return $ case ec of
        ExitSuccess -> Right (T.pack out)
        _           -> Left (T.strip (T.pack (if null err then out else err)))

--------------------------------------------------------------------------------
-- Hook payloads

-- | An agy tool call, named and shaped the way Claude Code's are — so the
-- activity line, the approval text and the edited-files record that read
-- Claude-shaped tool calls read agy's too.
agyClaudeShaped :: Text -> Object -> (Text, Object)
agyClaudeShaped name args = case name of
  "run_command" -> ("Bash", KM.fromList (pick "command" "CommandLine" <> pick "description" "toolSummary"))
  "write_file"  -> ("Write", KM.fromList (pick "file_path" "Path"))
  "edit_file"   -> ("Edit", KM.fromList (pick "file_path" "Path"))
  "read_file"   -> ("Read", KM.fromList (pick "file_path" "Path"))
  _ | Just (String p) <- KM.lookup "Path" args
    , any (`T.isInfixOf` name) ["edit", "write"] -> ("Edit", KM.fromList [("file_path", String p)])
    | otherwise -> (name, args)
  where
    pick to from = case KM.lookup from args of
      Just v@(String _) -> [(to, v)]
      _                 -> []

-- | What an @agent_state@ (the title command's) says about the session:
-- the status leksah records (idle | busy | waiting), when it says anything.
agyStateStatus :: Text -> Maybe Text
agyStateStatus = \case
  "idle"     -> Just "idle"
  "working"  -> Just "busy"
  "tool_use" -> Just "waiting"   -- an approval prompt is up (verified)
  _          -> Nothing          -- authenticating, initializing …

--------------------------------------------------------------------------------
-- Transcripts

-- | Is this an agy transcript (rather than a Claude Code one or a rollout)?
isAgyTranscript :: FilePath -> Bool
isAgyTranscript p = ("/brain/" `isInfixOf` p) && takeFileName p `elem` ["transcript_full.jsonl", "transcript.jsonl"]

-- | The conversation in the tail of a transcript, oldest first, as
-- @(fromUser, text)@: what was typed (@USER_INPUT@ steps, inside their
-- @\<USER_REQUEST\>@) and what the agent said (@PLANNER_RESPONSE@ content).
agyTurns :: FilePath -> IO [(Bool, Text)]
agyTurns path = (`catch` \(_ :: SomeException) -> return []) $ do
  ls <- tailLines path
  return [ t | l <- ls, Just t <- [decodeStrict' l >>= parseMaybe turnP]
             , not (T.null (T.strip (snd t))) ]
  where
    turnP = withObject "step" $ \o -> do
      ty <- o .:? "type"
      c  <- o .:? "content"
      case (ty :: Maybe Text, c) of
        (Just "USER_INPUT", Just txt)       -> return (True, userRequest txt)
        (Just "PLANNER_RESPONSE", Just txt) -> return (False, txt)
        _ -> fail "not a turn"

-- | What was typed, out of a @USER_INPUT@ step's content (which wraps it in
-- @\<USER_REQUEST\>@ and adds metadata).
userRequest :: Text -> Text
userRequest c = case T.breakOn "<USER_REQUEST>" c of
  (_, rest) | not (T.null rest) ->
    T.strip (fst (T.breakOn "</USER_REQUEST>" (T.drop (T.length "<USER_REQUEST>") rest)))
  _ -> T.strip c

-- | The first thing typed into a session — its label until it says what it
-- is.  Reads from the front, a bounded number of lines.
agyFirstPrompt :: FilePath -> IO (Maybe Text)
agyFirstPrompt path = (`catch` \(_ :: SomeException) -> return Nothing) $
  withFile path ReadMode $ \h -> go h (40 :: Int)
  where
    go _ 0 = return Nothing
    go h n = hIsEOF h >>= \case
      True  -> return Nothing
      False -> do
        l <- BS.hGetLine h
        if not ("\"USER_INPUT\"" `BS.isInfixOf` l) then go h (n - 1) else
          case decodeStrict' l >>= parseMaybe (withObject "s" (.:? "content")) of
            Just (Just c) | let u = userRequest c, not (T.null u) ->
              return (Just (T.takeWhile (/= '\n') u))
            _ -> go h (n - 1)

-- | The tool call a session is waiting on approval for: the last model step's
-- (agy writes it to the transcript before it asks), Claude-shaped.
agyPendingTool :: FilePath -> IO (Maybe (Text, Object))
agyPendingTool path = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  ls <- tailLines path
  let calls = mapMaybe (\l -> decodeStrict' l >>= parseMaybe callsP) ls
  return $ case reverse calls of
    ((c : _) : _) -> Just c
    _             -> Nothing
  where
    callsP = withObject "step" $ \o -> do
      ty <- o .:? "type"
      cs <- o .:? "tool_calls"
      case (ty :: Maybe Text, cs) of
        (Just "PLANNER_RESPONSE", Just (Array xs)) ->
          return [ agyClaudeShaped n a
                 | Object c <- toList xs
                 , Just (String n) <- [KM.lookup "name" c]
                 , let a = case KM.lookup "args" c of Just (Object x) -> x; _ -> mempty ]
        _ -> fail "no tool calls"

-- | The last lines of a file (its last 512KB; the first, likely cut, line is
-- dropped by the JSON decode that follows).
tailLines :: FilePath -> IO [BS.ByteString]
tailLines path = withFile path ReadMode $ \h -> do
  size <- hFileSize h
  let back = min size (512 * 1024)
  hSeek h SeekFromEnd (negate back)
  BS.lines <$> BS.hGet h (fromIntegral back)

