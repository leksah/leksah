{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Codex and Antigravity (agy) agents on a shared host — the machine an
-- @ssh:\/\/HOST\/…@ project lives on, whose leksah tmux server this leksah
-- shares ("IDE.Web.RemoteAgents").
--
-- Locally, leksah's hooks and its hook handler keep these CLIs' live records
-- ("IDE.Web.HookSessions").  On a shared host the same files are kept — in
-- the host's @~\/.leksah@, where the snapshot reads them — by whoever is
-- there to keep them: the host's own leksah, when one runs (its hook handler
-- answers the hooks, exactly as here); otherwise THIS leksah, from the
-- host's event log, which leksah-cmd writes what it could not deliver to
-- (with the CLI's process and the launch token: @offline@ lines).  The
-- records' logic lives in "IDE.Web.Agent"; this module is the file and
-- process work on the host: installing the hooks there, the briefing a
-- launch leaves for its session, which CLIs the host has, the records and
-- the index.  Everything goes over the user's own ssh ("IDE.Utils.RemoteExec").
--
-- FS/process only: must not import "IDE.Web.Claude" (which imports it).
module IDE.Web.RemoteCli
  ( remoteCliInstall
  , remoteLeaveBriefing
  , remoteAvailableClis
  , remoteCliStarted
  , remoteAgySettingsArg
    -- * Records kept from the event log
  , remoteHookRecords
  , remoteWriteRecord
  , remoteDropRecords
  , remoteIndexAdd
  , remoteIndexHas
  , remoteAgyTranscript
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (unless, void, when)
import Data.Aeson (Value(..), (.=), (.:?), decodeStrict', encode, object, withObject)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import IDE.Utils.RemoteExec (remoteHomeDir, runSsh)
import IDE.Web.AgentCli (AgentCli(..), cliTag)
import IDE.Web.Agy (agyHooksWith, agyScripts, agySettingsWith)
import IDE.Web.Codex (codexHooksWith, codexRulesText, codexScripts)
import IDE.Web.HookSessions (HookLive(..), hookRecordFromJson, hookRecordJson)

-- | Install leksah's hooks for @cli@ on @host@ — the wrapper scripts (running
-- the host's leksah-cmd), the CLI's hook config merged as the local install
-- merges it ('codexHooksWith', 'agyHooksWith', 'agySettingsWith'), the rules
-- \/ allow-list, the MCP server — without needing a leksah running there.
-- Done once per host and CLI in this run.  'Left' why it could not be.
remoteCliInstall :: Text -> AgentCli -> IO (Either Text ())
remoteCliInstall _ CliClaude = return (Right ())
remoteCliInstall host cli = do
  done <- S.member (host, cli) <$> readIORef installedRef
  if done then return (Right ()) else (`catch` \(e :: SomeException) -> return (Left (T.pack (show e)))) $ do
    home <- remoteHomeDir host
    exe <- hostLeksahCmd host
    case exe of
      Nothing -> return . Left $
        "there is no leksah-cmd on " <> host <> " for its hooks to run (build leksah there once)"
      Just cmd -> do
        let leksah = home </> ".leksah"
            scripts = (if cli == CliCodex then codexScripts else agyScripts) cmd
        mapM_ (\(n, body) -> writeHostFile host (leksah </> n) False (TE.encodeUtf8 body)) scripts
        _ <- runSsh host "chmod +x \"$0\" \"$@\"" (map (T.pack . (leksah </>) . fst) scripts) mempty
        r <- case cli of
          CliCodex -> do
            let hooks = home </> ".codex" </> "hooks.json"
            modifyHostJson host hooks False (codexHooksWith (leksah </> "codex-hook"))
            writeHostFile host (home </> ".codex" </> "rules" </> "leksah.rules") False
              (TE.encodeUtf8 codexRulesText)
            hostShell host ("codex mcp get leksah >/dev/null 2>&1 || codex mcp add leksah -- "
                            <> shq (T.pack (leksah </> "codex-mcp")))
          _ -> do
            let hooks = home </> ".gemini" </> "config" </> "hooks.json"
                settings = home </> ".gemini" </> "antigravity-cli" </> "settings.json"
            modifyHostJson host hooks False (agyHooksWith (leksah </> "agy-hook"))
            modifyHostJson host settings True (fst . agySettingsWith (leksah </> "agy-title"))
            hostShell host ("grep -q '\"leksah\"' \"$HOME/.gemini/config/mcp_config.json\" 2>/dev/null || agy mcp add leksah "
                            <> shq (T.pack (leksah </> "agy-mcp")))
        case r of
          Left err -> return (Left err)
          Right () -> do
            atomicModifyIORef' installedRef (\s -> (S.insert (host, cli) s, ()))
            return (Right ())

{-# NOINLINE installedRef #-}
installedRef :: IORef (S.Set (Text, AgentCli))
installedRef = unsafePerformIO (newIORef S.empty)

-- | The host's leksah-cmd: in the bin-dir its leksah writes at start-up
-- (@~\/.leksah\/bin-dir@), else on its PATH.
hostLeksahCmd :: Text -> IO (Maybe FilePath)
hostLeksahCmd host = do
  (_, out, _) <- runSsh host
    (T.concat [ "d=$(head -n1 \"$HOME/.leksah/bin-dir\" 2>/dev/null); "
              , "if [ -n \"$d\" ] && [ -x \"$d/leksah-cmd\" ]; then echo \"$d/leksah-cmd\"; "
              , "else command -v leksah-cmd; fi; exit 0" ]) [] mempty
  return $ case filter (not . null) (lines (BC.unpack out)) of
    (p : _) -> Just p
    []      -> Nothing

-- | What a session started on @host@ under launch token @token@ is told at
-- its SessionStart when no leksah there answers the hook (leksah-cmd prints
-- it: @~\/.leksah\/briefings\/TOKEN.txt@).
remoteLeaveBriefing :: Text -> Text -> Text -> IO ()
remoteLeaveBriefing host token note = (`catch` \(_ :: SomeException) -> return ()) $ do
  home <- remoteHomeDir host
  writeHostFile host (home </> ".leksah" </> "briefings" </> (T.unpack token <> ".txt")) False
    (TE.encodeUtf8 note)

-- | The agent CLIs @host@ has (Claude Code always: the remote agents need
-- it).  Answered at once from what was last found; looked for again, in the
-- background, when that is older than ten minutes — the menus ask while
-- they are being built.
remoteAvailableClis :: Text -> IO [AgentCli]
remoteAvailableClis host = do
  now <- getCurrentTime
  known <- M.lookup host <$> readIORef availRef
  let stale = maybe True (\(_, at) -> diffUTCTime now at > 600) known
  when stale $ do
    -- (marked as fresh at once, so one lookup runs at a time)
    atomicModifyIORef' availRef (\m -> (M.insert host (maybe [CliClaude] fst known, now) m, ()))
    void . forkIO . (`catch` \(_ :: SomeException) -> return ()) $ do
      (code, out, _) <- runSsh host "for c in codex agy; do command -v \"$c\" >/dev/null 2>&1 && echo \"$c\"; done; exit 0" [] mempty
      when (code == ExitSuccess) $ do
        let found = [ c | (w, c) <- [("codex", CliCodex), ("agy", CliAgy)], w `elem` lines (BC.unpack out) ]
        t <- getCurrentTime
        atomicModifyIORef' availRef (\m -> (M.insert host (CliClaude : found, t) m, ()))
  return (maybe [CliClaude] fst known)

{-# NOINLINE availRef #-}
availRef :: IORef (Map Text ([AgentCli], UTCTime))
availRef = unsafePerformIO (newIORef M.empty)

-- | Has session @sid@ of @cli@ on @host@ a conversation of its own to resume
-- (the host's index — @~\/.leksah\/\<kind\>-index.json@ — has its transcript)?
-- 'False' for a launch token that never reached its first prompt.
remoteCliStarted :: Text -> AgentCli -> Text -> IO Bool
remoteCliStarted host cli sid = (`catch` \(_ :: SomeException) -> return False) $
  maybe False (not . null . fst) . M.lookup sid <$> readIndex host cli

-- | agy's @--settings@ for a launch on a shared host (the shell there
-- expands @$HOME@): leksah's title command, as 'IDE.Web.Agy.agyLaunchSettings'.
remoteAgySettingsArg :: Text
remoteAgySettingsArg =
  "\"{\\\"title\\\":{\\\"type\\\":\\\"command\\\",\\\"command\\\":\\\"$HOME/.leksah/agy-title\\\"}}\""

--------------------------------------------------------------------------------
-- Records kept from the event log

-- | The live records of @cli@ on @host@ (its @~\/.leksah\/\<kind\>-sessions@).
remoteHookRecords :: Text -> AgentCli -> IO [HookLive]
remoteHookRecords host cli = (`catch` \(_ :: SomeException) -> return []) $ do
  (_, out, _) <- runSsh host
    "for f in \"$HOME/.leksah/$0-sessions\"/*.json; do [ -f \"$f\" ] && { cat \"$f\"; echo; }; done; exit 0"
    [cliTag cli] mempty
  return (mapMaybe hookRecordFromJson (BC.lines out))

-- | Write record @r@ on @host@ — and, while there, drop the records of
-- processes that have gone (an agy says nothing when it exits).
remoteWriteRecord :: Text -> AgentCli -> HookLive -> IO ()
remoteWriteRecord host cli r = (`catch` \(_ :: SomeException) -> return ()) $ do
  now <- realToFrac <$> getPOSIXTime
  void $ runSsh host
    (T.concat [ "d=\"$HOME/.leksah/$0-sessions\"; mkdir -p \"$d\" || exit 1; "
              , "t=\"$d/.$1.tmp.$$\"; cat > \"$t\" && mv \"$t\" \"$d/$1.json\"; "
              , "for f in \"$d\"/*.json; do p=$(basename \"$f\" .json); "
              , "kill -0 \"$p\" 2>/dev/null || rm -f \"$f\"; done; exit 0" ])
    [cliTag cli, T.pack (show (hlPid r))] (BL.toStrict (hookRecordJson (cliTag cli) now r))

-- | Drop @host@'s records of @cli@ for these processes.
remoteDropRecords :: Text -> AgentCli -> [Int] -> IO ()
remoteDropRecords _ _ [] = return ()
remoteDropRecords host cli pids = (`catch` \(_ :: SomeException) -> return ()) $
  void $ runSsh host "d=\"$HOME/.leksah/$0-sessions\"; for p in \"$@\"; do rm -f \"$d/$p.json\"; done"
    (cliTag cli : map (T.pack . show) pids) mempty

-- | Add session @sid@ to @host@'s index of @cli@ sessions (with its
-- directory and transcript; a known transcript is not forgotten for none).
remoteIndexAdd :: Text -> AgentCli -> Text -> FilePath -> FilePath -> IO ()
remoteIndexAdd host cli sid cwd transcript = (`catch` \(_ :: SomeException) -> return ()) $ do
  home <- remoteHomeDir host
  m <- readIndex host cli
  let tr = if null transcript then maybe "" fst (M.lookup sid m) else transcript
      m' = M.insert sid (tr, cwd) m
  unless (m' == m) $
    writeHostFile host (home </> ".leksah" </> (T.unpack (cliTag cli) <> "-index.json")) False
      . BL.toStrict . encode . object $
        [ Key.fromText k .= object [ "transcript" .= t, "cwd" .= c ] | (k, (t, c)) <- M.toList m' ]

-- | Has @host@'s index of @cli@ sessions got @sid@?
remoteIndexHas :: Text -> AgentCli -> Text -> IO Bool
remoteIndexHas host cli sid = (`catch` \(_ :: SomeException) -> return False) $
  M.member sid <$> readIndex host cli

readIndex :: Text -> AgentCli -> IO (Map Text (FilePath, FilePath))
readIndex host cli = do
  (_, out, _) <- runSsh host "cat \"$HOME/.leksah/$0-index.json\" 2>/dev/null; exit 0" [cliTag cli] mempty
  return . M.fromList $ case decodeStrict' out of
    Just (Object o) ->
      [ (Key.toText k, (T.unpack tr, T.unpack cw))
      | (k, e) <- KM.toList o
      , Just (tr, cw) <- [parseMaybe (withObject "e" $ \x ->
                              (,) <$> (fromMaybe "" <$> x .:? "transcript")
                                  <*> (fromMaybe "" <$> x .:? "cwd")) e] ]
    _ -> []

-- | Where agy keeps conversation @sid@'s transcript on @host@.
remoteAgyTranscript :: Text -> Text -> IO FilePath
remoteAgyTranscript host sid = do
  home <- remoteHomeDir host
  return (home </> ".gemini" </> "antigravity-cli" </> "brain" </> T.unpack sid
            </> ".system_generated" </> "logs" </> "transcript_full.jsonl")

--------------------------------------------------------------------------------

-- | Write a file on @host@ by renaming a sibling into place (@private@: only
-- its owner may read it — agy's settings.json).
writeHostFile :: Text -> FilePath -> Bool -> BC.ByteString -> IO ()
writeHostFile host path private bytes = do
  (code, _, err) <- runSsh host
    (T.concat [ "mkdir -p \"$(dirname \"$0\")\" || exit 1; "
              , if private then "umask 077; " else ""
              , "t=\"$0.tmp-leksah.$$\"; cat > \"$t\" && mv \"$t\" \"$0\"" ])
    [T.pack path] bytes
  unless (code == ExitSuccess) . ioError . userError $
    "could not write " <> path <> " on " <> T.unpack host <> ": " <> BC.unpack err

-- | Read a JSON object file on @host@ (empty when there is none), change it
-- and write it back if that changed it.  A file that is not a JSON object is
-- left alone (an error): what the user keeps there must not be lost.
modifyHostJson :: Text -> FilePath -> Bool -> (KM.KeyMap Value -> KM.KeyMap Value) -> IO ()
modifyHostJson host path private f = do
  (_, out, _) <- runSsh host "cat \"$0\" 2>/dev/null; exit 0" [T.pack path] mempty
  let bom = "\xef\xbb\xbf"
      body = fromMaybe out (BC.stripPrefix bom out)
  old <- if BC.null (BC.strip body) then return mempty else case decodeStrict' body of
    Just (Object o) -> return o
    _ -> ioError . userError $ path <> " on " <> T.unpack host
           <> " is not a JSON object leksah can read; leksah leaves it alone"
  let new = f old
  unless (new == old) $ writeHostFile host path private (BL.toStrict (encode (Object new)))

hostShell :: Text -> Text -> IO (Either Text ())
hostShell host script = do
  (code, out, err) <- runSsh host script [] mempty
  return $ case code of
    ExitSuccess -> Right ()
    _ -> Left (T.strip (TE.decodeUtf8With lenientDecode (if BC.null err then out else err)))

shq :: Text -> Text
shq t = "'" <> T.replace "'" "'\\''" t <> "'"
