{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- SPDX-License-Identifier: Apache-2.0

-- | The Claude Code agents running on OTHER machines — the hosts whose leksah
-- tmux server this leksah shares (@remote.tmuxSockets@).
--
-- Everything about an agent lives on the machine it runs on: Claude Code's own
-- @~\/.claude@, and the agent stores ("IDE.Web.SharedStore") that the leksah
-- there — if one is running — and every leksah showing that machine read and
-- write.  So nothing here depends on a leksah running on the host.
--
-- A poll ('startRemoteAgentsPoll') takes one snapshot of each host every few
-- seconds in ONE ssh round trip: the session files, a @ps@ pid → parent table,
-- the leksah tmux server's panes, the agent stores (only when their checksums
-- changed) and each live session's transcript tail (only when it changed).
-- The rest of leksah reads the latest snapshots: the status poll adds the
-- remote sessions to its rows ("IDE.Web.ClaudeStatus"), the Agents pane its
-- cards ("IDE.Web.AgentInfo"), and the remote Terminals tree its agent names.
--
-- A session no-one has named yet is named HERE, in its own machine's store
-- (a compare-and-swap write — 'remoteModify'), so whichever leksah sees it
-- first, both agree.
module IDE.Web.RemoteAgents
  ( startRemoteAgentsPoll
  , remoteLiveSessions
  , remoteGist
  , remoteActivity
  , remoteInfoBytes
  , remotePaneSessions
  , remotePaneOf
  , remoteHostOf
  , remoteTranscriptTail
  , remoteAgentName
  , remoteStartingPane
  , setRemoteEventHandler
  , startLocalEventsPoll
  , HostStatus(..)
  , remoteHostStatus
  , startAliveWriter
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forM, forM_, forever, unless, void, when)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Aeson (Value(..), decode')
import qualified Data.Aeson.KeyMap as KM (lookup)
import Data.Foldable (toList)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import System.IO (IOMode(ReadMode), SeekMode(AbsoluteSeek), hSeek, withFile)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, getFileSize, getHomeDirectory, renameFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

import IDE.App (appConfig, getGlobalApp)
import IDE.Config (Config(..), RemoteC(..), currentConfig)
import IDE.Utils.RemoteExec (runSsh)
import IDE.Web.AgentNames
       (NameRec(..), nameSessions, namesHeldElsewhere, noteRemoteName, setRemoteNames)
import IDE.Web.Claude
       (ClaudeLive(..), TranscriptGist(..), gistOfTail, latestActivityOf,
        parseSessionFile, pidAncestors)
import IDE.Web.SharedStore (remoteModifyJSON)
import IDE.Web.TmuxServer (remoteSocketArgs, remoteTmux)

-- | One host, as last seen.
data HostSnapshot = HostSnapshot
  { hsAt       :: UTCTime              -- ^ when it was last read successfully
  , hsLive     :: [ClaudeLive]         -- ^ its running sessions
  , hsPanes    :: Map Text Text        -- ^ tmux pane id → the session it shows
  , hsPaneAt   :: Map Text (Text, Text) -- ^ tmux pane id → its tmux session and
                                       --   window ids (raw, on the host)
  , hsRunKeys  :: Map Text Text        -- ^ tmux pane id → its @\@leksah_run@ key
  , hsNames    :: (Text, Map Text NameRec)   -- ^ its name store, and its cksum
  , hsInfos    :: (Text, LBS.ByteString)     -- ^ its agents.json, and its cksum
  , hsEventsAt :: Integer              -- ^ how far into the host's event log
                                       --   (@~\/.leksah\/agent-events.jsonl@)
                                       --   this leksah has read; -1 = not yet
  , hsTails    :: Map Text (Text, TranscriptGist, Maybe Text)
                                       -- ^ session → (transcript stamp, gist,
                                       --   what it is doing)
  , hsHome     :: Maybe Text           -- ^ its @$HOME@
  , hsLeksahAge :: Maybe Integer       -- ^ seconds since a leksah there last
                                       --   wrote @~\/.leksah\/alive@
                                       --   ('startAliveWriter'), on the host's
                                       --   own clock; 'Nothing' = never
  }

emptySnapshot :: UTCTime -> HostSnapshot
emptySnapshot t = HostSnapshot t [] M.empty M.empty M.empty ("", M.empty) ("", "") (-1) M.empty Nothing Nothing

{-# NOINLINE snapshotsRef #-}
snapshotsRef :: IORef (Map Text HostSnapshot)
snapshotsRef = unsafePerformIO (newIORef M.empty)

-- | A host not read for this long counts as gone: its sessions drop out.
staleAfter :: Double
staleAfter = 45

-- | Every session running on the shared hosts (from fresh snapshots only).
remoteLiveSessions :: IO [ClaudeLive]
remoteLiveSessions = do
  now <- getCurrentTime
  m <- readIORef snapshotsRef
  return [ l | hs <- M.elems m, fresh now hs, l <- hsLive hs ]
  where fresh now hs = realToFrac (diffUTCTime now (hsAt hs)) < staleAfter

-- | What the last look at a shared host says about the host itself.
data HostStatus = HostStatus
  { hstFresh  :: Bool        -- ^ read within 'staleAfter' (else: unreachable)
  , hstLeksah :: Bool        -- ^ a leksah there said it was alive lately
  , hstHome   :: Maybe Text  -- ^ its home directory
  } deriving (Eq, Show)

-- | Every shared host this leksah has read at least once.
remoteHostStatus :: IO (Map Text HostStatus)
remoteHostStatus = do
  now <- getCurrentTime
  m <- readIORef snapshotsRef
  return $ flip fmap m $ \hs ->
    let fresh = realToFrac (diffUTCTime now (hsAt hs)) < staleAfter
    in HostStatus fresh (fresh && maybe False (< aliveWithin) (hsLeksahAge hs)) (hsHome hs)

-- | A leksah that has not touched its @alive@ file for this long is gone.
aliveWithin :: Integer
aliveWithin = 30

-- | Say this leksah is running — @~\/.leksah\/alive@, the time, every 10s —
-- for a leksah on another machine showing this one ('remoteHostStatus').
startAliveWriter :: IO ThreadId
startAliveWriter = forkIO $ do
  home <- getHomeDirectory
  let f = home </> ".leksah" </> "alive"
  createDirectoryIfMissing True (home </> ".leksah") `catch` \(_ :: SomeException) -> return ()
  forever $ do
    (`catch` \(_ :: SomeException) -> return ()) $ do
      t <- round <$> getPOSIXTime :: IO Integer
      writeFile (f <> ".tmp") (show t <> "\n")
      renameFile (f <> ".tmp") f
    threadDelay 10000000

remoteGist :: Text -> IO (Maybe TranscriptGist)
remoteGist sid = fmap (\(_, g, _) -> g) . lookupTail sid <$> readIORef snapshotsRef

-- | What a remote session is doing, from its transcript tail (its last tool
-- call, or Thinking \/ Writing).
remoteActivity :: Text -> IO (Maybe Text)
remoteActivity sid = (>>= \(_, _, a) -> a) . lookupTail sid <$> readIORef snapshotsRef

lookupTail :: Text -> Map Text HostSnapshot -> Maybe (Text, TranscriptGist, Maybe Text)
lookupTail sid m = case mapMaybe (M.lookup sid . hsTails) (M.elems m) of
  (x : _) -> Just x
  []      -> Nothing

-- | Each shared host's @agents.json@, as it was last read.
remoteInfoBytes :: IO [(Text, LBS.ByteString)]
remoteInfoBytes = map (\(h, hs) -> (h, snd (hsInfos hs))) . M.toList <$> readIORef snapshotsRef

-- | @host@'s tmux panes that show an agent: pane id → session.
remotePaneSessions :: Text -> IO (Map Text Text)
remotePaneSessions host = maybe M.empty hsPanes . M.lookup host <$> readIORef snapshotsRef

-- | A remote session's pane, as server-qualified tmux ids
-- (@ssh:\/\/HOST#$S@, @ssh:\/\/HOST#\@W@, @ssh:\/\/HOST#%P@): the shape every
-- server-aware tmux helper ('IDE.Web.TmuxServer.tmuxOnIds') takes, so
-- sending keys, capturing the screen or killing the pane reach the host.
remotePaneOf :: Text -> IO (Maybe (Text, Text, Text))
remotePaneOf sid = do
  m <- readIORef snapshotsRef
  return $ listToMaybe
    [ (q h s, q h w, q h pane)
    | (h, hs) <- M.toList m, (pane, sid') <- M.toList (hsPanes hs), sid' == sid
    , Just (s, w) <- [M.lookup pane (hsPaneAt hs)] ]
  where q h x = "ssh://" <> h <> "#" <> x

-- | A pane on a shared host that was started for session @sid@ (its run key
-- names it) but whose session has not registered yet — nearly always a
-- first-run question there (the folder-trust one).  The qualified pane id.
remoteStartingPane :: Text -> IO (Maybe Text)
remoteStartingPane sid = do
  m <- readIORef snapshotsRef
  return $ listToMaybe
    [ "ssh://" <> h <> "#" <> pane
    | (h, hs) <- M.toList m, not (any ((== sid) . clSession) (hsLive hs))
    , (pane, key) <- M.toList (hsRunKeys hs), ("#claude#" <> sid) `T.isSuffixOf` key ]

-- | The host a remote session runs on.
remoteHostOf :: Text -> IO (Maybe Text)
remoteHostOf sid = do
  m <- readIORef snapshotsRef
  return $ listToMaybe [ h | (h, hs) <- M.toList m, any ((== sid) . clSession) (hsLive hs) ]

-- | The last 512KB of a remote session's transcript, fetched now (one ssh
-- round trip) into @~\/.leksah\/remote-tails\/SID.jsonl@ — a local file the
-- transcript readers take as they take any other.  'Nothing' when the session
-- is on no shared host, or its transcript cannot be read.
remoteTranscriptTail :: Text -> IO (Maybe FilePath)
remoteTranscriptTail sid = remoteHostOf sid >>= \case
  Nothing   -> return Nothing
  Just host -> (`catch` \(_ :: SomeException) -> return Nothing) $ do
    (code, out, _) <- runSsh host
      "for x in \"$HOME\"/.claude/projects/*/\"$0\".jsonl; do [ -f \"$x\" ] && exec tail -c 524288 \"$x\"; done; exit 3"
      [sid] mempty
    if code /= ExitSuccess then return Nothing else do
      home <- getHomeDirectory
      let dir = home </> ".leksah" </> "remote-tails"
          f   = dir </> (T.unpack sid <> ".jsonl")
      createDirectoryIfMissing True dir
      -- (from the first whole line: tail cut the first one short)
      BS.writeFile f (BS.drop 1 (BC.dropWhile (/= '\n') out))
      return (Just f)

-- | Snapshot every shared host every few seconds, forever.
startRemoteAgentsPoll :: IO ThreadId
startRemoteAgentsPoll = forkIO . forever $ do
  hosts <- sharedHosts
  -- (all at once: one slow or unreachable host must not hold up the others)
  dones <- forM hosts $ \h -> do
    done <- newEmptyMVar
    _ <- forkIO $ (pollHost h `catch` \(_ :: SomeException) -> return ()) `finally` putMVar done ()
    return done
  mapM_ takeMVar dones
  -- (a host that went from the setting is forgotten)
  atomicModifyIORef' snapshotsRef (\m -> (M.restrictKeys m (S.fromList hosts), ()))
  threadDelay 3000000

-- | The hosts whose leksah tmux server this leksah shares.
sharedHosts :: IO [Text]
sharedHosts = getGlobalApp >>= \case
  Nothing  -> return []
  Just app -> M.keys . rcTmuxSockets . cfgRemote <$> currentConfig (appConfig app)

pollHost :: Text -> IO ()
pollHost host = remoteTmux host >>= \case
  Left _ -> return ()
  Right (bin, _) -> do
    sock <- remoteSocketArgs host
    prev <- M.lookup host <$> readIORef snapshotsRef
    now  <- getCurrentTime
    let p = fromMaybe (emptySnapshot now) prev
        sockName = case sock of ["-L", n] -> T.pack n; _ -> ""
        known = [ sid <> "=" <> st | (sid, (st, _, _)) <- M.toList (hsTails p) ]
    (code, out, _) <- runSsh host snapshotScript
      ([ T.pack bin, sockName, fst (hsNames p), fst (hsInfos p)
       , T.pack (show (hsEventsAt p)) ] <> known) mempty
    unless (code /= ExitSuccess) $ do
      let fs = frames out
          (eventsAt, eventLines) = newEvents (hsEventsAt p) fs
          hs = (parseSnapshot host now p fs) { hsEventsAt = eventsAt }
      atomicModifyIORef' snapshotsRef (\m -> (M.insert host hs m, ()))
      -- What the agents there did since the last look: into the same records
      -- their own leksah would keep ('setRemoteEventHandler').
      -- (On first contact the lines are history, already recorded, if at all,
      -- by whoever was watching then: only the logged commands are applied,
      -- each once — 'IDE.Web.AgentInfo.applyLoggedCommand'.)
      unless (null eventLines) $ do
        handler <- readIORef eventHandlerRef
        let history = hsEventsAt p < 0
        forM_ eventLines $ \l -> handler host history l `catch` \(_ :: SomeException) -> return ()
      setRemoteNames host (snd (hsNames hs))
      nameUnnamed host hs

-- | Name session @sid@ in @host@'s own store (keeping the name it has, if it
-- has one) — a launch on that host, which needs the name before the agent
-- starts — and remember it here at once.
remoteAgentName :: Text -> Text -> IO (Either Text Text)
remoteAgentName host sid = do
  now <- realToFrac <$> getPOSIXTime
  elsewhere <- namesHeldElsewhere (Just host)
  held <- maybe S.empty hostHeld . M.lookup host <$> readIORef snapshotsRef
  r <- remoteModifyJSON host "agent-names.json" M.empty $ \m -> do
    let m' = nameSessions now held elsewhere [sid] m
    return (m', maybe "" nrName (M.lookup sid m'))
  case r of
    Right n | not (T.null n) -> noteRemoteName host sid n >> return (Right n)
    Right _  -> return (Left "the host's name store gave no name")
    Left err -> return (Left err)

-- | The new whole lines of a host's event log in this snapshot (frame @E@,
-- META the offset it starts at), and the offset to ask from next time.  A
-- line the snapshot cut short is left for the next one.
newEvents :: Integer -> [(BS.ByteString, BS.ByteString, BS.ByteString)] -> (Integer, [BS.ByteString])
newEvents was fs = case [ (meta, b) | (t, meta, b) <- fs, t == "E" ] of
  ((meta, b) : _) | Just off <- readMaybe (BC.unpack meta) ->
    let complete = BS.take (maybe 0 (+ 1) (BC.elemIndexEnd '\n' b)) b
    in (off + fromIntegral (BS.length complete), filter (not . BS.null) (BC.lines complete))
  _ -> (was, [])

-- | Who folds a machine's events into leksah's records: set once at startup
-- by "IDE.Web.Agent", which knows how ('IDE.Web.Agent.remoteAgentEvent').  The
-- machine (@""@: this one — 'startLocalEventsPoll'), whether the line is
-- history (read on first contact: only commands count), and the line.
setRemoteEventHandler :: (Text -> Bool -> BS.ByteString -> IO ()) -> IO ()
setRemoteEventHandler h = atomicModifyIORef' eventHandlerRef (const (h, ()))

{-# NOINLINE eventHandlerRef #-}
eventHandlerRef :: IORef (Text -> Bool -> BS.ByteString -> IO ())
eventHandlerRef = unsafePerformIO (newIORef (\_ _ _ -> return ()))

-- | Follow THIS machine's own event log for the commands an agent logged
-- while no leksah answered here — this leksah restarting, say
-- (@leksah-cmd agent describe@ \/ @done@ — see @agentOffline@ in leksah-cmd).
-- Hook events need no replay here: a running leksah got them through its
-- socket, and the session files say the rest.  From the last 64KB at start,
-- then every few seconds.
startLocalEventsPoll :: IO ThreadId
startLocalEventsPoll = forkIO $ do
  home <- getHomeDirectory
  let f = home </> ".leksah" </> "agent-events.jsonl"
  offRef <- newIORef (-1 :: Integer)
  forever $ do
    (`catch` \(_ :: SomeException) -> return ()) $ do
      size <- (`catch` \(_ :: SomeException) -> return 0) (getFileSize f)
      off0 <- readIORef offRef
      let off | off0 < 0    = max 0 (size - 65536)
              | off0 > size = 0          -- rotated
              | otherwise   = off0
      when (size > off) $ do
        bytes <- withFile f ReadMode $ \h -> hSeek h AbsoluteSeek off >> BS.hGet h (fromIntegral (size - off))
        let complete = BS.take (maybe 0 (+ 1) (BC.elemIndexEnd '\n' bytes)) bytes
        handler <- readIORef eventHandlerRef
        forM_ (filter (not . BS.null) (BC.lines complete)) $ \l ->
          handler "" True l `catch` \(_ :: SomeException) -> return ()
        writeIORef offRef (off + fromIntegral (BS.length complete))
      when (size <= off) $ writeIORef offRef off
    threadDelay 3000000

-- | Name, in the host's own store, its live sessions that have no name there.
--
-- Every live session is touched, named or not: with no leksah running on the
-- host, nothing else keeps a named agent's last-seen time fresh, and its name
-- would fall out of reservation and be handed to a newcomer.  (Written only
-- when something changed: a new name, or a last-seen older than the slack.)
nameUnnamed :: Text -> HostSnapshot -> IO ()
nameUnnamed host hs = do
  let sids = map clSession (hsLive hs)
      known = snd (hsNames hs)
  now <- realToFrac <$> getPOSIXTime
  let stale = [ s | s <- sids, maybe True (\r -> now - nrSeen r >= 600) (M.lookup s known) ]
  unless (null stale) $ do
    elsewhere <- namesHeldElsewhere (Just host)
    void . remoteModifyJSON host "agent-names.json" M.empty $ \m ->
      return (nameSessions now (hostHeld hs) elsewhere sids m, ())

-- | The sessions @host@'s agent store marks suspended or interrupted: their
-- names stay reserved there ('nameSessions').  (Read from the raw store, as
-- the record type belongs to "IDE.Web.AgentInfo".)
hostHeld :: HostSnapshot -> S.Set Text
hostHeld hs = case decode' (snd (hsInfos hs)) of
  Just (Array xs) -> S.fromList
    [ s | Object o <- toList xs
        , Just (String s) <- [KM.lookup "session" o]
        , any (\k -> maybe False (/= Null) (KM.lookup k o)) ["suspended", "interrupted"] ]
  _ -> S.empty

-- | One snapshot from the frames of the script's output.
parseSnapshot :: Text -> UTCTime -> HostSnapshot -> [(BS.ByteString, BS.ByteString, BS.ByteString)]
              -> HostSnapshot
parseSnapshot host now p fs = HostSnapshot
  { hsAt    = now
  , hsLive  = live
  , hsPanes = panes
  , hsPaneAt = paneAt
  , hsRunKeys = M.fromList [ (T.pack (BC.unpack pid), T.pack (BC.unpack (BC.unwords key)))
                           | (pid : _ : _ : _ : key) <- paneRows, not (null key) ]
  , hsNames = names
  , hsInfos = infos
  , hsTails = tails
  , hsHome = case [ T.strip (decodeUtf8With lenientDecode b) | (t, _, b) <- fs, t == "H" ] of
      (h : _) | not (T.null h) -> Just h
      _ -> Nothing
  , hsLeksahAge = listToMaybe [ n | (t, _, b) <- fs, t == "A"
                                  , Just n <- [readMaybe (BC.unpack (BC.strip b))] ]
  }
  where
    sessions = mapMaybe (\(t, _, b) -> if t == "S" then parseSessionFile (Just host) b else Nothing) fs
    parents  = M.fromList [ (pid, ppid) | (t, _, b) <- fs, t == "P"
                                        , [a, b'] <- map BC.words (BC.lines b)
                                        , Just pid <- [readMaybe (BC.unpack a)]
                                        , Just ppid <- [readMaybe (BC.unpack b')] ]
    alive    = M.keysSet parents
    live     = [ l | l <- sessions, clPid l `S.member` alive ]
    -- A pane shows the session whose claude process descends from the pane's
    -- own process.
    owners   = M.fromList [ (a, clSession l) | l <- live
                                            , a <- clPid l : pidAncestors parents (clPid l) ]
    paneRows = [ ws | (t, _, b) <- fs, t == "T", ws <- map BC.words (BC.lines b) ]
    panes    = M.fromList [ (T.pack (BC.unpack pid), sid)
                          | (pid : ppid : _) <- paneRows
                          , Just pp <- [readMaybe (BC.unpack ppid)]
                          , Just sid <- [M.lookup pp owners] ]
    paneAt   = M.fromList [ (T.pack (BC.unpack pid), (T.pack (BC.unpack sess), T.pack (BC.unpack win)))
                          | (pid : _ : sess : win : _) <- paneRows ]
    store tag decodeIt keep = case [ (meta, b) | (t, meta, b) <- fs, t == tag ] of
      ((meta, b) : _)
        | Just c <- BC.stripPrefix "=" meta -> (dec c, snd keep)   -- unchanged
        | otherwise -> (dec meta, decodeIt b)
      [] -> ("", decodeIt "")        -- no such file there
      where dec = T.pack . BC.unpack
    names = store "N" (fromMaybe M.empty . decode' . LBS.fromStrict) (hsNames p)
    infos = store "I" LBS.fromStrict (hsInfos p)
    liveIds = S.fromList (map clSession live)
    tails = M.fromList
      [ (sid, entry)
      | (t, meta, b) <- fs, t == "X"
      , let (sidB, rest) = BC.break (== '=') meta
            sid = T.pack (BC.unpack sidB)
            (stB, same) = BC.break (== '=') (BC.drop 1 rest)
            st = T.pack (BC.unpack stB)
      , sid `S.member` liveIds
      , Just entry <- [ if same == "=same"
                          then M.lookup sid (hsTails p)
                          else Just (fresh sid st b) ] ]
    -- A changed tail: its gist and activity.  A tail that no longer reaches
    -- a title or anything said keeps the last ones known.
    fresh sid st b =
      let ls = drop 1 (BC.lines b)          -- (the first line is cut short)
          g  = gistOfTail ls
          g' = case M.lookup sid (hsTails p) of
                 Just (_, o, _) -> g { tgTitle    = maybe (tgTitle o) Just (tgTitle g)
                                     , tgLastSaid = maybe (tgLastSaid o) Just (tgLastSaid g) }
                 Nothing -> g
      in (st, g', latestActivityOf ls)

-- | Split the script's output into @(tag, meta, body)@ frames: a header line
-- @\@\@TAG META LENGTH@, then exactly LENGTH bytes.
frames :: BS.ByteString -> [(BS.ByteString, BS.ByteString, BS.ByteString)]
frames bs
  | Just rest <- BC.stripPrefix "@@" bs
  , (hdr, afterHdr) <- BC.break (== '\n') rest
  , [tag, meta, lenB] <- BC.words hdr
  , Just n <- readMaybe (BC.unpack lenB)
  = let body = BS.take n (BS.drop 1 afterHdr)
    in (tag, meta, body) : frames (BS.drop (n + 1) afterHdr)
  | otherwise = []

-- | @$0@ tmux, @$1@ its socket name (or empty), @$2@ \/ @$3@ the cksums of
-- @agent-names.json@ \/ @agents.json@ already known, then @SID=STAMP@ for every
-- transcript tail already known.  Frames: S session file, P @ps@ table,
-- T tmux panes (@pane_id pane_pid@), N \/ I the stores (META @=CKSUM@ and no
-- body when unchanged), X a transcript tail (META @SID=STAMP@, or
-- @SID=STAMP=same@ and no body when unchanged).
snapshotScript :: Text
snapshotScript = T.concat
  [ "T=\"$0\"; S=\"$1\"; NK=\"$2\"; IK=\"$3\"; EO=\"$4\"; shift 4; "
  , "tx() { if [ -n \"$S\" ]; then \"$T\" -L \"$S\" \"$@\"; else \"$T\" \"$@\"; fi; }; "
  , "tmp=$(mktemp) || exit 1; trap 'rm -f \"$tmp\"' EXIT; "
  , "frame() { printf '@@%s %s %s\\n' \"$1\" \"$2\" \"$(wc -c < \"$3\" | tr -d ' ')\"; cat \"$3\"; }; "
  , "for f in \"$HOME\"/.claude/sessions/*.json; do [ -f \"$f\" ] && frame S - \"$f\"; done; "
  , "ps -Ao pid=,ppid= > \"$tmp\"; frame P - \"$tmp\"; "
  , "tx list-panes -a -f '#{==:#{@leksah_viewer},}' -F '#{pane_id} #{pane_pid} #{session_id} #{window_id} #{@leksah_run}' > \"$tmp\" 2>/dev/null; "
  , "frame T - \"$tmp\"; "
  , "st() { f=\"$HOME/.leksah-0.17/$1\"; [ -f \"$f\" ] || return 0; "
  ,   "c=$(cksum < \"$f\" | cut -d' ' -f1,2 | tr ' ' :); "
  ,   "if [ \"$c\" = \"$2\" ]; then : > \"$tmp\"; frame \"$3\" \"=$c\" \"$tmp\"; "
  ,   "else frame \"$3\" \"$c\" \"$f\"; fi; }; "
  , "st agent-names.json \"$NK\" N; st agents.json \"$IK\" I; "
  -- Its home (where a new agent there starts), and how long ago a leksah
  -- there last said it was alive — worked out on the host, so the two
  -- machines' clocks need not agree.
  , "printf '%s\\n' \"$HOME\" > \"$tmp\"; frame H - \"$tmp\"; "
  , "al=$(head -c 20 \"$HOME/.leksah/alive\" 2>/dev/null | tr -cd 0-9); "
  , "if [ -n \"$al\" ]; then echo $(( $(date +%s) - al )) > \"$tmp\"; frame A - \"$tmp\"; fi; "
  -- The event log from where this leksah left off (a log that shrank was
  -- rotated: from its start); on first contact, only its last 64KB.  At
  -- most 256KB a time.
  , "ev=\"$HOME/.leksah/agent-events.jsonl\"; if [ -f \"$ev\" ]; then "
  ,   "sz=$(wc -c < \"$ev\" | tr -d ' '); off=$EO; "
  ,   "if [ \"$off\" -lt 0 ]; then off=$((sz > 65536 ? sz - 65536 : 0)); fi; "
  ,   "if [ \"$off\" -gt \"$sz\" ]; then off=0; fi; "
  ,   "tail -c +$((off + 1)) \"$ev\" | head -c 262144 > \"$tmp\"; frame E \"$off\" \"$tmp\"; fi; "
  , "for f in \"$HOME\"/.claude/sessions/*.json; do [ -f \"$f\" ] || continue; "
  ,   "sid=$(sed -n 's/.*\"sessionId\" *: *\"\\([^\"]*\\)\".*/\\1/p' \"$f\" | head -1); "
  ,   "[ -n \"$sid\" ] || continue; "
  ,   "for x in \"$HOME\"/.claude/projects/*/\"$sid\".jsonl; do [ -f \"$x\" ] || continue; "
  ,     "stamp=$(stat -c '%s:%Y' \"$x\" 2>/dev/null || stat -f '%z:%m' \"$x\"); "
  ,     "k=; for a in \"$@\"; do [ \"$a\" = \"$sid=$stamp\" ] && k=1; done; "
  ,     "if [ -n \"$k\" ]; then : > \"$tmp\"; frame X \"$sid=$stamp=same\" \"$tmp\"; "
  ,     "else tail -c 131072 \"$x\" > \"$tmp\"; frame X \"$sid=$stamp\" \"$tmp\"; fi; break; "
  ,   "done; "
  , "done" ]
