{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Live-session records that leksah's own hooks keep, for the agent CLIs
-- that keep none themselves (Codex, Antigravity — "IDE.Web.Codex",
-- "IDE.Web.Agy").
--
-- Claude Code writes @~\/.claude\/sessions\/\<pid\>.json@ for every running
-- session, which is how leksah knows what is running and what it is doing.
-- The other CLIs have hooks shaped much like Claude Code's but no such file,
-- so leksah's hook handler writes one per session in the same shape:
-- @~\/.leksah\/\<kind\>-sessions\/\<pid\>.json@ — and "IDE.Web.Claude" reads them
-- beside Claude Code's, so such a session is one more live session to
-- everything downstream.  An index (@~\/.leksah\/\<kind\>-index.json@) remembers
-- every session the hooks have seen start, with its transcript and directory,
-- which is what makes an id one of that CLI's once the session has exited.
--
-- One 'HookStore' per CLI.  FS/process only.
module IDE.Web.HookSessions
  ( HookStore
  , hsKind
  , newHookStore
  , HookLive(..)
    -- * Live sessions
  , hookLiveRecords
  , hookRecordFor
  , hookProcessOf
  , hookSessionStart
  , hookKnownPid
  , hookClaimLaunch
  , hookRememberLaunch
  , hookSetStatus
  , hookSessionEnd
    -- * Sessions the hooks have seen
  , hookIsSession
  , hookSessionDir
  , hookTranscriptOf
  , hookStarted
    -- * Files
  , leksahDir
  , atomicWrite
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forM, unless)
import Data.Aeson (Value(..), (.=), (.:?), decodeStrict', encode, object, withObject)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory
       (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
        getHomeDirectory, listDirectory, removeFile, renameFile)
import System.FilePath ((</>), takeExtension, dropTrailingPathSeparator)
import IDE.Utils.Spawn (readProcess)
import Text.Read (readMaybe)

leksahDir :: IO FilePath
leksahDir = (</> ".leksah") <$> getHomeDirectory

-- | The records and the index are read on every status poll, for every
-- session — mostly Claude ones that can never match — so they are kept in
-- memory.  Only leksah writes them (its hook handler and its discovery, in
-- this process), and every write drops the copy ('invalidate').  The copy is
-- versioned: a read that started before a write must not store what it read
-- after the write has dropped the copy, or the cache would keep the old state
-- (a turn's final "busy", say) until something else is written.
type Cache a = IORef (Int, Maybe a)

cached :: Cache a -> IO a -> IO a
cached ref load = readIORef ref >>= \case
  (_, Just x)  -> return x
  (v, Nothing) -> do
    x <- load
    atomicModifyIORef' ref (\(v', c) -> if v' == v then ((v', Just x), ()) else ((v', c), ()))
    return x

invalidate :: Cache a -> IO ()
invalidate ref = atomicModifyIORef' ref (\(v, _) -> ((v + 1, Nothing), ()))

-- | One CLI's records.  Make ONE per CLI, at top level ('newHookStore' under
-- a NOINLINE 'System.IO.Unsafe.unsafePerformIO'): the caches and locks are
-- what keep concurrent hooks from losing each other's writes.
data HookStore = HookStore
  { hsKind       :: Text                     -- ^ @codex@, @agy@: names the files
  , hsRecords    :: Cache [HookLive]
  , hsIndex      :: Cache (Map Text (FilePath, FilePath))
  , hsRecordLock :: MVar ()                  -- ^ hooks arrive concurrently (from
                                             --   several sessions at once): one
                                             --   record writer at a time
  , hsIndexLock  :: MVar ()                  -- ^ … and one index writer
  , hsLastOfPid  :: IORef (Map Int Text)     -- ^ what each process last started
  }

newHookStore :: Text -> IO HookStore
newHookStore kind = HookStore kind
  <$> newIORef (0, Nothing) <*> newIORef (0, Nothing)
  <*> newMVar () <*> newMVar () <*> newIORef M.empty

--------------------------------------------------------------------------------
-- Live sessions

-- | One running session, as leksah's hooks recorded it.
data HookLive = HookLive
  { hlPid        :: Int
  , hlSession    :: Text
  , hlDir        :: FilePath
  , hlStatus     :: Text        -- ^ idle | busy | waiting
  , hlWaitingFor :: Maybe Text
  , hlTranscript :: Maybe FilePath
  } deriving (Eq, Show)

liveDir :: HookStore -> IO FilePath
liveDir st = (</> (T.unpack (hsKind st) <> "-sessions")) <$> leksahDir

-- | Every hook-written live record (pid liveness is the reader's business —
-- 'IDE.Web.Claude.claudeLiveBySession' checks it with one @ps@).
hookLiveRecords :: HookStore -> IO [HookLive]
hookLiveRecords st = cached (hsRecords st) (readRecordsFromDisk st)

removeRecordFile :: HookStore -> Int -> IO ()
removeRecordFile st pid = do
  d <- liveDir st
  removeFile (d </> (show pid <> ".json")) `catch` \(_ :: SomeException) -> return ()
  invalidate (hsRecords st)

readRecordsFromDisk :: HookStore -> IO [HookLive]
readRecordsFromDisk st = (`catch` \(_ :: SomeException) -> return []) $ do
  d <- liveDir st
  ex <- doesDirectoryExist d
  if not ex then return [] else do
    fs <- filter ((== ".json") . takeExtension) <$> listDirectory d
    concat <$> forM fs (\f -> maybe [] (: []) <$> readRecord (d </> f))

readRecord :: FilePath -> IO (Maybe HookLive)
readRecord f = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  v <- decodeStrict' <$> BS.readFile f
  return $ v >>= parseMaybe (withObject "record" $ \o -> do
    p  <- o .:? "pid"
    s  <- o .:? "sessionId"
    cw <- o .:? "cwd"
    sts <- o .:? "status"
    wf <- o .:? "waitingFor"
    tr <- o .:? "transcript"
    case (p, s) of
      (Just p', Just s') -> return HookLive
        { hlPid = p', hlSession = s'
        , hlDir = maybe "" (dropTrailingPathSeparator . T.unpack) cw
        , hlStatus = fromMaybe "idle" sts
        , hlWaitingFor = mfilterT wf
        , hlTranscript = T.unpack <$> mfilterT tr }
      _ -> fail "no pid/session")
  where mfilterT = (>>= \t -> if T.null t then Nothing else Just t)

writeRecord :: HookStore -> HookLive -> IO ()
writeRecord st r = do
  d <- liveDir st
  createDirectoryIfMissing True d
  now <- realToFrac . utcTimeToPOSIXSeconds <$> getCurrentTime :: IO Double
  atomicWrite (d </> (show (hlPid r) <> ".json")) . encode $ object
    [ "pid" .= hlPid r, "sessionId" .= hlSession r, "cwd" .= hlDir r
    , "status" .= hlStatus r, "waitingFor" .= hlWaitingFor r
    , "transcript" .= hlTranscript r, "kind" .= hsKind st
    , "updatedAt" .= now ]
  invalidate (hsRecords st)

-- | The live record for session @sid@, if one is open.
hookRecordFor :: HookStore -> Text -> IO (Maybe HookLive)
hookRecordFor st sid = (\rs -> case [ r | r <- rs, hlSession r == sid ] of
                                   (r : _) -> Just r
                                   []      -> Nothing) <$> hookLiveRecords st

-- | The CLI process a hook runs under: the nearest ancestor of @pid@ (the
-- hook client) whose program @isProg@ says is the CLI's.
hookProcessOf :: (FilePath -> Bool) -> Int -> IO (Maybe (Int, String))
hookProcessOf isProg pid = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  out <- readProcess "ps" ["-Ao", "pid=,ppid=,args="] ""
  let procs = M.fromList
        [ (p, (q, args)) | l <- lines out
                         , (pT : qT : args@(_ : _)) <- [words l]
                         , Just p <- [readMaybe pT], Just q <- [readMaybe qT] ]
      go :: Int -> Int -> Maybe (Int, String)
      go 0 _ = Nothing
      go n p = case M.lookup p procs of
        Just (q, args@(exe : _)) | isProg exe -> Just (p, unwords args)
                      | q > 1       -> go (n - 1) q
        _ -> Nothing
  return (go (14 :: Int) pid)

-- | A session started in process @pid@ (a new one, a resume, or the fresh id
-- a @\/clear@ gives the same process): its record now says so, with
-- @status@.  Answers the session that process was running before, if it was
-- another.  Records of processes that are gone are tidied away while we are
-- here.
hookSessionStart :: HookStore -> Int -> Text -> FilePath -> Maybe FilePath -> Text -> IO (Maybe Text)
hookSessionStart st pid sid cwd transcript status = withMVar (hsRecordLock st) $ \_ -> do
  rs <- hookLiveRecords st
  -- The process's previous session: its record, else what it last started
  -- (a /clear can report the old session's end first, which takes its
  -- record away — so the process's last session is remembered too).
  remembered <- M.lookup pid <$> readIORef (hsLastOfPid st)
  let prev = [ hlSession r | r <- rs, hlPid r == pid, hlSession r /= sid ]
             <> [ o | Just o <- [remembered], o /= sid ]
  alive <- livePids
  -- (and any other process's record of this same session: a conversation
  -- runs in one process at a time — resumed in a new one, the old one is on
  -- its way out — and 'hookRecordFor' must find the new one)
  mapM_ (removeRecordFile st . hlPid)
        [ r | r <- rs, hlPid r /= pid
            , hlSession r == sid || (not (M.null alive) && hlPid r `M.notMember` alive) ]
  writeRecord st HookLive { hlPid = pid, hlSession = sid
                          , hlDir = dropTrailingPathSeparator cwd
                          , hlStatus = status, hlWaitingFor = Nothing
                          , hlTranscript = transcript }
  atomicModifyIORef' (hsLastOfPid st) (\m -> (M.insert pid sid m, ()))
  rememberSession st sid cwd (fromMaybe "" transcript)
  return (case prev of (p : _) -> Just p; [] -> Nothing)
  where
    livePids = (`catch` \(_ :: SomeException) -> return M.empty) $ do
      out <- readProcess "ps" ["-Ao", "pid="] ""
      return $ M.fromList [ (p, ()) | l <- lines out, Just p <- [readMaybe l :: Maybe Int] ]

-- | Has leksah recorded a session for process @pid@ before (in this run)?
-- Discovery leaves such a process alone: between a @/clear@'s SessionEnd and
-- the next prompt's SessionStart it has no record, and claiming it then would
-- break the chain from its old session to its new.
hookKnownPid :: HookStore -> Int -> IO Bool
hookKnownPid st pid = M.member pid <$> readIORef (hsLastOfPid st)

-- | A launch's process has appeared before it has a session id of its own:
-- record it under its launch token — unless its SessionStart got there first,
-- which is decided under the same lock the hooks write under, so neither can
-- overwrite the other.
hookClaimLaunch :: HookStore -> Int -> Text -> FilePath -> IO ()
hookClaimLaunch st pid tok cwd = withMVar (hsRecordLock st) $ \_ -> do
  rs <- hookLiveRecords st
  unless (any ((== pid) . hlPid) rs) $ do
    writeRecord st HookLive { hlPid = pid, hlSession = tok
                            , hlDir = dropTrailingPathSeparator cwd
                            , hlStatus = "idle", hlWaitingFor = Nothing
                            , hlTranscript = Nothing }
    atomicModifyIORef' (hsLastOfPid st) (\m -> (M.insert pid tok m, ()))

-- | A leksah launch of a NEW agent under token @tok@: remember it as one of
-- this CLI's agents that has not started a conversation yet ('hookStarted'),
-- so resuming it later starts the CLI afresh rather than Claude Code — or a
-- resume of an id the CLI never had.
hookRememberLaunch :: HookStore -> Text -> FilePath -> IO ()
hookRememberLaunch st tok cwd = rememberSessionWith st (M.member tok) tok cwd ""

-- | Has session @sid@ got a conversation of its own to resume?  'False' for a
-- launch token that never reached its first prompt.
hookStarted :: HookStore -> Text -> IO Bool
hookStarted st sid = isJust <$> hookTranscriptOf st sid

-- | Session @sid@ is now @status@ (idle | busy | waiting), waiting for @what@.
-- 'True' when that changed anything.  Nothing to change when no record is
-- open for it (a session leksah never saw start).
hookSetStatus :: HookStore -> Text -> Text -> Maybe Text -> IO Bool
hookSetStatus st sid status what = withMVar (hsRecordLock st) $ \_ ->
  hookRecordFor st sid >>= \case
    Just r | hlStatus r /= status || hlWaitingFor r /= what -> do
      writeRecord st r { hlStatus = status, hlWaitingFor = what }
      return True
    _ -> return False

-- | Session @sid@ ended: its record goes.
hookSessionEnd :: HookStore -> Text -> IO ()
hookSessionEnd st sid = withMVar (hsRecordLock st) $ \_ -> do
  rs <- hookLiveRecords st
  mapM_ (removeRecordFile st . hlPid) [ r | r <- rs, hlSession r == sid ]

--------------------------------------------------------------------------------
-- Sessions the hooks have seen

indexPath :: HookStore -> IO FilePath
indexPath st = (</> (T.unpack (hsKind st) <> "-index.json")) <$> leksahDir

readIndex :: HookStore -> IO (Map Text (FilePath, FilePath))
readIndex st = cached (hsIndex st) (readIndexFromDisk st)

readIndexFromDisk :: HookStore -> IO (Map Text (FilePath, FilePath))
readIndexFromDisk st = (`catch` \(_ :: SomeException) -> return M.empty) $ do
  p <- indexPath st
  v <- decodeStrict' <$> BS.readFile p
  return . M.fromList $ case v of
    Just (Object o) ->
      [ (Key.toText k, (T.unpack tr, T.unpack cw))
      | (k, e) <- KM.toList o
      , Just (tr, cw) <- [parseMaybe (withObject "e" $ \x ->
                              (,) <$> (fromMaybe "" <$> x .:? "transcript")
                                  <*> (fromMaybe "" <$> x .:? "cwd")) e] ]
    _ -> []

rememberSession :: HookStore -> Text -> FilePath -> FilePath -> IO ()
rememberSession st sid cwd transcript =
  rememberSessionWith st (\m -> M.lookup sid m == Just (transcript, cwd)) sid cwd transcript

-- | Add (or update) a session in the index unless @skip@ says it is already
-- there as it should be.  One writer at a time: the hooks and discovery both
-- add to it, and an update read by one and written over by the other would
-- lose the first's entry.  (A transcript already known is not forgotten for
-- an empty one.)
rememberSessionWith :: HookStore -> (Map Text (FilePath, FilePath) -> Bool) -> Text -> FilePath -> FilePath -> IO ()
rememberSessionWith st skip sid cwd transcript = withMVar (hsIndexLock st) $ \_ ->
  (`catch` \(_ :: SomeException) -> return ()) $ do
    m <- readIndex st
    unless (skip m) $ do
      let tr' = if null transcript then maybe "" fst (M.lookup sid m) else transcript
      p <- indexPath st
      atomicWrite p . encode . object $
        [ Key.fromText k .= object [ "transcript" .= tr, "cwd" .= cw ]
        | (k, (tr, cw)) <- M.toList (M.insert sid (tr', cwd) m) ]
      invalidate (hsIndex st)

-- | Is @sid@ one of this CLI's sessions (running now, or seen by leksah's
-- hooks before)?
hookIsSession :: HookStore -> Text -> IO Bool
hookIsSession st sid = do
  live <- isJust <$> hookRecordFor st sid
  if live then return True else M.member sid <$> readIndex st

-- | Where a session ran, if leksah knows.
hookSessionDir :: HookStore -> Text -> IO (Maybe FilePath)
hookSessionDir st sid = hookRecordFor st sid >>= \case
  Just r | not (null (hlDir r)) -> return (Just (hlDir r))
  _ -> (>>= \(_, cw) -> if null cw then Nothing else Just cw) . M.lookup sid <$> readIndex st

-- | A session's transcript, if leksah knows it.
hookTranscriptOf :: HookStore -> Text -> IO (Maybe FilePath)
hookTranscriptOf st sid = hookRecordFor st sid >>= \case
  Just r | Just t <- hlTranscript r -> return (Just t)
  -- (A launch that never started has an entry with no transcript.)
  _ -> (>>= \(tr, _) -> if null tr then Nothing else Just tr) . M.lookup sid <$> readIndex st

--------------------------------------------------------------------------------

-- | Write a file by renaming a sibling into place — readers never see half of
-- it — and not at all when it already says the same.
atomicWrite :: FilePath -> BL.ByteString -> IO ()
atomicWrite path bytes = do
  ex <- doesFileExist path
  same <- if not ex then return False else (== BL.toStrict bytes) <$> BS.readFile path
  unless same $ do
    let tmp = path <> ".tmp-leksah"
    BL.writeFile tmp bytes
    renameFile tmp path
