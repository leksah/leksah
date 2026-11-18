{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- SPDX-License-Identifier: Apache-2.0

-- | A sidecar file more than one leksah writes.
--
-- The agent stores (@agent-names.json@, @agents.json@, @worktrees.json@)
-- describe the agents running on THIS machine, and every leksah that shows
-- them reads and writes the same file: the one running here, and another
-- reaching this machine over ssh ('remoteRead' \/ 'remoteModify').  So a store
-- can never trust a value it read earlier:
--
-- * every change takes @flock(2)@ on @FILE.lock@ — the same lock the
--   @flock(1)@ command takes, which is what a remote leksah's write runs under
--   — re-reads the file if it changed, applies the change and writes it back
--   atomically (a temporary file renamed over it), so readers never see half
--   a file;
-- * a read without a change uses the cached value only while the file's
--   modification time and size are the ones it was read at (one @stat@).
module IDE.Web.SharedStore
  ( SharedStore
  , newSharedStore
  , peekStore
  , modifyStore
  , withFileLock
  , atomicWriteFile
    -- * A store on another machine
  , remoteRead
  , remoteModify
  , remoteModifyJSON
  , StoreUnreadable(..)
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (Exception, SomeException, bracket, catch, evaluate, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (FromJSON, ToJSON, decode', encode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import System.Directory
       (createDirectoryIfMissing, doesFileExist, getFileSize, getModificationTime,
        renameFile, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, takeFileName, (<.>))
import System.IO (IOMode(..), hClose, openBinaryTempFile, openFile)
#if !defined(ghcjs_HOST_OS) && !defined(mingw32_HOST_OS)
import Foreign.C.Error (throwErrnoIfMinus1Retry_)
import Foreign.C.Types (CInt(..))
import GHC.IO.FD (FD(..))
import GHC.IO.Handle.FD (handleToFd)
#endif

import IDE.Paths (sidecarPath)
import IDE.Utils.RemoteExec (runSsh)

-- | One sidecar file, its decoder and encoder, and the value last read or
-- written with the file's stamp at that moment.
data SharedStore a = SharedStore
  { ssName   :: FilePath                 -- ^ under @~\/.leksah-0.17@
  , ssDecode :: LBS.ByteString -> Maybe a
  , ssEncode :: a -> LBS.ByteString
  , ssEmpty  :: a
  , ssVar    :: MVar (Maybe (Maybe Stamp, a))
  }

-- | A file's modification time and size; 'Nothing' (in the cache) when there
-- was no file.
type Stamp = (UTCTime, Integer)

newSharedStore :: FilePath -> (LBS.ByteString -> Maybe a) -> (a -> LBS.ByteString) -> a
               -> IO (SharedStore a)
newSharedStore name dec enc empty = SharedStore name dec enc empty <$> newMVar Nothing

stampOf :: FilePath -> IO (Maybe Stamp)
stampOf p = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  ex <- doesFileExist p
  if not ex then return Nothing else do
    t <- getModificationTime p
    n <- getFileSize p
    return (Just (t, n))

-- | Read the file (an unreadable or undecodable one reads as empty), with the
-- stamp taken BEFORE reading: a write racing the read leaves a newer stamp
-- than the one recorded, so the next look reads it again.
--
-- A file that is there but does not decode (another build's format, or one
-- caught mid-write by a writer that is not atomic) is 'Nothing': NOT empty.
-- Read as empty, the next change would write back a store holding only that
-- change — every other agent's record or name on the machine gone.
readFresh :: SharedStore a -> FilePath -> IO (Maybe Stamp, Maybe a)
readFresh st p = do
  stamp <- stampOf p
  v <- case stamp of
    Nothing -> return (Just (ssEmpty st))
    Just _  -> (`catch` \(_ :: SomeException) -> return Nothing) $ do
      bytes <- LBS.fromStrict <$> BS.readFile p
      if LBS.null bytes then return (Just (ssEmpty st))
        else maybe (return Nothing) (fmap Just . evaluate) (ssDecode st bytes)
  return (stamp, v)

-- | A store file this leksah cannot read: it is left as it is, not replaced.
newtype StoreUnreadable = StoreUnreadable FilePath deriving Show
instance Exception StoreUnreadable

-- | The current value: the cached one while the file is unchanged.
peekStore :: SharedStore a -> IO a
peekStore st = do
  p <- sidecarPath (ssName st)
  now <- stampOf p
  modifyMVar (ssVar st) $ \cached -> case cached of
    Just (s, v) | s == now -> return (cached, v)
    _ -> readFresh st p >>= \case
      (s, Just v) -> return (Just (s, v), v)
      -- (unreadable: what was last read, else nothing — never cached)
      (_, Nothing) -> return (cached, maybe (ssEmpty st) snd cached)

-- | Change the store: under the file's lock, from what is in the file NOW,
-- writing it back only when the change changed it.  The function may do IO
-- (pruning entries whose worktree has gone, say) but must not touch the store.
modifyStore :: Eq a => SharedStore a -> (a -> IO (a, b)) -> IO b
modifyStore st f = do
  p <- sidecarPath (ssName st)
  modifyMVar (ssVar st) $ \cached -> withFileLock (p <.> "lock") $ do
    now <- stampOf p
    (s0, v) <- case cached of
      Just c@(s, _) | s == now, s /= Nothing -> return c
      _ -> readFresh st p >>= \case
        (s, Just v) -> return (s, v)
        (_, Nothing) -> throwIO (StoreUnreadable p)
    (v', b) <- f v
    if v' == v then return (Just (s0, v), b) else do
      atomicWriteFile p (ssEncode st v')
      s' <- stampOf p
      return (Just (s', v'), b)

-- | Write a file so that a reader sees the old contents or the new, never
-- part of either: a temporary file in the same directory, renamed over it.
atomicWriteFile :: FilePath -> LBS.ByteString -> IO ()
atomicWriteFile p bytes = do
  let dir = takeDirectory p
  createDirectoryIfMissing True dir
  bracket (openBinaryTempFile dir ("." <> takeFileName p <> ".tmp"))
          (\(tmp, h) -> hClose h >> (removeFile tmp `catch` \(_ :: SomeException) -> return ()))
          (\(tmp, h) -> do
              LBS.hPut h bytes
              hClose h
              renameFile tmp p)

-- | Run @act@ holding @flock(2)@ on @lockFile@ (created if need be): the lock
-- a shell's @flock(1)@ takes too, released when the file is closed — by this
-- process ending, as well.  (No lock on Windows or in the browser demo, where
-- one leksah is all there is.)
withFileLock :: FilePath -> IO a -> IO a
#if defined(ghcjs_HOST_OS) || defined(mingw32_HOST_OS)
withFileLock _ act = act
#else
withFileLock lockFile act = do
  createDirectoryIfMissing True (takeDirectory lockFile)
  bracket (openFile lockFile AppendMode) hClose $ \h -> do
    fd <- handleToFd h
    throwErrnoIfMinus1Retry_ "flock" (c_flock (fdFD fd) lockEx)
    act
  where lockEx = 2

foreign import ccall interruptible "flock" c_flock :: CInt -> CInt -> IO CInt
#endif

--------------------------------------------------------------------------------
-- A store on another machine

-- | Sidecar @name@ on @host@ (its @~\/.leksah-0.17\/name@): its contents and a
-- checksum of them to hand back to 'remoteModify' — @("", "")@ when there is
-- no such file — or why it could not be read.  One ssh round trip.
remoteRead :: Text -> FilePath -> IO (Either Text (Text, LBS.ByteString))
remoteRead host name = (`catch` \(e :: SomeException) -> return (Left (T.pack (show e)))) $ do
  (code, out, err) <- runSsh host readScript [T.pack name] mempty
  case code of
    ExitSuccess -> do
      -- First line: the checksum; the rest: the file.
      let (sumLine, rest) = BC.break (== '\n') out
      return (Right (T.pack (BC.unpack sumLine), LBS.fromStrict (BS.drop 1 rest)))
    _ -> return (Left (T.strip (T.pack (BC.unpack err))))
  where
    readScript = T.concat
      [ "f=\"$HOME/.leksah-0.17/$0\"; "
      , "if [ -f \"$f\" ]; then { cksum < \"$f\" | cut -d' ' -f1,2; cat \"$f\"; }; "
      , "else echo; fi" ]

-- | 'remoteModify' for a JSON store: @f@ changes the decoded value (an absent
-- or empty file is @empty@).  A file that does not decode is left as it is
-- and answered with 'Left' — never replaced by a store holding only this
-- change (see 'readFresh').
remoteModifyJSON :: (FromJSON a, ToJSON a, Eq a) => Text -> FilePath -> a
                 -> (a -> IO (a, b)) -> IO (Either Text b)
remoteModifyJSON host name empty f = do
  r <- remoteModify host name $ \old ->
    if LBS.null old then go empty old else
      case decode' old of
        Nothing -> return (old, Left ("cannot read " <> T.pack name <> " on " <> host
                                      <> "; left as it is"))
        Just v  -> go v old
  return (r >>= id)
  where
    go v old = do
      (v', b) <- f v
      return (if v' == v then old else encode v', Right b)

-- | Change sidecar @name@ on @host@: read it, apply @f@ to its contents, and
-- write the result back under the file's lock — only if the file is still
-- what was read (its checksum); otherwise read again and re-apply, up to
-- three times.  Nothing is written when @f@ leaves the contents as they were.
remoteModify :: Text -> FilePath -> (LBS.ByteString -> IO (LBS.ByteString, b))
             -> IO (Either Text b)
remoteModify host name f = go (3 :: Int)
  where
    go n = remoteRead host name >>= \case
      Left err -> return (Left err)
      Right (sumWas, old) -> do
        (new, b) <- f old
        if new == old then return (Right b) else do
          (code, _, err) <- runSsh host writeScript [T.pack name, sumWas] (LBS.toStrict new)
          case code of
            ExitSuccess -> return (Right b)
            ExitFailure 3 | n > 1 -> go (n - 1)
            _ -> return (Left (T.strip (T.pack (BC.unpack err))))
    -- $0 the name, $1 the checksum it was read at ("" = there was no file);
    -- stdin the new contents.  Exit 3: it changed meanwhile.
    writeScript = T.concat
      [ "d=\"$HOME/.leksah-0.17\"; f=\"$d/$0\"; mkdir -p \"$d\" || exit 1; "
      , "t=\"$d/.$0.tmp.$$\"; cat > \"$t\" || exit 1; "
      , "w() { if [ -f \"$f\" ]; then now=$(cksum < \"$f\" | cut -d' ' -f1,2); else now=; fi; "
      ,       "if [ \"$now\" != \"$1\" ]; then rm -f \"$t\"; exit 3; fi; "
      ,       "mv \"$t\" \"$f\"; }; "
      , "if command -v flock >/dev/null 2>&1; then "
      ,   "exec 9>>\"$f.lock\"; flock 9 || exit 1; "
      -- (no flock(1) — stock macOS: Perl's flock is the same flock(2) lock,
      -- held while it checks and renames)
      , "elif command -v perl >/dev/null 2>&1; then "
      ,   "exec perl -MFcntl=:flock -e '"
      ,     "my ($lock, $f, $t, $want) = @ARGV; "
      ,     "open(my $l, \">>\", $lock) or exit 1; flock($l, LOCK_EX) or exit 1; "
      ,     "my $now = -f $f ? `cksum < \"$f\" | cut -d\" \" -f1,2` : \"\"; chomp $now; "
      ,     "if ($now ne $want) { unlink $t; exit 3 } "
      ,     "rename($t, $f) or exit 1; exit 0' \"$f.lock\" \"$f\" \"$t\" \"$1\"; "
      , "fi; "
      , "w \"$1\"" ]
