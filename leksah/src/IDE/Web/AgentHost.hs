{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | WHERE an agent's process ran — so that, once it is gone, leksah can tell an
-- agent that ENDED (@\/exit@, a closed pane, a crash of its own) from one that
-- was taken down with its host (the machine restarted, or leksah's tmux server
-- died).  Only the second kind is offered back to the user as "interrupted".
--
-- The host is two facts, both cheap and both independent of the order in
-- which things shut down:
--
--   * the machine's BOOT TIME — a session last seen running under an earlier
--     boot cannot have exited on its own terms afterwards;
--   * the identity of leksah's private TMUX SERVER (its pid and start time) —
--     agents launched by leksah run in its panes, and when that server goes,
--     they all go with it, while leksah itself may well stay up.
--
-- FS\/process only, and low in the import graph (no reflex, nothing from
-- "IDE.Web.Claude" or above).
module IDE.Web.AgentHost
  ( Host(..)
  , RanOn(..)
  , currentHost
  , ranOnFor
  , diedWithHost
  , processParents
  ) where

import Control.Exception (catch, SomeException)
import Data.Aeson
       (FromJSON(..), ToJSON(..), object, (.=), (.:?), withObject)
import Data.Char (isDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, findExecutable)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess, readProcessWithExitCode)
import Text.Read (readMaybe)

import IDE.Web.Instance (tmuxServerSocket)

-- | The host as it is now.
data Host = Host
  { hBoot     :: Maybe Double  -- ^ when the machine booted (POSIX seconds)
  , hTmux     :: Maybe Text    -- ^ leksah's tmux server: @"\<pid\> \<start time\>"@
  , hTmuxPid  :: Maybe Int     -- ^ its pid
  , hParents  :: Map Int Int   -- ^ @pid -> ppid@, the whole process table
  }

-- | Where one agent was running when leksah last saw it.
data RanOn = RanOn
  { roBoot :: Maybe Double  -- ^ the boot it ran under
  , roTmux :: Maybe Text    -- ^ leksah's tmux server it ran in, if it ran in
                            --   one of leksah's panes ('Nothing' for a session
                            --   started in some other terminal)
  } deriving (Eq, Ord, Show)

instance ToJSON RanOn where
  toJSON r = object [ "boot" .= roBoot r, "tmux" .= roTmux r ]

instance FromJSON RanOn where
  parseJSON = withObject "RanOn" $ \o -> RanOn <$> o .:? "boot" <*> o .:? "tmux"

-- | Look at the host: boot time (read once per process), tmux server, process
-- table.
currentHost :: IO Host
currentHost = do
  boot    <- bootTime
  tmux    <- tmuxServer
  parents <- processParents
  return Host { hBoot = boot, hTmux = tmux
              , hTmuxPid = tmux >>= serverPid, hParents = parents }

-- | Where the process @pid@ is running: this boot, and leksah's tmux server
-- when that server is one of its ancestors.
ranOnFor :: Host -> Int -> RanOn
ranOnFor h pid = RanOn
  { roBoot = hBoot h
  , roTmux = case hTmuxPid h of
      Just s | s `elem` ancestors (hParents h) pid -> hTmux h
      _ -> Nothing }

-- | Was an agent that ran on @r@, and is no longer running, taken down with its
-- host?  Yes when the machine has rebooted since, or when it ran in a tmux
-- server that is not there any more.
--
-- The tmux server counts as still there while its pid is in the process table,
-- even if asking tmux for its identity just failed — a transient failure of
-- one @tmux@ call must not turn every agent that the user closed into an
-- "interrupted" one.
diedWithHost :: Host -> RanOn -> Bool
diedWithHost h r = rebooted || tmuxGone
  where
    rebooted = case (roBoot r, hBoot h) of
      -- (kern.boottime drifts by a second or so when the clock is adjusted)
      (Just a, Just b) -> abs (a - b) > 120
      _                -> False
    tmuxGone = case roTmux r of
      Nothing -> False
      Just t  -> case hTmux h of
        Just now -> now /= t
        Nothing  -> maybe True (`M.notMember` hParents h) (serverPid t)

serverPid :: Text -> Maybe Int
serverPid = readMaybe . T.unpack . T.takeWhile isDigit

ancestors :: Map Int Int -> Int -> [Int]
ancestors parents = go (16 :: Int)
  where go 0 _ = []
        go n p = case M.lookup p parents of
          Just q | q > 0 -> q : go (n - 1) q
          _              -> []

-- | @pid -> ppid@ for every process on the machine ('M.empty' on failure).
processParents :: IO (Map Int Int)
processParents = (`catch` \(_ :: SomeException) -> return M.empty) $ do
  out <- readProcess "ps" ["-Ao", "pid=,ppid="] ""
  return $ M.fromList
    [ (p, q) | l <- lines out, (pT : qT : _) <- [words l]
             , Just p <- [readMaybe pT], Just q <- [readMaybe qT] ]

-- | Leksah's tmux server, as @"\<pid\> \<start time\>"@ (the pair, because a
-- pid alone can be reused), or 'Nothing' when it is not running.
tmuxServer :: IO (Maybe Text)
tmuxServer = (`catch` \(_ :: SomeException) -> return Nothing) $
  findExecutable "tmux" >>= ask
  where
    ask Nothing = return Nothing
    ask (Just tmux) = do
      (_, out, _) <- readProcessWithExitCode tmux
        ["-L", tmuxServerSocket, "list-sessions", "-F", "#{pid} #{start_time}"] ""
      return . listToMaybe $
        [ l | l <- map T.strip (T.lines (T.pack out))
            , isJust (serverPid l), T.any (== ' ') l ]

-- | When the machine booted, read once (it cannot change under a running
-- process): @sysctl kern.boottime@ on macOS and the BSDs, @btime@ in
-- @\/proc\/stat@ on Linux.
bootTime :: IO (Maybe Double)
bootTime = readIORef bootRef >>= maybe readIt return
 where
  readIt = do
    b <- (`catch` \(_ :: SomeException) -> return Nothing) $ do
      linux <- doesFileExist "/proc/stat"
      if linux
        then do
          ls <- lines <$> readFile "/proc/stat"
          return $ listToMaybe [ v | ("btime" : n : _) <- map words ls
                                   , Just v <- [readMaybe n] ]
        else do
          -- "{ sec = 1794426538, usec = 391722 } Thu Nov 12 09:48:58 2026"
          out <- readProcess "sysctl" ["-n", "kern.boottime"] ""
          let afterSec = snd (T.breakOn "sec = " (T.pack out))
          return . readMaybe . T.unpack . T.takeWhile isDigit $ T.drop 6 afterSec
    writeIORef bootRef (Just b)
    return b

{-# NOINLINE bootRef #-}
bootRef :: IORef (Maybe (Maybe Double))
bootRef = unsafePerformIO (newIORef Nothing)
