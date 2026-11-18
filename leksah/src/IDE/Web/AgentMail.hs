{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- SPDX-License-Identifier: Apache-2.0
-- | Messages between agents that the sending machine's leksah cannot deliver
-- — an agent on another host reporting to the agent that forked it, say.
--
-- Such a message is written to the SENDING machine's event log
-- (@~\/.leksah\/agent-events.jsonl@) as a @send@ command ('logCommand'), and
-- every leksah that reads that log — the machine's own, and each one that
-- shares the machine ("IDE.Web.RemoteAgents") — offers to deliver it.  The
-- one that can reach the target claims it in the sending machine's
-- @agent-deliveries.json@ ('claimDelivery'), so it is delivered once,
-- whoever reads it, however often (a restart re-reads the log's last 64KB).
-- One that cannot reach the target yet keeps it pending, and tries again on
-- every poll for an hour ('addPending' \/ 'retryPending') — the parent may
-- be resumed, or a leksah that shows it started.
--
-- The delivering itself is "IDE.Web.Agent"'s (it has the panes).
module IDE.Web.AgentMail
  ( logCommand
  , commandKey
  , claimDelivery
  , addPending
  , retryPending
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Control.Exception (SomeException, catch)
import Control.Monad (filterM, when)
import Data.Aeson (decode', encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, getFileSize, getHomeDirectory, renameFile)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import IDE.Web.SharedStore (SharedStore, modifyStore, newSharedStore, remoteModifyJSON)

-- | Append a command to this machine's event log, for whichever leksah can
-- act on it — the same line @leksah-cmd@ writes when no leksah answers
-- (@appendEventLog@ in @main\/Cmd.hs@; the two must agree).  The caller's
-- session, the verb, its arguments.  Never fails.
logCommand :: Text -> Text -> [Text] -> IO ()
logCommand sid verb args = (`catch` \(_ :: SomeException) -> return ()) $ do
  home <- getHomeDirectory
  let dir = home </> ".leksah"
      f   = dir </> "agent-events.jsonl"
  createDirectoryIfMissing True dir
  big <- (`catch` \(_ :: SomeException) -> return False) $ (> 4000000) <$> getFileSize f
  when big $ renameFile f (f <> ".1") `catch` \(_ :: SomeException) -> return ()
  t <- realToFrac <$> getPOSIXTime :: IO Double
  LBS.appendFile f . (<> "\n") . encode $ object
    [ "t" .= t, "kind" .= ("command" :: Text), "event" .= verb
    , "payload" .= object [ "session_id" .= sid, "args" .= args ] ]

-- | What a logged command is known by — the same for every leksah that reads
-- the line: who sent it, when (the sending machine's clock), and the verb.
commandKey :: Text -> Double -> Text -> Text
commandKey sid t verb = sid <> "@" <> T.pack (show t) <> "@" <> verb

-- | Claim the delivery of logged command @key@ on the machine whose log it is
-- in (@Nothing@: this one): @Just True@ when THIS call claimed it — nobody
-- had — and so must deliver it, @Just False@ when someone already had, and
-- 'Nothing' when the store could not be reached (try again later).  Claims
-- older than two days go (far older than any log a leksah re-reads).
claimDelivery :: Maybe Text -> Text -> IO (Maybe Bool)
claimDelivery mhost key = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  now <- realToFrac <$> getPOSIXTime :: IO Double
  let claim m
        | key `M.member` m = (m, False)
        | otherwise = (M.insert key now (M.filter (> now - 2 * 86400) m), True)
  case mhost of
    Nothing   -> Just <$> modifyStore deliveriesStore (return . claim)
    Just host -> either (const Nothing) Just
                   <$> remoteModifyJSON host "agent-deliveries.json" M.empty (return . claim)

{-# NOINLINE deliveriesStore #-}
deliveriesStore :: SharedStore (Map Text Double)
deliveriesStore = unsafePerformIO $ newSharedStore "agent-deliveries.json" decode' encode M.empty

-- | Messages this leksah read but could not deliver yet: key → (give up at,
-- try again).  A try answers whether the message is settled (delivered, or
-- claimed by another leksah); unsettled ones stay until they expire.
{-# NOINLINE pendingRef #-}
pendingRef :: MVar (Map Text (Double, IO Bool))
pendingRef = unsafePerformIO (newMVar M.empty)

-- | Keep trying @try@ (see 'pendingRef') for an hour.
addPending :: Text -> IO Bool -> IO ()
addPending key try = do
  now <- realToFrac <$> getPOSIXTime :: IO Double
  modifyMVar_ pendingRef (return . M.insertWith (\_ old -> old) key (now + 3600, try))

-- | Try every pending message again (each poll): settled and expired ones go.
retryPending :: IO ()
retryPending = modifyMVar_ pendingRef $ \m -> do
  now <- realToFrac <$> getPOSIXTime :: IO Double
  kept <- filterM (\(_, (expiry, try)) ->
                      if expiry < now then return False
                      else not <$> (try `catch` \(_ :: SomeException) -> return False))
                  (M.toList m)
  return (M.fromList kept)
