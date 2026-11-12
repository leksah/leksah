{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Wake-on-LAN for ssh hosts that sleep: a host listed in @remote.wake@
-- (see 'IDE.Config.WakeOnLan') is sent the "magic packet" — six @0xFF@
-- bytes, then its MAC address sixteen times, as a UDP broadcast — whenever
-- leksah needs it and it doesn't answer, and leksah waits for it to come up
-- before going on (a remote tab connecting, a session being opened, the Add
-- Server probe, the Terminals tree's "Wake" item).
module IDE.Web.WakeOnLan
  ( parseMac
  , sendMagicPacket
  , wakeFor
  , hostAnswers
  , ensureAwake
  , wakeAndWait
  , detectMac
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, catch, try)
import Control.Monad (forM_, replicateM_)
import qualified Data.ByteString as BS
import Data.Char (isHexDigit, digitToInt, toLower)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Word (Word8)
import Network.Socket
       (AddrInfo(..), SocketOption(Broadcast), SocketType(Datagram),
        defaultHints, getAddrInfo, setSocketOption, socket, close)
import Network.Socket.ByteString (sendTo)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import IDE.App (App(..), getGlobalApp)
import IDE.Config (Config(..), RemoteC(..), WakeOnLan(..), currentConfig)

-- | The six bytes of a MAC address written @aa:bb:cc:dd:ee:ff@ (or with @-@,
-- or none, as separators).
parseMac :: Text -> Maybe [Word8]
parseMac t =
  let hex = filter (`notElem` (":-. " :: String)) (T.unpack (T.strip t))
      pairs (a : b : rest) = (16 * digitToInt a + digitToInt b) : pairs rest
      pairs _              = []
  in if length hex == 12 && all isHexDigit hex
       then Just (map fromIntegral (pairs (map toLower hex)))
       else Nothing

-- | Broadcast the magic packet for @mac@ to @bcast@ (default
-- 255.255.255.255), UDP ports 9 and 7 (cards listen on either).  'Left'
-- says why it could not be sent.
sendMagicPacket :: Text -> Maybe Text -> IO (Either Text ())
sendMagicPacket macT mbcast = case parseMac macT of
  Nothing -> return (Left ("Not a MAC address: " <> macT))
  Just mac -> do
    let payload = BS.pack (replicate 6 0xff ++ concat (replicate 16 mac))
        bcast   = maybe "255.255.255.255" T.unpack mbcast
    r <- try $ forM_ ["9", "7"] $ \port -> do
      addrs <- getAddrInfo (Just defaultHints { addrSocketType = Datagram })
                           (Just bcast) (Just port)
      forM_ (take 1 addrs) $ \ai ->
        bracket (socket (addrFamily ai) Datagram (addrProtocol ai)) close $ \s -> do
          setSocketOption s Broadcast 1
          -- (a few copies: UDP is lossy and a sleeping card gets one chance)
          replicateM_ 3 $ sendTo s payload (addrAddress ai)
    return $ case r of
      Left (e :: SomeException) -> Left (T.pack (show e))
      Right ()                  -> Right ()

-- | The Wake-on-LAN entry for @host@, if it has one.
wakeFor :: Text -> IO (Maybe WakeOnLan)
wakeFor host = getGlobalApp >>= \case
  Nothing  -> return Nothing
  Just app -> find ((== host) . wolHost) . rcWake . cfgRemote
                <$> currentConfig (appConfig app)

-- | Does @host@ answer ssh now?  A quick no-op exec with a short connect
-- timeout (ssh exits 255 for its own failures: unreachable, refused, …).
hostAnswers :: Text -> IO Bool
hostAnswers host = (`catch` \(_ :: SomeException) -> return False) $ do
  (c, _, _) <- readProcessWithExitCode "ssh"
    [ "-o", "BatchMode=yes", "-o", "ConnectTimeout=3", T.unpack host, "true" ] ""
  return (c /= ExitFailure 255)

-- | Before connecting to @host@: if it has a Wake-on-LAN entry and doesn't
-- answer, wake it and wait for it ('wakeAndWait').  Hosts without an entry
-- cost nothing (no probe).  'True' when it answers (or has no entry).
ensureAwake :: Text -> IO Bool
ensureAwake host = wakeFor host >>= \case
  Nothing -> return True
  Just w  -> do
    up <- hostAnswers host
    if up then return True else either (const False) id <$> wakeAndWait w

-- | Send the magic packet and poll until the host answers ssh, for up to two
-- minutes (resending now and then: the first may be lost, or the card may
-- need the machine fully asleep first).  'Left' when the packet could not be
-- sent; @Right False@ when the host never came up.
wakeAndWait :: WakeOnLan -> IO (Either Text Bool)
wakeAndWait w = sendMagicPacket (wolMac w) (wolBroadcast w) >>= \case
  Left err -> return (Left err)
  Right () -> do
    t0 <- getCurrentTime
    let loop n = do
          up <- hostAnswers (wolHost w)
          if up then return (Right True) else do
            now <- getCurrentTime
            if diffUTCTime now t0 > 120 then return (Right False) else do
              threadDelay 2000000
              -- resend every ~20s
              _ <- if n `mod` (10 :: Int) == 9
                     then sendMagicPacket (wolMac w) (wolBroadcast w)
                     else return (Right ())
              loop (n + 1)
    loop 0

-- | Ask a host that is up for the MAC address of its default-route network
-- interface (Linux: @/sys/class/net@; macOS: @ifconfig@).  'Nothing' when it
-- can't be reached or doesn't say.
detectMac :: Text -> IO (Maybe Text)
detectMac host = (`catch` \(_ :: SomeException) -> return Nothing) $ do
  (c, out, _) <- readProcessWithExitCode "ssh"
    [ "-o", "BatchMode=yes", "-o", "ConnectTimeout=5", T.unpack host
    , "sh -c " <> shq script ] ""
  let mac = T.strip (T.pack out)
  return $ if c == ExitSuccess && maybe False (const True) (parseMac mac)
             then Just mac else Nothing
  where
    shq s = "'" <> concatMap (\ch -> if ch == '\'' then "'\\''" else [ch]) s <> "'"
    script = unwords
      [ "i=$(ip route show default 2>/dev/null | awk '{print $5; exit}');"
      , "if [ -n \"$i\" ] && [ -r /sys/class/net/$i/address ]; then cat /sys/class/net/$i/address;"
      , "else i=$(route -n get default 2>/dev/null | awk '/interface:/{print $2}');"
      , "[ -n \"$i\" ] && ifconfig $i 2>/dev/null | awk '/ether /{print $2; exit}'; fi" ]
