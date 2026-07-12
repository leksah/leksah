{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The frontend↔backend call seam for the web-UI split.
--
-- The web UI is being split into two Haskell halves: a /frontend/ half
-- (reflex-dom widgets, DOM, CM6\/xterm driving) and a /backend/ half (IDE
-- state, FS, processes, LSP, terminals).  The two halves always share ONE JS
-- context — natively via jsaddle, in the browser directly — and this module
-- is the mechanism they call each other through:
--
--   * 'newDirectBridge' (dev + pure demo): both halves in one RTS.  The two
--     'Bridge' ends share an in-process endpoint table; values cross as-is
--     with __zero serialization__.  Deliberately still async with the same
--     per-endpoint FIFO ordering as the JS mode, so dev never grants
--     synchronicity production lacks.
--
--   * 'newJsBridge' (split production): the frontend half is compiled with
--     the GHC JS backend and runs in the page; the backend half is native
--     and reaches the same page through jsaddle.  Each half attaches to a
--     tiny @window.leksahBridge@ glue object; calls cross as one JSON
--     'Text' per direction with correlation ids.  Everything is ASYNC —
--     the bridge never issues synchronous JS, so it can never enter the
--     jsaddle-wkwebview synchronous main-thread path (the multi-window
--     deadlock class documented in "IDE.Web.WindowBridge").
--
-- Rich values never cross: they stay in the owning half's 'Handle' registry
-- (an Int-keyed 'Dynamic' table — NOT 'StablePtr', which cannot be
-- enumerated for cleanup when a page reloads and is UB on misuse) and only
-- the opaque key travels.  Haskell callbacks cross as anonymous endpoints
-- ('toCallback').  High-frequency data (PTY output, build logs) must use
-- 'openStreamSink' — batched flushes with one-in-flight ack coalescing, the
-- same shape as @registerResync@ in "IDE.Web.WindowBridge".
--
-- Threading rules for seam code:
--
--   * exposed handlers run on bridge worker threads (one per endpoint,
--     FIFO), never on a jsaddle runner thread — they may block and may
--     'call' back across the bridge;
--   * the /blocking/ 'call' must NOT be used inside a jsaddle callback or a
--     reflex frame — use 'call_', 'toCallback', or hop with 'forkIO'.
module IDE.Web.Bridge
  ( -- * Bridge construction
    Bridge
  , Side(..)
  , otherSide
  , bridgeSide
  , newDirectBridge
  , newJsBridge
  , bridgeGlueJs
    -- * Values crossing the seam
  , BridgeValue(..)
  , BridgeError(..)
    -- * Expose / call
  , expose
  , unexpose
  , call
  , call_
  , exposeJSON
  , callJSON
    -- * Opaque handles (rich values stay put)
  , Handle(..)
  , HandleError(..)
  , newHandle
  , derefHandle
  , freeHandle
  , withHandle
    -- * Callbacks and streams
  , CallbackRef(..)
  , toCallback
  , callbackClosure
  , StreamConfig(..)
  , defaultStreamConfig
  , StreamSink(..)
  , newStreamTarget
  , openStreamSink
    -- * Wire encoding (debug/tests)
  , encodeValue1
    -- * Lifecycle
  , HelloInfo(..)
  , bridgeProtocolVersion
  , helloHandshake
  , onPeerReset
  , bridgeReset
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
       (TQueue, TVar, atomically, newTQueueIO, newTVarIO, readTQueue,
        readTVar, retry, writeTQueue, writeTVar)
import Control.Exception (SomeException, bracket, try)
import Control.Lens ((^.))
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
       (FromJSON, ToJSON, Value(..), (.=), (.:), eitherDecodeStrict',
        fromJSON, object, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AT
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Int (Int64)
import Data.IORef
       (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef,
        writeIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

import Language.Javascript.JSaddle
       (JSM, JSContextRef, askJSM, asyncFunction, eval, js4,
        js5, jsg, runJSM, valToNumber, valToText)

-- ---------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------

-- | Which half of the split a bridge end (or a handle) belongs to.
data Side = FrontendSide | BackendSide
  deriving (Eq, Ord, Show)

otherSide :: Side -> Side
otherSide FrontendSide = BackendSide
otherSide BackendSide  = FrontendSide

sideText :: Side -> Text
sideText FrontendSide = "F"
sideText BackendSide  = "B"

sideFromText :: Text -> Maybe Side
sideFromText "F" = Just FrontendSide
sideFromText "B" = Just BackendSide
sideFromText _   = Nothing

-- | An opaque reference to a rich value living in the owning end's
-- registry.  Only ever meaningful when handed back to its owner; carries
-- the peer epoch it was minted under so a reloaded peer's stale keys are
-- rejected instead of resolving to the wrong value.
data Handle = Handle
  { hSide  :: !Side   -- ^ the OWNING side
  , hKey   :: !Int64
  , hEpoch :: !Int
  } deriving (Eq, Ord, Show)

-- | A Haskell callback exposed as an anonymous endpoint (@__cb/<key>@) on
-- its owning side; the peer turns it back into a fire-and-forget closure
-- with 'callbackClosure'.
data CallbackRef = CallbackRef
  { cbSide  :: !Side
  , cbKey   :: !Int64
  , cbEpoch :: !Int
  } deriving (Eq, Ord, Show)

-- | The value vocabulary of the seam.  In direct mode these cross as-is
-- (a 'BJson' is a shared 'Value', a 'BBytes' the same 'ByteString'); on the
-- JS wire they serialize to JSON with tagged escapes (@{"$j":…}@,
-- @{"$b64":…}@, @{"$h":…}@, @{"$cb":…}@).
data BridgeValue
  = BNull
  | BBool !Bool
  | BNum !Double
  | BText !Text
  | BJson !Value
  | BBytes !ByteString
  | BHandle !Handle
  | BCallback !CallbackRef
  deriving (Eq, Show)

data BridgeError
  = BridgeUnknownEndpoint Text
  | BridgeTimeout
  | BridgeDisconnected
  | BridgeRemoteError Text   -- ^ the peer's handler threw
  | BridgeDecodeError Text   -- ^ payload didn't decode (wire or JSON layer)
  deriving (Eq, Show)

data HandleError
  = HandleWrongSide     -- ^ deref'd on the non-owning end
  | HandleFreed         -- ^ key not in the registry (freed or never existed)
  | HandleStaleEpoch    -- ^ minted for a previous peer generation
  | HandleTypeMismatch  -- ^ 'fromDynamic' failed
  deriving (Eq, Show)

-- | One incoming piece of work for an endpoint's FIFO worker.
data Job = Job
  { jobHandler :: [BridgeValue] -> IO BridgeValue
  , jobArgs    :: [BridgeValue]
  , jobReply   :: Either BridgeError BridgeValue -> IO ()
  }

-- | How this end reaches its peer.
data Transport
  = DirectT (IORef (Maybe Bridge))
    -- ^ the peer end itself (dev/demo: same RTS, zero marshaling)
  | JsT JSContextRef (IORef Int64)
    -- ^ jsaddle context + outgoing dispatch sequence counter; calls go
    -- through @window.leksahBridge@ and are seq-stamped because native
    -- jsaddle delivers async callback results on freshly forked threads
    -- (one per Results frame), which can reorder delivery.

-- | One end of the seam.
data Bridge = Bridge
  { brSide      :: !Side
  , brExposed   :: IORef (M.Map Text ([BridgeValue] -> IO BridgeValue))
  , brWorkers   :: IORef (M.Map Text (TQueue Job))
  , brPending   :: IORef (M.Map Int64 (MVar (Either BridgeError BridgeValue)))
  , brNextId    :: IORef Int64  -- ^ call ids, handle keys, callback keys
  , brHandles   :: IORef (M.Map Int64 (Dynamic, Int))  -- ^ value, peer epoch at mint
  , brCbEpochs  :: IORef (M.Map Int64 Int)  -- ^ callback key → peer epoch at mint
  , brPeerEpoch :: IORef Int
  , brOnReset   :: IORef [IO ()]
  , brAlive     :: IORef Bool
  , brTransport :: Transport
  }

bridgeSide :: Bridge -> Side
bridgeSide = brSide

-- | Default per-call timeout (same as the jsaddle-terminal tunnel's).
callTimeoutUs :: Int
callTimeoutUs = 30 * 1000000

bridgeProtocolVersion :: Int
bridgeProtocolVersion = 1

-- ---------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------

mkEnd :: Side -> Transport -> IO Bridge
mkEnd side transport = do
  br <- Bridge side
    <$> newIORef M.empty
    <*> newIORef M.empty
    <*> newIORef M.empty
    <*> newIORef 1
    <*> newIORef M.empty
    <*> newIORef M.empty
    <*> newIORef 0
    <*> newIORef []
    <*> newIORef True
    <*> pure transport
  -- Built-in endpoints: peer-initiated handle free, and the handshake
  -- responder (filled in properly by 'helloHandshake'; answering with just
  -- the protocol version keeps an early peer probe from erroring).
  expose br "__free" $ \case
    [BHandle h] | hSide h == side -> do
      modifyIORef' (brHandles br) (M.delete (hKey h))
      return BNull
    _ -> ioError (userError "__free: expected a handle owned by this side")
  expose br "__hello" $ \_ ->
    return (BJson (toJSON (HelloInfo bridgeProtocolVersion "" [])))
  return br

-- | Both halves in one RTS (dev, and the pure in-browser demo): returns the
-- @(frontend, backend)@ ends of one in-process seam.
newDirectBridge :: IO (Bridge, Bridge)
newDirectBridge = do
  peerOfF <- newIORef Nothing
  peerOfB <- newIORef Nothing
  f <- mkEnd FrontendSide (DirectT peerOfF)
  b <- mkEnd BackendSide (DirectT peerOfB)
  writeIORef peerOfF (Just b)
  writeIORef peerOfB (Just f)
  return (f, b)

-- | One half of a split deployment: attach this end to
-- @window.leksahBridge@ in the current JS context.  The same code runs on
-- the native side (through jsaddle's async batches) and on the ghcjs side
-- (directly in the page).
newJsBridge :: Side -> JSM Bridge
newJsBridge side = do
  _ <- eval bridgeGlueJs   -- idempotent install
  ctx <- askJSM
  seqRef <- liftIO $ newIORef 1
  br <- liftIO $ mkEnd side (JsT ctx seqRef)
  -- Inbound dispatches are seq-stamped by the sender; buffer + reorder so
  -- per-endpoint FIFO survives jsaddle's forkIO-per-Results delivery.
  inBuf <- liftIO $ newTVarIO (1 :: Int64, M.empty :: M.Map Int64 (Int64, Text, Text))
  dispatchFn <- asyncFunction $ \_ _ args -> case args of
    [seqV, callIdV, nameV, argsJsonV] -> do
      sq <- round <$> valToNumber seqV
      cid <- round <$> valToNumber callIdV
      name <- valToText nameV
      argsJson <- valToText argsJsonV
      liftIO . atomically $ do
        (next, m) <- readTVar inBuf
        writeTVar inBuf (next, M.insert sq (cid, name, argsJson) m)
    _ -> return ()
  replyFn <- asyncFunction $ \_ _ args -> case args of
    [callIdV, okV, resultJsonV] -> do
      cid <- round <$> valToNumber callIdV
      okN <- valToNumber okV
      resultJson <- valToText resultJsonV
      liftIO $ completePending br cid $
        if okN /= 0
          then case decodeValue1 resultJson of
                 Right v -> Right v
                 Left e  -> Left (BridgeDecodeError (T.pack e))
          else Left (decodeError resultJson)
    _ -> return ()
  _ <- jsg ("leksahBridge" :: Text) ^. js4 ("attach" :: Text)
         (sideText side) bridgeProtocolVersion dispatchFn replyFn
  -- The in-order pump: deliver buffered dispatches strictly by seq.
  _ <- liftIO . forkIO . forever $ do
    (cid, name, argsJson) <- atomically $ do
      (next, m) <- readTVar inBuf
      case M.lookup next m of
        Just x  -> writeTVar inBuf (next + 1, M.delete next m) >> return x
        Nothing -> retry
    handleIncomingJs br cid name argsJson
  -- Watchdog: a hole in the seq stream (a lost dispatch) would stall the
  -- pump forever; after ~10s of no progress with messages buffered, skip to
  -- the lowest buffered seq and log the loss loudly.
  _ <- liftIO . forkIO $ watchdog inBuf
  return br
 where
  watchdog inBuf = do
    let loop lastNext (stuckFor :: Int) = do
          threadDelay 2000000
          acted <- atomically $ do
            (next, m) <- readTVar inBuf
            if M.null m || M.member next m
              then return (Left next)                 -- progressing / empty
              else if next == lastNext && stuckFor >= 8000000
                then case M.lookupMin m of
                  Just (lo, _) -> writeTVar inBuf (lo, m) >> return (Right (next, lo))
                  Nothing      -> return (Left next)
                else return (Left next)
          case acted of
            Right (next, lo) -> do
              hPutStrLn stderr $ "[Bridge] seq hole: expected " <> show next
                <> ", skipping to " <> show lo <> " (message lost?)"
              loop lo 0
            Left next
              | next == lastNext -> loop next (stuckFor + 2000000)
              | otherwise        -> loop next 0
    loop 1 0

-- ---------------------------------------------------------------------
-- Expose / call
-- ---------------------------------------------------------------------

-- | Register an endpoint on this end.  The handler runs on the endpoint's
-- own FIFO worker thread (never a jsaddle thread); exceptions become
-- 'BridgeRemoteError' on the calling side.
expose :: Bridge -> Text -> ([BridgeValue] -> IO BridgeValue) -> IO ()
expose br name h = modifyIORef' (brExposed br) (M.insert name h)

unexpose :: Bridge -> Text -> IO ()
unexpose br name = modifyIORef' (brExposed br) (M.delete name)

-- | Call an endpoint exposed by the PEER end and wait (≤30s) for its
-- result.  Blocks only the calling thread — never call from a jsaddle
-- callback or reflex frame; hop with 'forkIO' or use 'call_'.
call :: Bridge -> Text -> [BridgeValue] -> IO (Either BridgeError BridgeValue)
call br name args = do
  alive <- readIORef (brAlive br)
  if not alive then return (Left BridgeDisconnected) else do
    cid <- freshId br
    mv <- newEmptyMVar
    modifyIORef' (brPending br) (M.insert cid mv)
    sendCall br cid name args
    r <- timeout callTimeoutUs (takeMVar mv)
    case r of
      Just v  -> return v
      Nothing -> do
        modifyIORef' (brPending br) (M.delete cid)
        return (Left BridgeTimeout)

-- | Fire-and-forget notification: no reply, never blocks on the peer.
call_ :: Bridge -> Text -> [BridgeValue] -> IO ()
call_ br name args = do
  alive <- readIORef (brAlive br)
  when alive $ sendCall br 0 name args

-- | JSON-typed sugar over 'expose': the endpoint takes and returns one
-- aeson-encodable value.
exposeJSON :: (FromJSON a, ToJSON b) => Bridge -> Text -> (a -> IO b) -> IO ()
exposeJSON br name h = expose br name $ \args -> do
  v <- case args of
    [BJson v]  -> return v
    [BText t]  -> return (String t)
    [BNum n]   -> return (Number (realToFrac n))
    [BBool b]  -> return (Bool b)
    [BNull]    -> return Null
    []         -> return Null
    _          -> ioError (userError "exposeJSON: expected one JSON argument")
  case fromJSON v of
    Aeson.Error e   -> ioError (userError ("exposeJSON: bad argument: " <> e))
    Aeson.Success a -> BJson . toJSON <$> h a

-- | JSON-typed sugar over 'call'.
callJSON :: (ToJSON a, FromJSON b) => Bridge -> Text -> a -> IO (Either BridgeError b)
callJSON br name a = do
  r <- call br name [BJson (toJSON a)]
  return $ case r of
    Left e  -> Left e
    Right v ->
      let jv = case v of
            BJson x -> x
            BText t -> String t
            BNum n  -> Number (realToFrac n)
            BBool b -> Bool b
            BNull   -> Null
            other   -> toJSON (encodeValue1' other)  -- last resort: wire form
      in case fromJSON jv of
           Aeson.Success b -> Right b
           Aeson.Error e   -> Left (BridgeDecodeError (T.pack e))

-- Deliver a call into a bridge end's endpoint table (used directly by the
-- direct transport, and by the JS pump after decode).
dispatchIncoming :: Bridge -> Text -> [BridgeValue]
                 -> (Either BridgeError BridgeValue -> IO ()) -> IO ()
dispatchIncoming br name args reply = do
  handlers <- readIORef (brExposed br)
  case M.lookup name handlers of
    Nothing -> reply (Left (BridgeUnknownEndpoint name))
    Just h  -> do
      q <- ensureWorker br name
      atomically $ writeTQueue q (Job h args reply)

-- One FIFO worker per endpoint; a slow handler can't starve other
-- endpoints, and one endpoint's calls can't reorder.
ensureWorker :: Bridge -> Text -> IO (TQueue Job)
ensureWorker br name = do
  ws <- readIORef (brWorkers br)
  case M.lookup name ws of
    Just q  -> return q
    Nothing -> do
      q <- newTQueueIO
      created <- atomicModifyIORef' (brWorkers br) $ \m ->
        case M.lookup name m of
          Just q'  -> (m, Left q')   -- lost the race; reuse the winner's
          Nothing  -> (M.insert name q m, Right q)
      case created of
        Left q' -> return q'
        Right _ -> do
          _ <- forkIO . forever $ do
            Job h args reply <- atomically (readTQueue q)
            r <- try (h args)
            reply $ case r of
              Left (e :: SomeException) -> Left (BridgeRemoteError (T.pack (show e)))
              Right v                   -> Right v
          return q

freshId :: Bridge -> IO Int64
freshId br = atomicModifyIORef' (brNextId br) (\n -> (n + 1, n))

completePending :: Bridge -> Int64 -> Either BridgeError BridgeValue -> IO ()
completePending br cid r = do
  mmv <- atomicModifyIORef' (brPending br) $ \m ->
    (M.delete cid m, M.lookup cid m)
  mapM_ (`putMVar` r) mmv

-- Route a call to the peer, per transport.
sendCall :: Bridge -> Int64 -> Text -> [BridgeValue] -> IO ()
sendCall br cid name args = case brTransport br of
  DirectT peerRef -> readIORef peerRef >>= \case
    Nothing   -> when (cid /= 0) $ completePending br cid (Left BridgeDisconnected)
    Just peer -> dispatchIncoming peer name args $ \r ->
      when (cid /= 0) $ completePending br cid r
  JsT ctx seqRef -> do
    sq <- atomicModifyIORef' seqRef (\n -> (n + 1, n))
    let argsJson = encodeValues args
    ok <- try . flip runJSM ctx . void $
      jsg ("leksahBridge" :: Text) ^. js5 ("call" :: Text)
        (sideText (brSide br))
        (fromIntegral sq :: Double)
        (fromIntegral cid :: Double)
        name argsJson
    case ok of
      Right () -> return ()
      Left (e :: SomeException) -> do
        hPutStrLn stderr $ "[Bridge] send failed (context dead?): " <> show e
        when (cid /= 0) $ completePending br cid (Left BridgeDisconnected)

-- Handle one in-order inbound dispatch on a JS-mode end.
handleIncomingJs :: Bridge -> Int64 -> Text -> Text -> IO ()
handleIncomingJs br cid name argsJson =
  case decodeValues argsJson of
    Left e
      | cid /= 0  -> replyJs br cid (Left (BridgeDecodeError (T.pack e)))
      | otherwise -> hPutStrLn stderr $ "[Bridge] dropped bad notification payload for "
                       <> T.unpack name <> ": " <> e
    Right args -> dispatchIncoming br name args $ \r ->
      when (cid /= 0) $ replyJs br cid r

replyJs :: Bridge -> Int64 -> Either BridgeError BridgeValue -> IO ()
replyJs br cid r = case brTransport br of
  DirectT{} -> return ()  -- unreachable: direct replies close over the caller
  JsT ctx _ -> do
    let (okN, payload) = case r of
          Right v -> (1 :: Double, encodeValue1 v)
          Left e  -> (0, encodeError e)
    res <- try . flip runJSM ctx . void $
      jsg ("leksahBridge" :: Text) ^. js4 ("complete" :: Text)
        (sideText (brSide br)) (fromIntegral cid :: Double) okN payload
    case res of
      Right () -> return ()
      Left (e :: SomeException) ->
        hPutStrLn stderr $ "[Bridge] reply failed (context dead?): " <> show e

-- ---------------------------------------------------------------------
-- Handles
-- ---------------------------------------------------------------------

-- | Park a rich value in this end's registry and get its opaque key.  The
-- entry is dropped by 'freeHandle' (either side may call it) or wholesale
-- by 'bridgeReset' when the peer generation it was minted for dies.
newHandle :: Typeable a => Bridge -> a -> IO Handle
newHandle br v = do
  k <- freshId br
  pe <- readIORef (brPeerEpoch br)
  modifyIORef' (brHandles br) (M.insert k (toDyn v, pe))
  return (Handle (brSide br) k pe)

derefHandle :: forall a. Typeable a => Bridge -> Handle -> IO (Either HandleError a)
derefHandle br h
  | hSide h /= brSide br = return (Left HandleWrongSide)
  | otherwise = do
      m <- readIORef (brHandles br)
      case M.lookup (hKey h) m of
        Nothing -> return (Left HandleFreed)
        Just (d, e)
          | e /= hEpoch h -> return (Left HandleStaleEpoch)
          | otherwise -> return $ case fromDynamic d :: Maybe a of
              Nothing -> Left HandleTypeMismatch
              Just a  -> Right a

-- | Free a handle.  On the owning end this drops the registry entry; on the
-- borrowing end it routes a @__free@ notification to the owner.
freeHandle :: Bridge -> Handle -> IO ()
freeHandle br h
  | hSide h == brSide br = modifyIORef' (brHandles br) (M.delete (hKey h))
  | otherwise            = call_ br "__free" [BHandle h]

-- | Owner-side bracket for handles that don't outlive the enclosing action.
withHandle :: Typeable a => Bridge -> a -> (Handle -> IO r) -> IO r
withHandle br v = bracket (newHandle br v) (freeHandle br)

-- ---------------------------------------------------------------------
-- Callbacks and streams
-- ---------------------------------------------------------------------

-- | Expose a Haskell callback as an anonymous endpoint; the returned
-- 'BridgeValue' can be passed in any argument position and turned back
-- into a fire-and-forget closure on the peer with 'callbackClosure'.
toCallback :: Bridge -> ([BridgeValue] -> IO ()) -> IO BridgeValue
toCallback br k = do
  key <- freshId br
  pe <- readIORef (brPeerEpoch br)
  expose br (cbName key) $ \args -> k args >> return BNull
  modifyIORef' (brCbEpochs br) (M.insert key pe)
  return (BCallback (CallbackRef (brSide br) key pe))

cbName :: Int64 -> Text
cbName key = "__cb/" <> T.pack (show key)

-- | The receiving side of 'toCallback': invoke the peer's callback
-- (fire-and-forget; a callback that needs a result is just a named
-- endpoint).
callbackClosure :: Bridge -> CallbackRef -> [BridgeValue] -> IO ()
callbackClosure br (CallbackRef s k _) args
  | s == brSide br = do
      -- our own callback came back around: run it locally
      handlers <- readIORef (brExposed br)
      mapM_ (\h -> void (h args)) (M.lookup (cbName k) handlers)
  | otherwise = call_ br (cbName k) args

-- | Batching knobs for 'openStreamSink'.
data StreamConfig = StreamConfig
  { scMaxDelayUs :: Int  -- ^ wait this long after the first buffered item
  , scMaxItems   :: Int  -- ^ …unless this many items pile up first
  } deriving (Eq, Show)

defaultStreamConfig :: StreamConfig
defaultStreamConfig = StreamConfig { scMaxDelayUs = 16000, scMaxItems = 256 }

data StreamSink = StreamSink
  { ssPush  :: BridgeValue -> IO ()
  , ssClose :: IO ()
  }

-- | Receiver side of a stream: get a token to hand to the sender.  Items
-- arrive in batches on this end's worker; an empty batch is EOF (senders
-- never flush empty).
newStreamTarget :: Bridge -> ([BridgeValue] -> IO ()) -> IO () -> IO BridgeValue
newStreamTarget br onItems onClose = toCallback br $ \case
  []    -> onClose
  items -> onItems items

-- | Sender side: turn the receiver's token into a batching sink.  Flushes
-- coalesce with exactly one flush in flight (the reply is the ack) — the
-- @registerResync@ shape, which is the proven answer to flooding a jsaddle
-- context.
openStreamSink :: Bridge -> StreamConfig -> BridgeValue -> IO StreamSink
openStreamSink br cfg tok = do
  name <- case tok of
    BCallback (CallbackRef s k _) | s /= brSide br -> return (cbName k)
    _ -> ioError (userError "openStreamSink: expected the peer's stream token")
  buf    <- newTVarIO ([] :: [BridgeValue])
  closed <- newTVarIO False
  _ <- forkIO $
    let loop = do
          -- wait for something to do
          (items, isClosed) <- atomically $ do
            xs <- readTVar buf
            c <- readTVar closed
            when (null xs && not c) retry
            return (xs, c)
          -- brief accumulation window unless the buffer is already big
          when (not (null items) && length items < scMaxItems cfg) $
            threadDelay (scMaxDelayUs cfg)
          batch <- atomically $ do
            xs <- readTVar buf
            writeTVar buf []
            return (reverse xs)
          unless (null batch) . void $ call br name batch  -- reply = ack
          done <- atomically $ do
            xs <- readTVar buf
            c <- readTVar closed
            return (c && null xs)
          if done
            then void $ call br name []  -- EOF marker
            else loop
    in loop
  return StreamSink
    { ssPush  = \v -> atomically (readTVar closed >>= \c ->
                        unless c (readTVar buf >>= writeTVar buf . (v :)))
    , ssClose = atomically (writeTVar closed True)
    }

-- ---------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------

data HelloInfo = HelloInfo
  { hiProtocol     :: Int
  , hiCommit       :: Text
  , hiCapabilities :: [Text]
  } deriving (Eq, Show, Generic)

instance ToJSON HelloInfo
instance FromJSON HelloInfo

-- | Exchange hello info with the peer: installs our responder, then calls
-- the peer's.  Protocol mismatch is an error; commit mismatch only warns
-- (the halves WILL be built from different commits in dev).
helloHandshake :: Bridge -> HelloInfo -> IO (Either BridgeError HelloInfo)
helloHandshake br mine = do
  exposeJSON br "__hello" $ \(theirs :: HelloInfo) -> do
    warnSkew theirs
    return mine
  r <- callJSON br "__hello" mine
  case r of
    Left e -> return (Left e)
    Right theirs
      | hiProtocol theirs /= bridgeProtocolVersion ->
          return . Left . BridgeRemoteError $
            "bridge protocol mismatch: peer " <> T.pack (show (hiProtocol theirs))
            <> " vs ours " <> T.pack (show bridgeProtocolVersion)
      | otherwise -> warnSkew theirs >> return (Right theirs)
 where
  warnSkew theirs =
    when (hiCommit theirs /= hiCommit mine && not (T.null (hiCommit theirs))) $
      hPutStrLn stderr $ "[Bridge] halves built from different commits: "
        <> T.unpack (hiCommit mine) <> " vs " <> T.unpack (hiCommit theirs)

-- | Run an action whenever the peer goes away (page reload / transport
-- death).  Never fires in direct mode.
onPeerReset :: Bridge -> IO () -> IO ()
onPeerReset br act = modifyIORef' (brOnReset br) (act :)

-- | The host detected peer death (e.g. the jsaddle context died): reject
-- every pending call, bump the peer epoch, wipe handles and callbacks
-- minted for the dead generation, and notify subscribers.
bridgeReset :: Bridge -> IO ()
bridgeReset br = do
  pend <- atomicModifyIORef' (brPending br) (\m -> (M.empty, m))
  mapM_ (`putMVar` Left BridgeDisconnected) (M.elems pend)
  dead <- atomicModifyIORef' (brPeerEpoch br) (\e -> (e + 1, e))
  modifyIORef' (brHandles br) (M.filter ((/= dead) . snd))
  deadCbs <- atomicModifyIORef' (brCbEpochs br) $ \m ->
    let (gone, keep) = M.partition (== dead) m in (keep, M.keys gone)
  modifyIORef' (brExposed br) (\m -> foldr (M.delete . cbName) m deadCbs)
  readIORef (brOnReset br) >>= sequence_

-- ---------------------------------------------------------------------
-- Wire encoding (JS mode only; direct mode passes values as-is)
-- ---------------------------------------------------------------------

bvToJSON :: BridgeValue -> Value
bvToJSON BNull        = Null
bvToJSON (BBool b)    = Bool b
bvToJSON (BNum n)     = toJSON n
bvToJSON (BText t)    = String t
bvToJSON (BJson v)    = object ["$j" .= v]
bvToJSON (BBytes bs)  = object ["$b64" .= either (const "") id (decodeUtf8' (B64.encode bs))]
bvToJSON (BHandle (Handle s k e)) =
  object ["$h" .= object ["s" .= sideText s, "k" .= k, "e" .= e]]
bvToJSON (BCallback (CallbackRef s k e)) =
  object ["$cb" .= object ["s" .= sideText s, "k" .= k, "e" .= e]]

bvFromJSON :: Value -> Either String BridgeValue
bvFromJSON Null       = Right BNull
bvFromJSON (Bool b)   = Right (BBool b)
bvFromJSON (Number n) = Right (BNum (realToFrac n))
bvFromJSON (String t) = Right (BText t)
bvFromJSON v@(Object o) = case KM.toList o of
  [("$j", inner)]  -> Right (BJson inner)
  [("$b64", String t)] -> case B64.decode (encodeUtf8 t) of
    Right bs -> Right (BBytes bs)
    Left e   -> Left ("bad base64: " <> e)
  [("$h", ref)]  -> parseRef ref >>= \(s, k, e) -> Right (BHandle (Handle s k e))
  [("$cb", ref)] -> parseRef ref >>= \(s, k, e) -> Right (BCallback (CallbackRef s k e))
  _ -> Right (BJson v)  -- untagged structure: robustness fallback
 where
  parseRef r = case AT.parseEither
                 (AT.withObject "ref" $ \ro ->
                    (,,) <$> ro .: "s" <*> ro .: "k" <*> ro .: "e") r of
    Left e -> Left e
    Right (st :: Text, k, e) -> case sideFromText st of
      Just s  -> Right (s, k, e)
      Nothing -> Left ("bad side " <> T.unpack st)
bvFromJSON v@(Array _) = Right (BJson v)

encodeValues :: [BridgeValue] -> Text
encodeValues = jsonText . toJSON . map bvToJSON

encodeValue1 :: BridgeValue -> Text
encodeValue1 = jsonText . bvToJSON

encodeValue1' :: BridgeValue -> Value
encodeValue1' = bvToJSON

encodeError :: BridgeError -> Text
encodeError = jsonText . String . errText
 where
  errText (BridgeUnknownEndpoint n) = "unknown endpoint: " <> n
  errText BridgeTimeout             = "timeout"
  errText BridgeDisconnected        = "disconnected"
  errText (BridgeRemoteError t)     = t
  errText (BridgeDecodeError t)     = "decode error: " <> t

decodeError :: Text -> BridgeError
decodeError t = case eitherDecodeStrict' (encodeUtf8 t) of
  Right (String s)
    | Just rest <- T.stripPrefix "unknown endpoint: " s -> BridgeUnknownEndpoint rest
    | s == "timeout"      -> BridgeTimeout
    | s == "disconnected" -> BridgeDisconnected
    | otherwise           -> BridgeRemoteError s
  _ -> BridgeRemoteError t

decodeValues :: Text -> Either String [BridgeValue]
decodeValues t = case eitherDecodeStrict' (encodeUtf8 t) of
  Left e           -> Left e
  Right (Array xs) -> traverse bvFromJSON (foldr (:) [] xs)
  Right _          -> Left "expected a JSON array of arguments"

decodeValue1 :: Text -> Either String BridgeValue
decodeValue1 t = eitherDecodeStrict' (encodeUtf8 t) >>= bvFromJSON

jsonText :: Value -> Text
jsonText = either (const "null") id . decodeUtf8' . LBS.toStrict . Aeson.encode

-- ---------------------------------------------------------------------
-- The JS glue
-- ---------------------------------------------------------------------

-- | The @window.leksahBridge@ glue: a pure router between the two halves'
-- dispatchers, plus a page-object stash table.  Idempotent — whichever half
-- (or the hosting page) evaluates it first wins; calls made before the
-- other half attaches are queued and replayed on attach.
bridgeGlueJs :: Text
bridgeGlueJs = T.unlines
  [ "window.leksahBridge = window.leksahBridge || (function(){"
  , "  var VERSION = 1;"
  , "  var ends = {};      // side ('F'|'B') -> {dispatch, reply}"
  , "  var queued = {};    // side -> [[kind, args]] until that side attaches"
  , "  var epoch = { F: 0, B: 0 };"
  , "  var objs = new Map(); var nextObj = 1;"
  , "  function other(s){ return s === 'F' ? 'B' : 'F'; }"
  , "  function deliver(side, kind, args){"
  , "    var e = ends[side];"
  , "    if (e) { e[kind].apply(null, args); }"
  , "    else { (queued[side] = queued[side] || []).push([kind, args]); }"
  , "  }"
  , "  return {"
  , "    version: VERSION,"
  , "    attach: function(side, version, dispatch, reply){"
  , "      if (version !== VERSION)"
  , "        throw new Error('leksahBridge version mismatch: ' + version + ' vs ' + VERSION);"
  , "      epoch[side]++;"
  , "      ends[side] = { dispatch: dispatch, reply: reply };"
  , "      var q = queued[side] || []; delete queued[side];"
  , "      q.forEach(function(m){ ends[side][m[0]].apply(null, m[1]); });"
  , "      return epoch[side];"
  , "    },"
  , "    detach: function(side){ delete ends[side]; },"
  , "    call: function(fromSide, seq, callId, name, argsJson){"
  , "      deliver(other(fromSide), 'dispatch', [seq, callId, name, argsJson]);"
  , "    },"
  , "    complete: function(fromSide, callId, ok, resultJson){"
  , "      deliver(other(fromSide), 'reply', [callId, ok, resultJson]);"
  , "    },"
  , "    stash: function(o){ var i = nextObj++; objs.set(i, o); return i; },"
  , "    unstash: function(i){ return objs.get(i); },"
  , "    drop: function(i){ objs.delete(i); }"
  , "  };"
  , "})();"
  ]
