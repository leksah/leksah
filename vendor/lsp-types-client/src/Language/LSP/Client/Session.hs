{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | A concurrent, callback-driven LSP client session.
--
-- 'start' spawns a language server and begins a reader thread that
-- continuously decodes incoming JSON-RPC messages and routes them:
--
--   * /Responses/ (an @id@, no @method@) are matched to the originating
--     request and its callback is invoked with a typed result or error.
--   * /Notifications/ (a @method@, no @id@) are handed to 'onNotification'
--     as @(method-string, raw-params)@ — the caller decodes the params with
--     the matching @lsp-types@ @FromJSON@ instance.
--   * /Server-to-client requests/ (both @method@ and @id@) are answered by
--     'onServerRequest'; a sensible default replies to the handshake
--     requests every server issues (@workspace\/configuration@, capability
--     registration, progress creation).
--
-- Sending is non-blocking: 'request' records a callback and returns
-- immediately, so a single session can have many requests in flight and is
-- always listening for server-initiated traffic — the shape an event-driven
-- editor UI wants (unlike a blocking, run-a-script session monad).
module Language.LSP.Client.Session
    ( Client
    , ClientConfig(..)
    , defaultClientConfig
    , start
    , stop
    , request
    , notify
    ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Exception (SomeException, catch, try)
import           Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Int (Int32)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO (hGetLine, hIsEOF)
import           System.Process (terminateProcess)

import           Language.LSP.Protocol.Message

import           Language.LSP.Client.Transport

-- | Callbacks the session invokes on server-initiated traffic.  Params and
-- results are handed over as raw JSON 'Value's; decode them with the
-- @lsp-types@ @FromJSON@ instance for the relevant method (this keeps the
-- library independent of exactly which methods a given editor cares about).
data ClientConfig = ClientConfig
    { onNotification  :: Text -> Value -> IO ()
      -- ^ a server-to-client notification: @method@ and its @params@.
    , onServerRequest :: Text -> Value -> IO (Either ResponseError Value)
      -- ^ a server-to-client request: @method@ and @params@, producing the
      -- response @result@ (or an error).
    , onStderr        :: Text -> IO ()
      -- ^ one line of the server's stderr (its human-readable log).
    }

-- | A no-op configuration that still answers the mandatory handshake
-- requests so a server can finish initializing.
defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
    { onNotification  = \_ _ -> return ()
    , onServerRequest = defaultServerRequest
    , onStderr        = \_ -> return ()
    }

-- | A running session.
data Client = Client
    { clientHandles :: ServerHandles
    , clientNextId  :: TVar Int32
    , clientPending :: TVar (Map.Map Int32 (Either ResponseError Value -> IO ()))
    , clientConfig  :: ClientConfig
    }

-- | Spawn a language server and start listening.  Returns as soon as the
-- process is up; use 'request' \/ 'notify' to drive it and the
-- 'ClientConfig' callbacks to observe it.
start
    :: FilePath                 -- ^ server executable
    -> [String]                 -- ^ arguments
    -> Maybe FilePath           -- ^ working directory (project root)
    -> Maybe [(String, String)] -- ^ environment
    -> ClientConfig
    -> IO Client
start cmd args mcwd menv cfg = do
    hs   <- spawnServer cmd args mcwd menv
    nid  <- newTVarIO 1
    pend <- newTVarIO Map.empty
    let client = Client hs nid pend cfg
    _ <- forkIO (stderrLoop hs cfg)
    _ <- forkIO (readerLoop client)
    return client

-- | Terminate the server and fail any outstanding request callbacks.
stop :: Client -> IO ()
stop client = do
    terminateProcess (shProc (clientHandles client))
    failAllPending client "lsp-client: session stopped"

-- | Send a request and register a callback for its response.  Non-blocking:
-- returns the assigned request id immediately.  The result 'Value' from the
-- server is decoded to @MessageResult m@ using the type known here at the
-- call site; a decode failure is reported as an internal 'ResponseError'.
request
    :: forall f (m :: Method f Request)
     . (ToJSON (MessageParams m), FromJSON (MessageResult m))
    => Client
    -> SMethod m
    -> MessageParams m
    -> (Either ResponseError (MessageResult m) -> IO ())
    -> IO (LspId m)
request client method params cb = do
    n <- atomically $ do
        i <- readTVar (clientNextId client)
        writeTVar (clientNextId client) (i + 1)
        return i
    let store = cb . (>>= decodeResult)
        decodeResult v = case fromJSON v of
            Success x -> Right x
            Error e   -> Left (synthError (-32603)
                                 ("lsp-client: could not decode result: " <> T.pack e))
    atomically $ modifyTVar' (clientPending client) (Map.insert n store)
    let msg = TRequestMessage "2.0" (IdInt n) method params
    sendFramed (shIn (clientHandles client)) (BSL.toStrict (encode msg))
    return (IdInt n)

-- | Send a notification (fire-and-forget, no response).
notify
    :: forall f (m :: Method f Notification)
     . ToJSON (MessageParams m)
    => Client -> SMethod m -> MessageParams m -> IO ()
notify client method params = do
    let msg = TNotificationMessage "2.0" method params
    sendFramed (shIn (clientHandles client)) (BSL.toStrict (encode msg))

--------------------------------------------------------------------------------
-- Reader
--------------------------------------------------------------------------------

readerLoop :: Client -> IO ()
readerLoop client = do
    m <- recvFramed (shOut (clientHandles client))
    case m of
        Nothing -> failAllPending client "lsp-client: language server closed the connection"
        Just bs -> do
            handleIncoming client bs `catch` \(e :: SomeException) ->
                onStderr (clientConfig client) ("lsp-client reader error: " <> T.pack (show e))
            readerLoop client

handleIncoming :: Client -> BS.ByteString -> IO ()
handleIncoming client bs =
    case decodeStrict' bs of
        Just (Object o) ->
            let mMethod = KM.lookup "method" o >>= asText
                mId     = KM.lookup "id" o
                params  = fromMaybe Null (KM.lookup "params" o)
            in case (mMethod, mId) of
                (Just method, Just idv) -> handleServerRequest client method idv params
                (Just method, Nothing)  -> onNotification (clientConfig client) method params
                (Nothing,     Just idv) -> handleResponse client idv
                                             (KM.lookup "result" o) (KM.lookup "error" o)
                (Nothing,     Nothing)  -> logMalformed client "message with neither method nor id"
        _ -> logMalformed client "message was not a JSON object"
  where
    asText (String t) = Just t
    asText _          = Nothing

handleResponse :: Client -> Value -> Maybe Value -> Maybe Value -> IO ()
handleResponse client idv mResult mError =
    case idv of
        Number sci -> do
            let i = round sci :: Int32
            mcb <- atomically $ do
                m <- readTVar (clientPending client)
                writeTVar (clientPending client) (Map.delete i m)
                return (Map.lookup i m)
            case mcb of
                Nothing -> logMalformed client ("response for unknown id " <> T.pack (show i))
                Just cb -> case mError of
                    Just ev -> cb (Left (valueToError ev))
                    Nothing -> cb (Right (fromMaybe Null mResult))
        _ -> logMalformed client "response id was not an integer"

handleServerRequest :: Client -> Text -> Value -> Value -> IO ()
handleServerRequest client method idv params = do
    resp <- onServerRequest (clientConfig client) method params
    let obj = case resp of
            Right v -> object ["jsonrpc" .= t "2.0", "id" .= idv, "result" .= v]
            Left e  -> object ["jsonrpc" .= t "2.0", "id" .= idv, "error"  .= e]
    sendFramed (shIn (clientHandles client)) (BSL.toStrict (encode obj))
  where
    t :: Text -> Text
    t = id

--------------------------------------------------------------------------------
-- Default server-request handling (the handshake round-trips)
--------------------------------------------------------------------------------

defaultServerRequest :: Text -> Value -> IO (Either ResponseError Value)
defaultServerRequest method params = return . Right $ case method of
    -- workspace/configuration expects one entry per requested item; we have
    -- no configuration to offer, so reply null for each.
    "workspace/configuration" -> case params of
        Object o -> case KM.lookup "items" o of
            Just (Array items) -> Array (fmap (const Null) items)
            _                  -> Array mempty
        _ -> Array mempty
    -- client/registerCapability, window/workDoneProgress/create, and the
    -- various */refresh requests are acknowledged with a null result.
    _ -> Null

--------------------------------------------------------------------------------
-- stderr drain
--------------------------------------------------------------------------------

stderrLoop :: ServerHandles -> ClientConfig -> IO ()
stderrLoop hs cfg = go
  where
    go = do
        eof <- hIsEOF (shErr hs) `catch` \(_ :: SomeException) -> return True
        if eof
            then return ()
            else do
                r <- try (hGetLine (shErr hs))
                case r of
                    Left (_ :: SomeException) -> return ()
                    Right l -> onStderr cfg (T.pack l) >> go

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

failAllPending :: Client -> Text -> IO ()
failAllPending client msg = do
    cbs <- atomically $ do
        m <- readTVar (clientPending client)
        writeTVar (clientPending client) Map.empty
        return (Map.elems m)
    let err = synthError (-32603) msg
    mapM_ (\cb -> cb (Left err)) cbs

logMalformed :: Client -> Text -> IO ()
logMalformed client m = onStderr (clientConfig client) ("lsp-client: " <> m)

-- | Decode a JSON-RPC error object into a 'ResponseError', falling back to a
-- synthetic internal error if it does not parse.
valueToError :: Value -> ResponseError
valueToError v = case fromJSON v of
    Success e -> e
    Error _   -> synthError (-32603) "lsp-client: malformed error object"

-- | Build a 'ResponseError' via its JSON representation, so we do not depend
-- on the exact spelling of the @lsp-types@ error-code constructors.
synthError :: Int -> Text -> ResponseError
synthError code msg =
    case fromJSON (object ["code" .= code, "message" .= msg]) of
        Success e -> e
        Error _   -> error "lsp-client: ResponseError JSON shape changed"
