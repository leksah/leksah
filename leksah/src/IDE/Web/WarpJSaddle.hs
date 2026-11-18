{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- SPDX-License-Identifier: Apache-2.0
--
-- 'jsaddleOrWatched' is jsaddle-warp 0.9.9.7's @jsaddleOr@ (MIT, (c) Hamish
-- Mackenzie), with its two internal helpers, plus the one thing it lacks:
-- knowing when a browser connection has gone.

-- | The warp front end's jsaddle server, as jsaddle-warp's @jsaddleOr@, but it
-- notices a browser connection going away and tells the rest of leksah.
--
-- Under jsaddle-warp a page reload or a closed tab ends a connection with no
-- word to the application: its handler pings the socket only AFTER the
-- window's build has returned, and nothing ends the window's frame thread.  A
-- build, or a frame, waiting for a JavaScript reply from that browser then
-- waits for ever — under the frame lock every window shares, so no window,
-- the reloaded page included, can open again ("warp is not loading").
--
-- Here the socket is watched from the moment it is accepted, alongside the
-- build; when it goes, a build still running is interrupted
-- ('IDE.Web.WindowScope.ConnectionGone', which it reports as a failed build)
-- and 'IDE.Web.WindowScope.connectionClosed' retires the window.
-- (Nothing under the JS backend: there is no server there — and no wai or
-- websockets — and "IDE.Web.Main" imports this only outside it.)
module IDE.Web.WarpJSaddle
#if defined(ghcjs_HOST_OS)
  () where
#else
  ( jsaddleOrWatched
  ) where


import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay, throwTo)
import Control.Concurrent.MVar (newEmptyMVar, readMVar, tryPutMVar, tryReadMVar)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, takeTMVar, tryTakeTMVar)
import Control.Exception (AsyncException, SomeException, bracket, catch, fromException, handle, throwIO)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy (ByteString)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Network.HTTP.Types as H
import Network.HTTP.Types (Status(..))
import Network.Wai (Application, Response)
import qualified Network.Wai as W
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
       (ConnectionOptions, Connection, DataMessage(..), ServerApp, acceptRequest,
        receiveDataMessage, sendPing, sendTextData)

import Language.Javascript.JSaddle (askJSM)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (indexHtml)
import Language.Javascript.JSaddle.Types (Batch, JSM, JSContextRef(..))

import IDE.Web.WindowScope (ConnectionGone(..), connectionClosed)

jsaddleOrWatched :: ConnectionOptions -> JSM () -> Application -> IO Application
jsaddleOrWatched opts entryPoint otherApp = do
    syncHandlers <- newIORef M.empty
    asyncHandlers <- newIORef M.empty
    let wsApp :: ServerApp
        wsApp pending_conn = do
            conn <- acceptRequest pending_conn
            initMsg <- receiveDataMessage conn
            case textOf initMsg of
                Just "" -> withBatchSender (sendTextData conn . encode) $ \publishBatch -> do
                    ctxVar <- newEmptyMVar
                    rec (processResult, processSyncResult, start) <- runJavaScript publishBatch $ do
                            ctx <- contextId <$> askJSM
                            liftIO . void $ tryPutMVar ctxVar ctx
                            let syncKey = T.pack (show ctx)
                            liftIO $ atomicModifyIORef' syncHandlers (\m -> (M.insert syncKey processSyncResult m, ()))
                            liftIO $ atomicModifyIORef' asyncHandlers (\m -> (M.insert syncKey processResult m, ()))
                            liftIO $ sendTextData conn (encode syncKey)
                            entryPoint
                    -- The socket is watched WHILE the window builds: a build
                    -- waiting on a browser that went away is interrupted, and
                    -- the window is retired either way.
                    me <- myThreadId
                    building <- newIORef True
                    closed <- newEmptyMVar
                    _ <- forkIO $ do
                        waitTillClosed conn
                        stillBuilding <- readIORef building
                        if stillBuilding then throwTo me ConnectionGone else return ()
                        tryReadMVar ctxVar >>= mapM_ connectionClosed
                        void (tryPutMVar closed ())
                    start `catch` \ConnectionGone -> return ()
                    writeIORef building False
                    readMVar closed
                Just syncKey ->
                    M.lookup (T.decodeUtf8 $ LBS.toStrict syncKey) <$> readIORef syncHandlers >>= \case
                      Nothing -> error "jsaddle missing sync message handler"
                      Just processResult ->
                        forever $
                          receiveDataMessage conn >>= \msg -> case textOf msg of
                              Just t ->
                                  case decode t of
                                      Nothing -> error $ "jsaddle Results decode failed : " <> show t
                                      Just r  -> processResult r
                              _ -> error "jsaddle WebSocket unexpected binary data"
                _ -> error "jsaddle WebSocket unexpected binary data"

        -- Until the socket is gone: a browser that closes it properly (a
        -- reload: close code 1001) is seen at once, by reading — nothing else
        -- reads this socket after its first message (results come on the
        -- page's second one) — and one that vanishes (a dropped network) by
        -- a ping every 3s failing.  (jsaddle-warp: pings only, every 10s and
        -- only after the build; a cleanly closed peer can keep accepting
        -- pings for a while — a reload took 20s to be noticed.)
        waitTillClosed :: Connection -> IO ()
        waitTillClosed conn = do
            gone <- newEmptyMVar
            let done = void (tryPutMVar gone ())
            r <- forkIO $ (ignore `handle` forever (void (receiveDataMessage conn))) >> done
            p <- forkIO $ (ignore `handle` ping (1 :: Int)) >> done
            readMVar gone
            killThread r >> killThread p
          where
            ping i = do
                threadDelay (3 * 1000 * 1000)
                sendPing conn (T.pack $ show i)
                ping (i + 1)

        ignore :: SomeException -> IO ()
        ignore e = case fromException e of
            Just (async :: AsyncException) -> throwIO async
            Nothing                        -> return ()

        syncHandler :: Application
        syncHandler req sendResponse = case (W.requestMethod req, W.pathInfo req) of
            ("POST", ["sync", syncKey]) -> do
                body <- W.lazyRequestBody req
                case decode body of
                    Nothing -> error $ "jsaddle sync message decode failed : " <> show body
                    Just result ->
                        M.lookup syncKey <$> readIORef syncHandlers >>= \case
                            Nothing -> error "jsaddle missing sync message handler"
                            Just handler -> do
                                next <- encode <$> handler result
                                sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json")] next
            (method, _) -> catch404 otherApp req sendResponse
              where catch404 = W.modifyResponse $ \resp ->
                      case (method, W.responseStatus resp) of
                        ("GET", Status 404 _) -> indexResponse
                        _ -> resp
    return $ websocketsOr opts wsApp syncHandler

indexResponse :: Response
indexResponse = W.responseLBS H.status200 [("Content-Type", "text/html")] indexHtml

textOf :: DataMessage -> Maybe ByteString
textOf = \case
    Text t _ -> Just t
    _        -> Nothing

-- | jsaddle-warp's batch sender: at most one batch in flight and one waiting
-- (a newer one replaces it); the sender stops with the connection.
withBatchSender :: (Batch -> IO ()) -> ((Batch -> IO ()) -> IO a) -> IO a
withBatchSender send action = do
    pending <- newEmptyTMVarIO
    let publish batch = atomically $ do
            void $ tryTakeTMVar pending
            putTMVar pending batch
        sender = forever $ atomically (takeTMVar pending) >>= send
    bracket (forkIO (sender `catch` \(_ :: SomeException) -> return ())) killThread $ \_ ->
        action publish
#endif
