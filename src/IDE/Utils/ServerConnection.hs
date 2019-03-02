{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.ServerConnection
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | Server functionality
--
-----------------------------------------------------------------------------

module IDE.Utils.ServerConnection (
    doServerCommand,
) where

import Prelude ()
import Prelude.Compat
import IDE.Core.State
       (ServerCommand, ServerAnswer, IDEM, IDEAction, Prefs,
        readIDE, serverQueue, modifyIDE_, reflectIDE, server,
        prefs, serverIP, serverPort, throwIDE, triggerEventIDE_,
        IDEEvent(..), StatusbarCompartment(..))
import IDE.Gtk.State (postAsyncIDE)
import Network (connectTo,PortID(..))
import IDE.Utils.Tool (runProcess)
import GHC.Conc(threadDelay)
import System.IO (hGetLine, hFlush, hPrint, hIsOpen, Handle)
import Control.Exception (SomeException(..), catch)
import Control.Concurrent(forkIO, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ask)
import System.Log.Logger (getLevel, getRootLogger, debugM)
import Control.Monad (void, forever)
import qualified Data.Text as T (pack, unpack)
import Control.Lens ((.~), (?~))

doServerCommand :: ServerCommand -> (ServerAnswer -> IDEM ()) -> IDEAction
doServerCommand command cont = do
    q <- readIDE serverQueue >>= \case
        Just q -> return q
        Nothing -> do
            q <- liftIO newEmptyMVar
            modifyIDE_ $ serverQueue ?~ q
            ideR <- ask
            void . liftIO . forkIO . forever $ do
                debugM "leksah" "Ready for command"
                (command', cont') <- takeMVar q
                reflectIDE (doServerCommand' command' cont') ideR
            return q
    liftIO $ do
        _ <- tryTakeMVar q
        debugM "leksah" $ "Queue new command " ++ show command
        putMVar q (command, cont)

doServerCommand' :: ServerCommand -> (ServerAnswer -> IDEM ()) -> IDEAction
doServerCommand' command cont =
    readIDE server >>= \case
        Just handle -> do
            isOpen <- liftIO $ hIsOpen handle
            if isOpen
                then void (doCommand handle)
                else do
                    modifyIDE_ $ server .~ Nothing
                    doServerCommand command cont
        Nothing -> do
            prefs' <- readIDE prefs
            handle <- liftIO $
                catch (connectTo (T.unpack $ serverIP prefs') (PortNumber (fromIntegral $ serverPort prefs')))
                    (\(_ :: SomeException) -> do
                        catch (startServer (serverPort prefs'))
                            (\(exc :: SomeException) -> throwIDE ("Can't start leksah-server" <> T.pack (show exc)))
                        mbHandle <- waitForServer prefs' 100
                        case mbHandle of
                            Just handle ->  return handle
                            Nothing     ->  throwIDE "Can't connect to leksah-server")
            modifyIDE_ $ server ?~ handle
            doCommand handle
            return ()
    where
        doCommand handle = do
            postAsyncIDE $ triggerEventIDE_ (StatusbarChanged [CompartmentCollect True])
            resp <- liftIO $ do
                debugM "leksah" $ "Sending server command " ++ show command
                hPrint handle command
                hFlush handle
                debugM "leksah" $ "Waiting on server command " ++ show command
                hGetLine handle
            liftIO . debugM "leksah" $ "Server result " ++ resp
            postAsyncIDE $ do
                triggerEventIDE_ (StatusbarChanged [CompartmentCollect False])
                cont (read resp)

startServer :: Int -> IO ()
startServer port = do
    logger <- getRootLogger
    let verbosity = case getLevel logger of
                        Just level -> ["--verbosity=" ++ show level]
                        Nothing    -> []
    void $ runProcess "leksah-server"
        (["--server=" ++ show port, "+RTS", "-N2", "-RTS"] ++ verbosity)
        Nothing Nothing Nothing Nothing Nothing

-- | s is in tenth's of seconds
waitForServer :: Prefs -> Int -> IO (Maybe Handle)
waitForServer _ 0 = return Nothing
waitForServer prefs' s = do
    threadDelay 100000 -- 0.1 second
    catch (do
        handle <- liftIO $ connectTo (T.unpack $ serverIP prefs') (PortNumber (fromIntegral $ serverPort prefs'))
        return (Just handle))
        (\(_ :: SomeException) -> waitForServer prefs' (s-1))




