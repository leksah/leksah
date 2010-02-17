{-# OPTIONS_GHC -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.ServerConnection
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
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

import IDE.Core.State
import Network (connectTo,PortID(..))
import Network.Socket (PortNumber(..))
import System.Process(runCommand)
import GHC.Conc(threadDelay)
import Control.Monad.Trans(liftIO)
import System.IO
import Control.Exception (SomeException(..), catch)
import Prelude hiding(catch)
import Control.Concurrent(forkIO)
import Graphics.UI.Gtk(postGUIAsync)
import Control.Event(triggerEvent)
import Debug.Trace


doServerCommand :: ServerCommand -> (ServerAnswer -> IDEM alpha) -> IDEAction
doServerCommand command cont = do
    server' <- readIDE server
    case server' of
        Just handle -> do
            isOpen <- liftIO $ hIsOpen handle
            if isOpen
                then doCommand handle >> return ()
                else do
                    modifyIDE_ (\ ide -> ide{server = Nothing})
                    doServerCommand command cont
        Nothing -> do
            prefs' <- readIDE prefs
            handle <- liftIO $ do
                catch (connectTo "localhost" (PortNumber(PortNum (fromIntegral $ serverPort prefs'))))
                    (\(exc :: SomeException) -> do
                        catch (startServer (serverPort prefs'))
                            (\(exc :: SomeException) -> throwIDE ("Can't start leksah-server" ++ show exc))
                        threadDelay 5000000 -- in microseconds, so five seconds to start
                        catch (connectTo "localhost" (PortNumber(PortNum (fromIntegral $ serverPort prefs'))))
                            (\(exc :: SomeException) -> throwIDE ("Can't connect to leksah-server" ++ show exc)))
            doCommand handle
            return ()
    where
        doCommand handle = do
        triggerEventIDE (StatusbarChanged [CompartmentCollect True])
        reifyIDE $ \ideR -> forkIO $ do
            trace ("server call: " ++ show command) $
                hPutStrLn handle (show command)
            hFlush handle
            resp <- hGetLine handle
            trace ("server answer: " ++ resp)
                $ postGUIAsync (reflectIDE (do
                    triggerEvent ideR (StatusbarChanged [CompartmentCollect False])
                    cont (read resp)
                    return ()) ideR)

startServer :: Int -> IO ()
startServer port = do
    runCommand ("leksah-server --server=" ++ show port ++ " +RTS -N2 -RTS")
    return ()

