{-# OPTIONS_GHC -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.ServerConnection
-- Copyright   :  2007-2010 Juergen Nicklisch-Franken, Hamish Mackenzie
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
import IDE.System.Process(runProcess)
import GHC.Conc(threadDelay)
import Control.Monad.Trans(liftIO)
import System.IO
import Control.Exception (SomeException(..), catch)
import Prelude hiding(catch)
import Control.Concurrent(forkIO)
import Graphics.UI.Gtk(postGUIAsync)
import Control.Event(triggerEvent)

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
            handle <- reifyIDE $ \ideR -> do
                catch (connectTo (serverIP prefs') (PortNumber(PortNum (fromIntegral $ serverPort prefs'))))
                    (\(exc :: SomeException) -> do
                        catch (startServer (serverPort prefs'))
                            (\(exc :: SomeException) -> throwIDE ("Can't start leksah-server" ++ show exc))
                        mbHandle <- waitForServer prefs' 100
                        case mbHandle of
                            Just handle ->  return handle
                            Nothing     ->  throwIDE ("Can't connect to leksah-server"))
            modifyIDE_ (\ ide -> ide{server = Just handle})
            doCommand handle
            return ()
    where
        doCommand handle = do
        triggerEventIDE (StatusbarChanged [CompartmentCollect True])
        reifyIDE $ \ideR -> forkIO $ do
            hPutStrLn handle (show command)
            hFlush handle
            resp <- hGetLine handle
            postGUIAsync (reflectIDE (do
                    triggerEvent ideR (StatusbarChanged [CompartmentCollect False])
                    cont (read resp)
                    return ()) ideR)

startServer :: Int -> IO ()
startServer port = do
    runProcess "leksah-server" ["--server=" ++ show port, "+RTS", "-N2", "-RTS"]
        Nothing Nothing Nothing Nothing Nothing
    return ()

-- | s is in tenth's of seconds
waitForServer :: Prefs -> Int -> IO (Maybe Handle)
waitForServer _ 0 = return Nothing
waitForServer prefs s = do
    threadDelay 100000 -- 0.1 second
    catch (do
        handle <- liftIO $ connectTo (serverIP prefs) (PortNumber(PortNum (fromIntegral $ serverPort prefs)))
        return (Just handle))
        (\(exc :: SomeException) -> waitForServer prefs (s-1))




