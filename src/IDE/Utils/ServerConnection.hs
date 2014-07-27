{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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

import IDE.Core.State
import Network (connectTo,PortID(..))
import Network.Socket (PortNumber(..))
import IDE.Utils.Tool (runProcess)
import GHC.Conc(threadDelay)
import System.IO
import Control.Exception (SomeException(..), catch)
import Prelude hiding(catch)
import Control.Concurrent(forkIO)
import Graphics.UI.Gtk(postGUIAsync)
import Control.Event(triggerEvent)
import Control.Monad.IO.Class (MonadIO(..))
import System.Log.Logger (getLevel, getRootLogger)
import Control.Monad (void)
import qualified Data.Text as T (pack, unpack)
import Data.Monoid ((<>))

doServerCommand :: ServerCommand -> (ServerAnswer -> IDEM alpha) -> IDEAction
doServerCommand command cont = do
    server' <- readIDE server
    case server' of
        Just handle -> do
            isOpen <- liftIO $ hIsOpen handle
            if isOpen
                then void (doCommand handle)
                else do
                    modifyIDE_ (\ ide -> ide{server = Nothing})
                    doServerCommand command cont
        Nothing -> do
            prefs' <- readIDE prefs
            handle <- reifyIDE $ \ideR ->
                catch (connectTo (T.unpack $ serverIP prefs') (PortNumber(PortNum (fromIntegral $ serverPort prefs'))))
                    (\(exc :: SomeException) -> do
                        catch (startServer (serverPort prefs'))
                            (\(exc :: SomeException) -> throwIDE ("Can't start leksah-server" <> T.pack (show exc)))
                        mbHandle <- waitForServer prefs' 100
                        case mbHandle of
                            Just handle ->  return handle
                            Nothing     ->  throwIDE "Can't connect to leksah-server")
            modifyIDE_ (\ ide -> ide{server = Just handle})
            doCommand handle
            return ()
    where
        doCommand handle = do
            triggerEventIDE (StatusbarChanged [CompartmentCollect True])
            reifyIDE $ \ideR -> forkIO $ do
                hPrint handle command
                hFlush handle
                resp <- hGetLine handle
                postGUIAsync (reflectIDE (do
                        triggerEvent ideR (StatusbarChanged [CompartmentCollect False])
                        cont (read resp)
                        return ()) ideR)

startServer :: Int -> IO ()
startServer port = do
    logger <- getRootLogger
    let verbosity = case getLevel logger of
                        Just level -> ["--verbosity=" ++ show level]
                        Nothing    -> []
    runProcess "leksah-server"
        (["--server=" ++ show port, "+RTS", "-N2", "-RTS"] ++ verbosity)
        Nothing Nothing Nothing Nothing Nothing
    return ()

-- | s is in tenth's of seconds
waitForServer :: Prefs -> Int -> IO (Maybe Handle)
waitForServer _ 0 = return Nothing
waitForServer prefs s = do
    threadDelay 100000 -- 0.1 second
    catch (do
        handle <- liftIO $ connectTo (T.unpack $ serverIP prefs) (PortNumber(PortNum (fromIntegral $ serverPort prefs)))
        return (Just handle))
        (\(exc :: SomeException) -> waitForServer prefs (s-1))




