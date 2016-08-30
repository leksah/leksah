{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
,   collectPackage
) where

import Prelude ()
import Prelude.Compat
import System.Console.GetOpt
    (ArgDescr(..), usageInfo, ArgOrder(..), getOpt, OptDescr(..))
import System.Environment (getArgs)
import Control.Monad (when, forM)
import Data.Version (showVersion)
import IDE.Utils.FileUtils
import IDE.Utils.Utils
import IDE.Utils.GHCUtils
import IDE.StrippedPrefs
import IDE.Metainfo.WorkspaceCollector
import Data.Maybe(catMaybes, fromJust, mapMaybe, isJust)
import qualified Data.Set as Set (member)
import IDE.Core.CTypes hiding (Extension)
import IDE.Metainfo.SourceDB (buildSourceForPackageDB, getDataDir, version)
import Data.Time
import Control.Exception
       (catch, SomeException)
import System.Log
import System.Log.Logger(updateGlobalLogger,rootLoggerName,addHandler,debugM,infoM,errorM,
    setLevel)
import System.Log.Handler.Simple(fileHandler)
import Network(withSocketsDo)
import Network.Socket
       (inet_addr, SocketType(..), SockAddr(..))
import IDE.Utils.Server
import System.IO (Handle, hPutStrLn, hGetLine, hFlush, hClose)
import IDE.HeaderParser(parseTheHeader)
import Data.IORef
import Control.Concurrent (MVar,putMVar)
import IDE.Metainfo.PackageCollector(collectPackage)
import Data.List (nub, delete)
import System.Directory
       (removeFile, doesFileExist, removeDirectoryRecursive,
        doesDirectoryExist)
import IDE.Metainfo.SourceCollectorH (PackageCollectStats(..))
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text as T (strip, pack, unpack)
import Data.Text (Text)
import Data.Monoid ((<>))

-- --------------------------------------------------------------------
-- Command line options
--


data Flag =    CollectSystem

             | ServerCommand (Maybe Text)
             --modifiers
             | Rebuild
             | Sources
             -- | Directory FilePath
             --others
             | VersionF
             | Help
             | Debug
             | Verbosity Text
             | LogFile Text
             | Forever
             | EndWithLast
       deriving (Show,Eq)

options :: [OptDescr Flag]

options =   [
-- main functions
             Option ['s'] ["system"] (NoArg CollectSystem)
                "Collects new information for installed packages"
         ,   Option ['r'] ["server"] (OptArg (ServerCommand . (T.pack <$>)) "Maybe Port")
                "Start as server."
         ,   Option ['b'] ["rebuild"] (NoArg Rebuild)
                "Modifier for -s and -p: Rebuild metadata"
         ,   Option ['o'] ["sources"] (NoArg Sources)
                "Modifier for -s: Gather info about pathes to sources"
         ,   Option ['v'] ["version"] (NoArg VersionF)
                "Show the version number of ide"
         ,   Option ['h'] ["help"] (NoArg Help)
                "Display command line options"
         ,   Option ['d'] ["debug"] (NoArg Debug)
                "Write ascii pack files"
         ,   Option ['e'] ["verbosity"] (ReqArg (Verbosity . T.pack) "Verbosity")
                "One of DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY"
         ,   Option ['l'] ["logfile"] (ReqArg (LogFile . T.pack) "LogFile")
                "File path for logging messages"
         ,   Option ['f'] ["forever"] (NoArg Forever)
                "Don't end the server when last connection ends"
         ,   Option ['c'] ["endWithLast"] (NoArg EndWithLast)
                "End the server when last connection ends"

    ]

header :: String
header = "Usage: leksah-server [OPTION...] files..."

ideOpts :: [String] -> IO ([Flag], [String])
ideOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options

-- ---------------------------------------------------------------------
-- | Main function
--

main :: IO ()
main =  withSocketsDo $ catch inner handler
    where
        handler (e :: SomeException) = do
            putStrLn $ "leksah-server: " ++ show e
            errorM "leksah-server" (show e)
            return ()
        inner = do
            args            <- getArgs
            (o,_)           <- ideOpts args
            let verbosity'   =  catMaybes $
                                    map (\x -> case x of
                                        Verbosity s -> Just s
                                        _           -> Nothing) o
            let verbosity    =  case verbosity' of
                                    [] -> INFO
                                    h:_ -> read $ T.unpack h
            let logFile'     =  catMaybes $
                                    map (\x -> case x of
                                        LogFile s   -> Just s
                                        _           -> Nothing) o
            let logFile     =  case logFile' of
                                    [] -> Nothing
                                    h:_ -> Just h
            updateGlobalLogger rootLoggerName (setLevel verbosity)
            when (isJust logFile) $  do
                handler' <- fileHandler (T.unpack $ fromJust logFile) verbosity
                updateGlobalLogger rootLoggerName (addHandler handler')
            infoM "leksah-server" "***server start"
            debugM "leksah-server" $ "args: " ++ show args
            dataDir         <- getDataDir
            prefsPath       <- getConfigFilePathForLoad strippedPreferencesFilename Nothing dataDir
            prefs           <- readStrippedPrefs prefsPath
            debugM "leksah-server" $ "prefs " ++ show prefs
            connRef  <- newIORef []
            localServerAddr <- inet_addr "127.0.0.1"

            if elem VersionF o
                then putStrLn $ "Leksah Haskell IDE (server), version " ++ showVersion version
                else if elem Help o
                    then putStrLn $ "Leksah Haskell IDE (server) " ++ usageInfo header options
                    else do
                        let servers     =   catMaybes $
                                                map (\x -> case x of
                                                                ServerCommand s -> Just s
                                                                _        -> Nothing) o
                        let sources     =   elem Sources o
                        let rebuild     =   elem Rebuild o
                        let debug       =   elem Debug o
                        let forever     =   elem Forever o
                        let endWithLast =   elem EndWithLast o
                        let newPrefs
                              | forever && not endWithLast = prefs{endWithLastConn = False}
                              | not forever && endWithLast = prefs{endWithLastConn = True}
                              | otherwise = prefs
                        if elem CollectSystem o
                            then do
                                debugM "leksah-server" "collectSystem"
                                collectSystem prefs debug rebuild sources []
                            else
                                case servers of
                                    (Nothing:_)  -> do
                                        running <- serveOne Nothing (server (fromIntegral
                                            (serverPort prefs)) newPrefs connRef localServerAddr)
                                        waitFor running
                                        return ()
                                    (Just ps:_)  -> do
                                        let port = read $ T.unpack ps
                                        running <- serveOne Nothing (server
                                            (fromIntegral port) newPrefs connRef localServerAddr)
                                        waitFor running
                                        return ()
                                    _ -> return ()

        server port prefs connRef hostAddr = Server (SockAddrInet port hostAddr) Stream
                                        (doCommands prefs connRef)

doCommands :: Prefs -> IORef [Handle] -> (Handle, t1, t2) -> MVar ()-> IO ()
doCommands prefs connRef (h,n,p) mvar = do
    atomicModifyIORef connRef (\ list -> (h : list, ()))
    doCommands' prefs connRef (h,n,p) mvar


doCommands' :: Prefs -> IORef [Handle] -> (Handle, t1, t2) -> MVar () -> IO ()
doCommands' prefs connRef (h,n,p) mvar = do
    debugM "leksah-server" "***wait"
    mbLine <- catch (Just <$> hGetLine h)
                (\ (_e :: SomeException) -> do
                    infoM "leksah-server" "***lost connection"
                    hClose h
                    atomicModifyIORef connRef (\ list -> (delete h list,()))
                    handles <- readIORef connRef
                    case handles of
                        [] -> do
                                if endWithLastConn prefs
                                    then do
                                       infoM "leksah-server" "***lost last connection - exiting"
                                       -- we're waiting on that mvar before exiting
                                       putMVar mvar ()
                                    else infoM "leksah-server" "***lost last connection - waiting"
                                return Nothing
                        _  -> return Nothing)
    case mbLine of
        Nothing -> return ()
        Just line -> do
            case read line of
                    SystemCommand rebuild sources _extract dbs -> --the extract arg is not used
                        catch (do
                            collectSystem prefs False rebuild sources dbs
                            hPutStrLn h (show ServerOK)
                            hFlush h)
                        (\ (e :: SomeException) -> do
                            hPutStrLn h (show (ServerFailed (T.pack $ show e)))
                            hFlush h)
                    WorkspaceCommand rebuild package path modList ->
                        catch (do
                            collectWorkspace package modList rebuild False path
                            hPutStrLn h (show ServerOK)
                            hFlush h)
                        (\ (e :: SomeException) -> do
                            hPutStrLn h (show (ServerFailed (T.pack $ show e)))
                            hFlush h)
                    ParseHeaderCommand filePath ->
                        catch (do
                            res <- parseTheHeader filePath
                            hPutStrLn h (show res)
                            hFlush h)
                        (\ (e :: SomeException) -> do
                            hPutStrLn h (show (ServerFailed (T.pack $ show e)))
                            hFlush h)
            doCommands' prefs connRef (h,n,p) mvar

collectSystem :: Prefs -> Bool -> Bool -> Bool -> [[FilePath]] -> IO()
collectSystem prefs writeAscii forceRebuild findSources dbLists = do
    collectorPath       <- getCollectorPath
    when forceRebuild $ do
        exists           <- doesDirectoryExist collectorPath
        when exists $ removeDirectoryRecursive collectorPath
        reportPath       <-  getConfigFilePathForSave "collectSystem.report"
        exists'          <- doesFileExist reportPath
        when exists' (removeFile reportPath)
        return ()
    knownPackages       <-  findKnownPackages collectorPath
    libDir <- getSysLibDir VERSION_ghc
    debugM "leksah-server" $ "collectSystem knownPackages= " ++ show knownPackages
    packageInfos        <-  concat <$> forM dbLists (\dbs -> inGhcIO libDir [] [] dbs $  \ _ -> map (,dbs) <$> getInstalledPackageInfos)
    debugM "leksah-server" $ "collectSystem packageInfos= " ++ show (map (packId . getThisPackage . fst) packageInfos)
    let newPackages = nub $ filter (\pi' -> not $ Set.member (packageIdentifierToString . packId . getThisPackage $ fst pi') knownPackages)
                            packageInfos
    if null newPackages
        then infoM "leksah-server" "Metadata collector has nothing to do"
        else do
            when findSources $ liftIO $ buildSourceForPackageDB prefs
            infoM "leksah-server" "update_toolbar 0.0"
            stats <- mapM (collectPackage writeAscii prefs (length newPackages))
                            (zip newPackages [1 .. length newPackages])
            writeStats stats
    infoM "leksah-server" "Metadata collection has finished"

writeStats :: [PackageCollectStats] -> IO ()
writeStats stats = do
    reportPath       <-  getConfigFilePathForSave "collectSystem.report"
    time             <-  getCurrentTime
    appendFile reportPath (report time)
    where
        report time = "\n++++++++++++++++++++++++++++++\n" ++ show time ++ "\n++++++++++++++++++++++++++++++\n"
                        ++ header' time ++ summary ++ T.unpack details
        header' _time = "\nLeksah system metadata collection "
        summary = "\nSuccess with         = " ++ T.unpack packs ++
                  "\nPackages total       = " ++ show packagesTotal ++
                  "\nPackages with source = " ++ show packagesWithSource ++
                  "\nPackages retrieved   = " ++ show packagesRetreived ++
                  "\nModules total        = " ++ show modulesTotal' ++
                  "\nModules with source  = " ++ show modulesWithSource ++
                  "\nPercentage source    = " ++ show percentageWithSource
        packagesTotal        = length stats
        packagesWithSource   = length (filter withSource stats)
        packagesRetreived    = length (filter retrieved stats)
        modulesTotal'        = sum (mapMaybe modulesTotal stats)
        modulesWithSource    = sum (mapMaybe modulesTotal (filter withSource stats))
        percentageWithSource = fromIntegral modulesWithSource * 100.0 / fromIntegral modulesTotal'
        details              = foldr detail "" (filter (isJust . mbError) stats)
        detail stat string   = string <> "\n" <> packageString stat <> " " <> (T.strip . fromJust $ mbError stat)
        packs                = foldr (\stat string -> string <> packageString stat <> " ")
                                        "" (take 10 (filter withSource stats))
                                        <> if packagesWithSource > 10 then "..." else ""


