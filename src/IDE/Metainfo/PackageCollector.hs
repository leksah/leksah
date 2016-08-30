{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.PackageCollector
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Metainfo.PackageCollector (

    collectPackage

) where

import Control.Applicative
import Prelude
import IDE.StrippedPrefs (RetrieveStrategy(..), Prefs(..))
import PackageConfig (PackageConfig)
import IDE.Metainfo.SourceCollectorH
       (findSourceForPackage, packageFromSource, PackageCollectStats(..))
import System.Log.Logger (errorM, debugM, infoM)
import IDE.Metainfo.InterfaceCollector (collectPackageFromHI)
import IDE.Core.CTypes
       (metadataVersion, PackageDescr(..), leksahVersion,
        packageIdentifierToString, getThisPackage, packId)
import IDE.Utils.FileUtils (getCollectorPath)
import System.Directory (doesDirectoryExist, setCurrentDirectory)
import IDE.Utils.Utils
       (leksahMetadataPathFileExtension,
        leksahMetadataSystemFileExtension)
import System.FilePath (dropFileName, takeBaseName, (<.>), (</>))
import Data.Binary.Shared (encodeFileSer)
import Distribution.Text (display)
import Control.Monad.IO.Class (MonadIO, MonadIO(..))
import qualified Control.Exception as E (SomeException, catch)
import IDE.Utils.Tool (runTool')
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack, pack)
import Data.Text (Text)
import Network.HTTP.Proxy (Proxy(..), fetchProxy)
import Network.Browser
       (request, setAuthorityGen, setOutHandler, setErrHandler, setProxy,
        browse)
import Data.Char (isSpace)
import Network.URI (parseURI)
import Network.HTTP (rspBody, rspCode, Header(..), Request(..))
import Network.HTTP.Base (RequestMethod(..))
import Network.HTTP.Headers (HeaderName(..))
import qualified Data.ByteString as BS (writeFile, empty)
import qualified Paths_leksah (version)
import Distribution.System (buildArch, buildOS)
import Control.Monad (unless)

collectPackage :: Bool -> Prefs -> Int -> ((PackageConfig, [FilePath]), Int) -> IO PackageCollectStats
collectPackage writeAscii prefs numPackages ((packageConfig, dbs), packageIndex) = do
    infoM "leksah-server" ("update_toolbar " ++ show
        ((fromIntegral packageIndex / fromIntegral numPackages) :: Double))
    eitherStrFp    <- findSourceForPackage prefs pid
    case eitherStrFp of
        Left message -> do
            debugM "leksah-server" . T.unpack $ message <> " : " <> packageName
            packageDescrHi <- collectPackageFromHI packageConfig dbs
            writeExtractedPackage False packageDescrHi
            return stat {packageString = message, modulesTotal = Just (length (pdModules packageDescrHi))}
        Right fpSource ->
            case retrieveStrategy prefs of
                RetrieveThenBuild ->
                    retrieve fpSource >>= \case
                        Just stats -> return stats
                        Nothing -> buildOnly fpSource
                BuildThenRetrieve -> do
                    debugM "leksah-server" $ "Build (then retrieve) " <> T.unpack packageName <> " in " <> fpSource
                    build fpSource >>= \case
                        (True, bstat) -> return bstat
                        (False, bstat) ->
                            retrieve fpSource >>= \case
                                Just stats -> return stats
                                Nothing -> do
                                    packageDescrHi <- collectPackageFromHI packageConfig dbs
                                    writeExtractedPackage False packageDescrHi
                                    return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}
                NeverRetrieve -> do
                    debugM "leksah-server" $ "Build " <> T.unpack packageName <> " in " <> fpSource
                    buildOnly fpSource
    where
        pid = packId $ getThisPackage packageConfig
        packageName = packageIdentifierToString pid
        stat = PackageCollectStats packageName Nothing False False Nothing
        retrieve :: FilePath -> IO (Maybe PackageCollectStats)
        retrieve fpSource = do
            collectorPath   <- liftIO getCollectorPath
            setCurrentDirectory collectorPath
            let fullUrl  = T.unpack (retrieveURL prefs) <> "/metadata-" <> leksahVersion <> "/" <> T.unpack packageName <> leksahMetadataSystemFileExtension
                filePath = collectorPath </> T.unpack packageName <.> leksahMetadataSystemFileExtension

            case parseURI fullUrl of
                Nothing -> do
                    errorM "leksah-server" $ "collectPackage: invalid URI = " <> fullUrl
                    return Nothing
                Just uri -> do
                    debugM "leksah-server" $ "collectPackage: before retreiving = " <> fullUrl
                    proxy <- filterEmptyProxy . trimProxyUri <$> fetchProxy True
                    (_, rsp) <- browse $ do
                        setProxy proxy
                        setErrHandler (errorM "leksah-server")
                        setOutHandler (debugM "leksah-server")
                        setAuthorityGen (\_ _ -> return Nothing)
                        request Request{ rqURI      = uri
                                        , rqMethod  = GET
                                        , rqHeaders = [Header HdrUserAgent userAgent]
                                        , rqBody    = BS.empty }
                    if rspCode rsp == (2,0,0)
                        then do
                            BS.writeFile filePath $ rspBody rsp
                            debugM "leksah-server" . T.unpack $ "collectPackage: retreived = " <> packageName
                            liftIO $ writePackagePath (dropFileName fpSource) packageName
                            return (Just stat {withSource=True, retrieved= True, mbError=Nothing})
                        else do
                            debugM "leksah-server" . T.unpack $ "collectPackage: Can't retreive = " <> packageName
                            return Nothing

        build :: FilePath -> IO (Bool, PackageCollectStats)
        build fpSource = do
            runCabalConfigure fpSource
            mbPackageDescrPair <- packageFromSource fpSource packageConfig
            case mbPackageDescrPair of
                (Just packageDescrS, bstat) -> do
                    writePackageDesc packageDescrS fpSource
                    return (True, bstat{modulesTotal = Just (length (pdModules packageDescrS))})
                (Nothing, bstat) -> return (False, bstat)

        buildOnly :: FilePath -> IO PackageCollectStats
        buildOnly fpSource =
            build fpSource >>= \case
                (True, bstat) -> return bstat
                (False, bstat) -> do
                    packageDescrHi <- collectPackageFromHI packageConfig dbs
                    writeExtractedPackage False packageDescrHi
                    return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}

        trimProxyUri (Proxy uri auth) = Proxy (trim uri) auth
        trimProxyUri p = p
        filterEmptyProxy (Proxy "" _) = NoProxy
        filterEmptyProxy p = p
        trim = f . f where f = reverse . dropWhile isSpace
        userAgent = concat [ "leksah-server/", display Paths_leksah.version
                           , " (", display buildOS, "; ", display buildArch, ")"
                           ]
        writePackageDesc packageDescr fpSource = do
            liftIO $ writeExtractedPackage writeAscii packageDescr
            liftIO $ writePackagePath (dropFileName fpSource) packageName
        runCabalConfigure fpSource = do
            let dirPath         = dropFileName fpSource
                packageName'    = takeBaseName fpSource
                flagsFor "base" = ["-finteger-gmp", "-finteger-gmp2"]
                flagsFor _      = []
                flags           = flagsFor packageName'
            distExists <- doesDirectoryExist $ dirPath </> "dist"
            unless distExists $ do
                setCurrentDirectory dirPath
                E.catch (do runTool' "cabal" ["clean"] Nothing Nothing
                            debugM "leksah" $ "fpSource = " <> show fpSource
                            runTool' "cabal" ("configure":flags ++ map (("--package-db"<>) .T.pack) dbs) Nothing Nothing
                            return ())
                        (\ (_e :: E.SomeException) -> do
                            debugM "leksah-server" "Can't configure"
                            return ())

writeExtractedPackage :: MonadIO m => Bool -> PackageDescr -> m ()
writeExtractedPackage writeAscii pd = do
    collectorPath   <- liftIO getCollectorPath
    let filePath    =  collectorPath </> T.unpack (packageIdentifierToString $ pdPackage pd) <.>
                            leksahMetadataSystemFileExtension
    if writeAscii
        then liftIO $ writeFile (filePath ++ "dpg") (show pd)
        else liftIO $ encodeFileSer filePath (metadataVersion, pd)

writePackagePath :: MonadIO m => FilePath -> Text -> m ()
writePackagePath fp packageName = do
    collectorPath   <- liftIO getCollectorPath
    let filePath    =  collectorPath </> T.unpack packageName <.> leksahMetadataPathFileExtension
    liftIO $ writeFile filePath fp

