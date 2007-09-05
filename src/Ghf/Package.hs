--
-- | The packages methods of ghf. 
-- 

module Ghf.Package (
    packageConfig
,   packageBuild
) where

import Ghf.Core
import Ghf.Editor.PackageEditor
import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Environment
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Configure
import Distribution.Setup 
import Distribution.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.PreProcess
import Distribution.Simple.Build
import Data.Maybe

{--
data GhfPackage     =   GhfPackage {
    packageId       ::  PackageIdentifier 
,   cabalFile       ::  FilePath
,   packageDescr    ::  Maybe PackageDescription
,   configFlags     ::  Maybe ConfigFlags
,   localBuildInfo  ::  Maybe LocalBuildInfo
,   buildFlags      ::  Maybe BuildFlags 
    } 
    deriving (Eq,Show)
--}

getActivePackage :: GhfM (Maybe GhfPackage)
getActivePackage = do
    active <- readGhf activePack
    case active of
        Just p -> return (Just p)
        Nothing -> selectActivePackage

selectActivePackage :: GhfM (Maybe GhfPackage)
selectActivePackage = do
    window  <- readGhf window  
    mbDirName <- lift $choosePackageFile window
    case mbDirName of
        Nothing -> return Nothing
        Just dir -> do
            let flags = emptyConfigFlags defaultProgramConfiguration
            packageD <- lift $readPackageDescription dir
            let pack = GhfPackage (package packageD) dir (Just packageD) Nothing Nothing Nothing
            modifyGhf_ (\ghf -> return (ghf{activePack = (Just pack)})) 
            return (Just pack)

getPackageDescription :: GhfPackage -> GhfM (PackageDescription,GhfPackage)
getPackageDescription package = 
    case packageDescr package of
        Nothing -> do                     
            pd <- lift $readPackageDescription (cabalFile package)
            return (pd,package{packageDescr = Just pd})
        Just pd -> return (pd,package)

getConfigFlags :: GhfPackage -> GhfM (ConfigFlags,GhfPackage)
getConfigFlags package = 
    case configFlags package of
        Nothing -> do                     
            let flags = emptyConfigFlags defaultProgramConfiguration
            return (flags,package{configFlags = Just flags})
        Just flags -> return (flags,package)

getLocalBuildInfo :: GhfPackage -> Bool -> GhfM (LocalBuildInfo,GhfPackage)
getLocalBuildInfo package force = 
    if not force && isJust (localBuildInfo package)
        then return (fromJust $localBuildInfo package,package)
        else do
            (packageDescription,p1) <- getPackageDescription package
            (configFlags,p2) <- getConfigFlags p1                
            buildInfo <- lift $configure packageDescription configFlags
            let pack = p2{localBuildInfo = Just buildInfo}
            return (buildInfo,pack)

getBuildFlags :: GhfPackage -> GhfM (BuildFlags,GhfPackage)
getBuildFlags package = 
    case buildFlags package of
        Nothing -> do                     
            let flags = BuildFlags 2
            return (flags,package{buildFlags = Just flags})
        Just flags -> return (flags,package)
 

packageConfig :: Bool -> GhfAction
packageConfig force = do
    mbPackage <- getActivePackage
    case mbPackage of
        Nothing -> return ()
        Just package -> do
            (_,pack) <- getLocalBuildInfo package force
            modifyGhf_ (\ghf -> return (ghf{activePack = (Just pack)})) 
            return ()

packageBuild :: Bool -> GhfM ()
packageBuild forceReconfig = do
    mbPackage <- getActivePackage
    case mbPackage of
        Nothing -> return ()
        Just package -> do
            (packageDescription,p1) <- getPackageDescription package
            (buildInfo,p2) <- getLocalBuildInfo p1 forceReconfig
            (buildFlags,p3) <- getBuildFlags p2  
            lift $build packageDescription buildInfo buildFlags knownSuffixHandlers
            modifyGhf_ (\ghf -> return (ghf{activePack = (Just p3)}))            
                

        