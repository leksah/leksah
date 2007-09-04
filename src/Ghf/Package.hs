--
-- | The packages methods of ghf. 
-- 

module Ghf.Package (
    packageConfig
) where

import Ghf.Core
import Ghf.Editor.PackageEditor


packageConfig :: GhfAction
packageConfig = do
    case mbPackage of
        Nothing -> return ()
        Just (GhfPackage cabalFile configFlags) -> lift $ do
            packageDescriotion <- readPackageDescription cabalFile                 
            buildInfo <- configure packageDescription configFlags

getActivePackage :: Ghf (Maybe GhfPackage)
getActivePackage = do
    active <- readGhf activePack
    case activePack of
        Just p -> return (Just p)
        Nothing -> selectActivePackage

selectActivePackage :: Ghf (Maybe GhfPackage)
selectActivePackage = do
    window  <- readGhf window  
    mbDirName <- lift $choosePackageFile window
    case mbDirName of
        Nothing -> Nothing
        Just dir -> do
            let package = GhfPackage dir []
            writeGhf activePack (Just package)
            return (Just package)


        