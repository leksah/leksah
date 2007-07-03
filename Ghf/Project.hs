--
-- | Module for menus and toolbars
-- 

module Ghf.Project (
    projectNew
) where

import Graphics.UI.Gtk
import System.Directory
import Control.Monad.Reader
import Distribution.PackageDescription
import Distribution.Package
import Distribution.License

import Ghf.Core
import Ghf.PreferencesBase

standardSetup = "#!/usr/bin/runhaskell \n\
\> module Main where\n\
\> import Distribution.Simple\n\
\> main :: IO ()\n\
\> main = defaultMain\n\n"


projectNew :: GhfAction
projectNew = do
    window  <- readGhf window  
    mbDirName <- lift $ do     
        dialog <- fileChooserDialogNew
                        (Just $ "Select root folder for project")             
                        (Just window)                   
                    FileChooserActionSelectFolder              
                    [("gtk-cancel"                       
                    ,ResponseCancel)
                    ,("gtk-open"                                  
                    ,ResponseAccept)]
        widgetShow dialog
        response <- dialogRun dialog
        case response of
            ResponseAccept -> do                
                fn <- fileChooserGetFilename dialog
                widgetDestroy dialog
                return fn
            ResponseCancel -> do        
                widgetDestroy dialog
                return Nothing
            ResponseDeleteEvent -> do   
                widgetDestroy dialog                
                return Nothing
    case mbDirName of
        Nothing -> return ()
        Just dirName -> lift $do
            putStrLn dirName
            b1 <- doesFileExist (dirName ++ "Setup.hs")
            b2 <- doesFileExist (dirName ++ "Setup.lhs")   
            if  b1 || b2  
                then putStrLn "Setup.(l)hs already exist"
                else writeFile (dirName ++ "/Setup.lhs") standardSetup         
            return ()
                                         
{--
   data PackageDescription = PackageDescription {
   package :: PackageIdentifier
   license :: License
licenseFile :: FilePath
copyright :: String
maintainer :: String
author :: String
stability :: String
testedWith :: [(CompilerFlavor, VersionRange)]
homepage :: String
pkgUrl :: String
synopsis :: String
description :: String
category :: String
buildDepends :: [Dependency]
descCabalVersion :: VersionRange
library :: (Maybe Library)
executables :: [Executable]
dataFiles :: [FilePath]
extraSrcFiles :: [FilePath]
extraTmpFiles :: [FilePath]
}
--}

editPackage :: PackageDescription -> String -> IO ()
editPackage package packageDir = do
    ghfR <- ask
    res <- editPrefs' package packageDescription ghfR (writePackageDescription (packageDir ++ "cabal.cabal"))
    lift $putStrLn $show res


packageDescription :: [FieldDescription PackageDescription]
packageDescription = [
        field "Package Identifier" "" 
            emptyPrinter
            emptyParser
            package
            (\ (n,v) b -> b{package = PackageIdentifier n v})
            (\ (PackageIdentifier n v) -> (pairEditor (stringEditor "Name") 
                                            (genericEditor "Version") "Package Identifier") (n,v))
            (\a -> return ())
    ,   field "License"
            emptyPrinter
            emptyParser
            license
            (\ a b -> b{license = a})
            selectionEditor [GPL, LGPL, BSD3, BSD4, PublicDomain, AllRightsReserved, OtherLicense]    
            (\a -> return ()) ]
