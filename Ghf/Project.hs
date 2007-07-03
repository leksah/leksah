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
import Data.IORef
import Data.List(unzip4)


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
        Just dirName -> do
            lift $do
                putStrLn dirName
                b1 <- doesFileExist (dirName ++ "Setup.hs")
                b2 <- doesFileExist (dirName ++ "Setup.lhs")   
                if  b1 || b2  
                    then putStrLn "Setup.(l)hs already exist"
                    else writeFile (dirName ++ "/Setup.lhs") standardSetup  
            editPackage emptyPackageDescription dirName      
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


packageDescription :: [FieldDescription PackageDescription]
packageDescription = [
        field "Package Identifier" "" 
            emptyPrinter
            emptyParser
            package
            (\ a b -> b{package = a})
            packageEditor
            (\a -> return ())
    ,   field "License" ""
            emptyPrinter
            emptyParser
            license
            (\ a b -> b{license = a})
            (selectionEditor [GPL, LGPL, BSD3, BSD4, PublicDomain, AllRightsReserved, OtherLicense])   
            (\a -> return ()) ]

editPackage :: PackageDescription -> String -> GhfAction
editPackage package packageDir = do
    ghfR <- ask
    res <- lift $editPackage' packageDir package packageDescription ghfR 
    lift $putStrLn $show res

editPackage' :: String -> PackageDescription -> [FieldDescription PackageDescription] -> GhfRef -> IO ()
editPackage' packageDir prefs prefsDesc ghfR   = do
    lastAppliedPrefsRef <- newIORef prefs
    dialog  <- windowNew
    vb      <- vBoxNew False 12
    bb      <- hButtonBoxNew
    apply   <- buttonNewFromStock "gtk-apply"
    restore <- buttonNewFromStock "gtk-restore"
    ok      <- buttonNewFromStock "gtk-ok"
    cancel  <- buttonNewFromStock "gtk-cancel"
    boxPackStart bb apply PackNatural 0
    boxPackStart bb restore PackNatural 0
    boxPackStart bb ok PackNatural 0
    boxPackStart bb cancel PackNatural 0
    resList <- mapM (\ (FD _ _ _ _ editorF _) -> editorF prefs) prefsDesc
    let (widgets, setInjs, getExts,_) = unzip4 resList 
    mapM_ (\ sb -> boxPackStart vb sb PackNatural 12) widgets
    ok `onClicked` (do
        newPrefs <- foldM (\ a b -> b a) prefs getExts
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF newPrefs lastAppliedPrefs) ghfR) prefsDesc
        let PackageIdentifier n v =  package newPrefs
        writePackageDescription (packageDir ++ "/" ++ n ++ ".cabal") newPrefs
        --runReaderT (modifyGhf_ (\ghf -> return (ghf{prefs = newPrefs}))) ghfR
        widgetDestroy dialog)
    apply `onClicked` (do
        newPrefs <- foldM (\ prf getEx -> getEx prf) prefs getExts
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF newPrefs lastAppliedPrefs) ghfR) prefsDesc
        writeIORef lastAppliedPrefsRef newPrefs)
    restore `onClicked` (do
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF prefs lastAppliedPrefs) ghfR) prefsDesc
        mapM_ (\ setInj -> setInj prefs) setInjs
        writeIORef lastAppliedPrefsRef prefs)
    cancel `onClicked` (do
        lastAppliedPrefs <- readIORef lastAppliedPrefsRef
        mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF prefs lastAppliedPrefs) ghfR) prefsDesc
        widgetDestroy dialog)
    boxPackStart vb bb PackNatural 0
    containerAdd dialog vb
    widgetShowAll dialog    
    return ()

packageEditor :: Editor PackageIdentifier
packageEditor name = do
    (wid,inj,ext,notif) <- (pairEditor (stringEditor) (genericEditor) "Package Identifier")
    let pinj (PackageIdentifier n v) = inj (n,v)
    let pext = do
        (n,v) <- ext
        return (PackageIdentifier n v)
    return (wid,pinj,pext,notif)   
