--
-- | Module for menus and toolbars
-- 

module Ghf.Project (
    projectNew
) where

import Graphics.UI.Gtk
import System.Directory
import Control.Monad.Reader

import Ghf.Core
import Distribution.PackageDescription

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
                                         

