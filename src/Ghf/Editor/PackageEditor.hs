--
-- | Module for editing of cabal packages
-- 

module Ghf.Editor.PackageEditor (
    packageNew
) where

import Graphics.UI.Gtk
import System.Directory
import Control.Monad.Reader
import Distribution.PackageDescription
import Distribution.Package
import Distribution.License
import Data.IORef
import Data.List(unzip4)
import Data.Version
import Distribution.Compiler
import Distribution.Version
import qualified Data.Map as Map
import Data.Map (Map,(!))
import Text.ParserCombinators.ReadP(readP_to_S)

import Ghf.Core
import GUI.PropertyEditor
import GUI.ViewFrame

standardSetup = "#!/usr/bin/runhaskell \n\
\> module Main where\n\
\> import Distribution.Simple\n\
\> main :: IO ()\n\
\> main = defaultMain\n\n"

packageNew :: GhfAction
packageNew = do
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

type PDescr = [(String,[FieldDescription PackageDescription])]

packageDescription :: PDescr
packageDescription = [
    ("Basic", [
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
            (\a -> return ())
    ,   field "Author" "" 
            emptyPrinter
            emptyParser
            author
            (\ a b -> b{author = a})
            stringEditor
            (\a -> return ())
    ,   field "Synopsis" "A one-line summary of this package" 
            emptyPrinter
            emptyParser
            synopsis
            (\ a b -> b{synopsis = a})
            stringEditor
            (\a -> return ())
    ]),
    ("Specification",[
        field "Build Dependencies" 
            "If this package depends on a specific version of Cabal, give that here. components" 
            emptyPrinter
            emptyParser
            descCabalVersion
            (\ a b -> b{descCabalVersion = a})
            (multisetEditor dependencyEditor "Dependency")
            (\a -> return ())
    ,   field "Cabal version" "" 
            emptyPrinter
            emptyParser
            buildDepends
            (\ a b -> b{buildDepends = a})
            versionRangeEditor
            (\a -> return ())
    ]),        
    ("Library",
       field "Library" "" 
            emptyPrinter
            emptyParser
            library
            (\ a b -> b{library = a})
            (maybeEditor libraryEditor True "Is this package a library?" "")
            (\a -> return ())
    ]),        
    
    ("Additional",[
        field "License File" ""
            emptyPrinter
            emptyParser
            licenseFile
            (\ a b -> b{licenseFile = a})
            (fileEditor)   
            (\a -> return ())
    ,   field "Copyright" "" 
            emptyPrinter
            emptyParser
            copyright
            (\ a b -> b{copyright = a})
            stringEditor
            (\a -> return ())
    ,   field "Maintainer" "" 
            emptyPrinter
            emptyParser
            maintainer
            (\ a b -> b{maintainer = a})
            stringEditor
            (\a -> return ())
    ,   field "Stability" "" 
            emptyPrinter
            emptyParser
            stability
            (\ a b -> b{stability = a})
            stringEditor
            (\a -> return ())
    ,   field "Tested with" "" 
            emptyPrinter
            emptyParser
            (\a -> case testedWith a of
                        []          -> (GHC,AnyVersion)
                        (p:r)       -> p)  
            (\ a b -> b{testedWith = [a]})
            testedWidthEditor
            (\a -> return ())
    ,   field "Homepage" "" 
            emptyPrinter
            emptyParser
            homepage
            (\ a b -> b{homepage = a})
            stringEditor
            (\a -> return ())
    ,   field "Package Url" "" 
            emptyPrinter
            emptyParser
            pkgUrl
            (\ a b -> b{pkgUrl = a})
            stringEditor
            (\a -> return ())
    ,   field "Description" "A more verbose description of this package" 
            emptyPrinter
            emptyParser
            description
            (\ a b -> b{description = a})
            multilineStringEditor
            (\a -> return ())
    ,   field "Category" "" 
            emptyPrinter
            emptyParser
            category
            (\ a b -> b{category = a})
            stringEditor
            (\a -> return ())
    ])]

editPackage :: PackageDescription -> String -> GhfAction
editPackage package packageDir = do
    ghfR <- ask
    res <- lift $editPackage' packageDir package packageDescription ghfR 
    lift $putStrLn $show res

editPackage' :: String -> PackageDescription -> PDescr -> GhfRef -> IO ()
editPackage' packageDir prefs prefsDesc ghfR   = 
    let flatPrefsDescr = concatMap snd prefsDesc in do
        lastAppliedPrefsRef <- newIORef prefs
        dialog  <- windowNew
        vb      <- vBoxNew False 0
        bb      <- hButtonBoxNew
        restore <- buttonNewFromStock "Restore"
        ok      <- buttonNewFromStock "gtk-ok"
        cancel  <- buttonNewFromStock "gtk-cancel"
        boxPackStart bb restore PackNatural 0
        boxPackStart bb ok PackNatural 0
        boxPackStart bb cancel PackNatural 0
        nb <- newNotebook
        notebookSetTabPos nb PosLeft
        res <- mapM (\ (tabLabel, partPrefsDescr) -> do
            resList <- mapM (\ (FD _ _ _ _ editorF _) -> editorF prefs) partPrefsDescr
            let (widgetsP, setInjsP, getExtsP,notifiersP) = unzip4 resList
            nbbox <- vBoxNew False 0
            mapM_ (\ w -> boxPackStart nbbox w PackNatural 0) widgetsP
            notebookAppendPage nb nbbox tabLabel
            return (widgetsP, setInjsP, getExtsP, notifiersP)) prefsDesc
        let (widgets, setInjs, getExts, notifiers) = 
                foldl (\ (w,i,e,n) (w2,i2,e2,n2) -> (w ++ w2, i ++ i2, e ++ e2, n ++ n2)) ([],[],[],[]) res
        ok `onClicked` (do
            newPrefs <- foldM (\ a b -> b a) prefs getExts
            lastAppliedPrefs <- readIORef lastAppliedPrefsRef
            mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF newPrefs lastAppliedPrefs) ghfR) flatPrefsDescr
            let PackageIdentifier n v =  package newPrefs
            writePackageDescription (packageDir ++ "/" ++ n ++ ".cabal") newPrefs
            --runReaderT (modifyGhf_ (\ghf -> return (ghf{prefs = newPrefs}))) ghfR
            widgetDestroy dialog)
        restore `onClicked` (do
            lastAppliedPrefs <- readIORef lastAppliedPrefsRef
            mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF prefs lastAppliedPrefs) ghfR) flatPrefsDescr
            mapM_ (\ setInj -> setInj prefs) setInjs
            writeIORef lastAppliedPrefsRef prefs)
        cancel `onClicked` (do
            lastAppliedPrefs <- readIORef lastAppliedPrefsRef
            mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF prefs lastAppliedPrefs) ghfR) flatPrefsDescr
            widgetDestroy dialog)
        boxPackStart vb nb PackNatural 0
        boxPackStart vb bb PackNatural 0
        containerAdd dialog vb
        widgetShowAll dialog    
        return ()

packageEditor :: Editor PackageIdentifier
packageEditor name = do
    (wid,inj,ext,notif) <- pairEditor (stringEditor) (versionEditor) "Name" "Version" name
    let pinj (PackageIdentifier n v) = inj (n,v)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (n,v) -> return (Just $PackageIdentifier n v)
    return (wid,pinj,pext,notif)   

compilerFlavorEditor :: Editor CompilerFlavor
compilerFlavorEditor name = do
    (wid,inj,ext,notif) <- eitherOrEditor (selectionEditor flavors) (stringEditor) 
                                "Known compilers" "" "Other" name
    let cfinj (OtherCompiler str) = inj (Right "")
    let cfinj other = inj (Left other)    
    let cfext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (Right s) -> return (Just $OtherCompiler s)
            Just (Left other) -> return (Just other)
    return (wid,cfinj,cfext,notif)
        where 
        flavors = [GHC, NHC, Hugs, HBC, Helium, JHC]

testedWidthEditor :: Editor (CompilerFlavor, VersionRange)
testedWidthEditor name = do
    pairEditor (compilerFlavorEditor) (versionRangeEditor) "Compiler Flavor" "Version Range" name

versionRangeEditor :: Editor VersionRange
versionRangeEditor name = do
    (wid,inj,ext,notif) <- 
        maybeEditor (eitherOrEditor             
            (pairEditor (selectionEditor v1) 
                        versionEditor "" "")
            (pairEditor (selectionEditor v2)
                        (pairEditor versionRangeStringEditor versionRangeStringEditor "" "")
                "" "")
                    "Simple" "Simple Version Range" "Complex Version Range") 
                        False "Any Version" "" name
    let vrinj AnyVersion                =   inj Nothing
        vrinj (ThisVersion v)           =   inj (Just (Left (ThisVersionS,v))) 
        vrinj (LaterVersion v)          =   inj (Just (Left (LaterVersionS,v)))
        vrinj (EarlierVersion v)        =   inj (Just (Left (EarlierVersionS,v)))
        vrinj (UnionVersionRanges v1 v2)=  inj (Just (Right (UnionVersionRangesS,(v1,v2))))    
        vrinj (IntersectVersionRanges v1 v2) 
                                        =    inj (Just (Right (IntersectVersionRangesS,(v1,v2))))
    let vrext = do  mvr <- ext
                    case mvr of
                        Nothing -> return (Just AnyVersion)
                        Just Nothing -> return (Just AnyVersion)
                        Just (Just (Left (ThisVersionS,v)))     -> return (Just (ThisVersion v))
                        Just (Just (Left (LaterVersionS,v)))    -> return (Just (LaterVersion v))
                        Just (Just (Left (EarlierVersionS,v)))   -> return (Just (EarlierVersion v))
                        Just (Just (Right (UnionVersionRangesS,(v1,v2)))) 
                                                        -> return (Just (UnionVersionRanges v1 v2))    
                        Just (Just (Right (IntersectVersionRangesS,(v1,v2)))) 
                                                        -> return (Just (IntersectVersionRanges v1 v2))    
    return (wid,vrinj,vrext,notif)
        where
            v1 = [ThisVersionS,LaterVersionS,EarlierVersionS]
            v2 = [UnionVersionRangesS,IntersectVersionRangesS]

data Version1 = ThisVersionS | LaterVersionS | EarlierVersionS
    deriving (Eq)
instance Show Version1 where
    show ThisVersionS   =  "ThisVersion"
    show LaterVersionS  =  "LaterVersion"
    show EarlierVersionS =  "EarlierVersion"

data Version2 = UnionVersionRangesS | IntersectVersionRangesS 
    deriving (Eq)
instance Show Version2 where
    show UnionVersionRangesS =  "UnionVersionRanges"
    show IntersectVersionRangesS =  "IntersectVersionRanges"

versionRangeStringEditor :: Editor VersionRange
versionRangeStringEditor name = do
    (wid,inj,ext,notif) <- stringEditor name
    let pinj v = inj (showVersionRange v)
    let pext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> do
                let l = (readP_to_S parseVersionRange) s
                if null l then
                    return Nothing
                    else return (Just (fst $head l))
    return (wid,pinj,pext,notif) 

dependencyEditor :: Editor Dependency
dependencyEditor name = do
    (wid,inj,ext,notif) <- pairEditor (stringEditor versionRangeEditor) "Package" "Version" name
    let pinj (Dependency s v) = inj (s,v)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just ("",v) -> return Nothing
            Just (s,v) -> return (Just $Dependency s v)
    return (wid,pinj,pext,notif)   

libraryEditor :: Editor Library
libraryEditor name = do
    (wid,inj,ext,notif) <- (pairEditor (multisetEditor fileEditor) buildInfoEditor) 
        "Exposed Modules" "BuildInfo" name
    let pinj (Library em bi) = inj (em,bi)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (em,bi) -> return (Just $Library em bi)
    return (wid,pinj,pext,notif)   

buildInfoEditor :: Editor Library
buildInfoEditor name = do
    (wid,inj,ext,notif) <- pairEditor (booleanEditor, "Buildable?") 
                           (pairEditor ((multisetEditor stringEditor), "Options for C compiler")
                           (pairEditor ((multisetEditor stringEditor), "Options for Linker")      
                           (pairEditor ((multisetEditor stringEditor), "support frameworks for Mac OS X")
                           (pairEditor ((multisetEditor fileEditor),   "C Sources")
                           (pairEditor ((multisetEditor fileEditor),   
                                "where to look for the haskell module hierarchy")
                           (pairEditor ((multisetEditor fileEditor),   
                                "Other modules: non-exposed or non-main modules")
                           (pairEditor ((multisetEditor extensionEditor),
                                "Haskell extensions the source needs for compilation")
                           (pairEditor ((multisetEditor stringEditor),
                                "Extra Libraries: what libraries to link with when compiling a program that uses your package")
                           (pairEditor ((multisetEditor fileEditor),
                                "Extra Library directories")
                           (pairEditor ((multisetEditor fileEditor),
                                "Include directories: Directories to find the .h files")
                           (pairEditor ((multisetEditor fileEditor),
                                "Includes: The .h files to be found in includeDirs")
                           (pairEditor ((multisetEditor fileEditor),
                                "Install Includes:.h files to install with the package")
                           (pairEditor ((multisetEditor (pairEditor (compilerFlavorEditor, "Compiler Flavor") 
                                                                    ((multisetEditor stringEditor), "Options")))
                                        "Options")
                           ((multisetEditor stringEditor), "Ghc profiler options"))))))))))))))
    let pinj BI = inj (buildable bi,(ccptions bi,(ldOptions bi,(frameworks bi,(cSources bi,(hsSourceDirs bi,
                        (otherModules bi,(extensions bi,(extraLibs bi,(extraLibDirs bi,(includeDirs bi,
                            (includes bi,(installIncludes bi, (options bi,ghcProfOptions bi))))))))))))))
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (buildablebi,(ccptionsbi,(ldOptionsbi,(frameworksbi,(cSourcesbi,(hsSourceDirsbi,
                        (otherModulesbi,(extensionsbi,(extraLibsbi,(extraLibDirsbi,(includeDirsbi,
                            (includesbi,(installIncludesbi, (optionsbi, ghcProfOptionsbi)))))))))))))) 
                    -> return (Just $BuildInfo buildablebi ccptionsbi ldOptionsbi frameworksbi cSourcesbi
                                hsSourceDirsbi otherModulesbi extensionsbi extraLibsbi extraLibDirsbi
                                    includeDirsbi includesbi installIncludesbi optionsbi ghcProfOptionsbi)
    return (wid,pinj,pext,notif)   




T
                               
                            
                              




