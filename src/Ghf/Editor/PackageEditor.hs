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
import Ghf.Editor.PropertyEditor hiding(synopsis)
import qualified Ghf.Editor.PropertyEditor as PE (synopsis)
import Ghf.GUI.ViewFrame

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

type PDescr = [(String,[FieldDescriptionE PackageDescription])]

packageDescription :: PDescr
packageDescription = [
    ("Basic", [
        mkFieldE (emptyParams{paraName=Just "Package Identifier"}) 
            package
            (\ a b -> b{package = a})
            packageEditor
    ,   mkFieldE (emptyParams{paraName=Just "Synopsis",PE.synopsis= Just"A one-line summary of this package"}) 
            synopsis
            (\ a b -> b{synopsis = a})
            stringEditor
    ,   mkFieldE (emptyParams{paraName=Just "Description",PE.synopsis=Just "A more verbose description of this package"}) 
            description
            (\ a b -> if null a then b{description = " \n\n\n\n\n"} else  b{description = a})
            multilineStringEditor
    ,   mkFieldE (emptyParams{paraName=Just "Author"}) 
            author
            (\ a b -> b{author = a})
            stringEditor
    ,   mkFieldE (emptyParams{paraName=Just "Copyright"}) 
            copyright
            (\ a b -> b{copyright = a})
            stringEditor
    ]){--,
    ("Specification",[
        mkFieldE "Build Dependencies" 
            "If this package depends on a specific version of Cabal, give that here. components" 
            descCabalVersion
            (\ a b -> b{descCabalVersion = a})
            versionRangeEditor
    ,   mkFieldE "Cabal version" "" 
            buildDepends
            (\ a b -> b{buildDepends = a})
            (multisetEditor dependencyEditor "Dependency")
    ]),        
    ("Library",[
       mkFieldE "Library" "" 
            emptyPrinter
            emptyParser
            library
            (\ a b -> b{library = a})
            (maybeEditor libraryEditor True "Is this package a library?" "")
            (\a -> return ())
    ]),       
    
    ("Additional",[
        mkFieldE "License" ""
            license
            (\ a b -> b{license = a})
            (staticSelectionEditor [GPL, LGPL, BSD3, BSD4, PublicDomain, AllRightsReserved, OtherLicense])   
    ,   mkFieldE "License File" ""
            licenseFile
            (\ a b -> b{licenseFile = a})
            (fileEditor)   
    ,   mkFieldE "Maintainer" "" 
            maintainer
            (\ a b -> b{maintainer = a})
            stringEditor
    ,   mkFieldE "Stability" "" 
            stability
            (\ a b -> b{stability = a})
            stringEditor
    ,   mkFieldE "Tested with" "" 
            (\a -> case testedWith a of
                        []          -> (GHC,AnyVersion)
                        (p:r)       -> p)  
            (\ a b -> b{testedWith = [a]})
            testedWidthEditor
    ,   mkFieldE "Homepage" "" 
            homepage
            (\ a b -> b{homepage = a})
            stringEditor
    ,   mkFieldE "Package Url" "" 
            pkgUrl
            (\ a b -> b{pkgUrl = a})
            stringEditor
    ,   mkFieldE "Category" "" 
            category
            (\ a b -> b{category = a})
            stringEditor
    ])--}]

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
        vb      <- vBoxNew False 7
        bb      <- hButtonBoxNew
        restore <- buttonNewFromStock "Restore"
        ok      <- buttonNewFromStock "gtk-ok"
        cancel  <- buttonNewFromStock "gtk-cancel"
        boxPackStart bb restore PackNatural 0
        boxPackStart bb ok PackNatural 0
        boxPackStart bb cancel PackNatural 0
        nb <- newNotebook
        notebookSetTabPos nb PosTop
        res <- mapM (\ (tabLabel, partPrefsDescr) -> do
            resList <- mapM (\ (FDE _ editorF) -> editorF prefs) partPrefsDescr
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
--            mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF newPrefs lastAppliedPrefs) ghfR) flatPrefsDescr
            let PackageIdentifier n v =  package newPrefs
            writePackageDescription (packageDir ++ "/" ++ n ++ ".cabal") newPrefs
            --runReaderT (modifyGhf_ (\ghf -> return (ghf{prefs = newPrefs}))) ghfR
            widgetDestroy dialog)
{--        cancel `onClicked` (do
            lastAppliedPrefs <- readIORef lastAppliedPrefsRef
            mapM_ (\ (FD _ _ _ _ _ applyF) -> runReaderT (applyF prefs lastAppliedPrefs) ghfR) flatPrefsDescr
            widgetDestroy dialog)--}
        boxPackStart vb nb PackGrow 7
        boxPackEnd vb bb PackNatural 7
        containerAdd dialog vb
        widgetShowAll dialog    
        return ()

packageEditor :: Editor PackageIdentifier
packageEditor name = do
    (wid,inj,ext,notif) <- pairEditor (stringEditor, "Name") (versionEditor, "Version") Horizontal name
    let pinj (PackageIdentifier n v) = inj (n,v)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (n,v) -> return (Just $PackageIdentifier n v)
    return (wid,pinj,pext,notif)   

compilerFlavorEditor :: Editor CompilerFlavor
compilerFlavorEditor name = do
    (wid,inj,ext,notif) <- eitherOrEditor (staticSelectionEditor flavors,"Select compiler") 
                            (stringEditor, "Specify compiler") name
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
    pairEditor (compilerFlavorEditor, "Known Compiler?") (versionRangeEditor, "Version Range") Horizontal name

versionRangeEditor :: Editor VersionRange
versionRangeEditor name = do
    (wid,inj,ext,notif) <- 
        maybeEditor (eitherOrEditor             
            ((pairEditor (staticSelectionEditor v1, "") 
                        (versionEditor,"") Horizontal),"Simple Version Range")
            ((pairEditor (staticSelectionEditor v2, "")
                        ((pairEditor (versionRangeEditor,"") (versionRangeEditor, "") Vertical),
                            "") Vertical), "Complex Version Range")) 
                False "Any Version" "Simple Version Specification" name
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

instance Default (Either (Version1,Version) (Version2, (VersionRange, VersionRange))) 
    where 
        getDefault =  Left(getDefault) 

instance Default (Version1,Version)
    where getDefault = let version = (let l = (readP_to_S parseVersion) "0" 
                                        in if null l 
                                            then error "verion parser failed"
                                            else fst $head l)
                        in (ThisVersionS, version) 

instance Default VersionRange
    where getDefault = AnyVersion

instance Default (Version2, (VersionRange, VersionRange))
    where getDefault = (UnionVersionRangesS, (AnyVersion, AnyVersion))
 
instance Default String
    where 
        getDefault =    ""

instance Default Int
    where 
        getDefault =    1

instance Default CompilerFlavor
    where 
        getDefault =    GHC

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
    (wid,inj,ext,notif) <- pairEditor (stringEditor,"Package Name") (versionRangeEditor,"Version") Horizontal name
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
    (wid,inj,ext,notif) <- multisetEditor fileEditor "" name 
    let pinj (Library em _) = inj (em)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (em) -> return (Just $Library em emptyBuildInfo)
    return (wid,pinj,pext,notif)   

        
versionEditor :: Editor Version
versionEditor label = do
    (wid,inj,ext,notiRef) <- stringEditor label
    let pinj v = inj (showVersion v)
    let pext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> do
                let l = (readP_to_S parseVersion) s
                if null l then
                    return Nothing
                    else return (Just (fst $head l))
    let handler = (do 
        v <- ext
        case v of
            Just _ -> return ()
            Nothing -> do
                md <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                        $"Field " ++ label ++ " has invalid value. Please correct"
                dialogRun md
                widgetDestroy md
                return ())
--    registerHandler notiRef handler "onFocusOut"
    return (wid, pinj, pext, notiRef)


{--
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
--}




                               
                            
                              




