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
import Data.List(unzip4,filter)
import Data.Version
import Distribution.Compiler
import Distribution.Version
import qualified Data.Map as Map
import Data.Map (Map,(!))
import Text.ParserCombinators.ReadP(readP_to_S)
import Language.Haskell.Extension

import Ghf.Core
import Ghf.Editor.PropertyEditor hiding(synopsis)
import qualified Ghf.Editor.PropertyEditor as PE (synopsis)
import Ghf.GUI.ViewFrame
import Ghf.Editor.BuildInfoEditor

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
                setCurrentDirectory dirName
                b1 <- doesFileExist (dirName ++ "Setup.hs")
                b2 <- doesFileExist (dirName ++ "Setup.lhs")   
                if  b1 || b2  
                    then putStrLn "Setup.(l)hs already exist"
                    else writeFile (dirName ++ "/Setup.lhs") standardSetup  
            editPackage emptyPackageDescription dirName      
            return ()

type PDescr = [(String,[FieldDescriptionE PackageDescription])]

packageDD :: FilePath -> PDescr
packageDD fp = [
    ("Description", [
        mkFieldE (emptyParams
            {   paraName = Just "Package Identifier"}) 
            package
            (\ a b -> b{package = a})
            packageEditor
    ,   mkFieldE (emptyParams
            {   paraName    = Just "Synopsis"
            ,   PE.synopsis = Just"A one-line summary of this package"}) 
            synopsis
            (\ a b -> b{synopsis = a})
            stringEditor
    ,   mkFieldE (emptyParams
            {   paraName    = Just "Homepage"})  
            homepage
            (\ a b -> b{homepage = a})
            stringEditor
    ,   mkFieldE (emptyParams
            {   paraName    = Just "Category"})   
            category
            (\ a b -> b{category = a})
            stringEditor
    ,   mkFieldE (emptyParams
            {   paraName    = Just "Description"
            ,   PE.synopsis = Just "A more verbose description of this package"
            ,   shadow      = Just ShadowOut
            ,   minSize     = Just (-1,250)}) 
            description
            (\ a b -> if null a then b{description = " \n\n\n\n\n"} else  b{description = a})
            multilineStringEditor
    ]),
    ("Description -2-",[
        mkFieldE (emptyParams{paraName=Just "Author"}) 
            author
            (\ a b -> b{author = a})
            stringEditor
    ,   mkFieldE (emptyParams{paraName=Just "Maintainer"})
            maintainer
            (\ a b -> b{maintainer = a})
            stringEditor
    ,   mkFieldE (emptyParams{paraName=Just "Stability"}) 
            stability
            (\ a b -> b{stability = a})
            stringEditor
    ,   mkFieldE (emptyParams{paraName=Just "Copyright"}) 
            copyright
            (\ a b -> b{copyright = a})
            stringEditor
    ,   mkFieldE (emptyParams{paraName=Just "License"}) 
            license
            (\ a b -> b{license = a})
            (staticSelectionEditor [GPL, LGPL, BSD3, BSD4, PublicDomain, AllRightsReserved, OtherLicense])   
    ,   mkFieldE (emptyParams{paraName=Just "License File"})  
            licenseFile
            (\ a b -> b{licenseFile = a})
            (fileEditor (Just fp) FileChooserActionOpen)   
    ,   mkFieldE (emptyParams{paraName=Just "Package Url"})  
            pkgUrl
            (\ a b -> b{pkgUrl = a})
            stringEditor
    ]),
    ("Dependencies",[
        mkFieldE (emptyParams
        {   paraName    = Just "Build Dependencies"
        ,   PE.synopsis = Just "Does this package depends on other packages?"
        ,   direction   = Just Vertical}) 
            buildDepends
            (\ a b -> b{buildDepends = a})
            dependenciesEditor
    ]),
    ("Library",[
        mkFieldE (emptyParams
        {   paraName    = Just "Library"
        ,   PE.synopsis = Just "If the package contains a library, specify the exported modules here"
        ,   shadow      = Just ShadowIn
        ,   direction   = Just Vertical}) 
            library 
            (\ a b -> b{library  = a})
            (maybeEditor (libraryEditor (Just fp),emptyParams{paraName = Just "Specify exported modules"}) True
                "Does this package contain a library?") 
    ]),
    ("Executables",[
        mkFieldE (emptyParams
        {   paraName    = Just "Executables"
        ,   PE.synopsis = Just "Describe executable programs contained in the package"
        ,   direction   = Just Vertical}) 
            executables 
            (\ a b -> b{executables = a})
            (executablesEditor (Just fp))
    ]),
    ("Other Files",[
        mkFieldE (emptyParams
        {   paraName    = Just "Data Files"
        ,   PE.synopsis = Just "A list of files to be installed for run-time use by the package."
        ,   direction   = Just Vertical}) 
            dataFiles
            (\ a b -> b{dataFiles = a})
            (filesEditor (Just fp))
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Extra Source Files"
        ,   PE.synopsis = Just "A list of additional files to be included in source distributions."
        ,   direction   = Just Vertical}) 
            extraSrcFiles
            (\ a b -> b{extraSrcFiles = a})
            (filesEditor (Just fp))
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Extra Tmp Files"
        ,   PE.synopsis = Just "A list of additional files or directories to be removed by setup clean."
        ,   direction   = Just Vertical}) 
            extraTmpFiles
            (\ a b -> b{extraTmpFiles = a})
            (filesEditor (Just fp))
    ]),
    ("Rest",[
        mkFieldE (emptyParams
        {   paraName    = Just "Tested with compiler"
        ,   shadow      = Just ShadowIn
        ,   direction   = Just Vertical})  
            (\a -> case testedWith a of
                []          -> [(GHC,AnyVersion)]
                l           -> l)  
            (\ a b -> b{testedWith = a})
            testedWidthEditor
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Cabal version"
        ,   PE.synopsis = Just "Does this package depends on a specific version of Cabal?"
        ,   shadow      = Just ShadowIn}) 
            descCabalVersion
            (\ a b -> b{descCabalVersion = a})
            versionRangeEditor
    ])]     


editPackage :: PackageDescription -> FilePath -> GhfAction
editPackage packageD packageDir = do
    ghfR <- ask
    res <- lift $editPackage' packageDir packageD (packageDD packageDir) ghfR 
    lift $putStrLn $show res

editPackage' :: String -> PackageDescription -> PDescr -> GhfRef -> IO ()
editPackage' packageDir packageD packageDD ghfR   = 
    let flatPackageDesc = concatMap snd packageDD 
    in do
        lastAppliedPackageRef <- newIORef packageD
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
        res <- mapM 
            (\ (tabLabel, partPackageDesc) -> do
                resList <- mapM (\ (FDE _ editorF) -> editorF packageD) partPackageDesc
                let (widgetsP, setInjsP, getExtsP,notifiersP) = unzip4 resList
                nbbox <- vBoxNew False 0
                mapM_ (\ w -> boxPackStart nbbox w PackNatural 0) widgetsP
                sw <- scrolledWindowNew Nothing Nothing
                scrolledWindowAddWithViewport sw nbbox
                scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
                notebookAppendPage nb sw tabLabel
                return (widgetsP, setInjsP, getExtsP, notifiersP)) 
                    packageDD
        let (widgets, setInjs, getExts, notifiers) = 
                foldl (\ (w,i,e,n) (w2,i2,e2,n2) -> (w ++ w2, i ++ i2, e ++ e2, n ++ n2)) ([],[],[],[]) res
        ok `onClicked` (do
            mbNewPackage <- validate packageD getExts
            case mbNewPackage of 
                Nothing -> return ()
                Just newPackage -> do
                    lastAppliedPackage <- readIORef lastAppliedPackageRef
                    let PackageIdentifier n v =  package newPackage
                    writePackageDescription (packageDir ++ "/" ++ n ++ ".cabal") newPackage
                    widgetDestroy dialog)
        cancel `onClicked` (do
            widgetDestroy dialog)
        boxPackStart vb nb PackGrow 7
        boxPackEnd vb bb PackNatural 7
        containerAdd dialog vb
        widgetSetSizeRequest dialog 500 700
        widgetShowAll dialog    
        return ()

-- ------------------------------------------------------------
-- * Special Editors
-- ------------------------------------------------------------

packageEditor :: Editor PackageIdentifier
packageEditor para = do
    (wid,inj,ext,notif) <- pairEditor
        (stringEditor, emptyParams{paraName=Just "Name"}) 
        (versionEditor, emptyParams{paraName=Just "Version"}) 
        (para{direction = Just Horizontal,shadow   = Just ShadowIn})
    let pinj (PackageIdentifier n v) = inj (n,v)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (n,v) -> return (Just $PackageIdentifier n v)
    return (wid,pinj,pext,notif)   

testedWidthEditor :: Editor [(CompilerFlavor, VersionRange)]
testedWidthEditor para = do
    multisetEditor    
       (pairEditor 
            (compilerFlavorEditor, emptyParams{shadow = Just ShadowNone}) 
            (versionRangeEditor, emptyParams{shadow = Just ShadowNone}), 
            emptyParams{direction = Just Vertical}) 
       para

compilerFlavorEditor :: Editor CompilerFlavor
compilerFlavorEditor para = do
    (wid,inj,ext,notif) <- eitherOrEditor 
        (staticSelectionEditor flavors, emptyParams{paraName=Just"Select compiler"}) 
        (stringEditor, emptyParams{paraName=Just "Specify compiler"}) 
        "Other" 
        para{paraName = Just "Select"}
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

versionRangeEditor :: Editor VersionRange
versionRangeEditor para = do      
    (wid,inj,ext,notif) <- 
        maybeEditor 
            (eitherOrEditor             
                (pairEditor 
                    (staticSelectionEditor v1, emptyParams) 
                    (versionEditor,emptyParams{paraName = Just "Enter Version"}),
                    emptyParams{direction = Just Vertical,paraName= Just "Simple Version Range"})
                (pairEditor 
                    (staticSelectionEditor v2, emptyParams)
                    (pairEditor 
                        (versionRangeEditor, emptyParams{shadow = Just ShadowIn}) 
                        (versionRangeEditor, emptyParams{shadow = Just ShadowIn}), 
                        emptyParams{direction = Just Vertical}),
                            emptyParams{direction = Just Vertical, paraName= Just "Complex Version Range"})              
                "Complex",emptyParams{paraName= Just "Simple"}) False "Any Version" 
                    para{direction = Just Vertical}           
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
    show ThisVersionS   =  "This Version"
    show LaterVersionS  =  "Later Version"
    show EarlierVersionS =  "Earlier Version"

data Version2 = UnionVersionRangesS | IntersectVersionRangesS 
    deriving (Eq)
instance Show Version2 where
    show UnionVersionRangesS =  "Union Version Ranges"
    show IntersectVersionRangesS =  "Intersect Version Ranges"

dependencyEditor :: Editor Dependency
dependencyEditor para = do
    (wid,inj,ext,notif) <- pairEditor 
        (stringEditor,emptyParams {paraName = Just "Package Name"}) 
        (versionRangeEditor,emptyParams {paraName = Just "Version"}) 
        (para{direction = Just Vertical})
    let pinj (Dependency s v) = inj (s,v)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just ("",v) -> return Nothing
            Just (s,v) -> return (Just $Dependency s v)
    return (wid,pinj,pext,notif) 

dependenciesEditor :: Editor [Dependency]
dependenciesEditor p =  multisetEditor (dependencyEditor,emptyParams) p{shadow = Just ShadowIn}    

filesEditor :: Maybe FilePath -> Editor [FilePath]
filesEditor fp p =  multisetEditor (fileEditor fp FileChooserActionOpen,emptyParams) p{shadow = Just ShadowIn}    

libraryEditor :: Maybe FilePath -> Editor Library
libraryEditor fp para = do
    (wid,inj,ext,notif) <- 
        pairEditor
            (multisetEditor (fileEditor fp  FileChooserActionOpen,emptyParams), para{direction = Just Vertical})
            (buildInfoEditor fp, para{paraName = Just "Build Info"})
            para{direction = Just Vertical}
    let pinj (Library em bi) = inj (em,bi)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (em,bi) -> return (Just $Library em bi)
    return (wid,pinj,pext,notif)   

        
versionEditor :: Editor Version
versionEditor para = do
    (wid,inj,ext,notiRef) <- stringEditor para
    let pinj v = inj (showVersion v)
    let pext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> do
                let l = filter (\(h,t) -> null t) (readP_to_S parseVersion s)
                if null l then
                    return Nothing
                    else return (Just (fst $head l))
    let handler = (do 
        v <- ext
        case v of
            Just _ -> return ()
            Nothing -> do
                md <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                        $"Field " ++ (getParameter paraName para) ++ " has invalid value. Please correct"
                dialogRun md
                widgetDestroy md
                return ())
--    registerHandler notiRef handler "onFocusOut"
    return (wid, pinj, pext, notiRef)

executableEditor :: Maybe FilePath -> Editor Executable
executableEditor fp para = do
    (wid,inj,ext,notif) <- pairEditor 
        (pairEditor 
            (stringEditor,emptyParams {paraName = Just "Executable Name"}) 
            (fileEditor fp FileChooserActionOpen,emptyParams {paraName = Just "Main module"}), 
            (emptyParams{direction = Just Vertical}))
        (buildInfoEditor fp, emptyParams{paraName = Just "Build Info"})
        para
    let pinj (Executable s f bi) = inj ((s,f),bi)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just ((s,f),bi) -> return (Just $Executable s f bi)
    return (wid,pinj,pext,notif) 

executablesEditor :: Maybe FilePath -> Editor [Executable]
executablesEditor fp p = multisetEditor (executableEditor fp,emptyParams) p{shadow = Just ShadowIn}    

buildInfoEditor :: Maybe FilePath -> Editor BuildInfo
buildInfoEditor fp p = otherEditor (editBuildInfo fp) p 

-- ------------------------------------------------------------
-- * (Boring) default values
-- ------------------------------------------------------------

instance Default Version1
    where getDefault = ThisVersionS

instance Default Version2
    where getDefault = UnionVersionRangesS

instance Default Version
    where getDefault = let version = (let l = (readP_to_S parseVersion) "0" 
                                        in if null l 
                                            then error "verion parser failed"
                                            else fst $head l)
                        in version

instance Default VersionRange
    where getDefault = AnyVersion

instance Default CompilerFlavor
    where getDefault =  GHC

instance Default BuildInfo
    where getDefault =  emptyBuildInfo

instance Default Library 
    where getDefault =  Library [] getDefault

instance Default Dependency 
    where getDefault = Dependency getDefault getDefault

instance Default Executable 
    where getDefault = Executable getDefault getDefault getDefault

