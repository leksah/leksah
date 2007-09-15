--
-- | Module for editing of cabal packages
--

module Ghf.PackageEditor (
    packageNew
,   packageEdit
,   choosePackageDir
,   choosePackageFile
) where

import Data.Version()    -- Instances only
import Graphics.UI.Gtk(fileChooserDialogNew, Window, widgetSetSizeRequest,
		       widgetDestroy, widgetShowAll, widgetShow, containerAdd, boxPackEnd,
		       boxPackStart, scrolledWindowAddWithViewport, scrolledWindowSetPolicy,
		       scrolledWindowNew, notebookSetTabPos, notebookAppendPage, hButtonBoxNew,
		       vBoxNew,
		       FileChooserAction(FileChooserActionSelectFolder, FileChooserActionOpen),
		       fileChooserGetFilename, onClicked, buttonNewFromStock, messageDialogNew,
		       windowNew, dialogRun, MessageType(MessageWarning), ButtonsType(ButtonsClose),
		       Packing(PackNatural, PackGrow), PositionType(PosTop),
		       ShadowType(ShadowOut, ShadowIn), PolicyType(PolicyAutomatic),
		       ResponseId(ResponseDeleteEvent, ResponseCancel, ResponseAccept))
import Graphics.UI.Gtk.ModelView()    -- Instances only
import Control.Monad.Reader(Monad(return), MonadReader(ask), MonadTrans(..),
			    mapM_, mapM)
import Distribution.Compiler(CompilerFlavor(GHC))
import Distribution.License(License(..))
import Distribution.Package(PackageIdentifier(PackageIdentifier))
import Distribution.PackageDescription(PackageDescription(testedWith,
							  licenseFile, library, extraTmpFiles, extraSrcFiles,
							  executables, descCabalVersion, dataFiles, buildDepends,
							  synopsis, category, description, pkgUrl, homepage, stability,
							  author, maintainer, copyright, license, package),
				       emptyPackageDescription, readPackageDescription, writePackageDescription)
import Distribution.Version(VersionRange(AnyVersion))
import Language.Haskell.Extension()    -- Instances only
import System.FilePath((</>), FilePath)
import Data.IORef(readIORef, newIORef)
import Data.List(unzip4)
import Data.Maybe(Maybe(..), isJust, isNothing, fromJust)
import Data.Map()    -- Instances only
import System.Directory(doesFileExist)
import Text.ParserCombinators.ReadP()    -- Instances only
import Ghf.Core(Direction(Vertical), GhfAction, GhfRef, Ghf(window), readGhf)
import Ghf.File(allModules, cabalFileName)
import Ghf.SpecialEditors(packageEditor, testedWidthEditor,
				 versionRangeEditor, dependenciesEditor, filesEditor)
import Ghf.ViewFrame(newNotebook)
import Ghf.PropertyEditor(Parameters(shadow, paraName, synopsisP, minSize,
					    direction),
				 FieldDescriptionE(parameters, FDE), emptyParams, extractAndValidate, mkFieldE,
				 stringEditor, multilineStringEditor, staticSelectionEditor, fileEditor,
				 maybeEditor)
import Ghf.BuildInfoEditor(libraryEditor, executablesEditor)


standardSetup = "#!/usr/bin/runhaskell \n\
\> module Main where\n\
\> import Distribution.Simple\n\
\> main :: IO ()\n\
\> main = defaultMain\n\n"

packageNew :: GhfAction
packageNew = packageNewOrEdit True Nothing

packageEdit :: Maybe FilePath -> GhfAction
packageEdit = packageNewOrEdit False

choosePackageDir :: Window -> IO (Maybe FilePath)
choosePackageDir window = do
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

choosePackageFile :: Window -> IO (Maybe FilePath)
choosePackageFile window = do
    dialog <- fileChooserDialogNew
                    (Just $ "Select file of project")
                    (Just window)
                FileChooserActionOpen
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

packageNewOrEdit :: Bool -> Maybe FilePath -> GhfAction
packageNewOrEdit isNew mbPath = do
    window  <- readGhf window
    mbDirName <- lift $choosePackageDir window
    case mbDirName of
        Nothing -> return ()
        Just dirName -> do
                cfn <- lift $cabalFileName dirName
                if (not isNew) && isNothing cfn
                    then lift $do
                        md <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                            $ "There is no unique .cabal file in this directory."
                        dialogRun md
                        widgetDestroy md
                    else if isNew && isJust cfn
                            then lift $do
                                md <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                                    $ "There is already a .cabal file in this directory."
                                dialogRun md
                                widgetDestroy md
                            else do
                                modules <- lift $do
                                    b1 <- doesFileExist (dirName </> "Setup.hs")
                                    b2 <- doesFileExist (dirName </> "Setup.lhs")
                                    if  not (b1 || b2)
                                        then do
                                            putStrLn "Setup.(l)hs does not exist. Writing Standard"
                                            writeFile (dirName </> "Setup.lhs") standardSetup
                                        else putStrLn "Setup.(l)hs already exist"
                                    allModules dirName
                                lift $putStrLn "after finding modules"
                                package <- lift $if isNew
                                    then return emptyPackageDescription
                                    else readPackageDescription (dirName </> fromJust cfn)
                                editPackage package dirName modules
                return ()

type PDescr = [(String,[FieldDescriptionE PackageDescription])]

packageDD :: FilePath -> [String] -> PDescr
packageDD fp modules = [
    ("Description -1-", [
        mkFieldE (emptyParams
            {   paraName = Just "Package Identifier"})
            package
            (\ a b -> b{package = a})
            packageEditor
    ,   mkFieldE (emptyParams
            {   paraName    = Just "Cabal version"
            ,   synopsisP = Just "Does this package depends on a specific version of Cabal?"
            ,   shadow      = Just ShadowIn})
            descCabalVersion
            (\ a b -> b{descCabalVersion = a})
            versionRangeEditor
    ,   mkFieldE (emptyParams{paraName=Just "License"})
            license
            (\ a b -> b{license = a})
            (staticSelectionEditor [GPL, LGPL, BSD3, BSD4, PublicDomain, AllRightsReserved, OtherLicense])
    ,   mkFieldE (emptyParams{paraName=Just "License File"})
            licenseFile
            (\ a b -> b{licenseFile = a})
            (fileEditor (Just fp) FileChooserActionOpen "Select file")
    ,   mkFieldE (emptyParams{paraName=Just "Copyright"})
            copyright
            (\ a b -> b{copyright = a})
            stringEditor
    ,   mkFieldE (emptyParams{paraName=Just "Author"})
            author
            (\ a b -> b{author = a})
            stringEditor
    ,   mkFieldE (emptyParams{paraName=Just "Maintainer"})
            maintainer
            (\ a b -> b{maintainer = a})
            stringEditor
    ]),
    ("Description -2-",[
        mkFieldE (emptyParams{paraName=Just "Stability"})
            stability
            (\ a b -> b{stability = a})
            stringEditor
    ,   mkFieldE (emptyParams
            {   paraName    = Just "Homepage"})
            homepage
            (\ a b -> b{homepage = a})
            stringEditor
    ,   mkFieldE (emptyParams{paraName=Just "Package Url"})
            pkgUrl
            (\ a b -> b{pkgUrl = a})
            stringEditor
    ,   mkFieldE (emptyParams
            {   paraName    = Just "Synopsis"
            ,   synopsisP   = Just"A one-line summary of this package"})
            synopsis
            (\ a b -> b{synopsis = a})
            stringEditor
    ,   mkFieldE (emptyParams
            {   paraName    = Just "Description"
            ,   synopsisP   = Just "A more verbose description of this package"
            ,   shadow      = Just ShadowOut
            ,   minSize     = Just (-1,250)})
            description
            (\ a b -> if null a then b{description = " \n\n\n\n\n"} else  b{description = a})
            multilineStringEditor
    ,   mkFieldE (emptyParams
            {   paraName    = Just "Category"})
            category
            (\ a b -> b{category = a})
            stringEditor
    ]),
    ("Tested With",[
        mkFieldE (emptyParams
        {   paraName    = Just "Tested with compiler"
        ,   shadow      = Just ShadowIn
        ,   direction   = Just Vertical})
            (\a -> case testedWith a of
                []          -> [(GHC,AnyVersion)]
                l           -> l)
            (\ a b -> b{testedWith = a})
            testedWidthEditor
    ]),
    ("Dependencies",[
        mkFieldE (emptyParams
        {   paraName    = Just "Build Dependencies"
        ,   synopsisP   = Just "Does this package depends on other packages?"
        ,   direction   = Just Vertical})
            buildDepends
            (\ a b -> b{buildDepends = a})
            dependenciesEditor
    ]),
    ("Other Files",[
        mkFieldE (emptyParams
        {   paraName    = Just "Data Files"
        ,   synopsisP   = Just "A list of files to be installed for run-time use by the package."
        ,   direction   = Just Vertical})
            dataFiles
            (\ a b -> b{dataFiles = a})
            (filesEditor (Just fp) FileChooserActionOpen "Select File")
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Extra Source Files"
        ,   synopsisP   = Just "A list of additional files to be included in source distributions."
        ,   direction   = Just Vertical})
            extraSrcFiles
            (\ a b -> b{extraSrcFiles = a})
            (filesEditor (Just fp) FileChooserActionOpen "Select File")
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Extra Tmp Files"
        ,   synopsisP   = Just "A list of additional files or directories to be removed by setup clean."
        ,   direction   = Just Vertical})
            extraTmpFiles
            (\ a b -> b{extraTmpFiles = a})
            (filesEditor (Just fp) FileChooserActionOpen "Select File")
    ]),
    ("Library",[
        mkFieldE (emptyParams
        {   paraName    = Just "Library"
        ,   synopsisP   = Just "If the package contains a library, specify the exported modules here"
        ,   shadow      = Just ShadowIn
        ,   direction   = Just Vertical})
            library
            (\ a b -> b{library  = a})
            (maybeEditor (libraryEditor (Just fp) modules,emptyParams{paraName = Just "Specify exported modules"}) True
                "Does this package contain a library?")
    ]),
    ("Executables",[
        mkFieldE (emptyParams
        {   paraName    = Just "Executables"
        ,   synopsisP   = Just "Describe executable programs contained in the package"
        ,   direction   = Just Vertical})
            executables
            (\ a b -> b{executables = a})
            (executablesEditor (Just fp) modules)
    ])]


editPackage :: PackageDescription -> FilePath -> [String] -> GhfAction
editPackage packageD packageDir modules = do
    ghfR <- ask
    res <- lift $editPackage' packageDir packageD (packageDD packageDir modules) ghfR
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
                let (widgetsP, setInjsP, getExtsP, notifiersP) = unzip4 resList
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
        let fieldNames = map (\fd -> case paraName (parameters fd) of
                                            Just s -> s
                                            Nothing -> "Unnamed")
                            $concat
                                $map snd packageDD
        ok `onClicked` (do
            mbNewPackage <- extractAndValidate packageD getExts fieldNames
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

