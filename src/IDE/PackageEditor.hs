-----------------------------------------------------------------------------
--
-- Module      :  IDE.PackageEditor
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Module for editing of cabal packages
--
-----------------------------------------------------------------------------------


module IDE.PackageEditor (
    packageNew
,   packageEdit
,   choosePackageDir
,   choosePackageFile
) where

import Graphics.UI.Gtk
import Control.Monad.Reader
import Distribution.Compiler
import Distribution.License
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import Distribution.Verbosity
import System.FilePath
import Data.IORef
import Data.List
import Data.Maybe
import System.Directory

import IDE.Core.State
import IDE.Utils.File
import IDE.SpecialEditors
import IDE.Framework.ViewFrame
import IDE.BuildInfoEditor
import IDE.Framework.MakeEditor
import IDE.Framework.SimpleEditors
import IDE.Framework.CompositeEditors
import IDE.Framework.Parameters

standardSetup = "#!/usr/bin/runhaskell \n"
                    ++ "> module Main where\n"
                    ++ "> import Distribution.Simple\n"
                    ++ "> main :: IO ()\n"
                    ++ "> main = defaultMain\n\n"

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
        _                   -> return Nothing

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
        _                   -> return Nothing

packageEdit :: IDEAction
packageEdit = do
    window  <- readIDE window
    mbFileName <- lift $choosePackageFile window
    case mbFileName of
        Nothing -> return ()
        Just fileName -> do
            let dirName = dropFileName fileName
            modules <- lift $allModules dirName
            package <- lift $readPackageDescription normal fileName
            editPackage (flattenPackageDescription package) dirName modules
            return ()

packageNew ::  IDEAction
packageNew = do
    window  <- readIDE window
    mbDirName <- lift $choosePackageDir window
    case mbDirName of
        Nothing -> return ()
        Just dirName -> do
            cfn <-  lift $cabalFileName dirName
            continue <- do
                if isJust cfn
                    then lift $do
                        md <- messageDialogNew Nothing [] MessageQuestion ButtonsYesNo
                                    $ "There is already a .cabal file in this directory."
                                    ++  " Continue anyway?"
                        rid <- dialogRun md
                        widgetDestroy md
                        case rid of
                            ResponseYes ->  return True
                            otherwise   ->  return False
                    else return False
            when continue $do
                    modules <- lift $do
                        b1 <- doesFileExist (dirName </> "Setup.hs")
                        b2 <- doesFileExist (dirName </> "Setup.lhs")
                        if  not (b1 || b2)
                            then do
                                putStrLn "Setup.(l)hs does not exist. Writing Standard"
                                writeFile (dirName </> "Setup.lhs") standardSetup
                            else message "Setup.(l)hs already exist"
                        allModules dirName
                    editPackage emptyPackageDescription dirName modules
                    return ()

type PDescr = [(String,[FieldDescription PackageDescription])]

packageDD :: FilePath -> [String] -> PDescr
packageDD fp modules = [
    ("Description -1-", [
        mkField
            (paraName <<<- ParaName "Package Identifier" $ emptyParams)
            package
            (\ a b -> b{package = a})
            packageEditor
    ,   mkField
            (paraName <<<- ParaName "Cabal version"
                $ paraSynopsis <<<- ParaSynopsis
                    "Does this package depends on a specific version of Cabal?"
                    $ paraShadow <<<- ParaShadow ShadowIn $ emptyParams)
            descCabalVersion
            (\ a b -> b{descCabalVersion = a})
            versionRangeEditor
    ,   mkField
            (paraName <<<- ParaName "License" $ emptyParams)
            license
            (\ a b -> b{license = a})
            (staticSelectionEditor [GPL, LGPL, BSD3, BSD4, PublicDomain, AllRightsReserved, OtherLicense])
    ,   mkField
            (paraName <<<- ParaName "License File" $ emptyParams)
            licenseFile
            (\ a b -> b{licenseFile = a})
            (fileEditor (Just fp) FileChooserActionOpen "Select file")
    ,   mkField
            (paraName <<<- ParaName "Copyright" $ emptyParams)
            copyright
            (\ a b -> b{copyright = a})
            stringEditor
    ,   mkField
            (paraName <<<- ParaName "Author" $ emptyParams)
            author
            (\ a b -> b{author = a})
            stringEditor
    ,   mkField
            (paraName <<<- ParaName "Maintainer" $ emptyParams)
            maintainer
            (\ a b -> b{maintainer = a})
            stringEditor
    ]),
    ("Description -2-",[
        mkField
            (paraName <<<- ParaName "Stability" $ emptyParams)
            stability
            (\ a b -> b{stability = a})
            stringEditor
    ,   mkField
            (paraName <<<- ParaName "Homepage" $ emptyParams)
            homepage
            (\ a b -> b{homepage = a})
            stringEditor
    ,   mkField
            (paraName <<<- ParaName "Package URL" $ emptyParams)
            pkgUrl
            (\ a b -> b{pkgUrl = a})
            stringEditor
    ,   mkField
            (paraName <<<- ParaName "Synopsis"
                $ paraSynopsis <<<- ParaSynopsis "A one-line summary of this package"
                    $ emptyParams)
            synopsis
            (\ a b -> b{synopsis = a})
            stringEditor
    ,   mkField
            (paraName <<<- ParaName "Description"
                $ paraSynopsis <<<- ParaSynopsis "A more verbose description of this package"
                    $ paraShadow <<<- ParaShadow ShadowOut
                        $ paraMinSize <<<- ParaMinSize (-1,250)
                            $ emptyParams)
            description
            (\ a b -> if null a then b{description = " \n\n\n\n\n"} else  b{description = a})
            multilineStringEditor
    ,   mkField
            (paraName <<<- ParaName "Category" $ emptyParams)
            category
            (\ a b -> b{category = a})
            stringEditor
    ]),
    ("Tested With",[
        mkField
            (paraName <<<- ParaName "Tested with compiler"
                $ paraShadow <<<- ParaShadow ShadowIn
                    $ paraDirection <<<- ParaDirection Vertical
                        $ emptyParams)
            (\a -> case testedWith a of
                []          -> [(GHC,AnyVersion)]
                l           -> l)
            (\ a b -> b{testedWith = a})
            testedWidthEditor
    ]),
    ("Dependencies",[
        mkField
            (paraName <<<- ParaName "Build Dependencies"
                $ paraSynopsis <<<- ParaSynopsis "Does this package depends on other packages?"
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            buildDepends
            (\ a b -> b{buildDepends = a})
            dependenciesEditor
    ]),
    ("Other Files",[
        mkField
            (paraName <<<- ParaName "Data Files"
                $ paraSynopsis <<<- ParaSynopsis
                    "A list of files to be installed for run-time use by the package."
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            dataFiles
            (\ a b -> b{dataFiles = a})
            (filesEditor (Just fp) FileChooserActionOpen "Select File")
    ,   mkField
            (paraName <<<- ParaName "Extra Source Files"
                $ paraSynopsis <<<- ParaSynopsis
                    "A list of additional files to be included in source distributions."
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            extraSrcFiles
            (\ a b -> b{extraSrcFiles = a})
            (filesEditor (Just fp) FileChooserActionOpen "Select File")
    ,   mkField
            (paraName <<<- ParaName "Extra Tmp Files"
                $ paraSynopsis <<<- ParaSynopsis
                    "A list of additional files or directories to be removed by setup clean."
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            extraTmpFiles
            (\ a b -> b{extraTmpFiles = a})
            (filesEditor (Just fp) FileChooserActionOpen "Select File")
    ]),
    ("Library",[
        mkField
            (paraName <<<- ParaName "Library"
                $ paraSynopsis <<<- ParaSynopsis
                    "If the package contains a library, specify the exported modules here"
                    $ paraDirection <<<- ParaDirection Vertical
                        $ paraShadow <<<- ParaShadow ShadowIn $ emptyParams)
            library
            (\ a b -> b{library  = a})
            (maybeEditor (libraryEditor BuildEditorFactoryI (Just fp) modules,
                paraName <<<- ParaName "Specify exported modules" $ emptyParams) True
                "Does this package contain a library?")
    ]),
    ("Executables",[
        mkField
            (paraName <<<- ParaName "Executables"
                $ paraSynopsis <<<- ParaSynopsis
                "Describe executable programs contained in the package"
                    $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            executables
            (\ a b -> b{executables = a})
            (executablesEditor BuildEditorFactoryI (Just fp) modules)
    ])]


editPackage :: PackageDescription -> FilePath -> [String] -> IDEAction
editPackage packageD packageDir modules = do
    ideR <- ask
    res <- lift $editPackage' packageDir packageD (packageDD packageDir modules) ideR
    lift $putStrLn $show res

editPackage' :: String -> PackageDescription -> PDescr -> IDERef -> IO ()
editPackage' packageDir packageD packageDD ideR   =
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
                resList <- mapM (\ (FD _ editorF) -> editorF packageD) partPackageDesc
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
        let fieldNames = map (\fd -> case getParameterPrim paraName (parameters fd) of
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
                    widgetDestroy dialog
                    mainQuit)
        cancel `onClicked` (do
            widgetDestroy dialog
            mainQuit)
        dialog `onDelete` (\e -> do
            widgetDestroy dialog
            mainQuit
            return True)
        boxPackStart vb nb PackGrow 7
        boxPackEnd vb bb PackNatural 7
        containerAdd dialog vb
        widgetSetSizeRequest dialog 500 700
        widgetShowAll dialog
        mainGUI
        return ()

