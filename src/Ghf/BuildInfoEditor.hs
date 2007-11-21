--
-- | Module for editing of cabal build infos
--

module Ghf.BuildInfoEditor (
    editBuildInfo
,   buildInfoEditor
,   libraryEditor
,   executableEditor
,   executablesEditor
) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as New(cellText)
import Control.Monad.Reader
import Distribution.PackageDescription
import Data.IORef
import Data.List

import Ghf.Core.State
import Ghf.SpecialEditors
import Ghf.ViewFrame
import GUI.Ghf.EditorBasics
import GUI.Ghf.MakeEditor
import GUI.Ghf.SimpleEditors
import GUI.Ghf.CompositeEditors
import GUI.Ghf.Parameters

-- ------------------------------------------------------------
-- * Build Infos
-- ------------------------------------------------------------

buildInfoEditor :: Maybe FilePath -> [String] -> Editor BuildInfo
buildInfoEditor fp modules p = do
    (wid,inj,ext,notif) <- otherEditor (editBuildInfo fp modules) p
    box      <-  vBoxNew False 1
    textView <-  textViewNew
    widgetSetSizeRequest textView (-1) 300
    containerAdd box wid
    containerAdd box textView
    buffer <- textViewGetBuffer textView
    let binj bi = do
        inj bi
        textBufferSetText buffer $showHookedBuildInfo (Just bi,[])
    notif FocusIn $Left (changedHandler buffer ext)
    return (castToWidget box,binj,ext,notif)
    where
    changedHandler buffer ext _ = do
        putStrLn "FocusIn"
        mbv <- ext
        putStrLn (show mbv)
        case mbv of
            Just v -> textBufferSetText buffer $showHookedBuildInfo (Just v,[])
            Nothing -> return ()
        return True

libraryEditor :: Maybe FilePath -> [String] -> Editor Library
libraryEditor fp modules para = do
    (wid,inj,ext,notif) <-
        pairEditor
            (modulesEditor modules, paraName <<<- ParaName "Exposed Modules" $para)
            (buildInfoEditor fp modules, paraName <<<- ParaName "Build Info" $ para)
            (paraDirection <<<- ParaDirection Vertical $ emptyParams)
    let pinj (Library em bi) = inj (em,bi)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (em,bi) -> return (Just $Library em bi)
    return (wid,pinj,pext,notif)

modulesEditor :: [String] -> Editor [String]
modulesEditor modules   =   staticMultiselectionEditor modules

moduleEditor :: [String] -> Editor String
moduleEditor modules    =   staticSelectionEditor modules


executableEditor :: Maybe FilePath -> [String] -> Editor Executable
executableEditor fp modules para = do
    (wid,inj,ext,notif) <- pairEditor
        (pairEditor
            (stringEditor, paraName <<<- ParaName "Executable Name" $ emptyParams)
            (fileEditor fp FileChooserActionOpen "Select File",
                paraName <<<- ParaName "Main Module" $ emptyParams),
            (paraDirection <<<- ParaDirection Vertical $ emptyParams))
        (buildInfoEditor fp modules, paraName <<<- ParaName "Build Info" $ emptyParams)
        (paraDirection  <<<- ParaDirection Vertical $ para)
    let pinj (Executable s f bi) = inj ((s,f),bi)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just ((s,f),bi) -> return (Just $Executable s f bi)
    return (wid,pinj,pext,notif)

executablesEditor :: Maybe FilePath -> [String] -> Editor [Executable]
executablesEditor fp modules p =
    multisetEditor
        (ColumnDescr False [("Executable Name",\(Executable exeName _ _) -> [New.cellText := exeName])
                           ,("Module Path",\(Executable  _ mp _) -> [New.cellText := mp])])
        (executableEditor fp modules ,emptyParams)
            (paraShadow  <<<- ParaShadow ShadowIn $ p)

buildInfoD :: Maybe FilePath -> [String] -> [(String,[FieldDescription BuildInfo])]
buildInfoD fp modules = [
    ("Description", [
        mkField
            (paraName <<<- ParaName "Component is buildable here" $ emptyParams)
            buildable
            (\ a b -> b{buildable = a})
            boolEditor
    ,   mkField
            (paraName <<<- ParaName "Non-exposed or non-main modules"
                $ paraSynopsis <<<- ParaSynopsis
                    "A list of modules used by the component but not exposed to users."
                    $ paraShadow <<<- ParaShadow ShadowIn
                        $ paraDirection <<<- ParaDirection Vertical
                            $ emptyParams)
            otherModules
            (\ a b -> b{otherModules = a})
            (modulesEditor modules)
    ,   mkField
            (paraName  <<<- ParaName
                "Where to look for the haskell module hierarchy"
                $ paraSynopsis <<<- ParaSynopsis
                    "Root directories for the module hierarchy."
                    $ paraShadow  <<<- ParaShadow ShadowIn
                        $ paraDirection  <<<- ParaDirection Vertical
                            $ emptyParams)
            hsSourceDirs
            (\ a b -> b{hsSourceDirs = a})
            (filesEditor fp FileChooserActionSelectFolder "Select folder")
    ]),
    ("Extensions",[
        mkField
            (paraName  <<<- ParaName "Extensions"
                $ paraSynopsis  <<<- ParaSynopsis
                    "A list of Haskell extensions used by every module."
                    $ emptyParams)
            extensions
            (\ a b -> b{extensions = a})
            extensionsEditor
    ]),
    ("Options",[
        mkField
            (paraName  <<<- ParaName "Options for haskell compilers"
                $ paraDirection <<<- ParaDirection Vertical
                    $ emptyParams)
            options
            (\ a b -> b{options = a})
            (multisetEditor
                (ColumnDescr True [("Compiler Flavor",\(cv,_) -> [New.cellText := show cv])
                                   ,("Options",\(_,op) -> [New.cellText := show op])])
                ((pairEditor
                    (compilerFlavorEditor,emptyParams)
                    (stringsEditor,emptyParams)),
                        (paraDirection <<<- ParaDirection Vertical
                            $ paraShadow  <<<- ParaShadow ShadowIn $ emptyParams)))
     ,  mkField
            (paraName <<<- ParaName "Additional options for GHC when built with profiling"
                $ paraDirection <<<- ParaDirection Vertical
                    $ emptyParams)
            ghcProfOptions
            (\ a b -> b{ghcProfOptions = a})
            stringsEditor
    ,   mkField
            (paraName <<<- ParaName "Options for C compiler"
                $ paraDirection <<<- ParaDirection Vertical
                    $ emptyParams)
            ccOptions
            (\ a b -> b{ccOptions = a})
            stringsEditor
    ,   mkField
            (paraName <<<- ParaName "Options for linker"
                $ paraDirection <<<- ParaDirection Vertical
                    $ emptyParams)
            ldOptions
            (\ a b -> b{ldOptions = a})
            stringsEditor
    ]),
    ("C",[
         mkField
            (paraName <<<- ParaName "A list of header files already installed on the system"
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            includes
            (\ a b -> b{includes = a})
            stringsEditor
     ,   mkField
            (paraName <<<- ParaName "A list of header files from this package"
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            installIncludes
            (\ a b -> b{installIncludes = a})
            (filesEditor fp FileChooserActionOpen "Select File")
     ,   mkField
            (paraName <<<- ParaName "A list of directories to search for header files"
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            includeDirs
            (\ a b -> b{includeDirs = a})
            (filesEditor fp FileChooserActionSelectFolder "Select Folder")
     ,   mkField
            (paraName <<<- ParaName
                "A list of C source files to be compiled,linked with the Haskell files."
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            cSources
            (\ a b -> b{cSources = a})
            (filesEditor fp FileChooserActionOpen "Select file")
     ,   mkField
            (paraName <<<- ParaName "A list of extra libraries to link with"
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            extraLibs
            (\ a b -> b{extraLibs = a})
            stringsEditor
     ,   mkField
            (paraName <<<- ParaName "A list of directories to search for libraries."
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            extraLibDirs
            (\ a b -> b{extraLibDirs = a})
            (filesEditor fp FileChooserActionSelectFolder "Select Folder")
   ]),
    ("Mac OS X",[
        mkField
            (paraName <<<- ParaName "Support frameworks for Mac OS X"
                $ paraDirection <<<- ParaDirection Vertical $ emptyParams)
            cSources
            (\ a b -> b{cSources = a})
            stringsEditor
    ])]


editBuildInfo :: Maybe FilePath -> [String] -> BuildInfo -> String -> IO (Maybe BuildInfo)
editBuildInfo fp modules buildInfo contextStr = do
    res <- editBuildInfo' buildInfo contextStr (buildInfoD fp modules)
    return res

editBuildInfo' :: BuildInfo -> String -> [(String,[FieldDescription BuildInfo])] -> IO (Maybe BuildInfo)
editBuildInfo' buildInfo contextStr buildInfoD = do
    resRef  <- newIORef Nothing
    dialog  <- windowNew
    windowSetModal dialog True
    vb      <- vBoxNew False 7
    bb      <- hButtonBoxNew
    ok      <- buttonNewFromStock "gtk-ok"
    cancel  <- buttonNewFromStock "gtk-cancel"
    boxPackStart bb ok PackNatural 0
    boxPackStart bb cancel PackNatural 0
    nb <- newNotebook
    notebookSetTabPos nb PosTop
    res <- mapM
        (\ (tabLabel, partBuildInfoD) -> do
            resList <- mapM (\ (FD _ editorF) -> editorF buildInfo) partBuildInfoD
            let (widgetsP, setInjsP, getExtsP,notifiersP) = unzip4 resList
            nbbox <- vBoxNew False 0
            mapM_ (\ w -> boxPackStart nbbox w PackNatural 0) widgetsP
            sw <- scrolledWindowNew Nothing Nothing
            scrolledWindowAddWithViewport sw nbbox
            scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
            notebookAppendPage nb sw tabLabel
            return (widgetsP, setInjsP, getExtsP, notifiersP))
                buildInfoD
    let (widgets, setInjs, getExts, notifiers) =
            foldl (\ (w,i,e,n) (w2,i2,e2,n2) -> (w ++ w2, i ++ i2, e ++ e2, n ++ n2)) ([],[],[],[]) res
    let fieldNames = map (\fd -> case getParameterPrim paraName (parameters fd) of
                                        Just s -> s
                                        Nothing -> "Unnamed")
                        $concatMap snd buildInfoD
    ok `onClicked` (do
        mbNewBuildInfo <- extractAndValidate buildInfo getExts fieldNames
        case mbNewBuildInfo of
            Nothing -> do
                putStrLn "Cant't validate build info"
                return ()
            Just newBuildInfo -> do
                    writeIORef resRef (Just newBuildInfo)
                    widgetDestroy dialog
                    mainQuit
                    return ())
    cancel `onClicked` (do
        widgetDestroy dialog
        mainQuit
        return ())
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
    res <- readIORef resRef
    return (res)








