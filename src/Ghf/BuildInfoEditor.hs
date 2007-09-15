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

import Data.Version()    -- Instances only
import Graphics.UI.Gtk(castToWidget, widgetSetSizeRequest, widgetDestroy,
		       widgetShowAll, containerAdd, boxPackEnd, boxPackStart,
		       scrolledWindowAddWithViewport, scrolledWindowSetPolicy, scrolledWindowNew,
		       notebookSetTabPos, notebookAppendPage, hButtonBoxNew, vBoxNew,
		       FileChooserAction(FileChooserActionSelectFolder, FileChooserActionOpen),
		       textViewGetBuffer, textViewNew, textBufferSetText, onClicked,
		       buttonNewFromStock, windowSetModal, windowNew, mainQuit, mainGUI, AttrOp((:=)),
		       Packing(PackNatural, PackGrow), PositionType(PosTop), ShadowType(ShadowIn),
		       PolicyType(PolicyAutomatic))
import qualified Graphics.UI.Gtk.ModelView as New(cellText)
import Control.Monad.Reader(Monad(return), mapM_, mapM)
import Distribution.Compiler()    -- Instances only
import Distribution.License()    -- Instances only
import Distribution.Package()    -- Instances only
import Distribution.PackageDescription(Executable(Executable),
				       BuildInfo(otherModules, options, installIncludes, hsSourceDirs, ghcProfOptions,
						 extraLibs, extraLibDirs, extensions, cSources, buildable, ldOptions,
						 ccOptions, includes, includeDirs),
				       Library(Library), showHookedBuildInfo)
import Distribution.Version()    -- Instances only
import Language.Haskell.Extension()    -- Instances only
import Data.IORef(writeIORef, readIORef, newIORef)
import Data.List(unzip4)
import Data.Map()    -- Instances only
import System.Directory()    -- Instances only
import Text.ParserCombinators.ReadP()    -- Instances only
import Ghf.Core(Direction(Vertical))
import Ghf.SpecialEditors(compilerFlavorEditor, filesEditor,
				 stringsEditor, extensionsEditor)
import Ghf.ViewFrame(newNotebook)
import Ghf.PropertyEditor(ColumnDescr(..),
				 Parameters(shadow, paraName, direction, synopsisP), EventSelector(FocusIn),
				 FieldDescriptionE(parameters, FDE), Editor, emptyParams, extractAndValidate,
				 mkFieldE, boolEditor, stringEditor, staticMultiselectionEditor, fileEditor,
				 otherEditor, pairEditor, multisetEditor,staticSelectionEditor)

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
            (modulesEditor modules, para {paraName = Just "Exposed Modules"})
            (buildInfoEditor fp modules, para {paraName = Just "Build Info"})
            para{direction = Just Vertical}
    let pinj (Library em bi) = inj (em,bi)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (em,bi) -> return (Just $Library em bi)
    return (wid,pinj,pext,notif)

modulesEditor :: [String] -> Editor [String]
modulesEditor modules = staticMultiselectionEditor modules

moduleEditor :: [String] -> Editor String
moduleEditor modules = staticSelectionEditor modules


executableEditor :: Maybe FilePath -> [String] -> Editor Executable
executableEditor fp modules para = do
    (wid,inj,ext,notif) <- pairEditor
        (pairEditor
            (stringEditor,emptyParams {paraName = Just "Executable Name"})
            (fileEditor fp FileChooserActionOpen "Select File",emptyParams {paraName = Just "Main module"}),
            (emptyParams{direction = Just Vertical}))
        (buildInfoEditor fp modules, emptyParams{paraName = Just "Build Info"})
        para{direction = Just Vertical}
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
        (executableEditor fp modules ,emptyParams) p{shadow = Just ShadowIn}

buildInfoD :: Maybe FilePath -> [String] -> [(String,[FieldDescriptionE BuildInfo])]
buildInfoD fp modules = [
    ("Description", [
        mkFieldE (emptyParams
        {   paraName    = Just "Component is buildable here"
        ,   synopsisP = Nothing})
            buildable
            (\ a b -> b{buildable = a})
            boolEditor
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Non-exposed or non-main modules"
        ,   synopsisP = Just "A list of modules used by the component but not exposed to users."
        ,   shadow = Just ShadowIn
        ,   direction = Just Vertical})
            otherModules
            (\ a b -> b{otherModules = a})
            (modulesEditor modules)
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Where to look for the haskell module hierarchy"
        ,   synopsisP = Just "Root directories for the module hierarchy."
        ,   shadow = Just ShadowIn
        ,   direction = Just Vertical})
            hsSourceDirs
            (\ a b -> b{hsSourceDirs = a})
            (filesEditor fp FileChooserActionSelectFolder "Select folder")
    ]),
    ("Extensions",[
        mkFieldE (emptyParams
        {   paraName    = Just "Extensions"
        ,   synopsisP = Just "A list of Haskell extensions used by every module."})
            extensions
            (\ a b -> b{extensions = a})
            extensionsEditor
    ]),
    ("Options",[
        mkFieldE (emptyParams
        {   paraName    = Just "Options for haskell compilers"
        ,   direction = Just Vertical})
            options
            (\ a b -> b{options = a})
            (multisetEditor
                (ColumnDescr True [("Compiler Flavor",\(cv,_) -> [New.cellText := show cv])
                                   ,("Options",\(_,op) -> [New.cellText := show op])])
                ((pairEditor
                    (compilerFlavorEditor,emptyParams)
                    (stringsEditor,emptyParams)),emptyParams
                        {   direction = Just Vertical
                        ,   shadow   = Just ShadowIn}))
     ,  mkFieldE (emptyParams
        {   paraName    = Just "Additional options for GHC when built with profiling"
        ,   direction = Just Vertical})
            ghcProfOptions
            (\ a b -> b{ghcProfOptions = a})
            stringsEditor
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Options for C compiler"
        ,   direction = Just Vertical})
            ccOptions
            (\ a b -> b{ccOptions = a})
            stringsEditor
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Options for linker"
        ,   direction = Just Vertical})
            ldOptions
            (\ a b -> b{ldOptions = a})
            stringsEditor
    ]),
    ("C",[
         mkFieldE (emptyParams
        {   paraName    = Just "A list of header files already installed on the system"
        ,   direction = Just Vertical})
            includes
            (\ a b -> b{includes = a})
            stringsEditor
     ,   mkFieldE (emptyParams
        {   paraName    = Just "A list of header files from this package"
        ,   direction = Just Vertical})
            installIncludes
            (\ a b -> b{installIncludes = a})
            (filesEditor fp FileChooserActionOpen "Select File")
     ,   mkFieldE (emptyParams
        {   paraName    = Just "A list of directories to search for header files"
        ,   direction = Just Vertical})
            includeDirs
            (\ a b -> b{includeDirs = a})
            (filesEditor fp FileChooserActionSelectFolder "Select Folder")
     ,   mkFieldE (emptyParams
        {   paraName    = Just "A list of C source files to be compiled,linked with the Haskell files."
        ,   direction = Just Vertical})
            cSources
            (\ a b -> b{cSources = a})
            (filesEditor fp FileChooserActionOpen "Select file")
     ,   mkFieldE (emptyParams
        {   paraName    = Just "A list of extra libraries to link with"
        ,   direction = Just Vertical})
            extraLibs
            (\ a b -> b{extraLibs = a})
            stringsEditor
     ,   mkFieldE (emptyParams
        {   paraName    = Just "A list of directories to search for libraries."
        ,   direction = Just Vertical})
            extraLibDirs
            (\ a b -> b{extraLibDirs = a})
            (filesEditor fp FileChooserActionSelectFolder "Select Folder")
   ]),
    ("Mac OS X",[
        mkFieldE (emptyParams
        {   paraName    = Just "Support frameworks for Mac OS X"
        ,   direction = Just Vertical})
            cSources
            (\ a b -> b{cSources = a})
            stringsEditor
    ])]


editBuildInfo :: Maybe FilePath -> [String] -> BuildInfo -> String -> IO (Maybe BuildInfo)
editBuildInfo fp modules buildInfo contextStr = do
    res <- editBuildInfo' buildInfo contextStr (buildInfoD fp modules)
    return res

editBuildInfo' :: BuildInfo -> String -> [(String,[FieldDescriptionE BuildInfo])] -> IO (Maybe BuildInfo)
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
            resList <- mapM (\ (FDE _ editorF) -> editorF buildInfo) partBuildInfoD
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
    let fieldNames = map (\fd -> case paraName (parameters fd) of
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
    boxPackStart vb nb PackGrow 7
    boxPackEnd vb bb PackNatural 7
    containerAdd dialog vb
    widgetSetSizeRequest dialog 500 700
    widgetShowAll dialog
    mainGUI
    res <- readIORef resRef
    return (res)








