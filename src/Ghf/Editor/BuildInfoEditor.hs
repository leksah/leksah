--
-- | Module for editing of cabal build infos
-- 

module Ghf.Editor.BuildInfoEditor (
    editBuildInfo
,   buildInfoEditor
,   libraryEditor
,   executableEditor
,   executablesEditor
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
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
import Ghf.Editor.SpecialEditors

-- ------------------------------------------------------------
-- * Build Infos
-- ------------------------------------------------------------

buildInfoEditor :: Maybe FilePath -> Editor BuildInfo
buildInfoEditor fp p = do
    (wid,inj,ext,notif) <- otherEditor (editBuildInfo fp) p
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

libraryEditor :: Maybe FilePath -> Editor Library
libraryEditor fp para = do
    (wid,inj,ext,notif) <- 
        pairEditor
            (multisetEditor 
                (ColumnDescr False [("",(\row -> [New.cellText := show row]))])                 
                (fileEditor fp  FileChooserActionOpen "Select File",emptyParams), 
                    emptyParams{direction = Just Vertical})
            (buildInfoEditor fp, para {paraName = Just "Build Info"})
            para{direction = Just Vertical}
    let pinj (Library em bi) = inj (em,bi)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (em,bi) -> return (Just $Library em bi)
    return (wid,pinj,pext,notif)   

executableEditor :: Maybe FilePath -> Editor Executable
executableEditor fp para = do
    (wid,inj,ext,notif) <- pairEditor 
        (pairEditor 
            (stringEditor,emptyParams {paraName = Just "Executable Name"}) 
            (fileEditor fp FileChooserActionOpen "Select File",emptyParams {paraName = Just "Main module"}), 
            (emptyParams{direction = Just Vertical}))
        (buildInfoEditor fp, emptyParams{paraName = Just "Build Info"})
        para{direction = Just Vertical}
    let pinj (Executable s f bi) = inj ((s,f),bi)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just ((s,f),bi) -> return (Just $Executable s f bi)
    return (wid,pinj,pext,notif)

executablesEditor :: Maybe FilePath -> Editor [Executable]
executablesEditor fp p = 
    multisetEditor
        (ColumnDescr False [("Executable Name",\(Executable exeName _ _) -> [New.cellText := exeName])
                           ,("Module Path",\(Executable  _ mp _) -> [New.cellText := mp])])  
        (executableEditor fp,emptyParams) p{shadow = Just ShadowIn}    

buildInfoD :: Maybe FilePath -> [(String,[FieldDescriptionE BuildInfo])]
buildInfoD fp = [
    ("Description", [
        mkFieldE (emptyParams
        {   paraName    = Just "Component is buildable here"
        ,   PE.synopsis = Nothing})  
            buildable 
            (\ a b -> b{buildable = a})
            boolEditor
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Non-exposed or non-main modules"
        ,   PE.synopsis = Just "A list of modules used by the component but not exposed to users."
        ,   shadow = Just ShadowIn
        ,   direction = Just Vertical})  
            otherModules 
            (\ a b -> b{otherModules = a})
            (filesEditor fp FileChooserActionOpen "Select file")  
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Where to look for the haskell module hierarchy"
        ,   PE.synopsis = Just "Root directories for the module hierarchy."
        ,   shadow = Just ShadowIn
        ,   direction = Just Vertical})  
            hsSourceDirs 
            (\ a b -> b{hsSourceDirs = a})
            (filesEditor fp FileChooserActionSelectFolder "Select folder")
    ]),
    ("Extensions",[ 
        mkFieldE (emptyParams
        {   paraName    = Just "Extensions"
        ,   PE.synopsis = Just "A list of Haskell extensions used by every module."})  
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


editBuildInfo :: Maybe FilePath -> BuildInfo -> String -> IO (Maybe BuildInfo)
editBuildInfo fp buildInfo contextStr = do
    res <- editBuildInfo' buildInfo contextStr (buildInfoD fp)
    return res

editBuildInfo' :: BuildInfo -> String -> [(String,[FieldDescriptionE BuildInfo])] -> IO (Maybe BuildInfo)
editBuildInfo' buildInfo contextStr buildInfoD = do
    resRef  <- newIORef Nothing
    let flatBuildInfoD = concatMap snd buildInfoD
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
                        $concat
                            $map snd buildInfoD    
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








