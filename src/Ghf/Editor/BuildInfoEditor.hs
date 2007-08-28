--
-- | Module for editing of cabal build infos
-- 

module Ghf.Editor.BuildInfoEditor (
    editBuildInfo
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

-- ------------------------------------------------------------
-- * Build Infos
-- ------------------------------------------------------------

{--
BuildInfo	
    buildable :: Bool	component is buildable here
    ccOptions :: [String]	options for C compiler
    ldOptions :: [String]	options for linker
    frameworks :: [String]	support frameworks for Mac OS X
cSources :: [FilePath]	
    hsSourceDirs :: [FilePath]	where to look for the haskell module hierarchy
    otherModules :: [String]	non-exposed or non-main modules
    extensions :: [Extension]	
extraLibs :: [String]	what libraries to link with when compiling a program that uses your package
extraLibDirs :: [String]	
includeDirs :: [FilePath]	directories to find .h files
includes :: [FilePath]	The .h files to be found in includeDirs
installIncludes :: [FilePath]	.h files to install with the package
options :: [(CompilerFlavor, [String])]	
ghcProfOptions :: [String]
--}

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
            (multisetEditor 
                (ColumnDescr False [("",(\row -> [New.cellText := show row]))])
                (fileEditor fp FileChooserActionOpen "Select file" ,emptyParams))  
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Where to look for the haskell module hierarchy"
        ,   PE.synopsis = Just "Root directories for the module hierarchy."
        ,   shadow = Just ShadowIn
        ,   direction = Just Vertical})  
            hsSourceDirs 
            (\ a b -> b{hsSourceDirs = a})
            (multisetEditor 
                (ColumnDescr False [("",(\row -> [New.cellText := show row]))])
                (fileEditor fp FileChooserActionSelectFolder "Select folder" ,emptyParams)) 
    ]),
    ("Extensions",[ 
        mkFieldE (emptyParams
        {   paraName    = Just "Extensions"
        ,   PE.synopsis = Just "A list of Haskell extensions used by every module."})  
            extensions 
            (\ a b -> b{extensions = a})
            extensionsEditor
    ])]
  
{--    

    ,   mkFieldE (emptyParams
        {   paraName    = Just "Options for C compiler"})  
            ccOptions 
            (\ a b -> b{ccOptions = a})
            multisetEditor (stringEditor,emptyParams) p{shadow = Just ShadowIn}    
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Options for linker"})  
            ldOptions 
            (\ a b -> b{ldOptions = a})
            multisetEditor (stringEditor,emptyParams) p{shadow = Just ShadowIn}    
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Support frameworks for Mac OS X"})  
            frameworks 
            (\ a b -> b{frameworks = a})
            multisetEditor (stringEditor,emptyParams) p{shadow = Just ShadowIn}
    ,   mkFieldE (emptyParams
        {   paraName    = Just "Support frameworks for Mac OS X"})  
            cSources 
            (\ a b -> b{cSources = a})
            multisetEditor (fileEditor,emptyParams) p{shadow = Just ShadowIn}    
--}

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
    ok `onClicked` (do
        mbNewBuildInfo <- validate buildInfo getExts
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

extensionsL :: [Extension]
extensionsL = [
        OverlappingInstances
   ,    UndecidableInstances
   ,    IncoherentInstances
   ,    RecursiveDo
   ,    ParallelListComp
   ,    MultiParamTypeClasses
   ,    NoMonomorphismRestriction
   ,    FunctionalDependencies
   ,    Rank2Types
   ,    RankNTypes
   ,    PolymorphicComponents
   ,    ExistentialQuantification
   ,    ScopedTypeVariables
   ,    ImplicitParams
   ,    FlexibleContexts
   ,    FlexibleInstances
   ,    EmptyDataDecls
   ,    CPP
   ,    BangPatterns
   ,    TypeSynonymInstances
   ,    TemplateHaskell
   ,    ForeignFunctionInterface
   ,    InlinePhase
   ,    ContextStack
   ,    Arrows
   ,    Generics
   ,    NoImplicitPrelude
   ,    NamedFieldPuns
   ,    PatternGuards
   ,    GeneralizedNewtypeDeriving
   ,    ExtensibleRecords
   ,    RestrictedTypeSynonyms
   ,    HereDocuments]


extensionsEditor :: Editor [Extension]
extensionsEditor = staticMultiselectionEditor extensionsL






