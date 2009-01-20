--
-- | Special Editors
--

module IDE.SpecialEditors (
    packageEditor
,   testedWidthEditor
,   compilerFlavorEditor
,   versionRangeEditor
,   versionEditor
,   dependencyEditor
,   dependenciesEditor
,   filesEditor
,   stringsEditor
,   extensionsEditor
,   panePathEditor
,   libraryEditor
,   executablesEditor
,   modulesEditor
,   styleEditor
,   Library'(..)
,   Executable'(..)
) where

import Graphics.UI.Editor.Simple (intEditor,genericEditor,staticListEditor,comboSelectionEditor,stringEditor,fileEditor)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import Graphics.UI.Gtk.SourceView (sourceStyleSchemeManagerGetSchemeIds)
import Graphics.UI.Gtk.SourceView (sourceStyleSchemeManagerNew)
import Distribution.ModuleName (ModuleName(..))
import Data.List (sort)
import Distribution.Text (simpleParse,display)
import Control.Monad.Reader
import Distribution.PackageDescription
import Distribution.Package
import Distribution.Compiler
import Distribution.Version
import Language.Haskell.Extension

import IDE.Core.State hiding (packageId)
import Graphics.UI.Editor.Basics
import Graphics.UI.Editor.Composite
import Graphics.UI.Editor.Parameters
import Default
import MyMissing

packageEditor :: Editor PackageIdentifier
packageEditor para noti = do
    (wid,inj,ext) <- pairEditor
        (stringEditor (\s -> not (null s)), paraName <<<- ParaName "Name" $ emptyParams)
        (versionEditor, paraName <<<- ParaName "Version" $ emptyParams)
        (paraDirection <<<- ParaDirection Horizontal
            $ paraShadow <<<- ParaShadow ShadowIn
                $ para) noti
    let pinj (PackageIdentifier (PackageName n) v) = inj (n,v)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (n,v) -> do
                if null n
                    then return Nothing
                    else return (Just $PackageIdentifier (PackageName n) v)
    return (wid,pinj,pext)

testedWidthEditor :: Editor [(CompilerFlavor, VersionRange)]
testedWidthEditor para = do
    multisetEditor
       (ColumnDescr True [("Compiler Flavor",\(cv,_) -> [New.cellText := show cv])
                           ,("Version Range",\(_,vr) -> [New.cellText := display vr])])
       (pairEditor
            (compilerFlavorEditor, paraShadow <<<- (ParaShadow ShadowNone) $ emptyParams)
            (versionRangeEditor, paraShadow <<<- (ParaShadow ShadowNone) $ emptyParams),
            (paraDirection <<<- (ParaDirection Vertical) $ emptyParams))
       para

compilerFlavorEditor :: Editor CompilerFlavor
compilerFlavorEditor para noti = do
    noti2 <- emptyNotifier
    noti3 <- emptyNotifier
    (wid,inj,ext) <- eitherOrEditor
        (comboSelectionEditor flavors , paraName <<<- (ParaName "Select compiler") $ emptyParams)
        (stringEditor (\s -> not (null s)), paraName <<<- (ParaName "Specify compiler") $ emptyParams)
        "Other"
        (paraName <<<- ParaName "Select" $ para)
        noti
    let cfinj comp  = case comp of
                        (OtherCompiler str) -> inj (Right "")
                        other               -> inj (Left other)
    let cfext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (Right s) -> return (Just $OtherCompiler s)
            Just (Left other) -> return (Just other)
    return (wid,cfinj,cfext)
        where
        flavors = [GHC, NHC, Hugs, HBC, Helium, JHC]

versionRangeEditor :: Editor VersionRange
versionRangeEditor para noti = do
    (wid,inj,ext) <-
        maybeEditor
            (eitherOrEditor
                (pairEditor
                    (comboSelectionEditor v1, emptyParams)
                    (versionEditor, paraName <<<- ParaName "Enter Version" $ emptyParams),
                    (paraDirection <<<- ParaDirection Vertical)
                        $ paraOuterAlignment <<<- ParaOuterAlignment  (0.0, 0.0, 0.0, 0.0)
                        $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                        $ paraInnerAlignment <<<- ParaInnerAlignment  (0.0, 0.0, 0.0, 0.0)
                        $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                        $ emptyParams)
                (pairEditor
                    (comboSelectionEditor v2, emptyParams)
                    (pairEditor
                        (versionRangeEditor, paraShadow <<<- ParaShadow ShadowIn $ emptyParams)
                        (versionRangeEditor, paraShadow <<<- ParaShadow ShadowIn $ emptyParams),
                        paraDirection <<<- ParaDirection Vertical $ emptyParams),
                            paraDirection <<<- ParaDirection Vertical
                            $ paraOuterAlignment <<<- ParaOuterAlignment  (0.0, 0.0, 0.0, 0.0)
                            $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                            $ paraInnerAlignment <<<- ParaInnerAlignment  (0.0, 0.0, 0.0, 0.0)
                            $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                            $ emptyParams )
                        "Complex",paraName <<<- ParaName "Simple"
                                    $ paraOuterAlignment <<<- ParaOuterAlignment  (0.0, 0.0, 0.0, 0.0)
                                    $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                                    $ paraInnerAlignment <<<- ParaInnerAlignment  (0.0, 0.0, 0.0, 0.0)
                                    $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                                    $ emptyParams) False "Any Version"
                        (paraDirection <<<- ParaDirection Vertical $ para)
                        noti
    let vrinj AnyVersion                =   inj Nothing
        vrinj (ThisVersion v)           =   inj (Just (Left (ThisVersionS,v)))
        vrinj (LaterVersion v)          =   inj (Just (Left (LaterVersionS,v)))
        vrinj (EarlierVersion v)        =   inj (Just (Left (EarlierVersionS,v)))
        vrinj (UnionVersionRanges (ThisVersion v1) (LaterVersion v2)) | v1 == v2
                                        =  inj (Just (Left (ThisOrLaterVersionS,v1)))
        vrinj (UnionVersionRanges (LaterVersion v1) (ThisVersion v2)) | v1 == v2
                                        =  inj (Just (Left (ThisOrLaterVersionS,v1)))
        vrinj (UnionVersionRanges (ThisVersion v1) (EarlierVersion v2)) | v1 == v2
                                        =  inj (Just (Left (ThisOrEarlierVersionS,v1)))
        vrinj (UnionVersionRanges (EarlierVersion v1) (ThisVersion v2)) | v1 == v2
                                        =  inj (Just (Left (ThisOrEarlierVersionS,v1)))
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

                        Just (Just (Left (ThisOrLaterVersionS,v)))   -> return (Just (orLaterVersion  v))
                        Just (Just (Left (ThisOrEarlierVersionS,v)))   -> return (Just (orEarlierVersion  v))
                        Just (Just (Right (UnionVersionRangesS,(v1,v2))))
                                                        -> return (Just (UnionVersionRanges v1 v2))
                        Just (Just (Right (IntersectVersionRangesS,(v1,v2))))
                                                        -> return (Just (IntersectVersionRanges v1 v2))
    return (wid,vrinj,vrext)
        where
            v1 = [ThisVersionS,LaterVersionS,ThisOrLaterVersionS,EarlierVersionS,ThisOrEarlierVersionS]
            v2 = [UnionVersionRangesS,IntersectVersionRangesS]

data Version1 = ThisVersionS | LaterVersionS | ThisOrLaterVersionS | EarlierVersionS | ThisOrEarlierVersionS
    deriving (Eq)
instance Show Version1 where
    show ThisVersionS   =  "This Version"
    show LaterVersionS  =  "Later Version"
    show ThisOrLaterVersionS = "This or later Version"
    show EarlierVersionS =  "Earlier Version"
    show ThisOrEarlierVersionS = "This or earlier Version"

data Version2 = UnionVersionRangesS | IntersectVersionRangesS
    deriving (Eq)
instance Show Version2 where
    show UnionVersionRangesS =  "Union Version Ranges"
    show IntersectVersionRangesS =  "Intersect Version Ranges"

versionEditor :: Editor Version
versionEditor para noti = do
    (wid,inj,ext) <- stringEditor (\s -> not (null s)) para noti
    let pinj v = inj (display v)
    let pext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> return (simpleParse s)
    return (wid, pinj, pext)

dependencyEditor :: [PackageIdentifier] -> Editor Dependency
dependencyEditor packages para noti = do
    (wid,inj,ext) <- pairEditor
        (comboSelectionEditor (sort (map (display . pkgName) packages))
            , paraName <<<- ParaName "Package Name" $ emptyParams)
        (versionRangeEditor,paraName <<<- ParaName "Version" $ emptyParams)
        (paraDirection <<<- ParaDirection Vertical $ para)
        noti
    let pinj (Dependency (PackageName s) v) = inj (s,v)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just ("",v) -> return Nothing
            Just (s,v) -> return (Just $Dependency (PackageName s) v)
    return (wid,pinj,pext)

dependenciesEditor :: [PackageIdentifier] -> Editor [Dependency]
dependenciesEditor packages p noti =
    multisetEditor
        (ColumnDescr True [("Package",\(Dependency (PackageName str) _) -> [New.cellText := str])
                           ,("Version",\(Dependency _ vers) -> [New.cellText := display vers])])
        (dependencyEditor packages,
            paraOuterAlignment <<<- ParaInnerAlignment (0.0, 0.5, 1.0, 1.0)
                $ paraInnerAlignment <<<- ParaOuterAlignment (0.0, 0.5, 1.0, 1.0)
                   $ emptyParams)
        (paraShadow <<<- ParaShadow ShadowIn $
            paraOuterAlignment <<<- ParaInnerAlignment (0.0, 0.5, 1.0, 1.0)
                $ paraInnerAlignment <<<- ParaOuterAlignment (0.0, 0.5, 1.0, 1.0)
                    $ paraDirection  <<<-  ParaDirection Vertical
                        $ paraPack <<<- ParaPack PackGrow
                            $ p)
        noti

filesEditor :: Maybe FilePath -> FileChooserAction -> String -> Editor [FilePath]
filesEditor fp act label p =
    multisetEditor
        (ColumnDescr False [("",(\row -> [New.cellText := row]))])
        (fileEditor fp act label, emptyParams)
            (paraShadow <<<- ParaShadow ShadowIn $
                paraDirection  <<<- ParaDirection Vertical $ p)

stringsEditor :: (String -> Bool) -> Editor [String]
stringsEditor validation p =
    multisetEditor
        (ColumnDescr False [("",(\row -> [New.cellText := row]))])
        (stringEditor validation, emptyParams)
            (paraShadow <<<- ParaShadow ShadowIn $ p)

panePathEditor :: Editor StandardPath
panePathEditor = genericEditor

extensionsEditor :: Editor [Extension]
extensionsEditor p = staticListEditor extensionsL (paraMultiSel <<<- ParaMultiSel True $ p)

styleEditor :: Editor (Maybe String)
styleEditor p n = do
    styleManager <- sourceStyleSchemeManagerNew
    ids          <- sourceStyleSchemeManagerGetSchemeIds styleManager
    maybeEditor (comboSelectionEditor ids, p) True "Select a special style?" p n


extensionsL :: [Extension]
extensionsL = knownExtensions

{--[
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
   ,    Arrows
   ,    Generics
   ,    NoImplicitPrelude
   ,    NamedFieldPuns
   ,    PatternGuards
   ,    GeneralizedNewtypeDeriving
   ,    ExtensibleRecords
   ,    RestrictedTypeSynonyms
   ,    HereDocuments]--}

-- ------------------------------------------------------------
-- * BuildInfos
-- ------------------------------------------------------------

data Library' = Library'{
    exposedModules' :: [ModuleName]
,   libExposed'     :: Bool
,   libBuildInfoIdx :: Int}
    deriving (Show, Eq)

data Executable' = Executable'{
    exeName'        :: String
,   modulePath'     :: FilePath
,   buildInfoIdx    :: Int}
    deriving (Show, Eq)

instance Default Library'
    where getDefault =  Library' [] getDefault getDefault

instance Default Executable'
    where getDefault = Executable' getDefault getDefault getDefault


libraryEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Library'
libraryEditor fp modules numBuildInfos para noti = do
    (wid,inj,ext) <-
        pairEditor
            (modulesEditor modules,
            paraName <<<- ParaName "Exposed Modules"
            $ paraPack <<<- ParaPack PackGrow
            $ paraMinSize <<<- ParaMinSize (-1,500)
            $ para)
            (buildInfoEditorP numBuildInfos, paraName <<<- ParaName "Build Info"
            $ paraPack <<<- ParaPack PackNatural
            $ para)
            (paraDirection <<<- ParaDirection Vertical
            $ paraPack <<<- ParaPack PackGrow
            $ emptyParams)
            noti
    let pinj (Library' em True bi) = inj (map display em,bi)
        pinj _  = throwIDE  "SpecialEditors>>libraryEditor: No Library"
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (em,bi) -> return (Just $Library' (map (\s -> forceJust (simpleParse s)
                "SpecialEditor >> libraryEditor: no parse for moduile name") em) True bi)
    return (wid,pinj,pext)

moduleEditor :: [ModuleName] -> Editor String
moduleEditor modules    =   comboSelectionEditor (map display modules)

modulesEditor :: [ModuleName] -> Editor [String]
modulesEditor modules   =   staticListEditor (map display modules)

executablesEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor [Executable']
executablesEditor fp modules countBuildInfo p =
    multisetEditor
        (ColumnDescr True [("Executable Name",\(Executable' exeName _ _) -> [New.cellText := exeName])
                           ,("Module Path",\(Executable'  _ mp _) -> [New.cellText := mp])

                           ,("Build info index",\(Executable'  _ _ bii) -> [New.cellText := show (bii + 1)])])
        (executableEditor fp modules countBuildInfo,emptyParams)
            (paraShadow  <<<- ParaShadow ShadowIn $ p)

executableEditor :: Maybe FilePath -> [ModuleName] -> Int -> Editor Executable'
executableEditor fp modules countBuildInfo para noti = do
    (wid,inj,ext) <- pairEditor
        (pairEditor
            (stringEditor (\s -> not (null s)),
                paraName <<<- ParaName "Executable Name"
                $ emptyParams)
            (stringEditor (\s -> not (null s)),
                paraDirection <<<- ParaDirection Vertical
                $ paraName <<<- ParaName "File with main function"
                $ emptyParams), para)
        (buildInfoEditorP countBuildInfo, paraName <<<- ParaName "Build Info"
                $ paraOuterAlignment <<<- ParaOuterAlignment  (0.0, 0.0, 0.0, 0.0)
                    $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                        $ paraInnerAlignment <<<- ParaInnerAlignment  (0.0, 0.0, 0.0, 0.0)
                            $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                                $ emptyParams)
        (paraDirection  <<<- ParaDirection Vertical $ para)
        noti
    let pinj (Executable' s f bi) = inj ((s,f),bi)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just ((s,f),bi) -> return (Just $Executable' s f bi)
    return (wid,pinj,pext)

buildInfoEditorP :: Int -> Editor Int
buildInfoEditorP numberOfBuildInfos para noti = do
    (wid,inj,ext) <- intEditor (1.0,fromIntegral numberOfBuildInfos,1.0)
        (paraName <<<- ParaName "Build Info" $para) noti
    let pinj i = inj (i + 1)
    let pext =   do
        mbV <- ext
        case mbV of
            Nothing -> return Nothing
            Just i  -> return (Just (i - 1))
    return (wid,pinj,pext)

-- ------------------------------------------------------------
-- * (Boring) default values
-- ------------------------------------------------------------

instance Default Version1
    where getDefault = ThisVersionS

instance Default Version2
    where getDefault = UnionVersionRangesS

instance Default Version
    where getDefault = forceJust (simpleParse "0") "SpecialEditors>>default version"

instance Default VersionRange
    where getDefault = AnyVersion

instance Default CompilerFlavor
    where getDefault =  GHC

instance Default BuildInfo
    where getDefault =  emptyBuildInfo

instance Default Library
    where getDefault =  Library [] getDefault getDefault

instance Default Dependency
    where getDefault = Dependency getDefault getDefault

instance Default Executable
    where getDefault = Executable getDefault getDefault getDefault

instance Default PackageName
    where getDefault = PackageName getDefault




