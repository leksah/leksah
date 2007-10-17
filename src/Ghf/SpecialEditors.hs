--
-- | Special Editors
--

module Ghf.SpecialEditors (
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
import Ghf.PropertyEditor
import Ghf.ViewFrame

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
            Just (n,v) -> do
                if null n
                    then return Nothing
                    else return (Just $PackageIdentifier n v)
    return (wid,pinj,pext,notif)

testedWidthEditor :: Editor [(CompilerFlavor, VersionRange)]
testedWidthEditor para = do
    multisetEditor
       (ColumnDescr False [("Compiler Flavor",\(cv,_) -> [New.cellText := show cv])
                           ,("Version Range",\(_,vr) -> [New.cellText := showVersionRange vr])])
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
    return (wid,vrinj,vrext,notif)
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
    return (wid, pinj, pext, notiRef)

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
dependenciesEditor p =
    multisetEditor
        (ColumnDescr True [("Package",\(Dependency str _) -> [New.cellText := str])
                           ,("Version",\(Dependency _ vers) -> [New.cellText := showVersionRange vers])])
        (dependencyEditor,emptyParams) p{shadow = Just ShadowIn}

filesEditor :: Maybe FilePath -> FileChooserAction -> String -> Editor [FilePath]
filesEditor fp act label p =
    multisetEditor
        (ColumnDescr False [("",(\row -> [New.cellText := row]))])
        (fileEditor fp act label, emptyParams) p{shadow = Just ShadowIn}

stringsEditor :: Editor [String]
stringsEditor p =
    multisetEditor
        (ColumnDescr False [("",(\row -> [New.cellText := row]))])
        (stringEditor, emptyParams) p{shadow = Just ShadowIn}

panePathEditor :: Editor StandardPath
panePathEditor = staticSelectionEditor [LeftTop,LeftBottom,RightTop,RightBottom]

extensionsEditor :: Editor [Extension]
extensionsEditor = staticMultiselectionEditor extensionsL

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
   ,    Arrows
   ,    Generics
   ,    NoImplicitPrelude
   ,    NamedFieldPuns
   ,    PatternGuards
   ,    GeneralizedNewtypeDeriving
   ,    ExtensibleRecords
   ,    RestrictedTypeSynonyms
   ,    HereDocuments]

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






