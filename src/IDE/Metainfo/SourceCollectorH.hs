{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.SourceCollectorH
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Metainfo.SourceCollectorH (
--    collectPackageFromSource
    findSourceForPackage
,   packageFromSource
,   interfaceToModuleDescr
,   PackageCollectStats(..)
) where

import IDE.Core.CTypes
       (getThisPackage, PackageDescr(..), TypeDescr(..), RealDescr(..),
        Descr(..), ModuleDescr(..), PackModule(..), SimpleDescr(..),
        packageIdentifierToString, Location(..), RealDescr(..), PackageIdAndKey(..))

#ifdef MIN_VERSION_haddock_leksah
import Haddock.Types
       (ExportItem(..), DeclInfo,
        Interface(..))
import Haddock.Interface
#else
import Documentation.Haddock
#endif
import Distribution.Text (simpleParse)
#if MIN_VERSION_ghc(7,6,0)
import InstEnv (ClsInst(..))
#else
import InstEnv (Instance(..))
#endif
import MyMissing (forceJust)
import Data.Map (Map)
import qualified Data.Map as Map (empty)

import Data.List (nub, isSuffixOf)
import qualified Data.ByteString.Char8 as BS (pack)
import IDE.Metainfo.WorkspaceCollector
       (srcSpanToLocation, uncommentDecl, uncommentData, printHsDoc, sortByLoc)
import PackageConfig (PackageConfig)
import Distribution.Verbosity (verbose)
#if MIN_VERSION_ghc(7,10,0)
import GHC.PackageDb (exposedModules, hiddenModules, exposedName)
import Documentation.Haddock.Types (_doc)
#else
import qualified Distribution.InstalledPackageInfo as IPI
#endif
import IDE.StrippedPrefs (getUnpackDirectory, Prefs(..))
import IDE.Metainfo.SourceDB (sourceForPackage, getSourcesMap)
import MonadUtils (liftIO)
import System.Directory (setCurrentDirectory, doesDirectoryExist,createDirectory)
import System.FilePath ((<.>), dropFileName, (</>), splitDirectories, dropExtension)
import Data.Maybe(mapMaybe)
import IDE.Utils.GHCUtils (inGhcIO)
import qualified Control.Exception as NewException (SomeException, catch)
import IDE.Utils.Tool
import Control.Monad (unless)
import IDE.Utils.FileUtils (figureOutGhcOpts, myCanonicalizePath, getSysLibDir)
import Distribution.Package(PackageIdentifier)
import GHC hiding(Id,Failed,Succeeded,ModuleName)
import Distribution.ModuleName (components)
import System.Log.Logger (warningM, debugM)
import Control.DeepSeq (deepseq)
import Data.ByteString.Char8 (ByteString)
#if MIN_VERSION_ghc(7,6,0)
import Outputable hiding(trace, (<>))
#else
import Outputable hiding(trace, (<>), showSDoc, showSDocUnqual)
import qualified Outputable as O
#endif
import GHC.Show(showSpace)
import Name
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Data.Monoid ((<>))

type HsDoc a = Doc a

type NDoc  = HsDoc Name

isEmptyDoc :: NDoc -> Bool
isEmptyDoc DocEmpty  = True
isEmptyDoc _         = False

type MyLDocDecl = LDocDecl

#if !MIN_VERSION_ghc(7,6,0)
showSDoc :: DynFlags -> SDoc -> Text
showSDoc _ = O.showSDoc
showSDocUnqual :: DynFlags -> SDoc -> Text
showSDocUnqual _ = O.showSDocUnqual
#endif

show' :: Outputable alpha => DynFlags -> alpha  -> String
show' dflags = showSDoc dflags . ppr

data PackageCollectStats = PackageCollectStats {
    packageString       :: Text,
    modulesTotal        :: Maybe Int,
    withSource          :: Bool,
    retrieved           :: Bool,
    mbError             :: Maybe Text}

findSourceForPackage :: Prefs -> PackageIdentifier -> IO (Either Text FilePath)
findSourceForPackage prefs packageId = do
    sourceMap <- liftIO $ getSourcesMap prefs
    case sourceForPackage packageId sourceMap of
        Just fpSource -> return (Right fpSource)
        Nothing -> do
            unpackDir <- getUnpackDirectory prefs
            case unpackDir of
                Nothing -> return (Left "No source found. Prefs don't allow for retreiving")
                Just fpUnpack -> do
                    exists <- doesDirectoryExist fpUnpack
                    unless exists $ createDirectory fpUnpack
                    setCurrentDirectory fpUnpack
                    runTool' "cabal" ["unpack", packageName] Nothing Nothing
                    success <- doesDirectoryExist (fpUnpack </> packageName')
                    if not success
                        then return (Left "Failed to download and unpack source")
                        else return (Right (fpUnpack </> packageName' </>  takeWhile (/= '-') packageName' <.> "cabal"))
    where
        packageName = packageIdentifierToString packageId
        packageName' = T.unpack packageName


packageFromSource :: FilePath -> PackageConfig -> IO (Maybe PackageDescr, PackageCollectStats)
packageFromSource cabalPath packageConfig = do
    setCurrentDirectory dirPath
    ghcFlags <- figureOutGhcOpts dirPath
    debugM "leksah-server" ("ghcFlags:  " ++ show ghcFlags)
    NewException.catch (inner ghcFlags) handler
    where
        _handler' (_e :: NewException.SomeException) = do
            debugM "leksah-server" "would block"
            return []
        handler (e :: NewException.SomeException) = do
            warningM "leksah-server" ("Ghc failed to process: " ++ show e ++ " (" ++ cabalPath ++ ")")
            return (Nothing, PackageCollectStats packageName Nothing False False
                                            (Just ("Ghc failed to process: " <> T.pack (show e) <> " (" <> T.pack cabalPath <> ")")))
        inner ghcFlags = do
            libDir <- getSysLibDir VERSION_ghc
            inGhcIO libDir ghcFlags [Opt_Haddock] [] $ \ dflags -> do
                (interfaces,_) <- processModules verbose (exportedMods ++ hiddenMods) [] []
                liftIO $ print (length interfaces)
                let mods = map (interfaceToModuleDescr dflags dirPath (packId $ getThisPackage packageConfig)) interfaces
                sp <- liftIO $ myCanonicalizePath dirPath
                let pd = PackageDescr {
                        pdPackage           =   packId (getThisPackage packageConfig)
                    ,   pdModules           =   mods
                    ,   pdBuildDepends      =   [] -- TODO depends packageConfig
                    ,   pdMbSourcePath      =   Just sp}
                let stat = PackageCollectStats packageName (Just (length mods)) True False Nothing
                liftIO $ deepseq pd $ return (Just pd, stat)
#if MIN_VERSION_ghc(7,10,0)
        exportedMods = map (moduleNameString . exposedName) $ exposedModules packageConfig
        hiddenMods   = map moduleNameString $ hiddenModules packageConfig
#else
        exportedMods = map moduleNameString $ IPI.exposedModules packageConfig
        hiddenMods   = map moduleNameString $ IPI.hiddenModules packageConfig
#endif
        dirPath      = dropFileName cabalPath
        packageName  = packageIdentifierToString (packId $ getThisPackage packageConfig)
-- Heaven

interfaceToModuleDescr :: DynFlags -> FilePath -> PackageIdentifier -> Interface -> ModuleDescr
interfaceToModuleDescr dflags _dirPath pid interface =
    ModuleDescr {
        mdModuleId          =   PM pid modName
    ,   mdMbSourcePath      =   Just filepath
    ,   mdReferences        =   imports
    ,   mdIdDescriptions    =   descrs}
    where
        -- ifaceOrigFilename points at the hs output file (not chs file)
        -- So if possible we look up one of the things in the module and
        -- get the file it is located in.
        filepath   = head $
            [locationFile loc | Real RealDescr{dscMbLocation' = Just loc,
                dscMbModu' = Just dscMod} <- descrs, dscMod == PM pid modName,
                filenameMatchesModule (locationFile loc)]
            ++ [ifaceOrigFilename interface]
        modName    = forceJust ((simpleParse . moduleNameString . moduleName . ifaceMod) interface)
                        "Can't parse module name"
        filenameMatchesModule fn = components modName `isSuffixOf` splitDirectories (dropExtension fn)
        descrs     = extractDescrs dflags (PM pid modName)
                        (ifaceDeclMap interface) (ifaceExportItems interface)
                        (ifaceInstances interface) [] --(ifaceLocals interface)
        imports    = Map.empty --TODO

getDoc :: Documentation Name -> Maybe NDoc
#if MIN_VERSION_ghc(7,10,0)
getDoc = fmap _doc . documentationDoc
#else
getDoc = documentationDoc
#endif

#if MIN_VERSION_ghc(7,4,1)
type DeclInfo = [LHsDecl Name]
#endif
#if MIN_VERSION_ghc(7,6,0)
extractDescrs :: DynFlags -> PackModule -> Map Name DeclInfo -> [ExportItem Name] -> [ClsInst] -> [Name] -> [Descr]
#else
extractDescrs :: DynFlags -> PackModule -> Map Name DeclInfo -> [ExportItem Name] -> [Instance] -> [Name] -> [Descr]
#endif
extractDescrs dflags pm _ifaceDeclMap ifaceExportItems' ifaceInstances' _ifaceLocals =
        transformToDescrs dflags pm exportedDeclInfo ++ map (toDescrInst dflags pm) ifaceInstances'
    where
        exportedDeclInfo                    =  mapMaybe toDeclInfo  ifaceExportItems'
        toDeclInfo ExportDecl{expItemDecl=decl, expItemMbDoc=mbDoc, expItemSubDocs=subDocs}   =
                                        Just(decl,getDoc $ fst mbDoc,map (\ (a,b) -> (a,getDoc $ fst b)) subDocs)
        toDeclInfo (ExportNoDecl{})         = Nothing
        toDeclInfo (ExportGroup{})          = Nothing
        toDeclInfo (ExportDoc{})            = Nothing
        toDeclInfo (ExportModule{})         = Nothing

transformToDescrs :: DynFlags -> PackModule -> [(LHsDecl Name, Maybe NDoc, [(Name, Maybe NDoc)])] -> [Descr]
transformToDescrs dflags pm = concatMap transformToDescr
    where
#if MIN_VERSION_ghc(8,0,0)
    transformToDescr (L loc (SigD (TypeSig names typ)), mbComment, _subCommentList) = map nameDescr names
#elif MIN_VERSION_ghc(7,10,0)
    transformToDescr (L loc (SigD (TypeSig names typ _)), mbComment, _subCommentList) = map nameDescr names
#elif MIN_VERSION_ghc(7,2,0)
    transformToDescr (L loc (SigD (TypeSig names typ)), mbComment,_subCommentList) = map nameDescr names
#else
    transformToDescr (L loc (SigD (TypeSig name' typ)), mbComment,_subCommentList) = [nameDescr name']
#endif
      where
        nameDescr name = Real RealDescr {
                dscName'        =   T.pack . getOccString $ unLoc name
            ,   dscMbTypeStr'   =   Just . BS.pack $ getOccString (unLoc name) <> " :: " <> showSDocUnqual dflags (ppr typ)
            ,   dscMbModu'      =   Just pm
            ,   dscMbLocation'  =   srcSpanToLocation loc
            ,   dscMbComment'   =   toComment dflags mbComment []
            ,   dscTypeHint'    =   VariableDescr
            ,   dscExported'    =   True}

#if MIN_VERSION_ghc(7,8,0)
#if MIN_VERSION_ghc(8,0,0)
    transformToDescr (L loc (SigD (PatSynSig name typ)), mbComment, _subCommentList) =
#else
    transformToDescr (L loc (SigD (PatSynSig name _ _ _ typ)), mbComment, _subCommentList) =
#endif
            [Real RealDescr {
            dscName'        =   T.pack . getOccString $ unLoc name
        ,   dscMbTypeStr'   =   Just . BS.pack $ getOccString (unLoc name) <> " :: " <> showSDocUnqual dflags (ppr typ)
        ,   dscMbModu'      =   Just pm
        ,   dscMbLocation'  =   srcSpanToLocation loc
        ,   dscMbComment'   =   toComment dflags mbComment []
        ,   dscTypeHint'    =   PatternSynonymDescr
        ,   dscExported'    =   True}]
#endif

    transformToDescr (L _loc (SigD _), _mbComment, _subCommentList) = []

#if MIN_VERSION_ghc(7,6,0)
    transformToDescr (L loc for@(ForD (ForeignImport lid _ _ _)), mbComment, _sigList) =
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc lid
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr for
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(7,7,0)
    transformToDescr (L loc (TyClD typ@(FamDecl {tcdFam = (FamilyDecl {fdLName = lid})})), mbComment,_sigList) =
#else
    transformToDescr (L loc (TyClD typ@(TyFamily {tcdLName = lid})), mbComment,_sigList) =
#endif
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc lid
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr typ
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]
#endif

#if MIN_VERSION_ghc(7,7,0)
    transformToDescr (L loc (TyClD typ@(SynDecl {tcdLName = lid})), mbComment,_sigList) =
#elif MIN_VERSION_ghc(7,6,0)
    transformToDescr (L loc (TyClD typ@(TyDecl {tcdLName = lid, tcdTyDefn = TySynonym {}})), mbComment,_sigList) =
#else
    transformToDescr (L loc (TyClD typ@(TySynonym lid _ _ _ )), mbComment, _subCommentList) =
#endif
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc lid
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr typ
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(7,7,0)
    transformToDescr (L loc (TyClD typ@(DataDecl {tcdLName = lid, tcdDataDefn = HsDataDefn {dd_cons=lConDecl, dd_derivs=tcdDerivs'}})), mbComment,_sigList) =
#elif MIN_VERSION_ghc(7,6,0)
    transformToDescr (L loc (TyClD typ@(TyDecl {tcdLName = lid, tcdTyDefn = TyData {td_cons=lConDecl, td_derivs=tcdDerivs'}})), mbComment,_sigList) =
#else
    transformToDescr (L loc (TyClD typ@(TyData DataType _ lid _ _ _ lConDecl tcdDerivs')), mbComment,_subCommentList) =
#endif
        Real RealDescr {
        dscName'        =   T.pack name
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags . ppr $ uncommentData typ
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   DataDescr constructors fields
    ,   dscExported'    =   True}
            : derivings tcdDerivs'
        where
        constructors    =   concatMap (extractConstructor dflags) lConDecl
        fields          =   nub $ concatMap (extractRecordFields dflags) lConDecl
        name            =   getOccString (unLoc lid)
        derivings Nothing = []
        derivings (Just _l) = []

#if !MIN_VERSION_ghc(7,6,0)
    transformToDescr ((L loc (TyClD typ@(TyData NewType _ tcdLName' _ _ _ lConDecl tcdDerivs'))), mbComment,_subCommentList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags . ppr $ uncommentData typ
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   NewtypeDescr constructor mbField
    ,   dscExported'    =   True}]
        ++ derivings tcdDerivs'
        where
        constructor     =   forceHead (map (extractConstructor dflags) lConDecl)
                                "WorkspaceCollector>>transformToDescr: no constructor for newtype"
        mbField         =   case concatMap (extractRecordFields dflags) lConDecl of
                                [] -> Nothing
                                a:_ -> Just a
        name            =   getOccString (unLoc tcdLName')
        derivings Nothing = []
        derivings (Just _l) = []
#endif

    transformToDescr (L loc (TyClD cl@(ClassDecl{tcdLName=tcdLName', tcdSigs=tcdSigs', tcdDocs=docs})), mbComment,_subCommentList) =
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc tcdLName'
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr cl{tcdMeths = emptyLHsBinds}
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   ClassDescr super methods
    ,   dscExported'    =   True    }]
        where
        methods         =   extractMethods dflags tcdSigs' docs
        super           =   []

    transformToDescr (_, _mbComment, _sigList) = []

#if MIN_VERSION_ghc(7,6,0)
toDescrInst :: DynFlags -> PackModule -> ClsInst -> Descr
toDescrInst dflags pm inst@(ClsInst {is_cls = is_cls', is_tys = is_tys'}) =
#else
toDescrInst :: DynFlags -> PackModule -> Instance -> Descr
toDescrInst dflags pm inst@(Instance is_cls' _is_tcs _is_tvs is_tys' _is_dfun _is_flag) =
#endif
        Real RealDescr {
        dscName'        =   T.pack $ getOccString is_cls'
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr inst
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation (getSrcSpan inst)
    ,   dscMbComment'   =   Nothing
    ,   dscTypeHint'    =   InstanceDescr (map (T.pack . showSDocUnqual dflags . ppr) is_tys')
    ,   dscExported'    =   True}

extractMethods :: DynFlags -> [LSig Name] -> [MyLDocDecl] -> [SimpleDescr]
extractMethods dflags sigs docs =
    let pairs = attachComments' dflags sigs docs
    in concatMap (extractMethod dflags) pairs

extractMethod :: DynFlags -> (LHsDecl Name, Maybe NDoc) -> [SimpleDescr]
#if MIN_VERSION_ghc(8,0,0)
extractMethod dflags (L loc (SigD ts@(TypeSig names _typ)), mbDoc) = map extractName names
#elif MIN_VERSION_ghc(7,10,0)
extractMethod dflags (L loc (SigD ts@(TypeSig names _typ _)), mbDoc) = map extractName names
#elif MIN_VERSION_ghc(7,2,0)
extractMethod dflags (L loc (SigD ts@(TypeSig names _typ)), mbDoc) = map extractName names
#else
extractMethod dflags (L loc (SigD ts@(TypeSig name' _typ)), mbDoc) = [extractName name']
#endif
  where
  extractName name =
    SimpleDescr
        (T.pack . getOccString $ unLoc name)
        (Just . BS.pack . showSDocUnqual dflags $ ppr ts)
        (srcSpanToLocation loc)
        (toComment dflags mbDoc [])
        True
extractMethod _dflags (_, _mbDoc) = []

extractConstructor :: DynFlags -> LConDecl Name -> [SimpleDescr]
#if MIN_VERSION_ghc(8,0,0)
extractConstructor dflags decl@(L loc d) = extractDecl d
 where
  extractDecl (ConDeclGADT {..}) = map (extractName con_doc) con_names
  extractDecl (ConDeclH98 {..}) = [extractName con_doc con_name]
#elif MIN_VERSION_ghc(7,10,0)
extractConstructor dflags decl@(L loc (ConDecl {con_names = names, con_doc = doc})) =
  map (extractName doc) names
  where
#else
extractConstructor dflags decl@(L loc (ConDecl {con_name = name', con_doc = doc})) =
  [extractName doc name']
  where
#endif
  extractName doc name =
    SimpleDescr
        (T.pack . getOccString $ unLoc name)
        (Just . BS.pack . showSDocUnqual dflags . ppr $uncommentDecl decl)
        (srcSpanToLocation loc)
        (case doc of
            Nothing -> Nothing
            Just (L _ d) -> Just . BS.pack . T.unpack $ printHsDoc d)
        True

#if MIN_VERSION_ghc(7,10,0)
unLoc710 :: GenLocated l e -> e
unLoc710 = unLoc
#else
unLoc710 :: a -> a
unLoc710 = id
#endif

extractRecordFields :: DynFlags -> LConDecl Name -> [SimpleDescr]
#if MIN_VERSION_ghc(8,0,0)
extractRecordFields dflags (L _ _decl@ConDeclH98 {con_details = RecCon flds}) =
#else
extractRecordFields dflags (L _ _decl@(ConDecl {con_details=(RecCon flds)})) =
#endif
    concatMap extractRecordFields' (unLoc710 flds)
    where
#if MIN_VERSION_ghc(7,10,0)
    extractRecordFields' :: LConDeclField Name -> [SimpleDescr]
    extractRecordFields' (L _ _field@(ConDeclField names typ doc)) = map extractName names
#else
    extractRecordFields' :: ConDeclField Name -> [SimpleDescr]
    extractRecordFields' _field@(ConDeclField name' typ doc) = [extractName name']
#endif
      where
      extractName name =
        SimpleDescr
            (T.pack . showSDoc dflags . ppr $ unLoc710 name)
            (Just (BS.pack (showSDocUnqual dflags $ ppr typ)))
            (srcSpanToLocation $ getLoc name)
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just . BS.pack . T.unpack $ printHsDoc d)
            True
extractRecordFields _ _ = []

toComment :: DynFlags -> Maybe NDoc -> [NDoc] -> Maybe ByteString
toComment dflags (Just c) _    =  Just . BS.pack . T.unpack $ printHsDoc' dflags c
toComment dflags Nothing (c:_) =  Just . BS.pack . T.unpack $ printHsDoc' dflags c
toComment _ Nothing []         =  Nothing


{--
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Data] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TyFamily _ lid _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (ClassDecl _ lid _ _ _ _ _ _ )))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Class] []
--}

printHsDoc' :: DynFlags -> HsDoc Name  -> Text
printHsDoc' dflags d = T.pack . show $ PPDoc dflags d

data PPDoc alpha = PPDoc DynFlags (HsDoc alpha)

instance Outputable alpha => Show (PPDoc alpha)  where
    showsPrec _ (PPDoc _ DocEmpty)                 =   id
    showsPrec _ (PPDoc d (DocAppend l r))          =   shows (PPDoc d l)  . shows (PPDoc d r)
    showsPrec _ (PPDoc _ (DocString str))          =   showString str
    showsPrec _ (PPDoc d (DocParagraph doc))       =   shows (PPDoc d doc) . showChar '\n'
    showsPrec _ (PPDoc d (DocIdentifier l))        =   foldr (\i _f -> showChar '\'' .
                                                     (showString . showSDoc d . ppr) i . showChar '\'') id [l]
    showsPrec _ (PPDoc _ (DocModule str))          =   showChar '"' . showString str . showChar '"'
    showsPrec _ (PPDoc d (DocEmphasis doc))        =   showChar '/' . shows (PPDoc d doc)  . showChar '/'
    showsPrec _ (PPDoc d (DocMonospaced doc))      =   showChar '@' . shows (PPDoc d doc) . showChar '@'
    showsPrec _ (PPDoc d (DocUnorderedList l))     =
        foldr (\s r -> showString "* " . shows (PPDoc d s) . showChar '\n' . r) id l
    showsPrec _ (PPDoc d (DocOrderedList l))       =
        foldr (\(i,n) _f -> shows n . showSpace .  shows (PPDoc d i)) id (zip l [1 .. length l])
    showsPrec _ (PPDoc d (DocDefList li))          =
        foldr (\(l,r) f -> showString "[@" . shows (PPDoc d l) . showString "[@ " . shows (PPDoc d r) . f) id li
    showsPrec _ (PPDoc d (DocCodeBlock doc))      =   showChar '@' . shows (PPDoc d doc) . showChar '@'
#if MIN_VERSION_ghc(7,6,0)
    showsPrec _ (PPDoc _ (DocHyperlink h))            =   showChar '<' . showString (show h) . showChar '>'
#else
    showsPrec _ (PPDoc _ (DocURL str))            =   showChar '<' . showString str . showChar '>'
#endif
    showsPrec _ (PPDoc _ (DocAName str))          =   showChar '#' . showString str . showChar '#'
    showsPrec _ (PPDoc _ _)                       =   id

attachComments' :: DynFlags -> [LSig Name] -> [MyLDocDecl] -> [(LHsDecl Name, Maybe (HsDoc Name))]
attachComments' dflags sigs docs = collectDocs' dflags $ sortByLoc
                                                           (map (\ (L l i) -> L l (SigD i)) sigs ++
                                                              map (\ (L l i) -> L l (DocD i)) docs)

-- | Collect the docs and attach them to the right declaration.
collectDocs' :: DynFlags -> [LHsDecl Name] -> [(LHsDecl Name, Maybe (HsDoc Name))]
collectDocs' dflags = collect' dflags Nothing DocEmpty

collect' :: DynFlags -> Maybe (LHsDecl Name) -> HsDoc Name -> [LHsDecl Name] -> [(LHsDecl Name, Maybe (HsDoc Name))]
collect' _dflags d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc' d0 doc_so_far []

collect' dflags d doc_so_far (e:es) =
  case e of
    L _ (DocD (DocCommentNext str)) ->
      case d of
        Nothing -> collect' dflags d (DocAppend doc_so_far (DocString (show' dflags str))) es
        Just d0 -> finishedDoc' d0 doc_so_far (collect' dflags Nothing (DocString (show' dflags str)) es)

    L _ (DocD (DocCommentPrev str)) -> collect' dflags d (DocAppend doc_so_far (DocString (show' dflags str))) es

    _ -> case d of
      Nothing -> collect' dflags (Just e) doc_so_far es
      Just d0 -> finishedDoc' d0 doc_so_far (collect' dflags (Just e) DocEmpty es)

finishedDoc' :: LHsDecl alpha -> NDoc -> [(LHsDecl alpha, Maybe (HsDoc Name))]
                    -> [(LHsDecl alpha, Maybe (HsDoc Name))]
finishedDoc' d doc rest | isEmptyDoc doc = (d, Nothing) : rest
finishedDoc' d doc rest | notDocDecl d   = (d, Just doc) : rest
  where
    notDocDecl (L _ (DocD _)) = False
    notDocDecl _              = True
finishedDoc' _ _ rest = rest
