{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, PatternGuards, OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.WorkspaceCollector
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

module IDE.Metainfo.WorkspaceCollector (

    collectWorkspace

,   sortByLoc
,   attachComments
,   uncommentData
,   uncommentDecl
,   printHsDoc
,   toComment
,   srcSpanToLocation
,   sigToByteString

) where


import Control.Applicative hiding(empty)
import Prelude
import IDE.Utils.Utils
import IDE.Utils.GHCUtils
#if MIN_VERSION_ghc(7,10,0)
import GHC hiding(Id,Failed,Succeeded,ModuleName,PackageKey)
#else
import GHC hiding(Id,Failed,Succeeded,ModuleName)
import PackageConfig (mkPackageId)
#endif
#if !MIN_VERSION_ghc(7,2,0)
import HscTypes hiding (liftIO)
#endif
#if MIN_VERSION_ghc(7,6,0)
import Outputable hiding(trace, (<>))
#else
import Outputable hiding(trace, (<>), showSDoc, showSDocUnqual)
import qualified Outputable as O
#endif
import ErrUtils
import qualified Data.Map as Map
import Data.Map(Map)
import System.Directory
import Distribution.Package hiding (PackageId)
import Distribution.ModuleName
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Data.Binary.Shared
import IDE.Utils.FileUtils
import IDE.Core.Serializable ()
import IDE.Core.CTypes hiding (SrcSpan(..))
import Data.ByteString.Char8 (ByteString)
import DriverPipeline (preprocess)
import StringBuffer(hGetStringBuffer)
import Data.List(partition,sortBy,nub,find)
import Data.Ord(comparing)
import GHC.Exception
#if !MIN_VERSION_ghc(7,6,0)
import MyMissing(forceHead)
#endif
import LoadIface(findAndReadIface)
import Distribution.Text(display)
import TcRnMonad (initTcRnIf, IfGblEnv(..))
import qualified Maybes as M
import IDE.Metainfo.InterfaceCollector
import Data.Maybe
       (isJust, catMaybes, isNothing)
import PrelNames
import System.Log.Logger
import Control.DeepSeq (deepseq)
import FastString(mkFastString,appendFS,nullFS,unpackFS)
import Control.Monad.IO.Class (MonadIO, MonadIO(..))
import Control.Monad (when)
import Control.Exception as E
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))

type NDecl = LHsDecl RdrName
myDocEmpty :: NDoc
myDocAppend :: NDoc -> NDoc -> NDoc
isEmptyDoc :: NDoc -> Bool

type NDoc  = HsDocString
type MyLDocDecl = LDocDecl

myDocEmpty=HsDocString(mkFastString "")
myDocAppend (HsDocString l) (HsDocString r) = HsDocString (appendFS l r)
isEmptyDoc (HsDocString fs) = nullFS fs

type NSig  = Located (Sig RdrName)

#if !MIN_VERSION_ghc(7,6,0)
showSDoc :: DynFlags -> SDoc -> Text
showSDoc _ = O.showSDoc
showSDocUnqual :: DynFlags -> SDoc -> Text
showSDocUnqual _ = O.showSDocUnqual
#endif

showRdrName :: DynFlags -> RdrName -> Text
showRdrName dflags r = T.pack . showSDoc dflags $ ppr r

collectWorkspace :: PackageIdentifier -> [(Text,FilePath)] -> Bool -> Bool -> FilePath -> IO()
collectWorkspace pid moduleList forceRebuild writeAscii dir = do
    debugM "leksah-server" $ "collectWorkspace called with modules " ++ show moduleList ++ " in folder " ++ dir
    collectorPath <- liftIO getCollectorPath
    let packageCollectorPath = collectorPath </> T.unpack (packageIdentifierToString pid)
    when forceRebuild $ do
        exists <- doesDirectoryExist packageCollectorPath
        when exists $ removeDirectoryRecursive packageCollectorPath
    -- Construct directory
    liftIO $ createDirectoryIfMissing True packageCollectorPath
    setCurrentDirectory dir
    opts1 <- filterOpts <$> figureOutGhcOpts dir
    opts2 <- figureOutHaddockOpts

    libDir <- getSysLibDir VERSION_ghc
    debugM "leksah-server" $ "before collect modules" ++ "\n\nopts1: " ++ show opts1 ++ "\n\n opt2: " ++ show opts2
    mapM_ (collectModule libDir packageCollectorPath writeAscii pid opts1) moduleList
    debugM "leksah-server" "after collect modules"
  where
    filterOpts :: [Text] -> [Text]
    filterOpts []    = []
    filterOpts (o:_:r) | o `elem` ["-link-js-lib", "-js-lib-outputdir", "-js-lib-src", "-package-id"] = filterOpts r
    filterOpts (o:r) = o:filterOpts r

collectModule :: FilePath -> FilePath -> Bool -> PackageIdentifier -> [Text] -> (Text,FilePath) -> IO()
collectModule libDir collectorPackagePath writeAscii pid opts (modId,sourcePath) = do
    case parseModuleKey (T.unpack modId) sourcePath of
        Nothing -> errorM "leksah-server" (T.unpack $ "Can't parse module name " <> modId)
        Just modKey -> do
            let collectorModulePath = collectorPackagePath </> (moduleCollectorFileName modKey) <.> leksahMetadataWorkspaceFileExtension
                moduleName' = moduleKeyToName modKey
            existCollectorFile <- doesFileExist collectorModulePath
            existSourceFile    <- doesFileExist sourcePath
            if existSourceFile
                then do
                    if not existCollectorFile
                        then collectModule' libDir sourcePath collectorModulePath writeAscii pid opts moduleName'
                        else do
                            sourceModTime <-  getModificationTime sourcePath
                            collModTime   <-  getModificationTime collectorModulePath
                            if sourceModTime > collModTime
                                then collectModule' libDir sourcePath collectorModulePath writeAscii pid
                                        opts moduleName'
                                else return ()
                else errorM "leksah-server" ("source file not found " ++ sourcePath)


collectModule' :: FilePath -> FilePath -> FilePath -> Bool -> PackageIdentifier -> [Text] -> ModuleName -> IO()
collectModule' libDir sourcePath destPath writeAscii pid opts moduleName' = gcatch (
   inGhcIO libDir (opts++["-cpp"]) [Opt_Haddock] [] $ \ dynFlags -> do
        session         <-  getSession
#if MIN_VERSION_ghc(7,2,0)
        (dynFlags3,fp') <-  liftIO $ preprocess session (sourcePath,Nothing)
#else
        (dynFlags3,fp') <-  preprocess session (sourcePath,Nothing)
#endif
        let packIdAndKey = PackageIdAndKey pid
#if MIN_VERSION_ghc(7,10,0)
                                (thisPackage dynFlags3)
#endif
        mbInterfaceDescr <- mayGetInterfaceDescription dynFlags packIdAndKey moduleName'
        liftIO $ do
            stringBuffer    <-  hGetStringBuffer fp'
            parseResult     <-  myParseModule dynFlags3 sourcePath (Just stringBuffer)
            case parseResult of
                Right (L _ hsMod@(HsModule{})) -> do
                    let moduleDescr = extractModDescr dynFlags pid moduleName' sourcePath hsMod
                    let moduleDescr' = case mbInterfaceDescr of
                                            Nothing -> moduleDescr
                                            Just md  -> mergeWithInterfaceDescr moduleDescr md
                    E.catch (writeExtractedModule destPath writeAscii moduleDescr')
                        (\ (_:: IOException) -> errorM "leksah-server" ("Can't write extracted package " ++ destPath))
                Left errMsg -> do
                    errorM "leksah-server" $ "Failed to parse " ++ sourcePath ++ " " ++ show errMsg
                    let moduleDescr =  ModuleDescr {
                        mdModuleId          =   PM pid moduleName'
                    ,   mdMbSourcePath      =   Just sourcePath
                    ,   mdReferences        =   Map.empty -- imports
                    ,   mdIdDescriptions    =   [Real $ RealDescr {
                            dscName'        =   "Parse Error"
                        ,   dscMbTypeStr'   =   Nothing
                        ,   dscMbModu'      =   Just (PM pid moduleName')
#if MIN_VERSION_ghc(7,7,0)
                        ,   dscMbLocation'  =   srcSpanToLocation $ errMsgSpan errMsg
#else
                        ,   dscMbLocation'  =   case errMsgSpans errMsg of
                                                    (sp:_) -> srcSpanToLocation sp
                                                    [] -> Nothing
#endif
                        ,   dscMbComment'   =   Just (BS.pack $ show errMsg)
                        ,   dscTypeHint'    =   ErrorDescr
                        ,   dscExported'    =   False}]}
                    E.catch (deepseq moduleDescr $ writeExtractedModule destPath writeAscii moduleDescr)
                        (\ (_:: IOException) -> errorM "leksah-server" ("Can't write extracted module " ++ destPath))
    ) (\ (e :: SomeException) -> errorM "leksah-server" ("Can't extract module " ++ destPath ++ " " ++ show e))


writeExtractedModule :: MonadIO m => FilePath -> Bool -> ModuleDescr -> m ()
writeExtractedModule filePath writeAscii md =
    if writeAscii
        then liftIO $ writeFile (filePath ++ "dpg") (show md)
        else liftIO $ encodeFileSer filePath (metadataVersion, md)

-----------------------------------------------------------------------------------
-- Format conversion
#if MIN_VERSION_ghc(7,10,0)
unLoc710 :: GenLocated l e -> e
unLoc710 = unLoc
#else
unLoc710 :: a -> a
unLoc710 = id
#endif

extractModDescr :: DynFlags -> PackageIdentifier -> ModuleName -> FilePath -> HsModule RdrName -> ModuleDescr
extractModDescr dflags pid moduleName' sourcePath hsMod = ModuleDescr {
        mdModuleId          =   PM pid moduleName'
    ,   mdMbSourcePath      =   modFile $ hsmodName hsMod
    ,   mdReferences        =   Map.empty -- imports
    ,   mdIdDescriptions    =   descrs'}
    where
        descrs = extractDescrs dflags (PM pid moduleName') (hsmodDecls hsMod)
        descrs' = fixExports dflags (fmap unLoc710 $ hsmodExports hsMod) descrs
        modFile (Just (L loc _)) =
            (locationFile <$> srcSpanToLocation loc) <|> Just sourcePath
        modFile _ = Just sourcePath

-----------------------------------------------------------------------------------
-- Add exported hint

fixExports :: DynFlags -> Maybe [LIE RdrName] -> [Descr] -> [Descr]
fixExports _ Nothing descrs = descrs
fixExports dflags (Just iel) descrs = map (fixDescr (map unLoc iel)) descrs
    where
        fixDescr ::  [IE RdrName] -> Descr -> Descr
        fixDescr _ d@(Reexported _) = d
        fixDescr list (Real rd) = Real rd'
            where
                rd' = case dscTypeHint' rd of
                          VariableDescr       -> rd{dscExported' = isJust findVar}
                          PatternSynonymDescr -> rd{dscExported' = isJust findVar}
                          InstanceDescr _     -> rd
                          _                   -> case findThing of
                                                    Nothing                -> nothingExported rd
                                                    Just (IEThingAll _)    -> allExported rd
                                                    Just (IEThingAbs _)    -> someExported rd []
#if MIN_VERSION_ghc(8,0,0)
                                                    Just (IEThingWith _ _ l _) -> someExported rd (map (showRdrName dflags . unLoc710) l)
#else
                                                    Just (IEThingWith _ l) -> someExported rd (map (showRdrName dflags . unLoc710) l)
#endif
                                                    _                      -> allExported rd
                findVar = find (\ a ->
                            case a of
                                IEVar r | showRdrName dflags (unLoc710 r) == dscName' rd -> True
                                _                                     -> False)
                                    list
                findThing = find (\ a ->
                                case a of
                                IEThingAbs r | showRdrName dflags (unLoc710 r) == dscName' rd -> True
                                IEThingAll r | showRdrName dflags (unLoc710 r) == dscName' rd -> True
#if MIN_VERSION_ghc(8,0,0)
                                IEThingWith r _ _list _ | showRdrName dflags (unLoc710 r) == dscName' rd -> True
#else
                                IEThingWith r _list | showRdrName dflags (unLoc710 r) == dscName' rd -> True
#endif
                                _                                     -> False)
                                    list
        allExported rd                                 = rd
        nothingExported rd                             = rd{dscExported' = False,
                                                             dscTypeHint' = nothingExportedS (dscTypeHint' rd)}
        nothingExportedS (DataDescr lsd1 lsd2)         = DataDescr (map (setExportedSD False) lsd1)
                                                            (map (setExportedSD False) lsd2)
        nothingExportedS (NewtypeDescr sd1 Nothing)    = NewtypeDescr (setExportedSD False sd1)
                                                            Nothing
        nothingExportedS (NewtypeDescr sd1 (Just _sd2)) = NewtypeDescr (setExportedSD False sd1)
                                                            (Just (setExportedSD False sd1))
        nothingExportedS (ClassDescr n lsd2)           = ClassDescr n (map (setExportedSD False) lsd2)
        nothingExportedS other                         = other

        someExported rd l                              = rd{dscExported' = True,
                                                            dscTypeHint' = someExportedS (dscTypeHint' rd) l}
        someExportedS (DataDescr lsd1 lsd2) l          = DataDescr (map (maySetExportedSD l) lsd1)
                                                            (map (maySetExportedSD l) lsd2)
        someExportedS (NewtypeDescr sd1 Nothing) l     = NewtypeDescr (maySetExportedSD l sd1)
                                                            Nothing
        someExportedS (NewtypeDescr sd1 (Just _sd2)) l  = NewtypeDescr (maySetExportedSD l sd1)
                                                            (Just (maySetExportedSD l sd1))
        someExportedS (ClassDescr n lsd2) l            = ClassDescr n (map (maySetExportedSD l) lsd2)
        someExportedS other _                          = other


        setExportedSD bool sd = sd{sdExported = bool}
        maySetExportedSD list sd = sd{sdExported = elem (sdName sd) list}


extractDescrs :: DynFlags -> PackModule -> [NDecl] -> [Descr]
extractDescrs dflags pm decls = transformToDescrs dflags pm tripleWithSigs
    where
        sortedDecls                    = sortByLoc decls
        pairedWithDocs                 = collectDocs sortedDecls
        filteredDecls                  = filterUninteresting pairedWithDocs
        (withoutSignatures,signatures) = partitionSignatures filteredDecls
        tripleWithSigs                 = attachSignatures dflags signatures withoutSignatures

-- | Sort by source location
sortByLoc :: [Located a] -> [Located a]
sortByLoc = sortBy (comparing getLoc)

filterUninteresting :: [(NDecl,Maybe NDoc)] -> [(NDecl,Maybe NDoc)]
filterUninteresting = filter filterSignature
    where
    filterSignature ((L _srcDecl (SpliceD _)),_)  = False
    filterSignature ((L _srcDecl (RuleD _)),_)    = False
    filterSignature ((L _srcDecl (WarningD _)),_) = False
    filterSignature ((L _srcDecl (ForD _)),_)     = False
    filterSignature ((L _srcDecl (DefD _)),_)     = False
    filterSignature _                            = True

partitionSignatures :: [(NDecl,Maybe NDoc)] -> ([(NDecl,Maybe NDoc)],[(NDecl,Maybe NDoc)])
partitionSignatures = partition filterSignature
    where
    filterSignature ((L _srcDecl (SigD _)),_) = False
    filterSignature _ = True

--partitionInstances :: [(NDecl,Maybe NDoc)] -> ([(NDecl,Maybe NDoc)],[(NDecl,Maybe NDoc)])
--partitionInstances i = (i,[])
--partition filterInstances
--    where
--    filterInstances ((L srcDecl (InstD _)),_) = False
--    filterInstances _ = True

-- | Collect the docs and attach them to the right declaration.
collectDocs :: [LHsDecl RdrName] -> [(LHsDecl RdrName, (Maybe NDoc))]
collectDocs = collect Nothing myDocEmpty

collect :: Maybe (LHsDecl RdrName) -> NDoc -> [LHsDecl RdrName] -> [(LHsDecl RdrName, (Maybe (NDoc)))]
collect d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc d0 doc_so_far []

collect d doc_so_far (e:es) =
  case e of
    L _ (DocD (DocCommentNext str)) ->
      case d of
        Nothing -> collect d (myDocAppend doc_so_far str) es
        Just d0 -> finishedDoc d0 doc_so_far (collect Nothing str es)

    L _ (DocD (DocCommentPrev str)) -> collect d (myDocAppend doc_so_far str) es

    _ -> case d of
      Nothing -> collect (Just e) doc_so_far es
      Just d0 -> finishedDoc d0 doc_so_far (collect (Just e) myDocEmpty es)

finishedDoc :: LHsDecl RdrName -> NDoc -> [(LHsDecl RdrName, (Maybe NDoc))] -> [(LHsDecl RdrName, (Maybe NDoc))]
finishedDoc d doc rest | isEmptyDoc doc = (d, Nothing) : rest
finishedDoc d doc rest | notDocDecl d = (d, Just doc) : rest
  where
    notDocDecl (L _ (DocD _)) = False
    notDocDecl _              = True
finishedDoc _ _ rest = rest

sigNameNoLoc' :: Sig name -> [name]
#if MIN_VERSION_ghc(7,2,0)
#if MIN_VERSION_ghc(8,0,0)
sigNameNoLoc' (TypeSig   ns _)          = map unLoc ns
#elif MIN_VERSION_ghc(7,10,0)
sigNameNoLoc' (TypeSig   ns _ _)        = map unLoc ns
#else
sigNameNoLoc' (TypeSig   ns _)          = map unLoc ns
#endif
sigNameNoLoc' (SpecSig   n _ _)         = [unLoc n]
sigNameNoLoc' (InlineSig n _)           = [unLoc n]
#if MIN_VERSION_ghc(7,10,0)
sigNameNoLoc' (FixSig (FixitySig ns _)) = map unLoc ns
#else
sigNameNoLoc' (FixSig (FixitySig n _)) = [unLoc n]
#endif
sigNameNoLoc' _                         = []
#else
sigNameNoLoc' = maybe [] (:[]) . sigNameNoLoc
#endif

attachSignatures :: DynFlags -> [(NDecl, (Maybe NDoc))] -> [(NDecl,Maybe NDoc)]
    -> [(NDecl, (Maybe NDoc), [(NSig,Maybe NDoc)])]
attachSignatures dflags signatures = map (attachSignature signaturesMap)
    where
    signaturesMap = Map.fromListWith (++) $ concatMap sigMap signatures

    sigMap (L loc (SigD sig),c) | nameList <- sigNameNoLoc' sig =
        map (\n -> (n, [(L loc sig,c)])) nameList
    sigMap v = error ("Unexpected location type" ++ (showSDoc dflags . ppr) v)

    attachSignature :: Map RdrName  [(NSig,Maybe NDoc)] -> (NDecl, (Maybe NDoc))
        -> (NDecl, (Maybe NDoc), [(NSig,Maybe NDoc)])
    attachSignature signaturesMap'  (decl,mbDoc) =
        case declName (unLoc decl) of
            Nothing -> (decl,mbDoc, [])
            Just name -> case name `Map.lookup` signaturesMap' of
                            Just sigList -> (decl,mbDoc, sigList)
                            Nothing ->  (decl, mbDoc, [])
    declName _t@(TyClD x)                           = Just (tcdName x)
    declName _t@(ValD (FunBind {fun_id = fun_id'} )) = Just (unLoc fun_id')
    declName _                                      = Nothing


transformToDescrs :: DynFlags -> PackModule -> [(NDecl, (Maybe NDoc), [(NSig, Maybe NDoc)])] -> [Descr]
transformToDescrs dflags pm = concatMap transformToDescr
    where
    transformToDescr :: (NDecl, (Maybe NDoc), [(NSig, Maybe NDoc)]) -> [Descr]
    transformToDescr ((L loc (ValD (FunBind {fun_id = lid}))), mbComment,sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   sigToByteString dflags sigList
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment (catMaybes (map snd sigList))
    ,   dscTypeHint'    =   VariableDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(7,8,0)
#if MIN_VERSION_ghc(7,10,0)
    transformToDescr ((L loc (ValD (PatSynBind PSB{..}))), mbComment,sigList) =
#else
    transformToDescr ((L loc (ValD (PatSynBind{..}))), mbComment,sigList) =
#endif
        [Real $ RealDescr {
#if MIN_VERSION_ghc(7,10,0)
        dscName'        =   showRdrName dflags (unLoc psb_id)
#else
        dscName'        =   showRdrName dflags (unLoc patsyn_id)
#endif
    ,   dscMbTypeStr'   =   sigToByteString dflags sigList
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment (catMaybes (map snd sigList))
    ,   dscTypeHint'    =   PatternSynonymDescr
    ,   dscExported'    =   True}]
#endif

#if MIN_VERSION_ghc(7,7,0)
    transformToDescr ((L loc for@(ForD (ForeignImport lid _ _ _))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr for))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(FamDecl {tcdFam = (FamilyDecl{ fdLName = lid})}))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(SynDecl {tcdLName = lid}))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(DataDecl {tcdLName = lid, tcdDataDefn = HsDataDefn {dd_cons=lConDecl, dd_derivs=tcdDerivs'}}))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr (uncommentData typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   DataDescr constructors fields
    ,   dscExported'    =   True}]
            ++ derivings tcdDerivs'
        where
        constructors    =   concatMap (extractConstructor dflags) lConDecl
        fields          =   nub $ concatMap (extractRecordFields dflags) lConDecl
        name            =   showRdrName dflags (unLoc lid)
        derivings Nothing = []
        derivings (Just l) = map (extractDeriving dflags pm name) (unLoc710 l)
#elif MIN_VERSION_ghc(7,6,0)
    transformToDescr ((L loc (TyClD typ@(ForeignType {tcdLName = lid}))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(TyFamily {tcdLName = lid}))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(TyDecl {tcdLName = lid, tcdTyDefn = TySynonym {}}))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(TyDecl {tcdLName = lid, tcdTyDefn = TyData {td_cons=lConDecl, td_derivs=tcdDerivs'}}))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr (uncommentData typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   DataDescr constructors fields
    ,   dscExported'    =   True}]
            ++ derivings tcdDerivs'
        where
        constructors    =   concatMap (extractConstructor dflags) lConDecl
        fields          =   nub $ concatMap (extractRecordFields dflags) lConDecl
        name            =   showRdrName dflags (unLoc lid)
        derivings Nothing = []
        derivings (Just l) = map (extractDeriving dflags pm name) l
#else
    transformToDescr ((L loc (TyClD typ@(TySynonym lid _ _ _ ))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(TyData DataType _ tcdLName' _ _ _ lConDecl tcdDerivs'))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr (uncommentData typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   DataDescr constructors fields
    ,   dscExported'    =   True}]
            ++ derivings tcdDerivs'
        where
        constructors    =   map (extractConstructor dflags) lConDecl
        fields          =   nub $ concatMap (extractRecordFields dflags) lConDecl
        name            =   showRdrName dflags (unLoc tcdLName')
        derivings Nothing = []
        derivings (Just l) = map (extractDeriving dflags pm name) l

    transformToDescr ((L loc (TyClD typ@(TyData NewType _ tcdLName' _ _ _ lConDecl tcdDerivs'))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr (uncommentData typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   NewtypeDescr constructor mbField
    ,   dscExported'    =   True}]
            ++ derivings tcdDerivs'
        where
        constructor     =   forceHead (map (extractConstructor dflags) lConDecl)
                                "WorkspaceCollector>>transformToDescr: no constructor for newtype"
        mbField         =   case concatMap (extractRecordFields dflags) lConDecl of
                                [] -> Nothing
                                a:_ -> Just a
        name            =   showRdrName dflags (unLoc tcdLName')
        derivings Nothing = []
        derivings (Just l) = map (extractDeriving dflags pm name) l
#endif

    transformToDescr ((L loc (TyClD cl@(ClassDecl{tcdLName=tcdLName', tcdSigs=tcdSigs', tcdDocs=docs}))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName dflags (unLoc tcdLName')
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr cl{tcdMeths = emptyLHsBinds}))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   ClassDescr super methods
    ,   dscExported'    =   True    }]
        where
        methods         =   extractMethods dflags tcdSigs' docs
        super           =   []

#if MIN_VERSION_ghc(7,7,0)
    transformToDescr ((L loc (InstD inst)), mbComment, _sigList) =
        let typp = case inst of
                     ClsInstD t -> ppr t
                     DataFamInstD t -> ppr t
                     TyFamInstD t -> ppr t
            (instn,nameI,other) =   case T.words . T.pack $ showSDocUnqual dflags typp of
                                        instn':nameI':tl -> (instn',nameI',takeWhile (/= "where") tl)
                                        _ -> ("","",[])
        in
            [Real $ RealDescr {
            dscName'        =   instn <> " " <> nameI
        ,   dscMbTypeStr'   =   Just (BS.pack . T.unpack $ instn <> " " <> nameI <> " " <> (T.intercalate " " other))
        ,   dscMbModu'      =   Just pm
        ,   dscMbLocation'  =   srcSpanToLocation loc
        ,   dscMbComment'   =   toComment mbComment []
        ,   dscTypeHint'    =   InstanceDescr other
        ,   dscExported'    =   True}]
            where

#elif MIN_VERSION_ghc(7,6,0)
    transformToDescr ((L loc (InstD _inst@(ClsInstD typ _ _ _))), mbComment, _sigList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack ("instance " ++ (showSDocUnqual dflags $ppr typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   InstanceDescr other
    ,   dscExported'    =   True}]
        where
        (name,other)           =   case T.words $ T.pack (showSDocUnqual dflags $ppr typ) of
                                [] -> ("",[])
                                hd:tl -> (hd,tl)
#else
    transformToDescr ((L loc (InstD _inst@(InstDecl typ _ _ _))), mbComment, _sigList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack ("instance " ++ (showSDocUnqual dflags $ppr typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   InstanceDescr other
    ,   dscExported'    =   True}]
        where
        (name,other)           =   case T.words $ T.pack (showSDocUnqual dflags $ppr typ) of
                                [] -> ("",[])
                                hd:tl -> (hd,tl)
#endif

    transformToDescr (_, _mbComment, _sigList) = []


uncommentData :: TyClDecl a -> TyClDecl a
#if MIN_VERSION_ghc(7,7,0)
uncommentData td@(DataDecl {tcdDataDefn = def@(HsDataDefn{dd_cons = conDecls})}) = td{
    tcdDataDefn = def{dd_cons = map uncommentDecl conDecls}}
#elif MIN_VERSION_ghc(7,6,0)
uncommentData td@(TyDecl {tcdTyDefn = def@(TyData{td_cons = conDecls})}) = td{
    tcdTyDefn = def{td_cons = map uncommentDecl conDecls}}
#else
uncommentData td@(TyData {tcdCons = conDecls}) = td{tcdCons = map uncommentDecl conDecls}
#endif
uncommentData other                            = other

uncommentDecl :: LConDecl a -> LConDecl a
#if MIN_VERSION_ghc(8,0,1)
uncommentDecl (L l cd@ConDeclGADT{}) =
    L l cd{con_doc = Nothing}
uncommentDecl (L l cd@ConDeclH98{}) =
    L l cd{con_details= uncommentDetails (con_details cd), con_doc = Nothing}
#else
uncommentDecl (L l cd) =
    L l cd{con_details= uncommentDetails (con_details cd)}
#endif

uncommentDetails :: HsConDeclDetails a -> HsConDeclDetails a
#if MIN_VERSION_ghc(7,10,0)
uncommentDetails (RecCon (L l flds)) = RecCon (L l (map uncommentField flds))
    where
    uncommentField (L l2 (ConDeclField a1 a2 _doc))  =  L l2 (ConDeclField a1 a2 Nothing)
#else
uncommentDetails (RecCon flds) = RecCon (map uncommentField flds)
    where
    uncommentField (ConDeclField a1 a2 _doc)  =  ConDeclField a1 a2 Nothing
#endif
uncommentDetails other = other

mergeWithInterfaceDescr :: ModuleDescr -> ModuleDescr -> ModuleDescr
mergeWithInterfaceDescr md imd = md {
    mdReferences = mdReferences imd,
    mdIdDescriptions = mergeIdDescrs (mdIdDescriptions md) (mdIdDescriptions imd)}

mergeIdDescrs :: [Descr] -> [Descr] -> [Descr]
mergeIdDescrs d1 d2 = dres ++ reexported
    where
        (reexported,real)  = partition isReexported d2
        lm = Map.fromList $ zip (map (\d -> (dscName d,dscTypeHint d)) real) real
        dres =  map (addType lm) d1

        addType lm' (Real d1') | isNothing (dscMbTypeStr' d1') =
            Real $ d1'{dscMbTypeStr' = case (dscName' d1', dscTypeHint' d1') `Map.lookup` lm' of
                                        Nothing -> Nothing
                                        Just d -> dscMbTypeStr d}
        addType _ d                     = d

#if MIN_VERSION_ghc(8,0,0)
extractDeriving :: OutputableBndr alpha => DynFlags -> PackModule -> Text -> LHsSigType alpha -> Descr
extractDeriving dflags pm name HsIB { hsib_body = (L loc typ) } =
#else
extractDeriving :: OutputableBndr alpha => DynFlags -> PackModule -> Text -> LHsType alpha -> Descr
extractDeriving dflags pm name (L loc typ) =
#endif
        Real $ RealDescr {
        dscName'        =   className
    ,   dscMbTypeStr'   =   Just (BS.pack . T.unpack $ "instance " <> className <> " " <> name)
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment (Nothing :: Maybe NDoc) []
    ,   dscTypeHint'    =   InstanceDescr (T.words name)
    ,   dscExported'    =   True}
        where
        className       =   T.pack . showSDocUnqual dflags $ ppr typ

extractMethods :: DynFlags -> [LSig RdrName] -> [MyLDocDecl] -> [SimpleDescr]
extractMethods dflags sigs docs =
    let pairs = attachComments sigs docs
    in concatMap (extractMethod dflags) pairs

extractMethod :: OutputableBndr alpha => DynFlags -> (LHsDecl alpha, Maybe (NDoc)) -> [SimpleDescr]
#if MIN_VERSION_ghc(8,0,0)
extractMethod dflags ((L loc (SigD ts@(TypeSig names _typ))), mbDoc) = map extractName names
#elif MIN_VERSION_ghc(7,10,0)
extractMethod dflags ((L loc (SigD ts@(TypeSig names _typ _))), mbDoc) = map extractName names
#elif MIN_VERSION_ghc(7,2,0)
extractMethod dflags ((L loc (SigD ts@(TypeSig names _typ))), mbDoc) = map extractName names
#else
extractMethod dflags ((L loc (SigD ts@(TypeSig name' _typ))), mbDoc) = [extractName name']
#endif
  where
  extractName name =
    SimpleDescr
        (T.pack . showSDoc dflags . ppr $ unLoc name)
        (Just . BS.pack . showSDocUnqual dflags $ ppr ts)
        (srcSpanToLocation loc)
        (toComment mbDoc [])
        True
extractMethod _ (_, _mbDoc) = []

extractConstructor :: DynFlags -> Located (ConDecl RdrName) -> [SimpleDescr]
#if MIN_VERSION_ghc(8,0,0)
extractConstructor dflags decl@(L loc d') = extractDecl d'
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
        (T.pack . showSDoc dflags . ppr $ unLoc name)
        (Just . BS.pack . showSDocUnqual dflags . ppr $ uncommentDecl decl)
        (srcSpanToLocation loc)
        (case doc of
            Nothing -> Nothing
            Just (L _ d) -> Just . BS.pack . T.unpack $ printHsDoc d)
        True

extractRecordFields :: DynFlags -> Located (ConDecl RdrName) -> [SimpleDescr]
#if MIN_VERSION_ghc(8,0,0)
extractRecordFields dflags (L _ _decl@ConDeclH98 {con_details = RecCon flds}) =
#else
extractRecordFields dflags (L _ _decl@(ConDecl {con_details = RecCon flds})) =
#endif
    concatMap extractRecordFields' (unLoc710 flds)
    where
#if MIN_VERSION_ghc(7,10,0)
    extractRecordFields' (L _ _field@(ConDeclField names typ doc)) = map extractName names
#else
    extractRecordFields' _field@(ConDeclField name' typ doc) = [extractName name']
#endif
      where
      extractName name =
        SimpleDescr
            (T.pack . showSDoc dflags . ppr $ unLoc710 name)
            (Just . BS.pack . showSDocUnqual dflags $ ppr typ)
            (srcSpanToLocation $ getLoc name)
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just . BS.pack . T.unpack $ printHsDoc d)
            True
extractRecordFields _ _ = []

attachComments :: [LSig RdrName] -> [MyLDocDecl] -> [(LHsDecl RdrName, Maybe (NDoc))]
attachComments sigs docs = collectDocs $ sortByLoc $
        ((map (\ (L l i) -> L l (SigD i)) sigs) ++ (map (\ (L l i) -> L l (DocD i)) docs))

sigToByteString :: DynFlags -> [(NSig, Maybe NDoc)] -> Maybe ByteString
sigToByteString _ [] = Nothing
sigToByteString dflags [(sig,_)] = Just (BS.pack (showSDocUnqual dflags $ppr sig))
sigToByteString dflags ((sig,_):_) = Just (BS.pack (showSDocUnqual dflags $ppr sig))

srcSpanToLocation :: SrcSpan -> Maybe Location
#if MIN_VERSION_ghc(7,2,0)
srcSpanToLocation (RealSrcSpan span')
    =   Just (Location (unpackFS $ srcSpanFile span') (srcSpanStartLine span') (srcSpanStartCol span')
                 (srcSpanEndLine span') (srcSpanEndCol span'))
srcSpanToLocation _ = Nothing
#else
srcSpanToLocation span' | not (isGoodSrcSpan span')
    =   Nothing
srcSpanToLocation span'
    =   Just (Location (unpackFS $ srcSpanFile span') (srcSpanStartLine span') (srcSpanStartCol span')
                 (srcSpanEndLine span') (srcSpanEndCol span'))
#endif

toComment :: Maybe (NDoc) -> [NDoc] -> Maybe ByteString
toComment (Just c) _    =  Just . BS.pack . T.unpack $ printHsDoc c
toComment Nothing (c:_) =  Just . BS.pack . T.unpack $ printHsDoc c
toComment Nothing []    =  Nothing


{--
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Data] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TyFamily _ lid _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (ClassDecl _ lid _ _ _ _ _ _ )))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Class] []
--}
printHsDoc :: NDoc  -> Text
printHsDoc (HsDocString fs) = T.pack $ unpackFS fs

---------------------------------------------------------------------------------
-- Now the interface file stuff

mayGetInterfaceFile :: PackageIdAndKey -> ModuleName -> Ghc (Maybe (ModIface,FilePath))
mayGetInterfaceFile p mn =
    let pid             =   packId p
        makeMod         =   mkModule
#if MIN_VERSION_ghc(8,0,0)
                                 (packUnitId p)
#elif MIN_VERSION_ghc(7,10,0)
                                 (packKey p)
#else
                                 (mkPackageId pid)
#endif
        isBase  =   pkgName pid == (PackageName "base")
        mn'     =   mkModuleName (display mn)
        iface   =   findAndReadIface empty (if isBase
                                                then mkBaseModule_ mn'
                                                else makeMod mn') False
        gblEnv  =   IfGblEnv { if_rec_types = Nothing }
    in do
        hscEnv              <-  getSession
        maybe'              <-  liftIO $ initTcRnIf  'i' hscEnv gblEnv () iface
        case maybe' of
            M.Succeeded val ->    return (Just val)
            _               ->    return Nothing

mayGetInterfaceDescription :: DynFlags -> PackageIdAndKey -> ModuleName -> Ghc (Maybe ModuleDescr)
mayGetInterfaceDescription dflags pid mn = do
    mbIf <- mayGetInterfaceFile pid mn
    case mbIf of
        Nothing -> do
            liftIO $ debugM "leksah-server" ("no interface file for " ++ show mn)
            return Nothing
        Just (mif,_) ->
            let allDescrs  =    extractExportedDescrH dflags pid mif
                mod'       =    extractExportedDescrR dflags pid allDescrs mif
            in do
                liftIO $ debugM "leksah-server" ("interface file for " ++ show mn ++ " descrs: " ++
                                    show (length (mdIdDescriptions mod')))
                return (Just mod')



