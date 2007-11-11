-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Extractor
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | This modulle extracts information from the ModIface data structure
--   and converts it into ModuleDescriptions which are collected into an
--   GHFPackageDescription
--
-------------------------------------------------------------------------------

module Ghf.Extractor (
    extractInfo

,   fromPackageIdentifier
,   toPackageIdentifier

) where

import GHC hiding(Id)
import Module
import TcRnMonad
import qualified Maybes as M
import ErrUtils
import HscTypes
import Finder
import LoadIface
import Outputable hiding(trace)
import qualified Pretty as P
import MkIface
import Config
import IfaceSyn
import OccName
import FastString
import Outputable hiding(trace)
import UniqFM
--import PackageConfig hiding (PackageIdentifier)
import qualified PackageConfig as DP
import Name
import Unique
import SrcLoc

import Distribution.Package
--import Distribution.InstalledPackageInfo
import Distribution.Version
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable (maximumBy)
import Text.ParserCombinators.ReadP

import Ghf.Core.State
--import Ghf.Info



extractInfo :: ([(ModIface, FilePath)],[(ModIface, FilePath)],PackageIdentifier,
                    [PackageIdentifier]) -> PackageDescr
extractInfo (ifacesExp,ifacesHid,pi,depends) =
    let hiddenDescrs        =   foldr (extractExportedDescrH pi) Map.empty (map fst ifacesHid)
        (ids,mods)          =   --trace  ("\nhidden: " ++ show (Map.keysSet hiddenDescrs))
                                foldr (extractExportedDescrR pi hiddenDescrs)
                                        (Map.empty,[]) (map fst ifacesExp)
    in PackageDescr {
        packagePD           =   pi
    ,   exposedModulesPD    =   mods
    ,   buildDependsPD      =   depends
    ,   mbSourcePathPD      =   Nothing
    ,   idDescriptionsPD    =   ids}

extractExportedDescrH :: PackageIdentifier -> ModIface -> SymbolTable -> SymbolTable
extractExportedDescrH pid iface amap =
    let exportedNames       = Set.fromList
                                $ map occNameString
                                    $ concatMap availNames
                                        $ concatMap snd (mi_exports iface)
        exportedDecls       =   filter (\ ifdecl -> (occNameString $ ifName ifdecl)
                                                    `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
    in  foldr (extractIdentifierDescr' pid []) amap exportedDecls

extractIdentifierDescr' :: PackageIdentifier -> [ModuleIdentifier] -> IfaceDecl ->
                                SymbolTable -> SymbolTable
extractIdentifierDescr' pid mods ifDecl amap =
    let descrs =  extractIdentifierDescr ifDecl mods pid
    in foldr addToMap amap descrs
        where
            addToMap :: IdentifierDescr -> SymbolTable -> SymbolTable
            addToMap descr amap =
                let id = identifierID descr
                in case Map.lookup id amap of
                        Nothing -> Map.insert id [descr] amap
                        Just v  -> Map.adjust (\v -> descr:v) id amap

extractExportedDescrR :: PackageIdentifier -> SymbolTable -> ModIface ->
                            (SymbolTable,[ModuleDescr]) -> (SymbolTable,[ModuleDescr])
extractExportedDescrR pid hidden iface (imap,mdList) =
    let mid             =   moduleNameString $moduleName (mi_module iface)
        exportedNames   =   Set.fromList
                                $map occNameString
                                    $concatMap availNames
                                        $concatMap snd (mi_exports iface)
        exportedDecls   =   filter (\ ifdecl -> (occNameString $ifName ifdecl)
                                                    `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
        mapWithOwnDecls =   foldr (extractIdentifierDescr' pid [mid]) imap exportedDecls
        otherDecls      =   exportedNames `Set.difference` (Map.keysSet mapWithOwnDecls)
        reexported      =   Map.map (\v -> map (\id -> id{moduleIdID = (PM pid mid)
                                                                    : moduleIdID id}) v)
                                $Map.filterWithKey (\k v -> k `Set.member` otherDecls)
                                    hidden
        inst            =   concatMap extractInstances (mi_insts iface)
        uses            =   Map.fromList $ map extractUsages (mi_usages iface)
        mdescr          =   ModuleDescr {
                    moduleIdMD        =   PM pid mid
                ,   exportedNamesMD   =   exportedNames
                ,   mbSourcePathMD    =   Nothing
                ,   instancesMD       =   inst
                ,   usagesMD          =   uses}
        newids          =   Map.unionWith (\ v1 v2 -> error "impossible: extractExported")
                                mapWithOwnDecls reexported

    in  --trace  ("\n\n module: " ++ mid
        --            ++ "\n exported " ++ show exportedNames
        --            ++ "\n own: " ++ show (Map.keysSet mapWithOwnDecls)
        --            ++ "\n other: " ++ show otherDecls
        --            ++ "\n reexported " ++ show (Map.keysSet reexported))
                    (newids, mdescr : mdList)

extractIdentifierDescr :: IfaceDecl -> [ModuleIdentifier] -> PackageIdentifier
                            -> [IdentifierDescr]
extractIdentifierDescr (IfaceId ifName ifType ifIdInfo) modules package
       = [IdentifierDescr{
    identifierID        =   unpackFS $occNameFS ifName
,   typeInfoID          =   showSDocUnqual $ppr ifType
,   identifierTypeID    =   Function
,   moduleIdID          =   map (PM package) modules}]

#if __GHC__ >= 670
extractIdentifierDescr (IfaceData ifName ifTyVars ifCtxt ifCons _ _ _ _ ) modules package
#else
extractIdentifierDescr (IfaceData ifName ifTyVars ifCtxt ifCons _ ifVrcs _) modules package
#endif
        = IdentifierDescr{
    identifierID        =   unpackFS $occNameFS ifName
,   typeInfoID          =   "" --showSDocUnqual $pprIfaceForAllPart ifTyVars ifCtxt empty
,   identifierTypeID    =   case ifCons of
                                IfDataTyCon _ -> Data
                                IfNewTyCon _  -> Newtype
                                IfAbstractTyCon -> AbstractData
,   moduleIdID          =   map (PM package) modules} :
#if __GHC__ >= 670
        concatMap (extractIdentifierDescrConst modules package ifName)
                (visibleIfConDecls ifCons)
#else
        (concatMap (extractIdentifierDescrConst modules package (LocalTop ifName))
                                    $ visibleIfConDecls ifCons)
#endif

#if __GHC__ >= 670
extractIdentifierDescr (IfaceSyn ifName _ _ ifSynRhs _) modules package
#else
extractIdentifierDescr (IfaceSyn ifName ifTyVars ifVrcs ifSynRhs) modules package
#endif
        = [IdentifierDescr{
    identifierID        =   unpackFS $occNameFS ifName
,   typeInfoID          =   showSDocUnqual $ppr ifSynRhs
,   identifierTypeID    =   Synonym
,   moduleIdID          =   map (PM package) modules}]

#if __GHC__ >= 670
extractIdentifierDescr (IfaceClass ifCtxt ifName ifTyVars ifFDs ifATs ifSigs ifRec) modules package
#else
extractIdentifierDescr (IfaceClass ifCtxt ifName ifTyVars ifFDs ifSigs ifRec ifVrcs) modules package
#endif
        =  IdentifierDescr{
    identifierID        =   unpackFS $occNameFS ifName
,   typeInfoID          =   "" --showSDocUnqual $pprIfaceForAllPart ifTyVars ifCtxt empty
,   identifierTypeID    =   Class
,   moduleIdID          =   map (PM package) modules} :
                                map (extractIdentifierDescrClassOp modules package) ifSigs

extractIdentifierDescr (IfaceForeign ifName _) modules package
        = [IdentifierDescr{
    identifierID        =   unpackFS $occNameFS ifName
,   typeInfoID          =   ""
,   identifierTypeID    =   Foreign
,   moduleIdID          =   map (PM package) modules}]


#if __GHC__ >= 670
extractIdentifierDescrConst :: [ModuleIdentifier] -> PackageIdentifier -> OccName -> IfaceConDecl
                                    -> [IdentifierDescr]
extractIdentifierDescrConst modules package extName
                                    (IfCon ifConOcc _ _ _ _ _ ifConArgTys ifConFields _) =
    let name            =   mkInternalName (getUnique extName) extName noSrcSpan
    in IdentifierDescr{
    identifierID        =   unpackFS $occNameFS ifConOcc
,   typeInfoID          =   showSDocUnqual $ppr
                                (foldr IfaceFunTy (IfaceTyConApp (IfaceTc name)[]) ifConArgTys)
,   identifierTypeID    =   Constructor
,   moduleIdID          =   map (PM package) modules}
                                : map (extractIdentifierDescrField modules package extName)
                                        (zip ifConFields ifConArgTys)
#else
extractIdentifierDescrConst :: [ModuleIdentifier] -> PackageIdentifier -> IfaceExtName -> IfaceConDecl
                                    -> [IdentifierDescr]
extractIdentifierDescrConst modules package extName
        (IfVanillaCon ifConOcc _ ifConArgTys _ ifConFields) =
    IdentifierDescr{
    identifierID        =   unpackFS $occNameFS ifConOcc
,   typeInfoID          =   showSDocUnqual $ppr
                                (foldr IfaceFunTy (IfaceTyConApp (IfaceTc extName)[]) ifConArgTys)
,   identifierTypeID    =   Constructor
,   moduleIdID          =   map (PM package) modules}
                                : (map (extractIdentifierDescrField modules package extName)
                                    $ zip ifConFields ifConArgTys)

extractIdentifierDescrConst modul package extName _ = []
#endif

-- ##TODO: GADTs not yet analysed

#if __GHC__ >= 670
extractIdentifierDescrField :: [ModuleIdentifier] -> PackageIdentifier -> OccName ->
                                (OccName,IfaceType) -> IdentifierDescr
extractIdentifierDescrField modules package extName (fieldName, atype) =
    let name            =   mkInternalName (getUnique extName) extName noSrcSpan
    in IdentifierDescr{
    identifierID        =   unpackFS
                                $ occNameFS fieldName
,   typeInfoID          =   showSDocUnqual
                                $ ppr (IfaceFunTy (IfaceTyConApp (IfaceTc name)[]) atype)--}
,   identifierTypeID    =   Field
,   moduleIdID          =   map (PM package) modules}
#else
extractIdentifierDescrField :: [ModuleIdentifier] -> PackageIdentifier -> IfaceExtName -> (OccName,IfaceType)
                                    -> IdentifierDescr
extractIdentifierDescrField modules package extName (fieldName, atype) =
    IdentifierDescr{
    identifierID        =   unpackFS
                                $ occNameFS fieldName
,   typeInfoID           =   showSDocUnqual
                                $ ppr (IfaceFunTy (IfaceTyConApp (IfaceTc extName)[]) atype)
,   identifierTypeID    =   Field
,   moduleIdID          =   map (PM package) modules}
#endif

extractIdentifierDescrClassOp :: [ModuleIdentifier] -> PackageIdentifier -> IfaceClassOp -> IdentifierDescr
extractIdentifierDescrClassOp modules package (IfaceClassOp name _ atype) =
    IdentifierDescr{
    identifierID        =   unpackFS $ occNameFS name
,   typeInfoID          =   showSDocUnqual $ ppr atype
,   identifierTypeID    =   ClassOp
,   moduleIdID          =   map (PM package) modules}


extractInstances :: IfaceInst -> [(ClassId, DataId)]
extractInstances ifaceInst  =
    let className   =   showSDocUnqual $ ppr $ ifInstCls ifaceInst
        dataNames   =   map (\iftc -> showSDocUnqual $ ppr iftc)
                            $ map fromJust
                                $ filter isJust
                                    $ ifInstTys ifaceInst
    in map (\dn -> (className, dn)) dataNames

extractUsages :: Usage -> (ModuleIdentifier, Set Symbol)
extractUsages usage =
    let name    =   (showSDoc . ppr) $  usg_name usage
        ids     =   map (showSDocUnqual . ppr . fst) $ usg_entities usage
    in (name, Set.fromList ids)

fromPackageIdentifier :: PackageIdentifier -> String
fromPackageIdentifier   =   showPackageId

toPackageIdentifier :: String -> PackageIdentifier
toPackageIdentifier pd    =   case readP_to_S parsePackageId pd of
                                [(ps,_)]  -> ps
                                _         -> error "cannot parse package identifier"
