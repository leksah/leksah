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
,   getInstalledPackageInfos
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

import Distribution.Package
import Distribution.InstalledPackageInfo
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)

import Ghf.Core

getInstalledPackageInfos :: Session -> IO [InstalledPackageInfo]
getInstalledPackageInfos session = do
    dflags1         <-  getSessionDynFlags session
    pkgInfos        <-  case pkgDatabase dflags1 of
                            Nothing -> return []
                            Just fm -> return (eltsUFM fm)
    return pkgInfos

extractInfo :: ([(ModIface, FilePath)],[(ModIface, FilePath)],InstalledPackageInfo)
                    -> PackageDescr
extractInfo (ifacesExp,ifacesHid,ipi) =
    let pi = trace ("now extracting " ++ showPackageId (package ipi))
                $package ipi
        hiddenDescrs    =   foldr (extractExportedDescrH pi) Map.empty
                                        (map fst ifacesHid)
        (ids,mods)      =   --trace  ("\nhidden: " ++ show (Map.keysSet hiddenDescrs))
                                foldr (extractExportedDescrR pi hiddenDescrs)
                                        (Map.empty,[]) (map fst ifacesExp)
    in PackageDescr {
        packageIdW      =   pi
    ,   exposedModulesD =   mods
    ,   buildDependsW   =   depends ipi
    ,   mbSourcePathP   =   Nothing
    ,   idDescriptions  =   ids}

extractExportedDescrH :: PackageIdentifier -> ModIface -> SymbolTable -> SymbolTable
extractExportedDescrH pid iface amap =
    let exportedNames   = Set.fromList
                            $ map occNameString
                                $ concatMap availNames
                                    $ concatMap snd (mi_exports iface)
        exportedDecls   =   filter (\ ifdecl -> (occNameString $ ifName ifdecl)
                                                    `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
    in  foldr (extractIdentifierDescr' pid []) amap exportedDecls

extractIdentifierDescr' :: PackageIdentifier -> [ModuleIdentifier] -> IfaceDecl -> SymbolTable -> SymbolTable
extractIdentifierDescr' pid mods ifDecl amap =
    let descrs =  extractIdentifierDescr ifDecl mods pid
    in foldr addToMap amap descrs
        where
            addToMap :: IdentifierDescr -> SymbolTable -> SymbolTable
            addToMap descr amap =
                let id = identifierW descr
                in case Map.lookup id amap of
                        Nothing -> Map.insert id [descr] amap
                        Just v  -> Map.adjust (\v -> descr:v) id amap

extractExportedDescrR :: PackageIdentifier -> SymbolTable -> ModIface -> (SymbolTable,[ModuleDescr]) -> (SymbolTable,[ModuleDescr])
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
        reexported      =   Map.map (\v -> map (\id -> id{moduleIdI = mid : moduleIdI id}) v)
                                $Map.filterWithKey (\k v -> k `Set.member` otherDecls) hidden
        inst            =   concatMap extractInstances (mi_insts iface)
        uses            =   Map.fromList $ map extractUsages (mi_usages iface)
        mdescr          =   ModuleDescr {
                    moduleId        =   mid
                ,   exportedNames   =   exportedNames
                ,   packageIdM      =   pid
                ,   mbSourcePathM   =   Nothing
                ,   instances       =   inst
                ,   usages          =   uses}
        newids          =   Map.unionWith (\ v1 v2 -> error "impossible: extractExported")
                                mapWithOwnDecls reexported

    in  --trace  ("\n\n module: " ++ mid
        --            ++ "\n exported " ++ show exportedNames
        --            ++ "\n own: " ++ show (Map.keysSet mapWithOwnDecls)
        --            ++ "\n other: " ++ show otherDecls
        --            ++ "\n reexported " ++ show (Map.keysSet reexported))
                    (newids, mdescr : mdList)

extractIdentifierDescr :: IfaceDecl -> [ModuleIdentifier] -> PackageIdentifier -> [IdentifierDescr]
extractIdentifierDescr (IfaceId ifName ifType ifIdInfo) modul package
       = [IdentifierDescr{
    identifierW      =   unpackFS $occNameFS ifName
,   typeInfo        =   showSDocUnqual $ppr ifType
,   identifierType  =   Function
,   moduleIdI       =   modul
,   packageIdI      =   package}]

extractIdentifierDescr (IfaceData ifName ifTyVars ifCtxt ifCons _ ifVrcs _) modul package
        = IdentifierDescr{
    identifierW      =   unpackFS $occNameFS ifName
,   typeInfo        =   "" --showSDocUnqual $pprIfaceForAllPart ifTyVars ifCtxt empty
,   identifierType  =   case ifCons of
                            IfDataTyCon _ -> Data
                            IfNewTyCon _  -> Newtype
                            IfAbstractTyCon -> AbstractData
,   moduleIdI       =   modul
,   packageIdI      =   package} :
        concatMap (extractIdentifierDescrConst modul package (LocalTop ifName))
                (visibleIfConDecls ifCons)

extractIdentifierDescr (IfaceSyn ifName ifTyVars ifVrcs ifSynRhs) modul package
        = [IdentifierDescr{
    identifierW     =   unpackFS $occNameFS ifName
,   typeInfo        =   showSDocUnqual $ppr ifSynRhs
,   identifierType  =   Synonym
,   moduleIdI       =   modul
,   packageIdI      =   package}]

extractIdentifierDescr (IfaceClass ifCtxt ifName ifTyVars ifFDs ifSigs ifRec ifVrcs) modul package
        =  IdentifierDescr{
    identifierW     =   unpackFS $occNameFS ifName
,   typeInfo        =   "" --showSDocUnqual $pprIfaceForAllPart ifTyVars ifCtxt empty
,   identifierType  =   Class
,   moduleIdI       =   modul
,   packageIdI      =   package} :
        map (extractIdentifierDescrClassOp modul package) ifSigs

extractIdentifierDescr (IfaceForeign ifName _) modul package
        = [IdentifierDescr{
    identifierW     =   unpackFS $occNameFS ifName
,   typeInfo        =   ""
,   identifierType  =   Foreign
,   moduleIdI       =   modul
,   packageIdI      =   package}]

extractIdentifierDescrConst :: [ModuleIdentifier] -> PackageIdentifier -> IfaceExtName -> IfaceConDecl
                                    -> [IdentifierDescr]
extractIdentifierDescrConst modul package extName
        (IfVanillaCon ifConOcc _ ifConArgTys _ ifConFields) =
    IdentifierDescr{
    identifierW     =   unpackFS $occNameFS ifConOcc
,   typeInfo        =   showSDocUnqual $ppr
                            (foldr IfaceFunTy (IfaceTyConApp (IfaceTc extName)[]) ifConArgTys)
,   identifierType  =   Constructor
,   moduleIdI       =   modul
,   packageIdI      =   package} : map (extractIdentifierDescrField modul package extName)
                                        (zip ifConFields ifConArgTys)

-- ##TODO: GADTs not yet analysed
extractIdentifierDescrConst modul package extName _ = []

extractIdentifierDescrField :: [ModuleIdentifier] -> PackageIdentifier -> IfaceExtName -> (OccName,IfaceType)
                                    -> IdentifierDescr
extractIdentifierDescrField modul package extName (fieldName, atype) =
    IdentifierDescr{
    identifierW     =   unpackFS
                            $ occNameFS fieldName
,   typeInfo        =   showSDocUnqual
                            $ ppr (IfaceFunTy (IfaceTyConApp (IfaceTc extName)[]) atype)
,   identifierType  =   Field
,   moduleIdI       =   modul
,   packageIdI      =   package}

extractIdentifierDescrClassOp :: [ModuleIdentifier] -> PackageIdentifier -> IfaceClassOp -> IdentifierDescr
extractIdentifierDescrClassOp modul package (IfaceClassOp name _ atype) =
    IdentifierDescr{
    identifierW     =   unpackFS $ occNameFS name
,   typeInfo        =   showSDocUnqual $ ppr atype
,   identifierType  =   ClassOp
,   moduleIdI       =   modul
,   packageIdI      =   package}

extractInstances :: IfaceInst -> [(ClassId,DataId)]
extractInstances ifaceInst  =
    let className   =   showSDocUnqual $ ppr $ ifInstCls ifaceInst
        dataNames   =   map (\iftc -> showSDocUnqual $ ppr iftc)
                            $ map fromJust
                                $ filter isJust
                                    $ ifInstTys ifaceInst
    in map (\dn -> (className, dn)) dataNames

extractUsages :: Usage -> (ModuleIdentifier,Set Symbol)
extractUsages usage =
    let name    =   (showSDoc . ppr) $  usg_name usage
        ids     =   map (showSDocUnqual . ppr . fst) $ usg_entities usage
    in (name, Set.fromList ids)
