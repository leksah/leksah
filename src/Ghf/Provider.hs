-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Provider
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | This module provides the infos collected by the extractor before
--
-------------------------------------------------------------------------------

module Ghf.Provider (
    loadInfosForPackages
,   clearInfosForPackages
,   findFittingPackages
,   typeDescription
) where

import Distribution.Package
import Config
import Control.Monad
import Control.Monad.Trans
import System.FilePath
import System.Directory
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import GHC
import Distribution.PackageDescription hiding (package)
import Distribution.InstalledPackageInfo
import Distribution.Version
import Data.List


import Ghf.Core
import Ghf.File
import Ghf.Extractor

loadInfosForPackages :: [PackageIdentifier] -> GhfAction
loadInfosForPackages packages = do
    let version     =   cProjectVersion
    collectorPath   <-  lift $getCollectorPath version
    (packageList,symbolTable) <-  lift $foldM (loadInfosForPackage collectorPath)
                                                ([],Map.empty) packages
    modifyGhf_ (\ghf -> return (ghf{packWorld = (Just (packageList,symbolTable))}))

clearInfosForPackages :: GhfAction
clearInfosForPackages = do
    modifyGhf_ (\ghf -> return (ghf{packWorld = Nothing}))

loadInfosForPackage :: FilePath -> ([PackageDescr],SymbolTable) -> PackageIdentifier ->
                            IO ([PackageDescr],SymbolTable)
loadInfosForPackage dirPath (packageList, symbolTable) pid = do
    let filePath = dirPath </> showPackageId pid ++ ".pack"
    exists <- doesFileExist filePath
    if exists
        then do
            hdl <- openFile filePath ReadMode
            str <- hGetContents hdl
            packageInfo <- readIO str
            hClose hdl
            let newSymbols = Map.unionWith (++) symbolTable (idDescriptions packageInfo)
            return (packageInfo:packageList, newSymbols)
        else do
            message $"packaeInfo not found for " ++ showPackageId pid
            return (packageList, symbolTable)


findFittingPackages :: Session -> [Dependency] -> IO  [PackageIdentifier]
findFittingPackages session dependencyList = do
    knownPackages   <-  getInstalledPackageInfos session
    let packages    =   map package knownPackages
    return (concatMap (fittingKnown packages) dependencyList)
    where
    fittingKnown packages (Dependency dname versionRange) =
        let filtered =  filter (\ (PackageIdentifier name version) ->
                                    name == dname && withinRange version versionRange)
                        packages
        in  if length filtered > 1
                then [maximumBy (\a b -> compare (pkgVersion a) (pkgVersion b)) filtered]
                else filtered

typeDescription :: String -> SymbolTable -> String
typeDescription "" _ = "No selection"
typeDescription str st =
    case str `Map.lookup` st of
        Nothing -> "No info found -- Testing for scoped symbols missing"
        Just list -> concatMap generateText list
    where
        ttString TypeFunction   =   "identifies a function of type "
        ttString TypeData       =   "identifies data definition"
        ttString TypeNewtype    =   "identifies a Newtype"
        ttString TypeSyn        =   "identifies a synonym type for"
        ttString TypeAbstractData = "identifies an abstract data type"
        ttString TypeConstructor =  "identifies a constructor of data type"
        ttString TypeField      =   "identifies a field in a record with type"
        ttString TypeClass      =   "identifies a class"
        ttString TypeClassOp    =   "identifies a class operation with type "
        ttString TypeForeign    =   "identifies something strange"
        generateText (IdentifierDescr _ tt ti m p) =
            str ++ " "  ++   (ttString tt) ++ "\n   "
                ++   ti ++  "\n   "
                ++   "exported by modules "  ++   show m ++ " in package " ++ show p ++ "\n   "









