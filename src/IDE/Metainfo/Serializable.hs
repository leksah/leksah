{-# OPTIONS_GHC
    -XScopedTypeVariables
    -XStandaloneDeriving
    -XDeriveDataTypeable #-}
    -----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.Serializable
-- Copyright   :  2007-2009 Jürgen Nicklisch-Franken
-- License     :  GPL
--
-- Maintainer  :  Jutaro <jutaro@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Metainfo.Serializable (


) where

import IDE.Core.State
import Distribution.Text (simpleParse,display)
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.Binary.Shared (BinaryShared(..))
import Data.Typeable (Typeable(..))
import Distribution.Package (PackageName(..),PackageIdentifier(..))
import Data.Version (Version(..))
import Distribution.ModuleName (ModuleName(..))

deriving instance Typeable PackageIdentifier
deriving instance Typeable ModuleName
deriving instance Typeable PackageName
-----------------------------------------------------------

instance BinaryShared PackModule where
    put =   putShared (\ (PM pack' modu') -> do
                (put pack')
                (put modu'))
    get =   getShared (do
                pack'                <- get
                modu'                <- get
                return (PM pack' modu'))


instance BinaryShared PackageIdentifier where
    put =   putShared  (\ (PackageIdentifier name' version') -> do
                put name'
                put version')
    get =   getShared (do
                name'                <- get
                version'             <- get
                return (PackageIdentifier name' version'))

instance BinaryShared Version where
    put =   putShared (\ (Version branch' tags') -> do
                put branch'
                put tags')
    get =   getShared (do
                branch'              <- get
                tags'                <- get
                return (Version branch' tags'))

instance BinaryShared PackageDescr where
    put =   putShared (\ (PackageDescr packagePD' exposedModulesPD' buildDependsPD'
                                        mbSourcePathPD') -> do
                put packagePD'
                put exposedModulesPD'
                put buildDependsPD'
                put mbSourcePathPD')
    get =   getShared (do
                packagePD'           <- get
                exposedModulesPD'    <- get
                buildDependsPD'      <- get
                mbSourcePathPD'      <- get
                return (PackageDescr packagePD' exposedModulesPD' buildDependsPD'
                                        mbSourcePathPD'))

instance BinaryShared ModuleDescr where
    put = putShared (\ (ModuleDescr moduleIdMD' exportedNamesMD' mbSourcePathMD' usagesMD'
                idDescriptionsMD') -> do
            put moduleIdMD'
            put exportedNamesMD'
            put mbSourcePathMD'
            put usagesMD'
            put idDescriptionsMD')
    get = getShared (do
            moduleIdMD'          <- get
            exportedNamesMD'     <- get
            mbSourcePathMD'      <- get
            usagesMD'            <- get
            idDescriptionsMD'    <- get
            return (ModuleDescr moduleIdMD' exportedNamesMD' mbSourcePathMD'
                                usagesMD' idDescriptionsMD'))

instance BinaryShared Descr where
    put (Descr descrName2 typeInfo2 descrModu2 mbLocation2 mbComment2 details2)
        = do    put (1:: Int)
                put descrName2
                put typeInfo2
                put descrModu2
                put mbLocation2
                put mbComment2
                put details2
    put (Reexported reexpModu' impDescr')
        = do    put (2:: Int)
                put reexpModu'
                put impDescr'
    get = do    (typeHint :: Int) <- get
                case typeHint of
                    1 -> do
                        descrName2          <- get
                        typeInfo2           <- get
                        descrModu2          <- get
                        mbLocation2         <- get
                        mbComment2          <- get
                        details2            <- get
                        return (Descr descrName2 typeInfo2 descrModu2 mbLocation2
                            mbComment2 details2)
                    2 -> do
                        reexpModu'          <- get
                        impDescr'           <- get
                        return (Reexported reexpModu' impDescr')
                    _ -> throwIDE "Impossible in Binary Descr get"

instance BinaryShared SpDescr where
    put VariableDescr
        = do    put (1:: Int)
    put (FieldDescr typeDescrF')
        = do    put (2:: Int)
                put typeDescrF'
    put (ConstructorDescr typeDescrC')
        = do    put (3:: Int)
                put typeDescrC'
    put (DataDescr constructors' fields')
        = do    put (4:: Int)
                put constructors'
                put fields'
    put TypeDescr
        = do    put (5:: Int)
    put (NewtypeDescr constructor' mbField')
        = do    put (6:: Int)
                put constructor'
                put mbField'
    put (ClassDescr super' methods')
        = do    put (7:: Int)
                put super'
                put methods'
    put (MethodDescr classDescrM')
        = do    put (8:: Int)
                put classDescrM'
    put (InstanceDescr binds')
        = do    put (9:: Int)
                put binds'

    get = do    (typeHint :: Int)                <- get
                case typeHint of
                    1 -> return VariableDescr
                    2 -> do
                            typeDescrF'         <- get
                            return (FieldDescr typeDescrF')
                    3 -> do
                            typeDescrC'         <- get
                            return (ConstructorDescr typeDescrC')
                    4 -> do
                            constructors'       <- get
                            fields'             <- get
                            return (DataDescr constructors' fields')
                    5 -> return TypeDescr
                    6 -> do
                            constructor'        <- get
                            mbField'            <- get
                            return (NewtypeDescr constructor' mbField')
                    7 -> do
                            super'              <- get
                            methods'            <- get
                            return (ClassDescr super' methods')
                    8 -> do
                            classDescrM'        <- get
                            return (MethodDescr classDescrM')
                    9 -> do
                            binds'              <- get
                            return (InstanceDescr binds')
                    _ -> throwIDE "Impossible in Binary SpDescr get"

instance BinaryShared Location where
    put (Location locationSLine' locationSCol' locationELine' locationECol')
        = do    put locationSLine'
                put locationSCol'
                put locationELine'
                put locationECol'
    get = do    locationSLine'       <-  get
                locationSCol'        <-  get
                locationELine'       <-  get
                locationECol'        <-  get
                return (Location locationSLine' locationSCol' locationELine' locationECol')

instance BinaryShared ModuleName where
    put    =  put . display
    get    =  liftM (fromJust . simpleParse) get

instance BinaryShared PackageName where
    put (PackageName pn) =  put pn
    get  =  liftM PackageName get





