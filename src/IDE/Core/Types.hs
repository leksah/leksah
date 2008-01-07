{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.Data
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The core state of ide. This module is imported from every other module,
-- | and all data structures of the state are declared here, to avoid circular
-- | module dependencies.
--
-------------------------------------------------------------------------------

module IDE.Core.Types (

    IDEPackage(..)
,   StandardPath(..)

,   ActionDescr(..)
,   ActionString
,   KeyString

,   Prefs(..)

,   CandyTables
,   CandyTableForth
,   CandyTableBack
,   SpecialKeyTable
,   SpecialKeyCons

,   ErrorSpec(..)

,   PackageDescr(..)
,   ModuleDescr(..)
,   IdentifierDescr(..)
,   Symbol
,   ClassId
,   DataId
,   TypeInfo
,   ModuleIdentifier
,   IdType(..)
,   IdTypeS(..)
,   SymbolTable
,   PackageScope
,   PackModule(..)
,   showPackModule
,   parsePackModule
,   fromPackageIdentifier
,   toPackageIdentifier
,   idDescriptionsPD
,   allFieldsID
,   allConstructorsID
,   allClassOpsID
,   typeInfo
,   idType


,   Location(..)

) where

import Control.Monad.Reader
import Graphics.UI.Gtk hiding (get)
import Distribution.Package
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)

import IDE.Utils.Default


data StandardPath = LeftTop | LeftBottom | RightTop | RightBottom
    deriving(Read,Show,Eq,Enum)

-- ---------------------------------------------------------------------
-- IDEPackages
--

data IDEPackage     =   IDEPackage {
    packageId       ::   PackageIdentifier
,   cabalFile       ::   FilePath
,   configFlags     ::   [String]
,   buildFlags      ::   [String]
,   haddockFlags    ::   [String]
,   exeFlags        ::   [String]
,   installFlags    ::   [String]
,   registerFlags   ::   [String]
,   unregisterFlags ::   [String]
,   sdistFlags      ::   [String]
}
    deriving (Eq,Show)

-- ---------------------------------------------------------------------
-- Other data structures which are used in the state
--

--
-- | ActionDescr is a data structure from which GtkActions are build, which are used for
--   menus, toolbars, and accelerator keystrokes
--
data ActionDescr alpha = AD {
    name        ::   ActionString
,   label       ::   String
,   tooltip     ::   Maybe String
,   stockID     ::   Maybe String
,   action      ::   ReaderT alpha IO ()
,   accelerator ::   [KeyString]
,   isToggle    ::   Bool
}

type ActionString = String
type KeyString = String

--
-- | Preferences is a data structure to hold configuration data
--
data Prefs = Prefs {
    showLineNumbers     ::   Bool
,   rightMargin         ::   Maybe Int
,   tabWidth            ::   Int
,   sourceCandy         ::   Maybe String
,   keymapName          ::   String
,   forceLineEnds       ::   Bool
,   textviewFont        ::   Maybe String
,   logviewFont         ::   Maybe String
,   defaultSize         ::   (Int,Int)
,   browser             ::   String
,   sourcePanePath      ::   StandardPath
,   logPanePath         ::   StandardPath
,   infoPanePath        ::   StandardPath
,   modulesPanePath     ::   StandardPath
,   controlPanePath     ::   StandardPath
,   sourceDirectories   ::   [FilePath]
} deriving(Eq,Show)


type CandyTableForth    =   [(Bool,String,String)]
type CandyTableBack     =   [(String,String,Int)]
type CandyTables        =   (CandyTableForth,CandyTableBack)

type SpecialKeyTable alpha  =   Map (KeyVal,[Modifier]) (Map (KeyVal,[Modifier]) (ActionDescr alpha))
type SpecialKeyCons  alpha  =   Maybe ((Map (KeyVal,[Modifier]) (ActionDescr alpha)),String)

instance Show Modifier
    where show Shift    =   "<shift>"
          show Control  =   "<ctrl>"
          show Alt      =   "<alt>"
          show Apple    =   "<apple>"
          show Compose  =   "<compose>"

--
-- | Other types
--
data ErrorSpec = ErrorSpec {
    filePath            ::   FilePath
,   line                ::   Int
,   column              ::   Int
,   errDescription      ::   String
,   logLines            ::   (Int,Int)
}   deriving Show

-- ---------------------------------------------------------------------
--  | Information about the world, extraced from .hi and maybe source files
--

type PackageScope       =   (Map PackageIdentifier PackageDescr,SymbolTable)
type SymbolTable        =   Map Symbol [IdentifierDescr]

data PackageDescr       =   PackageDescr {
    packagePD           ::   PackageIdentifier
,   mbSourcePathPD      ::   (Maybe FilePath)
,   exposedModulesPD    ::   [ModuleDescr]
,   buildDependsPD      ::   [PackageIdentifier]
} deriving (Eq,Ord,Show)

data ModuleDescr        =   ModuleDescr {
    moduleIdMD          ::   PackModule
,   mbSourcePathMD      ::   (Maybe FilePath)
,   exportedNamesMD     ::   (Set Symbol)                        -- unqualified
,   usagesMD            ::   (Map ModuleIdentifier (Set Symbol)) -- imports
,   idDescriptionsMD    ::   [IdentifierDescr]
} deriving (Eq,Ord,Show)

data IdentifierDescr    =
    SimpleDescr {
            identifierID        ::   Symbol
        ,   identifierTypeID    ::   IdTypeS
        ,   typeInfoID          ::   TypeInfo
        ,   moduleIdID          ::   PackModule
        ,   mbLocation          ::   (Maybe Location)
        ,   mbComment           ::   (Maybe ByteString)}
    |    DataDescr {
            identifierID        ::   Symbol
        ,   typeInfoID          ::   TypeInfo
        ,   moduleIdID          ::   PackModule
        ,   constructorsID      ::   [Symbol]
        ,   fieldsID            ::   [Symbol]
        ,   mbLocation          ::   (Maybe Location)
        ,   mbComment           ::   (Maybe ByteString)}
    |    ClassDescr {
            identifierID        ::   Symbol
        ,   typeInfoID          ::   TypeInfo
        ,   moduleIdID          ::   PackModule
        ,   classOpsID          ::   [Symbol]
        ,   mbLocation          ::   (Maybe Location)
        ,   mbComment           ::   (Maybe ByteString)}
    |    InstanceDescr {
            identifierID        ::   Symbol --the class
        ,   binds               ::   [Symbol]
        ,   moduleIdID          ::   PackModule
        ,   mbLocation          ::   (Maybe Location)
        ,   mbComment           ::   (Maybe ByteString)}
    deriving (Show,Eq,Ord,Read)

allFieldsID :: IdentifierDescr -> [Symbol]
allFieldsID (DataDescr _ _ _ _ fieldsId _ _)              =   fieldsId
allFieldsID _                                             =   []

allConstructorsID :: IdentifierDescr -> [Symbol]
allConstructorsID (DataDescr _ _ _ constructorsID _ _ _)  =   constructorsID
allConstructorsID _                                       =   []

allClassOpsID :: IdentifierDescr -> [Symbol]
allClassOpsID (ClassDescr _ _ _ classOpsID _ _)           =   classOpsID
allClassOpsID _                                           =   []

typeInfo :: IdentifierDescr -> TypeInfo
typeInfo (SimpleDescr _ _ ti _ _ _)     =   ti
typeInfo (DataDescr _ ti _ _ _ _ _)     =   ti
typeInfo (ClassDescr _ ti _ _ _ _)      =   ti
typeInfo (InstanceDescr _ _ _ _ _)      =   BS.pack ""

idDescriptionsPD :: PackageDescr -> [IdentifierDescr]
idDescriptionsPD pd =  concatMap idDescriptionsMD (exposedModulesPD pd)

instance Default IdentifierDescr where
    getDefault = SimpleDescr getDefault getDefault getDefault getDefault getDefault
                                    getDefault

data IdType = Function | Newtype | Synonym | AbstractData | OpenData | Foreign
    | Data | Class | Instance | Constructor | Field | ClassOP | OrphanedInstance
  deriving (Show, Eq, Ord, Enum, Read)

instance Default IdType where
    getDefault = Function

data IdTypeS = FunctionS | NewtypeS | SynonymS | AbstractDataS | OpenDataS | ForeignS
  deriving (Show, Eq, Ord, Enum, Read)

instance Default IdTypeS where
    getDefault = FunctionS

idType :: IdentifierDescr -> IdType
idType (SimpleDescr _ stype _ _ _ _)    =   case stype of
                                                FunctionS   ->  Function
                                                NewtypeS    ->  Newtype
                                                SynonymS    ->  Synonym
                                                AbstractDataS -> AbstractData
                                                OpenDataS   ->  OpenData
                                                ForeignS    ->  Foreign
idType (DataDescr _ _ _ _ _ _ _)        =   Data
idType (ClassDescr _ _ _ _ _ _)         =   Class
idType (InstanceDescr _ _ _ _ _)        =   Instance

type Symbol             =   String  -- Qualified or unqualified
type ClassId            =   String  -- Qualified or unqualified
type DataId             =   String  -- Qualified or unqualified
type TypeInfo           =   ByteString
type ModuleIdentifier   =   String  -- always qualified

data PackModule         =   PM {    pack :: PackageIdentifier
                                ,   modu :: ModuleIdentifier}
                                deriving (Eq, Ord,Read,Show)



showPackModule ::  PackModule -> String
showPackModule (PM p m) =   showPackageId p ++ ":" ++ m

parsePackModule         ::   String -> PackModule
parsePackModule str     =   let (pack',mod') = span (\c -> c /= ':') str
                            in  if null (tail mod')
                                 then perror str
                                 else case toPackageIdentifier $ pack' of
                                        Nothing -> perror str
                                        Just pi'-> (PM pi' (tail mod'))
    where perror s      =   error $ "cannot parse PackModule from " ++ s

fromPackageIdentifier :: PackageIdentifier -> String
fromPackageIdentifier   =   showPackageId

toPackageIdentifier :: String -> Maybe PackageIdentifier
toPackageIdentifier pd      =   let l = filter (\ (_,s) -> null s)
                                            $ readP_to_S parsePackageId pd
                                in  if null l
                                    then Nothing
                                    else Just (fst $ head l)

instance Default PackModule where
    getDefault = parsePackModule "unknow-0:Undefined"

data Location           =   Location {
    locationSLine       ::   Int
,   locationSCol	    ::   Int
,   locationELine       ::   Int
,   locationECol        ::   Int
}   deriving (Show,Eq,Ord,Read)

instance Default ByteString
    where getDefault = BS.empty


