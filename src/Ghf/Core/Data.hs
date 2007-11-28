{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Core.Data
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The core state of ghf. This module is imported from every other module,
-- | and all data structures of the state are declared here, to avoid circular
-- | module dependencies.
--
-------------------------------------------------------------------------------

module Ghf.Core.Data (

    GhfPackage(..)
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
,   SymbolTable
,   PackageScope
,   PackModule(..)
,   showPackModule
,   parsePackModule

,   Location(..)

) where

import Control.Monad.Reader
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.ModelView as New
import Distribution.Package
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP

import Data.Ghf.Default

data StandardPath = LeftTop | LeftBottom | RightTop | RightBottom
    deriving(Read,Show,Eq,Enum)

-- ---------------------------------------------------------------------
-- GhfPackages
--

data GhfPackage     =   GhfPackage {
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
    ,   sourceDirectories   ::   [FilePath]
} deriving(Eq,Show)


type CandyTableForth =  [(Bool,String,String)]
type CandyTableBack  =  [(String,String,Int)]
type CandyTables     =  (CandyTableForth,CandyTableBack)

type SpecialKeyTable alpha =  Map (KeyVal,[Modifier]) (Map (KeyVal,[Modifier]) (ActionDescr alpha))
type SpecialKeyCons  alpha =  Maybe ((Map (KeyVal,[Modifier]) (ActionDescr alpha)),String)

instance Show Modifier
    where show Shift    = "<shift>"
          show Control  = "<ctrl>"
          show Alt      = "<alt>"
          show Apple    = "<apple>"
          show Compose  = "<compose>"

type FileName        =  String

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
    packagePD           ::   ! PackageIdentifier
,   mbSourcePathPD      ::   ! (Maybe FilePath)
,   exposedModulesPD    ::   ! [ModuleDescr]
,   buildDependsPD      ::   ! [PackageIdentifier]
,   idDescriptionsPD    ::   ! SymbolTable
} deriving (Eq,Ord,Show)

data ModuleDescr        =   ModuleDescr {
    moduleIdMD          ::   ! PackModule
,   mbSourcePathMD      ::   ! (Maybe FilePath)
,   exportedNamesMD     ::   ! (Set Symbol)                        -- unqualified
,   instancesMD         ::   ! [(ClassId,DataId)]
,   usagesMD            ::   ! (Map ModuleIdentifier (Set Symbol)) -- imports
} deriving (Eq,Ord,Show)

data IdentifierDescr    =  IdentifierDescr {
    identifierID        ::   ! Symbol
,   identifierTypeID    ::   ! IdType
,   typeInfoID          ::   ! TypeInfo
,   moduleIdID          ::   ! PackModule
,   mbLocation          ::   ! (Maybe Location)
} deriving (Show,Eq,Ord,Read)

instance Default IdentifierDescr where
    getDefault = IdentifierDescr getDefault getDefault getDefault getDefault getDefault

data IdType = Function | Data | Newtype | Synonym | AbstractData |
                Constructor | Field | Class | ClassOp | Foreign
  deriving (Show, Eq, Ord, Enum,Read)

instance Default IdType where
    getDefault = Function

emptyIdentifierDescr = IdentifierDescr ""

type Symbol             =   String  -- Qualified or unqualified
type ClassId            =   String  -- Qualified or unqualified
type DataId             =   String  -- Qualified or unqualified
type TypeInfo           =   String
type ModuleIdentifier   =   String  -- always qualified

data PackModule         =   PM {    pack :: PackageIdentifier
                                ,   modu :: ModuleIdentifier}
                                deriving (Eq, Ord,Read,Show)

showPackModule ::  PackModule -> String
showPackModule (PM p m) =   showPackageId p ++ ":" ++ m

parsePackModule         ::   String -> PackModule
parsePackModule str     =   let (pack,mod) = span (\c -> c /= ':') str
                            in  case readP_to_S parsePackageId pack of
                                [(ps,_)]  -> if null mod
                                                then perror str
                                                else (PM ps (tail mod))
                                _         -> perror str
    where perror s      =   error $ "cannot parse PackModule from " ++ s

instance Default PackModule where
    getDefault = parsePackModule "unknow-0.0:undefined"

data Location           =   Location {
    locationSLine       ::   !Int
,   locationSCol	    ::   !Int
,   locationELine       ::   !Int
,   locationECol        ::   !Int
}   deriving (Show,Eq,Ord,Read)

