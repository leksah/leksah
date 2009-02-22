{-# OPTIONS_GHC
    -XDisambiguateRecordFields
    -XExistentialQuantification
    -XRank2Types
    -XFlexibleInstances
    -XDeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.Data
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
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

,   ActionDescr(..)
,   ActionString
,   KeyString

,   Prefs(..)

,   ErrorSpec(..)

,   PackageDescr(..)
,   ModuleDescr(..)
,   Descr(..)
,   Present(..)
,   SpDescr(..)
,   DescrType(..)
,   descrName
,   typeInfo
,   descrModu
,   mbLocation
,   mbComment
,   details
,   descrType
,   stockIdFromType
,   isReexported
,   Symbol
,   ClassId
,   DataId
,   TypeInfo
,   SymbolTable
,   PackageScope
,   PackModule(..)
,   parsePackModule
,   showPackModule
,   fromPackageIdentifier
,   toPackageIdentifier
,   Location(..)
,   Scope(..)

,   SearchHint(..)
,   CandyTable(..)
,   CandyTableForth
,   CandyTableBack
,   KeymapI(..)
,   SpecialKeyTable
,   SpecialKeyCons

,   LogTag(..)

,   GUIHistory
,   GUIHistory'(..)

,   SensitivityMask(..)
,   SearchMode(..)
) where

import Control.Monad.Reader
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Gdk.Events(Modifier(..))
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

import Default
import IDE.Exception
import Graphics.UI.Frame.Panes
import Distribution.Package (PackageIdentifier(..),Dependency(..))
import Distribution.Text
import Data.Map (Map(..))
import Distribution.ModuleName(ModuleName)
import Distribution.Text
import qualified Data.ByteString.Char8 as BS  (unpack,empty)
import Data.ByteString.Char8 (ByteString)
import MyMissing
import Data.Typeable (Typeable(..))

-- ---------------------------------------------------------------------
-- IDEPackages
--

data IDEPackage     =   IDEPackage {
    packageId       ::   PackageIdentifier
,   cabalFile       ::   FilePath
,   depends         ::   [Dependency]
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
    ,   removeTBlanks       ::   Bool
    ,   textviewFont        ::   Maybe String
    ,   sourceStyle         ::   Maybe String
    ,   logviewFont         ::   Maybe String
    ,   defaultSize         ::   (Int,Int)
    ,   browser             ::   String
    ,   sourcePanePath      ::   StandardPath
    ,   logPanePath         ::   StandardPath
    ,   modulesPanePath     ::   StandardPath
    ,   sourceDirectories   ::   [FilePath]
    ,   packageBlacklist    ::   [Dependency]
    ,   collectAfterBuild   ::   Bool
    ,   collectAtStart      ::   Bool
    ,   autoExtractTars     ::   Maybe FilePath
    ,   useCtrlTabFlipping  ::   Bool
    ,   docuSearchURL       ::   String
    ,   completeRestricted  ::   Bool
    ,   saveAllBeforeBuild  ::   Bool
} deriving(Eq,Show)

data SearchHint = Forward | Backward | Insert | Delete | Initial
    deriving (Eq)

instance Ord Modifier
    where compare a b = compare (fromEnum a) (fromEnum b)

--
-- | Other types
--
data ErrorSpec = ErrorSpec {
    filePath            ::   FilePath
,   line                ::   Int
,   column              ::   Int
,   errDescription      ::   String
,   logLines            ::   (Int,Int)
,   isError             ::   Bool
}   deriving Show

-- ---------------------------------------------------------------------
--  | Information about the world, extraced from .hi and maybe source files
--

newtype Present alpha       =   Present alpha

type PackageScope       =   (Map PackageIdentifier PackageDescr,SymbolTable)
type SymbolTable        =   Map Symbol [Descr]

data PackageDescr       =   PackageDescr {
    packagePD           ::   PackageIdentifier
,   mbSourcePathPD      ::   (Maybe FilePath)
,   exposedModulesPD    ::   [ModuleDescr]
,   buildDependsPD      ::   [PackageIdentifier]
} deriving (Show,Typeable)

instance Show (Present PackageDescr) where
    show (Present pd)   =   (fromPackageIdentifier . packagePD) pd

instance Eq PackageDescr where
    (== ) a b             =   packagePD a == packagePD b

instance Ord PackageDescr where
    (<=) a b             =   packagePD a <=  packagePD b

data ModuleDescr        =   ModuleDescr {
    moduleIdMD          ::   PackModule
,   mbSourcePathMD      ::   (Maybe FilePath)
,   exportedNamesMD     ::   (Set Symbol)                        -- unqualified
,   usagesMD            ::   (Map ModuleName (Set Symbol)) -- imports
,   idDescriptionsMD    ::   [Descr]
} deriving (Show,Typeable)

instance Show (Present ModuleDescr) where
    show (Present md)   =   (show . moduleIdMD) md

instance Eq ModuleDescr where
    (== ) a b             =   moduleIdMD a == moduleIdMD b

instance Ord ModuleDescr where
    (<=) a b             =   moduleIdMD a <=  moduleIdMD b

data Descr              =   Descr {
    descrName'          ::   Symbol
,   typeInfo'           ::   TypeInfo
,   descrModu'          ::   PackModule
,   mbLocation'         ::   Maybe Location
,   mbComment'          ::   Maybe ByteString
,   details'            ::   SpDescr}
    | Reexported {
    descrModu'          ::   PackModule
,   impDescr            ::   Descr}
    deriving (Show,Read,Typeable)

instance Show (Present Descr) where
    showsPrec _ (Present descr) =   case mbComment descr of
                                        Just comment -> p . showChar '\n' . c comment . t
                                        Nothing      -> p . showChar '\n' . t
        where p         =   showString "-- | " . shows (Present (descrModu' descr))
              c com     =   showString $ (unlines . map ((++) "-- ") .  nonEmptyLines) (BS.unpack com)
              t         =   showString $ BS.unpack (typeInfo descr)


isReexported :: Descr -> Bool
isReexported (Reexported _ _)   =   True
isReexported _                  =   False

descrName :: Descr -> Symbol
descrName d
    |   isReexported d  =   descrName (impDescr d)
    |   otherwise       =   descrName' d

typeInfo :: Descr -> TypeInfo
typeInfo d
    |   isReexported d  =   typeInfo (impDescr d)
    |   otherwise       =   typeInfo' d

descrModu :: Descr -> PackModule
descrModu d
    |   isReexported d  =   descrModu (impDescr d)
    |   otherwise       =   descrModu' d

mbLocation :: Descr -> Maybe Location
mbLocation d
    |   isReexported d  =   mbLocation (impDescr d)
    |   otherwise       =   mbLocation' d

mbComment :: Descr -> Maybe ByteString
mbComment d
    |   isReexported d  =   mbComment (impDescr d)
    |   otherwise       =   mbComment' d

details :: Descr -> SpDescr
details d
    |   isReexported d  =   details (impDescr d)
    |   otherwise       =   details' d


data SpDescr   =   VariableDescr
                    |   FieldDescr {typeDescrF :: Descr}
                    |   ConstructorDescr {typeDescrC :: Descr}
                    |   DataDescr {constructors :: [(Symbol,TypeInfo)],
                            fields :: [(Symbol,TypeInfo)]}
                    |   TypeDescr
                    |   NewtypeDescr {constructor :: (Symbol,TypeInfo),
                            mbField :: Maybe (Symbol,TypeInfo)}
                    |   ClassDescr  {super :: [Symbol], methods :: [(Symbol,TypeInfo)]}
                    |   MethodDescr {classDescrM :: Descr}
                    |   InstanceDescr {binds :: [Symbol]}
                            --the descrName is the type Konstructor?
    deriving (Show,Read,Eq,Ord,Typeable)

instance Eq Descr where
    (== ) a b             =   descrName a == descrName b
                                && descrType (details a)   == descrType (details b)

instance Ord Descr where
    (<=) a b             =   if descrName a == descrName b
                                then descrType (details a)   <= descrType (details b)
                                else descrName a <  descrName b

data DescrType = Variable | Field | Constructor | Data  | Type | Newtype
    | Class | Method | Instance
  deriving (Show, Eq, Ord, Bounded, Enum, Read)

instance Default DescrType where
    getDefault = Variable

descrType ::  SpDescr -> DescrType
descrType VariableDescr      =   Variable
descrType (FieldDescr _)     =   Field
descrType (ConstructorDescr _) = Constructor
descrType (DataDescr _ _)    =   Data
descrType TypeDescr          =   Type
descrType (NewtypeDescr _ _) =   Newtype
descrType (ClassDescr  _ _)  =   Class
descrType (MethodDescr _)    =   Method
descrType (InstanceDescr _)  =   Instance

stockIdFromType :: DescrType -> StockId
stockIdFromType Variable        =   "ide_function"
stockIdFromType Newtype         =   "ide_newtype"
stockIdFromType Type            =   "ide_type"
stockIdFromType Data            =   "ide_data"
stockIdFromType Class           =   "ide_class"
stockIdFromType Instance        =   "ide_instance"
stockIdFromType Constructor     =   "ide_konstructor"
stockIdFromType Field           =   "ide_slot"
stockIdFromType Method          =   "ide_method"

type Symbol             =   String  -- Qualified or unqualified
type ClassId            =   String  -- Qualified or unqualified
type DataId             =   String  -- Qualified or unqualified
type TypeInfo           =   ByteString
--type ModuleIdentifier   =   String  -- always qualified

data PackModule         =   PM {    pack :: PackageIdentifier
                                ,   modu :: ModuleName}
                                deriving (Eq, Ord,Read,Show,Typeable)

instance Show (Present PackModule) where
    showsPrec _ (Present pd)  =   showString ((fromPackageIdentifier . pack) pd) . showChar ':'
                                    .  showString (display (modu pd))

parsePackModule         ::   String -> PackModule
parsePackModule str     =   let (pack',mod') = span (\c -> c /= ':') str
                            in case toPackageIdentifier $ pack' of
                                Nothing -> perror $ "Types>>parsePackModule: Can't parse package:" ++ str
                                Just pi'-> case simpleParse $ tail mod' of
                                            Nothing -> perror $
                                                "Types>>parsePackModule: Can't parse module:" ++ str
                                            Just mn -> (PM pi' mn)
    where perror s      =   throwIDE $ "cannot parse PackModule from " ++ s

showPackModule :: PackModule -> String
showPackModule = show. Present

fromPackageIdentifier :: PackageIdentifier -> String
fromPackageIdentifier   =   display

toPackageIdentifier :: String -> Maybe PackageIdentifier
toPackageIdentifier         =   simpleParse

instance Default PackModule where
    getDefault = parsePackModule "unknow-0:Undefined"

data Location           =   Location {
    locationSLine       ::   Int
,   locationSCol	    ::   Int
,   locationELine       ::   Int
,   locationECol        ::   Int
}   deriving (Show,Eq,Ord,Read,Typeable)

instance Default ByteString
    where getDefault = BS.empty

data Scope = Local | Package | System
  deriving (Show, Eq, Ord, Enum, Read)

newtype CandyTable      =   CT (CandyTableForth,CandyTableBack)

type CandyTableForth    =   [(Bool,String,String)]

type CandyTableBack     =   [(String,String,Int)]

newtype KeymapI         =   KM  (Map ActionString
                                [(Maybe (Either KeyString (KeyString,KeyString)), Maybe String)])

type SpecialKeyTable alpha  =   Map (KeyVal,[Modifier]) (Map (KeyVal,[Modifier]) (ActionDescr alpha))

type SpecialKeyCons  alpha  =   Maybe ((Map (KeyVal,[Modifier]) (ActionDescr alpha)),String)

data LogTag = LogTag | ErrorTag | FrameTag

-- | the first one is the new and the second the old state
type GUIHistory = (GUIHistory', GUIHistory')

data GUIHistory' =
        ModuleSelected  {
            moduleS :: Maybe ModuleName
        ,   facetS  :: Maybe Symbol}
    |   ScopeSelected {
            scope   :: Scope
        ,   blacklist :: Bool}
    |   InfoElementSelected {
            info    :: Descr}
    |   PaneSelected {
            paneN   :: Maybe (String)}
   deriving (Eq, Ord, Show)

data SensitivityMask =
        SensitivityForwardHist
    |   SensitivityBackwardHist
    |   SensitivityProjectActive
    |   SensitivityError
    |   SensitivityEditor

   deriving (Eq, Ord, Show)

data SearchMode = Exact {caseSense :: Bool} | Prefix {caseSense :: Bool}
                | Regex {caseSense :: Bool}
    deriving (Eq,Ord,Read,Show)




