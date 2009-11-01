{-# OPTIONS_GHC
    -XDisambiguateRecordFields
    -XExistentialQuantification
    -XRank2Types
    -XFlexibleInstances
    -XDeriveDataTypeable
    -XFlexibleContexts
    -XDeriveDataTypeable
    -XTypeSynonymInstances
    -XMultiParamTypeClasses #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.Data
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | The core state of ide. This module is imported from every other module,
-- | and all data structures of the state are declared here, to avoid circular
-- | module dependencies.
--
-------------------------------------------------------------------------------

module IDE.Core.Types (
    IDE(..)
,   IDEState(..)
,   IDERef
,   IDEM
,   IDEAction
,   IDEEvent(..)

,   IDEPackage(..)
,   Workspace(..)

,   ActionDescr(..)
,   ActionString
,   KeyString

,   Prefs(..)

,   LogRefType(..)
,   LogRef(..)
,   filePath
,   isError
,   isBreakpoint
,   colorHexString

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

,   CompletionWindow(..)
,   StatusbarCompartment(..)
) where

import Control.Monad.Reader
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Gdk.Events(Modifier(..))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Default
import IDE.Exception
import Graphics.UI.Frame.Panes
import Distribution.Package
    (PackageIdentifier(..), Dependency(..))
import Distribution.Text
import Data.Map (Map(..))
import Distribution.ModuleName(ModuleName)
import Distribution.Text
import qualified Data.ByteString.Char8 as BS  (unpack,empty)
import Data.ByteString.Char8 (ByteString)
import MyMissing
import Data.Typeable (Typeable(..))
import SrcLoc (SrcSpan(..))
import Outputable (ppr, showSDoc)
import Data.Set (Set(..))
import Data.Unique (newUnique, Unique(..))
import System.Process (ProcessHandle(..))
import IDE.Tool (ToolState(..))
import Data.IORef (writeIORef, readIORef, IORef(..))
import FastString (unpackFS)
import Numeric (showHex)
import Control.Event
    (EventSelector(..), EventSource(..), Event(..))

-- ---------------------------------------------------------------------
-- IDE State
--

--
-- | The IDE state
--
data IDE            =  IDE {
    frameState      ::   FrameState IDEM         -- ^ state of the windows framework
,   recentPanes     ::   [PaneName]
,   specialKeys     ::   SpecialKeyTable IDERef  -- ^ a structure for emacs like keystrokes
,   specialKey      ::   SpecialKeyCons IDERef   -- ^ the first of a double keystroke
,   candy           ::   CandyTable              -- ^ table for source candy
,   prefs           ::   Prefs                   -- ^ configuration preferences
,   workspace       ::   Maybe Workspace         -- ^ may be a workspace (set of packages)
,   activePack      ::   Maybe IDEPackage
,   bufferProjectCache ::   Map FilePath (Maybe IDEPackage)
,   allLogRefs      ::   [LogRef]
,   currentEBC      ::   (Maybe LogRef, Maybe LogRef, Maybe LogRef)
,   currentHist     ::   Int
,   systemInfo      ::   (Maybe (PackageScope))                           -- ^ the system scope
,   packageInfo     ::   (Maybe (PackageScope,PackageScope))              -- ^ the second are the imports
,   workspaceInfo   ::   (Maybe (PackageScope,PackageScope))              -- ^ the second are the imports
,   handlers        ::   Map String [(Unique, IDEEvent -> IDEM IDEEvent)] -- ^ event handling table
,   currentState    ::   IDEState
,   guiHistory      ::   (Bool,[GUIHistory],Int)
,   findbar         ::   (Bool,Maybe (Toolbar,ListStore String))
,   toolbar         ::   (Bool,Maybe Toolbar)
,   recentFiles     ::   [FilePath]
,   recentWorkspaces ::  [FilePath]
,   runningTool     ::   Maybe ProcessHandle
,   ghciState       ::   Maybe ToolState
,   completion      ::   Maybe CompletionWindow
#ifdef YI
,   yiControl       ::   Yi.Control
#endif
} --deriving Show

--
-- | A mutable reference to the IDE state
--
type IDERef = IORef IDE

--
-- The IDE Monad
--
type IDEM = ReaderT IDERef IO

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
--   which does not return a value
--
type IDEAction = IDEM ()


data IDEState =
        IsStartingUp
    |   IsShuttingDown
    |   IsRunning
    |   IsFlipping TreeView
    |   IsCompleting Connections

-- ---------------------------------------------------------------------
-- Events which can be signalled and handled
--

data IDEEvent  =
        InfoChanged
    |   SystemInfoChanged
    |   ActivePack (Maybe IDEPackage)
    |   SelectInfo String
    |   SelectIdent Descr
    |   LogMessage String LogTag
    |   RecordHistory GUIHistory
    |   Sensitivity [(SensitivityMask,Bool)]
    |   DescrChoice [Descr]
    |   SearchMeta String
    |   LoadSession FilePath
    |   SaveSession FilePath
    |   UpdateRecent
    |   VariablesChanged
    |   ErrorChanged
    |   CurrentErrorChanged (Maybe LogRef)
    |   BreakpointChanged
    |   CurrentBreakChanged (Maybe LogRef)
    |   TraceChanged
    |   GetTextPopup (Maybe (IDERef -> Menu -> IO ()))
    |   StatusbarChanged [StatusbarCompartment]
    |   WorkspaceChanged
    |   WorkspaceAddPackage FilePath

instance Event IDEEvent String where
    getSelector InfoChanged             =   "InfoChanged"
    getSelector SystemInfoChanged       =   "SystemInfoChanged"
    getSelector (ActivePack _)          =   "ActivePack"
    getSelector (LogMessage _ _)        =   "LogMessage"
    getSelector (SelectInfo _)          =   "SelectInfo"
    getSelector (SelectIdent _)         =   "SelectIdent"
    getSelector (RecordHistory _)       =   "RecordHistory"
    getSelector (Sensitivity _)         =   "Sensitivity"
    getSelector (DescrChoice _)         =   "DescrChoice"
    getSelector (SearchMeta _)          =   "SearchMeta"
    getSelector (LoadSession _)         =   "LoadSession"
    getSelector (SaveSession _)         =   "SaveSession"
    getSelector UpdateRecent            =   "UpdateRecent"
    getSelector VariablesChanged        =   "VariablesChanged"
    getSelector ErrorChanged            =   "ErrorChanged"
    getSelector (CurrentErrorChanged _) =   "CurrentErrorChanged"
    getSelector BreakpointChanged       =   "BreakpointChanged"
    getSelector (CurrentBreakChanged _) =   "CurrentBreakChanged"
    getSelector TraceChanged            =   "TraceChanged"
    getSelector (GetTextPopup _)        =   "GetTextPopup"
    getSelector (StatusbarChanged _)    =   "StatusbarChanged"
    getSelector WorkspaceChanged        =   "WorkspaceChanged"
    getSelector (WorkspaceAddPackage _) =   "WorkspaceAddPackage"

instance EventSource IDERef IDEEvent IDEM String where
    canTriggerEvent _ "InfoChanged"         = True
    canTriggerEvent _ "SystemInfoChanged"   = True
    canTriggerEvent _ "ActivePack"          = True
    canTriggerEvent _ "LogMessage"          = True
    canTriggerEvent _ "SelectInfo"          = True
    canTriggerEvent _ "SelectIdent"         = True
    canTriggerEvent _ "RecordHistory"       = True
    canTriggerEvent _ "Sensitivity"         = True
    canTriggerEvent _ "DescrChoice"         = True
    canTriggerEvent _ "SearchMeta"          = True
    canTriggerEvent _ "LoadSession"         = True
    canTriggerEvent _ "SaveSession"         = True
    canTriggerEvent _ "UpdateRecent"        = True
    canTriggerEvent _ "VariablesChanged"    = True
    canTriggerEvent _ "ErrorChanged"        = True
    canTriggerEvent _ "CurrentErrorChanged" = True
    canTriggerEvent _ "BreakpointChanged"   = True
    canTriggerEvent _ "CurrentBreakChanged" = True
    canTriggerEvent _ "TraceChanged"        = True
    canTriggerEvent _ "GetTextPopup"        = True
    canTriggerEvent _ "StatusbarChanged"    = True
    canTriggerEvent _ "WorkspaceChanged"    = True
    canTriggerEvent _ "WorkspaceAddPackage" = True
    canTriggerEvent _ _                   = False
    getHandlers ideRef = do
        ide <- liftIO $ readIORef ideRef
        return (handlers ide)
    setHandlers ideRef nh = do
        ide <- liftIO $ readIORef ideRef
        liftIO $ writeIORef ideRef (ide {handlers= nh})
    myUnique _ = do
        liftIO $ newUnique

instance EventSelector String

-- ---------------------------------------------------------------------
-- IDEPackages
--
data IDEPackage     =   IDEPackage {
    packageId       ::   PackageIdentifier
,   cabalFile       ::   FilePath
,   depends         ::   [Dependency]
,   modules         ::   Set ModuleName
,   extraSrcs       ::   Set FilePath
,   srcDirs         ::   [FilePath]
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
-- Workspace
--
data Workspace = Workspace {
    wsVersion       ::   Int
,   wsSaveTime      ::   String
,   wsName          ::   String
,   wsFile          ::   FilePath
,   wsPackages      ::   [IDEPackage]
,   wsActivePack    ::   Maybe IDEPackage
,   wsPackagesFiles ::   [FilePath]
,   wsActivePackFile::   Maybe FilePath
} deriving Show

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
        prefsFormat         ::   Int
    ,   prefsSaveTime       ::   String
    ,   showLineNumbers     ::   Bool
    ,   rightMargin         ::   Maybe Int
    ,   tabWidth            ::   Int
    ,   sourceCandy         ::   Maybe String
    ,   keymapName          ::   String
    ,   forceLineEnds       ::   Bool
    ,   removeTBlanks       ::   Bool
    ,   textviewFont        ::   Maybe String
    ,   sourceStyle         ::   Maybe String
    ,   foundBackground     ::   Color
    ,   contextBackground   ::   Color
    ,   breakpointBackground ::  Color
    ,   useYi               ::   Bool
    ,   logviewFont         ::   Maybe String
    ,   defaultSize         ::   (Int,Int)
    ,   browser             ::   String
    ,   pathForCategory     ::   [(String, PanePath)]
    ,   defaultPath         ::   PanePath
    ,   categoryForPane     ::   [(String, String)]
    ,   sourceDirectories   ::   [FilePath]
    ,   packageBlacklist    ::   [Dependency]
    ,   collectAfterBuild   ::   Bool
    ,   collectAtStart      ::   Bool
    ,   autoExtractTars     ::   Bool
    ,   useCtrlTabFlipping  ::   Bool
    ,   docuSearchURL       ::   String
    ,   completeRestricted  ::   Bool
    ,   saveAllBeforeBuild  ::   Bool
    ,   backgroundBuild     ::   Bool
    ,   backgroundLink      ::   Bool
    ,   printEvldWithShow   ::   Bool
    ,   breakOnException    ::   Bool
    ,   breakOnError        ::   Bool
    ,   printBindResult     ::   Bool
} deriving(Eq,Show)

data SearchHint = Forward | Backward | Insert | Delete | Initial
    deriving (Eq)

instance Ord Modifier
    where compare a b = compare (fromEnum a) (fromEnum b)

--
-- | Other types
--
data LogRefType = WarningRef | ErrorRef | BreakpointRef | ContextRef deriving (Eq, Show)

data LogRef = LogRef {
    logRefSrcSpan       ::   SrcSpan
,   refDescription      ::   String
,   logLines            ::   (Int,Int)
,   logRefType          ::   LogRefType
}   deriving (Eq)

instance Show LogRef where
    show lr =  refDescription lr ++ showSDoc (ppr (logRefSrcSpan lr))

filePath :: LogRef -> FilePath
filePath = unpackFS . srcSpanFile. logRefSrcSpan

isError :: LogRef -> Bool
isError = (== ErrorRef) . logRefType

isBreakpoint :: LogRef -> Bool
isBreakpoint = (== BreakpointRef) . logRefType

isContext :: LogRef -> Bool
isContext = (== ContextRef) . logRefType

-- This should probably be in Gtk2Hs allong with a suitable parser
colorHexString (Color r g b) = '#' : (pad $ showHex r "")
                                  ++ (pad $ showHex g "")
                                  ++ (pad $ showHex b "")
    where pad s = replicate (4 - length s) '0' ++ s

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
    (== ) a b            =   packagePD a == packagePD b

instance Ord PackageDescr where
    (<=) a b              =   packagePD a <=  packagePD b

data ModuleDescr        =   ModuleDescr {
        moduleIdMD          ::   PackModule
    ,   mbSourcePathMD      ::   (Maybe FilePath)
    ,   exportedNamesMD     ::   (Set Symbol)                        -- unqualified
    ,   referencesMD        ::   (Map ModuleName (Set Symbol)) -- imports
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
    ,   typeInfo'           ::   Maybe TypeInfo
    ,   descrModu'          ::   Maybe PackModule
    ,   mbLocation'         ::   Maybe Location
    ,   mbComment'          ::   Maybe ByteString
    ,   details'            ::   SpDescr}
        | Reexported {
        descrModu'          ::   Maybe PackModule
    ,   impDescr            ::   Descr}
        deriving (Show,Read,Typeable)

data SpDescr   =   VariableDescr
    |   FieldDescr {typeDescrF :: Descr}
    |   ConstructorDescr {typeDescrC :: Descr}
    |   DataDescr {constructors :: [(Symbol,Maybe TypeInfo)],
            fields :: [(Symbol,Maybe TypeInfo)]}
    |   TypeDescr
    |   NewtypeDescr {constructor :: (Symbol,Maybe TypeInfo),
            mbField :: Maybe (Symbol,Maybe TypeInfo)}
    |   ClassDescr  {super :: [Symbol], methods :: [(Symbol,Maybe TypeInfo)]}
    |   MethodDescr {classDescrM :: Descr}
    |   InstanceDescr {binds :: [Symbol]}
    |   KeywordDescr
    |   ExtensionDescr
    |   ModNameDescr
    |   QualModNameDescr
            --the descrName is the type Konstructor?
        deriving (Show,Read,Eq,Ord,Typeable)

data DescrType = Variable | Field | Constructor | Data  | Type | Newtype
    | Class | Method | Instance | Keyword | Extension | ModName | QualModName
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
descrType KeywordDescr       =   Keyword
descrType ExtensionDescr     =   Extension
descrType ModNameDescr       =   ModName
descrType QualModNameDescr   =   QualModName

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
stockIdFromType _               =   "ide_other"

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

-- Metadata accessors

isReexported :: Descr -> Bool
isReexported (Reexported _ _)   =   True
isReexported _                  =   False

descrName :: Descr -> Symbol
descrName d
    |   isReexported d  =   descrName (impDescr d)
    |   otherwise       =   descrName' d

typeInfo :: Descr -> Maybe TypeInfo
typeInfo d
    |   isReexported d  =   typeInfo (impDescr d)
    |   otherwise       =   typeInfo' d

descrModu :: Descr -> Maybe PackModule
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

instance Show (Present Descr) where
    showsPrec _ (Present descr) =   case mbComment descr of
                                        Just comment -> p . showChar '\n' . c comment . t
                                        Nothing      -> p . showChar '\n' . showChar '\n' . t
        where p         =   case descrModu' descr of
                                Just ds -> showString "-- " . shows (Present ds)
                                Nothing -> id
              c com     =   showString $ unlines
                                $ map (\(i,l) -> if i == 0 then "-- | " ++ l else "--  " ++ l)
                                    $ zip [0 .. length lines - 1] lines
                                where lines = nonEmptyLines (BS.unpack com)
              t         =   case typeInfo descr of
                                Just ti -> showString $ BS.unpack ti
                                Nothing -> id

instance Eq Descr where
    (== ) a b             =   descrName a == descrName b
                                && descrType (details a)   == descrType (details b)

instance Ord Descr where
    (<=) a b             =   if descrName a == descrName b
                                then descrType (details a)   <= descrType (details b)
                                else descrName a <  descrName b

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

data Scope = PackageScope Bool | WorkspaceScope Bool | SystemScope
    -- True -> with imports, False -> without imports
  deriving (Show, Eq, Ord, Read)

newtype CandyTable      =   CT (CandyTableForth,CandyTableBack)

type CandyTableForth    =   [(Bool,String,String)]

type CandyTableBack     =   [(String,String,Int)]

newtype KeymapI         =   KM  (Map ActionString
                                [(Maybe (Either KeyString (KeyString,KeyString)), Maybe String)])

type SpecialKeyTable alpha  =   Map (KeyVal,[Modifier]) (Map (KeyVal,[Modifier]) (ActionDescr alpha))

type SpecialKeyCons  alpha  =   Maybe ((Map (KeyVal,[Modifier]) (ActionDescr alpha)),String)

data LogTag = LogTag | ErrorTag | FrameTag | InputTag | InfoTag

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
            mbInfo  :: Maybe Descr}
    |   PaneSelected {
            paneN   :: Maybe (String)}
   deriving (Eq, Ord, Show)

data SensitivityMask =
        SensitivityForwardHist
    |   SensitivityBackwardHist
    |   SensitivityProjectActive
    |   SensitivityWorkspaceOpen
    |   SensitivityError
    |   SensitivityEditor
    |   SensitivityInterpreting

   deriving (Eq, Ord, Show)

data SearchMode = Exact {caseSense :: Bool} | Prefix {caseSense :: Bool}
                | Regex {caseSense :: Bool}
    deriving (Eq,Ord,Read,Show)

data CompletionWindow = CompletionWindow {
    cwWindow :: Window,
    cwTreeView :: TreeView,
    cwListStore :: ListStore String}

data StatusbarCompartment =
        CompartmentCommand String
    |   CompartmentPane (Maybe (IDEPane IDEM))
    |   CompartmentPackage String
    |   CompartmentState String
    |   CompartmentOverlay Bool
    |   CompartmentBufferPos (Int,Int)




