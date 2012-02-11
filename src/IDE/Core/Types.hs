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
,   liftIDE
,   (?>>=)

,   WorkspaceM
,   WorkspaceAction
,   runWorkspace

,   PackageM
,   PackageAction
,   runPackage

,   DebugM
,   DebugAction
,   runDebug

,   IDEPackage(..)
,   Workspace(..)

,   ActionDescr(..)
,   ActionString
,   KeyString

,   Prefs(..)

,   LogRefType(..)
,   LogRef(..)
,   logRefFilePath
,   logRefFullFilePath
,   isError
,   isBreakpoint
,   displaySrcSpan
,   colorHexString

,   SearchHint(..)
,   CandyTable(..)
,   CandyTableForth
,   CandyTableBack
,   KeymapI(..)
,   SpecialKeyTable
,   SpecialKeyCons

,   PackageDescrCache
,   ModuleDescrCache

,   CompletionWindow(..)
,   LogTag(..)
,   GUIHistory
,   GUIHistory'(..)
,   SensitivityMask(..)
,   SearchMode(..)
,   StatusbarCompartment(..)
) where

import qualified IDE.YiConfig as Yi
import Graphics.UI.Gtk
       (Window(..), KeyVal(..), Color(..), Menu(..), TreeView(..),
        ListStore(..), Toolbar(..))
import Data.Unique (newUnique, Unique(..))
import Graphics.UI.Frame.Panes
import Distribution.Package
    (PackageIdentifier(..), Dependency(..))
import Distribution.PackageDescription (BuildInfo)
import Data.Map (Map(..))
import Data.Set (Set(..))
import Distribution.ModuleName (ModuleName(..))
#if MIN_VERSION_gtk(0,10,5)
import Graphics.UI.Gtk.Gdk.EventM (Modifier(..))
#else
import Graphics.UI.Gtk.Gdk.Enums (Modifier(..))
#endif
import System.Time (ClockTime(..))
import Distribution.Simple (Extension(..))
#ifdef MIN_VERSION_process_leksah
import IDE.System.Process (ProcessHandle(..))
#else
import System.Process (ProcessHandle(..))
#endif
import IDE.Utils.Tool (ToolState(..))
import Data.IORef (writeIORef, readIORef, IORef(..))
import Numeric (showHex)
import Control.Event
    (EventSelector(..), EventSource(..), Event(..))
import System.FilePath (dropFileName, (</>))
import IDE.Core.CTypes
import IDE.StrippedPrefs(RetrieveStrategy)
import System.IO (Handle)
import Distribution.Text(disp)
import Text.PrettyPrint (render)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))

-- ---------------------------------------------------------------------
-- IDE State
--

--
-- | The IDE state
--
data IDE            =  IDE {
    frameState      ::   FrameState IDEM         -- ^ state of the windows framework
,   recentPanes     ::   [PaneName]              -- ^ a list of panes which were selected last
,   specialKeys     ::   SpecialKeyTable IDERef  -- ^ a structure for emacs like keystrokes
,   specialKey      ::   SpecialKeyCons IDERef   -- ^ the first of a double keystroke
,   candy           ::   CandyTable              -- ^ table for source candy
,   prefs           ::   Prefs                   -- ^ configuration preferences
,   workspace       ::   Maybe Workspace         -- ^ may be a workspace (set of packages)
,   activePack      ::   Maybe IDEPackage
,   bufferProjCache ::   Map FilePath (Maybe IDEPackage)
,   allLogRefs      ::   [LogRef]
,   currentEBC      ::   (Maybe LogRef, Maybe LogRef, Maybe LogRef)
,   currentHist     ::   Int
,   systemInfo      ::   (Maybe GenScope)              -- ^ the system scope
,   packageInfo     ::   (Maybe (GenScope, GenScope)) -- ^ the second are the imports
,   workspaceInfo   ::   (Maybe (GenScope, GenScope)) -- ^ the second are the imports
,   workspInfoCache ::   PackageDescrCache
,   handlers        ::   Map String [(Unique, IDEEvent -> IDEM IDEEvent)] -- ^ event handling table
,   currentState    ::   IDEState
,   guiHistory      ::   (Bool,[GUIHistory],Int)
,   findbar         ::   (Bool,Maybe (Toolbar,ListStore String))
,   toolbar         ::   (Bool,Maybe Toolbar)
,   recentFiles     ::   [FilePath]
,   recentWorkspaces ::  [FilePath]
,   runningTool     ::   Maybe ProcessHandle
,   debugState      ::   Maybe (IDEPackage, ToolState)
,   completion      ::   ((Int, Int), Maybe CompletionWindow)
,   yiControl       ::   Yi.Control
,   server          ::   Maybe Handle
} --deriving Show

--
-- | A mutable reference to the IDE state
--
type IDERef = IORef IDE

--
-- | The IDE Monad
--
type IDEM = ReaderT IDERef IO

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
--   which does not return a value
--
type IDEAction = IDEM ()


data IDEState =
        -- | Leksah is in startup mode
        IsStartingUp
        -- | Leksah is about to go down
    |   IsShuttingDown
        -- | Leksah is running
    |   IsRunning
        -- | The flipper is used to switch between sources
    |   IsFlipping TreeView
        -- | The completion feature is used
    |   IsCompleting Connections


liftIDE :: IDEM a -> WorkspaceM a
liftIDE = lift

(?>>=) :: Monad m => (m (Maybe a)) -> (a -> m ()) -> m ()
a ?>>= b = do
    mA <- a
    case mA of
        Just v -> b v
        Nothing -> return ()

-- ---------------------------------------------------------------------
-- Monad for functions that need an open workspace
--
type WorkspaceM = ReaderT Workspace IDEM
type WorkspaceAction = WorkspaceM ()

runWorkspace :: WorkspaceM a -> Workspace -> IDEM a
runWorkspace = runReaderT

-- ---------------------------------------------------------------------
-- Monad for functions that need an active package
--
type PackageM = ReaderT IDEPackage IDEM
type PackageAction = PackageM ()

runPackage :: PackageM a -> IDEPackage -> IDEM a
runPackage = runReaderT

-- ---------------------------------------------------------------------
-- Monad for functions that need to use the GHCi debugger
--
type DebugM = ReaderT (IDEPackage, ToolState) IDEM
type DebugAction = DebugM ()

runDebug :: DebugM a -> (IDEPackage, ToolState) -> IDEM a
runDebug = runReaderT

-- ---------------------------------------------------------------------
-- Events which can be signalled and handled
--

data IDEEvent  =
        InfoChanged Bool-- is it the initial = True else False
    |   UpdateWorkspaceInfo
    |   SelectInfo String Bool -- navigate to source (== True)
    |   SelectIdent Descr
    |   LogMessage String LogTag
    |   RecordHistory GUIHistory
    |   Sensitivity [(SensitivityMask,Bool)]
    |   SearchMeta String
    |   StartFindInitial
    |   GotoDefinition Descr
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
    |   WorkspaceChanged Bool Bool -- ^ showPane updateFileCache

instance Event IDEEvent String where
    getSelector (InfoChanged _)         =   "InfoChanged"
    getSelector UpdateWorkspaceInfo     =   "UpdateWorkspaceInfo"
    getSelector (LogMessage _ _)        =   "LogMessage"
    getSelector (SelectInfo _ _)        =   "SelectInfo"
    getSelector (SelectIdent _)         =   "SelectIdent"
    getSelector (RecordHistory _)       =   "RecordHistory"
    getSelector (Sensitivity _)         =   "Sensitivity"
    getSelector (SearchMeta _)          =   "SearchMeta"
    getSelector (StartFindInitial)      =   "StartFindInitial"
    getSelector (GotoDefinition _)      =   "GotoDefinition"
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
    getSelector (WorkspaceChanged _ _)  =   "WorkspaceChanged"

instance EventSource IDERef IDEEvent IDEM String where
    canTriggerEvent _ "InfoChanged"         = True
    canTriggerEvent _ "UpdateWorkspaceInfo" = True
    canTriggerEvent _ "LogMessage"          = True
    canTriggerEvent _ "SelectInfo"          = True
    canTriggerEvent _ "SelectIdent"         = True
    canTriggerEvent _ "RecordHistory"       = True
    canTriggerEvent _ "Sensitivity"         = True
    canTriggerEvent _ "DescrChoice"         = True
    canTriggerEvent _ "SearchMeta"          = True
    canTriggerEvent _ "StartFindInitial"    = True
    canTriggerEvent _ "SearchSymbolDialog"  = True
    canTriggerEvent _ "GotoDefinition"      = True
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
    ipdPackageId       ::   PackageIdentifier
,   ipdCabalFile       ::   FilePath
,   ipdDepends         ::   [Dependency]
,   ipdModules         ::   Map ModuleName BuildInfo
,   ipdHasLibs         ::   Bool
,   ipdTests           ::   [String]
,   ipdMain            ::   [(FilePath, BuildInfo, Bool)]
,   ipdExtraSrcs       ::   Set FilePath
,   ipdSrcDirs         ::   [FilePath]
,   ipdExtensions      ::   [Extension]
,   ipdConfigFlags     ::   [String]
,   ipdBuildFlags      ::   [String]
,   ipdTestFlags       ::   [String]
,   ipdHaddockFlags    ::   [String]
,   ipdExeFlags        ::   [String]
,   ipdInstallFlags    ::   [String]
,   ipdRegisterFlags   ::   [String]
,   ipdUnregisterFlags ::   [String]
,   ipdSdistFlags      ::   [String]
}
    deriving (Eq)

instance Show IDEPackage where
    show p = show "IDEPackage for " ++ (render . disp) (ipdPackageId p)

instance Ord IDEPackage where
    compare x y     =   compare (ipdPackageId x) (ipdPackageId y)

-- ---------------------------------------------------------------------
-- Workspace
--
data Workspace = Workspace {
    wsVersion       ::   Int
,   wsSaveTime      ::   String
,   wsName          ::   String
,   wsFile          ::   FilePath
,   wsPackages      ::   [IDEPackage]
,   wsPackagesFiles ::   [FilePath]
,   wsActivePackFile::   Maybe FilePath
,   wsNobuildPack   ::   [IDEPackage]
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
    ,   rightMargin         ::   (Bool, Int)
    ,   tabWidth            ::   Int
    ,   wrapLines           ::   Bool
    ,   sourceCandy         ::   (Bool,String)
    ,   keymapName          ::   String
    ,   forceLineEnds       ::   Bool
    ,   removeTBlanks       ::   Bool
    ,   textviewFont        ::   Maybe String
    ,   sourceStyle         ::   (Bool, String)
    ,   foundBackground     ::   Color
    ,   contextBackground   ::   Color
    ,   breakpointBackground ::  Color
    ,   autoLoad            ::   Bool
    ,   useYi               ::   Bool
    ,   logviewFont         ::   Maybe String
    ,   defaultSize         ::   (Int,Int)
    ,   browser             ::   String
    ,   pathForCategory     ::   [(String, PanePath)]
    ,   defaultPath         ::   PanePath
    ,   categoryForPane     ::   [(String, String)]
    ,   packageBlacklist    ::   [Dependency]
    ,   collectAtStart      ::   Bool
    ,   useCtrlTabFlipping  ::   Bool
    ,   docuSearchURL       ::   String
    ,   completeRestricted  ::   Bool
    ,   saveAllBeforeBuild  ::   Bool
    ,   jumpToWarnings      ::   Bool
    ,   backgroundBuild     ::   Bool
    ,   runUnitTests        ::   Bool
    ,   makeMode            ::   Bool
    ,   singleBuildWithoutLinking :: Bool
    ,   dontInstallLast     ::   Bool
    ,   printEvldWithShow   ::   Bool
    ,   breakOnException    ::   Bool
    ,   breakOnError        ::   Bool
    ,   printBindResult     ::   Bool
    ,   serverIP            ::   String
            -- As well used by server
    ,   serverPort          ::   Int
    ,   sourceDirectories   ::   [FilePath]
    ,   unpackDirectory     ::   Maybe FilePath
    ,   retrieveURL         ::   String
    ,   retrieveStrategy    ::   RetrieveStrategy
    ,   endWithLastConn     ::   Bool
} deriving(Eq,Show)

data SearchHint = Forward | Backward | Insert | Delete | Initial
    deriving (Eq)

#ifndef LEKSAH_WITH_YI
instance Ord Modifier
    where compare a b = compare (fromEnum a) (fromEnum b)
#endif

--
-- | Other types
--
data LogRefType = WarningRef | ErrorRef | BreakpointRef | ContextRef deriving (Eq, Show)

data LogRef = LogRef {
    logRefSrcSpan       ::   SrcSpan
,   logRefPackage       ::   IDEPackage
,   refDescription      ::   String
,   logLines            ::   (Int,Int)
,   logRefType          ::   LogRefType
}   deriving (Eq)

instance Show LogRef where
    show lr =  refDescription lr ++ displaySrcSpan (logRefSrcSpan lr)

displaySrcSpan s = srcSpanFilename s ++ ":" ++
    if srcSpanStartLine s == srcSpanEndLine s
        then show (srcSpanStartLine s) ++ ":" ++
            if srcSpanStartColumn s == srcSpanEndColumn s
                then show (srcSpanStartColumn s)
                else show (srcSpanStartColumn s) ++ "-" ++ show (srcSpanEndColumn s)
        else show (srcSpanStartLine s) ++ ":" ++
            show (srcSpanStartColumn s) ++ "-" ++ show (srcSpanEndColumn s)

logRefRootPath :: LogRef -> FilePath
logRefRootPath = dropFileName . ipdCabalFile . logRefPackage

logRefFilePath :: LogRef -> FilePath
logRefFilePath = srcSpanFilename . logRefSrcSpan

logRefFullFilePath :: LogRef -- ^ The log ref
    -> FilePath -- ^ the result
logRefFullFilePath lr = logRefRootPath lr </> logRefFilePath lr

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
        ,   facetS  :: Maybe String}
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
    |   CompartmentBuild Bool
    |   CompartmentCollect Bool

type PackageDescrCache = Map PackageIdentifier ModuleDescrCache
type ModuleDescrCache = Map ModuleName (ClockTime, Maybe FilePath, ModuleDescr)

