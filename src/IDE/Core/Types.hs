{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
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
,   DebugState(..)
,   activeProject
,   activePack
,   activeComponent
,   nixEnv
,   IDEState(..)
,   IDERef
,   IDEM
,   IDEEventM
,   IDEAction
,   IDEEvent(..)
,   SymbolEvent(..)
,   MonadIDE
,   liftIDE
,   (?>>=)

,   WorkspaceM
,   WorkspaceAction
,   runWorkspace

,   ProjectM
,   ProjectAction
,   runProject

,   PackageM
,   PackageAction
,   runPackage

,   DebugM
,   DebugAction
,   runDebug

,   IDEPackage(..)
,   mkPackageMap
,   ipdPackageDir
,   ipdLib
,   ipdPackageName
,   ProjectTool(..)
,   Project(..)
,   pjPackages
,   pjLookupPackage
,   pjDir
,   pjToolCommand'
,   Workspace(..)
,   wsProjectFiles
,   wsLookupProject
,   wsActiveProject
,   wsActivePackage
,   wsPackages
,   wsProjectAndPackages
,   wsAllPackages
,   VCSConf

,   ActionDescr(..)
,   ActionString
,   KeyString

,   Prefs(..)
,   PrefsFile(..)
,   candyState
,   EditorStyle(..)
,   editorStyle

,   LogRefType(..)
,   Log(..)
,   logRootPath
,   LogRef(..)
,   logRefRootPath
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
,   TypeTip(..)
,   LogLaunch(..)
,   LogLaunchData(..)
,   LogTag(..)
,   GUIHistory
,   GUIHistory'(..)
,   SensitivityMask(..)
,   SearchMode(..)
,   StatusbarCompartment(..)

,   Color(..)
,   toGdkColor
,   fromGdkColor
,   KeyVal
) where

import Prelude ()
import Prelude.Compat
import qualified IDE.TextEditor.Yi.Config as Yi
import Data.Unique (newUnique, Unique(..))
import Graphics.UI.Frame.Panes
import Distribution.Package
       (PackageName(..), unPackageName, PackageIdentifier(..), Dependency(..))
import Distribution.PackageDescription (BuildInfo)
import Data.Map (Map(..))
import Data.Set (Set(..))
import Data.List (find, nubBy)
import Control.Concurrent (modifyMVar_, readMVar, MVar)
import Distribution.ModuleName (ModuleName(..))
import System.Time (ClockTime(..))
import Distribution.Simple (Extension(..))
import IDE.Utils.Tool (ToolState(..), ProcessHandle)
import Data.IORef (writeIORef, readIORef, IORef(..))
import Numeric (showHex)
import Control.Event
    (EventSelector(..), EventSource(..), Event(..))
import System.FilePath (dropFileName, (</>), isAbsolute, makeRelative)
import IDE.Core.CTypes
import IDE.StrippedPrefs(RetrieveStrategy)
import System.IO (Handle)
import Distribution.Text (display, disp)
import Text.PrettyPrint (render)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Time (UTCTime(..))

import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI
import qualified Data.Map as Map (Map)
import Data.Typeable (Typeable)
import Foreign (Ptr)
import Control.Monad.Reader.Class (MonadReader(..))
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Language.Haskell.HLint3 (Idea(..))
import Data.Function (on)
import Control.Concurrent.STM.TVar (TVar)
import Data.Sequence (Seq)
import Data.Maybe (maybeToList)
import GI.Gtk.Objects.Toolbar (Toolbar(..))
import Data.GI.Gtk.ModelView.SeqStore (SeqStore(..))
import GI.Gtk.Objects.MenuItem (MenuItem(..))
import GI.Gtk.Objects.TreeView (TreeView(..))
import GI.Gtk.Objects.Menu (Menu(..))
import GI.Gdk.Flags (ModifierType)
import GI.Gtk.Objects.TextBuffer (TextBuffer(..))
import Data.Word (Word32)
import GI.Gtk.Objects.Window (Window(..))
import Graphics.UI.Editor.Simple (Color(..), toGdkColor, fromGdkColor)
import GI.Gtk.Objects.Application (Application(..))
import Control.Monad ((>=>))
import System.FSNotify (StopListening, WatchManager)
import qualified Data.Map as M (fromList, lookup, keys, elems)
import System.Exit (ExitCode)
import Data.Int (Int32)
import System.Directory (doesFileExist)
import Control.Lens ((<&>))
import Distribution.Compiler (CompilerFlavor(..))
import System.Process (showCommandForUser)
import IDE.Utils.FileUtils (loadNixEnv)
import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)
import Data.Aeson.Types
       (genericParseJSON, genericToEncoding, genericToJSON,
        defaultOptions, fieldLabelModifier, Options)
import GI.Gtk (CssProvider)

-- ---------------------------------------------------------------------
-- IDE State
--

--
-- | The IDE state
--
data IDE            =  IDE {
    application         :: Application
,   exitCode            :: IORef ExitCode
,   frameState          :: FrameState IDEM         -- ^ state of the windows framework
,   specialKeys         :: SpecialKeyTable IDERef  -- ^ a structure for emacs like keystrokes
,   specialKey          :: SpecialKeyCons IDERef   -- ^ the first of a double keystroke
,   candy               :: CandyTable              -- ^ table for source candy
,   prefs               :: Prefs                   -- ^ configuration preferences
,   workspace           :: Maybe Workspace         -- ^ may be a workspace (set of packages)
,   bufferProjCache     :: Map FilePath [(Project, IDEPackage)] -- ^ cache the associated packages for a file
,   allLogRefs          :: Seq LogRef
,   currentEBC          :: (Maybe LogRef, Maybe LogRef, Maybe LogRef)
,   currentHist         :: Int
,   systemInfo          :: Maybe GenScope              -- ^ the system scope
,   packageInfo         :: Maybe (GenScope, GenScope) -- ^ the second are the imports
,   workspaceInfo       :: Maybe (GenScope, GenScope) -- ^ the second are the imports
,   workspInfoCache     :: PackageDescrCache
,   handlers            :: Map Text [(Unique, IDEEvent -> IDEM IDEEvent)] -- ^ event handling table
,   currentState        :: IDEState
,   flipper             :: Maybe TreeView             -- ^ used to select the active pane
,   typeTip             :: Maybe TypeTip
,   guiHistory          :: (Bool,[GUIHistory],Int)
,   findbar             :: (Bool,Maybe (Toolbar,SeqStore Text,CssProvider))
,   toolbar             :: (Bool,Maybe Toolbar)
,   recentFiles         :: [FilePath]
,   recentWorkspaces    :: [FilePath]
,   runningTool         :: Maybe (ProcessHandle, IO ())
,   debugState          :: [DebugState]
,   completion          :: ((Int, Int), Maybe CompletionWindow)
,   yiControl           :: Yi.Control
,   serverQueue         :: Maybe (MVar (ServerCommand, ServerAnswer -> IDEM ()))
,   server              :: Maybe Handle
,   hlintQueue          :: Maybe (TVar [Either FilePath FilePath])
,   vcsData             :: (Map FilePath MenuItem, Maybe (Maybe Text)) -- menus for packages, password
,   logLaunches         :: Map.Map Text LogLaunchData
,   autoCommand         :: ((FilePath, FilePath), IDEAction)
,   autoURI             :: Maybe Text
,   triggerBuild        :: MVar ()
,   fsnotify            :: WatchManager
,   watchers            :: MVar (Map FilePath StopListening, Map FilePath StopListening)
,   developLeksah       :: Bool -- If True leksah will exit when the `leksah` package is rebuilt
,   nixCache            :: Map (FilePath, Text) (Map String String)
,   externalModified    :: MVar (Set FilePath)
} --deriving Show

data DebugState = DebugState
    { dsProjectFile :: FilePath
    , dsPackages    :: [IDEPackage]
    , dsBasePath    :: FilePath
    , dsToolState   :: ToolState
    }

activeProject :: IDE -> Maybe Project
activeProject ide = workspace ide >>= wsActiveProject

activePack :: IDE -> Maybe IDEPackage
activePack ide = workspace ide >>= wsActivePackage

activeComponent :: IDE -> Maybe Text
activeComponent ide = workspace ide >>= wsActiveComponent

nixEnv :: FilePath -> Text -> IDE -> Maybe (Map String String)
nixEnv project compiler ide = M.lookup (project, compiler) $ nixCache ide

--
-- | A mutable reference to the IDE state
--
type IDERef = MVar IDE

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
        -- | The completion feature is used
    |   IsCompleting Connections


class (Applicative m, Monad m, MonadIO m) => MonadIDE m where
    liftIDE :: IDEM a -> m a

instance MonadIDE IDEM where
    liftIDE = id

instance MonadIDE WorkspaceM where
    liftIDE = lift

(?>>=) :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
a ?>>= b = do
    mA <- a
    case mA of
        Just v -> b v
        Nothing -> return ()

-- ---------------------------------------------------------------------
-- Monad for Gtk events (use onIDE instead of on)
--
type IDEEventM t = ReaderT IDERef (ReaderT t IO)

instance MonadIDE (IDEEventM t) where
    liftIDE f = do
        ideR <- ask
        liftIO $ runReaderT f ideR

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
type ProjectM = ReaderT Project WorkspaceM
type ProjectAction = ProjectM ()

instance MonadIDE ProjectM where
    liftIDE = lift . lift

runProject :: ProjectM a -> Project -> WorkspaceM a
runProject = runReaderT

-- ---------------------------------------------------------------------
-- Monad for functions that need an active package
--
type PackageM = ReaderT IDEPackage ProjectM
type PackageAction = PackageM ()

instance MonadIDE PackageM where
    liftIDE = lift . lift . lift

runPackage :: PackageM a -> IDEPackage -> ProjectM a
runPackage = runReaderT

-- ---------------------------------------------------------------------
-- Monad for functions that need to use the GHCi debugger
--
type DebugM = ReaderT DebugState IDEM
type DebugAction = DebugM ()

runDebug :: DebugM a -> DebugState -> IDEM a
runDebug = runReaderT

-- ---------------------------------------------------------------------
-- Events which can be signalled and handled
--

data IDEEvent  =
        InfoChanged Bool-- is it the initial = True else False
    |   UpdateWorkspaceInfo Bool
    |   SelectInfo SymbolEvent
    |   SelectIdent Descr
    |   LogMessage Text LogTag
    |   RecordHistory GUIHistory
    |   Sensitivity [(SensitivityMask,Bool)]
    |   SearchMeta Text
    |   StartFindInitial
    |   GotoDefinition Descr
    |   LoadSession FilePath
    |   SaveSession FilePath
    |   UpdateRecent
    |   VariablesChanged
    |   ErrorChanged Bool
    |   ErrorAdded Bool Int LogRef
    |   ErrorsRemoved Bool (LogRef -> Bool)
    |   CurrentErrorChanged (Maybe LogRef)
    |   BreakpointChanged
    |   CurrentBreakChanged (Maybe LogRef)
    |   TraceChanged
    |   GetTextPopup (Maybe (IDERef -> Menu -> IO ()))
    |   StatusbarChanged [StatusbarCompartment]
    |   WorkspaceChanged Bool Bool -- ^ showPane updateFileCache
    |   SelectSrcSpan (Maybe SrcSpan)
    |   SavedFile FilePath
    |   DebugStart (FilePath, FilePath)
    |   DebugStop (FilePath, FilePath)
    |   QuitToRestart

data SymbolEvent = SymbolEvent
    { selection :: Text
    , location :: Maybe (FilePath, (Int, Int), (Int, Int))
    , activatePanes :: Bool
    , openDefinition :: Bool
    , typeTipLocation :: (Int32, Int32)
    } deriving (Show, Eq)

instance Event IDEEvent Text where
    getSelector (InfoChanged _)         =   "InfoChanged"
    getSelector (UpdateWorkspaceInfo _) =   "UpdateWorkspaceInfo"
    getSelector (LogMessage _ _)        =   "LogMessage"
    getSelector (SelectInfo _)          =   "SelectInfo"
    getSelector (SelectIdent _)         =   "SelectIdent"
    getSelector (RecordHistory _)       =   "RecordHistory"
    getSelector (Sensitivity _)         =   "Sensitivity"
    getSelector (SearchMeta _)          =   "SearchMeta"
    getSelector StartFindInitial        =   "StartFindInitial"
    getSelector (GotoDefinition _)      =   "GotoDefinition"
    getSelector (LoadSession _)         =   "LoadSession"
    getSelector (SaveSession _)         =   "SaveSession"
    getSelector UpdateRecent            =   "UpdateRecent"
    getSelector VariablesChanged        =   "VariablesChanged"
    getSelector (ErrorChanged _)        =   "ErrorChanged"
    getSelector ErrorAdded{}            =   "ErrorAdded"
    getSelector (ErrorsRemoved _ _)     =   "ErrorsRemoved"
    getSelector (CurrentErrorChanged _) =   "CurrentErrorChanged"
    getSelector BreakpointChanged       =   "BreakpointChanged"
    getSelector (CurrentBreakChanged _) =   "CurrentBreakChanged"
    getSelector TraceChanged            =   "TraceChanged"
    getSelector (GetTextPopup _)        =   "GetTextPopup"
    getSelector (StatusbarChanged _)    =   "StatusbarChanged"
    getSelector (WorkspaceChanged _ _)  =   "WorkspaceChanged"
    getSelector (SelectSrcSpan _)       =   "SelectSrcSpan"
    getSelector (SavedFile _)           =   "SavedFile"
    getSelector (DebugStart _)          =   "DebugStart"
    getSelector (DebugStop _)           =   "DebugStop"
    getSelector QuitToRestart           =   "QuitToRestart"

instance EventSource IDERef IDEEvent IDEM Text where
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
    canTriggerEvent _ "ErrorAdded"          = True
    canTriggerEvent _ "ErrorsRemoved"       = True
    canTriggerEvent _ "CurrentErrorChanged" = True
    canTriggerEvent _ "BreakpointChanged"   = True
    canTriggerEvent _ "CurrentBreakChanged" = True
    canTriggerEvent _ "TraceChanged"        = True
    canTriggerEvent _ "GetTextPopup"        = True
    canTriggerEvent _ "StatusbarChanged"    = True
    canTriggerEvent _ "WorkspaceChanged"    = True
    canTriggerEvent _ "SelectSrcSpan"       = True
    canTriggerEvent _ "SavedFile"           = True
    canTriggerEvent _ "DebugStart"          = True
    canTriggerEvent _ "DebugStop"           = True
    canTriggerEvent _ "QuitToRestart"       = True
    canTriggerEvent _ _                   = False
    getHandlers ideRef =
        liftIO $ handlers <$> readMVar ideRef
    setHandlers ideRef nh =
        liftIO $ modifyMVar_ ideRef (\ide ->
            return ide {handlers= nh})
    myUnique _ =
        liftIO newUnique

instance EventSelector Text

-- ---------------------------------------------------------------------
-- Project
--
data ProjectTool = CabalTool | StackTool deriving (Show, Eq)

data Project = Project {
    pjTool       :: ProjectTool
,   pjFile       :: FilePath
,   pjPackageMap :: Map FilePath IDEPackage
} deriving (Show)

pjPackages :: Project -> [IDEPackage]
pjPackages = M.elems . pjPackageMap

pjLookupPackage :: FilePath -> Project -> Maybe IDEPackage
pjLookupPackage f = M.lookup f . pjPackageMap

pjToolCommand' :: Project -> FilePath
pjToolCommand' project = case pjTool project of
                            StackTool   -> "stack"
                            CabalTool   -> "cabal"

pjDir :: Project -> FilePath
pjDir = dropFileName . pjFile

-- ---------------------------------------------------------------------
-- IDEPackages
--
data IDEPackage     =   IDEPackage {
    ipdPackageId       ::   PackageIdentifier
,   ipdCabalFile       ::   FilePath
,   ipdDepends         ::   [Dependency]
,   ipdModules         ::   Map ModuleName BuildInfo
,   ipdHasLibs         ::   Bool
,   ipdExes            ::   [Text]
,   ipdTests           ::   [Text]
,   ipdBenchmarks      ::   [Text]
,   ipdMain            ::   [(FilePath, BuildInfo, Bool)]
,   ipdExtraSrcs       ::   Set FilePath
,   ipdSrcDirs         ::   [FilePath] -- ^ Relative paths to the source directories
,   ipdExtensions      ::   [Extension]
,   ipdConfigFlags     ::   [Text] -- ^ Flag for configure
,   ipdBuildFlags      ::   [Text] -- ^ Flags for building
,   ipdTestFlags       ::   [Text]  -- ^ Flags for test runs
,   ipdBenchmarkFlags  ::   [Text] -- ^ flags for benchmark runs
,   ipdHaddockFlags    ::   [Text] -- ^ Flags for haddock generation
,   ipdExeFlags        ::   [Text] -- ^ Flags for executable runs
,   ipdInstallFlags    ::   [Text] -- ^ Flags for install
,   ipdRegisterFlags   ::   [Text] -- ^ Flags for register
,   ipdUnregisterFlags ::   [Text] -- ^ Flags for unregister
,   ipdSdistFlags      ::   [Text]
}
    deriving (Eq)

instance Show IDEPackage where
    show p = show "IDEPackage for " ++ (render . disp) (ipdPackageId p)

-- | The directory of the cabal file
ipdPackageDir :: IDEPackage -> FilePath
ipdPackageDir = dropFileName . ipdCabalFile

-- | Gets the package name
ipdPackageName :: IDEPackage -> Text
ipdPackageName = T.pack . unPackageName . pkgName . ipdPackageId

-- | Gets the library name if the package has a library component
ipdLib :: IDEPackage -> Maybe Text
ipdLib pkg = if ipdHasLibs pkg then Just (ipdPackageName pkg) else Nothing

mkPackageMap :: [IDEPackage] -> Map FilePath IDEPackage
mkPackageMap = M.fromList . map (\p -> (ipdCabalFile p, p))

-- ---------------------------------------------------------------------
-- Workspace
--
data Workspace = Workspace {
    wsVersion           ::   Int
,   wsSaveTime          ::   Text
,   wsName              ::   Text
,   wsFile              ::   FilePath
,   wsProjects          ::   [Project]
,   wsActiveProjectFile ::   Maybe FilePath
,   wsActivePackFile    ::   Maybe FilePath
,   wsActiveComponent   ::   Maybe Text
,   packageVcsConf      ::   Map FilePath VCSConf -- ^ (FilePath to package, Version-Control-System Configuration)
} deriving Show

wsProjectFiles :: Workspace -> [FilePath]
wsProjectFiles = map pjFile . wsProjects

wsLookupProject :: FilePath -> Workspace -> Maybe Project
wsLookupProject f = find ((==f) . pjFile) . wsProjects

wsActiveProject :: Workspace -> Maybe Project
wsActiveProject w = wsActiveProjectFile w >>= (`wsLookupProject` w)

wsActivePackage :: Workspace -> Maybe IDEPackage
wsActivePackage w = do
    project <- wsActiveProject w
    wsActivePackFile w >>= (`pjLookupPackage` project)

wsPackages :: Workspace -> [IDEPackage]
wsPackages = wsProjects >=> pjPackages

wsProjectAndPackages :: Workspace -> [(Project, IDEPackage)]
wsProjectAndPackages = wsProjects >=> (\project -> (\pack -> (project, pack)) <$> pjPackages project)

-- | Includes sandbox sources
wsAllPackages :: Workspace -> [IDEPackage]
wsAllPackages w = nubBy ((==) `on` ipdCabalFile) $ wsPackages w

-- ---------------------------------------------------------------------
-- Other data structures which are used in the state
--

--
-- | ActionDescr is a data structure from which GtkActions are build, which are used for
--   menus, toolbars, and accelerator keystrokes
--
data ActionDescr alpha = AD {
    name        ::   ActionString
,   label       ::   Text
,   tooltip     ::   Maybe Text
,   stockID     ::   Maybe Text
,   action      ::   ReaderT alpha IO ()
,   accelerator ::   [KeyString]
,   isToggle    ::   Bool
}

type ActionString = Text
type KeyString = Text

--
-- | Preferences is a data structure to hold configuration data
--
data Prefs = Prefs {
        prefsFormat         ::   Int
    ,   prefsSaveTime       ::   Text
    ,   showLineNumbers     ::   Bool
    ,   rightMargin         ::   (Bool, Int)
    ,   tabWidth            ::   Int
    ,   wrapLines           ::   Bool
    ,   sourceCandy         ::   (Bool,Text)
    ,   darkUserInterface   ::   Bool
    ,   saveSessionOnClose  ::   Bool
    ,   keymapName          ::   Text
    ,   forceLineEnds       ::   Bool
    ,   removeTBlanks       ::   Bool
    ,   textviewFont        ::   Maybe Text
    ,   workspaceFont       ::   (Bool, Maybe Text)
    ,   sourceStyle         ::   (Bool, Text)
    ,   foundBackgroundLight      ::   Color
    ,   matchBackgroundLight      ::   Color
    ,   contextBackgroundLight    ::   Color
    ,   breakpointBackgroundLight ::   Color
    ,   lintBackgroundLight       ::   Color
    ,   foundBackgroundDark       ::   Color
    ,   matchBackgroundDark       ::   Color
    ,   contextBackgroundDark     ::   Color
    ,   breakpointBackgroundDark  ::   Color
    ,   lintBackgroundDark        ::   Color
    ,   autoLoad            ::   Bool
    ,   textEditorType      ::   Text
    ,   logviewFont         ::   (Bool, Maybe Text)
    ,   defaultSize         ::   (Int,Int)
    ,   browser             ::   Text
    ,   pathForCategory     ::   [(Text, PanePath)]
    ,   defaultPath         ::   PanePath
    ,   categoryForPane     ::   [(Text, Text)]
    ,   packageBlacklist    ::   [Dependency]
    ,   collectAtStart      ::   Bool
    ,   useCtrlTabFlipping  ::   Bool
    ,   docuSearchURL       ::   Text
    ,   completeRestricted  ::   Bool
    ,   saveAllBeforeBuild  ::   Bool
    ,   jumpToWarnings      ::   Bool
    ,   useVado             ::   Bool
    ,   backgroundBuild     ::   Bool
    ,   native              ::   Bool
    ,   javaScript          ::   Bool
    ,   debug               ::   Bool
    ,   makeDocs            ::   Bool -- ^ Make documentation on build
    ,   runUnitTests        ::   Bool -- ^ Run unit tests on build?
    ,   runBenchmarks        ::   Bool -- ^ Run benchmarks on build?
    ,   makeMode            ::   Bool -- ^ Also build other packages in workspace that depend on the active package
    ,   singleBuildWithoutLinking :: Bool
    ,   dontInstallLast     ::   Bool
    ,   printEvldWithShow   ::   Bool
    ,   breakOnException    ::   Bool
    ,   breakOnError        ::   Bool
    ,   printBindResult     ::   Bool
    ,   serverIP            ::   Text
    ,   showHiddenFiles     ::   Bool
    ,   showWorkspaceIcons  ::   Bool
    ,   hlintOnSave         ::   Bool
    ,   collapseErrors      ::   Bool
            -- As well used by server
    ,   serverPort          ::   Int
    ,   sourceDirectories   ::   [FilePath]
    ,   unpackDirectory     ::   Maybe FilePath
    ,   retrieveURL         ::   Text
    ,   retrieveStrategy    ::   RetrieveStrategy
    ,   endWithLastConn     ::   Bool
} deriving(Eq, Show, Generic)

data PrefsFile = PrefsFile {
    prefsFormat_         :: Maybe Int
  , prefsSaveTime_       :: Maybe Text
  , showLineNumbers_     :: Maybe Bool
  , rightMargin_         :: Maybe (Bool, Int)
  , tabWidth_            :: Maybe Int
  , wrapLines_           :: Maybe Bool
  , sourceCandy_         :: Maybe (Bool,Text)
  , darkUserInterface_   :: Maybe Bool
  , saveSessionOnClose_  :: Maybe Bool
  , keymapName_          :: Maybe Text
  , forceLineEnds_       :: Maybe Bool
  , removeTBlanks_       :: Maybe Bool
  , textviewFont_        :: Maybe (Maybe Text)
  , workspaceFont_       :: Maybe (Bool, Maybe Text)
  , sourceStyle_         :: Maybe (Bool, Text)
  , foundBackgroundLight_      :: Maybe Color
  , matchBackgroundLight_      :: Maybe Color
  , contextBackgroundLight_    :: Maybe Color
  , breakpointBackgroundLight_ :: Maybe Color
  , lintBackgroundLight_       :: Maybe Color
  , foundBackgroundDark_       :: Maybe Color
  , matchBackgroundDark_       :: Maybe Color
  , contextBackgroundDark_     :: Maybe Color
  , breakpointBackgroundDark_  :: Maybe Color
  , lintBackgroundDark_        :: Maybe Color
  , autoLoad_            :: Maybe Bool
  , textEditorType_      :: Maybe Text
  , logviewFont_         :: Maybe (Bool, Maybe Text)
  , defaultSize_         :: Maybe (Int,Int)
  , browser_             :: Maybe Text
  , pathForCategory_     :: Maybe [(Text, PanePath)]
  , defaultPath_         :: Maybe PanePath
  , categoryForPane_     :: Maybe [(Text, Text)]
  , packageBlacklist_    :: Maybe [Text]
  , collectAtStart_      :: Maybe Bool
  , useCtrlTabFlipping_  :: Maybe Bool
  , docuSearchURL_       :: Maybe Text
  , completeRestricted_  :: Maybe Bool
  , saveAllBeforeBuild_  :: Maybe Bool
  , jumpToWarnings_      :: Maybe Bool
  , useVado_             :: Maybe Bool
  , backgroundBuild_     :: Maybe Bool
  , native_              :: Maybe Bool
  , javaScript_          :: Maybe Bool
  , debug_               :: Maybe Bool
  , makeDocs_            :: Maybe Bool -- ^ Make documentation on build
  , runUnitTests_        :: Maybe Bool -- ^ Run unit tests on build?
  , runBenchmarks_        :: Maybe Bool -- ^ Run benchmarks on build?
  , makeMode_            :: Maybe Bool -- ^ Also build other packages in workspace that depend on the active package
  , singleBuildWithoutLinking_ :: Maybe Bool
  , dontInstallLast_     :: Maybe Bool
  , printEvldWithShow_   :: Maybe Bool
  , breakOnException_    :: Maybe Bool
  , breakOnError_        :: Maybe Bool
  , printBindResult_     :: Maybe Bool
  , serverIP_            :: Maybe Text
  , showHiddenFiles_     :: Maybe Bool
  , showWorkspaceIcons_  :: Maybe Bool
  , hlintOnSave_         :: Maybe Bool
  , collapseErrors_      :: Maybe Bool
  , serverPort_          :: Maybe Int
  , sourceDirectories_   :: Maybe [FilePath]
  , unpackDirectory_     :: Maybe (Maybe FilePath)
  , retrieveURL_         :: Maybe Text
  , retrieveStrategy_    :: Maybe RetrieveStrategy
  , endWithLastConn_     :: Maybe Bool
} deriving(Eq, Show, Generic)

prefsAesonOptions :: Options
prefsAesonOptions = defaultOptions
    { fieldLabelModifier = init
    }

instance ToJSON PrefsFile where
    toJSON     = genericToJSON prefsAesonOptions
    toEncoding = genericToEncoding prefsAesonOptions
instance FromJSON PrefsFile where
    parseJSON = genericParseJSON prefsAesonOptions

candyState :: Prefs -> Bool
candyState = fst . sourceCandy

data EditorStyle = EditorStyle { styleName    :: Maybe Text
                               , preferDark   :: Bool
                               , foundBG      :: (Color, Color)
                               , matchBG      :: (Color, Color)
                               , contextBG    :: (Color, Color)
                               , breakpointBG :: (Color, Color)
                               , lintBG       :: (Color, Color)
                               }

editorStyle :: Bool -> Prefs -> EditorStyle
editorStyle preferDark prefs = EditorStyle { styleName = case sourceStyle prefs of
                                                        (False,_) -> Nothing
                                                        (True,v)  -> Just v
                                           , preferDark = preferDark
                                           , foundBG      = (foundBackgroundDark      prefs, foundBackgroundLight      prefs)
                                           , matchBG      = (matchBackgroundDark      prefs, matchBackgroundLight      prefs)
                                           , contextBG    = (contextBackgroundDark    prefs, contextBackgroundLight    prefs)
                                           , breakpointBG = (breakpointBackgroundDark prefs, breakpointBackgroundLight prefs)
                                           , lintBG       = (lintBackgroundDark       prefs, lintBackgroundLight       prefs)
                                           }

data SearchHint = Forward | Backward | Insert | Delete | Initial
    deriving (Eq)

-- Version-Control-System Configuration
type VCSConf = (VCS.VCSType, VCS.Config, Maybe VCSGUI.MergeTool)

--
-- | Other types
--

data LogLaunchData = LogLaunchData {
    logLaunch :: LogLaunch
,   mbPid :: Maybe ProcessHandle
}

newtype LogLaunch = LogLaunch {
    logBuffer   :: TextBuffer
} deriving Typeable


-- Order determines priority of the icons in the gutter
data LogRefType = ContextRef | BreakpointRef | ErrorRef | TestFailureRef | WarningRef | LintRef
    deriving (Eq, Ord, Show, Enum, Bounded)

data Log =
    LogProject {logBasePath :: FilePath}
  | LogCabal {logCabalFile :: FilePath}
  | LogNix {logNixFile :: FilePath, logNixAttribute :: Text}
  deriving(Eq, Show)

logRootPath :: Log -> FilePath
logRootPath LogProject{..} = logBasePath
logRootPath LogCabal{..} = dropFileName logCabalFile
logRootPath LogNix{..} = dropFileName logNixFile

-- | Represents a message about a part of the source code
data LogRef = LogRef {
    logRefSrcSpan       ::   SrcSpan
,   logRefLog           ::   Log
,   refDescription      ::   Text
,   logRefIdea          ::   Maybe (Text, Idea)
,   logLines            ::   Maybe (Int, Int)
,   logRefType          ::   LogRefType
} deriving(Eq)

instance Show LogRef where
    show lr = T.unpack (refDescription lr) ++ displaySrcSpan (logRefSrcSpan lr)

displaySrcSpan s = srcSpanFilename s ++ ":" ++
    if srcSpanStartLine s == srcSpanEndLine s
        then show (srcSpanStartLine s) ++ ":" ++
            if srcSpanStartColumn s == srcSpanEndColumn s
                then show (srcSpanStartColumn s)
                else show (srcSpanStartColumn s) ++ "-" ++ show (srcSpanEndColumn s)
        else show (srcSpanStartLine s) ++ ":" ++
            show (srcSpanStartColumn s) ++ "-" ++ show (srcSpanEndColumn s)

-- | The root folder of the package the message references
logRefRootPath :: LogRef -> FilePath
logRefRootPath = logRootPath . logRefLog

-- | The file path the message references, relative to the root path
logRefFilePath :: LogRef -> FilePath
logRefFilePath lr = let
    f =srcSpanFilename $ logRefSrcSpan lr
    in if isAbsolute f -- can happen, at least when building with stack a source file that is present in several components (ie library and test)
            then makeRelative (logRefRootPath lr) f
            else f

-- | The absolute file path the message references
logRefFullFilePath :: LogRef -- ^ The log ref
    -> FilePath -- ^ the result
logRefFullFilePath lr = let
    f = srcSpanFilename $ logRefSrcSpan lr
    in if isAbsolute f
            then f
            else logRefRootPath lr </> f

isError :: LogRef -> Bool
isError = (== ErrorRef) . logRefType

isBreakpoint :: LogRef -> Bool
isBreakpoint = (== BreakpointRef) . logRefType

isContext :: LogRef -> Bool
isContext = (== ContextRef) . logRefType

-- This should probably be in Gtk2Hs allong with a suitable parser
colorHexString (Color r g b) = '#' : pad (showHex r "")
                                  ++ pad (showHex g "")
                                  ++ pad (showHex b "")
    where pad s = replicate (4 - length s) '0' ++ s


newtype CandyTable      =   CT (CandyTableForth,CandyTableBack)

type CandyTableForth    =   [(Bool,Text,Text)]

type CandyTableBack     =   [(Text,Text,Int)]

newtype KeymapI         =   KM  (Map ActionString
                                [(Maybe (Either KeyString (KeyString,KeyString)), Maybe Text)])

type KeyVal = Word32

type SpecialKeyTable alpha  =   Map (KeyVal,[ModifierType]) (Map (KeyVal,[ModifierType]) (ActionDescr alpha))

type SpecialKeyCons  alpha  =   Maybe (Map (KeyVal, [ModifierType]) (ActionDescr alpha), Text)

data LogTag = LogTag | ErrorTag | FrameTag | InputTag | InfoTag

-- | the first one is the new and the second the old state
type GUIHistory = (GUIHistory', GUIHistory')

data GUIHistory' =
        ModuleSelected  {
            moduleS :: Maybe ModuleName
        ,   facetS  :: Maybe Text}
    |   ScopeSelected {
            scope   :: Scope
        ,   blacklist :: Bool}
    |   InfoElementSelected {
            mbInfo  :: Maybe Descr}
    |   PaneSelected {
            paneN   :: Maybe Text}
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
    deriving (Eq,Ord,Read,Show,Generic)

instance ToJSON SearchMode
instance FromJSON SearchMode

data CompletionWindow = CompletionWindow {
    cwWindow :: Window,
    cwTreeView :: TreeView,
    cwSeqStore :: SeqStore Text}

data TypeTip = TypeTip {
    ttWindow :: Window,
    ttSetText :: Int32 -> Int32 -> Text -> IDEM (),
    ttUpdateStyle :: IDEAction}

data StatusbarCompartment =
        CompartmentCommand Text
    |   CompartmentPane (Maybe (IDEPane IDEM))
    |   CompartmentPackage Text
    |   CompartmentState Text
    |   CompartmentOverlay Bool
    |   CompartmentBufferPos (Int,Int)
    |   CompartmentBuild Bool
    |   CompartmentCollect Bool

type PackageDescrCache = Map PackageIdentifier ModuleDescrCache
type ModuleDescrCache = Map ModuleKey (UTCTime, Maybe FilePath, ModuleDescr)

