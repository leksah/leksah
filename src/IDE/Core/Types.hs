{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
,   IDEGtk
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
,   ProjectKey(..)
,   Project(..)
,   CabalProject(..)
,   StackProject(..)
,   CustomProject(..)
,   pjPackages
,   pjLookupPackage
,   pjDir
,   pjFile
,   pjFileOrDir
,   pjIsCabal
,   pjIsStack
,   filePathToProjectKey
--,   pjToolCommand'
,   Workspace(..)
,   wsProjectKeys
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

,   PackageDescrCache
,   ModuleDescrCache

,   LogLaunchData(..)
,   LogTag(..)
,   SensitivityMask(..)
,   SearchMode(..)
,   StatusbarCompartment(..)

-- IDE
,   ideGtk
,   exitCode
,   candy
,   prefs
,   workspace
,   bufferProjCache
,   allLogRefs
,   currentEBC
,   currentHist
,   systemInfo
,   packageInfo
,   workspaceInfo
,   workspInfoCache
,   handlers
,   currentState
,   recentFiles
,   recentWorkspaces
,   runningTool
,   debugState
,   yiControl
,   serverQueue
,   server
,   hlintQueue
,   logLaunches
,   autoCommand
,   autoURI
,   triggerBuild
,   fsnotify
,   watchers
,   developLeksah
,   nixCache
,   externalModified
,   jsContexts
,   logLineMap

-- Workspace
,   wsVersion
,   wsSaveTime
,   wsName
,   wsFile
,   wsProjects
,   wsActiveProjectKey
,   wsActivePackFile
,   wsActiveComponent
,   packageVcsConf

,   __
) where

import Prelude ()
import Prelude.Compat
import qualified IDE.TextEditor.Yi.Config as Yi
import Data.Unique (newUnique, Unique)
import Distribution.Package
       (unPackageName, PackageIdentifier(..), Dependency(..))
import Distribution.PackageDescription (BuildInfo)
import Data.Map (Map)
import Data.Set (Set)
import Data.List (find, nubBy)
import Control.Concurrent (modifyMVar_, readMVar, MVar)
import Distribution.ModuleName (ModuleName(..))
import Distribution.Simple (Extension(..))
import IDE.Utils.Tool (ToolState(..), ProcessHandle)
import Data.IORef (IORef)
import Numeric (showHex)
import System.FilePath
       (dropFileName, (</>), isAbsolute, makeRelative)
import IDE.Core.CTypes
import IDE.StrippedPrefs(RetrieveStrategy)
import System.IO (Handle)
import Text.PrettyPrint (render)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Time (UTCTime(..))

import qualified VCSWrapper.Common as VCS
import qualified Data.Map as Map (Map)
import Control.Monad.Reader.Class (MonadReader(..))
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Language.Haskell.HLint (Idea(..))
import Data.Function (on)
import Control.Concurrent.STM.TVar (TVar)
import Data.Sequence (Seq)
import Control.Monad ((>=>))
import System.FSNotify (StopListening, WatchManager)
import qualified Data.Map as M (fromList, lookup, elems)
import System.Exit (ExitCode)
import Data.Int (Int32)
import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)
import Data.Aeson.Types
       (genericParseJSON, genericToEncoding, genericToJSON,
        defaultOptions, fieldLabelModifier, Options)
import Language.Javascript.JSaddle (JSContextRef)
import Control.Lens (makeLenses, (^.), Getter, to, view)
import Control.Event
       (EventSelector, EventSource(..), Event(..))
import IDE.Gtk.Types
       (IDEState(..), IDEGtk, IDEGtkEvent, Color(..), PanePath,
       MergeTool, LogLaunchData(..), getGtkEventSelector,
       ActionString, KeyString, ActionDescr(..))

#ifdef LOCALIZATION

import Text.I18N.GetText
import System.IO.Unsafe (unsafePerformIO)

#endif

import IDE.Utils.Project
       (ProjectKey(..), pjCabalFile, pjStackFile, pjCustomDir, pjDir,
        CabalProject(..), StackProject(..), CustomProject(..), pjIsCabal,
        pjIsStack, pjFileOrDir, pjFile, filePathToProjectKey)
import Distribution.Pretty (prettyShow)

-- ---------------------------------------------------------------------
-- IDE State
--

--
-- | The IDE state
--

data IDE            =  IDE {
    _ideGtk              :: Maybe (IDEGtk IDEM IDERef)
,   _exitCode            :: IORef ExitCode
,   _candy               :: CandyTable              -- ^ table for source candy
,   _prefs               :: Prefs                   -- ^ configuration preferences
,   _workspace           :: Maybe Workspace         -- ^ may be a workspace (set of packages)
,   _bufferProjCache     :: Map FilePath [(Project, IDEPackage)] -- ^ cache the associated packages for a file
,   _allLogRefs          :: Seq LogRef
,   _currentEBC          :: (Maybe LogRef, Maybe LogRef, Maybe LogRef)
,   _currentHist         :: Int
,   _systemInfo          :: Maybe GenScope              -- ^ the system scope
,   _packageInfo         :: Maybe (GenScope, GenScope) -- ^ the second are the imports
,   _workspaceInfo       :: Maybe (GenScope, GenScope) -- ^ the second are the imports
,   _workspInfoCache     :: PackageDescrCache
,   _handlers            :: Map Text [(Unique, IDEEvent -> IDEM IDEEvent)] -- ^ event handling table
,   _currentState        :: IDEState
,   _recentFiles         :: [FilePath]
,   _recentWorkspaces    :: [FilePath]
,   _runningTool         :: Maybe (ProcessHandle, IO ())
,   _debugState          :: [DebugState]
,   _yiControl           :: Yi.Control
,   _serverQueue         :: Maybe (MVar (ServerCommand, ServerAnswer -> IDEM ()))
,   _server              :: Maybe Handle
,   _hlintQueue          :: Maybe (TVar [Either FilePath FilePath])
,   _logLaunches         :: Map.Map Text LogLaunchData
,   _autoCommand         :: Maybe ((ProjectKey, FilePath), IDEAction)
,   _autoURI             :: Maybe Text
,   _triggerBuild        :: MVar ()
,   _fsnotify            :: WatchManager
,   _watchers            :: MVar (Map ProjectKey StopListening, Map FilePath StopListening)
,   _developLeksah       :: Bool -- If True leksah will exit when the `leksah` package is rebuilt
,   _nixCache            :: Map (FilePath, Text) (Map String String)
,   _externalModified    :: MVar (Set FilePath)
,   _jsContexts          :: [JSContextRef]
,   _logLineMap          :: Map Int (Text, LogTag)
} -- deriving Show

data DebugState = DebugState
    { dsProjectKey  :: ProjectKey
    , dsPackages    :: [IDEPackage]
    , dsBasePath    :: FilePath
    , dsToolState   :: ToolState
    }

--
-- | A mutable reference to the IDE state
--
type IDERef = MVar (IDE -> IO (), IDE)

--
-- | The IDE Monad
--
type IDEM = ReaderT IDERef IO

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
--   which does not return a value
--
type IDEAction = IDEM ()


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
    |   StatusbarChanged [StatusbarCompartment]
    |   WorkspaceChanged Bool Bool -- ^ showPane updateFileCache
    |   SelectSrcSpan (Maybe SrcSpan)
    |   SavedFile FilePath
    |   DebugStart (ProjectKey, FilePath)
    |   DebugStop (ProjectKey, FilePath)
    |   QuitToRestart
    |   GtkEvent (IDEGtkEvent IDERef)

data SymbolEvent = SymbolEvent
    { selection :: Text
    , location :: Maybe (FilePath, (Int, Int), (Int, Int))
    , activatePanes :: Bool
    , openDefinition :: Bool
    , typeTipLocation :: (Int32, Int32)
    } deriving (Show, Eq)

instance EventSelector Text

instance Event IDEEvent Text where
    getSelector (InfoChanged _)         =   "InfoChanged"
    getSelector (UpdateWorkspaceInfo _) =   "UpdateWorkspaceInfo"
    getSelector (LogMessage _ _)        =   "LogMessage"
    getSelector (SelectInfo _)          =   "SelectInfo"
    getSelector (SelectIdent _)         =   "SelectIdent"
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
    getSelector (StatusbarChanged _)    =   "StatusbarChanged"
    getSelector (WorkspaceChanged _ _)  =   "WorkspaceChanged"
    getSelector (SelectSrcSpan _)       =   "SelectSrcSpan"
    getSelector (SavedFile _)           =   "SavedFile"
    getSelector (DebugStart _)          =   "DebugStart"
    getSelector (DebugStop _)           =   "DebugStop"
    getSelector QuitToRestart           =   "QuitToRestart"
    getSelector (GtkEvent e)            =   getGtkEventSelector e

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
        liftIO $ _handlers . snd <$> readMVar ideRef
    setHandlers ideRef nh =
        liftIO $ modifyMVar_ ideRef (\(a, ide) ->
            return (a, ide {_handlers= nh}))
    myUnique _ =
        liftIO newUnique

-- ---------------------------------------------------------------------
-- Project
--
--newtype CabalProject = CabalProject
--  { pjCabalFile :: FilePath
--  } deriving (Show, Eq)
--newtype StackProject = StackProject
--  { pjStackFile :: FilePath
--  } deriving (Show, Eq)
--data CustomProject = CustomProject
--  { pjCustomDir        :: FilePath
--  , pjCustomNixShell   :: [Text]
--  , pjCustomGhcBuild   :: [Text]
--  , pjCustomGhcjsBuild :: [Text]
--  , pjCustomRepl       :: [Text]
--  } deriving (Show, Eq)
--
--data ProjectKey =
--    CabalTool CabalProject
--  | StackTool StackProject
--  | CustomTool CustomProject
--  deriving (Show, Eq)

data Project = Project
  { pjKey        :: ProjectKey
  , pjPackageMap :: Map FilePath IDEPackage
  } deriving (Show)

pjPackages :: Project -> [IDEPackage]
pjPackages = M.elems . pjPackageMap

pjLookupPackage :: FilePath -> Project -> Maybe IDEPackage
pjLookupPackage f = M.lookup f . pjPackageMap

--pjToolCommand' :: Project -> FilePath
--pjToolCommand' project = case pjTool project of
--                            StackTool   -> "stack"
--                            CabalTool   -> "cabal"

-- ---------------------------------------------------------------------
-- IDEPackages
--
data IDEPackage     =   IDEPackage {
    ipdPackageId       ::   PackageIdentifier
,   ipdCabalFile       ::   FilePath
,   ipdDepends         ::   [Dependency]
,   ipdModules         ::   Map ModuleName BuildInfo
,   ipdHasLib          ::   Bool
,   ipdSubLibraries    ::   [Text]
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
    show p = "IDEPackage for " ++ prettyShow (ipdPackageId p)

-- | The directory of the cabal file
ipdPackageDir :: IDEPackage -> FilePath
ipdPackageDir = dropFileName . ipdCabalFile

-- | Gets the package name
ipdPackageName :: IDEPackage -> Text
ipdPackageName = T.pack . unPackageName . pkgName . ipdPackageId

-- | Gets the library name if the package has a library component
ipdLib :: IDEPackage -> Maybe Text
ipdLib pkg = if ipdHasLib pkg then Just (ipdPackageName pkg) else Nothing

mkPackageMap :: [IDEPackage] -> Map FilePath IDEPackage
mkPackageMap = M.fromList . map (\p -> (ipdCabalFile p, p))

-- ---------------------------------------------------------------------
-- Workspace
--
data Workspace = Workspace {
    _wsVersion           ::   Int
,   _wsSaveTime          ::   Text
,   _wsName              ::   Text
,   _wsFile              ::   FilePath
,   _wsProjects          ::   [Project]
,   _wsActiveProjectKey  ::   Maybe ProjectKey
,   _wsActivePackFile    ::   Maybe FilePath
,   _wsActiveComponent   ::   Maybe Text
,   _packageVcsConf      ::   Map FilePath VCSConf -- ^ (FilePath to package, Version-Control-System Configuration)
} deriving Show

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
    ,   runBenchmarks       ::   Bool -- ^ Run benchmarks on build?
    ,   makeMode            ::   Bool
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
  , makeMode_            :: Maybe Bool
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
type VCSConf = (VCS.VCSType, VCS.Config, Maybe MergeTool)

--
-- | Other types
--

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

displaySrcSpan :: SrcSpan -> String
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

--isContext :: LogRef -> Bool
--isContext = (== ContextRef) . logRefType

-- This should probably be in Gtk2Hs allong with a suitable parser
colorHexString :: Color -> String
colorHexString (Color r g b) = '#' : pad (showHex r "")
                                  ++ pad (showHex g "")
                                  ++ pad (showHex b "")
    where pad s = replicate (4 - length s) '0' ++ s


newtype CandyTable      =   CT (CandyTableForth,CandyTableBack)

type CandyTableForth    =   [(Bool,Text,Text)]

type CandyTableBack     =   [(Text,Text,Int)]

newtype KeymapI         =   KM  (Map ActionString
                                [(Maybe (Either KeyString (KeyString,KeyString)), Maybe Text)])

data LogTag = LogTag | ErrorTag | FrameTag | InputTag | InfoTag deriving(Eq, Ord, Show)

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

data StatusbarCompartment =
        CompartmentCommand Text
    |   CompartmentPane Text
    |   CompartmentPackage Text
    |   CompartmentState Text
    |   CompartmentOverlay Bool
    |   CompartmentBufferPos (Int,Int)
    |   CompartmentBuild Bool
    |   CompartmentCollect Bool

type PackageDescrCache = Map PackageIdentifier ModuleDescrCache
type ModuleDescrCache = Map ModuleKey (UTCTime, Maybe FilePath, ModuleDescr)

makeLenses ''IDE
makeLenses ''Workspace

wsProjectKeys :: Getter Workspace [ProjectKey]
wsProjectKeys = wsProjects . to (map pjKey)

wsLookupProject :: ProjectKey -> Workspace -> Maybe Project
wsLookupProject f = find ((==f) . pjKey) . _wsProjects

_wsActiveProject :: Workspace -> Maybe Project
_wsActiveProject w = (w ^. wsActiveProjectKey) >>= (`wsLookupProject` w)

wsActiveProject :: Getter Workspace (Maybe Project)
wsActiveProject = to _wsActiveProject

_wsActivePackage :: Workspace -> Maybe IDEPackage
_wsActivePackage w = do
    project <- _wsActiveProject w
    _wsActivePackFile w >>= (`pjLookupPackage` project)

wsActivePackage :: Getter Workspace (Maybe IDEPackage)
wsActivePackage = to _wsActivePackage

wsPackages :: Getter Workspace [IDEPackage]
wsPackages = to (_wsProjects >=> pjPackages)

_wsProjectAndPackages :: Workspace -> [(Project, IDEPackage)]
_wsProjectAndPackages = _wsProjects >=> (\project -> (project,) <$> pjPackages project)

wsProjectAndPackages :: Getter Workspace [(Project, IDEPackage)]
wsProjectAndPackages = to _wsProjectAndPackages

-- | Includes sandbox sources
_wsAllPackages :: Workspace -> [IDEPackage]
_wsAllPackages w = nubBy ((==) `on` ipdCabalFile) $ w ^. wsPackages

wsAllPackages :: Getter Workspace [IDEPackage]
wsAllPackages = to _wsAllPackages

activeProject :: Getter IDE (Maybe Project)
activeProject = workspace . to (>>= view wsActiveProject)

activePack :: Getter IDE (Maybe IDEPackage)
activePack = workspace . to (>>= view wsActivePackage)

activeComponent :: Getter IDE (Maybe Text)
activeComponent = workspace . to (>>= view wsActiveComponent)

nixEnv :: ProjectKey -> Text -> IDE -> Maybe (Map String String)
nixEnv project compiler ide = M.lookup (pjDir project, compiler) $ ide ^. nixCache

#ifdef LOCALIZATION

-- | For i18n using hgettext
__ :: Text -> Text
__ = T.pack . unsafePerformIO . getText . T.unpack


#else

-- | For i18n support. Not included in this build.
__ :: Text -> Text
__ = id

#endif

