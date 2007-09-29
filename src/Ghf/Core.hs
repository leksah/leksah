-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Core
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

module Ghf.Core (
-- * IDE State
    Ghf(..)
,   GhfRef
,   GhfM
,   GhfAction

-- * Convenience methods for accesing the IDE State
,   readGhf
,   modifyGhf
,   modifyGhf_
,   withGhf

-- * Packages
,   GhfPackage(..)

-- * Panes and pane layout
,   GhfPane(..)
,   Direction(..)
,   PaneDirection(..)
,   PanePath
,   PaneLayout(..)
,   StandardPath(..)
,   PaneName
,   Connections(..)

-- * The pane types
,   GhfBuffer(..)
,   GhfLog(..)

-- * Other state structures
,   ActionDescr(..)
,   ActionString
,   KeyString

,   Prefs(..)

,   FileName
,   CandyTables
,   CandyTableForth
,   CandyTableBack
,   SpecialKeyTable
,   SpecialKeyCons

,   ErrorSpec(..)

-- * debugging
,   helpDebug
,   message
,   trace
) where

import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk
import System.Glib.Signals
import Control.Monad.Reader
import Distribution.Package
import Distribution.Setup
import Distribution.Simple.LocalBuildInfo
import System.FilePath
import Data.IORef
import Data.Map
import qualified Data.Map as Map
import System.Time


--import Debug.Trace
--message m = trace m (return ())


message m = return ()
trace a b = b

-- ---------------------------------------------------------------------
-- IDE State
--

--
-- | The IDE state
--
data Ghf        =   Ghf {
    window      ::  Window                  -- ^ the gtk window
,   uiManager   ::  UIManager               -- ^ the gtk uiManager
,   panes       ::  Map PaneName GhfPane      -- ^ a map with all panes (subwindows)
,   activePane  ::  Maybe (PaneName,Connections)
,   paneMap     ::  Map PaneName (PanePath, Connections)
                    -- ^ a map from the pane to its gui path and signal connections
,   layout      ::  PaneLayout              -- ^ a description of the general gui layout
,   specialKeys ::  SpecialKeyTable         -- ^ a structure for emacs like keystrokes
,   specialKey  ::  SpecialKeyCons          -- ^ the first of a double keystroke
,   candy       ::  CandyTables             -- ^ table for source candy
,   prefs       ::  Prefs                   -- ^ configuration preferences
,   packages    ::  [GhfPackage]            -- ^ the packages known to ghf
,   activePack  ::  Maybe GhfPackage
,   errors      ::  [ErrorSpec]
,   currentErr  ::  Maybe Int
} deriving Show

--
-- | A mutable reference to the IDE state
--
type GhfRef = IORef Ghf

--
-- | A reader monad for a mutable reference to the IDE state
--
type GhfM = ReaderT (GhfRef) IO

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
-- | which does not return a value
--
type GhfAction = GhfM ()

-- ---------------------------------------------------------------------
-- Convenience methods for accesing the IDE State
--

-- | Read an attribute of the contents
readGhf :: (Ghf -> beta) -> GhfM beta
readGhf f = do
    e <- ask
    lift $ liftM f (readIORef e)

-- | Modify the contents, using an IO action.
modifyGhf_ :: (Ghf -> IO Ghf) -> GhfM ()
modifyGhf_ f = do
    e <- ask
    e' <- lift $ (f =<< readIORef e)
    lift $ writeIORef e e'

-- | Variation on modifyGhf_ that lets you return a value
modifyGhf :: (Ghf -> IO (Ghf,beta)) -> GhfM beta
modifyGhf f = do
    e <- ask
    (e',result) <- lift (f =<< readIORef e)
    lift $ writeIORef e e'
    return result

withGhf :: (Ghf -> IO alpha) -> GhfM alpha
withGhf f = do
    e <- ask
    lift $ f =<< readIORef e

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

instance Eq ConfigFlags
instance Show ConfigFlags

instance Eq LocalBuildInfo
instance Eq BuildFlags
instance Show BuildFlags

-- ---------------------------------------------------------------------
-- Panes and pane layout
--

--
-- | Description of the different pane types
--
data GhfPane        =   BufPane GhfBuffer
                    |   LogPane GhfLog
    deriving (Eq,Show)

--
-- | The direction of a split
--
data Direction      =   Horizontal | Vertical
    deriving (Eq,Show)

--
-- | The relative direction to a pane from the parent
--
data PaneDirection  =   TopP | BottomP | LeftP | RightP
    deriving (Eq,Show,Read)

  	
--
-- | A path to a pane
--
type PanePath       =   [PaneDirection]

--
-- | Logic description of a window layout
--
data PaneLayout =       HorizontalP PaneLayout PaneLayout Int
                    |   VerticalP PaneLayout PaneLayout Int
                    |   TerminalP (Maybe PaneDirection)
    deriving (Eq,Show,Read)


data StandardPath = LeftTop | LeftBottom | RightTop | RightBottom
    deriving(Read,Show,Eq,Enum)

--
-- | Signal handlers for the different pane types
--
data Connections =  BufConnections
                        [ConnectId SourceView]
                        [ConnectId TextBuffer]
                        [ConnectId TextView]
    deriving (Show)

type PaneName = String

-- ---------------------------------------------------------------------
-- Buffers - The text editor panes
--

--
-- | A text editor pane description
--
data GhfBuffer  =   GhfBuffer {
    fileName    ::  Maybe FileName
,   bufferName  ::  String
,   addedIndex  ::  Int
,   sourceView  ::  SourceView
,   scrolledWindow :: ScrolledWindow
,   modTime     ::  Maybe (ClockTime)
} deriving Show

instance Eq GhfBuffer
    where (== ) a b = bufferName a == bufferName b && addedIndex a == addedIndex b
instance Ord GhfBuffer
    where (<=) a b = if bufferName a < bufferName b
                        then True
                        else if bufferName a == bufferName b
                            then addedIndex a <= addedIndex b
                            else False

--
-- | A log view pane description
--
data GhfLog  =   GhfLog {
    textView  ::  TextView
,   scrolledWindowL :: ScrolledWindow
}

instance Eq GhfLog
    where (== ) a b = True

instance Ord GhfLog
    where (<=) a b = True

instance Show GhfLog
    where show _ = "GhfLog *"

-- ---------------------------------------------------------------------
-- Other data structures which are used in the state
--

--
-- | ActionDescr is a data structure from which GtkActions are build, which are used for
--   menus, toolbars, and accelerator keystrokes
--
data ActionDescr = AD {
                name        ::   ActionString
            ,   label       ::   String
            ,   tooltip     ::   Maybe String
            ,   stockID     ::   Maybe String
            ,   action      ::   GhfAction
            ,   accelerator ::   [KeyString]
            ,   isToggle    ::   Bool
} deriving (Show)

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
} deriving(Eq,Show)


type CandyTableForth =  [(Bool,String,String)]
type CandyTableBack  =  [(String,String,Int)]
type CandyTables     =  (CandyTableForth,CandyTableBack)

type SpecialKeyTable =  Map (KeyVal,[Modifier]) (Map (KeyVal,[Modifier]) ActionDescr)
type SpecialKeyCons  =  Maybe ((Map (KeyVal,[Modifier]) ActionDescr),String)

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
-- Debugging
--

instance Show Window
    where show _ = "Window *"

instance Show Modifier
    where show Shift    = "<shift>"
          show Control  = "<ctrl>"
          show Alt      = "<alt>"
          show Apple    = "<apple>"
          show Compose  = "<compose>"

instance Show UIManager
    where show _ = "UIManager *"

instance Show (ReaderT alpha beta gamma )
    where show _ = "ReaderT *"

instance Show (ConnectId alpha )
    where show cid = "ConnectId *"

instance Show SourceView
    where show _ = "SourceView *"

instance Show TextView
    where show _ = "TextView *"

instance Show ScrolledWindow
    where show _ = "ScrolledWindow *"

helpDebug :: GhfAction
helpDebug = do
    ref <- ask
    ghf <- lift $readIORef ref
    lift $do
        putStrLn $"------------------ "
        putStrLn $show ghf
        putStrLn $"------------------ "

