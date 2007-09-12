--
-- | The core state of ghf. This module is imported from every other module,
-- | and all data structures of the state are declared here, to avoid circular
-- | module dependencies.
--

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
,   Connections(..)

-- * Convenience methods for accesing Pane state
,   getTopWidget
,   getBufferName
,   getAddedIndex
,   realPaneName

-- * The Buffer pane
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

-- * debugging
,   helpDebug
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import System.Glib.Signals(ConnectId)
import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Directory
import System.Console.GetOpt
import System.Environment
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Setup
import Distribution.Setup(ConfigFlags)
import Data.Maybe ( fromMaybe, isJust)
import qualified Data.Map as Map
import Data.Map (Map,(!))


-- ---------------------------------------------------------------------
-- IDE State
--

--
-- | The IDE state
--
data Ghf        =   Ghf {
    window      ::  Window                  -- ^ the gtk window
,   uiManager   ::  UIManager               -- ^ the gtk uiManager
,   panes       ::  Map String GhfPane      -- ^ a map with all panes (subwindows)
,   activePane  ::  Maybe (GhfPane,Connections)
,   paneMap     ::  Map GhfPane (PanePath, [ConnectId Widget])
                    -- ^ a map from the pane to its gui path and signal connections
,   layout      ::  PaneLayout              -- ^ a description of the general gui layout
,   specialKeys ::  SpecialKeyTable         -- ^ a structure for emacs like keystrokes
,   specialKey  ::  SpecialKeyCons          -- ^ the first of a double keystroke
,   candy       ::  CandyTables             -- ^ table for source candy
,   prefs       ::  Prefs                   -- ^ configuration preferences
,   packages    ::  [GhfPackage]            -- ^ the packages known to ghf
,   activePack  ::  Maybe GhfPackage
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
    packageId       ::  PackageIdentifier
,   cabalFile       ::  FilePath
,   configFlags     ::  [String]
,   buildFlags      ::  [String]
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
data GhfPane        =   PaneBuf GhfBuffer
                    |   LogBuf  GhfLog
    deriving (Eq,Ord,Show)

--
-- | The direction of a split
--
data Direction      =   Horizontal | Vertical
    deriving (Eq,Ord,Show)

--
-- | The relative direction to a pane from the parent
--
data PaneDirection  =   TopP | BottomP | LeftP | RightP
    deriving (Eq,Ord,Show)

--
-- | A path to a pane
--
type PanePath       =   [PaneDirection]

--
-- | Logic description of a window layout
--
data PaneLayout =       HorizontalP PaneLayout PaneLayout
                    |   VerticalP PaneLayout PaneLayout
                    |   TerminalP
    deriving (Eq,Ord,Show)

--
-- | Signal handlers for the different pane types
--
data Connections =  BufConnections [ConnectId SourceView] [ConnectId TextBuffer]
    deriving (Show)

-- ---------------------------------------------------------------------
-- Convenience methods for panes
-- ### currently ugly



getTopWidget :: GhfPane -> Widget
getTopWidget (PaneBuf buf) = castToWidget(scrolledWindow buf)

getBufferName :: GhfPane -> String
getBufferName (PaneBuf buf) = bufferName buf
getBufferName (LogBuf _) = "Log"

getAddedIndex :: GhfPane -> Int
getAddedIndex (PaneBuf buf) = addedIndex buf
getAddedIndex _ = 0

realPaneName :: GhfPane -> String
realPaneName pane@(PaneBuf _) =
    if getAddedIndex pane == 0
        then getBufferName pane
        else getBufferName pane ++ "(" ++ show (getAddedIndex pane) ++ ")"
realPaneName other = getBufferName other


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
} deriving Show

instance Eq GhfBuffer
    where (==) a b = bufferName a == bufferName b && addedIndex a == addedIndex b
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
    where (==) a b = True

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
} deriving(Eq,Ord,Show)


type CandyTableForth =  [(Bool,String,String)]
type CandyTableBack  =  [(String,String,Int)]
type CandyTables     =  (CandyTableForth,CandyTableBack)

type SpecialKeyTable =  Map (KeyVal,[Modifier]) (Map (KeyVal,[Modifier]) ActionDescr)
type SpecialKeyCons  =  Maybe ((Map (KeyVal,[Modifier]) ActionDescr),String)

type FileName        =  String

--
-- | Other types
--


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

