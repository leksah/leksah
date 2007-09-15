--
-- | Main module of Genuine Haskell Face, an Haskell IDE written in Haskell
--

import Paths_ghf(getDataDir)
import Graphics.UI.Gtk.SourceView()    -- Instances only
import Graphics.UI.Gtk(onKeyPress, onDelete, widgetSetName, widgetShowAll,
		       widgetHide, containerAdd, boxPackStart, vBoxNew, uiManagerNew,
		       windowSetIconFromFile, windowAddAccelGroup, windowSetDefaultSize, windowNew,
		       mainGUI, initGUI, timeoutAddFull, priorityHigh, Packing(PackNatural, PackGrow))
import Graphics.UI.Gtk.Types()    -- Instances only
import Control.Monad.Reader(Monad((>>), return), ReaderT(runReaderT), mapM_)
import System.FilePath((</>))
import Control.Concurrent(rtsSupportsBoundThreads, yield)
import Data.IORef(newIORef)
import Data.Maybe(isJust)
import Data.Map(Map.empty)
import System.Console.GetOpt(OptDescr, ArgOrder(Permute), usageInfo, getOpt)
import System.Directory()    -- Instances only
import System.Environment(getArgs)
import System.IO(IO, putStrLn)

import Ghf.SaveLayout(recoverLayout)
import Ghf.Log()    -- Instances only
import Ghf.Core(Prefs(sourceCandy, defaultSize), PaneLayout(TerminalP), Ghf(..))
import Ghf.SourceCandy(parseCandy)
import Ghf.File(getConfigFilePathForLoad)
import Ghf.ViewFrame(newNotebook, setCandyState, getFindBar,
			 getGotoLineSpin)
import Ghf.SourceEditor()    -- Instances only
import Ghf.Statusbar(buildStatusbar)
import Ghf.Menu(actions, menuDescription, makeMenu, quit)
import Ghf.PreferencesEditor(readPrefs)
import Ghf.Keymap(parseKeymap, setKeymap, buildSpecialKeys,
		      handleSpecialKeystrokes)

import qualified Data.Map as Map

data Flag =  OpenFile
       deriving Show

options :: [OptDescr Flag]
options = [ ]

ghfOpts :: [String] -> IO ([Flag], [String])
ghfOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $userError $concat errs ++ usageInfo header options
    where header = "Usage: ghf [OPTION...] files..."

-- |Build the main window
main = do
    args        <-  getArgs
    (o,fl)      <-  ghfOpts args
    st          <-  initGUI
    if rtsSupportsBoundThreads
        then error "Don't link with -theaded, Gtk won't work"
        else timeoutAddFull (yield >> return True) priorityHigh 50
    mapM_ putStrLn st
    uiManager   <-  uiManagerNew
    prefsPath   <-  getConfigFilePathForLoad "Default.prefs"
    prefs       <-  readPrefs prefsPath
    keysPath    <-  getConfigFilePathForLoad "Default.keymap"
    keyMap      <-  parseKeymap keysPath
    let accelActions = setKeymap actions keyMap
    specialKeys <-  buildSpecialKeys keyMap accelActions
    candyPath   <-  getConfigFilePathForLoad
                        (case sourceCandy prefs of
                                    Nothing     ->   "Default.candy"
                                    Just name   ->   name ++ ".candy")
    candySt     <-  parseCandy candyPath
    win         <-  windowNew
    dataDir     <-  getDataDir
    windowSetIconFromFile win $dataDir </> "ghf.gif"
    let ghf = Ghf
          {   window        = win
          ,   uiManager     = uiManager
          ,   panes         = Map.empty
          ,   activePane    = Nothing
          ,   paneMap       = Map.empty
          ,   layout        = (TerminalP Nothing)
          ,   specialKeys   = specialKeys
          ,   specialKey    = Nothing
          ,   candy         = candySt
          ,   prefs         = prefs
          ,   packages      = []
          ,   activePack    = Nothing}
    ghfR <- newIORef ghf
    (acc,menus) <- runReaderT (makeMenu uiManager accelActions menuDescription) ghfR
    let mb = case menus !! 0 of
                Just m -> m
                Nothing -> error "Failed to build menu"
    let tb = case menus !! 1 of
                Just m -> m
                Nothing -> error "Failed to build toolbar"
    windowAddAccelGroup win acc
    nb <- newNotebook
    widgetSetName nb $"root"
    hb <- buildStatusbar ghfR
    vb <- vBoxNew False 1  -- Top-level vbox
    widgetSetName vb "topBox"
    boxPackStart vb mb PackNatural 0
    boxPackStart vb tb PackNatural 0
    boxPackStart vb nb PackGrow 0
    boxPackStart vb hb PackNatural 0
    win `onDelete` (\ _ -> do runReaderT quit ghfR; return True)
    win `onKeyPress` (\ e -> runReaderT (handleSpecialKeystrokes e) ghfR)
    containerAdd win vb
    runReaderT (setCandyState (isJust (sourceCandy prefs))) ghfR
    let (x,y) = defaultSize prefs
    windowSetDefaultSize win x y
    runReaderT recoverLayout ghfR
--    runReaderT flip runReaderT ghfR $ case fl of
--        [] -> newTextBuffer "Unnamed" Nothing
--        otherwise  -> mapM_ (\ fn -> (newTextBuffer (takeFileName fn) (Just fn))) fl
    widgetShowAll win
    hbf <- runReaderT getFindBar ghfR
    widgetHide hbf
    spinL <- runReaderT getGotoLineSpin ghfR
    widgetHide spinL
    mainGUI


