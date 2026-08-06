{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.State
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

module IDE.Core.State (
    errorRefs
,   breakpointRefs
,   contextRefs
,   currentError
,   currentBreak
,   currentContext
,   setCurrentError
,   setCurrentBreak
,   setCurrentContext
,   lookupDebugState
,   isInterpreting


-- * Convenience methods for accesing the IDE State
,   readIDE
,   modifyIDE
,   modifyIDE_
,   modifyIDEM
,   modifyIDEM_
,   focusLog
,   metaLog
,   setLoggerLevel
,   withIDE
,   getIDE
,   throwIDE
,   packageDebugState

,   reifyIDE
,   reflectIDE
,   reflectIDEI
,   catchIDE
,   forkIDE
,   ideJSM
,   ideJSM_

,   sysMessage
,   MessageLevel(..)
,   ideMessage
,   logMessage

,   changePackage
,   changeProject

,   leksahSubDir
,   leksahOrPackageDir
,   getDataDir
,   P.version

,   canResolve
,   addLogRef'
,   removeLogRefs'
,   removeFileLogRefs'
,   removeFileExtLogRefs'
,   removePackageLogRefs'
,   removeBuildLogRefs'
,   removeTestLogRefs'
,   removeLintLogRefs'

,   belongsToPackages
,   belongsToPackage
,   belongsToWorkspace

,   module Reexported

) where

import Prelude ()
import Prelude.Compat
--import Data.IORef
import Control.Exception (Exception, throw, catch, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import IDE.Core.Types as Reexported
import System.IO
import Data.Maybe (listToMaybe, isJust)
import System.FilePath
       (takeExtension, takeDirectory, (</>), takeFileName)
import IDE.Core.CTypes as Reexported
import Control.Concurrent
       (MVar, modifyMVar, modifyMVar_, newMVar, readMVar, forkIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime)
import IDE.Utils.Utils as Reexported
import Data.List (sortOn, nub)
import Data.Map (Map)
import qualified Data.Map as M (insert, fromListWith, lookup, size)
import Data.Conduit (ConduitT)
import qualified Data.Conduit as C
       (transPipe)
import Control.Monad (unless, join, void, when)
import Control.Monad.Trans.Reader (ask, ReaderT(..))
import qualified Paths_leksah as P (getDataDir, version)
import System.Environment.Executable (getExecutablePath)
import System.Directory (doesDirectoryExist, getHomeDirectory)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import qualified Data.Sequence as Seq
       (partition, length, spanl, filter, null)
import Data.Sequence ((|>), Seq)
import Control.Monad.Trans.Class (MonadTrans(..))
import System.Environment (getEnv)
import Data.Void (Void)
import Language.Javascript.JSaddle (runJSM, JSM)
import Control.Lens
       ((^.), view, over, (.~), (%~), _Just, Getter, to, _1, _2, _3,
        Getting, Lens')
import qualified Data.Foldable as F (Foldable(..))
#if !defined(ghcjs_HOST_OS) && !defined(LEKSAH_NO_HLINT)
-- On the JS backend (and under the no-hlint flag, leksah.sh --ghci) hlint is
-- unavailable / dropped; there the Idea stand-in comes from IDE.Core.Types via
-- the re-export above.
import Language.Haskell.HLint (Idea(..))
#endif
import System.Log.Logger (debugM, updateGlobalLogger, setLevel)
import qualified System.Log.Logger as HL (setHandlers)
import System.Log (Priority(..))
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Formatter (simpleLogFormatter)
import Data.Char (toUpper)
import Text.Read (readMaybe)
import Data.Ord (Down(..))
import IDE.Utils.FileUtils (isSubPath)

data MessageLevel = Silent | Normal | High
    deriving (Eq,Ord,Show)


-- Shall be replaced
sysMessage :: MonadIO m =>  MessageLevel -> Text -> m ()
sysMessage _ml str = liftIO $ do
    putStrLn $ T.unpack str
    hFlush stdout

ideMessage :: MonadIDE m => MessageLevel -> Text -> m ()
ideMessage level str = do
    liftIO $ sysMessage level str
    logMessage str LogTag

-- | Append a line to the Log pane (its widget renders 'logLineMap' from the
-- shared state; there is no event in between).
logMessage :: MonadIDE m => Text -> LogTag -> m ()
logMessage str tag =
    modifyIDE_ $ logLineMap %~ \l -> M.insert (M.size l) (str <> "\n", tag) l

---- ---------------------------------------------------------------------
---- Exception handling
----

newtype IDEException = IDEException Text

instance Show IDEException where
  show (IDEException str) = T.unpack str

instance Exception IDEException

throwIDE :: Text -> a
throwIDE str = throw (IDEException str)


errorRefs :: Getter IDE (Seq LogRef)
errorRefs = allLogRefs . to (Seq.filter ((`elem` [ErrorRef, WarningRef, LintRef, TestFailureRef]) . logRefType))

breakpointRefs :: Getter IDE (Seq LogRef)
breakpointRefs = allLogRefs . to (Seq.filter ((== BreakpointRef) . logRefType))

contextRefs :: Getter IDE (Seq LogRef)
contextRefs = allLogRefs . to (Seq.filter ((== ContextRef) . logRefType))

currentError, currentBreak, currentContext :: Lens' IDE (Maybe LogRef)
currentError     = currentEBC . _1
currentBreak     = currentEBC . _2
currentContext   = currentEBC . _3

setCurrentError, setCurrentBreak, setCurrentContext :: MonadIDE m => Maybe LogRef -> m ()
setCurrentError e = modifyIDE_ $ currentError .~ e
setCurrentBreak b = modifyIDE_ $ currentBreak .~ b
setCurrentContext c = modifyIDE_ $ currentContext .~ c

lookupDebugState :: MonadIDE m => (ProjectKey, FilePath) -> m (Maybe DebugState)
lookupDebugState (project, package) =
    listToMaybe . filter (\DebugState{..} -> dsProjectKey == project && any ((== package) . ipdCabalFile) dsPackages) <$> readIDE debugState

isInterpreting :: MonadIDE m => (ProjectKey, FilePath) -> m Bool
isInterpreting = fmap isJust . lookupDebugState

--
-- | A reader monad for a mutable reference to the IDE state
--

reifyIDE :: MonadIDE m => (IDERef -> IO a) -> m a
reifyIDE = liftIDE . ReaderT

reflectIDE :: IDEM a -> IDERef -> IO a
reflectIDE = runReaderT

reflectIDEI :: ConduitT a Void IDEM () -> IDERef -> ConduitT a Void IO ()
reflectIDEI c ideR = C.transPipe (`reflectIDE` ideR) c

catchIDE :: (MonadIDE m, Exception e) => IDEM a -> (e -> IDEM a) -> m a
catchIDE block handler = reifyIDE (\ideR -> catch (reflectIDE block ideR) (\e -> reflectIDE (handler e) ideR))

forkIDE :: MonadIDE m => IDEAction  -> m ()
forkIDE block = reifyIDE (void . forkIO . reflectIDE block)

ideJSM :: MonadIDE m => JSM a -> m [a]
ideJSM f =
    readIDE jsContexts >>= mapM (liftIO . runJSM f)

ideJSM_ :: MonadIDE m => JSM a -> m ()
ideJSM_ f =
    readIDE jsContexts >>= mapM_ (liftIO . runJSM f)

-- ---------------------------------------------------------------------
-- Convenience methods for accesing the IDE State
--

-- | Read an attribute of the contents
readIDE :: MonadIDE m => Getting beta IDE beta -> m beta
readIDE f = do
    e <- liftIDE ask
    liftIO $ view f . snd <$> readMVar e

-- | Modify the contents, without returning a value
modifyIDE_ :: MonadIDE m => (IDE -> IDE) -> m ()
modifyIDE_ f = modifyIDEM_ (return . f)

-- | Variation on modifyIDE_ that lets you return a value
modifyIDE :: MonadIDE m => (IDE -> (IDE,beta)) -> m beta
modifyIDE f = modifyIDEM (return . f)

-- | Modify the contents, without returning a value
--   Do not use function that may block on IDE MVar
modifyIDEM_ :: MonadIDE m => (IDE -> IO IDE) -> m ()
modifyIDEM_ f = do
    e <- liftIDE ask
    liftIO $ join $ modifyMVar e (\(a, ide) -> do
        newIde <- over ideVersion (+1) <$> f ide
        logMutation (newIde ^. ideVersion)
        return ((a, newIde), a newIde))

-- | Variation on modifyIDE_ that lets you return a value
--   Do not use function that may block on IDE MVar
modifyIDEM :: MonadIDE m => (IDE -> IO (IDE,beta)) -> m beta
modifyIDEM f = do
    e <- liftIDE ask
    liftIO $ do
        (t, b) <- modifyMVar e (\(a, ide) -> do
            (newIde0, b) <- f ide
            let newIde = over ideVersion (+1) newIde0
            logMutation (newIde ^. ideVersion)
            return ((a, newIde), (a newIde, b)))
        t
        return b

-- | Verbose diagnostic: every state mutation bumps 'ideVersion'; log the new
-- value so a stale web-UI window (whose ideVer lags this) is obvious.  Tagged
-- @LEK … [MUT]@ to match 'wlog' in IDE.Web.Main.
logMutation :: Int -> IO ()
logMutation v = do
    t <- getCurrentTime
    hPutStrLn stderr ("LEK " <> (takeWhile (/= ' ') . drop 11 $ show t) <> " [MUT] modifyIDE -> ideVer=" <> show v)

-- | Diagnostic for the metadata-load path (initInfo → loadSystemInfo →
-- updateWorkspaceInfo → InfoChanged).  Tagged @LEK … [meta]@ so it interleaves
-- with 'wlog'/'logMutation' on the SAME stderr the leksah-nix.sh loop shows,
-- letting a startup freeze be pinned to exactly which metadata stage was running
-- and on which thread.  Low volume (once at startup + on workspace-info updates),
-- so — unlike the per-keystroke focus log — it writes unconditionally.
metaLog :: MonadIO m => String -> m ()
metaLog msg = liftIO $ do
    t <- getCurrentTime
    hPutStrLn stderr ("LEK " <> (takeWhile (/= ' ') . drop 11 $ show t) <> " [meta] " <> msg)

-- | Name of the hslogger logger that carries focus/activation-path diagnostics.
-- Hierarchical under @leksah@, but with its OWN file handler (see
-- 'ensureFocusHandler') so its output lands in a dedicated, reliably-tailable
-- file rather than the leksah-nix.sh loop's stderr (which gets clobbered by
-- build output).  Off by default (inherits the root INFO level, which drops
-- DEBUG); toggle at runtime with @leksah-cmd log leksah.focus debug|off@ (see
-- 'setLoggerLevel'), or globally with @--verbosity DEBUG@ at launch.
focusLoggerName :: String
focusLoggerName = "leksah.focus"

-- | Ensures the dedicated file handler for 'focusLoggerName' is installed
-- exactly once.  Opens @~/.leksah/focus-debug.log@ line-buffered (so a @tail
-- -f@ sees writes immediately) and attaches it as the focus logger's sole
-- handler.  (Clear the log by truncating — @: > …/focus-debug.log@ — not @rm@:
-- the handler holds the handle open, and an unlinked file writes to a dead
-- inode invisible on disk.)
{-# NOINLINE focusHandlerInstalled #-}
focusHandlerInstalled :: MVar Bool
focusHandlerInstalled = unsafePerformIO (newMVar False)

ensureFocusHandler :: IO ()
ensureFocusHandler = modifyMVar_ focusHandlerInstalled $ \done ->
    if done then return True else (`catch` \(_ :: SomeException) -> return done) $ do
        home <- getHomeDirectory
        h <- openFile (home </> ".leksah" </> "focus-debug.log") AppendMode
        hSetBuffering h LineBuffering
        sh <- streamHandler h DEBUG
        -- The message already carries its own µs timestamp, so emit it verbatim.
        updateGlobalLogger focusLoggerName
            (HL.setHandlers [setFormatter sh (simpleLogFormatter "$msg")])
        return True

-- | Focus/activation-path diagnostic.  A DEBUG record on 'focusLoggerName';
-- hslogger gates it by the (effective) level, so it costs nothing while the
-- logger is off (the default).  Building the µs timestamp per call is cheap and
-- only happens on genuine focus events (the oscillation that once fired this
-- ~500×/s is fixed).
focusLog :: MonadIO m => String -> m ()
focusLog msg = liftIO $ do
    t <- getCurrentTime
    let tod = takeWhile (/= ' ') . drop 11 $ show t
    debugM focusLoggerName ("FOCUS " <> tod <> " " <> msg)

-- | Runtime logger-level control backing @leksah-cmd log <logger> <level>@.
-- Accepts any hslogger 'Priority' name (case-insensitive) plus @off@/@none@
-- (which maps to INFO, suppressing our DEBUG records).  When the focus logger
-- is switched to DEBUG its dedicated file handler is installed on demand.
setLoggerLevel :: String -> String -> IO Text
setLoggerLevel name levelStr =
    case parse (map toUpper levelStr) of
      Nothing -> return $ "log: unknown level '" <> T.pack levelStr
          <> "' (use: off, debug, info, notice, warning, error, critical, alert, emergency)\n"
      Just prio -> do
          when (name == focusLoggerName && prio == DEBUG) ensureFocusHandler
          updateGlobalLogger name (setLevel prio)
          return $ "log: " <> T.pack name <> " level = " <> T.pack (show prio) <> "\n"
  where
    parse "OFF"  = Just INFO
    parse "NONE" = Just INFO
    parse s      = readMaybe s

withIDE :: MonadIDE m => (IDE -> IO alpha) -> m alpha
withIDE f = do
    e <- liftIDE ask
    liftIO $ f . snd =<< readMVar e

getIDE :: MonadIDE m => m IDE
getIDE = liftIDE ask >>= (fmap snd . liftIO . readMVar)

packageDebugState :: PackageM (Maybe DebugState)
packageDebugState = do
    project <- lift ask
    package <- ask
    lookupDebugState (pjKey project, ipdCabalFile package)


-- | Replaces an 'IDEPackage' in the workspace by the given 'IDEPackage' and
-- replaces the current package if it matches.
--  Comparison is done based on the package's build directory.
changePackage :: IDEPackage -> IDEAction
changePackage ideP =
    modifyIDE_ $
          over (workspace . _Just . wsProjects . traverse) (\p -> p {
                  pjPackageMap = mkPackageMap $ map exchange (pjPackages p)})
        . ( bufferProjCache .~ mempty )
  where
    key = ipdPackageDir
    idePKey = key ideP
    exchange p | key p == idePKey = ideP
               | otherwise        = p

-- | Replaces an 'Project' in the workspace by the given 'Project' and
-- replaces the current package if it matches.
--  Comparison is done based on the package's build directory.
changeProject :: Project -> IDEAction
changeProject project =
    modifyIDE_ $
          over (workspace . _Just . wsProjects . traverse) exchange
        . ( bufferProjCache .~ mempty )
    where
        exchange p | pjKey p == pjKey project = project
                   | otherwise        = p

-- | Find a directory relative to the leksah install directory, so binary
-- packages can carry their data dir next to the executable (see
-- 'leksahOrPackageDir').  Two relocatable layouts are recognised:
--
--   * Windows: @\<installroot>\\bin\\leksah.exe@ → @\<installroot>\\\<subDir>@
--   * macOS:   @Leksah.app\/Contents\/MacOS\/leksah@ →
--              @Leksah.app\/Contents\/Resources\/\<subDir>@
leksahSubDir :: FilePath    -- ^ Sub directory to look for
             -> IO (Maybe FilePath)
leksahSubDir subDir = do
    exePath <- getExecutablePath
    let candidate
          -- Windows install tree.
          | takeFileName exePath == "leksah.exe" =
              Just (takeDirectory (takeDirectory exePath) </> subDir)
          -- macOS .app bundle (exe is Contents/MacOS/leksah).
          | takeFileName exePath == "leksah"
            && takeFileName (takeDirectory exePath) == "MacOS" =
              Just (takeDirectory (takeDirectory exePath) </> "Resources" </> subDir)
          | otherwise = Nothing
    case candidate of
        Nothing      -> return Nothing
        Just dataDir -> do
            exists <- doesDirectoryExist dataDir
            return (if exists then Just dataDir else Nothing)

-- | Get the leksah data dir based on the executable name or if that fails
-- use the directroy for the package.  This is allows us to make binary packages
-- where the data directory id relative to the leksah executable.
-- This is important for Wind32 where setting environment variables for the
-- locations in a launch script causes problems (you can't pin the exe).
leksahOrPackageDir :: FilePath    -- ^ Sub directory to look for
                   -> IO FilePath -- ^ Used to get the package dir if we can't find the leksah one
                   -> IO FilePath
leksahOrPackageDir subDir getPackageDir =
    catch (not . null <$> getEnv (subDir <> "_datadir")) (\(_ :: SomeException) -> return False) >>= \case
        True -> getPackageDir
        False ->
            leksahSubDir subDir >>= \case
                Just result -> return result
                Nothing     -> getPackageDir

getDataDir :: MonadIO m => m FilePath
getDataDir = liftIO $ leksahOrPackageDir "leksah" P.getDataDir

canResolve :: LogRef -> Bool
canResolve LogRef { logRefIdea = Just (_, Idea{..}) }
    = ideaHint /= "Reduce duplication" && isJust ideaTo
canResolve _ = False

addLogRef' :: Bool -> Bool -> LogRef -> IDEAction -> IDEAction
addLogRef' hlintFileScope backgroundBuild ref markInBuffers = unless (srcSpanFilename (logRefSrcSpan ref) == "<interactive>") $ do
    liftIO . debugM "leksah" $ "addLogRef " <> show hlintFileScope <> " " <> show (logRefType ref) <> " " <> logRefFullFilePath ref
    -- Put most important errors first.
    -- If the importance of two errors is the same then
    -- then the older one might be stale (unless it is in the same file)
    allLogRefs'   <- readIDE allLogRefs
    currentError' <- readIDE currentError
    let (moreImportant, rest) =
           Seq.spanl (\old ->
                let samePackage = logRefRootPath old     == logRefRootPath ref
                    sameFile    = logRefFullFilePath old == logRefFullFilePath ref in
                -- Work out when the old ref is more important than the new
                case (logRefType ref, logRefType old) of
                    (ErrorRef      , ErrorRef      ) -> sameFile
                    (ErrorRef      , _             ) -> False
                    (WarningRef    , ErrorRef      ) -> samePackage
                    (WarningRef    , WarningRef    ) -> samePackage
                    (WarningRef    , _             ) -> False
                    (TestFailureRef, ErrorRef      ) -> samePackage  -- Probably should never be True
                    (TestFailureRef, TestFailureRef) -> samePackage
                    (TestFailureRef, _             ) -> False
                    (LintRef       , LintRef       ) -> (if hlintFileScope then sameFile else samePackage)
                                                            && (canResolve old
                                                               || not (canResolve ref))
                    (LintRef       , _             ) -> samePackage
                    (ContextRef    , _             ) -> False
                    (BreakpointRef , _             ) -> False) allLogRefs'
        currErr = if currentError' `elem` map Just (F.toList moreImportant)
                        then currentError'
                        else Nothing
    modifyIDE_ $
          (allLogRefs .~ (moreImportant |> ref) <> rest)
        . (currentError .~ currErr)

    markInBuffers

removeLogRefs' :: (Log -> FilePath -> Bool) -> [LogRefType] -> (Map FilePath [LogRefType] -> IDEAction) -> IDEAction
removeLogRefs' toRemove' types removeFromBuffers = do
    (remove, keep) <- Seq.partition toRemove <$> readIDE allLogRefs
    liftIO . debugM "leksah" $ "removeLogRefs': removing " <> show (length remove)
        <> ", keeping " <> show (length keep)
    let removeDetails = M.fromListWith (<>) . nub $ map (\ref ->
                            (logRefRootPath ref </> logRefFilePath ref,
                            [logRefType ref])) $ F.toList remove
    modifyIDE_ $ allLogRefs .~ keep

    removeFromBuffers removeDetails
  where
    toRemove ref = toRemove' (logRefLog ref) (logRefFilePath ref)
                && logRefType ref `elem` types

removeFileLogRefs' :: FilePath -> [LogRefType] -> (Map FilePath [LogRefType] -> IDEAction) -> IDEAction
removeFileLogRefs' file types removeFromBuffers = do
    liftIO . debugM "leksah" $ "removeFileLogRefs " <> file <> " " <> show types
    removeLogRefs' (\l f -> logRootPath l </> f == file) types removeFromBuffers

removeFileExtLogRefs' :: Log -> String -> [LogRefType] -> (Map FilePath [LogRefType] -> IDEAction) -> IDEAction
removeFileExtLogRefs' log' fileExt types removeFromBuffers = do
    liftIO . debugM "leksah" $ "removeFileTypeLogRefs " <> show log' <> " " <> fileExt <> " " <> show types
    removeLogRefs' (\l f -> l == log' && takeExtension f == fileExt) types removeFromBuffers

removePackageLogRefs' :: Log -> [LogRefType] -> (Map FilePath [LogRefType] -> IDEAction) -> IDEAction
removePackageLogRefs' log' types removeFromBuffers = do
    liftIO . debugM "leksah" $ "removePackageLogRefs " <> show log' <> " " <> show types
    removeLogRefs' (\l _ -> l == log') types removeFromBuffers

removeBuildLogRefs' :: FilePath -> (Map FilePath [LogRefType] -> IDEAction) -> IDEAction
removeBuildLogRefs' file = removeFileLogRefs' file [ErrorRef, WarningRef]

removeTestLogRefs' :: Log -> (Map FilePath [LogRefType] -> IDEAction) -> IDEAction
removeTestLogRefs' log' = removePackageLogRefs' log' [TestFailureRef]

removeLintLogRefs' :: FilePath -> (Map FilePath [LogRefType] -> IDEAction) -> IDEAction
removeLintLogRefs' file = removeFileLogRefs' file [LintRef]

-- | Returns the packages to which this file belongs
--   uses the 'bufferProjCache' and might extend it
belongsToPackages :: MonadIDE m => FilePath -> m [(Project, IDEPackage)]
belongsToPackages fp = do
    bufferToProject' <-  readIDE bufferProjCache
    case M.lookup fp bufferToProject' of
        Just p  -> return p
        Nothing -> readIDE workspace >>= \case
                        Nothing   -> return []
                        Just ws -> do
                            let res = sortOn (Down . length . ipdPackageDir . snd) .
                                         filter (belongsToPackage fp . snd) $ ws ^. wsProjectAndPackages
                            modifyIDE_ $ bufferProjCache .~ M.insert fp res bufferToProject'
                            return res

-- | Checks whether a file belongs to a package (includes files in
-- sandbox source dirs)
belongsToPackage :: FilePath -> IDEPackage -> Bool
belongsToPackage f = (`isSubPath` f) . ipdPackageDir

-- | Checks whether a file belongs to the workspace
belongsToWorkspace :: MonadIDE m => FilePath -> m Bool
belongsToWorkspace fp = not . null <$> belongsToPackages fp

