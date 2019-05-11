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

,   triggerEventIDE
,   triggerEventIDE_

-- * Convenience methods for accesing the IDE State
,   readIDE
,   modifyIDE
,   modifyIDE_
,   modifyIDEM
,   modifyIDEM_
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

,   liftYiControl
,   liftYi

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
import Control.Event
import System.IO
import Data.Maybe (listToMaybe, isJust)
import System.FilePath
       (takeExtension, takeDirectory, (</>), takeFileName)
import IDE.Core.CTypes as Reexported
import Control.Concurrent
       (modifyMVar, readMVar, forkIO)
import IDE.Utils.Utils as Reexported
import Data.List (sortOn, nub)
import Data.Map (Map)
import qualified Data.Map as M (insert, fromListWith, lookup)
import Data.Typeable(Typeable)
import qualified IDE.TextEditor.Yi.Config as Yi
import Data.Conduit (ConduitT)
import qualified Data.Conduit as C
       (transPipe)
import Control.Monad (unless, join, void)
import Control.Monad.Trans.Reader (ask, ReaderT(..))
import qualified Paths_leksah as P (getDataDir, version)
import System.Environment.Executable (getExecutablePath)
import System.Directory (doesDirectoryExist)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Sequence as Seq
       (partition, length, spanl, filter, null)
import Data.Sequence ((|>), Seq)
import Control.Monad.Trans.Class (MonadTrans(..))
import System.Environment (getEnv)
import Data.Void (Void)
import Language.Javascript.JSaddle (runJSM, JSM)
import Control.Lens
       ((^.), view, over, traverse, (.~), _Just, Getter, to, _1, _2, _3,
        Getting, Lens')
import qualified Data.Foldable as F (Foldable(..))
import Language.Haskell.HLint3 (Idea(..))
import System.Log.Logger (debugM)
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
    triggerEventIDE_ (LogMessage (str <> "\n") LogTag)

logMessage :: MonadIDE m => Text -> LogTag -> m ()
logMessage str tag =
    triggerEventIDE_ (LogMessage (str <> "\n") tag)
-- with hslogger

---- ---------------------------------------------------------------------
---- Exception handling
----

newtype IDEException = IDEException Text
    deriving Typeable

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
setCurrentError e = do
    modifyIDE_ $ currentError .~ e
    triggerEventIDE_ (CurrentErrorChanged e)
setCurrentBreak b = do
    modifyIDE_ $ currentBreak .~ b
    triggerEventIDE_ (CurrentBreakChanged b)
setCurrentContext c = modifyIDE_ $ currentContext .~ c

lookupDebugState :: MonadIDE m => (ProjectKey, FilePath) -> m (Maybe DebugState)
lookupDebugState (project, package) =
    listToMaybe . filter (\DebugState{..} -> dsProjectKey == project && any ((== package) . ipdCabalFile) dsPackages) <$> readIDE debugState

isInterpreting :: MonadIDE m => (ProjectKey, FilePath) -> m Bool
isInterpreting = fmap isJust . lookupDebugState

triggerEventIDE :: MonadIDE m => IDEEvent -> m IDEEvent
triggerEventIDE e = liftIDE $ ask >>= \ideR -> triggerEvent ideR e

triggerEventIDE_ :: MonadIDE m => IDEEvent -> m ()
triggerEventIDE_ = void . triggerEventIDE

--
-- | A reader monad for a mutable reference to the IDE state
--

reifyIDE :: MonadIDE m => (IDERef -> IO a) -> m a
reifyIDE = liftIDE . ReaderT

reflectIDE :: IDEM a -> IDERef -> IO a
reflectIDE = runReaderT

reflectIDEI :: ConduitT a Void IDEM () -> IDERef -> ConduitT a Void IO ()
reflectIDEI c ideR = C.transPipe (`reflectIDE` ideR) c

liftYiControl :: Yi.ControlM a -> IDEM a
liftYiControl f = do
    control <- readIDE yiControl
    liftIO $ Yi.runControl f control

liftYi :: Yi.YiM a -> IDEM a
liftYi = liftYiControl . Yi.liftYi

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
        newIde <- f ide
        return ((a, newIde), a newIde))

-- | Variation on modifyIDE_ that lets you return a value
--   Do not use function that may block on IDE MVar
modifyIDEM :: MonadIDE m => (IDE -> IO (IDE,beta)) -> m beta
modifyIDEM f = do
    e <- liftIDE ask
    liftIO $ do
        (t, b) <- modifyMVar e (\(a, ide) -> do
            (newIde, b) <- f ide
            return ((a, newIde), (a newIde, b)))
        t
        return b

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

-- | Find a directory relative to the leksah install directory
leksahSubDir :: FilePath    -- ^ Sub directory to look for
             -> IO (Maybe FilePath)
leksahSubDir subDir = do
    exePath <- getExecutablePath
    if takeFileName exePath == "leksah.exe"
        then do
            let dataDir = takeDirectory (takeDirectory exePath) </> subDir
            exists <- doesDirectoryExist dataDir
            return (if exists then Just dataDir else Nothing)
        else return Nothing

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

    triggerEventIDE_ $ ErrorAdded
        (not backgroundBuild && Seq.null moreImportant) (Seq.length moreImportant) ref
    return ()

removeLogRefs' :: (Log -> FilePath -> Bool) -> [LogRefType] -> (Map FilePath [LogRefType] -> IDEAction) -> IDEAction
removeLogRefs' toRemove' types removeFromBuffers = do
    (remove, keep) <- Seq.partition toRemove <$> readIDE allLogRefs
    let removeDetails = M.fromListWith (<>) . nub $ map (\ref ->
                            (logRefRootPath ref </> logRefFilePath ref,
                            [logRefType ref])) $ F.toList remove
    modifyIDE_ $ allLogRefs .~ keep

    removeFromBuffers removeDetails
--    buffers <- allBuffers
--    let matchingBufs = filter (maybe False (`M.member` removeDetails) . fileName) buffers
--    F.forM_ matchingBufs $ \ IDEBuffer {..} -> do
--        buf <- getBuffer sourceView
--        F.forM_ (maybe [] (fromMaybe [] . (`M.lookup` removeDetails)) fileName) $
--            removeTagByName buf . T.pack . show

    triggerEventIDE_ (ErrorsRemoved False toRemove)
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

