module IDE.Pane.SourceBuffer where

import Control.Monad.IO.Class (MonadIO(..))

import Data.Text (Text)

import System.Log.Logger (errorM)

import IDE.Core.Types
       (IDEPackage(..), Project(..), MonadIDE, LogRef, IDEM, IDEAction,
        LogRefType, Log)
import IDE.Core.State
       (Location(..), addLogRef', removeBuildLogRefs', removeFileExtLogRefs',
        removeTestLogRefs')
import IDE.Core.CTypes (PackModule)

newtype IDEBuffer = IDEBuffer {
    fileName        ::  Maybe FilePath
  }

setModifiedOnDisk :: MonadIDE m => FilePath -> m Bool
setModifiedOnDisk _ = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk setModifiedOnDisk"
  return False

markRefInSourceBuf :: IDEBuffer -> LogRef -> Bool -> IDEAction
markRefInSourceBuf _ _ _ = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk markRefInSourceBuf"
  return ()

unmarkRefInSourceBuf :: IDEBuffer -> LogRef -> IDEAction
unmarkRefInSourceBuf _ _ = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk unmarkRefInSourceBuf"
  return ()

selectSourceBuf :: MonadIDE m => FilePath -> m (Maybe IDEBuffer)
selectSourceBuf _ = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk selectSourceBuf"
  return Nothing

allBuffers :: MonadIDE m => m [IDEBuffer]
allBuffers = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk allBuffers"
  return []

goToSourceDefinition :: FilePath -> Location -> IDEM (Maybe IDEBuffer)
goToSourceDefinition _ _ = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk goToSourceDefinition"
  return Nothing

goToLocation :: Maybe PackModule -> Maybe Location -> IDEAction
goToLocation _ _ = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk goToLocation"
  return ()

removeBuildLogRefs :: FilePath -> IDEAction
removeBuildLogRefs file = removeBuildLogRefs' file (const $return ())

removeFileExtLogRefs :: Log -> String -> [LogRefType] -> IDEAction
removeFileExtLogRefs log' fileExt types = removeFileExtLogRefs' log' fileExt types (const $return ())

removeTestLogRefs :: Log -> IDEAction
removeTestLogRefs log' = removeTestLogRefs' log' (const $ return ())

fileSaveAll :: MonadIDE m => (IDEBuffer -> m Bool) -> m Bool
fileSaveAll _ = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk fileSaveAll"
  return False

belongsToWorkspace' :: MonadIDE m => IDEBuffer -> m Bool
belongsToWorkspace' _ = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk belongsToWorkspace'"
  return False

belongsToPackages' :: MonadIDE m => IDEBuffer -> m [(Project, IDEPackage)]
belongsToPackages' _ = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk belongsToPackages'"
  return []

selectedLocation :: IDEM (Maybe (Int, Int))
selectedLocation = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk selectedLocation"
  return Nothing

selectedText :: IDEM (Maybe IDEBuffer, Maybe Text)
selectedText = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk selectedText"
  return (Nothing, Nothing)

selectedModuleName  :: IDEM (Maybe Text)
selectedModuleName = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk selectedModuleName"
  return Nothing

insertTextAfterSelection :: Text -> IDEAction
insertTextAfterSelection _ = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk insertTextAfterSelection"
  return ()

selectedTextOrCurrentLine :: IDEM (Maybe (IDEBuffer, Text))
selectedTextOrCurrentLine = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk selectedTextOrCurrentLine"
  return Nothing

addLogRef :: Bool -> Bool -> LogRef -> IDEAction
addLogRef hlintFileScope backgroundBuild ref =
    addLogRef' hlintFileScope backgroundBuild ref $ return ()

fileCheckAll :: MonadIDE m => (IDEBuffer -> m [alpha]) -> m [alpha]
fileCheckAll _filterFunc = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk fileCheckAll"
  return []

maybeActiveBuf :: IDEM (Maybe IDEBuffer)
maybeActiveBuf = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk maybeActiveBuf"
  return Nothing

fileOpenThis :: FilePath -> IDEAction
fileOpenThis _fp = do
  liftIO $ errorM "leksah" "TODO SourceBuffer no-gtk fileOpenThis"
  return ()
