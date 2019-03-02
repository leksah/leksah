module IDE.Pane.SourceBuffer where

import Data.Text (Text)
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
setModifiedOnDisk _ = return False

markRefInSourceBuf :: IDEBuffer -> LogRef -> Bool -> IDEAction
markRefInSourceBuf _ _ _ = return ()

unmarkRefInSourceBuf :: IDEBuffer -> LogRef -> IDEAction
unmarkRefInSourceBuf _ _ = return ()

selectSourceBuf :: MonadIDE m => FilePath -> m (Maybe IDEBuffer)
selectSourceBuf _ = return Nothing

allBuffers :: MonadIDE m => m [IDEBuffer]
allBuffers = return []

goToSourceDefinition :: FilePath -> Location -> IDEM (Maybe IDEBuffer)
goToSourceDefinition _ _ = return Nothing

goToLocation :: Maybe PackModule -> Maybe Location -> IDEAction
goToLocation _ _ = return ()

removeBuildLogRefs :: FilePath -> IDEAction
removeBuildLogRefs file = removeBuildLogRefs' file (const $return ())

removeFileExtLogRefs :: Log -> String -> [LogRefType] -> IDEAction
removeFileExtLogRefs log' fileExt types = removeFileExtLogRefs' log' fileExt types (const $return ())

removeTestLogRefs :: Log -> IDEAction
removeTestLogRefs log' = removeTestLogRefs' log' (const $ return ())

fileSaveAll :: MonadIDE m => (IDEBuffer -> m Bool) -> m Bool
fileSaveAll _ = return False

belongsToWorkspace' :: MonadIDE m => IDEBuffer -> m Bool
belongsToWorkspace' _ = return False

belongsToPackages' :: MonadIDE m => IDEBuffer -> m [(Project, IDEPackage)]
belongsToPackages' _ = return []

selectedLocation :: IDEM (Maybe (Int, Int))
selectedLocation = return Nothing

selectedText :: IDEM (Maybe IDEBuffer, Maybe Text)
selectedText = return (Nothing, Nothing)

selectedModuleName  :: IDEM (Maybe Text)
selectedModuleName = return Nothing

insertTextAfterSelection :: Text -> IDEAction
insertTextAfterSelection _ = return ()

selectedTextOrCurrentLine :: IDEM (Maybe (IDEBuffer, Text))
selectedTextOrCurrentLine = return Nothing

addLogRef :: Bool -> Bool -> LogRef -> IDEAction
addLogRef hlintFileScope backgroundBuild ref =
    addLogRef' hlintFileScope backgroundBuild ref $ return ()

fileCheckAll :: MonadIDE m => (IDEBuffer -> m [alpha]) -> m [alpha]
fileCheckAll _filterFunc = return []

maybeActiveBuf :: IDEM (Maybe IDEBuffer)
maybeActiveBuf = return Nothing

fileOpenThis :: FilePath -> IDEAction
fileOpenThis _fp = return ()
