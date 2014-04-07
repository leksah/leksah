-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.VCS.Common.Helper
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Command.VCS.Common.Helper (
    --helper for vcs actions
    eMergeToolSetter
    ,createActionFromContext
) where


import IDE.Core.Types
import IDE.Core.State
import qualified IDE.Workspaces.Writer as Writer
import qualified IDE.Command.VCS.Types as Types

import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI

import Control.Monad.Reader
import Control.Monad.Trans(liftIO)
import Data.Maybe
import qualified Data.Map as Map

{- |
    Runs given vcs-action using the vcs-conf set in the ReaderT.
    Provides a basic exception handler for any errors occuring.
-}
createActionFromContext :: VCS.Ctx()    -- ^ computation to execute, i.e. showCommit
                        -> Types.VCSAction ()
createActionFromContext vcsAction = do
    ((_,conf,_),_) <- ask
    liftIO $ VCSGUI.defaultVCSExceptionHandler $ VCS.runVcs conf vcsAction

{- |
    Creates an 'eMergeToolSetter' (Either MergeTool or MT-Setter) from given parameters.
-}
eMergeToolSetter :: IDERef
                -> FilePath
                -> Maybe VCSGUI.MergeTool
                -> Either VCSGUI.MergeTool (VCSGUI.MergeTool -> IO())
eMergeToolSetter ideRef cabalFp mbMergeTool =
    case mbMergeTool of
        Nothing -> Right $ mergeToolSetter ideRef cabalFp
        Just mergeTool -> Left mergeTool

{- |
    Facility to set a mergetool for a given package.
-}
mergeToolSetter :: IDERef -> FilePath -> VCSGUI.MergeTool -> IO()
mergeToolSetter ideRef cabalFp mergeTool =
    runReaderT (workspaceSetMergeTool cabalFp mergeTool) ideRef

{- |
    Sets the given mergetool for given package in current workspace. Workspace must be set.
-}
workspaceSetMergeTool :: FilePath -> VCSGUI.MergeTool -> IDEAction
workspaceSetMergeTool pathToPackage mergeTool = do
    modifyIDE_ (\ide -> do
        let oldWs = fromJust (workspace ide)
        let oldMap = packageVcsConf oldWs
        case Map.lookup pathToPackage oldMap of
            Nothing -> ide --TODO error
            Just (vcsType,config,_) -> do
                let vcsConf = (vcsType,config,Just mergeTool)
                let newMap = Map.insert pathToPackage vcsConf oldMap
                let newWs = oldWs { packageVcsConf = newMap }
                ide {workspace = Just newWs })
    newWs <- readIDE workspace
    Writer.writeWorkspace $ fromJust newWs
