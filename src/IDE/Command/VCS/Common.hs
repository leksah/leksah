-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.VCS.Common
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

module IDE.Command.VCS.Common (
    createActionFromContext
) where

import qualified VCSWrapper.Common as VCS

import IDE.Core.Types
import IDE.Core.State

import Control.Monad.Reader
import Control.Monad.Trans(liftIO)

createActionFromContext :: VCS.Ctx()    -- ^ computation to execute, i.e. showCommit
                        -> (String   -- ^ error message
                            -> IO())    -- ^ fn for handling errors
                        -> IDEAction
createActionFromContext vcsAction errorHandler = do
    mbWorkspace <- readIDE workspace
    case mbWorkspace of
        Just workspace -> do
             let mbConfig = vcsConfig workspace
             case mbConfig of
                Nothing -> liftIO $ errorHandler "No active repository!"
                Just (_,config) -> liftIO $ VCS.runVcs config $ vcsAction
        Nothing -> return() --TODO error message




