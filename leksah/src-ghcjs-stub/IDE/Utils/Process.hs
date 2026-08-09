-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DeriveGeneric #-}

-- | Browser stub of IDE.Utils.Process (GHC JavaScript backend only; picked
-- over @src/@ by hs-source-dirs order).  There is no System.Process in a
-- browser: 'runTool' yields only a failure exit so callers degrade
-- gracefully in the web demo.
module IDE.Utils.Process
  ( ToolOutput(..)
  , toolline
  , runTool
  , ProcessHandle
  , getProcessExitCode
  , interruptProcessGroupOf
  , terminateProcess
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Conduit (ConduitT)
import qualified Data.Conduit as C
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))

data ToolOutput
    = ToolInput Text
    | ToolError Text
    | ToolOutput Text
    | ToolPrompt Text
    | ToolExit ExitCode
    deriving (Eq, Show, Generic)

instance NFData ToolOutput where
    rnf (ToolInput  t) = rnf t
    rnf (ToolError  t) = rnf t
    rnf (ToolOutput t) = rnf t
    rnf (ToolPrompt t) = rnf t
    rnf (ToolExit   c) = c `seq` ()

toolline :: ToolOutput -> Text
toolline (ToolInput  t) = t
toolline (ToolError  t) = t
toolline (ToolOutput t) = t
toolline (ToolPrompt t) = t
toolline (ToolExit _)   = ""

data ProcessHandle = ProcessHandle

runTool :: MonadIO m
        => FilePath -> [Text] -> Maybe FilePath -> Maybe [(String, String)]
        -> IO (ConduitT () ToolOutput m (), ProcessHandle)
runTool _ _ _ _ = return (C.yield (ToolExit (ExitFailure 1)), ProcessHandle)

getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode _ = return (Just (ExitFailure 1))

interruptProcessGroupOf :: ProcessHandle -> IO ()
interruptProcessGroupOf _ = return ()

terminateProcess :: ProcessHandle -> IO ()
terminateProcess _ = return ()
