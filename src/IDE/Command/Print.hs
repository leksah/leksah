{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.Print
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | TODO this module should be moved to some other place, maybe utils
--
-----------------------------------------------------------------------------

module IDE.Command.Print (
    print
    , PrintError (..)
) where

import System.Exit
import Prelude hiding (print)
import IDE.Core.Types
import IDE.Core.State
import IDE.Utils.Tool (readProcessWithExitCode)

data PrintError = PrintError {
    exitCode :: Int
    , stderr :: String
    , printCmd :: FilePath
    } deriving (Read,Show)


#if defined (windows_HOST_OS)
printCommand = "print"
#else
printCommand = "lpr"
#endif

print :: FilePath -> IO (Either PrintError String)
print fileName = do
        (ec, out, err) <- readProcessWithExitCode printCommand [fileName] ""
        case ec of
                ExitSuccess -> return $ Right out
                ExitFailure i -> return $ Left $ PrintError i err printCommand

