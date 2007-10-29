-----------------------------------------------------------------------------
--
--                CollectorMain
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | This executable collects information for packages to make navigation and information
--  accessible to ghf
--
-------------------------------------------------------------------------------

import GHC
import DynFlags hiding(Option)
import System.Environment
import Config
import System.Console.GetOpt

import Ghf.Collector
import Ghf.SourceCollector
import Ghf.File


data Flag =  UninstalledProject String | Rebuild | Sources
       deriving (Show,Eq)

options :: [OptDescr Flag]
options =   [Option ['r'] ["Rebuild"] (NoArg Rebuild)
                "Cleans all .pack files and rebuild everything"
         ,   Option ['u'] ["Uninstalled"] (ReqArg UninstalledProject "FILE")
                "Gather info about an uninstalled package"
         ,   Option ['s'] ["Sources"] (NoArg Sources)
                "Gather info about pathes to sources"]

ghfOpts :: [String] -> IO ([Flag], [String])
ghfOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $userError $concat errs ++ usageInfo header options
    where header = "Usage: ghf [OPTION...] files..."

-- |Build the main window
main = defaultErrorHandler defaultDynFlags $do

    args            <-  getArgs
    (o,fl)          <-  ghfOpts args
    if elem Sources o
        then do
            buildSourceForPackageDB
            putStrLn "rebuild SourceForPackageDB"
        else do
        libDir          <-  getSysLibDir
    --    putStrLn $"libdir '" ++ normalise libDir ++ "'"
#if __GHC__ > 670
        session     <-  newSession (Just libDir)
#else
        session     <-  newSession JustTypecheck (Just libDir)
#endif
        dflags0         <-  getSessionDynFlags session
        setSessionDynFlags session dflags0
        let version     =   cProjectVersion
        let uninstalled =   filter (\x -> case x of UninstalledProject _ -> True
                                                    otherwise -> False) o
        if length uninstalled > 0
            then mapM_ (collectUninstalled session version)
                    $ map (\ (UninstalledProject x) -> x) uninstalled
            else collectInstalled session version (elem Rebuild o)
