--
-- | The packages methods of ghf.
--

module Ghf.Package (
    packageConfig
,   packageBuild
) where

import Ghf.Core
import Ghf.Editor.PackageEditor
import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Environment
import System.Directory
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Configure
import Distribution.Setup
import Distribution.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.PreProcess
import Distribution.Simple.Build
import Data.Maybe
import Prelude hiding (catch)
import Control.Exception

{--
data GhfPackage     =   GhfPackage {
    packageId       ::  PackageIdentifier
,   cabalFile       ::  FilePath
,   configFlags     ::  Maybe ConfigFlags
,   buildFlags      ::  Maybe BuildFlags
    }
    deriving (Eq,Show)
--}

getActivePackage :: GhfM (Maybe GhfPackage)
getActivePackage = do
    active <- readGhf activePack
    case active of
        Just p -> return (Just p)
        Nothing -> selectActivePackage

selectActivePackage :: GhfM (Maybe GhfPackage)
selectActivePackage = do
    window  <- readGhf window
    mbFilePath <- lift $choosePackageFile window
    case mbFilePath of
        Nothing -> return Nothing
        Just filePath -> do
            let flags = emptyConfigFlags defaultProgramConfiguration
            packageD <- lift $readPackageDescription filePath
            let pack = GhfPackage (package packageD) filePath [] []
            modifyGhf_ (\ghf -> return (ghf{activePack = (Just pack)}))
            lift $putStrLn $"Set current directory " ++ dropFileName filePath
            lift $setCurrentDirectory $dropFileName filePath
            return (Just pack)


packageConfig :: Bool -> GhfAction
packageConfig force = return ()



packageBuild :: Bool -> GhfM ()
packageBuild forceReconfig = return ()

{--

getLogBuffer :: GfhM (TextView)


packageConfig :: Bool -> GhfAction
packageConfig force = do
    mbPackage <- getActivePackage
    case mbPackage of
        Nothing -> return ()
        Just package -> do
            logBuffer <- getLogBuffer
            (inp,out,err,pid) <- runExternal "runhaskell"
                                        $["Setup","config"] ++ (configFlags package))
	        oid <- forkIO (readOut name out)
	        eid <- forkIO (readErr name err)



        (configFlags,p2) <- configFlags p1
            buildInfo <- lift $configure packageDescription configFlags
            let pack = p2{localBuildInfo = Just buildInfo}
            return (buildInfo,pack)

runExternal :: FilePath -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
runExternal path args = do
    hndls@(inp, out, err, _) <- runInteractiveProcess path args Nothing Nothing
    putStrLn $ "Starting external tool: " ++ path
    hSetBuffering out NoBuffering
    hSetBuffering err NoBuffering
    hSetBuffering inp NoBuffering
    hSetBinaryMode out True
    hSetBinaryMode err True
    return hndls

readOut :: Handle -> IO ()
readOut hndl = do
    c <- hGetContents hndl
    let c2 = filter (/= '\r') $ tail $ dropWhile (/= '\01') c
    mapM_ app $ parseEscapeCodes c2

readErr :: Evaluator -> Handle -> IO ()
readErr Hugs _ = return ()
    readErr GHCi hndl = do
        c <- hGetContents hndl
        let c2 = filter (/= '\r') c
        mapM_ (\x -> appendRed dat [x]) c2

app (Left c) = appendText dat [c]
app (Right (FormatUnknown 50)) = running dat -< False
app (Right e) = applyEscape dat e




getPackageDescription :: GhfPackage -> GhfM (PackageDescription,GhfPackage)
getPackageDescription package =
    case packageDescr package of
        Nothing -> do
            pd <- lift $readPackageDescription (cabalFile package)
            return (pd,package{packageDescr = Just pd})
        Just pd -> return (pd,package)

getConfigFlags :: GhfPackage -> GhfM (ConfigFlags,GhfPackage)
getConfigFlags package =
    case configFlags package of
        Nothing -> do
            let flags = emptyConfigFlags defaultProgramConfiguration
            return (flags,package{configFlags = Just flags})
        Just flags -> return (flags,package)


getBuildFlags :: GhfPackage -> GhfM (BuildFlags,GhfPackage)
getBuildFlags package =
    case buildFlags package of
        Nothing -> do
            let flags = BuildFlags 2
            return (flags,package{buildFlags = Just flags})
        Just flags -> return (flags,package)


packageConfig :: Bool -> GhfAction
packageConfig force = do
    mbPackage <- getActivePackage
    case mbPackage of
        Nothing -> return ()
        Just package -> do
            (_,pack) <- getLocalBuildInfo package force
            modifyGhf_ (\ghf -> return (ghf{activePack = (Just pack)}))
            return ()

packageBuild :: Bool -> GhfM ()
packageBuild forceReconfig = do
    mbPackage <- getActivePackage
    case mbPackage of
        Nothing -> return ()
        Just package -> do
            (packageDescription,p1) <- getPackageDescription package
            (buildInfo,p2) <- getLocalBuildInfo p1 forceReconfig
            (buildFlags,p3) <- getBuildFlags p2
            lift $build packageDescription buildInfo buildFlags knownSuffixHandlers
            modifyGhf_ (\ghf -> return (ghf{activePack = (Just p3)}))


--}
