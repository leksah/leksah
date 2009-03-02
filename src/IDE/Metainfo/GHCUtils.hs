-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.GHCUtils
-- Copyright   :  2007-2009 Jürgen Nicklisch-Franken
-- License     :  GPL
--
-- Maintainer  :  Jutaro <jutaro@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
module IDE.Metainfo.GHCUtils (

    inGhc
,   inGhcIO
,   getInstalledPackageInfos
,   findFittingPackages
,   myParseModule
,   parseHeader

) where

import IDE.Core.State (MessageLevel(..),sysMessage,IDEM(..))
import UniqFM (eltsUFM)
import Distribution.Simple (withinRange,PackageIdentifier(..),Dependency(..))
import qualified Distribution.InstalledPackageInfo as IPI  (package)
import GHC
import StringBuffer (StringBuffer(..),hGetStringBuffer,stringToStringBuffer)
import FastString (mkFastString)
import Lexer (mkPState,ParseResult(..),getMessages,unP)
import Outputable (ppr)
import Bag (unitBag)
import IDE.FileUtils (getSysLibDir)
import ErrUtils (dumpIfSet_dyn,printErrorsAndWarnings,mkPlainErrMsg,showPass,ErrMsg(..),printBagOfErrors)
import PackageConfig (PackageConfig(..))
import Data.Foldable (maximumBy)
import DynFlags (defaultDynFlags)
import qualified Parser as P  (parseModule,parseHeader)
import HscStats (ppSourceStats)
import Control.Monad.Trans


inGhc :: Ghc a -> IDEM a
inGhc = liftIO . inGhcIO

inGhcIO :: Ghc a -> IO a
inGhcIO f = do
    libDir          <-   getSysLibDir
    runGhc (Just libDir) $ do
        getSessionDynFlags >>= setSessionDynFlags
        f

getInstalledPackageInfos :: Ghc [PackageConfig]
getInstalledPackageInfos = do
    dflags1         <-  getSessionDynFlags
    setSessionDynFlags dflags1{flags = Opt_ReadUserPackageConf : (flags dflags1)}
    pkgInfos        <-  case pkgDatabase dflags1 of
                            Nothing -> return []
                            Just fm -> return (eltsUFM fm)
    return pkgInfos

findFittingPackages :: [Dependency] -> Ghc [PackageIdentifier]
findFittingPackages dependencyList = do
    knownPackages   <-  getInstalledPackageInfos
    let packages    =   map IPI.package knownPackages
    return (concatMap (fittingKnown packages) dependencyList)
    where
    fittingKnown packages (Dependency dname versionRange) =
        let filtered =  filter (\ (PackageIdentifier name version) ->
                                    name == dname && withinRange version versionRange)
                        packages
        in  if length filtered > 1
                then [maximumBy (\a b -> compare (pkgVersion a) (pkgVersion b)) filtered]
                else filtered

parseHeader :: FilePath -> String -> IO (Maybe [LImportDecl RdrName])
parseHeader fp str = inGhcIO $ do
    dflags             <-  getSessionDynFlags
    let dflags2 = dflags {
--        topDir    = basePath,
        hscTarget = HscNothing,
        ghcMode   = CompManager,
        ghcLink   = NoLink}
    setSessionDynFlags dflags2
    dynFlags           <-  getSessionDynFlags
    liftIO $ do
        stringBuffer       <-  stringToStringBuffer str
        parseResult        <-  myParseModuleHeader dynFlags fp (Just stringBuffer)
        case parseResult of
            Right (L _ mod) -> return (Just (hsmodImports mod))
            Left errMsg     -> do
                sysMessage Normal $ "Failed to parse " ++ fp
                printBagOfErrors defaultDynFlags (unitBag errMsg)
                return Nothing

 ---------------------------------------------------------------------
--  | Parser function copied here, because it is not exported

myParseModule :: DynFlags -> FilePath -> Maybe StringBuffer
              -> IO (Either ErrMsg (Located (HsModule RdrName)))
myParseModule dflags src_filename maybe_src_buf
 =    --------------------------  Parser  ----------------
      showPass dflags "Parser" >>
      {-# SCC "Parser" #-} do

	-- sometimes we already have the buffer in memory, perhaps
	-- because we needed to parse the imports out of it, or get the
	-- module name.
      buf <- case maybe_src_buf of
		Just b  -> return b
		Nothing -> hGetStringBuffer src_filename

      let loc  = mkSrcLoc (mkFastString src_filename) 1 0

      case unP P.parseModule (mkPState buf loc dflags) of {

	PFailed span err -> return (Left (mkPlainErrMsg span err));

	POk pst rdr_module -> do {

      let {ms = getMessages pst};
      printErrorsAndWarnings dflags ms;
      -- when (errorsFound dflags ms) $ exitWith (ExitFailure 1);

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;

      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module) ;

      return (Right rdr_module)
	-- ToDo: free the string buffer later.
      }}

 ---------------------------------------------------------------------
--  | Parser function copied here, because it is not exported

myParseModuleHeader :: DynFlags -> FilePath -> Maybe StringBuffer
              -> IO (Either ErrMsg (Located (HsModule RdrName)))
myParseModuleHeader dflags src_filename maybe_src_buf
 =    --------------------------  Parser  ----------------
      showPass dflags "Parser" >>
      {-# SCC "Parser" #-} do

	-- sometimes we already have the buffer in memory, perhaps
	-- because we needed to parse the imports out of it, or get the
	-- module name.
      buf <- case maybe_src_buf of
		Just b  -> return b
		Nothing -> hGetStringBuffer src_filename

      let loc  = mkSrcLoc (mkFastString src_filename) 1 0

      case unP P.parseHeader (mkPState buf loc dflags) of {

	PFailed span err -> return (Left (mkPlainErrMsg span err));

	POk pst rdr_module -> do {

      let {ms = getMessages pst};
      printErrorsAndWarnings dflags ms;
      -- when (errorsFound dflags ms) $ exitWith (ExitFailure 1);

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;

      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module) ;

      return (Right rdr_module)
	-- ToDo: free the string buffer later.
      }}

