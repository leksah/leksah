{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.GHCUtils
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  Jutaro <jutaro@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
module IDE.Utils.GHCUtils (
    inGhcIO
,   getInstalledPackageInfos
,   findFittingPackages
,   myParseModule
,   myParseHeader
) where

import Distribution.Simple (withinRange,PackageIdentifier(..),Dependency(..))
#if MIN_VERSION_ghc(7,10,0)
import PackageConfig (sourcePackageIdString)
import Distribution.Text (simpleParse)
import Data.Maybe (fromJust)
#else
import qualified Distribution.InstalledPackageInfo as IPI  (sourcePackageId)
#endif
import GHC
import DriverPipeline(preprocess)
import StringBuffer (StringBuffer(..),hGetStringBuffer)
import FastString (mkFastString)
import Lexer (mkPState,ParseResult(..),getMessages,unP)
import Outputable (ppr)
#if MIN_VERSION_ghc(7,2,0)
#if MIN_VERSION_ghc(7,6,0)
import Bag (unitBag)
#else
import ErrUtils (printBagOfWarnings)
#endif
import ErrUtils (dumpIfSet_dyn,printBagOfErrors,errorsFound,mkPlainErrMsg,showPass,ErrMsg(..))
import Control.Monad (unless)
#else
import ErrUtils (dumpIfSet_dyn,printErrorsAndWarnings,mkPlainErrMsg,showPass,ErrMsg(..))
#endif
import PackageConfig (PackageConfig)
import Data.Foldable (maximumBy)
import qualified Parser as P (parseModule,parseHeader)
import HscStats (ppSourceStats)
#if MIN_VERSION_ghc(7,2,0)
#if !MIN_VERSION_ghc(7,7,0)
import GhcMonad (Ghc(..))
#endif
import SrcLoc (mkRealSrcLoc)
#else
import HscTypes (Ghc(..))
#endif
import IDE.Utils.FileUtils (getSysLibDir)
#if MIN_VERSION_ghc(7,7,0)
import DynFlags (DumpFlag(..), gopt_set, PkgConfRef(..))
#else
import DynFlags (dopt_set, PkgConfRef(..))
#endif
import System.Log.Logger(debugM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Monoid ((<>))
import Data.Function (on)

#if !MIN_VERSION_ghc(7,7,0)
-- this should not be repeated here, why is it necessary?
instance MonadIO Ghc where
  liftIO ioA = Ghc $ \_ -> ioA
#endif

#if MIN_VERSION_ghc(7,7,0)
inGhcIO :: FilePath -> [Text] -> [GeneralFlag] -> [FilePath] -> (DynFlags -> Ghc a) -> IO a
#else
inGhcIO :: FilePath -> [Text] -> [DynFlag] -> [FilePath] -> (DynFlags -> Ghc a) -> IO a
#endif
inGhcIO libDir flags' udynFlags dbs ghcAct = do
    debugM "leksah-server" $ "inGhcIO called with: " ++ show flags'
--    (restFlags, _) <-   parseStaticFlags (map noLoc flags')
    runGhc (Just libDir) $ do
        dynflags  <- getSessionDynFlags
#if MIN_VERSION_ghc(7,7,0)
        let dynflags' = foldl gopt_set dynflags udynFlags
#else
        let dynflags' = foldl dopt_set dynflags udynFlags
#endif
        let dynflags'' = dynflags' {
            hscTarget = HscNothing,
            ghcMode   = CompManager,
            ghcLink   = NoLink,
            extraPkgConfs = (map PkgConfFile dbs++) . extraPkgConfs dynflags'
            }
        dynflags''' <- parseGhcFlags dynflags'' (map (noLoc . T.unpack) flags') flags'
        res <- defaultCleanupHandler dynflags''' $ do
            setSessionDynFlags dynflags'''
            getSessionDynFlags >>= ghcAct
        unload
        return res
    where
        parseGhcFlags :: DynFlags -> [Located String]
                  -> [Text] -> Ghc DynFlags
        parseGhcFlags dynflags flags_ _origFlags = do
            (dynflags', rest, _) <- parseDynamicFlags dynflags flags_
            if not (null rest)
                then do
                    liftIO $ debugM "leksah-server" ("No dynamic GHC options: " ++ unwords (map unLoc rest))
                    return dynflags'
                else return dynflags'

-- | Unload whatever is currently loaded.
unload :: Ghc ()
unload = do
   setTargets []
   load LoadAllTargets
   return ()

getInstalledPackageInfos :: Ghc [PackageConfig]
getInstalledPackageInfos = do
    dflags1         <-  getSessionDynFlags
#if !MIN_VERSION_ghc(7,6,0)
    setSessionDynFlags $ dopt_set dflags1 Opt_ReadUserPackageConf
#endif
    case pkgDatabase dflags1 of
        Nothing -> return []
#if MIN_VERSION_ghc(8,0,0)
        Just fm -> return (fm >>= snd)
#else
        Just fm -> return fm
#endif

findFittingPackages :: [Dependency] -> Ghc [PackageIdentifier]
findFittingPackages dependencyList = do
    knownPackages   <-  getInstalledPackageInfos
#if MIN_VERSION_ghc(7,10,0)
    let packages    =   map (fromJust . simpleParse . sourcePackageIdString) knownPackages
#else
    let packages    =   map IPI.sourcePackageId knownPackages
#endif
    return (concatMap (fittingKnown packages) dependencyList)
    where
    fittingKnown packages (Dependency dname versionRange) =
        let filtered =  filter (\ (PackageIdentifier name version) ->
                                    name == dname && withinRange version versionRange)
                        packages
        in  if length filtered > 1
                then [maximumBy (compare `on` pkgVersion) filtered]
                else filtered

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
      buf' <- case maybe_src_buf of
                Just b  -> return b
                Nothing -> hGetStringBuffer src_filename

#if MIN_VERSION_ghc(7,2,0)
      let loc  = mkRealSrcLoc (mkFastString src_filename) 1 0
#else
      let loc  = mkSrcLoc (mkFastString src_filename) 1 0
#endif

      case unP P.parseModule (mkPState dflags buf' loc) of {

#if MIN_VERSION_ghc(7,6,0)
        PFailed span' err -> do {
            let {errMsg = mkPlainErrMsg dflags span' err};
            printBagOfErrors dflags (unitBag errMsg);
            return (Left errMsg);
            };
#else
        PFailed span' err -> return (Left (mkPlainErrMsg span' err));
#endif

        POk pst rdr_module -> do {

#if MIN_VERSION_ghc(7,2,0)
      let {ms@(warnings, errors) = getMessages pst};
      printBagOfErrors dflags errors;
#if MIN_VERSION_ghc(7,6,0)
      unless (errorsFound dflags ms) $ printBagOfErrors dflags warnings;
#else
      unless (errorsFound dflags ms) $ printBagOfWarnings dflags warnings;
#endif
#else
      let {ms = getMessages pst};
      printErrorsAndWarnings dflags ms;
#endif
      -- when (errorsFound dflags ms) $ exitWith (ExitFailure 1);

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;

      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
                           (ppSourceStats False rdr_module) ;

      return (Right rdr_module)
        -- ToDo: free the string buffer later.
      }}

myParseHeader :: FilePath -> String -> [Text] -> IO (Either Text (DynFlags, HsModule RdrName))
myParseHeader fp _str opts = do
  libDir <- getSysLibDir VERSION_ghc
  inGhcIO libDir (opts++["-cpp"]) [] [] $ \ _dynFlags -> do
    session   <- getSession
#if MIN_VERSION_ghc(7,2,0)
    (dynFlags',fp')    <-  liftIO $ preprocess session (fp,Nothing)
#else
    (dynFlags',fp')    <-  preprocess session (fp,Nothing)
#endif
    liftIO $ do
        stringBuffer  <-  hGetStringBuffer fp'
        parseResult   <-  myParseModuleHeader dynFlags' fp (Just stringBuffer)
        case parseResult of
            Right (L _ mod') -> return (Right (dynFlags', mod'))
            Left errMsg         -> do
                let str =  "Failed to parse " <> T.pack (show errMsg)
                return (Left str)

 ---------------------------------------------------------------------
--  | Parser function copied here, because it is not exported

myParseModuleHeader :: DynFlags -> FilePath -> Maybe StringBuffer
              -> IO (Either ErrMsg (Located (HsModule RdrName)))
myParseModuleHeader dflags src_filename maybe_src_buf
 =  --------------------------  Parser  ----------------
    showPass dflags "Parser" >>
    {-# SCC "Parser" #-} do

        -- sometimes we already have the buffer in memory, perhaps
        -- because we needed to parse the imports out of it, or get the
        -- module name.
      buf' <- case maybe_src_buf of
                Just b  -> return b
                Nothing -> hGetStringBuffer src_filename

#if MIN_VERSION_ghc(7,2,0)
      let loc  = mkRealSrcLoc (mkFastString src_filename) 1 0
#else
      let loc  = mkSrcLoc (mkFastString src_filename) 1 0
#endif

#if MIN_VERSION_ghc(7,0,1)
      case unP P.parseHeader (mkPState dflags buf' loc) of {
#else
      case unP P.parseHeader (mkPState buf' loc dflags) of {
#endif

#if MIN_VERSION_ghc(7,6,0)
        PFailed span' err -> return (Left (mkPlainErrMsg dflags span' err));
#else
        PFailed span' err -> return (Left (mkPlainErrMsg span' err));
#endif

        POk pst rdr_module -> do {

#if MIN_VERSION_ghc(7,2,0)
      let {ms@(warnings, errors) = getMessages pst};
      printBagOfErrors dflags errors;
#if MIN_VERSION_ghc(7,6,0)
      unless (errorsFound dflags ms) $ printBagOfErrors dflags warnings;
#else
      unless (errorsFound dflags ms) $ printBagOfWarnings dflags warnings;
#endif
#else
      let {ms = getMessages pst};
      printErrorsAndWarnings dflags ms;
#endif
      -- when (errorsFound dflags ms) $ exitWith (ExitFailure 1);

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;

      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
                           (ppSourceStats False rdr_module) ;

      return (Right rdr_module)
        -- ToDo: free the string buffer later.
      }}

