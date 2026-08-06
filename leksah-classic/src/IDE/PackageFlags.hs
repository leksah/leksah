{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IDE.PackageFlags (
    readFlags
,   readFlagsFromBytes
,   writeFlags
) where

import Prelude ()
import Prelude.Compat
import Data.Text (Text)
import Data.Aeson (eitherDecode, FromJSON, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Generics (Generic)
import qualified Control.Exception as E (catch)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import Control.Exception (IOException)
import Data.Maybe (fromMaybe)
import System.Log.Logger (errorM)

import IDE.Core.State (IDEPackage(..))
-- Flag files live next to the .cabal file, which may be on a remote host —
-- route through the FS seam.
import IDE.Web.FS (fsReadFileLazy, fsWriteFileLazy)

data FlagsFile = FlagsFile
  { configFlags     :: Maybe [Text]
  , buildFlags      :: Maybe [Text]
  , testFlags       :: Maybe [Text]
  , benchmarkFlags  :: Maybe [Text]
  , haddockFlags    :: Maybe [Text]
  , exeFlags        :: Maybe [Text]
  , installFlags    :: Maybe [Text]
  , registerFlags   :: Maybe [Text]
  , unregisterFlags :: Maybe [Text]
  , sdistFlags      :: Maybe [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON FlagsFile
instance FromJSON FlagsFile

setFlags :: IDEPackage -> FlagsFile -> IDEPackage
setFlags p FlagsFile{..} = p
  { ipdConfigFlags     = fromMaybe [] configFlags
  , ipdBuildFlags      = fromMaybe [] buildFlags
  , ipdTestFlags       = fromMaybe [] testFlags
  , ipdBenchmarkFlags  = fromMaybe [] benchmarkFlags
  , ipdHaddockFlags    = fromMaybe [] haddockFlags
  , ipdExeFlags        = fromMaybe [] exeFlags
  , ipdInstallFlags    = fromMaybe [] installFlags
  , ipdRegisterFlags   = fromMaybe [] registerFlags
  , ipdUnregisterFlags = fromMaybe [] unregisterFlags
  , ipdSdistFlags      = fromMaybe [] sdistFlags
  }

getFlagsFile :: IDEPackage -> FlagsFile
getFlagsFile IDEPackage{..} = FlagsFile
  { configFlags     = Just ipdConfigFlags
  , buildFlags      = Just ipdBuildFlags
  , testFlags       = Just ipdTestFlags
  , benchmarkFlags  = Just ipdBenchmarkFlags
  , haddockFlags    = Just ipdHaddockFlags
  , exeFlags        = Just ipdExeFlags
  , installFlags    = Just ipdInstallFlags
  , registerFlags   = Just ipdRegisterFlags
  , unregisterFlags = Just ipdUnregisterFlags
  , sdistFlags      = Just ipdSdistFlags
  }

-- | Read all the field values from the given 'FilePath'
readFlags :: FilePath -> IDEPackage -> IO IDEPackage
readFlags file pkg = E.catch
    (readFlagsFromBytes' (show file) pkg =<< fsReadFileLazy file)
    (\ (e::IOException) -> do
        liftIO . errorM "leksah" $ "Error reading file " ++ show file ++ " " ++ show e
        return pkg)

-- | Apply flag-file contents that are already in memory (remote projects
-- fetch every .lkshf in the project-open snapshot).
readFlagsFromBytes :: LBS.ByteString -> IDEPackage -> IO IDEPackage
readFlagsFromBytes bytes pkg = readFlagsFromBytes' "<snapshot>" pkg bytes

readFlagsFromBytes' :: String -> IDEPackage -> LBS.ByteString -> IO IDEPackage
readFlagsFromBytes' name pkg bytes = case eitherDecode bytes of
    Left e -> do
        liftIO . errorM "leksah" $ "Error reading file " ++ name ++ " " ++ show e
        return pkg
    Right f -> return $ setFlags pkg f

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

-- | Write all field values to the given 'FilePath'
writeFlags :: FilePath -> IDEPackage -> IO ()
writeFlags file = fsWriteFileLazy file . encodePretty . getFlagsFile

