{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.CabalPlan
-- Copyright   :  2016-2016 Herbert Valerio Riedel
-- License     :  GPL
--
-- | Parse plan.json files
--
-----------------------------------------------------------------------------

module IDE.Utils.CabalPlan (
    PlanJson(..)
  , PlanItem(..)
  , PID
  , unitIdToPackageId
) where

import GHC.Generics (Generic)
import Data.List (sortOn)
import qualified Data.Set as S (fromList, Set)
import qualified Data.Map as M (Map, toList)
import Data.Text (Text)
import qualified Data.Text as T
       (breakOnEnd, init, all, null, Text, pack, unpack, splitOn)
import Data.Aeson (FromJSON(..), withObject, (.:))
import Distribution.Package (PackageIdentifier, UnitId)
import Distribution.Text (display, simpleParse)

-- $setup
-- >>> import Data.Aeson (eitherDecodeStrict')
-- >>> import qualified Data.ByteString.Char8 as BS (pack)

-- | Information cabal-install writes to the plan.json file
-- >>> :{
--  eitherDecodeStrict' (BS.pack
--    "{\
--    \   \"cabal-lib-version\" : \"1.24.0.0\",\
--    \   \"cabal-version\" : \"1.24.0.0\",\
--    \   \"install-plan\" : [\
--    \      {\
--    \         \"type\" : \"pre-existing\",\
--    \         \"components\" : {\
--    \            \"lib\" : {\
--    \               \"depends\" : [\
--    \                  \"array-0.5.1.1\",\
--    \                  \"base-4.9.0.0\"\
--    \               ]\
--    \            }\
--    \         },\
--    \         \"id\" : \"Cabal-1.24.0.0\"\
--    \      },\
--    \      {\
--    \         \"id\" : \"QuickCheck-2.9.1-ec9a1c39266d75ed2c3314f6e846a8f11853eff43fc45db79c7256d9bfd94602\",\
--    \         \"components\" : {\
--    \            \"lib\" : {\
--    \               \"depends\" : [\
--    \                  \"base-4.9.0.0\",\
--    \                  \"containers-0.5.7.1\",\
--    \                  \"random-1.1-fe6ccf72ebd63a2d68570bb45b42bd08df5570c6151cb9af54907d40ef9af454\"\
--    \               ]\
--    \            }\
--    \         },\
--    \         \"flags\" : {\
--    \            \"templatehaskell\" : true\
--    \         },\
--    \         \"type\" : \"configured\"\
--    \      }\
--    \   ]\
--    \}") :: Either String PlanJson
-- :}
-- Right (PlanJson {pjPlan = [PlanItem {piId = "Cabal-1.24.0.0", piType = "pre-existing", piComps = [(ComponentLib,fromList ["array-0.5.1.1","base-4.9.0.0"])]},PlanItem {piId = "QuickCheck-2.9.1-ec9a1c39266d75ed2c3314f6e846a8f11853eff43fc45db79c7256d9bfd94602", piType = "configured", piComps = [(ComponentLib,fromList ["base-4.9.0.0","containers-0.5.7.1","random-1.1-fe6ccf72ebd63a2d68570bb45b42bd08df5570c6151cb9af54907d40ef9af454"])]}]})
data PlanJson = PlanJson
     { pjPlan :: [PlanItem]
     } deriving Show

instance FromJSON PlanJson where
    parseJSON = withObject "PlanJson" $ \o ->
      PlanJson <$> o .: "install-plan"

type PID = Text

data PlanItem = PlanItem
     { piId :: !PID
     , piType :: !Text
     , piComps :: [(Component, S.Set PID)]
     -- flags
     } deriving Show

instance FromJSON PlanItem where
    parseJSON = withObject "PlanItem" $ \o ->
      PlanItem <$> o .: "id"
               <*> o .: "type"
               <*> (doComps <$> o .: "components")
      where
        doComps :: M.Map Text CompInfo -> [(Component, S.Set PID)]
        doComps m = sortOn fst [ (toComp k, S.fromList v) | (k,CompInfo v) <- M.toList m ]

data CompInfo = CompInfo [PID]

instance FromJSON CompInfo where
    parseJSON = withObject "CompInfo" $ \o -> CompInfo <$> o .: "depends"

-- | Component of a package
--
-- NB: a similiar type exists in cabal's codebase
data Component =
    ComponentLib
  | ComponentSubLib !Text
  | ComponentExe    !Text
  | ComponentTest   !Text
  | ComponentBench  !Text
  | ComponentSetup
  deriving (Show, Eq, Ord, Generic)

toComp :: Text -> Component
toComp t0 = case T.splitOn ":" t0 of
              ["lib"]     -> ComponentLib
              ["lib",n]   -> ComponentSubLib n
              ["exe",n]   -> ComponentExe n
              ["bench",n] -> ComponentBench n
              ["test",n]  -> ComponentTest n
              ["setup"]   -> ComponentSetup
              _           -> error "IDE.Utils.CabalPlan.toComp"

-- |
-- >>> display <$> (unitIdToPackageId =<< simpleParse "base-4.9.0.0")
-- Just "base-4.9.0.0"
-- >>> display <$> (unitIdToPackageId =<< simpleParse "QuickCheck-2.9.1-ec9a1c39266d75ed2c3314f6e846a8f11853eff43fc45db79c7256d9bfd94602")
-- Just "QuickCheck-2.9.1"
unitIdToPackageId :: UnitId -> Maybe PackageIdentifier
unitIdToPackageId = (>>= simpleParse . T.unpack) . takePackageId . T.pack . display

takePackageId :: T.Text -> Maybe T.Text
takePackageId t
  | T.null pfx = Nothing
  | T.all (`elem` ("0123456789." :: String)) sfx = Just t -- assume hash-less
  | T.all (`elem` ("0123456789abcdef" :: String)) sfx = Just (T.init pfx)
  | otherwise = Nothing
  where
    (pfx, sfx) = T.breakOnEnd "-" t
