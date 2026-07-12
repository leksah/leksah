{-# LANGUAGE OverloadedStrings #-}
-- | Browser stub of leksah-server's IDE.Utils.VersionUtils (GHC JavaScript
-- backend only; picked over @vendor/leksah-server/src@ by hs-source-dirs
-- order).  The real module shells out to ghc/haddock and bakes in
-- @VERSION_ghc@ — a macro cabal only defines when depending on the `ghc`
-- package, which the JS build does not.  The web demo has no compiler; the
-- constants below are placeholders for display.
module IDE.Utils.VersionUtils (
    getHaddockVersion
,   getDefaultGhcVersion
,   getGhcInfo
,   supportedGhcVersions
,   ghcExeName
) where

import Data.Text (Text)

demoGhcVersion :: FilePath
demoGhcVersion = "9.14.1"

supportedGhcVersions :: [FilePath]
supportedGhcVersions = [demoGhcVersion]

getDefaultGhcVersion :: IO FilePath
getDefaultGhcVersion = return demoGhcVersion

getGhcInfo :: Maybe FilePath -> IO Text
getGhcInfo _ = return ""

getHaddockVersion :: IO Text
getHaddockVersion = return ""

ghcExeName :: Maybe FilePath -> FilePath
ghcExeName Nothing = "ghc"
ghcExeName (Just ver) = "ghc-" <> ver
