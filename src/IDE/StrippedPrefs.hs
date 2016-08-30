{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.StrippedPrefs
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.StrippedPrefs (

    Prefs(..)
,   RetrieveStrategy(..)
,   readStrippedPrefs
,   writeStrippedPrefs
,   getSourceDirectories
,   getUnpackDirectory
) where

import Text.PrinterParser
import Graphics.UI.Editor.Parameters
    (emptyParams, Parameter(..), (<<<-), paraName)
import qualified Text.PrettyPrint as  PP (text)
import System.FilePath
       (joinPath, (</>), dropTrailingPathSeparator, splitPath)
import System.Directory (getHomeDirectory)
import Control.Monad (liftM)
import IDE.Core.CTypes (RetrieveStrategy(..), configDirName)
import Data.Text (Text)

--
-- | Preferences is a data structure to hold configuration data
--
data Prefs = Prefs {
        sourceDirectories   ::   [FilePath]
    ,   unpackDirectory     ::   Maybe FilePath
    ,   retrieveURL         ::   Text
    ,   retrieveStrategy    ::   RetrieveStrategy
    ,   serverPort          ::   Int
    ,   endWithLastConn     ::   Bool
} deriving(Eq,Show)

defaultPrefs :: Prefs
defaultPrefs = Prefs {
        sourceDirectories   =   []
    ,   unpackDirectory     =   Just ("~" </> configDirName </> "packageSources")
    ,   retrieveURL         =   "http://www.leksah.org/"
    ,   retrieveStrategy    =   RetrieveThenBuild
    ,   serverPort          =   11111
    ,   endWithLastConn     =   True
    }

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readStrippedPrefs :: FilePath -> IO Prefs
readStrippedPrefs fn = readFields fn prefsDescription defaultPrefs

writeStrippedPrefs :: FilePath -> Prefs -> IO ()
writeStrippedPrefs fpath prefs = writeFields fpath prefs prefsDescription


prefsDescription :: [FieldDescriptionS Prefs]
prefsDescription = [
        mkFieldS
            (paraName <<<- ParaName
                "Paths under which haskell sources for packages may be found" $ emptyParams)
            (PP.text . show)
            readParser
            sourceDirectories
            (\b a -> a{sourceDirectories = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Unpack source for cabal packages to" $ emptyParams)
            (PP.text . show)
            readParser
            unpackDirectory
            (\b a -> a{unpackDirectory = b})
    ,   mkFieldS
            (paraName <<<- ParaName "URL from which to download prebuilt metadata" $ emptyParams)
            (PP.text . show)
            stringParser
            retrieveURL
            (\b a -> a{retrieveURL = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Strategy for downloading prebuilt metadata" $ emptyParams)
            (PP.text . show)
            readParser
            retrieveStrategy
            (\b a -> a{retrieveStrategy = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Port number for leksah to comunicate with leksah-server" $ emptyParams)
            (PP.text . show)
            intParser
            serverPort
            (\b a -> a{serverPort = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Stop the leksah-server process when leksah disconnects" $ emptyParams)
            (PP.text . show)
            boolParser
            endWithLastConn
            (\b a -> a{endWithLastConn = b})
    ]

-- ------------------------------------------------------------
-- * Cross platform support for "~" at the start of paths
-- ------------------------------------------------------------

-- | Expand the users home folder into paths such as "~/x"
expandHomePath :: FilePath -> IO FilePath
expandHomePath p = case splitPath p of
    h : rest | dropTrailingPathSeparator h == "~" ->  do
        home <- getHomeDirectory
        return $ home </> joinPath rest
    _ -> return p

getSourceDirectories :: Prefs -> IO [FilePath]
getSourceDirectories = mapM expandHomePath . sourceDirectories

getUnpackDirectory :: Prefs -> IO (Maybe FilePath)
getUnpackDirectory = maybe (return Nothing) (liftM Just . expandHomePath) . unpackDirectory
