{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Gtk.Workspaces.Writer (
--    writeWorkspace
) where

import Prelude ()
import Prelude.Compat
import IDE.Core.Types
import IDE.Core.State
import IDE.Package
       (getModuleTemplate, getPackageDescriptionAndPath, activatePackage,
        deactivatePackage, ideProjectFromPath)
import IDE.Utils.FileUtils(myCanonicalizePath)

import Data.Maybe
import Data.Function ((&))
import Control.Monad (unless, join, void, when)
import Control.Monad.Trans (liftIO)
import Control.Lens ((^.), (.~))
import System.Time (getClockTime)
import System.FilePath
       (takeFileName, (</>), isAbsolute, dropFileName, makeRelative,
        dropExtension, takeBaseName, addExtension, takeExtension,
        takeDirectory)
import Graphics.UI.Editor.Parameters
    (Parameter(..), (<<<-), paraName, emptyParams)
import qualified Text.PrettyPrint as  PP (text)
import System.Log.Logger (debugM)
import qualified Data.Text as T (unpack, pack)
import System.FSNotify (watchDir, Event(..), watchTree, eventPath, isPollingManager)
import Control.Monad.Reader (MonadReader(..))
import Data.Traversable (forM)
import qualified Data.Map as Map (empty)
import Text.ParserCombinators.Parsec (parseFromFile, CharParser)
import Data.Text (Text)
import Data.Map (Map)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as LBS (readFile, writeFile)
import Data.Aeson
       (eitherDecode, encode, ToJSON(..), FromJSON(..))
import Data.Aeson.Types
       (Options, genericParseJSON, genericToEncoding, genericToJSON,
        defaultOptions, fieldLabelModifier)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (isPrefixOf, stripPrefix)
import Data.Char (toLower)
import IDE.Pane.SourceBuffer (setModifiedOnDisk)
import System.Directory (doesDirectoryExist)
import Control.Concurrent (putMVar, takeMVar, tryPutMVar)
import qualified Data.Set as S (fromList, insert, member)
import Control.Exception (evaluate)
import qualified Data.Map as M
       (partitionWithKey, partition, fromList, member)
import Data.Foldable (forM_)

--writeWorkspace :: Workspace -> IDEAction
--writeWorkspace ws = do
--    timeNow      <- liftIO getClockTime
--    let newWs    =  ws & wsSaveTime .~ T.pack (show timeNow)
--                       & wsVersion .~ workspaceVersion
--    setWorkspace $ Just newWs
--    newWs' <- liftIO $ makePathsRelative newWs (ws ^. wsFile)
--    liftIO . LBS.writeFile (ws ^. wsFile) $ encodePretty newWs'
