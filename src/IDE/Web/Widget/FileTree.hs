{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module IDE.Web.Widget.FileTree
  ( filesAndDirs
  , joinPaths
  , fileTree
  ) where

import Control.Monad.Extra (partitionM)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Map (Map, mapKeys)
import qualified Data.Map as M (toList, fromList)
import Data.Set (Set)
import qualified Data.Set as S (member)
import Data.Text (Text)
import qualified Data.Text as T (pack)

import System.Directory
       (doesDirectoryExist, getDirectoryContents)
import System.FilePath (takeExtension, (</>))

import Reflex
       (listViewWithKey, Event, never,
        getPostBuild, performEvent, holdDyn)
import Reflex.Dom.Core
       (MonadWidget, elAttr, (=:), text, el, elClass,
        domEvent, EventName(..))

import IDE.Web.Events (FileEvents, FileEvent(..))
import IDE.Web.Widget.Tree (treeItem, treeSelect, treeSelect')

filesAndDirs :: MonadIO m => FilePath -> m ([FilePath], [FilePath])
filesAndDirs dir = liftIO $
  filter (`notElem` [".", ".."]) <$> getDirectoryContents dir >>=
    partitionM (doesDirectoryExist . (dir </>))

joinPaths :: Map FilePath (Map FilePath a) -> Map FilePath a
joinPaths m = mconcat [mapKeys (dir </>) m' | (dir, m') <- M.toList m]

fileTree
  :: MonadWidget t m
  => Text
  -> Set FilePath
  -> FilePath
  -> m (Event t FileEvents)
fileTree treeName srcDirs dir = do
  postBuild <- getPostBuild
  newListE <- performEvent $ filesAndDirs dir <$ postBuild -- leftmost [tag (current dirD) postBuild, updated dirD]
  allD <- holdDyn ([], []) newListE
  let subdirsD = M.fromList . map (,()) . fst <$> allD
      filesD = M.fromList . map (,()) . snd <$> allD
  subdirE <- listViewWithKey subdirsD $ \subdir _ -> do
    let isSrcDir = (dir </> subdir) `S.member` srcDirs
        imgSrc = "/pics/ide_" <> (if isSrcDir then "source_" else "") <> "folder.png"
    treeItem "dir" False (
      treeSelect treeName (return never) $ do
        elAttr "img" ("src" =: imgSrc) $ return ()
        text $ T.pack subdir
        return never
      ) $ el "ul" $ fileTree treeName srcDirs $ dir </> subdir
  fileE <- listViewWithKey filesD $ \file _ -> do
    let imgSrc = "/pics/" <> case takeExtension file of
                    ".cabal" -> "ide_cabal_file.png"
                    ".hs" -> "ide_source.png"
                    ".lhs" -> "ide_source.png"
                    _ -> "ide_cabal_file.png"
    elClass "li" "file" $ do
      (elFile, _) <- treeSelect' treeName (return never) $ do
        elAttr "img" ("src" =: imgSrc) $ return ()
        text $ T.pack file
        return never
      return $ OpenFile False (dir </> file) <$ domEvent Dblclick elFile
  return $ (joinPaths <$> subdirE) <> fileE
