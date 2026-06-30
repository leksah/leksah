{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Widget.FileTree
  ( filesAndDirs
  , joinPaths
  , fileTree
  , GitStatus(..)
  , gitClass
  , gitBadge
  ) where

import Control.Exception (catch, SomeException)
import Control.Monad.Extra (partitionM)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Bool (bool)
import Data.Char (isSpace)
import Data.List (isPrefixOf, tails, dropWhileEnd)
import Data.Map (Map, mapKeys)
import qualified Data.Map as M (toList, fromList, lookup)
import Data.Set (Set)
import qualified Data.Set as S (member, fromList)
import Data.Text (Text)
import qualified Data.Text as T (pack)

import System.Directory
       (doesDirectoryExist, getDirectoryContents)
import System.Exit (ExitCode(..))
import System.FilePath (takeExtension, (</>))
import System.Process (readProcessWithExitCode)

import Reflex
       (Dynamic, listViewWithKey, Event, never, ffilter, updated, leftmost,
        tag, current, getPostBuild, performEvent, holdDyn)
import Reflex.Dom.Core
       (MonadWidget, elAttr, elDynAttr, (=:), text, el, elDynClass, dynText,
        domEvent, EventName(..))

import IDE.Web.Events (FileEvents, FileEvent(..))
import IDE.Web.Widget.Tree
       (treeItem', treeItemDynAttr', treeSelect, treeSelect', scrollIntoViewNearest)

filesAndDirs :: MonadIO m => FilePath -> m ([FilePath], [FilePath])
filesAndDirs dir = liftIO $
  filter (`notElem` [".", ".."]) <$> getDirectoryContents dir >>=
    partitionM (doesDirectoryExist . (dir </>))

joinPaths :: Map FilePath (Map FilePath a) -> Map FilePath a
joinPaths m = mconcat [mapKeys (dir </>) m' | (dir, m') <- M.toList m]

-- | Git status of a file, ordered by ascending severity so that `maximum`
-- gives the most significant change in a directory (used to colour folders).
data GitStatus = Untracked | Added | Renamed | Modified | Deleted | Conflict
  deriving (Eq, Ord, Show)

-- | CSS class for a status, mirroring VS Code's git decoration colours
-- (modified gold, added/untracked green, deleted/conflict red).
gitClass :: GitStatus -> Text
gitClass Untracked = "git-untracked"
gitClass Added     = "git-added"
gitClass Renamed   = "git-renamed"
gitClass Modified  = "git-modified"
gitClass Deleted   = "git-deleted"
gitClass Conflict  = "git-conflict"

-- | The single-letter badge VS Code shows to the right of a changed file.
gitBadge :: GitStatus -> Text
gitBadge Untracked = "U"
gitBadge Added     = "A"
gitBadge Renamed   = "R"
gitBadge Modified  = "M"
gitBadge Deleted   = "D"
gitBadge Conflict  = "C"

-- | Class for a (file or folder) name span: the base `git-name` plus the
-- status colour when there is one.
gitNameClass :: Maybe GitStatus -> Text
gitNameClass = ("git-name" <>) . maybe "" ((" " <>) . gitClass)

-- | Classify a porcelain XY status code.
classifyStatus :: String -> Maybe GitStatus
classifyStatus = \case
  ('?':_)   -> Just Untracked
  code@(x:y:_)
    | x == 'U' || y == 'U' || code == "AA" || code == "DD" -> Just Conflict
    | x == 'R' || y == 'R'                                 -> Just Renamed
    | x == 'A'                                             -> Just Added
    | x == 'D' || y == 'D'                                 -> Just Deleted
    | x == 'M' || x == 'T' || y == 'M' || y == 'T'         -> Just Modified
  _ -> Nothing

-- | Run git for the repo containing `dir` and return (status map, ignored set),
-- both keyed/holding absolute paths.  Empty if `dir` is not in a git repo (or
-- git is unavailable) — decoration/filtering is then simply absent.
gitInfo :: MonadIO m => FilePath -> m (Map FilePath GitStatus, Set FilePath)
gitInfo dir = liftIO $ (`catch` \(_ :: SomeException) -> return (mempty, mempty)) $ do
  (rootRC, root, _) <- readProcessWithExitCode "git" ["-C", dir, "rev-parse", "--show-toplevel"] ""
  case rootRC of
    ExitSuccess -> do
      let root' = dropWhileEnd isSpace root
      -- `-uall` lists untracked files individually (so each is decorated).
      (rc, out, _) <- readProcessWithExitCode "git"
        ["-c", "core.quotePath=false", "-C", dir, "status", "--porcelain", "-uall"] ""
      -- A second pass with `--ignored` (default `-u`, which collapses fully
      -- ignored directories like dist-newstyle into one entry rather than
      -- listing every file).
      (_, iout, _) <- readProcessWithExitCode "git"
        ["-c", "core.quotePath=false", "-C", dir, "status", "--porcelain", "--ignored"] ""
      return ( if rc == ExitSuccess then parseStatus root' out else mempty
             , parseIgnored root' iout )
    _ -> return (mempty, mempty)
  where
    parseStatus root out = M.fromList
      [ (root </> afterArrow path, st)
      | line <- lines out
      , length line >= 4
      , let (code, rest) = splitAt 2 line
      , Just st <- [classifyStatus code]
      , let path = drop 1 rest  -- the separating space after the XY code
      ]
    parseIgnored root out = S.fromList
      [ root </> dropWhileEnd (== '/') (drop 3 line)
      | line <- lines out
      , length line >= 4
      , take 2 line == "!!"
      ]
    -- "old -> new" (renames/copies): decorate the new path.
    afterArrow p = case [ s | s <- tails p, " -> " `isPrefixOf` s ] of
      (s:_) -> drop 4 s
      []    -> p

-- | The most significant status among files under a directory (for colouring
-- the folder itself, as VS Code does).
dirStatus :: FilePath -> Map FilePath GitStatus -> Maybe GitStatus
dirStatus d m =
  case [ s | (p, s) <- M.toList m, (d <> "/") `isPrefixOf` p ] of
    [] -> Nothing
    ss -> Just (maximum ss)

fileTree
  :: MonadWidget t m
  => Text
  -> Set FilePath
  -> Set FilePath     -- ^ directories to omit (e.g. other packages' dirs)
  -> Dynamic t Bool   -- ^ show hidden (dot-) files
  -> Dynamic t Bool   -- ^ show git-ignored files
  -> Dynamic t (Maybe FilePath)  -- ^ the focused file (highlighted)
  -> Dynamic t (Maybe FilePath)  -- ^ the file to reveal (expand/scroll to)
  -> FilePath
  -> m (Event t FileEvents)
fileTree treeName srcDirs ignoreDirs showHiddenD showIgnoredD highlightD revealD dir = do
  -- Compute git status/ignored once for this (top-level) directory; thread down.
  postBuild <- getPostBuild
  infoD <- holdDyn (mempty, mempty) =<< performEvent (gitInfo dir <$ postBuild)
  fileTree' treeName srcDirs ignoreDirs showHiddenD showIgnoredD highlightD revealD infoD dir

fileTree'
  :: MonadWidget t m
  => Text
  -> Set FilePath
  -> Set FilePath
  -> Dynamic t Bool
  -> Dynamic t Bool
  -> Dynamic t (Maybe FilePath)
  -> Dynamic t (Maybe FilePath)
  -> Dynamic t (Map FilePath GitStatus, Set FilePath)
  -> FilePath
  -> m (Event t FileEvents)
fileTree' treeName srcDirs ignoreDirs showHiddenD showIgnoredD highlightD revealD infoD dir = do
  postBuild <- getPostBuild
  newListE <- performEvent $ filesAndDirs dir <$ postBuild
  allD <- holdDyn ([], []) newListE
  let statusD  = fst <$> infoD
      ignoredD = snd <$> infoD
      -- A reveal target is only reachable in this tree if it isn't inside (or
      -- equal to) one of the omitted directories — otherwise expanding towards
      -- it would just open a dead-end path that never shows the file.
      reachable f = not (any (\d -> d == f || (d <> "/") `isPrefixOf` f) ignoreDirs)
      -- A name is shown unless it is a dot-file (and hidden are off) or
      -- git-ignored (and ignored are off).
      visible showH showI ign name =
        (showH || not ("." `isPrefixOf` name)) &&
        (showI || not ((dir </> name) `S.member` ign))
      keepD = visible <$> showHiddenD <*> showIgnoredD <*> ignoredD
      -- Hide directories that are themselves another package's directory (passed
      -- in via ignoreDirs) so a package/project tree doesn't nest its siblings.
      subdirsD = (\keep (dirs, _) -> M.fromList
                    (map (,()) (filter (\s -> keep s && not ((dir </> s) `S.member` ignoreDirs)) dirs)))
                   <$> keepD <*> allD
      filesD   = (\keep (_, fs)  -> M.fromList (map (,()) (filter keep fs)))   <$> keepD <*> allD
  subdirE <- listViewWithKey subdirsD $ \subdir _ -> do
    let subPath = dir </> subdir
        isSrcDir = subPath `S.member` srcDirs
        imgSrc = "/pics/ide_" <> (if isSrcDir then "source_" else "") <> "folder.png"
        aggD = dirStatus subPath <$> statusD
        -- Auto-expand this directory when the reveal target is somewhere inside
        -- it *and* actually reachable here (not behind an omitted dir).  Highlight
        -- + scroll the directory itself when it *is* the highlight/reveal target
        -- (a find match can be a directory).
        underD = maybe False (\f -> reachable f && (subPath <> "/") `isPrefixOf` f) <$> revealD
        isRevealD = (== Just subPath) <$> revealD
        dirClassD = ("class" =:) . ("dir" <>) . bool "" " active" . (== Just subPath) <$> highlightD
    treeItemDynAttr' underD dirClassD False (do
      (dirEl, _) <- treeSelect' treeName (return never) $ do
        elAttr "img" ("src" =: imgSrc) $ return ()
        elDynClass "span" (gitNameClass <$> aggD) . text $ T.pack subdir
        return never
      pbD <- getPostBuild
      scrollIntoViewNearest (ffilter id $ leftmost [updated isRevealD, tag (current isRevealD) pbD]) dirEl
      return never
      ) $ el "ul" $ fileTree' treeName srcDirs ignoreDirs showHiddenD showIgnoredD highlightD revealD infoD subPath
  fileE <- listViewWithKey filesD $ \file _ -> do
    let imgSrc = "/pics/" <> case takeExtension file of
                    ".cabal" -> "ide_cabal_file.png"
                    ".hs" -> "ide_source.png"
                    ".lhs" -> "ide_source.png"
                    _ -> "ide_cabal_file.png"
        absPath = dir </> file
        fileStatusD = M.lookup absPath <$> statusD
        -- Highlight this file when it is the focused (active) editor tab.  The
        -- data-reveal-key lets the "already visible?" check find occurrences.
        liAttrsD = (\mf -> "class" =: ("file" <> bool "" " active" (mf == Just absPath))
                        <> "data-reveal-key" =: T.pack absPath) <$> highlightD
    elDynAttr "li" liAttrsD $ do
      (elFile, _) <- treeSelect' treeName (return never) $ do
        elAttr "img" ("src" =: imgSrc) $ return ()
        elDynClass "span" (gitNameClass <$> fileStatusD) . text $ T.pack file
        -- VS Code-style status letter to the right of the name.
        elDynClass "span" (("git-badge" <>) . maybe "" ((" " <>) . gitClass) <$> fileStatusD) $
          dynText $ maybe "" gitBadge <$> fileStatusD
        return never
      -- Scroll this file into view when it is the one to reveal.
      pbF <- getPostBuild
      let revealMeD = (== Just absPath) <$> revealD
      scrollIntoViewNearest (ffilter id $ leftmost [updated revealMeD, tag (current revealMeD) pbF]) elFile
      return $ OpenFile False absPath <$ domEvent Dblclick elFile
  return $ (joinPaths <$> subdirE) <> fileE
