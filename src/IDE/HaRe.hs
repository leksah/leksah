{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
#ifdef MIN_VERSION_HaRe
{-# LANGUAGE LambdaCase          #-}
#endif
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -Wall #-}
module IDE.HaRe (
   ifToCase
 , duplicateDef
 , liftToTopLevel
 , liftOneLevel
 , demote
 , rename
 , addOneParameter
 , rmOneParameter
 , deleteDef
) where

import Prelude hiding(getChar, getLine)

#ifdef MIN_VERSION_HaRe
import Control.Monad.IO.Class (MonadIO(..))
import Data.Map (Map)
import qualified Data.Text as T (unpack)
import System.Log.Logger (errorM)

import Language.Haskell.Refact.HaRe
       (compRename, compDeleteDef, compRmOneParameter,
        compAddOneParameter, compLiftOneLevel, compLiftToTopLevel,
        compDuplicateDef, compIfToCase, compDemote, VerboseLevel(..))
import Language.Haskell.Refact.API (ApplyRefacResult, RefactGhc)
import Language.Haskell.Refact.Utils.Utils
       (writeRefactoredFiles)

import Graphics.UI.Frame.ViewFrame (getMainWindow)

import IDE.Core.Types
       (ProjectTool, nixEnv, IDEPackage(..), Project(..), IDEAction)
import IDE.Core.State (readIDE)
import IDE.Utils.GUIUtils (showInputDialog)
import IDE.TextEditor.Class (TextEditor(..))
import IDE.Pane.SourceBuffer
       (belongsToPackages, fileName, inActiveBufContext)
#else
import IDE.Core.Types (IDEAction)
import IDE.Core.State (MessageLevel(..), ideMessage)
#endif

#ifdef MIN_VERSION_HaRe
runHaRe :: (FilePath -> (Int, Int) -> (Int, Int) -> RefactGhc [ApplyRefacResult]) -> IDEAction
runHaRe f =
    inActiveBufContext () $ \_ ebuf currentBuffer ->
        case fileName currentBuffer of
            Nothing -> return ()
            Just fn ->
                belongsToPackages fn >>= \case
                    [] -> return ()
                    (project, package) : _ -> do
                        (iStart, iEnd) <- getSelectionBounds ebuf
                        lStart <- (+1) <$> getLine iStart
                        cStart <- getLineOffset iStart
                        lEnd   <- (+1) <$> getLine iEnd
                        cEnd   <- getLineOffset iEnd
                        let _projectType :: ProjectTool = pjTool project  -- CabalTool (cabal new-build) or StackTool
                            projectFile :: FilePath = pjFile project     -- full path of cabal.project file (or stack.yaml)
                            _cabalFile :: FilePath = ipdCabalFile package -- full path of .cabal file
                        -- Leksah caches the nix-shell environment variables so that it does
                        -- not have to keep running nix-shell.
                        -- This environment should be passed to external processes.
                        _env :: Maybe (Map String String) <- readIDE (nixEnv projectFile "ghc")
                        -- TODO figure out how to write this function using the variables above
                        let lekashRefactGhc :: RefactGhc [ApplyRefacResult] -> IO [ApplyRefacResult]
                            lekashRefactGhc _ = do
                                errorM "leksah" "HaRe support not finished yet"
                                return []
                        refactoredMods <- liftIO $ lekashRefactGhc $ f fn (lStart, cStart) (lEnd, cEnd)
                        let verbosity = Normal
                        liftIO $ writeRefactoredFiles verbosity refactoredMods
                        return ()
#endif

ifToCase, duplicateDef, liftToTopLevel, liftOneLevel, demote, rename, addOneParameter, rmOneParameter, deleteDef :: IDEAction
#ifdef MIN_VERSION_HaRe
ifToCase = runHaRe compIfToCase
duplicateDef = do
    mainWindow <- getMainWindow
    runHaRe (\fn beginPos _endPos ->
        liftIO (showInputDialog (Just mainWindow) "New name" "") >>= \case
            Just s -> compDuplicateDef fn (T.unpack s) beginPos
            Nothing -> return [])
liftToTopLevel = runHaRe (\fn beginPos _endPos -> compLiftToTopLevel fn beginPos)
liftOneLevel = runHaRe (\fn beginPos _endPos -> compLiftOneLevel fn beginPos)
demote = runHaRe (\fn beginPos _endPos -> compDemote fn beginPos)
rename = do
    mainWindow <- getMainWindow
    runHaRe (\fn beginPos _endPos ->
        liftIO (showInputDialog (Just mainWindow) "Rename to" "") >>= \case
            Just s -> compRename fn (T.unpack s) beginPos
            Nothing -> return [])
addOneParameter = do
    mainWindow <- getMainWindow
    runHaRe (\fn beginPos _endPos ->
        liftIO (showInputDialog (Just mainWindow) "Parameter name" "") >>= \case
            Just s -> compAddOneParameter fn (T.unpack s) beginPos
            Nothing -> return [])
rmOneParameter = runHaRe (\fn beginPos _endPos -> compRmOneParameter fn beginPos)
deleteDef = runHaRe (\fn beginPos _endPos -> compDeleteDef fn beginPos)
#else
ifToCase        = ideMessage Normal "HaRe support not enabled"
duplicateDef    = ideMessage Normal "HaRe support not enabled"
liftToTopLevel  = ideMessage Normal "HaRe support not enabled"
liftOneLevel    = ideMessage Normal "HaRe support not enabled"
demote          = ideMessage Normal "HaRe support not enabled"
rename          = ideMessage Normal "HaRe support not enabled"
addOneParameter = ideMessage Normal "HaRe support not enabled"
rmOneParameter  = ideMessage Normal "HaRe support not enabled"
deleteDef       = ideMessage Normal "HaRe support not enabled"
#endif
