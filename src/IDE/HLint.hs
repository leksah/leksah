{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.HLint
-- Copyright   :  2007-2015 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.HLint (
    hlintSettings
  , scheduleHLint
  , packageHLint
  , resolveActiveHLint
) where

import Prelude ()
import Prelude.Compat hiding(getChar, getLine)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
       (newTVarIO, writeTVar, readTVar)
import Control.Exception (SomeException(..))
import Control.Lens ((?~), _Just)
import Control.Monad
       (void, when, foldM, forM_, forM, unless, forever)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (asks, MonadReader(..))
import Control.Monad.STM (retry, atomically)

import Data.List (intercalate, find)
import qualified Data.Map as M (keys, lookup)
import Data.Maybe (isJust, mapMaybe, catMaybes)
import qualified Data.Foldable as F (toList)
import Data.Text (Text)
import qualified Data.Text as T
       (replicate, init, unlines, reverse, take, drop, lines, unpack,
        pack)
import qualified Data.Text.IO as T (readFile)

import Distribution.Package (PackageIdentifier(..))
import Distribution.ModuleName (ModuleName)

#if MIN_VERSION_ghc(9,2,0)
#define GHC_LIB_PARSER "ghc-lib-parser"
#elif MIN_VERSION_ghc(9,0,0)
#define GHC_LIB_PARSER "ghc"
#else
#define GHC_LIB_PARSER "ghc-lib-parser"
#endif

import GHC_LIB_PARSER GHC.Data.FastString (unpackFS)
import qualified GHC_LIB_PARSER GHC.Types.SrcLoc as GHC
       (SrcLoc(..), SrcSpan(..), srcSpanFile, srcSpanStartLine, srcSpanStartCol,
        srcSpanEndLine, srcSpanEndCol)
import Language.Haskell.HLint
       (Note(..), Idea(..), Severity(..), applyHints, ParseError(..),
        parseModuleEx, CppFlags(..), defaultParseFlags,
        parseFlagsAddFixities, readSettingsFile,
        findSettings, Hint(..), Classify(..), ParseFlags(..))
import Language.Preprocessor.Cpphs
       (defaultCpphsOptions, runCpphsReturningSymTab, CpphsOptions(..))

import System.FilePath.Windows (makeRelative, equalFilePath, (</>))
import System.Directory (doesFileExist)
import qualified System.IO.Strict as S (readFile)
import System.Log.Logger (debugM, errorM)

import IDE.Core.CTypes
       (SrcSpan(..), mdMbSourcePath, pdModules, mdModuleId, modu,
        PackScope(..), GenScope(..), GenScope, packageIdentifierToString)
import IDE.Core.Types
       (pjDir, ProjectKey(..), Project(..), MonadIDE(..),
        logRefFullFilePath, Prefs(..), Log(..), LogRef(..), LogRefType(..),
        wsProjectAndPackages, ipdPackageDir, IDEM, IDEAction,
        IDEPackage(..), PackageAction, hlintQueue, workspace, allLogRefs, prefs, candy)
import IDE.Core.State
       (catchIDE, MessageLevel(..), ideMessage,
        leksahSubDir, reflectIDE, modifyIDE_, readIDE)
import IDE.BufferMode (IDEBuffer(..), editInsertCode)
import IDE.Gtk.SourceCandy
       (getCandylessPart, positionToCandy, stringToCandy)
import IDE.Gtk.State (postSyncIDE)
import IDE.Metainfo.Provider (getWorkspaceInfo)
import IDE.Pane.SourceBuffer
       (belongsToPackages, useCandyFor, selectSourceBuf, fileSave,
        inActiveBufContext, addLogRef, removeLintLogRefs)
import IDE.TextEditor (TextEditor(getSelectionBounds, getLine, getLineOffset, setModified, getIterAtLine, forwardCharsC, beginUserAction, delete, endUserAction))
import IDE.Utils.FileUtils (cabalProjectBuildDir)

packageHLint :: PackageAction
packageHLint = asks ipdCabalFile >>= (liftIDE . scheduleHLint . Left)

scheduleHLint :: Either FilePath FilePath -> IDEAction
scheduleHLint what = do
    liftIO $ debugM "leksah" "scheduleHLint"
    mbQueue <- readIDE hlintQueue
    queue <- case mbQueue of
                Nothing -> do
                    ideR <- ask
                    queue <- liftIO $ newTVarIO []
                    modifyIDE_ $ hlintQueue ?~ queue
                    _ <- liftIO . forkIO . forever $ do
                        x <- atomically $ do
                                schedule <- readTVar queue
                                case schedule of
                                    (x:xs) -> do
                                        writeTVar queue xs
                                        return x
                                    []     -> retry
                        reflectIDE (runHLint x) ideR
                    return queue
                Just queue -> return queue
    liftIO . atomically $ do
        scheduled <- readTVar queue
        unless (what `elem` scheduled) $
            writeTVar queue $ scheduled ++ [what]

runHLint :: Either FilePath FilePath -> IDEAction
runHLint (Right sourceFile) = do
    liftIO . debugM "leksah" $ "runHLint"
    belongsToPackages sourceFile >>= \case
        (package:_) -> runHLint' package (Just sourceFile)
        _ -> liftIO . debugM "leksah" $ "runHLint package not found for " <> sourceFile
runHLint (Left cabalFile) = do
    liftIO . debugM "leksah" $ "runHLint"
    packages <- readIDE (workspace . _Just . wsProjectAndPackages)
    case find ((== cabalFile) . ipdCabalFile . snd) packages of
        Just package -> runHLint' package Nothing
        _ -> liftIO . debugM "leksah" $ "runHLint package not found for " <> cabalFile

runHLint' :: (Project, IDEPackage) -> Maybe FilePath -> IDEAction
runHLint' (project, package) mbSourceFile = do
    liftIO . debugM "leksah" $ "runHLint'"
    (flags, classify, hint) <- hlintSettings project package
    let modules = M.keys (ipdModules package)
    paths <- case mbSourceFile of
                    Just f  -> return [f]
                    Nothing -> getSourcePaths (ipdPackageId package) modules
    res <- forM paths $ \ path -> do
        postSyncIDE $ removeLintLogRefs path
        text <- liftIO $ T.readFile path
        liftIO . debugM "leksah" $ "runHLint parsing " <> path
        do result <- liftIO $ parseModuleEx flags path (Just (T.unpack text))
           case result of
                Left e -> logHLintError (isJust mbSourceFile) package e >> return Nothing
                Right r -> do
                    liftIO . debugM "leksah" $ "runHLint parsed " <> path
                    return $ Just (r, (path, text))
        `catchIDE` (\(e :: SomeException) -> do
            ideMessage Normal . T.pack $ "HLint Exception : " <> show e
            return Nothing)
    liftIO $ debugM "leksah" "runHLint parse complete"
    let results = catMaybes res
        ideas = map fst results
        texts = map snd results
        getText' f = maybe "" snd $ find (equalFilePath f . fst) texts
    logHLintResult (isJust mbSourceFile) package (applyHints classify hint ideas) getText'


getSourcePaths :: PackageIdentifier -> [ModuleName] -> IDEM [FilePath]
getSourcePaths packId names = do
    mbWorkspaceInfo     <-  getWorkspaceInfo
    case mbWorkspaceInfo of
        Nothing -> return []
        Just (sc, _) -> return (mapMaybe (sourcePathFromScope sc) names)
  where
    sourcePathFromScope :: GenScope -> ModuleName -> Maybe FilePath
    sourcePathFromScope (GenScopeC (PackScope l _)) mn =
        case packId `M.lookup` l of
            Just pack ->
                case filter (\md -> modu (mdModuleId md) == mn)
                                    (pdModules pack) of
                    (mod' : _) ->  mdMbSourcePath mod'
                    []         -> Nothing
            Nothing -> Nothing

hlintSettings :: Project -> IDEPackage -> IDEM (ParseFlags, [Classify], Hint)
hlintSettings project package = do
    mbHlintDir <- liftIO $ leksahSubDir "hlint"
    cabalMacros <- case pjKey project of
        CabalTool {} -> do
            (buildDir, _, _) <- liftIO $ cabalProjectBuildDir (pjDir $ pjKey project) "dist-newstyle"
            return $ buildDir </> T.unpack (packageIdentifierToString $ ipdPackageId package)
                              </> "build/autogen/cabal_macros.h"
        StackTool {} -> return $ ipdPackageDir package </> ".stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/autogen/cabal_macros.h"
                        -- TODO run stack path --dist-dir
        _ -> return $ ipdPackageDir package </> ".hadrian_ghci/stage0/compiler/build/autogen/cabal_macros.h"
    cabalMacrosExist <- liftIO $ doesFileExist cabalMacros
    defines <- liftIO $ if cabalMacrosExist
                            then do
                                raw <- S.readFile cabalMacros
                                map (\(a, b) -> (a, concat (lines b))) . snd <$> runCpphsReturningSymTab defaultCpphsOptions cabalMacros raw
                            else return []
    (fixities, classify, hints) <- liftIO $ findSettings (readSettingsFile mbHlintDir) Nothing
    let flags = parseFlagsAddFixities fixities defaultParseFlags{ cppFlags = Cpphs defaultCpphsOptions
            { defines = defines } }
    liftIO . debugM "leksah" $ "hlintSettings defines = " <> show defines
    return (flags, classify, hints)

logHLintResult :: Bool -> IDEPackage -> [Idea] -> (FilePath -> Text) -> IDEAction
logHLintResult fileScope package allIdeas getText = do
    let ideas = filter (\Idea{ideaSeverity} -> ideaSeverity /= Ignore) allIdeas
    forM_ ideas $ \case
      idea@Idea{ideaSpan = GHC.RealSrcSpan ideaSpan _} -> do
        let text = getText (unpackFS $ GHC.srcSpanFile ideaSpan)
            -- fixColumn c = max 0 (c - 1)
            srcSpan = SrcSpan (makeRelative (ipdPackageDir package) . unpackFS $ GHC.srcSpanFile ideaSpan)
                              (GHC.srcSpanStartLine ideaSpan)
                              (GHC.srcSpanStartCol ideaSpan - 1)
                              (GHC.srcSpanEndLine ideaSpan)
                              (GHC.srcSpanEndCol ideaSpan - 1)
            fromLines = drop (GHC.srcSpanStartLine ideaSpan - 1)
                      . take (GHC.srcSpanEndLine ideaSpan) $ T.lines text
            fixHead [] = []
            fixHead (x:xs) = T.drop (GHC.srcSpanStartCol ideaSpan - 1) x : xs
            fixTail [] = []
            fixTail (x:xs) = T.take (GHC.srcSpanEndCol ideaSpan - 1) x : xs
            from = T.reverse . T.drop 1 . T.reverse
                 . T.unlines . fixHead . reverse . fixTail $ reverse fromLines
            ref = LogRef srcSpan (LogCabal $ ipdCabalFile package) (T.pack $ showHLint idea)
                    (Just (from, idea)) Nothing LintRef
        postSyncIDE $ addLogRef fileScope fileScope ref
      Idea{ideaSpan = GHC.UnhelpfulSpan s} ->
        liftIO . errorM "leksah" $ "hlint parse error without location " <> show s

logHLintError :: Bool -> IDEPackage -> ParseError -> IDEAction
logHLintError fileScope package err = do
    case parseErrorLocation err of
      GHC.RealSrcSpan loc _ -> do
        let srcSpan = SrcSpan (makeRelative (ipdPackageDir package) . unpackFS $ GHC.srcSpanFile loc)
                              (GHC.srcSpanStartLine loc)
                              (GHC.srcSpanStartCol loc - 1)
                              (GHC.srcSpanEndLine loc)
                              (GHC.srcSpanEndCol loc - 1)
            ref = LogRef srcSpan (LogCabal $ ipdCabalFile package) ("Hlint Parse Error: " <> T.pack (parseErrorMessage err)) Nothing Nothing LintRef
        postSyncIDE $ addLogRef fileScope fileScope ref
      GHC.UnhelpfulSpan s ->
        liftIO . errorM "leksah" $ "hlint parse error without location " <> show s

-- Cut down version of showEx from HLint
showHLint :: Idea -> String
showHLint Idea{ideaHint, ideaSeverity, ideaFrom, ideaTo, ideaNote} = intercalate "\n" $
    [if ideaHint == "" then "" else show ideaSeverity ++ ": " ++ ideaHint] ++
    f "Found" (if ideaHint == "Reduce duplication" then Just ideaFrom else Nothing) ++
    f "Why not" ideaTo ++
    ["Note: " ++ n | let n = showNotes ideaNote, n /= ""]
    where
        f _ Nothing = []
        f msg (Just x) | null x = [msg ++ " remove it."]
                       | otherwise = (msg ++ ":") : map ("  "++) (lines x)
        showNotes :: [Note] -> String
        showNotes = intercalate ", " . map show . filter use
            where use ValidInstance{} = False -- Not important enough to tell an end user
                  use _ = True

resolveActiveHLint :: IDEM Bool
resolveActiveHLint = inActiveBufContext False  $ \_ ebuf ideBuf -> do
    liftIO $ debugM "leksah" "resolveActiveHLint"
    allLogRefs' <- readIDE allLogRefs
    (iStart, iEnd) <- getSelectionBounds ebuf
    lStart <- getLine iStart
    cStart <- getLineOffset iStart
    lEnd <- getLine iEnd
    cEnd <- getLineOffset iEnd
    let fn = fileName ideBuf
    let selectedRefs = [ref | ref@LogRef{logRefType, logRefIdea, logRefSrcSpan} <- F.toList allLogRefs',
                            logRefType == LintRef
                         && fn == Just (logRefFullFilePath ref)
                         && maybe "" (ideaHint . snd) logRefIdea /= "Reduce duplication"
                         && (lStart+1, cStart) <= (srcSpanEndLine   logRefSrcSpan,
                                                   srcSpanEndColumn logRefSrcSpan)
                         && (lEnd+1, cEnd) >= (srcSpanStartLine logRefSrcSpan,
                                               srcSpanStartColumn logRefSrcSpan)]
        safeRefs = takeWhileNotOverlapping selectedRefs
    (changed, _) <- foldM replaceHLintSource (False, 0) . catMaybes $ map logRefIdea safeRefs
    prefs' <- readIDE prefs
    when changed $ if backgroundBuild prefs'
                        then setModified ebuf True
                        else void $ fileSave False
    return changed
  where
    takeWhileNotOverlapping = takeWhileNotOverlapping' (-1)
    takeWhileNotOverlapping' _ [] = []
    takeWhileNotOverlapping' line (ref:refs)
        | srcSpanEndLine (logRefSrcSpan ref) > line = ref : takeWhileNotOverlapping' (srcSpanEndLine $ logRefSrcSpan ref) refs
        | otherwise = takeWhileNotOverlapping' line refs

indentHLintText :: Int -> Text -> Text
indentHLintText startColumn text =
    T.init $ T.unlines (take 1 lines' <> drop 1 (map (indent <>) lines'))
  where
    lines' = T.lines text
    indent = T.replicate startColumn " "

replaceHLintSource :: (Bool, Int) -> (Text, Idea) -> IDEM (Bool, Int)
replaceHLintSource (changed, delta) (from, Idea{ideaSpan = GHC.RealSrcSpan ideaSpan _, ideaTo = Just ideaTo}) = do
    let srcSpanStartLine = GHC.srcSpanStartLine ideaSpan
        srcSpanStartCol  = GHC.srcSpanStartCol  ideaSpan
        srcSpanEndLine   = GHC.srcSpanEndLine   ideaSpan
        srcSpanEndCol    = GHC.srcSpanEndCol    ideaSpan
        to = indentHLintText (srcSpanStartCol-1) (T.pack ideaTo)
    liftIO . debugM "leksah" $ "replaceHLintSource From: " <> show from <> "\nreplaceHLintSource To:   " <> show to
    mbBuf <- selectSourceBuf . unpackFS $ GHC.srcSpanFile ideaSpan
    case mbBuf of
        Just buf -> inActiveBufContext (changed, delta) $ \_sv ebuf _ -> do
            useCandy   <- useCandyFor buf
            candy'     <- readIDE candy
            realString <- if useCandy then stringToCandy candy' to else return to
            (lineS', columnS', lineE', columnE') <- if useCandy
                    then do
                        (_,e1) <- positionToCandy candy' ebuf (srcSpanStartLine + delta, srcSpanStartCol - 1)
                        (_,e2) <- positionToCandy candy' ebuf (srcSpanEndLine + delta, srcSpanEndCol - 1)
                        return (srcSpanStartLine-1 + delta,e1,srcSpanEndLine-1 + delta,e2)
                    else return (srcSpanStartLine-1 + delta,srcSpanStartCol-1,srcSpanEndLine-1 + delta,srcSpanEndCol-1)
            i1  <- getIterAtLine ebuf lineS'
            i1' <- forwardCharsC i1 columnS'
            i2  <- getIterAtLine ebuf lineE'
            i2' <- forwardCharsC i2 columnE'
            check <- getCandylessPart candy' ebuf i1' i2'
            if check == from
                then do
                    beginUserAction ebuf
                    delete ebuf i1' i2'
                    editInsertCode ebuf i1' realString
                    endUserAction ebuf
                    return (True, delta + length (T.lines to) - length (T.lines from))
                else return (changed, delta)
        _ -> return (changed, delta)
replaceHLintSource x _ = return x

