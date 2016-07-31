{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Applicative
import Prelude hiding(getChar, getLine)
import IDE.Core.Types
       (logRefFullFilePath, Prefs(..), LogRef(..), LogRefType(..),
        wsAllPackages, ipdPackageDir, IDEM, IDEAction, IDE(..),
        IDEPackage(..), PackageAction)
import Control.Monad.Reader (asks, MonadReader(..))
import IDE.Core.State
       (postSyncIDE, catchIDE, MessageLevel(..), ideMessage,
        leksahSubDir, reflectIDE, modifyIDE_, readIDE)
import Control.Concurrent.STM.TVar
       (newTVarIO, writeTVar, readTVar)
import Control.Concurrent (forkIO)
import Control.Monad
       (void, when, foldM, forM_, forM, unless, forever)
import Control.Monad.STM (retry, atomically)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Language.Haskell.HLint3
       (Note(..), Idea(..), Severity(..), applyHints, ParseError(..),
        parseModuleEx, CppFlags(..), defaultParseFlags,
        parseFlagsAddFixities, resolveHints, readSettingsFile,
        findSettings, Hint(..), Classify(..), ParseFlags(..))
import System.FilePath.Windows (makeRelative, equalFilePath, (</>))
import System.Directory (doesFileExist)
import qualified System.IO.Strict as S (readFile)
import Language.Preprocessor.Cpphs
       (defaultCpphsOptions, runCpphsReturningSymTab, CpphsOptions(..))
import System.Log.Logger (debugM)
import Data.Monoid ((<>))
import Data.List (sortBy, intercalate, find)
import qualified Data.Map as M (keys, lookup)
import qualified Data.Text as T
       (replicate, init, unlines, reverse, take, drop, lines, unpack,
        pack)
import Control.Exception (SomeException(..))
import Data.Maybe (isJust, mapMaybe, catMaybes)
import Distribution.Package (PackageIdentifier(..))
import Distribution.ModuleName (ModuleName)
import IDE.Core.CTypes
       (SrcSpan(..), mdMbSourcePath, pdModules, mdModuleId, modu,
        PackScope(..), GenScope(..), GenScope, packageIdentifierToString)
import IDE.Metainfo.Provider (getWorkspaceInfo)
import qualified Language.Haskell.Exts.SrcLoc as HSE
       (SrcLoc(..), SrcSpan(..))
import IDE.Pane.SourceBuffer
       (useCandyFor, selectSourceBuf, fileSave, inActiveBufContext,
        addLogRef, belongsToPackage, removeLintLogRefs)
import qualified Data.Text.IO as T (readFile)
import Data.Text (Text)
import IDE.TextEditor (TextEditor(..))
import IDE.SourceCandy
       (getCandylessPart, positionToCandy, stringToCandy)
import IDE.BufferMode (IDEBuffer(..), editInsertCode)
import Data.Ord (comparing)
import qualified Data.Foldable as F (toList)
import IDE.Utils.CabalUtils (findProjectRoot)

packageHLint :: PackageAction
packageHLint = asks ipdCabalFile >>= (lift . lift . scheduleHLint . Left)

scheduleHLint :: Either FilePath FilePath -> IDEAction
scheduleHLint what = do
    liftIO $ debugM "leksah" "scheduleHLint"
    mbQueue <- readIDE hlintQueue
    queue <- case mbQueue of
                Nothing -> do
                    ideR <- ask
                    queue <- liftIO $ newTVarIO []
                    modifyIDE_ $ \ide -> ide { hlintQueue = Just queue }
                    liftIO . forkIO . forever $ do
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
    packages <- maybe [] wsAllPackages <$> readIDE workspace
    case sortBy (flip (comparing (length . ipdPackageDir))) $ filter (belongsToPackage sourceFile) packages of
        (package:_) -> runHLint' package (Just sourceFile)
        _ -> liftIO . debugM "leksah" $ "runHLint package not found for " <> sourceFile
runHLint (Left cabalFile) = do
    liftIO . debugM "leksah" $ "runHLint"
    packages <- maybe [] wsAllPackages <$> readIDE workspace
    case find ((== cabalFile) . ipdCabalFile) packages of
        Just package -> runHLint' package Nothing
        _ -> liftIO . debugM "leksah" $ "runHLint package not found for " <> cabalFile

runHLint' :: IDEPackage -> Maybe FilePath -> IDEAction
runHLint' package mbSourceFile = do
    liftIO . debugM "leksah" $ "runHLint'"
    ideR <- ask
    (flags, classify, hint) <- hlintSettings package
    let modules = M.keys (ipdModules package)
    paths <- case mbSourceFile of
                    Just f  -> return [f]
                    Nothing -> getSourcePaths (ipdPackageId package) modules
    res <- forM paths $ \ full -> do
        let file = makeRelative (ipdPackageDir package) full
        postSyncIDE $ removeLintLogRefs (ipdPackageDir package) file
        text <- liftIO $ T.readFile full
        liftIO . debugM "leksah" $ "runHLint parsing " <> full
        do result <- liftIO $ parseModuleEx flags full (Just (T.unpack text))
           case result of
                Left e -> logHLintError (isJust mbSourceFile) package e >> return Nothing
                Right r -> do
                    liftIO . debugM "leksah" $ "runHLint parsed " <> full
                    return $ Just (r, (full, text))
        `catchIDE` (\(e :: SomeException) -> do
            reflectIDE (ideMessage Normal . T.pack $ "HLint Exception : " <> show e) ideR
            return Nothing)
    liftIO $ debugM "leksah" "runHLint parse complete"
    let results = catMaybes res
        ideas = map fst results
        texts = map snd results
        getText f = maybe "" snd $ find (equalFilePath f . fst) texts
    logHLintResult (isJust mbSourceFile) package (applyHints classify hint ideas) getText


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
                    (mod : tl) ->  mdMbSourcePath mod
                    []         -> Nothing
            Nothing -> Nothing

hlintSettings :: IDEPackage -> IDEM (ParseFlags, [Classify], Hint)
hlintSettings package = do
    mbHlintDir <- liftIO $ leksahSubDir "hlint"
    projectRoot <- liftIO $ findProjectRoot (ipdPackageDir package)
    let cabalMacros = projectRoot </> "dist-newstyle/build"
                        </> T.unpack (packageIdentifierToString $ ipdPackageId package)
                        </> "build/autogen/cabal_macros.h"
    cabalMacrosExist <- liftIO $ doesFileExist cabalMacros
    defines <- liftIO $ if cabalMacrosExist
                            then do
                                raw <- S.readFile cabalMacros
                                map (\(a, b) -> (a, concat (lines b))) . snd <$> runCpphsReturningSymTab defaultCpphsOptions cabalMacros raw
                            else return []
    (fixities, classify, hints) <- liftIO $ findSettings (readSettingsFile mbHlintDir) Nothing
    let hint = resolveHints hints
        flags = parseFlagsAddFixities fixities defaultParseFlags{ cppFlags = Cpphs defaultCpphsOptions
            { defines = defines } }
    liftIO . debugM "leksah" $ "hlintSettings defines = " <> show defines
    return (flags, classify, hint)

logHLintResult :: Bool -> IDEPackage -> [Idea] -> (FilePath -> Text) -> IDEAction
logHLintResult fileScope package allIdeas getText = do
    let ideas = filter (\Idea{..} -> ideaSeverity /= Ignore) allIdeas
    forM_ ideas $ \ idea@Idea{..} -> do
        let text = getText (HSE.srcSpanFilename ideaSpan)
            fixColumn c = max 0 (c - 1)
            srcSpan = SrcSpan (makeRelative (ipdPackageDir package) $ HSE.srcSpanFilename ideaSpan)
                              (HSE.srcSpanStartLine ideaSpan)
                              (HSE.srcSpanStartColumn ideaSpan - 1)
                              (HSE.srcSpanEndLine ideaSpan)
                              (HSE.srcSpanEndColumn ideaSpan - 1)
            fromLines = drop (HSE.srcSpanStartLine ideaSpan - 1)
                      . take (HSE.srcSpanEndLine ideaSpan) $ T.lines text
            fixHead [] = []
            fixHead (x:xs) = T.drop (HSE.srcSpanStartColumn ideaSpan - 1) x : xs
            fixTail [] = []
            fixTail (x:xs) = T.take (HSE.srcSpanEndColumn ideaSpan - 1) x : xs
            from = T.reverse . T.drop 1 . T.reverse
                 . T.unlines . fixHead . reverse . fixTail $ reverse fromLines
            ref = LogRef srcSpan package (T.pack $ showHLint idea)
                    (Just (from, idea)) Nothing LintRef
        postSyncIDE $ addLogRef fileScope fileScope ref
    return ()

logHLintError :: Bool -> IDEPackage -> ParseError -> IDEAction
logHLintError fileScope package error = do
    let loc = parseErrorLocation error
        srcSpan = SrcSpan (makeRelative (ipdPackageDir package) $ HSE.srcFilename loc)
                          (HSE.srcLine loc)
                          (HSE.srcColumn loc - 1)
                          (HSE.srcLine loc)
                          (HSE.srcColumn loc - 1)
        ref = LogRef srcSpan package ("Hlint Parse Error: " <> T.pack (parseErrorMessage error)) Nothing Nothing LintRef
    postSyncIDE $ addLogRef fileScope fileScope ref

-- Cut down version of showEx from HLint
showHLint :: Idea -> String
showHLint Idea{..} = intercalate "\n" $
    [if ideaHint == "" then "" else show ideaSeverity ++ ": " ++ ideaHint] ++
    f "Found" (if ideaHint == "Reduce duplication" then Just ideaFrom else Nothing) ++
    f "Why not" ideaTo ++
    ["Note: " ++ n | let n = showNotes ideaNote, n /= ""]
    where
        f msg Nothing = []
        f msg (Just x) | null x = [msg ++ " remove it."]
                       | otherwise = (msg ++ ":") : map ("  "++) (lines x)
        showNotes :: [Note] -> String
        showNotes = intercalate ", " . map show . filter use
            where use ValidInstance{} = False -- Not important enough to tell an end user
                  use _ = True

resolveActiveHLint :: IDEM Bool
resolveActiveHLint = inActiveBufContext False  $ \_ _ ebuf ideBuf _ -> do
    liftIO $ debugM "leksah" "resolveActiveHLint"
    allLogRefs <- readIDE allLogRefs
    (iStart, iEnd) <- getSelectionBounds ebuf
    lStart <- getLine iStart
    cStart <- getLineOffset iStart
    lEnd <- getLine iEnd
    cEnd <- getLineOffset iEnd
    let fn = fileName ideBuf
    let selectedRefs = [ref | ref@LogRef{..} <- F.toList allLogRefs,
                            logRefType == LintRef
                         && fn == Just (logRefFullFilePath ref)
                         && maybe "" (ideaHint . snd) logRefIdea /= "Reduce duplication"
                         && (lStart+1, cStart) <= (srcSpanEndLine   logRefSrcSpan,
                                                   srcSpanEndColumn logRefSrcSpan)
                         && (lEnd+1, cEnd) >= (srcSpanStartLine logRefSrcSpan,
                                               srcSpanStartColumn logRefSrcSpan)]
        safeRefs = takeWhileNotOverlapping selectedRefs
    (changed, _) <- foldM replaceHLintSource (False, 0) . catMaybes $ map logRefIdea safeRefs
    prefs <- readIDE prefs
    when changed $ if backgroundBuild prefs
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
    T.init $ T.unlines (take 1 lines <> drop 1 (map (indent <>) lines))
  where
    lines = T.lines text
    indent = T.replicate startColumn " "

replaceHLintSource :: (Bool, Int) -> (Text, Idea) -> IDEM (Bool, Int)
replaceHLintSource (changed, delta) (from, Idea{ideaSpan = ideaSpan, ideaTo = Just ideaTo}) = do
    let HSE.SrcSpan{..} = ideaSpan
        to = indentHLintText (srcSpanStartColumn-1) (T.pack ideaTo)
    liftIO . debugM "leksah" $ "replaceHLintSource From: " <> show from <> "\nreplaceHLintSource To:   " <> show to
    mbBuf <- selectSourceBuf srcSpanFilename
    case mbBuf of
        Just buf -> inActiveBufContext (changed, delta) $ \_ sv ebuf _ _ -> do
            useCandy   <- useCandyFor buf
            candy'     <- readIDE candy
            realString <- if useCandy then stringToCandy candy' to else return to
            (lineS', columnS', lineE', columnE') <- if useCandy
                    then do
                        (_,e1) <- positionToCandy candy' ebuf (srcSpanStartLine + delta, srcSpanStartColumn - 1)
                        (_,e2) <- positionToCandy candy' ebuf (srcSpanEndLine + delta, srcSpanEndColumn - 1)
                        return (srcSpanStartLine-1 + delta,e1,srcSpanEndLine-1 + delta,e2)
                    else return (srcSpanStartLine-1 + delta,srcSpanStartColumn-1,srcSpanEndLine-1 + delta,srcSpanEndColumn-1)
            i1  <- getIterAtLine ebuf lineS'
            i1' <- forwardCharsC i1 columnS'
            i2  <- getIterAtLine ebuf lineE'
            i2' <- forwardCharsC i2 columnE'
            candy <- readIDE candy
            check <- getCandylessPart candy ebuf i1' i2'
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

