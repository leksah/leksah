-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Runs build-family verbs for the workspace's projects: resolve the
-- command through the project-type registry (plus the workspace file's
-- overrides and command prefix), stream its output to the build log, fold
-- the same stream through the problems parser, and record the outcome.
--
-- One verb runs at a time (per service); a second request while one runs
-- is refused with a note — interrupt first ('interruptBuild').  Colour is
-- forced only where a flag can do it without touching the build plan
-- (cargo); cabal's plan must never be perturbed by log cosmetics (a
-- changed @--ghc-options@ reconfigures the world).
--
-- ghci mode (the @build.ghci@ setting): a cabal build is first attempted
-- through @ffcabal@ (the fail-fast wrapper over cached repls); if ffcabal
-- cannot see the project's components (\"matches no local component\" \/
-- \"no local components selected\") or is not installed, the identical
-- build re-runs with plain cabal.
module IDE.Builder
  ( Builder(..)
  , BuilderState(..)
  , newBuilder
  , verbCommand
  , runVerb
  , runVerbWait
  , buildActiveTarget
  , interruptBuild
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
       (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.Exit (ExitCode(..))
import System.FilePath ((</>))

import IDE.BuildLog (BuildLog, blNote, blWrite)
import IDE.Config
       (BuildC(..), Config(..), ConfigService, currentConfig)
import IDE.Problems (Problems, setProblems)
import IDE.Problems.Parse (newParseSt, parseEnd, parseLine)
import IDE.Reactive (Cell, newCell, readCell, writeCell)
import IDE.Run
       (RunHandle, RunSpec(..), interruptRun, runSpec, startRun)
import IDE.Workspace
       (WorkspaceService(..), Ws, activeComponent, activePackage,
        activeProject, prDir, wsCmdPrefix, wsProjectFor, wsSpecFor)
import IDE.Ws.File (applyOverrides)
import IDE.Ws.Registry (typeById)
import IDE.Ws.Types
       (Component, Package, Project(..), ProjectKey(..), ProjectType(..),
        Scope(..), ToolCmd(..), Verb(..), verbId)

data BuilderState = BuilderIdle | BuilderRunning ProjectKey Verb
    deriving (Eq, Show)

data Builder = Builder
    { bLog      :: BuildLog
    , bProblems :: Problems
    , bConfig   :: ConfigService
    , bWs       :: WorkspaceService
    , bState    :: Cell BuilderState
    , bCurrent  :: IORef (Maybe RunHandle)
    }

newBuilder :: BuildLog -> Problems -> ConfigService -> WorkspaceService -> IO Builder
newBuilder lg pr cfg ws =
    Builder lg pr cfg ws <$> newCell BuilderIdle <*> newIORef Nothing

-- | The fully resolved command for a verb: registry command for the
-- scope, workspace-file override, command prefix, absolute directory.
verbCommand
    :: Ws -> Project -> Maybe Package -> Maybe Component -> Verb
    -> Maybe ToolCmd
verbCommand ws pr mbPkg mbComp verb = do
    pt <- typeById (pkType (prKey pr))
    let scope = case (mbPkg, mbComp) of
            (Just p, Just c)  -> ScopeComponent p c
            (Just p, Nothing) -> ScopePackage p
            _                 -> ScopeProject
        base = ptCommand pt verb scope
        spec = wsSpecFor (prKey pr) ws
    tc <- maybe base (\s -> applyOverrides s verb base) spec
    let prefixed = case T.words =<< maybe [] pure (wsCmdPrefix (prKey pr) ws) of
            []     -> tc
            (p:ps) -> tc { tcProgram = p
                         , tcArgs = ps <> (tcProgram tc : tcArgs tc) }
    return prefixed { tcDir = prDir pr </> tcDir prefixed }

-- | Run a verb against a target, asynchronously.
runVerb
    :: Builder -> ProjectKey -> Maybe Package -> Maybe Component -> Verb
    -> IO ()
runVerb b key mbPkg mbComp verb =
    void (startJob b key mbPkg mbComp verb (\_ -> return ()))

-- | Run a verb and wait for it; 'Nothing' when it could not start (busy,
-- no command, spawn failure — a note explains which).
runVerbWait
    :: Builder -> ProjectKey -> Maybe Package -> Maybe Component -> Verb
    -> IO (Maybe ExitCode)
runVerbWait b key mbPkg mbComp verb = do
    done <- newEmptyMVar
    started <- startJob b key mbPkg mbComp verb (putMVar done)
    if started then Just <$> takeMVar done else return Nothing

-- | Build whatever is active, with the configured extra steps' verbs
-- handled by the caller (tests/docs toggles are the caller's loop).
buildActiveTarget :: Builder -> IO ()
buildActiveTarget b = do
    ws <- readCell (wsCell (bWs b))
    case activeProject ws of
        Nothing -> blNote (bLog b) "build: nothing active"
        Just pr -> runVerb b (prKey pr)
                       (activePackage ws) (activeComponent ws) VBuild

interruptBuild :: Builder -> IO ()
interruptBuild b = readIORef (bCurrent b) >>= mapM_ interruptRun

-- Internals ------------------------------------------------------------

startJob
    :: Builder -> ProjectKey -> Maybe Package -> Maybe Component -> Verb
    -> (ExitCode -> IO ()) -> IO Bool
startJob b key mbPkg mbComp verb onExit = do
    st <- readCell (bState b)
    case st of
        BuilderRunning k v -> do
            blNote (bLog b) $ "busy: " <> verbId v <> " of "
                <> T.pack (pkRoot k) <> " still running"
            return False
        BuilderIdle -> do
            ws <- readCell (wsCell (bWs b))
            cfg <- currentConfig (bConfig b)
            case wsProjectFor key ws >>= \pr ->
                    verbCommand ws pr mbPkg mbComp verb of
                Nothing -> do
                    blNote (bLog b) $
                        "no " <> verbId verb <> " command for "
                        <> T.pack (pkRoot key)
                    return False
                Just tc -> do
                    let ghciMode = bcGhci (cfgBuild cfg)
                        useFF = ghciMode && tcProgram tc == "cabal"
                                         && verb == VBuild
                    launch b key verb tc useFF onExit

-- | Run the (possibly ffcabal-substituted) attempt; on an ffcabal
-- rejection, re-run the identical plain command.  Returns False when
-- nothing could be spawned at all.
launch :: Builder -> ProjectKey -> Verb -> ToolCmd -> Bool
       -> (ExitCode -> IO ()) -> IO Bool
launch b key verb tc useFF onExit = do
    writeCell (bState b) (BuilderRunning key verb)
    first <- spawn b key verb (colour attempt1)
    case first of
        Nothing
            | useFF -> do
                blNote (bLog b) "ffcabal unavailable — using plain cabal"
                plain (ExitFailure 127)
            | otherwise -> do
                writeCell (bState b) BuilderIdle
                return False
        Just wait -> do
            void . forkIO $ do
                (out, code) <- wait
                case code of
                    ExitFailure _ | useFF && ffReject out -> do
                        blNote (bLog b)
                            "ffcabal can't see this project — retrying with plain cabal"
                        void (plain code)
                    _ -> finish code
            return True
  where
    attempt1 = if useFF then tc { tcProgram = "ffcabal" } else tc
    plain failCode = do
        second <- spawn b key verb (colour tc)
        case second of
            Nothing -> do finish failCode; return False
            Just wait -> do
                void . forkIO $ do
                    (_, code) <- wait
                    finish code
                return True
    finish code = do
        blNote (bLog b) $ verbId verb <> ": " <> T.pack (show code)
        writeIORef (bCurrent b) Nothing
        writeCell (bState b) BuilderIdle
        onExit code
    ffReject out =
        "matches no local component" `T.isInfixOf` out
            || "no local components selected" `T.isInfixOf` out
    colour t
        | tcProgram t == "cargo" = t { tcArgs = "--color=always" : tcArgs t }
        | otherwise = t

-- | Spawn one command: raw bytes to the log, lines through the problems
-- parser (replacing this root's build problems on exit), a bounded tail
-- collected for the caller.  'Nothing' = could not spawn (noted).
spawn :: Builder -> ProjectKey -> Verb -> ToolCmd
      -> IO (Maybe (IO (Text, ExitCode)))
spawn b key verb tc = do
    blNote (bLog b) $ verbId verb <> ": " <> tcProgram tc <> " "
        <> T.unwords (tcArgs tc) <> "  (in " <> T.pack (tcDir tc) <> ")"
    st <- newIORef (BS.empty, newParseSt, [])   -- leftover, parser, problems
    tl <- newIORef T.empty                      -- bounded output tail
    done <- newEmptyMVar
    let onChunk chunk = do
            blWrite (bLog b) chunk
            atomicModifyIORef' tl $ \t ->
                (T.takeEnd 16384 (t <> toText chunk), ())
            atomicModifyIORef' st $ \(leftover, ps, acc) ->
                let (ls, rest) = splitLines (leftover <> chunk)
                    step (p, a) l = let (p', new) = parseLine p (toText l)
                                    in (p', a <> new)
                    (ps', acc') = foldl step (ps, acc) ls
                in ((rest, ps', acc'), ())
        onDone code = do
            (leftover, ps, acc) <- readIORef st
            let (ps', lastP)
                    | BS.null leftover = (ps, [])
                    | otherwise = parseLine ps (toText leftover)
                probs = acc <> lastP <> parseEnd ps'
            setProblems (bProblems b)
                ("build:" <> T.pack (pkRoot key)) probs
            out <- readIORef tl
            putMVar done (out, code)
    r <- startRun (runSpec (T.unpack (tcProgram tc)) (map T.unpack (tcArgs tc)))
        { rsDir = Just (tcDir tc)
        , rsOnChunk = onChunk
        , rsOnDone = onDone
        }
    case r of
        Left e -> do
            blNote (bLog b) ("spawn failed: " <> T.pack (show e))
            return Nothing
        Right h -> do
            writeIORef (bCurrent b) (Just h)
            return (Just (takeMVar done))

splitLines :: ByteString -> ([ByteString], ByteString)
splitLines bs = case BC.elemIndex '\n' bs of
    Nothing -> ([], bs)
    Just _  -> let ls = BC.split '\n' bs
               in (init ls, last ls)

toText :: ByteString -> Text
toText = TE.decodeUtf8With TE.lenientDecode
