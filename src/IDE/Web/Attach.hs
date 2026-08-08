{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- SPDX-License-Identifier: Apache-2.0

-- | Attaching the reflex network with a GUARDED frame loop.
--
-- reflex-dom's own @processAsyncEvents@ is
--
-- > void $ forkIO $ forever $ do { ers <- readChan events; …; fire … }
--
-- with no exception handler anywhere.  Every @performEvent@ action, every
-- widget built inside a @dyn@, and every trigger callback runs inside that
-- @fire@ — so ONE exception escaping ONE handler kills the thread, async
-- event processing stops forever, and the window is frozen: the process,
-- the jsaddle transport and the command socket all still answer, which is
-- why @leksah-cmd ping@ saying @ok@ proves nothing.  Two freezes have been
-- traced to exactly this (a @listDirectory@ on a path that was a file; a
-- @createProcess@ on a missing binary).
--
-- This module reimplements @mainWidgetWithCss@ over the same exported
-- pieces, differing only in that the loop body is wrapped: an exception
-- ABANDONS ONE FRAME and is reported, instead of ending the window.
--
-- The trade-off is deliberate.  Abandoning a frame mid-flight leaves that
-- frame's propagation incomplete, so a caught exception is not "handled" —
-- it is downgraded from fatal to survivable, and reported loudly precisely
-- because the surviving state may be inconsistent.  Widget code should
-- still keep frame-thread IO total (see @filesAndDirs@, or
-- @IDE.Ws.Types.defaultEffects@, for the house style); this is the net
-- under that discipline, not a substitute for it.
--
-- Exit requests and asynchronous exceptions are re-thrown untouched:
-- leksah restarts itself by throwing 'ExitCode' (rebuild-self, handoff),
-- and swallowing that would wedge the restart path.
--
-- It also SERIALIZES the windows against each other — see 'frameLock'.
module IDE.Web.Attach
  ( mainWidgetWithCssGuarded
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (readChan)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception
       (SomeException, SomeAsyncException, displayException, fromException,
        throwIO, try)
import Control.Monad (forM, forM_, forever, void)
import Control.Monad.IO.Class (MonadIO(..))

import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity (Identity(..))
import Data.IORef (newIORef, readIORef)
import Data.Maybe (catMaybes)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8)

import System.Exit (ExitCode)
import System.IO.Unsafe (unsafePerformIO)

import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Document (createDocumentFragment, getBodyUnchecked, getHeadUnchecked)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Node (getOwnerDocumentUnchecked, toNode)
import GHCJS.DOM.Types (JSM)
import qualified GHCJS.DOM.Types as DOM

import Reflex.Dom.Core
       (Widget, JSContextSingleton, HydrationDomBuilderEnv(..),
        FireCommand(..), EventTriggerRef(..), TriggerInvocation(..),
        EventChannel, DomTimeline, DomHost, attachImmediateWidget,
        never, replaceElementContents, runDomHost,
        runHydrationDomBuilderT, runPostBuildT, runWithJSContextSingleton,
        withJSContextSingleton)
import Reflex.Host.Class (newEventWithTriggerRef)

-- | Serializes everything that touches the Spider timeline, process-wide.
--
-- Every leksah window runs its OWN reflex network, but they all share ONE
-- Spider timeline (@SpiderTimeline Global@ — a top-level
-- @unsafePerformIO@ environment in "Reflex.Spider.Internal": one height
-- bag, one delayed-merge queue, one propagation depth).  Spider guards
-- event propagation with that timeline's own mutex, but NOT frame
-- construction: @runFrame@ — which is what @runHostFrame@, @hold@,
-- @holdDyn@, @buildDynamic@, @subscribeEvent@ and @sample@ all go through,
-- i.e. the whole of building a widget tree — carries reflex's own
--
-- > runFrame :: … --TODO: This function also needs to hold the mutex
--
-- So two windows BUILDING at once, or one building while another
-- propagates, interleave writes to that shared state and corrupt it.  The
-- damage surfaces as a Spider internal error on whichever window loses the
-- race — @merge: accumRef not yet initialized@, @heightBagRemove: Height N
-- not present in bag@, @causality loop detected@ — or, when the queue is
-- merely left inconsistent, as no error at all: that window's build never
-- returns, so its frame loop is never even forked and it hangs with a blank
-- or stale DOM while every other window runs on.
--
-- This bites exactly when several windows attach together, which is what a
-- multi-window session restore does (a window opened later, by hand, builds
-- while the others are idle and usually gets away with it).
--
-- Holding this around each build and each frame batch makes the timeline
-- single-threaded again.  It is the OUTER lock: reflex's own mutex is taken
-- inside propagation, never around a build, so there is no lock-order
-- inversion between the two.
--
-- The cost is that a frame-thread handler which blocks now stalls EVERY
-- window rather than its own, so the "keep frame-thread IO total and
-- non-blocking" rule above (and in
-- @docs\/development\/debugging-web-ui-freezes.md@) matters more, not less.
-- Frames are short and reflex already serialized propagation this way, so
-- the added contention is between builds and frames only.
{-# NOINLINE frameLock #-}
frameLock :: MVar ()
frameLock = unsafePerformIO (newMVar ())

withFrameLock :: IO a -> IO a
withFrameLock act = withMVar frameLock (const act)

-- | 'Reflex.Dom.Core.mainWidgetWithCss' with a frame loop that survives an
-- exception.  The callback is handed a human-readable description of any
-- exception that aborted a frame; it runs OFF the frame thread and must
-- itself be total.
mainWidgetWithCssGuarded
  :: (String -> IO ())     -- ^ report an aborted frame (must not throw)
  -> ByteString            -- ^ CSS for the @\<head\>@
  -> (forall x. Widget x ())
  -> JSM ()
mainWidgetWithCssGuarded onAbort css w = withJSContextSingleton $ \jsSing -> do
  doc <- currentDocumentUnchecked
  headElement <- getHeadUnchecked doc
  setInnerHTML headElement $ "<style>" <> T.unpack (decodeUtf8 css) <> "</style>"
  body <- getBodyUnchecked doc
  attachGuarded onAbort body jsSing w

-- | 'Reflex.Dom.Core.attachWidget'' with 'guardedAsyncEvents' in place of
-- reflex-dom's unguarded @processAsyncEvents@.  The body below mirrors
-- @attachWidget'@; keep it in step when the reflex-dom pin moves.
--
-- The BUILD is guarded too, and separately.  It is not a frame that can be
-- abandoned: 'attachImmediateWidget' constructs the entire widget tree and
-- fires its post-build frame, and 'guardedAsyncEvents' is forked only once
-- it returns — so an exception here means this window has no network and
-- never will.  Unguarded, that exception escaped to the RTS, which printed
-- a bare @Uncaught exception@ (in ghci mode, at the @\<interactive\>@
-- prompt) with no window number and no Log-pane note, while the window sat
-- there blank and the rest of leksah carried on.  Here it is reported like
-- an aborted frame, and this window alone is given up on.
attachGuarded
  :: DOM.IsElement e
  => (String -> IO ()) -> e -> JSContextSingleton x -> Widget x () -> JSM ()
attachGuarded onAbort rootElement jsSing w = do
  doc <- getOwnerDocumentUnchecked rootElement
  df <- createDocumentFragment doc
  -- Under 'frameLock': this call builds the whole widget tree AND fires its
  -- post-build frame, all on the shared Spider timeline.
  built <- liftIO . try . withFrameLock . attachImmediateWidget $ \hydrationMode events -> do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    unreadyChildren <- liftIO $ newIORef 0
    delayed <- liftIO $ newIORef $ pure ()
    let builderEnv = HydrationDomBuilderEnv
          { _hydrationDomBuilderEnv_document = DOM.toDocument doc
          , _hydrationDomBuilderEnv_parent = Left $ toNode df
          , _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
          , _hydrationDomBuilderEnv_commitAction = return ()
          , _hydrationDomBuilderEnv_switchover = never
          , _hydrationDomBuilderEnv_delayed = delayed
          , _hydrationDomBuilderEnv_hydrationMode = hydrationMode
          }
    a <- runWithJSContextSingleton
           (runPostBuildT (runHydrationDomBuilderT w builderEnv events) postBuild)
           jsSing
    return ((a, events), postBuildTriggerRef)
  case built of
    Left e
      | isPassThrough e -> liftIO (throwIO e)
      -- NB. no string gap: this module is compiled with CPP, which eats them.
      | otherwise       -> liftIO . reportSafely onAbort $
          "WINDOW BUILD FAILED (the reflex network never built; this window"
          <> " stays blank until leksah restarts): " <> displayException e
    Right ((_, events), fc) -> do
      replaceElementContents rootElement df
      liftIO $ guardedAsyncEvents onAbort events fc

-- | The frame loop.  One batch of triggers per iteration, exactly as
-- reflex-dom does it, except that the batch runs under 'try' — a frame that
-- throws is abandoned and reported, and the NEXT batch is still processed —
-- and under 'frameLock', so it cannot interleave with another window's
-- frame or build.
guardedAsyncEvents
  :: (String -> IO ()) -> EventChannel -> FireCommand DomTimeline DomHost -> IO ()
guardedAsyncEvents onAbort events (FireCommand fire) = void . forkIO . forever $ do
  ers <- readChan events
  r <- withFrameLock . try $ runDomHost $ do
    mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
      me <- readIORef er
      return $ fmap (\e -> e :=> Identity a) me
    _ <- fire (catMaybes mes) $ return ()
    -- The per-trigger completion callbacks (blocking triggerEventWithCallback
    -- waiters); skipped when the frame above threw, since it never completed.
    liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
  case r of
    Right () -> return ()
    Left e
      -- Never swallow a restart/exit request or an async exception: leksah
      -- exits by throwing, and a killed thread must stay killed.
      | isPassThrough e -> throwIO e
      | otherwise -> reportSafely onAbort
          ("FRAME ABORTED (exception on the frame thread): " <> displayException e)

-- | Report without ever throwing: the reporter runs on the very thread it
-- is reporting for, so a throwing reporter would put us right back where we
-- started.
reportSafely :: (String -> IO ()) -> String -> IO ()
reportSafely onAbort msg =
  either (\(_ :: SomeException) -> ()) id <$> try (onAbort msg)

-- | Exceptions that must never be caught here: leksah restarts and exits by
-- throwing 'ExitCode' (rebuild-self, handoff), and a killed thread must stay
-- killed.
isPassThrough :: SomeException -> Bool
isPassThrough e =
  case fromException e :: Maybe SomeAsyncException of
    Just _ -> True
    Nothing -> case fromException e :: Maybe ExitCode of
      Just _ -> True
      Nothing -> False
