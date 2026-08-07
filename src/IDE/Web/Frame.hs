{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- SPDX-License-Identifier: Apache-2.0

-- | Frame-thread safety: drop-in replacements for reflex's
-- 'Reflex.performEvent' \/ 'Reflex.performEvent_' that cannot take the
-- window down, plus the 'MonadWidget' synonym that makes them usable.
--
-- Widget code should import 'MonadWidget', 'performEvent' and
-- 'performEvent_' from HERE rather than from @Reflex@ \/
-- @Reflex.Dom.Core@.  The names are deliberately the same, so call sites
-- and type signatures read exactly as before.
--
-- == Why
--
-- performEvent handlers run on the reflex frame thread, inside the @fire@
-- of reflex-dom's event loop.  An exception escaping one of them used to
-- kill that loop and freeze the window ("IDE.Web.Attach" now catches it
-- there as a backstop).  Catching it HERE is better in a way that matters:
-- the frame still completes normally — reflex just sees a handler that did
-- nothing — instead of being abandoned half-propagated.  Attach's guard is
-- the net for everything these wrappers cannot cover (widget building
-- inside @dyn@, trigger callbacks, reflex's own internals).
--
-- A caught exception is a BUG, not a supported control-flow path: it is
-- reported to stderr (so it lands in the ghci pane and the launch log) and
-- to the Log pane, with the source location of the @performEvent@ call
-- that produced it.  'ExitCode' and asynchronous exceptions are re-thrown
-- untouched — leksah restarts itself by throwing 'ExitCode'.
--
-- The 'MonadWidget' here is reflex-dom's plus @MonadException
-- (Performable m)@, which reflex-dom's own synonym omits.  Both layers of
-- the concrete performable stack (@WithJSContextSingleton@ over
-- @SpiderHostFrame@) derive it, so nothing else has to change.
module IDE.Web.Frame
  ( MonadWidget
  , performEvent
  , performEvent_
  ) where

import qualified Control.Exception as E
       (SomeAsyncException, SomeException, displayException, fromException,
        try)
import Control.Monad.Exception (MonadException)
import qualified Control.Monad.Exception as X (catch, throw)
import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as T (pack)

import GHC.Stack
       (CallStack, HasCallStack, callStack, getCallStack, srcLocFile,
        srcLocStartLine, withFrozenCallStack)

import System.Exit (ExitCode)
import qualified System.IO as IO (hPutStrLn, stderr)

-- Only the associated type: importing PerformEvent(..) would drag
-- reflex's performEvent names in and make ours ambiguous.
import Reflex (Event, PerformEvent(Performable), Reflex, fmapMaybe)
import qualified Reflex as R (performEvent, performEvent_)
import qualified Reflex.Dom.Core as RD (MonadWidget)

import IDE.App (appNote, getGlobalApp)

-- | reflex-dom's 'RD.MonadWidget' plus the ability to catch in the
-- performable monad — the one constraint that makes the wrappers below
-- possible.  Shadowing the name keeps all 100+ existing signatures intact.
type MonadWidget t m = (RD.MonadWidget t m, MonadException (Performable m))

-- | What the wrappers actually need: reflex's own 'PerformEvent'
-- constraint plus the ability to catch and to report.  Deliberately NOT
-- 'MonadWidget' — several helpers (e.g. the ResizeObserver binding) are
-- polymorphic in ways that MonadWidget's @DomBuilderSpace m ~ GhcjsDomSpace@
-- would reject.
type Guarded t m =
    (PerformEvent t m, MonadIO (Performable m), MonadException (Performable m))

-- | 'Reflex.performEvent_' whose handler cannot kill the frame thread.
performEvent_ :: (HasCallStack, Guarded t m) => Event t (Performable m ()) -> m ()
performEvent_ e = withFrozenCallStack $ R.performEvent_ (guard_ callStack <$> e)

-- | 'Reflex.performEvent' whose handler cannot kill the frame thread.  An
-- occurrence whose action throws produces NO output occurrence — the event
-- simply does not fire that time, which is what a downstream 'holdDyn' or
-- 'switchHold' would have seen had the action never been scheduled.
performEvent :: (HasCallStack, Reflex t, Guarded t m) => Event t (Performable m a) -> m (Event t a)
performEvent e =
    withFrozenCallStack $ fmapMaybe id <$> R.performEvent (guardJust callStack <$> e)

guard_ :: (MonadException n, MonadIO n) => CallStack -> n () -> n ()
guard_ cs act = act `X.catch` \(ex :: E.SomeException) -> absorb cs ex

guardJust :: (MonadException n, MonadIO n) => CallStack -> n a -> n (Maybe a)
guardJust cs act =
    (Just <$> act) `X.catch` \(ex :: E.SomeException) -> Nothing <$ absorb cs ex

-- | Re-throw what must not be absorbed; report anything else.
absorb :: (MonadException n, MonadIO n) => CallStack -> E.SomeException -> n ()
absorb cs ex
  | isPassThrough ex = X.throw ex
  | otherwise        = liftIO (report cs ex)

-- | Exit requests and asynchronous exceptions are control flow, not faults:
-- leksah restarts by throwing 'ExitCode', and a killed thread must die.
isPassThrough :: E.SomeException -> Bool
isPassThrough ex =
    case E.fromException ex :: Maybe E.SomeAsyncException of
      Just _  -> True
      Nothing -> case E.fromException ex :: Maybe ExitCode of
        Just _  -> True
        Nothing -> False

-- | Report to stderr (ghci pane / launch log) and the Log pane.  Total: a
-- reporter that threw would land us back on the frame thread with an
-- exception, which is the whole thing we are preventing.
report :: CallStack -> E.SomeException -> IO ()
report cs ex = quietly $ do
    let msg = "handler at " <> site <> " threw: " <> E.displayException ex
    IO.hPutStrLn IO.stderr ("LEK HANDLER FAILED: " <> msg)
    -- Before boot there is no app to note into; stderr above still has it.
    quietly $ getGlobalApp >>= \case
        Nothing  -> return ()
        Just app -> appNote app ("A UI event handler failed — please report"
                                 <> " this. " <> T.pack msg)
  where
    site = case getCallStack cs of
        ((_, loc) : _) -> srcLocFile loc <> ":" <> show (srcLocStartLine loc)
        []             -> "<unknown>"
    quietly act = either (\(_ :: E.SomeException) -> ()) id <$> E.try act
