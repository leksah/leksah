-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | The build log pane: a read-only xterm.js fed raw bytes from the
-- 'IDE.BuildLog' service.  There is no line model — compilers run with
-- colour forced and the terminal renders whatever they printed.  The
-- find bar searches it through the same SearchAddon registry as the
-- shell terminals, and file\/line tokens are clickable via the shared
-- link handler baked into the xterm bundle.
module IDE.Web.Widget.Log
  ( logCss
  , logWidget
  ) where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.IORef
       (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef,
        writeIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.IO.Unsafe (unsafePerformIO)

import Clay (height, pct, (?), Css)
import Control.Lens ((^.))
import Language.Javascript.JSaddle
       (JSM, JSVal, js, js1, js2, jsg, jss, liftJSM, new, pToJSVal,
        valToNumber)
import Reflex
       (Event, attachPromptlyDyn, ffor, holdDyn, never, newTriggerEvent)
import Reflex.Dom.Core
       ((=:), elAttr', getPostBuild, _element_raw)

import IDE.App (App, appBuildLog)
import IDE.BuildLog (blAttach)
import IDE.Web.Ctx (Ctx(..))
import IDE.Web.Events (FindbarEvents, LogEvents)
import IDE.Web.Model (WindowId)
import IDE.Web.Widget.ResizeObserver (resizeObserverWithAttrs)
import IDE.Web.Frame (MonadWidget, performEvent, performEvent_)

logCss :: Css
logCss =
    ".log-term" ? height (pct 100)

-- | One BuildLog subscription per window: a pane rebuild (tab closed and
-- reopened, layout change) must replace its predecessor's feed, or the
-- terminal would receive every chunk twice.
{-# NOINLINE logFeeds #-}
logFeeds :: IORef (Map WindowId (IO ()))
logFeeds = unsafePerformIO (newIORef M.empty)

logWidget
  :: forall t m . MonadWidget t m
  => Ctx t
  -> Event t FindbarEvents  -- ^ find bar reaches us via the search registry
  -> Event t Bool           -- ^ old list-move keys; a terminal scrolls itself
  -> Event t ()
  -> m (Event t LogEvents)
logWidget ctx _findE _moveE _activateE = do
    let app = cApp ctx
        wid = cWindowId ctx
    (resizeE, el) <- resizeObserverWithAttrs
        ("class" =: "log-term" <> "data-pane" =: "log") $
        fst <$> elAttr' "div" ("class" =: "terminal") (pure ())
    let rawEl = pToJSVal (_element_raw el)

    -- Live chunks arrive on a service thread; a trigger event carries the
    -- decoded text into this window's frame without blocking the writer.
    (chunkE, fireChunk) <- newTriggerEvent
    -- Subscribe BEFORE building the terminal, and never behind anything
    -- that can throw: this pane is usually built in a hidden tab, and the
    -- setup below can fail there.  Output that arrives before the terminal
    -- exists (the history replay always does) waits in 'pendingRef'.
    pendingRef <- liftIO $ newIORef []
    liftIO $ do
        decodeRef <- newIORef (TE.streamDecodeUtf8With TE.lenientDecode)
        let deliver bs = do
                decode <- readIORef decodeRef
                let TE.Some t _ cont = decode bs
                writeIORef decodeRef cont
                fireChunk t
        (history, detach) <- blAttach (appBuildLog app) deliver
        -- Replace any previous pane's feed for this window.
        old <- atomicModifyIORef' logFeeds $ \m ->
            (M.insert wid detach m, M.lookup wid m)
        sequence_ old
        deliver history
    postBuild <- getPostBuild
    termE <- performEvent $ ffor postBuild $ \_ -> liftJSM $ do
        term <- new (jsg ("Terminal" :: Text)) ()
        opts <- term ^. js ("options" :: Text)
        win <- jsg ("window" :: Text)
        monoFam <- win ^. js ("__leksahMonoFamily" :: Text)
        _ <- opts ^. jss ("fontFamily" :: Text) monoFam
        -- Size via LeksahTerm (0 = the global pref), which folds in the window's
        -- page zoom; the .xterm subtree is counter-zoomed (see 'terminalCss').
        _ <- jsg ("LeksahTerm" :: Text) ^. js2 ("setFontSize" :: Text) term (0 :: Int)
        -- Read-only: no stdin, and tool output uses bare \n.
        _ <- opts ^. jss ("disableStdin" :: Text) True
        _ <- opts ^. jss ("convertEol" :: Text) True
        -- SearchAddon needs the proposed decorations API.
        _ <- opts ^. jss ("allowProposedApi" :: Text) True
        _ <- opts ^. jss ("scrollback" :: Text) (100000 :: Int)
        fit <- new (jsg ("FitAddon" :: Text) ^. js ("FitAddon" :: Text)) ()
        _ <- term ^. js1 ("loadAddon" :: Text) fit
        _ <- term ^. js1 ("open" :: Text) rawEl
        fitIfSized rawEl fit
        -- Find-bar search + clickable file tokens, same registry as the
        -- shell terminals.
        _ <- jsg ("LeksahCM" :: Text)
                ^. js2 ("loadTerminalSearch" :: Text) term rawEl
        return (term, fit)
    termD <- holdDyn Nothing (Just <$> termE)

    -- Flush whatever arrived while the terminal was still being built.
    performEvent_ $ ffor termE $ \(term, _) -> do
        buffered <- liftIO $ atomicModifyIORef' pendingRef (\ts -> ([], reverse ts))
        liftJSM $ mapM_ (void . (term ^.) . js1 ("write" :: Text)) buffered
    performEvent_ $ ffor (attachPromptlyDyn termD chunkE) $ \(mbTerm, t) ->
        case mbTerm of
            Just (term, _) -> liftJSM . void $ term ^. js1 ("write" :: Text) t
            Nothing        -> liftIO $ modifyIORef' pendingRef (t:)
    -- The pane is usually built hidden; the first real fit comes from the
    -- resize observer when its tab is shown.
    performEvent_ $ ffor (attachPromptlyDyn termD resizeE) $ \(mbTerm, _) ->
        case mbTerm of
            Just (_, fit) -> liftJSM $ fitIfSized rawEl fit
            Nothing       -> return ()
    return never

-- | 'FitAddon.fit' throws on a zero-sized element, which a pane in a
-- hidden tab always is.
fitIfSized :: JSVal -> JSVal -> JSM ()
fitIfSized el fit = do
    w <- valToNumber =<< el ^. js ("offsetWidth" :: Text)
    h <- valToNumber =<< el ^. js ("offsetHeight" :: Text)
    when (w > 0 && h > 0) . void $ fit ^. js1 ("fit" :: Text) ()
