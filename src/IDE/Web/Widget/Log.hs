-- SPDX-License-Identifier: Apache-2.0
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

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.IORef
       (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.IO.Unsafe (unsafePerformIO)

import Clay (height, pct, (?), Css)
import Control.Lens ((^.))
import Language.Javascript.JSaddle
       (js, js1, js2, jsg, jss, liftJSM, new, pToJSVal)
import Reflex
       (Event, attachPromptlyDyn, ffor, holdDyn, never, newTriggerEvent,
        performEvent, performEvent_)
import Reflex.Dom.Core
       (MonadWidget, (=:), elAttr', getPostBuild, _element_raw)

import IDE.App (App, appBuildLog)
import IDE.BuildLog (blAttach)
import IDE.Web.Ctx (Ctx(..))
import IDE.Web.Events (FindbarEvents, LogEvents)
import IDE.Web.Model (WindowId)
import IDE.Web.Widget.ResizeObserver (resizeObserverWithAttrs)

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
    postBuild <- getPostBuild
    termE <- performEvent $ ffor postBuild $ \_ -> liftJSM $ do
        term <- new (jsg ("Terminal" :: Text)) ()
        opts <- term ^. js ("options" :: Text)
        win <- jsg ("window" :: Text)
        monoFam <- win ^. js ("__leksahMonoFamily" :: Text)
        monoSz  <- win ^. js ("__leksahMonoSize" :: Text)
        _ <- opts ^. jss ("fontFamily" :: Text) monoFam
        _ <- opts ^. jss ("fontSize" :: Text) monoSz
        -- Read-only: no stdin, and tool output uses bare \n.
        _ <- opts ^. jss ("disableStdin" :: Text) True
        _ <- opts ^. jss ("convertEol" :: Text) True
        -- SearchAddon needs the proposed decorations API.
        _ <- opts ^. jss ("allowProposedApi" :: Text) True
        _ <- opts ^. jss ("scrollback" :: Text) (100000 :: Int)
        fit <- new (jsg ("FitAddon" :: Text) ^. js ("FitAddon" :: Text)) ()
        _ <- term ^. js1 ("loadAddon" :: Text) fit
        _ <- term ^. js1 ("open" :: Text) rawEl
        _ <- fit ^. js1 ("fit" :: Text) ()
        -- Find-bar search + clickable file tokens, same registry as the
        -- shell terminals.
        _ <- jsg ("LeksahCM" :: Text)
                ^. js2 ("loadTerminalSearch" :: Text) term rawEl
        -- Attach to the service: replay history, then follow.  A stateful
        -- UTF-8 decoder spans chunk boundaries (a multi-byte glyph can
        -- split across two reads).
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
        return (term, fit)
    termD <- holdDyn Nothing (Just <$> termE)

    performEvent_ $ ffor (attachPromptlyDyn termD chunkE) $ \(mbTerm, t) ->
        case mbTerm of
            Just (term, _) -> liftJSM . void $ term ^. js1 ("write" :: Text) t
            Nothing        -> return ()
    performEvent_ $ ffor (attachPromptlyDyn termD resizeE) $ \(mbTerm, _) ->
        case mbTerm of
            Just (_, fit) -> liftJSM . void $ fit ^. js1 ("fit" :: Text) ()
            Nothing       -> return ()
    return never
