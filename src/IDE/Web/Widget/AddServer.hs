{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- reflex-dom deprecates 'textInput'; the find bar (and the rest of leksah)
-- still uses it deliberately, so match that here rather than churn.
{-# OPTIONS_GHC -Wno-deprecations #-}
-- | The @Add Server…@ modal.  A one-field form that adds an ssh host to the
-- 'rcHosts' setting — the list shown as top-level nodes in the
-- Terminals tree and offered as autocomplete in the Add Remote Project
-- dialog.  The host is whatever ssh accepts as a destination (an ssh-config
-- alias like @x86_64-linux-0@, or @user\@host@); Add first probes it with a
-- no-op ssh exec so a typo is caught immediately, but an unreachable host
-- can still be added deliberately (press Add again — e.g. a machine that is
-- currently offline).
--
-- The dialog owns its whole flow (validation + the add) on a background
-- thread via 'getGlobalApp', so it needs no feedback wiring from
-- "IDE.Web.Main" — it just returns an 'Event' that fires when it should
-- close (Cancel, or a successful add).  The setting mutation goes through
-- 'saveConfig', which persists it and updates the config cell, so every
-- window's Terminals tree picks the new host up.
-- Shown via the "IDE.Web.AddServerRequest" bridge; styling mirrors
-- 'IDE.Web.Widget.AddRemote'.
module IDE.Web.Widget.AddServer
  ( addServerDialog
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Lens ((.~))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
       (any, null, pack, strip, takeWhile, lines)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode(..))

import Reflex
       (constDyn, ffor, leftmost, current, performEvent_, holdDyn,
        fmapMaybe, tag, newTriggerEvent, updated)
import Reflex.Dom.Core
       (elAttr, elAttr', textInput, text, dynText, MonadWidget, (=:),
        Event, attributes, domEvent, EventName(..), _textInput_value)

import IDE.App (appConfig, getGlobalApp)
import IDE.Config
       (Config(..), RemoteC(..), currentConfig, saveConfig)
import IDE.Utils.RemoteExec (runSsh)

-- | Render the modal.  Returns an 'Event' that fires (once) when the caller
-- should tear the modal down: on Cancel, or after a server is successfully
-- added.
addServerDialog :: MonadWidget t m => m (Event t ())
addServerDialog = do
    -- Result of a background add attempt:
    -- Left (retriable, msg) = inline error (retriable = probe failure, so the
    -- next Add on the same host skips the probe); Right = added.
    (resultE, fireResult) <- newTriggerEvent
    elAttr "div" ("class" =: "add-server-overlay" <> "style" =: overlayStyle) $
      elAttr "div" ("class" =: "add-server-dialog" <> "style" =: dialogStyle) $ do
        elAttr "p" ("style" =: "font-weight:bold;margin:0 0 10px 0") $
            text "Add Server (over ssh)"
        hostTi <- textInput $ def & attributes .~ constDyn
            ("placeholder" =: "ssh host — e.g. user@host or an ssh-config alias"
             <> "style" =: fieldStyle)
        let failE = fmapMaybe (either Just (const Nothing)) resultE
        errD <- holdDyn "" $ leftmost
            [ snd <$> failE
            , "" <$ updated (_textInput_value hostTi) ]
        elAttr "p" ("style" =: "color:#c0392b;min-height:1.1em;margin:8px 0 4px 0;font-size:12px") $
            dynText errD
        -- After a failed reachability probe, the NEXT Add of the same text
        -- skips the probe ("press Add again to add it anyway"); editing the
        -- host re-arms it.
        skipProbeD <- holdDyn False $ leftmost
            [ fst <$> failE
            , False <$ updated (_textInput_value hostTi) ]
        (addEl, _)    <- elAttr' "button" ("style" =: primaryBtnStyle) $ text "Add"
        (cancelEl, _) <- elAttr' "button" ("style" =: btnStyle) $ text "Cancel"
        let valsD = (,) <$> _textInput_value hostTi <*> skipProbeD
        performEvent_ $ ffor (tag (current valsD) (domEvent Click addEl)) $
            \(h, skip) -> liftIO . void . forkIO $ addServerIO h skip fireResult
        return $ leftmost
            [ ()  <$ domEvent Click cancelEl
            , fmapMaybe (either (const Nothing) Just) resultE ]

-- | Do the actual work on a background thread: validate, probe, append to the
-- 'rcHosts' setting; report Left (retriable, error) / Right () through the
-- callback.  The ssh probe stays off the frame thread.
addServerIO :: Text -> Bool -> (Either (Bool, Text) () -> IO ()) -> IO ()
addServerIO host skipProbe fire =
    (`catch` \(e :: SomeException) -> fire (Left (False, T.pack (show e)))) $ do
      let h = T.strip host
      if T.null h
        then fire (Left (False, "Host is required."))
      else if T.any (== ' ') h
        then fire (Left (False, "Host must not contain spaces — use an ssh destination or config alias."))
      else getGlobalApp >>= \case
        Nothing  -> fire (Left (False, "IDE is not ready yet."))
        Just app -> do
          cfg <- currentConfig (appConfig app)
          if h `elem` rcHosts (cfgRemote cfg)
            then fire (Left (False, h <> " is already in the server list."))
            else do
              probed <- if skipProbe then return (Right ()) else probeHost h
              case probed of
                Left err -> fire (Left (True, err))
                Right () -> do
                  -- Re-read at write time so the probe window can't clobber
                  -- a concurrent settings change.
                  cfg' <- currentConfig (appConfig app)
                  saveConfig (appConfig app) cfg'
                      { cfgRemote = (cfgRemote cfg')
                          { rcHosts = rcHosts (cfgRemote cfg') <> [h] } }
                  fire (Right ())

-- | One no-op ssh exec to catch typos/config problems up front.  ssh exits
-- 255 for its own failures (DNS, route, auth, timeout); anything else means
-- we reached a shell, which is all we need.
probeHost :: Text -> IO (Either Text ())
probeHost h = runSsh h "true" [] "" >>= \case
    (ExitFailure 255, _, err) -> return . Left $
        "Cannot reach " <> h
        <> (let e = firstLine err in if T.null e then "" else ": " <> e)
        <> " — press Add again to add it anyway."
    _ -> return (Right ())
  where
    firstLine bs = case T.lines (T.strip (decodeUtf8With lenientDecode bs)) of
        (l:_) -> T.takeWhile (/= '\r') l
        []    -> ""

overlayStyle, dialogStyle, fieldStyle, btnStyle, primaryBtnStyle :: Text
overlayStyle =
    "position:fixed;inset:0;z-index:1000;display:flex;align-items:center;\
    \justify-content:center;background:var(--leksah-scrim)"
dialogStyle =
    "min-width:360px;padding:16px 20px;border-radius:8px;background:var(--leksah-surface);\
    \color:var(--leksah-fg-muted);border:1px solid var(--leksah-border-control);\
    \box-shadow:0 0 64px var(--leksah-shadow-glow)"
fieldStyle =
    "display:block;width:100%;box-sizing:border-box;margin:4px 0;padding:5px 8px"
btnStyle        = "margin:12px 6px 0 0;padding:4px 12px"
primaryBtnStyle = "margin:12px 6px 0 0;padding:4px 12px;font-weight:bold"
