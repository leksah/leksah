{-# LANGUAGE FlexibleContexts #-}
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
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
       (any, null, pack, strip, takeWhile, lines)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode(..))

import Reflex
       (constDyn, delay, getPostBuild, ffor, ffilter, leftmost, current, holdDyn,
        fmapMaybe, tag, newTriggerEvent, updated)
import Reflex.Dom.Core
       (elAttr, elAttr', elDynAttr, textInput, text, dynText, (=:), checkbox,
        Event, attributes, domEvent, EventName(..), _textInput_value,
        _textInput_keydown, _checkbox_value, textInputConfig_setValue)

import IDE.App (appConfig, getGlobalApp)
import IDE.Config
       (Config(..), RemoteC(..), WakeOnLan(..), currentConfig, saveConfig)
import IDE.Web.WakeOnLan (detectMac, parseMac, wakeAndWait)
import IDE.Web.TmuxServer (remoteTmux)
import IDE.Utils.RemoteExec (runSsh)
import IDE.Web.Frame (MonadWidget, performEvent_)
import IDE.Web.TextAssist (noTextAssist)
import IDE.Web.Widget.PathField (focusFieldJs)

-- | Render the modal.  Returns an 'Event' that fires (once) when the caller
-- should tear the modal down: on Cancel, or after a server is successfully
-- added.
addServerDialog :: MonadWidget t m => m (Event t ())
addServerDialog = do
    -- Result of a background add attempt:
    -- Left (retriable, msg) = inline error (retriable = probe failure, so the
    -- next Add on the same host skips the probe); Right = added.
    (resultE, fireResult) <- newTriggerEvent
    -- Progress while it works (waking the machine can take a minute).
    (progressE, fireProgress) <- newTriggerEvent
    -- A detected MAC address, filled into its field.
    (macFoundE, fireMacFound) <- newTriggerEvent
    elAttr "div" ("class" =: "add-server-overlay" <> "style" =: overlayStyle) $
      elAttr "div" ("class" =: "add-server-dialog" <> "style" =: dialogStyle) $ do
        elAttr "p" ("style" =: "font-weight:bold;margin:0 0 10px 0") $
            text "Add Server (over ssh)"
        hostTi <- textInput $ def & attributes .~ constDyn
            ("id" =: "add-server-host" <> "placeholder" =: "ssh host — e.g. user@host or an ssh-config alias"
             <> "style" =: fieldStyle <> noTextAssist)
        -- The keyboard starts in the field (a just-built input is not focused).
        focusFieldJs "add-server-host" =<< delay 0.03 =<< getPostBuild
        -- Wake-on-LAN: wake the machine when it doesn't answer (here, and
        -- whenever leksah connects to it later).
        wolCb <- elAttr "label" ("style" =: "display:flex;align-items:center;gap:6px;margin:8px 0 2px 0;font-size:12px") $ do
            cb <- checkbox False def
            text "Wake on LAN — wake this machine when it doesn't answer"
            return cb
        let wolOnD = _checkbox_value wolCb
        (macTi, bcastTi, detectE) <- elDynAttr "div"
            ((\on -> "style" =: ("margin:2px 0 0 22px;" <> if on then "" else "display:none")) <$> wolOnD) $ do
          (macTi', detectEl) <- elAttr "div" ("style" =: "display:flex;gap:6px;align-items:center") $ do
            m <- textInput $ def
                & attributes .~ constDyn
                    ("placeholder" =: "MAC address — e.g. a8:a1:59:12:34:56"
                     <> "style" =: (fieldStyle <> ";flex:1") <> noTextAssist)
                & textInputConfig_setValue .~ macFoundE
            (d, _) <- elAttr' "button" ("style" =: "padding:3px 8px;white-space:nowrap"
                                         <> "title" =: "Ask the machine (it must be up now) for its network card's address") $
                text "Detect"
            return (m, d)
          b <- textInput $ def & attributes .~ constDyn
              ("placeholder" =: "broadcast address (optional) — default 255.255.255.255"
               <> "style" =: fieldStyle <> noTextAssist)
          return (macTi', b, domEvent Click detectEl)
        performEvent_ $ ffor (tag (current (_textInput_value hostTi)) detectE) $ \h ->
            liftIO . void . forkIO $ do
                fireProgress ("Asking " <> T.strip h <> " for its MAC address…")
                detectMac (T.strip h) >>= \case
                    Just mac -> fireMacFound mac >> fireProgress ""
                    Nothing  -> fireResult (Left (False,
                        "Could not detect it — the machine must be up and reachable over ssh."))
        let failE = fmapMaybe (either Just (const Nothing)) resultE
            editedE = leftmost [ () <$ updated (_textInput_value hostTi)
                               , () <$ updated (_textInput_value macTi) ]
        errD <- holdDyn "" $ leftmost
            [ snd <$> failE
            , "" <$ editedE
            , "" <$ progressE ]
        progD <- holdDyn "" $ leftmost [ progressE, "" <$ failE, "" <$ editedE ]
        elAttr "p" ("style" =: "color:var(--leksah-fg-dim);min-height:1.1em;margin:8px 0 0 0;font-size:12px") $
            dynText progD
        elAttr "p" ("style" =: "color:#c0392b;min-height:1.1em;margin:2px 0 4px 0;font-size:12px") $
            dynText errD
        -- After a failed reachability probe, the NEXT Add of the same text
        -- skips the probe ("press Add again to add it anyway"); editing the
        -- host re-arms it.
        skipProbeD <- holdDyn False $ leftmost
            [ fst <$> failE
            , False <$ updated (_textInput_value hostTi) ]
        (addEl, _)    <- elAttr' "button" ("style" =: primaryBtnStyle) $ text "Add"
        (cancelEl, _) <- elAttr' "button" ("style" =: btnStyle) $ text "Cancel"
        let wolD = (\on mac b -> if on then Just (mac, b) else Nothing)
                     <$> wolOnD <*> _textInput_value macTi <*> _textInput_value bcastTi
            valsD = (,,) <$> _textInput_value hostTi <*> skipProbeD <*> wolD
            -- Enter in a field = Add (keydown: keypress is unreliable for
            -- Enter in WKWebView).
            enterE = leftmost [ () <$ ffilter (== 13) (_textInput_keydown hostTi)
                              , () <$ ffilter (== 13) (_textInput_keydown macTi)
                              , () <$ ffilter (== 13) (_textInput_keydown bcastTi) ]
            submitE = leftmost [ domEvent Click addEl, enterE ]
        performEvent_ $ ffor (tag (current valsD) submitE) $
            \(h, skip, wol) -> liftIO . void . forkIO $
                addServerIO h skip wol fireProgress fireResult
        return $ leftmost
            [ ()  <$ domEvent Click cancelEl
            , fmapMaybe (either (const Nothing) Just) resultE ]

-- | Do the actual work on a background thread: validate, probe (waking the
-- machine first when Wake-on-LAN is on and it doesn't answer), add to the
-- 'rcHosts' setting (and its 'rcWake' entry); report progress, and Left
-- (retriable, error) / Right () through the callbacks.  An ALREADY-listed
-- host with Wake-on-LAN given just has its wake settings updated.
addServerIO :: Text -> Bool -> Maybe (Text, Text) -> (Text -> IO ())
            -> (Either (Bool, Text) () -> IO ()) -> IO ()
addServerIO host skipProbe mwol progress fire =
    (`catch` \(e :: SomeException) -> fire (Left (False, T.pack (show e)))) $ do
      let h = T.strip host
          mwake = ffor mwol $ \(mac, b) ->
            WakeOnLan h (T.strip mac) (if T.null (T.strip b) then Nothing else Just (T.strip b))
      if T.null h
        then fire (Left (False, "Host is required."))
      else if T.any (== ' ') h
        then fire (Left (False, "Host must not contain spaces — use an ssh destination or config alias."))
      else if maybe False (isNothing . parseMac . wolMac) mwake
        then fire (Left (False, "That is not a MAC address (six hex pairs, e.g. a8:a1:59:12:34:56)."))
      else getGlobalApp >>= \case
        Nothing  -> fire (Left (False, "IDE is not ready yet."))
        Just app -> do
          cfg <- currentConfig (appConfig app)
          let known = h `elem` rcHosts (cfgRemote cfg)
              save = do
                -- Re-read at write time so the probe window can't clobber
                -- a concurrent settings change.
                cfg' <- currentConfig (appConfig app)
                let rc = cfgRemote cfg'
                saveConfig (appConfig app) cfg'
                    { cfgRemote = rc
                        { rcHosts = if known then rcHosts rc else rcHosts rc <> [h]
                        , rcWake  = maybe (rcWake rc)
                            (\w -> filter ((/= h) . wolHost) (rcWake rc) <> [w]) mwake } }
                fire (Right ())
              -- …and that it can run tmux there (its own, or nix's).
              checkTmuxThenSave = do
                progress ("Checking for tmux on " <> h <> "…")
                remoteTmux h >>= \case
                  Left err -> fire (Left (True, err <> " — press Add again to add it anyway."))
                  Right (_, viaNix) -> do
                    if viaNix
                      then progress ("tmux is not installed on " <> h
                                     <> " — using nixpkgs' (kept in ~/.cache/leksah-tmux there).")
                      else return ()
                    save
          if known && isNothing mwake
            then fire (Left (False, h <> " is already in the server list."))
          else if known || skipProbe then save
          else do
            progress ("Checking " <> h <> "…")
            probed <- probeHost h
            case (probed, mwake) of
              (Right (), _) -> checkTmuxThenSave
              (Left err, Nothing) -> fire (Left (True, err))
              (Left _, Just w) -> do
                progress (h <> " is not answering — sent Wake-on-LAN, waiting for it to come up (up to 2 minutes)…")
                wakeAndWait w >>= \case
                  Right True  -> checkTmuxThenSave
                  Right False -> fire (Left (True, h <> " did not come up after the Wake-on-LAN packet — press Add again to add it anyway."))
                  Left e      -> fire (Left (False, "Could not send the Wake-on-LAN packet: " <> e))

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
