{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- reflex-dom deprecates 'textInput'; the find bar (and the rest of leksah)
-- still uses it deliberately, so match that here rather than churn.
{-# OPTIONS_GHC -Wno-deprecations #-}
-- | The @Add Remote Project…@ modal (File menu).  A small host / path / prefix
-- form that does exactly what @leksah-cmd project open host:path@ +
-- @project set-prefix@ do: resolve the scp-style input to a canonical
-- @ssh:\/\/@ project path (one cached ssh round trip expands a leading @~@),
-- add it to the workspace, and persist the optional per-project command prefix.
--
-- The dialog owns its whole flow (validation + the add) on a background thread
-- via 'getGlobalIDERef', so it needs no feedback wiring from "IDE.Web.Main" —
-- it just returns an 'Event' that fires when it should close (Cancel, or a
-- successful add).  Shown by the @CommandProjectAddRemote@ menu command, routed
-- through the "IDE.Web.AddRemoteRequest" bridge (native menu) or the web
-- menubar.  Styling is inline, mirroring the save-close prompt in Main.
module IDE.Web.Widget.AddRemote
  ( addRemoteDialog
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Lens ((.~))
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T (null, pack, strip)

import Reflex
       (constDyn, Dynamic, ffor, leftmost, current, performEvent_, holdDyn,
        fmapMaybe, tag, newTriggerEvent)
import Reflex.Dom.Core
       (elAttr, elAttr', textInput, text, dynText, dyn_, MonadWidget, (=:),
        Event, attributes, domEvent, EventName(..), blank, _textInput_value)

import IDE.Core.State (reflectIDE)
import IDE.Core.Types (filePathToProjectKey, ProjectSettings(..))
import IDE.Utils.RemoteExec (resolveProjectInput)
import IDE.Web.IDERefStore (getGlobalIDERef)
import IDE.Project.WorkspaceFile (projectOpenThis, setProjectSettings)

-- | Render the modal.  Returns an 'Event' that fires (once) when the caller
-- should tear the modal down: on Cancel, or after a project is successfully
-- added.  @hostsD@ seeds the host field's autocomplete datalist (the
-- @remoteHosts@ pref + any hosts already in the workspace).
addRemoteDialog :: MonadWidget t m => Dynamic t [Text] -> m (Event t ())
addRemoteDialog hostsD = do
    -- Result of a background add attempt: Left = inline error, Right = added.
    (resultE, fireResult) <- newTriggerEvent
    elAttr "div" ("class" =: "add-remote-overlay" <> "style" =: overlayStyle) $
      elAttr "div" ("class" =: "add-remote-dialog" <> "style" =: dialogStyle) $ do
        elAttr "p" ("style" =: "font-weight:bold;margin:0 0 10px 0") $
            text "Add Remote Project (over ssh)"
        hostTi <- textInput $ def & attributes .~ constDyn
            ("placeholder" =: "ssh host — e.g. x86_64-linux-0"
             <> "list" =: "leksah-remote-hosts" <> "style" =: fieldStyle)
        elAttr "datalist" ("id" =: "leksah-remote-hosts") $
            dyn_ (ffor hostsD $ mapM_ (\h -> elAttr "option" ("value" =: h) blank))
        pathTi <- textInput $ def & attributes .~ constDyn
            ("placeholder" =: "remote path — e.g. ~/proj/cabal.project" <> "style" =: fieldStyle)
        prefTi <- textInput $ def & attributes .~ constDyn
            ("placeholder" =: "command prefix (optional) — e.g. nix develop -c"
             <> "style" =: fieldStyle)
        errD <- holdDyn "" (fmapMaybe (either Just (const Nothing)) resultE)
        elAttr "p" ("style" =: "color:#c0392b;min-height:1.1em;margin:8px 0 4px 0;font-size:12px") $
            dynText errD
        (addEl, _)    <- elAttr' "button" ("style" =: primaryBtnStyle) $ text "Add"
        (cancelEl, _) <- elAttr' "button" ("style" =: btnStyle) $ text "Cancel"
        let valsD = (,,) <$> _textInput_value hostTi
                         <*> _textInput_value pathTi
                         <*> _textInput_value prefTi
        performEvent_ $ ffor (tag (current valsD) (domEvent Click addEl)) $
            \(h, p, pre) -> liftIO . void . forkIO $ addRemoteIO h p pre fireResult
        return $ leftmost
            [ ()  <$ domEvent Click cancelEl
            , fmapMaybe (either (const Nothing) Just) resultE ]

-- | Do the actual work on a background thread: validate, resolve, add, persist
-- the prefix; report Left error / Right () through the callback.  All ssh work
-- (the ~ expansion inside 'resolveProjectInput') stays off the frame thread.
addRemoteIO :: Text -> Text -> Text -> (Either Text () -> IO ()) -> IO ()
addRemoteIO host path prefix fire =
    (`catch` \(e :: SomeException) -> fire (Left (T.pack (show e)))) $
      let h = T.strip host; p = T.strip path; pre = T.strip prefix in
      if T.null h || T.null p
        then fire (Left "Host and path are both required.")
        else resolveProjectInput "." (h <> ":" <> p) >>= \case
          Left err -> fire (Left err)
          Right fp -> case filePathToProjectKey fp of
            Nothing -> fire (Left ("Not a project file: " <> T.pack fp))
            Just pk -> getGlobalIDERef >>= \case
              Nothing   -> fire (Left "IDE is not ready yet.")
              Just ideR -> do
                void $ reflectIDE (projectOpenThis pk) ideR
                unless (T.null pre) . void $ reflectIDE
                    (setProjectSettings pk ProjectSettings { psCmdPrefix = Just pre }) ideR
                fire (Right ())

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
