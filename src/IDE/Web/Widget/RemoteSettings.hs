{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- reflex-dom deprecates 'textInput'; the rest of leksah still uses it
-- deliberately, so match that here rather than churn.
{-# OPTIONS_GHC -Wno-deprecations #-}
-- | The @Remote Settings…@ modal (project-tree context menu).  Edits the
-- per-project command prefix ('psCmdPrefix') — the shell fragment spliced
-- before every tool command run for a remote project (e.g. @nix develop -c@).
--
-- Like "IDE.Web.Widget.AddRemote", the dialog owns its whole flow via
-- 'getGlobalIDERef', so it needs no feedback wiring from "IDE.Web.Main": it
-- reads the current prefix at build time, and on Save persists the new value on
-- a background thread.  It returns an 'Event' that fires when it should close
-- (Save or Cancel).  Shown by the @Remote Settings…@ context-menu item, routed
-- through the "IDE.Web.RemoteSettingsRequest" bridge.
module IDE.Web.Widget.RemoteSettings
  ( remoteSettingsDialog
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Lens ((.~))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T (null, pack, strip)

import Reflex
       (constDyn, ffor, leftmost, current, performEvent_, tag)
import Reflex.Dom.Core
       (elAttr, elAttr', textInput, text, MonadWidget, (=:), Event,
        attributes, domEvent, EventName(..), _textInput_value,
        textInputConfig_initialValue)

import IDE.Core.State (reflectIDE, readIDE)
import IDE.Core.Types
       (ProjectKey, pjFileOrDir, workspace, wsSettingsFor, psCmdPrefix,
        ProjectSettings(..))
import IDE.Utils.RemotePath (parseRemotePath)
import IDE.Web.IDERefStore (getGlobalIDERef)
import IDE.Project.WorkspaceFile (setProjectSettings)

-- | Render the modal for a project.  Reads the project's current command
-- prefix, lets the user edit it, and on Save persists it (empty clears it).
-- Returns an 'Event' that fires (once) when the caller should tear the modal
-- down: on Cancel or after a successful Save.
remoteSettingsDialog :: MonadWidget t m => ProjectKey -> m (Event t ())
remoteSettingsDialog pk = do
    current0 <- liftIO (currentPrefix pk)
    let label = case parseRemotePath (pjFileOrDir pk) of
                  Just (host, rlocal) -> host <> ":" <> T.pack rlocal
                  Nothing             -> T.pack (pjFileOrDir pk)
    elAttr "div" ("class" =: "remote-settings-overlay" <> "style" =: overlayStyle) $
      elAttr "div" ("class" =: "remote-settings-dialog" <> "style" =: dialogStyle) $ do
        elAttr "p" ("style" =: "font-weight:bold;margin:0 0 4px 0") $
            text "Project Settings"
        elAttr "p" ("style" =: "margin:0 0 10px 0;font-size:12px;color:var(--leksah-fg-dim)") $
            text label
        elAttr "label" ("style" =: "font-size:12px;color:var(--leksah-fg-muted)") $
            text "Command prefix (e.g. nix develop -c) — used to open terminals, and to run tools on remote projects:"
        prefTi <- textInput $ def
            & attributes .~ constDyn
                ("placeholder" =: "e.g. nix develop -c" <> "style" =: fieldStyle)
            & textInputConfig_initialValue .~ current0
        (saveEl, _)   <- elAttr' "button" ("style" =: primaryBtnStyle) $ text "Save"
        (cancelEl, _) <- elAttr' "button" ("style" =: btnStyle) $ text "Cancel"
        performEvent_ $ ffor (tag (current (_textInput_value prefTi)) (domEvent Click saveEl)) $
            \pre -> liftIO . void . forkIO $ savePrefix pk pre
        return $ leftmost [ () <$ domEvent Click cancelEl
                          , () <$ domEvent Click saveEl ]

-- | The project's current command prefix (empty string when unset / no IDE).
currentPrefix :: ProjectKey -> IO Text
currentPrefix pk = getGlobalIDERef >>= \case
    Nothing   -> return ""
    Just ideR -> do
        mbWs <- reflectIDE (readIDE workspace) ideR
        return $ fromMaybe "" (mbWs >>= psCmdPrefix . wsSettingsFor pk)

-- | Persist the (stripped) prefix on a background thread; empty clears it
-- (@setProjectSettings@ drops the entry when it equals the default).
savePrefix :: ProjectKey -> Text -> IO ()
savePrefix pk raw =
    (`catch` \(_ :: SomeException) -> return ()) $
      getGlobalIDERef >>= \case
        Nothing   -> return ()
        Just ideR -> do
            let pre = T.strip raw
                settings = ProjectSettings
                    { psCmdPrefix = if T.null pre then Nothing else Just pre }
            void $ reflectIDE (setProjectSettings pk settings) ideR

overlayStyle, dialogStyle, fieldStyle, btnStyle, primaryBtnStyle :: Text
overlayStyle =
    "position:fixed;inset:0;z-index:1000;display:flex;align-items:center;\
    \justify-content:center;background:var(--leksah-scrim)"
dialogStyle =
    "min-width:380px;padding:16px 20px;border-radius:8px;background:var(--leksah-surface);\
    \color:var(--leksah-fg-muted);border:1px solid var(--leksah-border-control);\
    \box-shadow:0 0 64px var(--leksah-shadow-glow)"
fieldStyle =
    "display:block;width:100%;box-sizing:border-box;margin:4px 0 0 0;padding:5px 8px"
btnStyle        = "margin:12px 6px 0 0;padding:4px 12px"
primaryBtnStyle = "margin:12px 6px 0 0;padding:4px 12px;font-weight:bold"
