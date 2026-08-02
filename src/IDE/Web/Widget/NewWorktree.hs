{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- reflex-dom deprecates 'textInput'; the find bar (and the rest of leksah)
-- still uses it deliberately, so match that here rather than churn.
{-# OPTIONS_GHC -Wno-deprecations #-}
-- | The @New Claude Session in Worktree…@ modal (Claude plan Stage 2).  A
-- task-name field with a live preview of the branch\/directory it will create;
-- Create runs the whole flow on a background thread: make the worktree
-- ('IDE.Web.Worktree.newClaudeWorktree'), add it to the workspace as its own
-- project (so it gets the git\/PR badges and file tree every project has), and
-- start a @claude@ session in it.  Mirrors "IDE.Web.Widget.AddRemote"'s
-- structure: the dialog owns its flow and just returns a close event.
module IDE.Web.Widget.NewWorktree
  ( newWorktreeDialog
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Lens ((.~))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T (null, pack, strip)

import Reflex
       (constDyn, ffor, leftmost, current, performEvent_, holdDyn,
        fmapMaybe, tag, newTriggerEvent)
import Reflex.Dom.Core
       (elAttr, elAttr', textInput, text, dynText, MonadWidget, (=:),
        Event, attributes, domEvent, EventName(..), _textInput_value)

import IDE.Core.State (reflectIDE)
import IDE.Web.Claude (ClaudeCmd(..), runClaudeCmd)
import IDE.Web.IDERefStore (getGlobalIDERef)
import IDE.Web.Worktree (newClaudeWorktree, slugify)
import IDE.Workspaces (projectOpenPath, workspaceTryQuiet)

-- | Render the modal for starting a Claude session in a fresh worktree of the
-- repo containing @dir@.  Returns an 'Event' that fires (once) when the caller
-- should tear the modal down: on Cancel, or after the session starts.
newWorktreeDialog :: MonadWidget t m => FilePath -> m (Event t ())
newWorktreeDialog dir = do
    -- Result of the background attempt: Left = inline error, Right = done.
    (resultE, fireResult) <- newTriggerEvent
    elAttr "div" ("class" =: "new-worktree-overlay" <> "style" =: overlayStyle) $
      elAttr "div" ("class" =: "new-worktree-dialog" <> "style" =: dialogStyle) $ do
        elAttr "p" ("style" =: "font-weight:bold;margin:0 0 6px 0") $
            text "New Claude Session in Worktree"
        elAttr "p" ("style" =: hintStyle) $
            text ("An isolated checkout for one agent task, so parallel sessions\
                  \ never step on each other's files.  Repo: " <> T.pack dir)
        nameTi <- textInput $ def & attributes .~ constDyn
            ("placeholder" =: "task name — e.g. fix flaky tests"
             <> "style" =: fieldStyle)
        -- Live preview of what will be created.
        elAttr "p" ("style" =: hintStyle) $
            dynText $ ffor (_textInput_value nameTi) $ \n ->
                let s = slugify n
                in if T.null s then " "
                   else "creates .worktrees/" <> s <> " on branch claude/" <> s
        errD <- holdDyn "" (fmapMaybe (either Just (const Nothing)) resultE)
        elAttr "p" ("style" =: "color:#c0392b;min-height:1.1em;margin:8px 0 4px 0;font-size:12px") $
            dynText errD
        (createEl, _) <- elAttr' "button" ("style" =: primaryBtnStyle) $
            text "Create & Start Claude"
        (cancelEl, _) <- elAttr' "button" ("style" =: btnStyle) $ text "Cancel"
        performEvent_ $ ffor (tag (current (_textInput_value nameTi))
                                  (domEvent Click createEl)) $
            \name -> liftIO . void . forkIO $ createIO dir name fireResult
        return $ leftmost
            [ ()  <$ domEvent Click cancelEl
            , fmapMaybe (either (const Nothing) Just) resultE ]

-- | The background flow: create the worktree, add it to the workspace as a
-- (directory) project, start @claude@ there.  Errors go back inline.
createIO :: FilePath -> Text -> (Either Text () -> IO ()) -> IO ()
createIO dir name fire =
    (`catch` \(e :: SomeException) -> fire (Left (T.pack (show e)))) $
      newClaudeWorktree dir (T.strip name) >>= \case
        Left err -> fire (Left err)
        Right (wtPath, _branch) -> do
          getGlobalIDERef >>= \case
            Nothing   -> return ()   -- no IDE yet: the session still starts
            Just ideR -> void $
                reflectIDE (workspaceTryQuiet (projectOpenPath wtPath)) ideR
          runClaudeCmd (ClaudeNew wtPath)
          fire (Right ())

overlayStyle, dialogStyle, fieldStyle, hintStyle, btnStyle, primaryBtnStyle :: Text
overlayStyle =
    "position:fixed;inset:0;z-index:1000;display:flex;align-items:center;\
    \justify-content:center;background:var(--leksah-scrim)"
dialogStyle =
    "min-width:420px;max-width:560px;padding:16px 20px;border-radius:8px;\
    \background:var(--leksah-surface);color:var(--leksah-fg-muted);\
    \border:1px solid var(--leksah-border-control);\
    \box-shadow:0 0 64px var(--leksah-shadow-glow)"
fieldStyle =
    "display:block;width:100%;box-sizing:border-box;margin:4px 0;padding:5px 8px"
hintStyle =
    "margin:4px 0;font-size:12px;color:var(--leksah-fg-dim);min-height:1.1em;\
    \overflow-wrap:anywhere"
btnStyle        = "margin:12px 6px 0 0;padding:4px 12px"
primaryBtnStyle = "margin:12px 6px 0 0;padding:4px 12px;font-weight:bold"
