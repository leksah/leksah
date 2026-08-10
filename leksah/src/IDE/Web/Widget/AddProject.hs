{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | The @Add Project…@ modal (File menu) — the one way to put something in the
-- workspace.
--
-- It replaced three menu items (@Open Project…@, @Open Folder…@,
-- @Add Remote Project…@) that all ended at the same
-- 'IDE.Workspace.projectOpenPath': a project file, a plain folder and a remote
-- directory differ only in what you type, so they are one form with a Server
-- field rather than three doors to choose between before you start.
--
-- Server defaults to @local@ and the keyboard starts in the Path field, so the
-- common case is "type a path, press Enter".  With a local server the path has
-- a Browse button beside it; on macOS one button, because @NSOpenPanel@ chooses
-- a file OR a folder in a single panel (GTK4 and Win32 cannot, so they draw
-- two — see 'IDE.Web.OpenPanel.PickCapability'), and none at all where there is
-- no native panel (warp, ghcjs) or when the path is on another machine.
--
-- Like its siblings ("IDE.Web.Widget.AddServer", "IDE.Web.Widget.NewWorktree")
-- the dialog owns its whole flow on a background thread via 'getGlobalApp', so
-- it needs no feedback wiring from "IDE.Web.Main" — it returns an 'Event' that
-- fires when it should close, plus one carrying the successful add so the web
-- session can persist the recents.
module IDE.Web.Widget.AddProject
  ( addProjectDialog
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, getHomeDirectory)

import Reflex
       (Dynamic, Event, attachWithMaybe, constDyn, current, ffilter, ffor,
        fmapMaybe, holdDyn, holdUniqDyn, leftmost, never, newTriggerEvent,
        switchHold, tag, updated)
import Reflex.Dom.Core
       (delay, domEvent, dyn, dynText, elAttr, elAttr', EventName(..),
        getPostBuild, text, (=:))

import IDE.App (appWorkspace, getGlobalApp)
import IDE.Utils.RemoteExec (resolveProjectInput)
import IDE.Web.OpenPanel
       (PickCapability(..), PickMode(..), pickCapability, runPickPathPanel)
import IDE.Web.PickPathRequest (newPickToken)
import IDE.Web.ProjectRecents
       (ProjectRecents(..), RecentEntry(..), readProjectRecents, recentPathsFor,
        recordProjectRecent)
import IDE.Web.Widget.PathField
       (PathField(..), PathFieldConfig(..), collapseHome, comboField, expandHome,
        focusFieldJs, localServer, pathField)
import IDE.Web.Frame (MonadWidget, performEvent_)
import IDE.Workspace (projectOpenKey, setProjectCmdPrefix)
import IDE.Ws.Registry (detectProject)
import IDE.Ws.Types (defaultEffects)

serverFieldId, pathFieldId, prefixFieldId :: Text
serverFieldId = "leksah-ap-server"
pathFieldId   = "leksah-ap-path"
prefixFieldId = "leksah-ap-prefix"

-- | Render the modal.  @serversD@ is every server the UI knows about
-- (@"local"@ first, then the ssh hosts); @pickedE@ carries native browse
-- results for EVERY dialog, which is why this one keeps only its own token's.
--
-- Returns the close event and, separately, the successful add — the latter only
-- so "IDE.Web.Main" can fold it into the saved session.  The recents themselves
-- are already updated by then: 'recordProjectRecent' writes the process-global
-- mirror on the add thread, which is what a dialog in another window reads.
addProjectDialog
  :: MonadWidget t m
  => Dynamic t [Text]
  -> Event t (Int, FilePath)
  -> m (Event t (), Event t RecentEntry)
addProjectDialog serversD pickedE = do
    -- All three reads are cheap, total and done once at build.  The token is an
    -- IORef bump; the capability is registered by the front end before any
    -- window builds; the recents are an IORef read.
    tok     <- liftIO newPickToken
    cap     <- liftIO pickCapability
    recents <- liftIO readProjectRecents
    -- Only for DISPLAY (collapsing a browse result back to ~/…); the add path
    -- expands ~ again on its own thread.  Caught because a throw on the frame
    -- thread would freeze the whole window.
    home    <- liftIO (getHomeDirectory `catch` \(_ :: SomeException) -> pure "")

    (resultE, fireResult) <- newTriggerEvent
    let failE     = fmapMaybe (either Just (const Nothing)) resultE
        successE  = fmapMaybe (either (const Nothing) Just) resultE
        recentsD  = constDyn recents
        initPath  = defaultPathFor localServer recents

    elAttr "div" ("class" =: "add-project-overlay" <> "style" =: overlayStyle) $ do
      (dialogEl, (closeE, submitE, valsD, anyPopupD)) <- elAttr' "div"
        ("class" =: "add-project-dialog" <> "tabindex" =: "-1"
         <> "style" =: dialogStyle) $ do
          elAttr "p" ("style" =: "font-weight:bold;margin:0 0 10px 0") $
              text "Add Project"

          rec
            -- Server.  Its drop-down is every known server, most recently used
            -- first — so "local" stays one keystroke away after remote work.
            let serverItemsD = ffor serversD $ \servers ->
                    nub (prServers recents <> servers)
            serverF <- labelled "Server" $
                comboField serverFieldId "local, or an ssh host" localServer
                           serverItemsD never
            serverD <- holdUniqDyn (pfValue serverF)

            -- Path, with the browse button(s) beside it.
            let pathRecentsD = ffor serverD (`recentPathsFor` recents)
            (pathF, browseE) <- labelled "Path" $
                -- 'center', not 'flex-start': the browse button is shorter than
                -- the input, so aligning their TOPS left it sitting low.  The
                -- row's height is the field wrapper's (the drop-down inside it
                -- is absolutely positioned and adds none), so centring here
                -- centres the button on the field itself.
                elAttr "div" ("style" =: "display:flex;align-items:center;gap:6px") $ do
                  p <- elAttr "div" ("style" =: "flex:1;min-width:0") $
                      pathField PathFieldConfig
                        { pfcId          = pathFieldId
                        , pfcPlaceholder = "a project file or a folder"
                        , pfcInitial     = initPath
                        , pfcServer      = serverD
                        , pfcRecents     = pathRecentsD
                        , pfcSetValue    = pathSetE
                        }
                  b <- browseButtons cap serverD
                  pure (p, b)

            -- Showing a panel is fire-and-forget; the path comes back on the
            -- PickPathRequest queue carrying this dialog's token.
            performEvent_ $ ffor browseE $ liftIO . (`runPickPathPanel` tok)

            let -- …so keep only OUR results: one from a dialog that has since
                -- closed, or from another window's, is dropped rather than
                -- landing in a field that never asked.
                pickedMineE = fmapMaybe
                    (\(t, p) -> if t == tok
                                  then Just (collapseHome home (T.pack p))
                                  else Nothing) pickedE
                -- Changing the server re-defaults the path, but only while the
                -- user has not edited it — otherwise choosing a server after
                -- typing a path would throw the path away.
                redefaultE = attachWithMaybe
                    (\(lastSet, cur) srv ->
                        if cur == lastSet then Just (defaultPathFor srv recents)
                                          else Nothing)
                    (current ((,) <$> lastSetD <*> pfValue pathF))
                    (updated serverD)
                pathSetE = leftmost [pickedMineE, redefaultE]
            lastSetD <- holdDyn initPath pathSetE

            prefixF <- labelled "Command prefix" $
                comboField prefixFieldId "optional — e.g. nix develop -c" ""
                           (prPrefixes <$> recentsD) never

            errD <- holdDyn "" $ leftmost
                [ failE
                  -- Clear a stale error as soon as anything is edited again.
                , "" <$ updated (pfValue pathF)
                , "" <$ updated (pfValue serverF) ]
            elAttr "p" ("class" =: "add-project-error"
                        <> "style" =: "color:#c0392b;min-height:1.1em;\
                                      \margin:8px 0 4px 0;font-size:12px") $
                dynText errD

            -- Cancel then Add, right-aligned: the default action is the
            -- rightmost button everywhere else on this platform, so the order
            -- follows the alignment.
            (cancelEl, addEl) <- elAttr "div" ("style" =: footerStyle) $ do
                (c, _) <- elAttr' "button" ("style" =: btnStyle) $ text "Cancel"
                (a, _) <- elAttr' "button" ("style" =: primaryBtnStyle) $ text "Add"
                pure (c, a)

          let vals = (,,) <$> pfValue serverF <*> pfValue pathF <*> pfValue prefixF
              -- Whether ANY field's drop-down is up, so the dialog's own
              -- Escape handler can stand aside for it.
              anyPopup = or <$> sequenceA
                  [ pfPopupOpen serverF, pfPopupOpen pathF, pfPopupOpen prefixF ]
          pure ( leftmost [ () <$ domEvent Click cancelEl
                          , pfCancel pathF, pfCancel serverF, pfCancel prefixF ]
               , leftmost [ () <$ domEvent Click addEl
                          , pfSubmit pathF, pfSubmit serverF, pfSubmit prefixF ]
               , vals
               , anyPopup )

      -- Escape anywhere in the dialog — including on a button, where no field
      -- sees the key — closes it.  But a key pressed IN a field also bubbles
      -- here, so this must not fire while a drop-down is up: there, the first
      -- Escape closes the list and only the second closes the dialog.
      let dialogEscE = fmapMaybe (\open -> if open then Nothing else Just ())
              (tag (current anyPopupD)
                   (ffilter (== (27 :: Word)) (domEvent Keydown dialogEl)))

      -- The keyboard starts in the Path field.  One tick after build, like the
      -- save-close prompt in "IDE.Web.Main": a just-built input is not focused.
      pb <- getPostBuild
      focusFieldJs pathFieldId =<< delay 0.03 pb

      performEvent_ $ ffor (tag (current valsD) submitE) $ \(s, p, pre) ->
          liftIO . void . forkIO $ addProjectIO s p pre fireResult

      pure ( leftmost [closeE, dialogEscE, () <$ successE], successE )

-- | A labelled row.  Written out rather than pulled into a shared form helper
-- because these are the only three.
labelled :: MonadWidget t m => Text -> m a -> m a
labelled label inner = elAttr "div" ("style" =: "margin:6px 0") $ do
    elAttr "label" ("style" =: "display:block;font-size:11px;\
                               \color:var(--leksah-fg-dim)") $ text label
    inner

-- | The Browse button(s), or nothing at all.
--
-- Hidden unless the path is local AND this front end has a native picker:
-- @NSOpenPanel@ cannot reach an ssh host, and warp\/ghcjs have no panel to
-- show.  One button where one panel can choose a file or a folder, two where
-- the toolkit insists on separate dialogs.
browseButtons :: MonadWidget t m
              => PickCapability -> Dynamic t Text -> m (Event t PickMode)
browseButtons cap serverD = do
    let showD = ffor serverD $ \s -> s == localServer && cap /= PickNone
    switchHold never =<< dyn (ffor showD $ \case
        False -> pure never
        True  -> case cap of
            PickSeparate -> do
                f <- browseButton "File…"   "Choose a project file"
                d <- browseButton "Folder…" "Choose a folder"
                pure (leftmost [PickFiles <$ f, PickDirs <$ d])
            _ -> (PickFilesAndDirs <$) <$>
                    browseButton "Browse…" "Choose a project file or a folder")
  where
    browseButton label tip = do
        (el, _) <- elAttr' "button"
            ("type" =: "button" <> "class" =: "ap-browse"
             <> "title" =: tip <> "style" =: btnStyle) $ text label
        pure (() <$ domEvent Click el)

-- | The path a freshly opened (or freshly re-servered) dialog starts on: the
-- newest path remembered for that server, else the home directory.
defaultPathFor :: Text -> ProjectRecents -> Text
defaultPathFor server r = case recentPathsFor server r of
    (p:_) -> p
    []    -> "~/"

-- | Validate, resolve, add, persist — all on a forked thread, so neither the
-- ssh round trip inside 'resolveProjectInput' nor the workspace write happens
-- on the frame thread.  Reports @Left@ as an inline error (the dialog stays
-- open with the text intact) or @Right@ with the entry to remember.
addProjectIO :: Text -> Text -> Text -> (Either Text RecentEntry -> IO ()) -> IO ()
addProjectIO server path prefix fire =
    (`catch` \(e :: SomeException) -> fire (Left (T.pack (show e)))) $
      let s   = let t = T.strip server in if T.null t then localServer else t
          p   = T.strip path
          pre = T.strip prefix
      in if T.null p
        then fire (Left "A path is required.")
        else do
          cwd  <- getCurrentDirectory
          home <- getHomeDirectory
          -- A remote path keeps its ~ and is expanded on the far side by
          -- resolveProjectInput (one cached ssh round trip); only a local one
          -- is expanded here.
          let input | s == localServer = expandHome home p
                    | otherwise        = s <> ":" <> p
          resolveProjectInput cwd input >>= \case
            Left err -> fire (Left err)
            Right fp -> detectProject defaultEffects fp >>= \case
              -- In practice only an unreadable or non-existent path gets here:
              -- the plain-directory type claims any directory that exists.
              Nothing -> fire (Left ("No project type claims " <> T.pack fp))
              Just pk -> getGlobalApp >>= \case
                Nothing  -> fire (Left "IDE is not ready yet.")
                Just app -> do
                  projectOpenKey (appWorkspace app) pk
                  unless (T.null pre) $
                      setProjectCmdPrefix (appWorkspace app) pk (Just pre)
                  let entry = RecentEntry s p pre
                  void (recordProjectRecent entry)
                  fire (Right entry)

overlayStyle, dialogStyle, footerStyle, btnStyle, primaryBtnStyle :: Text
overlayStyle =
    "position:fixed;inset:0;z-index:1000;display:flex;align-items:center;\
    \justify-content:center;background:var(--leksah-scrim)"
dialogStyle =
    "min-width:420px;padding:16px 20px;border-radius:8px;background:var(--leksah-surface);\
    \color:var(--leksah-fg-muted);border:1px solid var(--leksah-border-control);\
    \box-shadow:0 0 64px var(--leksah-shadow-glow)"
-- The two rows that hold buttons space them themselves (flex 'gap'), so the
-- buttons carry no margins of their own — a margin here would also push the
-- Browse button out of line with its field.
footerStyle     = "display:flex;justify-content:flex-end;gap:6px;margin-top:12px"
btnStyle        = "padding:4px 12px"
primaryBtnStyle = "padding:4px 12px;font-weight:bold"
