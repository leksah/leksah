{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- reflex-dom deprecates 'textInput' in favour of 'inputElement'; like the
-- find bar, the address field wants textInput's value/keypress accessors.
{-# OPTIONS_GHC -Wno-deprecations #-}
-- | The Browser pane: an embedded web page (an @iframe@) with minimal browser
-- chrome — back \/ forward \/ reload buttons and an address bar.
--
-- The page runs in its own JavaScript context (a cross-origin iframe is fully
-- isolated from the IDE's page), which also means the frame's real location
-- and history are invisible to us: the pane keeps its OWN history stack of the
-- navigations it drove (address bar, back\/forward), and links followed inside
-- the page can't move that stack.  Web Inspector reaches the frame's context
-- via right-click ▸ Inspect Element inside the page (wkwebview;
-- developerExtrasEnabled is on).
--
-- Panes are keyed by a small persistent id ('BrowserKey'); each pane's last
-- driven URL lives in a registry backed by @web-browser-panes.json@, so an
-- open browser tab survives a restart (and a ghci @:reload@, which wipes the
-- in-memory CAF) with its page, and a ⌘D conversion to a split leaf rebuilds
-- the widget at the same URL.  Closed panes' entries linger in the file —
-- they're a few bytes and their keys are never reused.
module IDE.Web.Widget.Browser
  ( browserWidget
  , browserCss
  , nextBrowserId
  , rememberUrl
  , isOwnUrl
  , embedBlockReason
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (SomeException, catch, try)
import Control.Monad (void, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, decode')
#if defined(ghcjs_HOST_OS)
import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Text.Encoding (encodeUtf8)
#endif
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (createProcess, proc, readProcessWithExitCode)
import Text.Read (readMaybe)

import Clay ((?), (-:), Css)

import GHCJS.DOM.Element (setAttribute)
import Language.Javascript.JSaddle (eval, liftJSM, valToText)

import Reflex
       (Dynamic, Event, never, constDyn, current, gate, leftmost, ffilter,
        ffor, foldDyn, fmapMaybe, tagPromptlyDyn, performEvent_, performEvent,
        delay, holdDyn, attachWithMaybe, newTriggerEvent, tag,
        tickLossyFromPostBuildTime)
import Reflex.Dom.Core
       (MonadWidget, (=:), elClass, elAttr, elDynAttr', elAttr', blank, text,
        domEvent, EventName(..), textInput, def, _element_raw,
        TextInputConfig, _textInput_value, _textInput_keypress,
        _textInputConfig_initialValue, _textInputConfig_setValue,
        _textInputConfig_attributes, getPostBuild)

import IDE.Utils.Files (getConfigFilePathForSave)
import IDE.Web.Events (BrowserEvents)
import IDE.Web.NativeBrowser (NativeBrowserOps(..), getNativeBrowserOps)

--
-- The pane registry: ids + last driven URL, persisted as a tiny JSON sidecar
-- (NOT the web session file — its record shape, and version, stay untouched).
--

data BrowserPanes = BrowserPanes { bpNext :: Int, bpUrls :: Map Int Text }

#if defined(ghcjs_HOST_OS)
-- | The page-seeded shape of the demo's registry (see 'loadPanes').
data DemoBrowserPanes = DemoBrowserPanes Int (Map Int Text)

instance FromJSON DemoBrowserPanes where
  parseJSON = withObject "DemoBrowserPanes" $ \o ->
    DemoBrowserPanes <$> o .: "next" <*> o .: "urls"
#endif

-- | @Nothing@ = not loaded from disk yet (also the state after a ghci
-- @:reload@ re-initialises the CAF — the next access reloads the file).
{-# NOINLINE browserPanesRef #-}
browserPanesRef :: MVar (Maybe BrowserPanes)
browserPanesRef = unsafePerformIO (newMVar Nothing)

panesPath :: IO FilePath
panesPath = getConfigFilePathForSave "web-browser-panes.json"

loadPanes :: IO BrowserPanes
#if defined(ghcjs_HOST_OS)
-- Browser demo: no config dir to read, so the hosting page seeds the registry
-- via @window.leksahDemoBrowserPanes@ (@{"next":2,"urls":{"1":"/try/breakout/"}}@
-- — pane 1 is the showcase window's game; @next@ keeps visitor-opened panes
-- from colliding with it).  Same page-seeded pattern as "IDE.Web.DemoTerminals";
-- JSM is in-process under the JS backend, so the eval is safe from plain IO.
loadPanes = (`catch` \(_ :: SomeException) -> return (BrowserPanes 1 M.empty)) $ do
    txt <- valToText
        =<< eval ("JSON.stringify(window.leksahDemoBrowserPanes || null)" :: Text)
    case decode' (LBS.fromStrict (encodeUtf8 txt)) of
      Just (DemoBrowserPanes nxt urls) -> return (BrowserPanes nxt urls)
      Nothing                          -> return (BrowserPanes 1 M.empty)
#else
loadPanes = (`catch` \(_ :: SomeException) -> return (BrowserPanes 1 M.empty)) $ do
    path <- panesPath
    bytes <- LBS.readFile path
    case decode' bytes of
      Just (nxt, urls) -> return (BrowserPanes nxt (M.fromList urls))
      Nothing          -> return (BrowserPanes 1 M.empty)
#endif

savePanes :: BrowserPanes -> IO ()
savePanes bp = (`catch` \(_ :: SomeException) -> return ()) $ do
    path <- panesPath
    LBS.writeFile path (encode (bpNext bp, M.toList (bpUrls bp)))

-- | Run an update against the loaded registry, writing it through to disk
-- (URL changes are address-bar-rate, so a write per update is fine).
withPanes :: (BrowserPanes -> (BrowserPanes, a)) -> IO a
withPanes f = modifyMVar browserPanesRef $ \mbp -> do
    bp <- maybe loadPanes return mbp
    let (bp', x) = f bp
    savePanes bp'
    return (Just bp', x)

-- | Mint the id for a new browser pane (monotonic, persisted, never reused —
-- so a restored tab can't collide with a freshly opened one).
nextBrowserId :: IO Int
nextBrowserId = withPanes $ \bp -> (bp { bpNext = bpNext bp + 1 }, bpNext bp)

rememberUrl :: Int -> Text -> IO ()
rememberUrl n u = withPanes $ \bp -> (bp { bpUrls = M.insert n u (bpUrls bp) }, ())

recallUrl :: Int -> IO Text
recallUrl n = withPanes $ \bp -> (bp, M.findWithDefault "" n (bpUrls bp))

--
-- The widget.
--

-- | The pane's own navigation history: back stack, current URL (@\"\"@ =
-- nothing loaded yet), forward stack.
data Hist = Hist [Text] Text [Text]

data Nav = NavTo Text | NavBack | NavForward | NavReload

stepHist :: Nav -> Hist -> Hist
stepHist (NavTo u)  (Hist b c _)      = Hist (bool (c:b) b (T.null c)) u []
stepHist NavBack    (Hist (p:b) c f)  = Hist b p (c:f)
stepHist NavForward (Hist b c (n:f))  = Hist (c:b) n f
stepHist _          h                 = h

-- | leksah's own web-UI port (@LEKSAH_PORT@, default 3367) — fixed for the
-- process lifetime, so reading the env once is sound.
{-# NOINLINE ownUiPort #-}
ownUiPort :: Int
ownUiPort = unsafePerformIO $
  fromMaybe 3367 . (>>= readMaybe) <$> lookupEnv "LEKSAH_PORT"

-- | Is @u@ leksah's OWN UI — a loopback host on 'ownUiPort'?  Loading that in
-- a browser pane nests the app inside itself: the child frame connects back
-- as a fresh jsaddle client (recursively, frame in frame), and when a nested
-- client dies it wedges the whole JS bridge.  So 'browserWidget' refuses to
-- load such URLs, and the terminal URL-click path ("IDE.Web.Main") sends them
-- to the system browser instead.
isOwnUrl :: Text -> Bool
isOwnUrl u0 =
    host `elem` ["127.0.0.1", "localhost", "0.0.0.0", "[::1]"]
      && port == Just ownUiPort
  where
    u = fromMaybe u0 $ case T.stripPrefix "http://" u0 of
          Just r  -> Just r
          Nothing -> T.stripPrefix "https://" u0
    hostPort = T.takeWhile (`notElem` ("/?#" :: String)) u
    (pre, post) = T.breakOnEnd ":" hostPort
    (host, port)
      | T.null pre = (hostPort, Nothing)
      | otherwise  = (T.dropEnd 1 pre, readMaybe (T.unpack post))

-- | What the iframe shows instead when 'isOwnUrl' refuses a page.
-- (Concatenation, not string gaps: CPP strips the backslash-newline gaps.)
ownUrlBlockedPage :: Text
ownUrlBlockedPage =
  "data:text/html;charset=utf-8,<body style=\"margin:0;height:100vh;display:flex;"
  <> "align-items:center;justify-content:center;background:%23181b20;"
  <> "color:%23848d97;font:13px -apple-system,sans-serif;text-align:center\">"
  <> "<div>This is leksah's own UI \x2014 it won't load inside itself.<br>"
  <> "Use <b>leksah-cmd open-browser URL</b> (or the system browser) to view it."
  <> "</div></body>"

-- | Does the site refuse to be embedded in a frame?  Big sites (google.com,
-- github.com, …) send @X-Frame-Options@ / CSP @frame-ancestors@, and WebKit
-- then blocks the iframe load SILENTLY — the pane just goes blank.  Fetch the
-- headers out-of-band (curl follows redirects; the Haskell side has no CORS
-- restrictions) and return the blocking header when there is one, so the pane
-- can explain itself instead.  @Nothing@ on any fetch failure too: if the
-- host is unreachable the frame shows its own failure, and a false "blocked"
-- would be worse than none.
embedBlockReason :: Text -> IO (Maybe Text)
embedBlockReason u = do
    r <- try (readProcessWithExitCode "curl"
                ["-sIL", "--max-time", "8", T.unpack u] "")
    return $ case r of
      Left (_ :: SomeException) -> Nothing
      Right (ExitSuccess, out, _) ->
        -- -IL prints one header block per redirect hop; judge the FINAL page.
        let blocks = filter (not . null) . splitBlocks . map T.strip
                       $ T.lines (T.pack out)
            hdrs   = map T.toLower (if null blocks then [] else last blocks)
            xfo    = [ h | h <- hdrs, "x-frame-options:" `T.isPrefixOf` h ]
            -- frame-ancestors that could still admit us (*, or an explicit
            -- loopback entry) don't count as blocked.
            csp    = [ h | h <- hdrs
                     , "content-security-policy:" `T.isPrefixOf` h
                     , "frame-ancestors" `T.isInfixOf` h
                     , not ("frame-ancestors *" `T.isInfixOf` h)
                     , not ("127.0.0.1" `T.isInfixOf` h) ]
        in case (xfo, csp) of
             (h:_, _) -> Just (T.strip h)
             (_, _:_) -> Just "content-security-policy: frame-ancestors"
             _        -> Nothing
      Right _ -> Nothing
  where
    splitBlocks :: [Text] -> [[Text]]
    splitBlocks = foldr step [[]]
      where step x acc@(a:as) | T.null x  = [] : acc
                              | otherwise = (x:a) : as
            step _ []                     = [[]]

-- | What the iframe shows when 'embedBlockReason' finds the site refuses
-- framing (mirrors 'ownUrlBlockedPage').  Only the sanitized host and header
-- name are interpolated into the data: URL.
frameBlockedPage :: Text -> Text -> Text
frameBlockedPage u reason =
  "data:text/html;charset=utf-8,<body style=\"margin:0;height:100vh;display:flex;"
  <> "align-items:center;justify-content:center;background:%23181b20;"
  <> "color:%23848d97;font:13px -apple-system,sans-serif;text-align:center\">"
  <> "<div><b>" <> sanitize host <> "</b> refuses to be shown inside another page<br>"
  <> "(" <> sanitize reason <> "),<br>so it cannot render in this pane.<br><br>"
  <> "Use the \x2197 toolbar button to open it in your browser.</div></body>"
  where
    host = T.takeWhile (`notElem` ("/?#" :: String))
         . fromMaybe u $ case T.stripPrefix "http://" u of
             Just r  -> Just r
             Nothing -> T.stripPrefix "https://" u
    sanitize = T.filter (`notElem` ("%<>&\"'#" :: String))

-- | Open a URL in the system's default browser (macOS @open@ / else
-- @xdg-open@) — the escape hatch for pages the pane can't embed.
openExternal :: Text -> IO ()
openExternal u = unless (T.null u) . void $
    (try (void $ createProcess (proc opener [T.unpack u]))
       :: IO (Either SomeException ()))
  where opener = if os == "darwin" then "open" else "xdg-open"

-- | Something typed into the address bar → a loadable URL: keep an explicit
-- scheme, default local-looking hosts to @http://@ and everything else to
-- @https://@.
normalizeUrl :: Text -> Text
normalizeUrl t0
  | T.null t              = t
  | hasScheme             = t
  | isLocal               = "http://" <> t
  | otherwise             = "https://" <> t
  where
    t = T.strip t0
    hasScheme = any (`T.isPrefixOf` t) ["http://", "https://", "file://", "about:"]
    isLocal   = any (`T.isPrefixOf` t) ["localhost", "127.", "0.0.0.0", "[::1]"]

browserWidget
  :: forall t m. MonadWidget t m
  => Int              -- ^ the pane's 'BrowserKey' id
  -> Event t ()       -- ^ \"take keyboard focus now\" (a tab's select pulse,
                      --   or a view leaf's reconciler pulse)
  -> Dynamic t Bool   -- ^ grab focus when created?  'True' for tabs; \"is
                      --   this the leksah window's focused leaf\" for view
                      --   leaves, so a restored background leaf can't steal
                      --   the keyboard at build time.
  -> m (Event t BrowserEvents)
browserWidget bid selectedE focusOnCreateD = do
  mops <- liftIO getNativeBrowserOps
  case mops of
    Just ops -> nativeBrowserWidget ops bid selectedE focusOnCreateD
    Nothing  -> iframeBrowserWidget bid selectedE focusOnCreateD

-- | A browser pane backed by a REAL native WKWebView overlaid on the pane
-- rect ('IDE.Web.NativeBrowser'; wkwebview front end).  The reflex side owns
-- only the chrome: the bar drives the native view through the ops, page
-- state (URL, back\/forward) is polled from the @window.__lkNb[bid]@ global
-- the native navigation delegate maintains, and the body is just the
-- @.browser-native@ placeholder whose rect the per-window reporter
-- ('IDE.Web.Main.browserNativeReporterJs') feeds to the native side.
nativeBrowserWidget
  :: forall t m. MonadWidget t m
  => NativeBrowserOps -> Int -> Event t () -> Dynamic t Bool
  -> m (Event t BrowserEvents)
nativeBrowserWidget ops bid selectedE focusOnCreateD = elClass "div" "browser" $ do
  url0 <- liftIO (recallUrl bid)
  let urlClass = "browser-url-" <> T.pack (show bid)
      -- Own-UI URLs must not load in the native view either: it would connect
      -- as a fresh jsaddle client exactly like a nested iframe.
      loadable u = if isOwnUrl u then ownUrlBlockedPage else u
  rec
    (backE, fwdE, reloadE, extE, goE, barValD) <- elClass "div" "browser-bar" $ do
      backE'   <- navButton canBackD "/pics/browser-back.svg" "Back"
      fwdE'    <- navButton canFwdD  "/pics/browser-forward.svg" "Forward"
      reloadE' <- navButton (constDyn True) "/pics/browser-reload.svg" "Reload"
      extE'    <- navButton (constDyn True) "/pics/browser-external.svg"
                    "Open in system browser"
      ti <- textInput $ (def :: TextInputConfig t)
        { _textInputConfig_initialValue = url0
        , _textInputConfig_setValue     = barSetE   -- page navigation tracks the bar
        , _textInputConfig_attributes   = constDyn
            (  "class" =: ("browser-url " <> urlClass)
            <> "placeholder" =: "Enter address (https://… or localhost:port)"
            <> "spellcheck" =: "false" <> "autocorrect" =: "off"
            <> "autocapitalize" =: "off" <> "autocomplete" =: "off" )
        }
      let enterE = () <$ ffilter (== 13) (_textInput_keypress ti)
          goE'   = fmapMaybe (\u -> let n = normalizeUrl u
                                    in if T.null n then Nothing else Just n)
                     (tagPromptlyDyn (_textInput_value ti) enterE)
      return (backE', fwdE', reloadE', extE', goE', _textInput_value ti)
    -- Poll the native view's state once a second: URL (tracks link clicks and
    -- redirects), back/forward availability, and whether the address bar has
    -- keyboard focus (never clobber mid-edit text).
    tick <- tickLossyFromPostBuildTime 1
    stateE <- performEvent $ ffor (() <$ tick) $ \_ -> liftJSM $
        valToText =<< eval
          (  "(function(){var s=(window.__lkNb||{})[" <> T.pack (show bid) <> "];"
          <> "if(!s)return '';"
          <> "var i=document.querySelector('.leksah ." <> urlClass <> "');"
          <> "var ed=(i&&document.activeElement===i)?1:0;"
          <> "return s.u+'\\t'+(s.b?1:0)+'\\t'+(s.f?1:0)+'\\t'+ed;})()" )
    let stParsedE = fmapMaybe parseNbState stateE
        pageUrlE  = fmapMaybe (\(u,_,_,_) -> if T.null u then Nothing else Just u)
                      stParsedE
    canBackD <- holdDyn False ((\(_,b,_,_) -> b) <$> stParsedE)
    canFwdD  <- holdDyn False ((\(_,_,f,_) -> f) <$> stParsedE)
    -- The pane's current URL: what the user asked for, corrected by what the
    -- page actually navigated to.
    curUrlD <- holdDyn url0 (leftmost [goE, pageUrlE])
    let changedUrlE = attachWithMaybe
          (\old (u,_,_,ed) -> if not (T.null u) && u /= old
                                then Just (u, ed) else Nothing)
          (current curUrlD) stParsedE
        -- Self-healing bar sync: every tick, if the page URL differs from
        -- what the bar SHOWS and the bar isn't focused, rewrite it.  (A
        -- one-shot on URL change could be permanently swallowed by the
        -- editing gate if the bar happened to hold focus at that moment.)
        barSetE = attachWithMaybe
          (\bar (u,_,_,ed) -> if not ed && not (T.null u) && u /= bar
                                 && not ("data:" `T.isPrefixOf` u)
                                then Just u else Nothing)
          (current barValD) stParsedE
  -- Drive the native view.
  performEvent_ $ ffor goE $ \u -> liftIO $ do
      nbLoad ops bid (loadable u)
      unless (isOwnUrl u) $ rememberUrl bid u
  performEvent_ $ liftIO (nbBack ops bid)    <$ backE
  performEvent_ $ liftIO (nbForward ops bid) <$ fwdE
  performEvent_ $ liftIO (nbReload ops bid)  <$ reloadE
  performEvent_ $ ffor (tag (current curUrlD) extE) $ liftIO . openExternal
  -- Persist real navigations (link clicks included), not just typed ones.
  performEvent_ $
      ffor (ffilter (\u -> not (isOwnUrl u || "data:" `T.isPrefixOf` u))
                    (fst <$> changedUrlE)) $
        liftIO . rememberUrl bid
  -- The restored URL: the native view is created lazily by the reporter, so
  -- this just parks the URL as pending (a beat after build, harmless).  The
  -- native view OUTLIVES widget rebuilds (resyncs and remounts rebuild the
  -- reflex network, not the native side), so only load when the view has no
  -- page yet (no __lkNb state) — a rebuild must never clobber a live page,
  -- e.g. re-loading the remembered URL over history the user navigated.
  pb <- getPostBuild
  restoreE <- delay 0.2 $
      fmapMaybe (\() -> if T.null url0 then Nothing else Just url0) pb
  performEvent_ $ ffor restoreE $ \u -> do
      has <- liftJSM $ valToText =<< eval
        ("(window.__lkNb&&window.__lkNb[" <> T.pack (show bid) <> "])?'1':''" :: Text)
      when (T.null has) . liftIO $ nbLoad ops bid (loadable u)
  -- The pane body: the placeholder the reporter measures; the native view
  -- paints over it, so the hint only shows before the first snapshot lands.
  elAttr "div" ("class" =: "browser-native" <> "data-bid" =: T.pack (show bid)) $
    elClass "div" "browser-native-hint" $
      text "This pane is a native web view — it appears once the page loads."
  -- Selecting the pane puts the keyboard where the pane's state says it
  -- belongs, and this is the ONLY place that decides it (see the focusin
  -- listener in 'IDE.Web.Main.browserNativeReporterJs'): a pane showing a page
  -- hands the keyboard to its native view — the view is a first responder of
  -- its own, so page-side DOM focus alone would leave the keys wherever they
  -- were — while a pane with nothing loaded yet wants the caret in its address
  -- bar.
  focusE <- delay 0.2 (leftmost [selectedE, gate (current focusOnCreateD) pb])
  performEvent_ $ ffor focusE $ \_ -> liftJSM . void . eval $
      "setTimeout(function(){"
      <> " var s=(window.__lkNb||{})[" <> T.pack (show bid) <> "];"
      <> " var i=document.querySelector('.leksah ." <> urlClass <> "');"
      -- A page — or an address bar already holding the URL this pane is about
      -- to show (a restored pane, whose view loads a beat later) — means the
      -- keyboard belongs to the view.  Asking before the view exists is
      -- harmless: the reporter's rising edge hands over once it does.
      <> " if ((s && s.u && s.u !== 'about:blank') || (i && i.value)) {"
      <> "   if (window.leksahBrowserFocusNative)"
      <> "     window.leksahBrowserFocusNative(" <> T.pack (show bid) <> ");"
      <> " } else if (i) i.focus();},0)"
  return never

-- | Parse the poll snapshot: @url \\t canBack \\t canFwd \\t barFocused@.
parseNbState :: Text -> Maybe (Text, Bool, Bool, Bool)
parseNbState t = case T.splitOn "\t" t of
  [u, b, f, ed] -> Just (u, b == "1", f == "1", ed == "1")
  _             -> Nothing

-- | The original iframe-backed pane: every front end without native ops
-- (warp, webkitgtk, the ghcjs demo).
iframeBrowserWidget
  :: forall t m. MonadWidget t m
  => Int -> Event t () -> Dynamic t Bool
  -> m (Event t BrowserEvents)
iframeBrowserWidget bid selectedE focusOnCreateD = elClass "div" "browser" $ do
  url0 <- liftIO (recallUrl bid)
  let urlClass = "browser-url-" <> T.pack (show bid)
  rec
    (backE, fwdE, reloadE, extE, goE) <- elClass "div" "browser-bar" $ do
      backE'   <- navButton canBackD "/pics/browser-back.svg" "Back"
      fwdE'    <- navButton canFwdD  "/pics/browser-forward.svg" "Forward"
      reloadE' <- navButton (constDyn True) "/pics/browser-reload.svg" "Reload"
      -- The escape hatch for pages that refuse to be embedded (see
      -- 'embedBlockReason'), and generally handy.
      extE'    <- navButton (constDyn True) "/pics/browser-external.svg"
                    "Open in system browser"
      ti <- textInput $ (def :: TextInputConfig t)
        { _textInputConfig_initialValue = url0
        , _textInputConfig_setValue     = loadE   -- back/forward/go track the bar
        , _textInputConfig_attributes   = constDyn
            (  "class" =: ("browser-url " <> urlClass)
            <> "placeholder" =: "Enter address (https://… or localhost:port)"
            <> "spellcheck" =: "false" <> "autocorrect" =: "off"
            <> "autocapitalize" =: "off" <> "autocomplete" =: "off" )
        }
      let enterE = () <$ ffilter (== 13) (_textInput_keypress ti)
          goE'   = fmapMaybe (\u -> let n = normalizeUrl u
                                    in if T.null n then Nothing else Just n)
                     (tagPromptlyDyn (_textInput_value ti) enterE)
      return (backE', fwdE', reloadE', extE', goE')
    let navE = leftmost [ NavTo <$> goE
                        , NavBack <$ backE
                        , NavForward <$ fwdE
                        , NavReload <$ reloadE ]
    histD <- foldDyn stepHist (Hist [] url0 []) navE
    let canBackD = (\(Hist b _ _) -> not (null b)) <$> histD
        canFwdD  = (\(Hist _ _ f) -> not (null f)) <$> histD
        -- What to (re)load after each navigation — tagPromptlyDyn samples the
        -- zipper AFTER the fold step, so this is the new current URL (and for
        -- NavReload the unchanged one: re-setting src reloads the frame).
        loadE = fmapMaybe (\(Hist _ c _) -> if T.null c then Nothing else Just c)
                  (tagPromptlyDyn histD navE)
  -- The iframe is created WITHOUT a src: a restored pane starts its page load
  -- a beat after postBuild instead of inside the initial jsaddle build batch
  -- (an external page loading mid-batch wedged the wkwebview bridge).
  (frameEl, _) <- elAttr' "iframe" ("class" =: "browser-frame") blank
  pb <- getPostBuild
  restoreLoadE <- delay 0.3 $
      fmapMaybe (\() -> if T.null url0 then Nothing else Just url0) pb
  -- Never load leksah's own UI into the frame (see 'isOwnUrl'): show the
  -- explanation page instead, and don't persist the refused URL (so a
  -- restored pane can't reintroduce it either).
  performEvent_ $ ffor (leftmost [loadE, restoreLoadE]) $ \u ->
      liftJSM $ setAttribute (_element_raw frameEl) ("src" :: Text)
                  (if isOwnUrl u then ownUrlBlockedPage else u)
  performEvent_ $ ffor (ffilter (not . isOwnUrl) loadE) $ liftIO . rememberUrl bid
  -- Sites that refuse framing (X-Frame-Options / CSP frame-ancestors) block
  -- the load SILENTLY — the frame just stays blank.  Check the headers
  -- out-of-band in parallel with the load and, when the site is refusing,
  -- swap the frame to an explanation page.  The result is dropped if the
  -- pane has navigated on in the meantime.
  curUrlD <- holdDyn url0 (leftmost [loadE, restoreLoadE])
  (blockedE, fireBlocked) <- newTriggerEvent
  -- One extra probe a beat after the restore load, and one per tab select: a
  -- trigger fired from the fork during the BOOT storm was observed to vanish
  -- (the fire ran — file-logged — but the event never propagated), so a
  -- single boot-time check isn't enough.  Re-checks are cheap (one HEAD
  -- request) and the swap below is idempotent.
  reprobeE <- delay 3 restoreLoadE
  let embeddableUrl u = not (isOwnUrl u)
                        && any (`T.isPrefixOf` u) ["http://", "https://"]
      checkE = ffilter embeddableUrl $ leftmost
        [ loadE, restoreLoadE, reprobeE
        , tag (current curUrlD) selectedE ]
  performEvent_ $ ffor checkE $ \u -> liftIO . void . forkIO $
      embedBlockReason u >>= mapM_ (\r -> fireBlocked (u, r))
  -- Only swap while the pane is still ON the checked URL (it may have
  -- navigated away while curl ran).
  let staleGuardedE = attachWithMaybe
        (\cur (u, r) -> if cur == u then Just (u, r) else Nothing)
        (current curUrlD) blockedE
  performEvent_ $ ffor staleGuardedE $ \(u, r) ->
      liftJSM $ setAttribute (_element_raw frameEl) ("src" :: Text)
                  (frameBlockedPage u r)
  -- ↗ button: this page in the system's default browser.
  performEvent_ $ ffor (tag (current curUrlD) extE) $ liftIO . openExternal
  -- Selecting the pane puts keyboard focus in it: the caret in an EMPTY
  -- address bar, else the page (the iframe) — focus is what promotes the tab
  -- and drives the active-pane shadow.  The build-time pulse is gated by
  -- focusOnCreateD (only the pane that should own the keyboard grabs it).
  -- Both pulses are delayed a beat — at postBuild (and at the open-time
  -- select, which arrives in the same frame) the input isn't attached to the
  -- document yet — and the lookup stays guarded regardless (the postBuild
  -- rule).
  focusE <- delay 0.2 (leftmost [selectedE, gate (current focusOnCreateD) pb])
  -- setTimeout: the focus (and its focusin listeners) must run OUTSIDE this
  -- frame's JS batch — a listener that re-enters jsaddle synchronously while
  -- the frame holds the spider lock deadlocks the wkwebview bridge.
  performEvent_ $ ffor focusE $ \_ -> liftJSM . void . eval $
      "setTimeout(function(){var i=document.querySelector('.leksah ." <> urlClass
      <> "'); if(!i) return; if(!i.value){ i.focus(); return; }"
      <> " var f=i.closest('.browser'); f=f&&f.querySelector('.browser-frame');"
      <> " if(f) f.focus();},0)"
  return never

navButton :: MonadWidget t m => Dynamic t Bool -> Text -> Text -> m (Event t ())
navButton enabledD src tip = do
  (e, _) <- elDynAttr' "button"
      (ffor enabledD $ \en ->
          "class" =: "browser-btn" <> "title" =: tip
          <> (if en then mempty else "disabled" =: ""))
      (elAttr "img" ("src" =: src <> "draggable" =: "false") blank)
  return $ gate (current enabledD) (domEvent Click e)

browserCss :: Css
browserCss = do
  ".browser" ? do
    "display" -: "flex"
    "flex-direction" -: "column"
    "height" -: "100%"
    "box-sizing" -: "border-box"
    "background" -: "var(--leksah-bg-sunken)"
  -- The native-view placeholder: the reporter measures this box; the real
  -- WKWebView paints over it, so its hint only shows before the first load.
  ".browser .browser-native" ? do
    "flex" -: "1 1 0"
    "min-height" -: "0"
    "position" -: "relative"
    -- Leave the pane's FIRST pixel column to the DOM.  A native view paints
    -- above everything in the page, and the active-pane ring's left border
    -- lands exactly there (see 'IDE.Web.Widget.Terminal.terminalCss') — without
    -- this inset a focused browser pane simply has no left ring line.  (Its
    -- right\/bottom lines sit on the pixel AFTER the pane, which is the
    -- neighbour's, so they are never covered.)
    "margin-left" -: "1px"
    "display" -: "flex"
    "align-items" -: "center"
    "justify-content" -: "center"
  ".browser .browser-native-hint" ? do
    "color" -: "var(--leksah-fg-dim)"
    "font-size" -: "12px"
    "text-align" -: "center"
    "padding" -: "16px"
  ".browser .browser-bar" ? do
    "flex" -: "0 0 auto"
    "display" -: "flex"
    "align-items" -: "center"
    "gap" -: "4px"
    "padding" -: "4px 6px"
  ".browser .browser-btn" ? do
    "display" -: "flex"
    "align-items" -: "center"
    "background" -: "var(--leksah-surface-alt)"
    "border" -: "1px solid var(--leksah-border-control)"
    "border-radius" -: "4px"
    "padding" -: "2px 5px"
  ".browser .browser-btn[disabled]" ? do
    "opacity" -: "0.4"
  ".browser .browser-btn img" ? do
    "width" -: "14px"
    "height" -: "14px"
    "display" -: "block"
  ".browser .browser-url" ? do
    "flex" -: "1 1 auto"
    "min-width" -: "0"
    "font-size" -: "12px"
    "color" -: "var(--leksah-fg)"
    "background" -: "var(--leksah-surface-alt)"
    "border" -: "1px solid var(--leksah-border-control)"
    "border-radius" -: "4px"
    "padding" -: "3px 8px"
  -- Pages assume a white canvas; keep it white even in the dark theme.
  ".browser .browser-frame" ? do
    "flex" -: "1 1 auto"
    "width" -: "100%"
    "border" -: "none"
    "background" -: "#fff"
