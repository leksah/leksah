{-# LANGUAGE OverloadedStrings #-}
-- | The JS side of the tunnel: a self-contained HTML page the IDE loads
-- into a per-pane iframe.  It embeds jsaddle's stock JS runtime
-- ('Language.Javascript.JSaddle.Run.Files') wired to @window.postMessage@:
--
-- * incoming batches arrive as @{leksahJsaddleBatch: key, batch: json}@
--   messages from the parent (the IDE page);
-- * async results go back as @{leksahJsaddle: key, type:'results',
--   seq: n, data: json}@ — @seq@ lets the IDE restore order (its jsaddle
--   callbacks run on forked threads);
-- * sync callbacks use a __synchronous__ XHR to the IDE's own local web
--   server (same origin), which answers by round-tripping over the pane's
--   stdin/stdout — the app machine itself needs no reachable socket;
-- * @{type:'ready'}@ is posted once the runtime is installed, and the IDE
--   only then ACKs the exe's HELLO.
module Language.Javascript.JSaddle.Terminal.Bootstrap
  ( bootstrapHtml
  , bootstrapJs
  , syncPath
  , framePath
  ) where

import Data.ByteString.Lazy (ByteString)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

-- | URL path (on the IDE's own server) answering sync XHRs for a pane key.
syncPath :: ByteString -> ByteString
syncPath key = "/jsaddle-terminal/sync/" <> key

-- | URL path (on the IDE's own server) serving 'bootstrapHtml' for a key.
framePath :: ByteString -> ByteString
framePath key = "/jsaddle-terminal/frame/" <> key

-- | The iframe page for pane @key@ (already URL/JS-safe: keys are made of
-- @[A-Za-z0-9._-]@ by the IDE's sanitiser).
bootstrapHtml :: ByteString -> ByteString
bootstrapHtml key =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \<head>\n\
  \<meta charset=\"utf-8\">\n\
  \<title>jsaddle-terminal</title>\n\
  \<style>html,body{margin:0;padding:0;width:100%;height:100%;\
  \background:rgb(16,16,16);color:#fff;\
  \font-family:-apple-system,BlinkMacSystemFont,sans-serif}</style>\n\
  \</head>\n\
  \<body>\n\
  \<script>\n" <> bootstrapJs key <> "</script>\n\
  \</body>\n\
  \</html>\n"

-- | The runtime script for pane @key@.
bootstrapJs :: ByteString -> ByteString
bootstrapJs key =
  ghcjsHelpers <>
  "var jstermKey = '" <> key <> "';\n\
  \var jstermSeq = 0;\n\
  \function jstermSend(r){\n\
  \  parent.postMessage({leksahJsaddle: jstermKey, type: 'results',\n\
  \                      seq: jstermSeq++, data: JSON.stringify(r)}, '*');\n\
  \}\n\
  \function jstermSendSync(r){\n\
  \  var xhr = new XMLHttpRequest();\n\
  \  xhr.open('POST', '" <> syncPath key <> "', false);\n\
  \  xhr.send(JSON.stringify(r));\n\
  \  if (xhr.status !== 200) { throw new Error('jsaddle-terminal sync failed: ' + xhr.status); }\n\
  \  return JSON.parse(xhr.responseText);\n\
  \}\n\
  \window.runJSaddleBatch = (function() {\n" <>
  initState <>
  "\n return function(batch) {\n" <>
  runBatch (\a -> "jstermSend(" <> a <> ");")
           (Just (\a -> "jstermSendSync(" <> a <> ")")) <>
  "\n };\n\
  \})();\n\
  \window.addEventListener('message', function(e){\n\
  \  if (e.data && e.data.leksahJsaddleBatch === jstermKey) {\n\
  \    runJSaddleBatch(JSON.parse(e.data.batch));\n\
  \  }\n\
  \});\n\
  \function jstermFocused(){\n\
  \  parent.postMessage({leksahJsaddle: jstermKey, type: 'focus'}, '*');\n\
  \}\n\
  \window.addEventListener('focus', jstermFocused);\n\
  \window.addEventListener('mousedown', jstermFocused, true);\n\
  \\n\
  \// Forward modifier-key chords to the host so its global keymap (build,\n\
  \// find, the ⌃` flipper, ⌘-navigation, …) works while this pane has focus —\n\
  \// keyboard events don't cross the frame boundary on their own.  Plain keys\n\
  \// (no modifier) stay in the app; we never preventDefault, so the app's own\n\
  \// ⌘C/⌘V etc. keep working (the host acts only on chords it recognises).\n\
  \function jstermFwdKey(kind, e){\n\
  \  if (e.ctrlKey || e.metaKey || e.altKey ||\n\
  \      e.key === 'Control' || e.key === 'Meta' || e.key === 'Alt') {\n\
  \    parent.postMessage({leksahJsaddle: jstermKey, type: 'key', kind: kind,\n\
  \      keyCode: e.keyCode, key: e.key, code: e.code,\n\
  \      ctrl: e.ctrlKey, shift: e.shiftKey, alt: e.altKey, meta: e.metaKey}, '*');\n\
  \  }\n\
  \}\n\
  \window.addEventListener('keydown', function(e){ jstermFwdKey('keydown', e); }, true);\n\
  \window.addEventListener('keyup',   function(e){ jstermFwdKey('keyup', e); }, true);\n\
  \parent.postMessage({leksahJsaddle: jstermKey, type: 'ready'}, '*');\n"
