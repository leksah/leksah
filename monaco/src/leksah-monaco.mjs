// Leksah's Monaco (VS Code editor control) backend, exposed as
// window.LeksahMonaco with the same editor API surface as window.LeksahCM
// (cm6/src/leksah-cm6.mjs) so the Haskell side only swaps the namespace:
//   createEditor, getDoc, setMarks, setOriginal, gotoPos,
//   setHoverHandler/resolveHover, setCompletionHandler/resolveComplete,
//   setNavHandlers, showSideBySide/showInline/hideDiff, showDiff/destroyDiff.
// The find* functions take the editor as their first argument — they are only
// called from the cm6 bundle's find-bar dispatch, never from Haskell.
//
// Workers: the Haskell loader sets window.leksahMonacoWorkerSrc (the bundled
// editor.worker source) before eval'ing this bundle; getWorker builds Blob-URL
// workers from it so the same path works in wkwebview (page not served from
// the warp origin) and in the browser front end.

import * as monaco from "monaco-editor"

let workerBlobUrl = null
self.MonacoEnvironment = {
  getWorker() {
    const src = window.leksahMonacoWorkerSrc
    if (!src) throw new Error("leksah: monaco worker source not loaded")
    if (!workerBlobUrl)
      workerBlobUrl = URL.createObjectURL(new Blob([src], { type: "text/javascript" }))
    return new Worker(workerBlobUrl, { name: "leksah-monaco" })
  },
}

// The full monaco-editor import registers ts/js/json language services whose
// specialized workers we don't ship (getWorker always returns the base editor
// worker) — turn their diagnostics off.  0.55 moved the nested namespaces
// top-level (monaco.typescript), so feature-detect both.
try {
  const ts = monaco.typescript ?? monaco.languages.typescript
  ts?.typescriptDefaults?.setDiagnosticsOptions({ noSemanticValidation: true, noSyntaxValidation: true })
  ts?.javascriptDefaults?.setDiagnosticsOptions({ noSemanticValidation: true, noSyntaxValidation: true })
} catch (_e) { /* diagnostics stay on; harmless */ }
try {
  (monaco.json ?? monaco.languages.json)?.jsonDefaults?.setDiagnosticsOptions({ validate: false })
} catch (_e) { /* ignore */ }

// ---- per-editor state -------------------------------------------------------
const viewState = new WeakMap()     // IStandaloneCodeEditor -> st
const statesByModel = new Map()     // ITextModel -> st (hover/completion provider dispatch)

// ---- languages --------------------------------------------------------------
// Monaco's basic-languages has no Haskell — a small Monarch tokenizer gives
// parity with the cm6 bundle's legacy-mode highlighting.
monaco.languages.register({ id: "haskell", extensions: [".hs", ".lhs", ".hsc"] })
monaco.languages.setLanguageConfiguration("haskell", {
  comments: { lineComment: "--", blockComment: ["{-", "-}"] },
  brackets: [["(", ")"], ["[", "]"], ["{", "}"]],
  autoClosingPairs: [
    { open: "(", close: ")" }, { open: "[", close: "]" }, { open: "{", close: "}" },
    { open: "\"", close: "\"", notIn: ["string", "comment"] },
  ],
  // Haskell identifiers include primes; qualified names complete via the
  // completion provider's own word range.
  wordPattern: /[a-zA-Z_][\w']*/,
})
monaco.languages.setMonarchTokensProvider("haskell", {
  defaultToken: "",
  keywords: ["case","class","data","default","deriving","do","else","foreign","if",
    "import","in","infix","infixl","infixr","instance","let","module","newtype","of",
    "then","type","where","forall","mdo","family","role","pattern","via","rec","proc"],
  tokenizer: {
    root: [
      [/\{-#/, "meta", "@pragma"],
      [/\{-/, "comment", "@blockComment"],
      [/--+(?![!#$%&*+.\/<=>?@\\^|~:-]).*$/, "comment"],
      [/"/, "string", "@string"],
      [/'([^'\\]|\\[^']+)'/, "string"],
      [/0[xX][0-9a-fA-F_]+|0[oO][0-7_]+|0[bB][01_]+|\d[\d_]*(\.\d+)?([eE][+-]?\d+)?/, "number"],
      [/[A-Z][\w']*/, "type.identifier"],
      [/[a-z_][\w']*/, { cases: { "@keywords": "keyword", "@default": "identifier" } }],
      [/[!#$%&*+.\/<=>?@\\^|~:-]+/, "operator"],
      [/[()\[\]{},;`]/, "delimiter"],
    ],
    blockComment: [[/\{-/, "comment", "@push"], [/-\}/, "comment", "@pop"], [/./, "comment"]],
    pragma:       [[/#-\}/, "meta", "@pop"], [/./, "meta"]],
    string:       [[/[^\\"]+/, "string"], [/\\./, "string.escape"], [/"/, "string", "@pop"]],
  },
})

// A tiny cabal highlighter (the cm6 bundle has none; cheap to provide here).
monaco.languages.register({ id: "cabal", extensions: [".cabal"] })
monaco.languages.setMonarchTokensProvider("cabal", {
  defaultToken: "",
  tokenizer: {
    root: [
      [/--.*$/, "comment"],
      [/^\s*(library|executable|test-suite|benchmark|common|flag|source-repository-package|source-repository|package|if|else)\b/, "type.identifier"],
      [/^\s*[A-Za-z][\w-]*\s*:/, "keyword"],
    ],
  },
})

const LANG_BY_EXT = { hs:"haskell", lhs:"haskell", hsc:"haskell", cabal:"cabal",
  js:"javascript", mjs:"javascript", cjs:"javascript", jsx:"javascript",
  ts:"typescript", tsx:"typescript", json:"json", css:"css", scss:"scss", less:"less",
  yaml:"yaml", yml:"yaml", nix:"plaintext", sh:"shell", bash:"shell", zsh:"shell",
  py:"python", rb:"ruby", rs:"rust", c:"c", h:"c", cpp:"cpp", hpp:"cpp", m:"objective-c",
  xml:"xml", html:"html", htm:"html", md:"markdown", sql:"sql", lua:"lua", java:"java" }
function langForFile(path) {
  const m = /\.([^.\/\\]+)$/.exec(String(path || "").toLowerCase())
  return (m && LANG_BY_EXT[m[1]]) || "plaintext"
}

// ---- theme: match the cm6 bundle's GitHub-dark palette ----------------------
monaco.editor.defineTheme("leksah-github-dark", {
  base: "vs-dark", inherit: true,
  rules: [
    { token: "comment",         foreground: "8b949e" },
    { token: "keyword",         foreground: "ff7b72" },
    { token: "string",          foreground: "a5d6ff" },
    { token: "string.escape",   foreground: "a5d6ff" },
    { token: "number",          foreground: "79c0ff" },
    { token: "type.identifier", foreground: "d2a8ff" },
    { token: "type",            foreground: "d2a8ff" },
    { token: "meta",            foreground: "8b949e" },
    { token: "operator",        foreground: "e6edf3" },
    { token: "identifier",      foreground: "e6edf3" },
  ],
  colors: {
    "editor.background": "#0d1117", "editor.foreground": "#e6edf3",
    "editorLineNumber.foreground": "#6e7681",
    "editorLineNumber.activeForeground": "#e6edf3",
    "editor.lineHighlightBackground": "#b1bac40f",
    "editor.selectionBackground": "#388bfd59",
    "editorBracketMatch.background": "#388bfd4d",
    "editorWidget.background": "#161b22", "editorWidget.border": "#30363d",
    "editorSuggestWidget.background": "#161b22",
    "editorHoverWidget.background": "#161b22", "editorHoverWidget.border": "#30363d",
  },
})

// The GitHub-light counterpart (Primer light palette), matching the cm6 bundle.
monaco.editor.defineTheme("leksah-github-light", {
  base: "vs", inherit: true,
  rules: [
    { token: "comment",         foreground: "6e7781" },
    { token: "keyword",         foreground: "cf222e" },
    { token: "string",          foreground: "0a3069" },
    { token: "string.escape",   foreground: "0a3069" },
    { token: "number",          foreground: "0550ae" },
    { token: "type.identifier", foreground: "8250df" },
    { token: "type",            foreground: "8250df" },
    { token: "meta",            foreground: "6e7781" },
    { token: "operator",        foreground: "1f2328" },
    { token: "identifier",      foreground: "1f2328" },
  ],
  colors: {
    "editor.background": "#ffffff", "editor.foreground": "#1f2328",
    "editorLineNumber.foreground": "#8c959f",
    "editorLineNumber.activeForeground": "#1f2328",
    "editor.lineHighlightBackground": "#eaeef280",
    "editor.selectionBackground": "#54aeff59",
    "editorBracketMatch.background": "#54aeff4d",
    "editorWidget.background": "#ffffff", "editorWidget.border": "#d0d7de",
    "editorSuggestWidget.background": "#ffffff",
    "editorHoverWidget.background": "#ffffff", "editorHoverWidget.border": "#d0d7de",
  },
})

// Same CSS vars the cm6 bundle reads (set from the Fonts preferences).
function fontOptions() {
  const cs = getComputedStyle(document.documentElement)
  const fam = cs.getPropertyValue("--leksah-mono").trim() || "Hasklig, Menlo, monospace"
  const sz = parseFloat(cs.getPropertyValue("--leksah-mono-size")) || 13
  return { fontFamily: fam, fontSize: sz, fontLigatures: true }
}

// ---- position conversion ----------------------------------------------------
// Leksah/CM positions are 1-based line + 0-based char; Monaco columns are
// 1-based.  Clamped like cm6's offsetOf.
function posOf(model, line, ch) {
  const ln = Math.max(1, Math.min(line, model.getLineCount()))
  const col = Math.max(0, Math.min(ch, model.getLineMaxColumn(ln) - 1)) + 1
  return new monaco.Position(ln, col)
}

// ---- dirty-line highlighting vs the git original ----------------------------
// Same line-LCS as the cm6 bundle (changedLineNumbers): 1-based current-doc
// lines inserted/modified relative to orig; deletions unmarked; big inputs
// skipped so we never build a giant DP table.
function changedLineNumbers(orig, cur) {
  const a = orig.split("\n"), b = cur.split("\n")
  const n = a.length, m = b.length
  if (n * m > 4000000) return []
  const dp = []
  for (let i = 0; i <= n; i++) dp.push(new Int32Array(m + 1))
  for (let i = n - 1; i >= 0; i--)
    for (let j = m - 1; j >= 0; j--)
      dp[i][j] = a[i] === b[j] ? dp[i + 1][j + 1] + 1 : Math.max(dp[i + 1][j], dp[i][j + 1])
  const changed = []
  let i = 0, j = 0
  while (j < m) {
    if (i < n && a[i] === b[j]) { i++; j++ }
    else if (i < n && dp[i + 1][j] >= dp[i][j + 1]) { i++ }
    else { changed.push(j + 1); j++ }
  }
  return changed
}

function refreshDirty(st) {
  if (st.original == null) { st.dirty.set([]); return }
  const model = st.model
  let lines
  if (st.original.length === 0) {
    // Untracked file: every line is new ("".split gives a phantom line).
    lines = []
    for (let i = 1; i <= model.getLineCount(); i++) lines.push(i)
  } else {
    lines = changedLineNumbers(st.original, model.getValue())
  }
  st.dirty.set(lines
    .filter(ln => ln >= 1 && ln <= model.getLineCount())
    .map(ln => ({
      range: new monaco.Range(ln, 1, ln, 1),
      options: { isWholeLine: true, className: "monaco-dirty-line",
                 linesDecorationsClassName: "monaco-dirty-gutter" },
    })))
}

// ---- LSP hover / completion (promise round trip, resolved from Haskell) -----
let hoverSeq = 0
const hoverResolvers = new Map()
let completeSeq = 0
const completeResolvers = new Map()

// LSP CompletionItemKind (1-based) -> monaco kind (same grouping as the cm6
// bundle's completionType icon mapping).
function monacoKind(kind) {
  const K = monaco.languages.CompletionItemKind
  switch (kind) {
    case 2: case 24: return K.Method
    case 3: return K.Function
    case 4: case 7: case 22: return K.Class
    case 5: case 10: return K.Property
    case 6: case 12: return K.Variable
    case 8: return K.Interface
    case 9: return K.Module
    case 13: case 20: return K.Enum
    case 14: return K.Keyword
    case 21: case 11: return K.Constant
    case 25: return K.TypeParameter
    default: return K.Variable
  }
}

const providersFor = new Set()
function ensureProviders(langId) {
  if (providersFor.has(langId)) return
  providersFor.add(langId)
  monaco.languages.registerHoverProvider(langId, {
    provideHover(model, position) {
      const st = statesByModel.get(model)
      if (!st || !st.onHover) return null
      const id = ++hoverSeq
      const p = new Promise(resolve => {
        hoverResolvers.set(id, resolve)
        setTimeout(() => { if (hoverResolvers.delete(id)) resolve(null) }, 4000)
      })
      st.onHover(id, position.lineNumber - 1, position.column - 1)
      // Monaco renders IMarkdownString natively (the cm6 bundle needs its own
      // markdown-lite renderer).
      return p.then(text => text ? { contents: [{ value: String(text) }] } : null)
    },
  })
  monaco.languages.registerCompletionItemProvider(langId, {
    triggerCharacters: ["."],
    async provideCompletionItems(model, position) {
      const st = statesByModel.get(model)
      if (!st || !st.onComplete) return { suggestions: [] }
      const id = ++completeSeq
      const items = await new Promise(resolve => {
        completeResolvers.set(id, resolve)
        setTimeout(() => { if (completeResolvers.delete(id)) resolve([]) }, 4000)
        st.onComplete(id, position.lineNumber - 1, position.column - 1)
      })
      if (!items || !items.length) return { suggestions: [] }
      const w = model.getWordUntilPosition(position)
      const range = new monaco.Range(position.lineNumber, w.startColumn,
                                     position.lineNumber, w.endColumn)
      return { suggestions: items.map(it => ({
        label: it.label,
        detail: it.detail || undefined,
        kind: monacoKind(it.kind),
        insertText: it.apply || it.label,
        range })) }
    },
  })
}

function resolveHover(id, text) {
  const r = hoverResolvers.get(id)
  if (r) { hoverResolvers.delete(id); r(text) }
}
function resolveComplete(id, itemsJson) {
  const r = completeResolvers.get(id)
  if (!r) return
  completeResolvers.delete(id)
  let items = []
  try { items = JSON.parse(itemsJson) } catch (_e) { items = [] }
  r(items)
}
function setHoverHandler(view, onHover) {
  const st = viewState.get(view); if (st) st.onHover = onHover
}
function setCompletionHandler(view, onComplete) {
  const st = viewState.get(view); if (st) st.onComplete = onComplete
}
function setNavHandlers(view, onDefinition, onReferences) {
  const st = viewState.get(view)
  if (st) { st.onDefinition = onDefinition; st.onReferences = onReferences }
}
function navAt(st, which, position) {
  const cb = st[which]
  if (cb && position) cb(position.lineNumber - 1, position.column - 1)
}

// ---- createEditor -----------------------------------------------------------
// Same contract as LeksahCM.createEditor: parent is the reflex-owned
// div.editor[data-file=…] raw element (never querySelector'd at postBuild),
// returns the editor handle (it has .focus()).
function createEditor(parent, doc, onChange, onGutterMenu) {
  const file = parent && parent.getAttribute && parent.getAttribute("data-file")
  const langId = langForFile(file)
  // Inner node so the diff views can swap in/out without fighting Monaco's
  // inline styles on the reflex-owned parent.
  const edNode = document.createElement("div")
  edNode.style.cssText = "width:100%;height:100%"
  parent.appendChild(edNode)
  const model = monaco.editor.createModel(doc, langId)
  const editor = monaco.editor.create(edNode, {
    model,
    theme: "leksah-github-dark",
    automaticLayout: true,
    ...fontOptions(),
    minimap: { enabled: false },       // parity with the CM editor (no minimap)
    contextmenu: false,                // content right-click = browser menu, like CM;
                                       // the gutter menu below is Leksah's own
    fixedOverflowWidgets: true,
    "semanticHighlighting.enabled": false,
  })
  editor.__leksahMonaco = true         // marker for the cm6 bundle's dispatch
  // Monaco's theme is global; a freshly-created editor would otherwise sit on
  // whatever `theme:` above named.  Re-apply the OS-appropriate theme (no-op if
  // leksahRetheme isn't wired yet — the startup call covers that case).
  if (window.leksahRetheme) window.leksahRetheme()
  const st = { parent, edNode, editor, model, langId,
               original: null, diff: null,
               marks: editor.createDecorationsCollection(),
               dirty: editor.createDecorationsCollection(),
               findDeco: editor.createDecorationsCollection(),
               find: null,
               onGutterMenu, onHover: null, onComplete: null,
               onDefinition: null, onReferences: null, dirtyTimer: 0 }
  viewState.set(editor, st)
  statesByModel.set(model, st)
  ensureProviders(langId)
  model.onDidChangeContent(() => {
    if (onChange) onChange()
    clearTimeout(st.dirtyTimer)
    st.dirtyTimer = setTimeout(() => refreshDirty(st), 300)
  })
  hookEditorEvents(st, editor)
  // The find bar / AI-send helpers track the focused editor through the shared
  // LeksahCM.activeView slot; __leksahMonaco distinguishes the handle type.
  const track = () => { if (window.LeksahCM) window.LeksahCM.activeView = editor }
  parent.addEventListener("focusin", track)
  track()
  return editor
}

// Gutter context menu + LSP navigation, attached to the plain editor and to a
// diff view's modified editor (so "Hide original" stays reachable).
function hookEditorEvents(st, editor) {
  editor.onContextMenu(e => {
    const T = monaco.editor.MouseTargetType
    if (e.target && (e.target.type === T.GUTTER_LINE_NUMBERS ||
                     e.target.type === T.GUTTER_GLYPH_MARGIN ||
                     e.target.type === T.GUTTER_LINE_DECORATIONS)) {
      e.event.preventDefault()
      e.event.stopPropagation()
      const be = e.event.browserEvent
      if (st.onGutterMenu && be) st.onGutterMenu(be.clientX, be.clientY)
    }
  })
  editor.addCommand(monaco.KeyCode.F12,
    () => navAt(st, "onDefinition", editor.getPosition()))
  editor.addCommand(monaco.KeyMod.Shift | monaco.KeyCode.F12,
    () => navAt(st, "onReferences", editor.getPosition()))
  editor.onMouseDown(e => {
    if ((e.event.metaKey || e.event.ctrlKey) && e.event.leftButton &&
        e.target && e.target.position && st.onDefinition) {
      e.event.preventDefault()
      navAt(st, "onDefinition", e.target.position)
    }
  })
}

function getDoc(view) {
  const st = viewState.get(view)
  return st ? st.model.getValue() : ""
}

// setMarks(view, [{fromLine,fromCh,toLine,toCh,cls}]) — error/warning/lint
// underline decorations; cls is the LogRef type whose CSS already exists
// (.ErrorRef/.WarningRef/.LintRef).  Decorations (not setModelMarkers): exact
// visual parity, no competing marker hovers next to the Haskell LSP hover.
function setMarks(view, marks) {
  const st = viewState.get(view); if (!st) return
  const decos = []
  for (const m of Array.from(marks)) {
    const a = posOf(st.model, m.fromLine, m.fromCh)
    const b = posOf(st.model, m.toLine, m.toCh)
    if (b.lineNumber > a.lineNumber || b.column > a.column)
      decos.push({ range: new monaco.Range(a.lineNumber, a.column, b.lineNumber, b.column),
                   options: { inlineClassName: m.cls,
                              stickiness: monaco.editor.TrackedRangeStickiness.NeverGrowsWhenTypingAtEdges } })
  }
  st.marks.set(decos)
}

function setOriginal(view, orig) {
  const st = viewState.get(view); if (!st) return
  st.original = orig
  refreshDirty(st)
}

function gotoPos(view, line, ch) {
  const st = viewState.get(view); if (!st) return
  const p = posOf(st.model, line, ch)
  view.setPosition(p)
  view.revealPositionInCenter(p)
  view.focus()
}

// ---- diff views against the git original (gutter context menu) --------------
// The diff editor shares the LIVE modified model, so edits made in the diff
// persist and getDoc/save/marks keep working while it is shown.
function showDiffView(view, sideBySide) {
  const st = viewState.get(view); if (!st) return
  if (st.diff) {
    st.diff.diffEditor.updateOptions({ renderSideBySide: sideBySide })
    return
  }
  const vr = view.getVisibleRanges()
  const topLine = vr.length ? vr[0].startLineNumber : 1
  st.edNode.style.display = "none"
  const node = document.createElement("div")
  node.style.cssText = "width:100%;height:100%"
  st.parent.appendChild(node)
  const origModel = monaco.editor.createModel(st.original ?? "", st.langId)
  const diffEditor = monaco.editor.createDiffEditor(node, {
    theme: "leksah-github-dark",
    automaticLayout: true,
    ...fontOptions(),
    renderSideBySide: sideBySide,
    originalEditable: false,
    minimap: { enabled: false },
    contextmenu: false,
    fixedOverflowWidgets: true,
  })
  diffEditor.setModel({ original: origModel, modified: st.model })
  hookEditorEvents(st, diffEditor.getModifiedEditor())
  st.diff = { node, diffEditor, origModel }
  requestAnimationFrame(() => diffEditor.getModifiedEditor().revealLineNearTop(topLine))
}
function showSideBySide(view) { showDiffView(view, true) }
function showInline(view) { showDiffView(view, false) }
function hideDiff(view) {
  const st = viewState.get(view); if (!st || !st.diff) return
  const vr = st.diff.diffEditor.getModifiedEditor().getVisibleRanges()
  const line = vr.length ? vr[0].startLineNumber : 1
  st.diff.diffEditor.dispose()
  st.diff.origModel.dispose()
  st.diff.node.remove()
  st.diff = null
  st.edNode.style.removeProperty("display")
  view.layout()
  view.revealLineNearTop(line)
}

// ---- find/replace (driven from the cm6 bundle's find-bar dispatch) ----------
// Same flags bitmask as the cm6 bundle: 1 = case sensitive, 2 = whole word,
// 4 = regexp.  Selection is moved to matches but focus is never taken, so
// typing in the find box keeps working.
function findMatches(st, searchText, flags) {
  if (!searchText) return []
  try {
    return st.model
      .findMatches(searchText, false, (flags & 4) !== 0, (flags & 1) !== 0,
                   (flags & 2) !== 0 ? " \t()[]{}<>`'\"-/;:,.!?" : null, false)
      .map(m => m.range)
  } catch (_e) { return [] }   // invalid regexp
}
function highlightFind(st) {
  st.findDeco.set(st.find.matches.map((r, i) => ({
    range: r,
    options: { inlineClassName: i === st.find.idx ? "monaco-leksah-find-active"
                                                  : "monaco-leksah-find" },
  })))
}
function findSet(view, searchText, replace, flags) {
  const st = viewState.get(view); if (!st) return
  st.find = { searchText, replace, flags, matches: findMatches(st, searchText, flags), idx: -1 }
  highlightFind(st)
  if (st.find.matches.length) {
    // Reveal the first match at/after the cursor without moving it.
    const pos = view.getPosition() || new monaco.Position(1, 1)
    let i = st.find.matches.findIndex(r => pos.isBeforeOrEqual(r.getStartPosition()))
    if (i < 0) i = 0
    view.revealRangeInCenter(st.find.matches[i])
  }
}
function findStep(view, dir) {
  const st = viewState.get(view); if (!st || !st.find) return
  const ms = st.find.matches, n = ms.length; if (!n) return
  let idx = st.find.idx
  if (idx < 0) {
    const pos = view.getPosition() || new monaco.Position(1, 1)
    if (dir > 0) {
      idx = ms.findIndex(r => pos.isBeforeOrEqual(r.getStartPosition()))
      if (idx < 0) idx = 0
    } else {
      idx = 0
      for (let i = n - 1; i >= 0; i--)
        if (ms[i].getEndPosition().isBeforeOrEqual(pos)) { idx = i; break }
    }
  } else idx = (idx + dir + n) % n
  st.find.idx = idx
  view.setSelection(ms[idx])
  view.revealRangeInCenter(ms[idx])
  highlightFind(st)
}
function recount(view, st) {
  st.find.matches = findMatches(st, st.find.searchText, st.find.flags)
  if (st.find.idx >= st.find.matches.length) st.find.idx = -1
  highlightFind(st)
}
function replaceNext(view) {
  const st = viewState.get(view); if (!st || !st.find || !st.find.matches.length) return
  const sel = view.getSelection()
  let i = sel ? st.find.matches.findIndex(r => r.equalsRange(sel)) : -1
  if (i < 0 && sel) i = st.find.matches.findIndex(r => sel.getStartPosition().isBeforeOrEqual(r.getStartPosition()))
  if (i < 0) i = 0
  view.executeEdits("leksah-find",
    [{ range: st.find.matches[i], text: st.find.replace || "" }])
  recount(view, st)
  findStep(view, 1)
}
function replaceAll(view) {
  const st = viewState.get(view); if (!st || !st.find || !st.find.matches.length) return
  view.executeEdits("leksah-find",
    st.find.matches.map(r => ({ range: r, text: st.find.replace || "" })))
  recount(view, st)
}

// ---- standalone read-only diff (the git log viewer's file diff) --------------
function showDiff(parent, filePath, oldDoc, newDoc) {
  destroyDiff(parent)
  const langId = langForFile(filePath)
  const diffEditor = monaco.editor.createDiffEditor(parent, {
    theme: "leksah-github-dark",
    automaticLayout: true,
    ...fontOptions(),
    readOnly: true,
    originalEditable: false,
    renderSideBySide: true,
    minimap: { enabled: false },
    contextmenu: false,
  })
  const a = monaco.editor.createModel(oldDoc, langId)
  const b = monaco.editor.createModel(newDoc, langId)
  diffEditor.setModel({ original: a, modified: b })
  parent.__leksahMonacoDiff = { diffEditor, a, b }
  return diffEditor
}
function destroyDiff(parent) {
  const d = parent && parent.__leksahMonacoDiff
  if (d) {
    d.diffEditor.dispose()
    d.a.dispose()
    d.b.dispose()
    parent.__leksahMonacoDiff = null
  }
}

window.LeksahMonaco = {
  monaco,
  createEditor, getDoc, setMarks, setOriginal, gotoPos,
  showSideBySide, showInline, hideDiff, showDiff, destroyDiff,
  setHoverHandler, resolveHover, setCompletionHandler, resolveComplete, setNavHandlers,
  findSet,
  findNext: v => findStep(v, 1),
  findPrev: v => findStep(v, -1),
  replaceNext, replaceAll,
}
