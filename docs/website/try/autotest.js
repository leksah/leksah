// Scripted smoke test for the demo page — inert unless the page is opened
// with ?autotest=1 (headless-Chrome console-log verification).  Waits for
// the UI to build, checks the Workspace tree picked up the demo project,
// then opens the sample Main.hs through window.leksahOpenFile (the reflex
// bridge the native Open dialogs use — registered by leksah once the
// network is up) and confirms CodeMirror got the file's contents.  Logs
// are prefixed AUTOTEST: so a console grep isolates them.
(function () {
  'use strict';
  if (!/[?&]autotest=1/.test(location.search)) return;
  var log = function (m) { console.log('AUTOTEST: ' + m); };
  var FILE = '/demo/breakout/app/Main.hs';
  var t0 = Date.now();
  var opened = false, treeChecked = false;

  var timer = setInterval(function () {
    if (Date.now() - t0 > 60000) {
      clearInterval(timer);
      log('TIMEOUT — treeChecked=' + treeChecked + ' opened=' + opened);
      return;
    }
    if (!treeChecked) {
      var ws = document.querySelector('.workspace');
      if (!ws) return;
      var txt = ws.textContent;
      log('workspace tree: ' + ws.querySelectorAll('li').length + ' nodes, '
          + (txt.indexOf('breakout') !== -1 ? 'has' : 'MISSING') + ' breakout');
      treeChecked = true;
      return;
    }
    if (!opened) {
      if (typeof window.leksahOpenFile !== 'function') return;
      log('calling leksahOpenFile(' + FILE + ')');
      window.leksahOpenFile(FILE);
      opened = true;
      return;
    }
    var cm = document.querySelector('.cm-editor .cm-content');
    if (cm && cm.textContent.length > 20) {
      clearInterval(timer);
      log('SUCCESS — editor has ' + cm.textContent.length + ' chars; first line: '
          + (cm.querySelector('.cm-line') || cm).textContent.slice(0, 80));
      // ⌘-held shortcut badges: the pref must be published to the page and a
      // Meta keydown must flip the body class badgesJs keys the overlay on.
      log('badges pref published: ' + window.__leksahShortcutBadges);
      window.dispatchEvent(new KeyboardEvent('keydown',
        { key: 'Meta', metaKey: true, bubbles: true }));
      log('badges class on Meta-down: '
          + document.body.classList.contains('leksah-show-badges'));
      window.dispatchEvent(new KeyboardEvent('keyup',
        { key: 'Meta', bubbles: true }));
      // Browser builds also reveal badges on Ctrl (the flip modifier there).
      window.dispatchEvent(new KeyboardEvent('keydown',
        { key: 'Control', ctrlKey: true, bubbles: true }));
      log('badges class on Control-down: '
          + document.body.classList.contains('leksah-show-badges'));
      window.dispatchEvent(new KeyboardEvent('keyup',
        { key: 'Control', bubbles: true }));
      checkTerminals();
    }
  }, 400);

  // --- Demo terminals + LSP hover stand-in ------------------------------
  var FILE_HS = '/demo/breakout/src/IDE/Web/Instance.hs';
  function checkTerminals() {
    log('demo terminals in page: '
        + (window.leksahDemoTerminals || []).length
        + '; hover files in page: '
        + Object.keys(window.leksahDemoHovers || {}).length);
    // The claude terminal tab should exist and have rendered the canned dump.
    var t0 = Date.now(), n = 0;
    var timer2 = setInterval(function () {
      var reg = window.LeksahTerm && window.LeksahTerm.byId;
      var term = reg && reg['$d0'];
      var buf = term && term.buffer && term.buffer.active;
      if (buf) {
        var txt = '';
        for (var i = 0; i < Math.min(buf.length, 200); i++) {
          var l = buf.getLine(i);
          if (l) txt += l.translateToString(true) + '\n';
        }
        if (txt.indexOf('Update(src/IDE/Web/Instance.hs)') !== -1) {
          clearInterval(timer2);
          log('claude terminal rendered with Update(Instance.hs) block');
          checkHover();
          return;
        }
      }
      if (Date.now() - t0 > 30000) {
        clearInterval(timer2);
        log('TERMINAL TIMEOUT — LeksahTerm ids: '
            + (reg ? Object.keys(reg).join(',') : 'none'));
        checkHover();
      }
    }, 500);
  }

  // Genuine round-trip through the Haskell LSP stand-in: call the hover
  // callback the terminal registered (debugHover), intercept the reply that
  // comes back through LeksahTermLinks.resolveHover, and check we got the
  // precomputed HLS text for unsafePerformIO (fixture line 35, col 18).
  function checkHover() {
    var L = window.LeksahTermLinks;
    if (!L || !L.debugHover) { log('HOVER: no debugHover seam'); flipCheck(); return; }
    var orig = L.resolveHover, got = null;
    L.resolveHover = function (rid, text) {
      if (rid === 424242) { got = text; L.resolveHover = orig; return; }
      return orig(rid, text);
    };
    L.debugHover(FILE_HS, 35, 18, 424242);
    var t0 = Date.now();
    var timer3 = setInterval(function () {
      if (got !== null) {
        clearInterval(timer3);
        log('terminal hover resolved: '
            + (got ? JSON.stringify(got.slice(0, 80)) : 'EMPTY'));
        flipCheck();
      } else if (Date.now() - t0 > 10000) {
        clearInterval(timer3);
        log('HOVER TIMEOUT — no resolveHover callback');
        flipCheck();
      }
    }, 200);
  }

  // Ctrl+` must flip tabs in the browser build (Cmd+` belongs to the OS).
  function flipCheck() {
    // The flipper cycles the CENTRE notebook's tabs; every area has its own
    // .selected tab, so scope to area-wide0 (unscoped, the first match is the
    // side pane's "Workspace", which never changes).
    function selectedTab() {
      var el = document.querySelector('.area-wide0 .tab-wrap.selected button');
      return el ? el.textContent.trim().slice(0, 30) : '(none)';
    }
    var before = selectedTab();
    var down = new KeyboardEvent('keydown',
      { key: '`', code: 'Backquote', ctrlKey: true, bubbles: true });
    Object.defineProperty(down, 'keyCode', { get: function () { return 192; } });
    document.dispatchEvent(down);
    var up = new KeyboardEvent('keyup', { key: 'Control', bubbles: true });
    Object.defineProperty(up, 'keyCode', { get: function () { return 17; } });
    setTimeout(function () {
      document.dispatchEvent(up);
      setTimeout(function () {
        log('Ctrl+` flip: "' + before + '" -> "' + selectedTab() + '"'
            + (selectedTab() === before ? ' (UNCHANGED)' : ''));
        rendererCheck();
        log('ALL CHECKS DONE');
      }, 600);
    }, 300);
  }

  // The terminal must actually PAINT, not just hold a buffer: with xterm.css
  // missing (the head-rebuild bug this guards against) .xterm-screen falls to
  // static flow far below the pane, xterm's IntersectionObserver reports it
  // off-screen and pauses rendering — buffer checks still pass, screen black.
  // So assert: css applied (screen position:relative), render service not
  // paused, screen inside its host, and visible painted output (DOM-renderer
  // row text, or a sized canvas under WebGL).
  function rendererCheck() {
    Object.keys((window.LeksahTerm || {}).byId || {}).forEach(function (id) {
      try {
        var t = window.LeksahTerm.byId[id];
        var renderer = t._core._renderService._renderer._value;
        var paused = t._core._renderService._isPaused;
        var el = t.element;
        var screen = el.querySelector('.xterm-screen');
        var hostR = el.getBoundingClientRect(), screenR = screen.getBoundingClientRect();
        var inHost = Math.abs(screenR.y - hostR.y) < hostR.height;
        var canvas = el.querySelector('.xterm-screen canvas');
        var rows = el.querySelector('.xterm-rows');
        var painted = canvas ? (canvas.width > 0 && canvas.height > 0)
                             : !!(rows && rows.textContent.trim().length > 0);
        var ok = getComputedStyle(screen).position === 'relative'
                 && !paused && inHost && painted;
        log('renderer ' + id + ': ' + (ok ? 'OK' : 'BROKEN')
            + ' kind=' + (canvas ? 'webgl' : 'dom')
            + ' cssApplied=' + (getComputedStyle(screen).position === 'relative')
            + ' paused=' + paused + ' screenInHost=' + inHost
            + ' painted=' + painted);
      } catch (e) { log('renderer ' + id + ': probe failed ' + e); }
    });
  }
})();
