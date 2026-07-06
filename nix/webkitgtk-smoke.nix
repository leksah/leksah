# Headless runtime smoke test for leksah-webkitgtk: launch the full IDE
# under Xvfb and assert its warp server comes up (proves the GTK4 window,
# the WebKitGTK 6.0 WebView and the jsaddle wiring all started).
#
# The environment exists to make WebKitGTK viable inside the nix build
# sandbox: no /etc/dbus-1 (point dbus at its store config), no bubblewrap
# (WebKit's own sandbox can't nest), and no /run/opengl-driver (point
# glvnd/mesa loaders at the store and software-render with llvmpipe).
{ lib, runCommand, xvfb-run, dbus, curl, gsettings-desktop-schemas, gtk4, mesa
, leksah-webkitgtk, leksah-cmd, leksah-src }:
let
  mesaDrivers = mesa.drivers or mesa;
in
runCommand "leksah-webkitgtk-smoke" {
  nativeBuildInputs = [ xvfb-run dbus curl leksah-webkitgtk leksah-cmd ];
} ''
  export HOME=$TMPDIR
  export XDG_RUNTIME_DIR=$TMPDIR
  export WEBKIT_DISABLE_SANDBOX_THIS_IS_DANGEROUS=1
  export WEBKIT_DISABLE_DMABUF_RENDERER=1
  export WEBKIT_DISABLE_COMPOSITING_MODE=1
  export GDK_BACKEND=x11
  export GTK_A11Y=none
  export GSK_RENDERER=cairo
  export LIBGL_ALWAYS_SOFTWARE=1
  export GALLIUM_DRIVER=llvmpipe
  export __EGL_VENDOR_LIBRARY_DIRS=${mesaDrivers}/share/glvnd/egl_vendor.d
  export LIBGL_DRIVERS_PATH=${mesaDrivers}/lib/dri
  export GBM_BACKENDS_PATH=${mesaDrivers}/lib/gbm
  # Data files (xterm/codemirror bundles, prefs, pics) come straight from
  # the source tree, like leksah-nix.sh's dev loop.
  export leksah_datadir=${leksah-src}

  xvfb-run -a -s "-screen 0 1280x800x24" \
    dbus-run-session --config-file=${dbus}/share/dbus-1/session.conf -- \
    leksah-webkitgtk > log 2>&1 &
  pid=$!

  ok=""
  for i in $(seq 1 90); do
    if curl -sf http://127.0.0.1:3367/jsaddle.js > /dev/null; then ok=1; break; fi
    if ! kill -0 $pid 2> /dev/null; then break; fi
    sleep 1
  done
  # A JS round trip through the control socket proves the whole stack: GTK
  # window, WebKit WebView, the jsaddle bridge and the built page.
  if [ -n "$ok" ]; then
    ok=""
    for i in $(seq 1 60); do
      if [ "$(leksah-cmd js eval "6 * 7" 2> /dev/null)" = "42" ]; then ok=1; break; fi
      if ! kill -0 $pid 2> /dev/null; then break; fi
      sleep 1
    done
  fi
  if [ -n "$ok" ]; then
    echo "SMOKE-OK: warp answered and js eval round-tripped" | tee -a log
  else
    echo "SMOKE-FAIL — app output follows:"
    cat log
    echo "SMOKE-FAIL" >> log
  fi
  kill $pid 2> /dev/null || true
  cp log $out
  grep -q "SMOKE-OK" $out
''
