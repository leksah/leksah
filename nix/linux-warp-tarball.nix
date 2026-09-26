# The zero-dependency Linux download: a statically-linked leksah-warp plus its
# data files.  Extract, run bin/leksah-warp, open http://127.0.0.1:3367/.
#
# This is the sibling of nix/linux-tarball.nix and they are deliberately NOT
# the same shape, because the two front ends have opposite runtime needs:
#
#   * exe:leksah is GTK4 + WebKitGTK.  That stack cannot be statically linked
#     (it dlopen()s codecs and GIO modules and spawns its own helper
#     processes), so that tarball ships the whole closure and binds it onto
#     /nix/store with bubblewrap — which costs a kernel feature (unprivileged
#     user namespaces) and a large download.
#   * exe:leksah-warp serves the same IDE over HTTP and links no GUI library
#     at all, so on musl it links fully static.  A static binary has NO runtime
#     closure: nothing to relocate, nothing to bind, nothing to detect.  That
#     makes this the artifact for servers, containers, and any machine where
#     the namespace trick is not allowed.
#
# The binary is UNWRAPPED.  On other platforms nix/hix.nix wraps bin/leksah-warp
# with a --prefix PATH pointing at the nix GHC and cabal; that wrapper would turn
# a self-contained binary back into one with a multi-GB store closure, so hix.nix
# skips it for musl (it also cannot be evaluated there — see the note beside
# `isMusl` in that file).  Leksah picks ghc/cabal up from the user's PATH
# instead.  The `.leksah-warp-wrapped` branch below is kept only so this
# derivation still does the right thing if that ever changes.
#
# Data files are found via the leksah_datadir environment variable
# (leksah/src/IDE/Paths.hs) — the compiled-in Paths_leksah fallback points at a
# /nix/store path that will not exist on the target — so bin/leksah-warp is a
# two-line sh launcher that sets it relative to its own location.
{ pkgs
, lib ? pkgs.lib
, leksah-warp            # musl cross exe (x86_64-unknown-linux-musl:…:leksah-warp)
, src
, version
}:

let
  tree = "leksah-warp-${version}";

  launcher = ''
    #!/bin/sh
    # Leksah (warp front end).  Serves the IDE over HTTP; open the URL it
    # prints, by default http://127.0.0.1:3367/.
    set -eu
    self=$(readlink -f "$0" 2>/dev/null || echo "$0")
    root=$(cd "$(dirname "$self")/.." && pwd)
    leksah_datadir="$root/share/leksah"
    export leksah_datadir
    exec "$root/libexec/leksah-warp" "$@"
  '';

in
pkgs.runCommand "leksah-warp-linux-tarball"
  {
    nativeBuildInputs = [ pkgs.gnutar pkgs.gzip pkgs.binutils ];
    meta = {
      description = "Statically-linked Leksah (warp front end) for x86_64 Linux";
      platforms = [ "x86_64-linux" ];
    };
  }
  ''
    set -euo pipefail
    mkdir -p ${tree}/bin ${tree}/libexec ${tree}/share/leksah

    # --- the binary, without hix.nix's PATH wrapper (see the header note) ---
    if [ -e ${leksah-warp}/bin/.leksah-warp-wrapped ]; then
      cp ${leksah-warp}/bin/.leksah-warp-wrapped ${tree}/libexec/leksah-warp
    else
      cp ${leksah-warp}/bin/leksah-warp ${tree}/libexec/leksah-warp
    fi
    chmod u+w ${tree}/libexec/leksah-warp
    strip ${tree}/libexec/leksah-warp || true

    # --- prove it really is static ---
    # The entire premise of this artifact is "no runtime dependencies".  A
    # dynamically-linked binary here would still run on the BUILDER (its
    # interpreter and libs are in the store) and fail only on a user's machine,
    # so assert it at build time rather than discover it in a bug report.
    if readelf -l ${tree}/libexec/leksah-warp | grep -q INTERP; then
      echo "ERROR: leksah-warp is dynamically linked — it has a PT_INTERP segment." >&2
      echo "The musl target is supposed to link it statically (enableStatic in" >&2
      echo "nix/hix.nix).  Interpreter and shared libraries wanted:" >&2
      readelf -l ${tree}/libexec/leksah-warp | grep -A1 INTERP >&2 || true
      readelf -d ${tree}/libexec/leksah-warp | grep NEEDED >&2 || true
      exit 1
    fi
    echo "static binary confirmed (no PT_INTERP)"

    # Belt and braces: a static binary must also want no shared libraries.
    if readelf -d ${tree}/libexec/leksah-warp 2>/dev/null | grep -q NEEDED; then
      echo "ERROR: leksah-warp has DT_NEEDED entries — it is not fully static:" >&2
      readelf -d ${tree}/libexec/leksah-warp | grep NEEDED >&2
      exit 1
    fi
    echo "no DT_NEEDED entries confirmed"

    # Report any /nix/store paths compiled into the binary.  These are expected
    # and inert: every cabal build bakes the Paths_<pkg> directory constants in,
    # and leksah does not consult them — the launcher sets leksah_datadir, and
    # IDE.Paths prefers that and then the exe-relative layout.  Listed so that a
    # regression which starts *reading* one is visible in the build log rather
    # than discovered on a machine that has no /nix/store.
    echo "--- store paths compiled into the binary (inert Paths_* constants) ---"
    LC_ALL=C grep -a -o '/nix/store/[[:print:]]\{0,100\}' ${tree}/libexec/leksah-warp \
      | sed 's|\(/nix/store/[a-z0-9]*-[^/]*\).*|\1|' | sort -u || true

    # --- launcher ---
    cp ${builtins.toFile "leksah-warp-launcher.sh" launcher} ${tree}/bin/leksah-warp
    chmod +x ${tree}/bin/leksah-warp

    # --- datadir (the set the app reads at run time) ---
    cp -r ${src}/leksah/pics   ${tree}/share/leksah/pics
    cp -r ${src}/leksah/cm6    ${tree}/share/leksah/cm6
    cp -r ${src}/leksah/xterm  ${tree}/share/leksah/xterm
    cp -r ${src}/leksah/fonts  ${tree}/share/leksah/fonts
    cp    ${src}/LICENSE       ${tree}/share/leksah/LICENSE
    cp    ${src}/Readme.md     ${tree}/share/leksah/Readme.md
    chmod -R u+w ${tree}/share/leksah

    cat > ${tree}/README <<README
    Leksah ${version} — x86_64 Linux (warp front end, statically linked)

      ./bin/leksah-warp

    then open http://127.0.0.1:3367/ in any browser.

    The binary is fully static: no libc, no GTK, no distro packages and no
    kernel features required.  It runs the same IDE as the desktop build,
    rendered in a browser tab instead of a native window — which also means you
    can run it on a server and use it from another machine.

      LEKSAH_PORT=4000 ./bin/leksah-warp     # a different port

    Move or rename the directory freely, but keep bin/, libexec/ and share/
    together — the launcher finds its data files relative to itself.

    ghc and cabal are NOT bundled; Leksah uses whatever is on your PATH.

    If you want the native desktop application instead, download the GTK4
    build (leksah-${version}-x86_64-linux.tar.gz).

    Source and build instructions: https://github.com/leksah/leksah
    README

    mkdir -p $out
    tar --sort=name --owner=0 --group=0 --numeric-owner \
        --mtime='@1' -czf $out/${tree}-x86_64-linux-musl.tar.gz ${tree}
  ''
