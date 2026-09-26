# A download-and-run Leksah for x86_64 Linux: a .tar.gz that needs no nix, no
# root and no distro packages.  Extract it anywhere and run bin/leksah.
#
# Why this is shaped differently from the macOS .app
# --------------------------------------------------
# nix/macos-app.nix relocates by REWRITING the binary: the non-system dylib
# closure is copied into Contents/Frameworks and every load command repointed
# at @executable_path.  That works there because a Mac already provides all the
# GUI frameworks, so only a handful of dylibs move.
#
# The Linux front end is GTK4 + WebKitGTK 6.0, and the same trick does not
# survive contact with it:
#
#   * The ELF interpreter is an ABSOLUTE path baked into the header and the
#     kernel does not expand $ORIGIN in it, so a relocated tree cannot even
#     start without invoking the loader by hand.
#   * WebKit exec()s its OWN helper processes (WebKitWebProcess,
#     WebKitNetworkProcess); they are not launched through our entry point, so
#     a launcher-script fix-up never reaches them.
#   * GTK resolves far more than shared objects by absolute path — the
#     gdk-pixbuf loaders cache, the GIO module cache, compiled GSettings
#     schemas and icon themes all contain store paths in their own file
#     formats, not in anything patchelf understands.
#
# So instead of moving the files, we keep them where they were built and make
# that location exist at run time: the tarball carries the runtime closure in
# store/, and the launcher uses a statically-linked bubblewrap to bind it onto
# /nix/store inside a mount namespace.  Nothing is patched, so everything
# resolves exactly as it did in the build.  This is the same approach
# nix-portable and nix-bundle take, and the reason they work on GTK apps.
#
# The cost is one runtime requirement: unprivileged user namespaces, which are
# on by default on current Ubuntu/Debian/Fedora/Arch/NixOS.  The launcher
# checks for them and explains itself if they are missing, rather than dying
# with a bare namespace errno.
#
# Note it bundles the UNWRAPPED executable.  nix/hix.nix wraps bin/leksah with
# a --prefix PATH that puts the nix GHC and cabal in front; keeping that would
# drag the whole toolchain (several GB) into a download whose user already has
# their own ghc and cabal.  Unwrapped, the IDE picks those up from PATH like
# any other install, and the launcher supplies the GUI environment the wrapper
# would otherwise have set.
{ pkgs
, lib ? pkgs.lib
, leksah                 # exe derivation (…leksah:exe:leksah)
, src
, version
}:

let
  inherit (pkgs) gtk4 webkitgtk_6_0 gsettings-desktop-schemas adwaita-icon-theme
                 hicolor-icon-theme shared-mime-info glib-networking dconf
                 gdk-pixbuf librsvg glib fontconfig mesa;
  # A minimal locale archive (~4MB), NOT glibcLocalesUtf8: the full UTF-8
  # archive is ~500MB uncompressed and doubled the download.  en_US.UTF-8
  # silences the "Locale not supported" warnings; C.UTF-8 is the fallback.
  glibcLocalesUtf8 = pkgs.glibcLocales.override {
    allLocales = false;
    locales = [ "en_US.UTF-8/UTF-8" "C.UTF-8/UTF-8" ];
  };

  tree = "leksah-${version}";

  # The executable without hix.nix's PATH wrapper (see the header note).
  # Copying the ELF keeps its embedded store paths, so nix's reference scanner
  # still finds the whole shared-object closure from here.
  #
  # ...too much of it, unless trimmed.  The exe is statically linked against
  # its Haskell libraries, and each library's Paths_<pkg> module compiles in
  # that library's own store path as a string constant.  The scanner cannot
  # tell a constant from a dependency, so those paths — and through them every
  # Haskell library's build output (.a archives, sources, ghc-internal, gcc) —
  # land in the closure: 3.5 GB unpacked, against 1.3 GB without them.  Nothing
  # reads them at run time (IDE.Paths prefers leksah_datadir, which the launcher
  # sets), so erase every store path that the dynamic loader does not use:
  # the ELF interpreter and the RPATH are what actually load, and the
  # shared-object closure hangs off those.
  runtime = pkgs.runCommand "leksah-linux-runtime" {
    nativeBuildInputs = [ pkgs.patchelf pkgs.removeReferencesTo ];
  } ''
    mkdir -p $out/bin
    if [ -e ${leksah}/bin/.leksah-wrapped ]; then
      cp ${leksah}/bin/.leksah-wrapped $out/bin/leksah
    else
      cp ${leksah}/bin/leksah $out/bin/leksah
    fi
    chmod +wx $out/bin/leksah

    exe=$out/bin/leksah
    keep="$(patchelf --print-interpreter $exe):$(patchelf --print-rpath $exe)"
    for ref in $(LC_ALL=C grep -a -o '/nix/store/[a-z0-9]\{32\}-[a-zA-Z0-9+._?=-]*' $exe | sort -u); do
      case ":$keep" in
        *":$ref/"*|*":$ref:"*|*":$ref") ;;
        *) echo "erasing unused reference $ref"
           remove-references-to -t "$ref" $exe ;;
      esac
    done
  '';

  # Runtime data GTK/WebKit look up by path rather than by linking, so the
  # reference scanner cannot infer them.  They have to be named.
  runtimeData = [
    gtk4 webkitgtk_6_0 gsettings-desktop-schemas adwaita-icon-theme
    hicolor-icon-theme shared-mime-info glib-networking dconf
    gdk-pixbuf librsvg glib
    # The fontconfig LIBRARY travels in the shared-object closure, but its
    # default configuration does not (fontconfig.out, named by FONTCONFIG_FILE
    # below) — without it every process prints "Cannot load default config
    # file" and font matching runs on empty rules.  Same story for locales:
    # glibc looks up LOCALE_ARCHIVE, and with none bundled GTK falls back to
    # the C locale with a warning per process.
    fontconfig.out glibcLocalesUtf8
    # The EGL driver.  WebKit renders through EGL and aborts without a display
    # ("Could not create default EGL display: EGL_BAD_PARAMETER"), and the
    # closure only carries libglvnd, the vendor-neutral dispatcher: the Mesa
    # driver it dispatches TO is found at run time through
    # /run/opengl-driver, which exists only on NixOS.  Bundle Mesa and point
    # glvnd at it — see the launcher, which does so only off NixOS.
    mesa
  ];

  closure = pkgs.closureInfo { rootPaths = [ runtime ] ++ runtimeData; };

  # Static, so binding over /nix/store cannot pull the launcher's own
  # interpreter out from under it (the host may be NixOS).
  bwrap = pkgs.pkgsStatic.bubblewrap;

  # Merge every schema we ship into one compiled directory: GTK reads exactly
  # one glib-2.0/schemas dir per XDG_DATA_DIRS entry, and gsettings-desktop-
  # schemas, gtk4 and webkitgtk each supply their own.
  schemas = pkgs.runCommand "leksah-linux-schemas" {
    nativeBuildInputs = [ glib.dev ];
  } ''
    mkdir -p $out/glib-2.0/schemas
    for d in ${gsettings-desktop-schemas}/share/gsettings-schemas/*/glib-2.0/schemas \
             ${gtk4}/share/gsettings-schemas/*/glib-2.0/schemas \
             ${webkitgtk_6_0}/share/gsettings-schemas/*/glib-2.0/schemas; do
      [ -d "$d" ] || continue
      cp -n "$d"/*.xml "$d"/*.gschema.override $out/glib-2.0/schemas/ 2>/dev/null || true
    done
    glib-compile-schemas $out/glib-2.0/schemas
  '';

  launcher = ''
    #!/bin/sh
    # Leksah launcher.  Binds the bundled runtime closure onto /nix/store in a
    # private mount namespace, then runs the IDE inside it.  Nothing is
    # installed and nothing outside this directory is modified.
    set -eu

    self=$(readlink -f "$0" 2>/dev/null || echo "$0")
    root=$(cd "$(dirname "$self")/.." && pwd)
    store="$root/store"
    bwrap="$root/libexec/bwrap"

    [ -d "$store" ] || { echo "leksah: $store is missing — extract the tarball again" >&2; exit 1; }

    # Fail with an explanation rather than a bare namespace errno.
    if ! "$bwrap" --dev-bind / / true 2>/dev/null; then
      cat >&2 <<'MSG'
    leksah: cannot create a user namespace.

    This build binds its bundled libraries onto /nix/store using bubblewrap,
    which needs unprivileged user namespaces.  They are enabled by default on
    current Ubuntu, Debian, Fedora, Arch and NixOS.  To enable them:

      sudo sysctl -w kernel.unprivileged_userns_clone=1     # Debian-family
      sudo sysctl -w user.max_user_namespaces=10000         # RHEL-family

    If your distribution does not allow this, build from source instead:
    https://github.com/leksah/leksah/blob/master/docs/building.md
    MSG
      exit 1
    fi

    # On a host that has its own /nix/store (NixOS), binding the bundled
    # store over the whole directory SHADOWS it: /run/opengl-driver (the
    # host's GPU drivers) dangles and WebKit dies with "Could not create
    # default EGL display", and the user's own nix-installed ghc/cabal stop
    # resolving inside Leksah's terminals.  Merge the two stores with a
    # read-only overlay instead (per-path binds cannot work: the host store
    # is mounted read-only, so mountpoints for bundled-only paths cannot be
    # created inside it).  The overlay needs bwrap >= 0.8 and unprivileged
    # overlayfs (kernel >= 5.11) — both older than current NixOS supports —
    # but probe it and fall back to the shadowing bind rather than assume.
    # A host WITHOUT /nix/store has nothing to shadow — one bind does it.
    #
    # POSIX sh has only one list, so the command line is assembled in "$@"
    # around the original arguments: the program invocation goes in after
    # them, then the mount options are prepended.
    set -- -- @RUNTIME@/bin/leksah "$@"
    if [ -d /nix/store ] && "$bwrap" --dev-bind / / \
         --overlay-src /nix/store --overlay-src "$store" \
         --ro-overlay /nix/store true 2>/dev/null; then
      set -- --overlay-src /nix/store --overlay-src "$store" \
             --ro-overlay /nix/store "$@"
    else
      # No host store, so no host drivers libglvnd could reach: point it, and
      # Mesa's own DRI/GBM lookups, at the bundled Mesa.  (On NixOS the
      # overlay branch above keeps /run/opengl-driver, whose drivers match the
      # host kernel and GPU.)  Mesa's EGL loads the GPU's own DRI driver when
      # there is one it knows, and falls back to llvmpipe software rendering.
      set -- --bind "$store" /nix/store \
             --setenv __EGL_VENDOR_LIBRARY_FILENAMES "@MESA@/share/glvnd/egl_vendor.d/50_mesa.json" \
             --setenv LIBGL_DRIVERS_PATH "@MESA@/lib/dri" \
             --setenv GBM_BACKENDS_PATH "@MESA@/lib/gbm" \
             "$@"
    fi

    exec "$bwrap" \
      --dev-bind / / \
      --setenv leksah_datadir "$root/share/leksah" \
      --setenv XDG_DATA_DIRS "$root/share/gsettings:@ADWAITA@/share:@HICOLOR@/share:@MIME@/share:''${XDG_DATA_DIRS:-/usr/local/share:/usr/share}" \
      --setenv GSETTINGS_SCHEMA_DIR "$root/share/gsettings/glib-2.0/schemas" \
      --setenv GIO_EXTRA_MODULES "@GLIBNET@/lib/gio/modules" \
      --setenv GDK_PIXBUF_MODULE_FILE "@PIXBUFCACHE@" \
      --setenv FONTCONFIG_FILE "@FONTCONFIG@/etc/fonts/fonts.conf" \
      --setenv LOCALE_ARCHIVE "@LOCALES@/lib/locale/locale-archive" \
      "$@"
  '';

in
pkgs.runCommand "leksah-linux-tarball"
  {
    nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ];
    meta = {
      description = "Relocatable Leksah tarball for x86_64 Linux (GTK4/WebKitGTK front end)";
      platforms = [ "x86_64-linux" ];
    };
  }
  ''
    set -euo pipefail
    mkdir -p ${tree}/bin ${tree}/libexec ${tree}/store ${tree}/share/leksah

    # --- the runtime closure, at the paths it was built for ---
    while read -r p; do
      cp -a "$p" ${tree}/store/
    done < ${closure}/store-paths
    chmod -R u+w ${tree}/store

    # --- launcher + bwrap ---
    cp ${bwrap}/bin/bwrap ${tree}/libexec/bwrap
    chmod u+w ${tree}/libexec/bwrap

    cp -r ${schemas} ${tree}/share/gsettings
    chmod -R u+w ${tree}/share/gsettings

    substitute ${builtins.toFile "leksah-launcher.sh" launcher} ${tree}/bin/leksah \
      --replace '@RUNTIME@'     '${runtime}' \
      --replace '@ADWAITA@'     '${adwaita-icon-theme}' \
      --replace '@HICOLOR@'     '${hicolor-icon-theme}' \
      --replace '@MIME@'        '${shared-mime-info}' \
      --replace '@GLIBNET@'     '${glib-networking}' \
      --replace '@PIXBUFCACHE@' '${gdk-pixbuf}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache' \
      --replace '@FONTCONFIG@'  '${fontconfig.out}' \
      --replace '@LOCALES@'     '${glibcLocalesUtf8}' \
      --replace '@MESA@'        '${mesa}'
    chmod +x ${tree}/bin/leksah

    # The launcher's off-NixOS GPU setup names these; a Mesa that moved them
    # would otherwise ship a tarball that renders nothing.
    for f in ${mesa}/share/glvnd/egl_vendor.d/50_mesa.json ${mesa}/lib/dri ${mesa}/lib/gbm; do
      [ -e "$f" ] || { echo "ERROR: $f missing — launcher's Mesa paths are stale" >&2; exit 1; }
    done

    # --- datadir (same set the Windows installer stages) ---
    cp -r ${src}/leksah/pics   ${tree}/share/leksah/pics
    cp -r ${src}/leksah/cm6    ${tree}/share/leksah/cm6
    cp -r ${src}/leksah/xterm  ${tree}/share/leksah/xterm
    cp -r ${src}/leksah/fonts  ${tree}/share/leksah/fonts
    cp    ${src}/LICENSE       ${tree}/share/leksah/LICENSE
    cp    ${src}/Readme.md     ${tree}/share/leksah/Readme.md
    chmod -R u+w ${tree}/share/leksah

    # --- optional desktop integration (not installed by running the app) ---
    cp ${src}/leksah/pics/leksah.png ${tree}/share/leksah.png
    cat > ${tree}/leksah.desktop <<'DESKTOP'
    [Desktop Entry]
    Type=Application
    Name=Leksah
    Comment=Haskell IDE
    Exec=leksah %F
    Icon=leksah
    Categories=Development;IDE;
    Terminal=false
    DESKTOP

    cat > ${tree}/README <<README
    Leksah ${version} — x86_64 Linux

      ./bin/leksah

    That is the whole installation.  The tarball carries its own GTK4 and
    WebKitGTK, so no distro packages are needed; it binds them onto /nix/store
    inside a private mount namespace and touches nothing else on the system.
    Move or rename the directory freely, but keep bin/, libexec/ and store/
    together.

    Requires unprivileged user namespaces (default on current Ubuntu, Debian,
    Fedora, Arch and NixOS).  bin/leksah says so plainly if they are off.

    ghc and cabal are NOT bundled — Leksah uses whatever is on your PATH, so
    install them however you normally would (ghcup, your distro, or nix).

    To add it to a desktop menu, copy leksah.desktop into
    ~/.local/share/applications and share/leksah.png into
    ~/.local/share/icons/hicolor/512x512/apps/, then edit Exec= to the full
    path of bin/leksah.

    Source and build instructions: https://github.com/leksah/leksah
    README

    mkdir -p $out
    tar --sort=name --owner=0 --group=0 --numeric-owner \
        --mtime='@1' -czf $out/${tree}-x86_64-linux.tar.gz ${tree}
  ''
