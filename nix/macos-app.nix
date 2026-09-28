# Assembles a relocatable Leksah.app for the leksah-wkwebview front end.
#
# The nix-built exe links a handful of dylibs by absolute /nix/store path;
# everything else is a macOS system framework present on every Mac.  Each store
# dependency is dealt with in one of two ways:
#
#   * `systemLibs` below — libraries macOS itself ships.  The load command is
#     repointed at /usr/lib/<name> and NOTHING is copied.  This is not merely
#     tidier: copying them produces a bundle that is broken off-nix.  nixpkgs'
#     libiconv is Apple's Citrus implementation, which dlopen()s its conversion
#     modules and reads its charset databases from *absolute paths under its own
#     store prefix* (…/lib/i18n/libiconv_std.dylib, …/share/i18n/csmapper/…).
#     Those paths do not exist on a user's Mac, so every iconv_open() fails with
#     EINVAL — and no amount of install_name_tool fixes it, because a dlopen is
#     not a load command.  The system copy has the same ABI (verified: the exe
#     imports only _iconv, _iconv_open, _iconv_close and _locale_charset, all
#     exported by /usr/lib/libiconv.2.dylib) and finds its data under /usr/share.
#   * everything else — copied into Contents/Frameworks with load commands
#     rewritten to @executable_path / @loader_path, so the bundle runs on a
#     machine without nix.
#
# The audit at the end enforces the distinction: if any *bundled* dylib still
# mentions /nix/store anywhere in its bytes, the build fails rather than shipping
# a library that cannot find its own data.
#
# The datadir lands in Contents/Resources/leksah, where IDE.Paths.packagedDataDir
# looks for it (../Resources/leksah, relative to the bundle executable).  The
# web assets it holds — cm6/, xterm/, fonts/, pics/ — are NOT cabal data-files,
# so Paths_leksah cannot supply them and staging them here is the only way the
# app gets them.  The
# bundle is ad-hoc signed (codesign -s -) so Gatekeeper does not report it as
# "damaged" after the load-command rewrite invalidates the original signature.
{ pkgs
, lib ? pkgs.lib
, leksah                 # exe derivation (…leksah:exe:leksah)
, src
, version
}:

pkgs.runCommand "Leksah.app"
  {
    nativeBuildInputs = [ pkgs.darwin.cctools ];
    meta = {
      description = "Relocatable Leksah.app (leksah-wkwebview front end)";
      platforms = pkgs.lib.platforms.darwin;
    };
  }
  ''
    set -euo pipefail
    APP="$out/Leksah.app"
    MACOS="$APP/Contents/MacOS"
    FW="$APP/Contents/Frameworks"
    RES="$APP/Contents/Resources"
    mkdir -p "$MACOS" "$FW" "$RES/leksah"

    # --- executables (CFBundleExecutable below names this one) ---
    cp ${leksah}/bin/leksah "$MACOS/leksah"
    chmod u+w "$MACOS/leksah"

    # It must be the real binary, not a makeWrapper shell script.  hix.nix wraps
    # bin/leksah (a --prefix PATH naming the nix ghc and cabal) for every
    # non-Windows, non-JS, non-musl target, which includes darwin; today that
    # wrapper does not materialise here, but if it ever did we would silently
    # ship a script that sets store paths and execs .leksah-wrapped — a file
    # that is not in the bundle.  The app would launch to nothing on a user's
    # Mac.  Fail here instead, where the message says what to do.
    if ! otool -h "$MACOS/leksah" >/dev/null 2>&1; then
      echo "ERROR: $MACOS/leksah is not a Mach-O binary — it looks like a wrapper:" >&2
      head -c 200 "$MACOS/leksah" >&2; echo >&2
      echo "  -> copy bin/.leksah-wrapped instead, and fold anything the wrapper" >&2
      echo "     set into the bundle (see nix/linux-tarball.nix for the pattern)." >&2
      exit 1
    fi
    # --- bundle the non-system dylib closure into Frameworks ---
    # Worklist: scan each Mach-O, copy every /nix/store dependency into FW, and
    # repoint the reference.  Executables reference @executable_path/../Frameworks;
    # dylibs reference their siblings via @loader_path.
    # Libraries macOS ships itself: repoint, never copy (see the header).
    systemLibs="libiconv.2.dylib libcharset.1.dylib"

    queue=()
    for m in "$MACOS"/*; do queue+=("$m"); done
    processed=""
    while [ ''${#queue[@]} -gt 0 ]; do
      f="''${queue[0]}"; queue=("''${queue[@]:1}")
      case " $processed " in *" $f "*) continue;; esac
      processed="$processed $f"

      isDylib=no; case "$f" in *.dylib) isDylib=yes;; esac
      if [ "$isDylib" = yes ]; then ref='@loader_path'; else ref='@executable_path/../Frameworks'; fi

      # deps (skip the header line and, for a dylib, its own id line)
      self="$(basename "$f")"
      otool -L "$f" | tail -n +2 | awk '{print $1}' | grep '^/nix/store/' | while read -r dep; do
        base="$(basename "$dep")"
        if [ "$isDylib" = yes ] && [ "$base" = "$self" ]; then
          install_name_tool -id "@rpath/$base" "$f" || true
          continue
        fi
        case " $systemLibs " in
          *" $base "*)
            echo "system lib: $base -> /usr/lib/$base (not bundled)"
            install_name_tool -change "$dep" "/usr/lib/$base" "$f"
            continue;;
        esac
        if [ ! -e "$FW/$base" ]; then
          cp "$dep" "$FW/$base"
          chmod u+w "$FW/$base"
        fi
        install_name_tool -change "$dep" "$ref/$base" "$f"
      done

      # enqueue any newly-copied dylibs for their own scan
      for d in "$FW"/*.dylib; do
        case " $processed " in *" $d "*) ;; *)
          case " ''${queue[*]:-} " in *" $d "*) ;; *) queue+=("$d");; esac
        esac
      done
    done

    # --- Info.plist ---
    cat > "$APP/Contents/Info.plist" <<PLIST
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
        <key>CFBundleDevelopmentRegion</key>            <string>English</string>
        <key>CFBundleExecutable</key>                   <string>leksah</string>
        <key>CFBundleIconFile</key>                     <string>leksah.icns</string>
        <key>CFBundleIdentifier</key>                   <string>org.leksah.Leksah</string>
        <key>CFBundleInfoDictionaryVersion</key>        <string>6.0</string>
        <key>CFBundleName</key>                         <string>Leksah</string>
        <key>CFBundleDisplayName</key>                  <string>Leksah</string>
        <key>CFBundlePackageType</key>                  <string>APPL</string>
        <key>CFBundleShortVersionString</key>           <string>${version}</string>
        <key>CFBundleVersion</key>                      <string>${version}</string>
        <key>NSHighResolutionCapable</key>              <true/>
        <key>NSHumanReadableCopyright</key>             <string>Copyright © 2007-2026 Hamish Mackenzie. Apache License 2.0.</string>
        <key>LSMinimumSystemVersion</key>              <string>11.0</string>
        <!-- .leksah-workspace files: a type of our own (JSON inside), which
             Finder then opens in Leksah on a double-click. -->
        <key>UTExportedTypeDeclarations</key>
        <array><dict>
            <key>UTTypeIdentifier</key>             <string>org.leksah.workspace</string>
            <key>UTTypeDescription</key>            <string>Leksah Workspace</string>
            <key>UTTypeConformsTo</key>             <array><string>public.json</string></array>
            <key>UTTypeTagSpecification</key>       <dict>
                <key>public.filename-extension</key> <array><string>leksah-workspace</string></array>
            </dict>
        </dict></array>
        <key>CFBundleDocumentTypes</key>
        <array><dict>
            <key>CFBundleTypeName</key>             <string>Leksah Workspace</string>
            <key>CFBundleTypeRole</key>             <string>Editor</string>
            <key>LSHandlerRank</key>                <string>Owner</string>
            <key>LSItemContentTypes</key>           <array><string>org.leksah.workspace</string></array>
        </dict></array>
    </dict>
    </plist>
    PLIST

    # --- icon + datadir ---
    cp ${src}/leksah/osx/leksah-macapp.icns "$RES/leksah.icns"
    cp -r ${src}/leksah/pics           "$RES/leksah/pics"
    cp -r ${src}/leksah/cm6            "$RES/leksah/cm6"
    cp -r ${src}/leksah/xterm          "$RES/leksah/xterm"
    cp -r ${src}/leksah/fonts          "$RES/leksah/fonts"
    cp    ${src}/LICENSE        "$RES/leksah/LICENSE"
    cp    ${src}/Readme.md      "$RES/leksah/Readme.md"
    chmod -R u+w "$RES/leksah"

    # --- audit: nothing that ships may need /nix/store at run time ---
    fail=0

    # (a) load commands — the linked dependencies.
    for f in "$MACOS"/* "$FW"/*.dylib; do
      [ -e "$f" ] || continue
      if otool -L "$f" | tail -n +2 | awk '{print $1}' | grep -q '^/nix/store/'; then
        echo "ERROR: $(basename "$f") still LINKS a store dylib:" >&2
        otool -L "$f" | tail -n +2 | awk '{print $1}' | grep '^/nix/store/' >&2
        fail=1
      fi
    done

    # (b) store paths in the BYTES of a bundled library.  A dylib that mentions
    # its own store prefix reads something from it at run time — dlopen'd
    # plugins, charset tables, locale data — and none of that survives the trip
    # to a user's Mac.  install_name_tool cannot fix it (it rewrites load
    # commands, not string constants), so such a library must be redirected to a
    # macOS-provided copy via systemLibs instead of bundled.  This is exactly how
    # libiconv shipped broken; see the header.
    for f in "$FW"/*.dylib; do
      [ -e "$f" ] || continue
      if LC_ALL=C grep -a -q '/nix/store/' "$f"; then
        echo "ERROR: bundled $(basename "$f") embeds store paths it reads at run time:" >&2
        LC_ALL=C grep -a -o '/nix/store/[[:print:]]\{0,100\}' "$f" | sort -u | head -20 >&2
        echo "  -> add it to systemLibs (if macOS ships it) or make it relocatable." >&2
        fail=1
      fi
    done

    [ "$fail" = 0 ] || exit 1

    # The executable is NOT subject to (b).  It does carry store paths — the
    # Paths_leksah / Paths_warp / Paths_Cabal_syntax directory constants that
    # every cabal build compiles in — but they are inert here: IDE.Paths finds
    # the datadir relative to the executable, and only falls back to
    # Paths_leksah for a cabal-install layout.  Listed for visibility so a
    # regression that starts *depending* on one is at least on the record.
    echo "--- store paths in the executable (inert Paths_* constants) ---"
    LC_ALL=C grep -a -o '/nix/store/[[:print:]]\{0,100\}' "$MACOS/leksah" \
      | sed 's|\(/nix/store/[a-z0-9]*-[^/]*\).*|\1|' | sort -u || true

    # --- ad-hoc sign (rewriting load commands invalidated any signature) ---
    /usr/bin/codesign --force --deep --sign - "$APP"
  ''
