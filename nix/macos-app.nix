# Assembles a relocatable Leksah.app for the leksah-wkwebview front end.
#
# The nix-built exe links a handful of dylibs by absolute /nix/store path
# (libiconv, libgmp, libffi and their closure); everything else is a macOS
# system framework present on every Mac.  We copy that non-system closure into
# Contents/Frameworks and rewrite the load commands to @executable_path /
# @loader_path so the bundle runs on a machine without nix.
#
# The datadir lands in Contents/Resources/leksah, which IDE.Core.State.leksahSubDir
# finds when the bundle executable is named "leksah" (see that function).  The
# bundle is ad-hoc signed (codesign -s -) so Gatekeeper does not report it as
# "damaged" after the load-command rewrite invalidates the original signature.
{ pkgs
, lib ? pkgs.lib
, leksah-wkwebview       # exe derivation (…leksah:exe:leksah-wkwebview)
, leksah-server ? null   # optional exe (…leksah-server:exe:leksah-server)
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

    # --- executables (named 'leksah' so leksahSubDir finds the datadir) ---
    cp ${leksah-wkwebview}/bin/leksah-wkwebview "$MACOS/leksah"
    chmod u+w "$MACOS/leksah"
    ${lib.optionalString (leksah-server != null) ''
      cp ${leksah-server}/bin/leksah-server "$MACOS/leksah-server"
      chmod u+w "$MACOS/leksah-server"
    ''}

    # --- bundle the non-system dylib closure into Frameworks ---
    # Worklist: scan each Mach-O, copy every /nix/store dependency into FW, and
    # repoint the reference.  Executables reference @executable_path/../Frameworks;
    # dylibs reference their siblings via @loader_path.
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
        <key>NSHumanReadableCopyright</key>             <string>Leksah authors, GNU General Public License.</string>
        <key>LSMinimumSystemVersion</key>              <string>11.0</string>
    </dict>
    </plist>
    PLIST

    # --- icon + datadir ---
    cp ${src}/osx/leksah-macapp.icns "$RES/leksah.icns"
    cp -r ${src}/data           "$RES/leksah/data"
    cp -r ${src}/pics           "$RES/leksah/pics"
    cp -r ${src}/cm6            "$RES/leksah/cm6"
    cp -r ${src}/xterm          "$RES/leksah/xterm"
    cp -r ${src}/fonts          "$RES/leksah/fonts"
    cp -r ${src}/language-specs "$RES/leksah/language-specs"
    cp    ${src}/LICENSE        "$RES/leksah/LICENSE"
    cp    ${src}/Readme.md      "$RES/leksah/Readme.md"
    chmod -R u+w "$RES/leksah"

    # --- ad-hoc sign (rewriting load commands invalidated any signature) ---
    /usr/bin/codesign --force --deep --sign - "$APP"
  ''
