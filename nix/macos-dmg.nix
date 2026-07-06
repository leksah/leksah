# Wraps Leksah.app in a compressed .dmg with an /Applications symlink, so a
# novice drags Leksah onto Applications to install.
#
# hdiutil is a macOS system tool that cannot run under the nix sandbox; this
# derivation therefore only builds where the daemon has `sandbox = false`
# (the case on the leksah dev machines).  It is the same class of constraint as
# the Windows installer needing the x86_64-linux cross builder.
{ pkgs
, lib ? pkgs.lib
, app                    # the Leksah.app derivation (nix/macos-app.nix)
, version
}:

pkgs.runCommand "leksah-macos-dmg"
  {
    meta = {
      description = "Leksah.dmg (drag-to-Applications installer, leksah-wkwebview)";
      platforms = pkgs.lib.platforms.darwin;
    };
  }
  ''
    set -euo pipefail
    mkdir -p "$out" stage
    cp -R ${app}/Leksah.app stage/Leksah.app
    chmod -R u+w stage
    ln -s /Applications stage/Applications
    /usr/bin/hdiutil create \
      -volname "Leksah" \
      -srcfolder stage \
      -ov -format UDZO \
      "$out/Leksah-${version}.dmg"
  ''
