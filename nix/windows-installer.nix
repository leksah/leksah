# Builds a Windows Setup.exe for the leksah-webview2 front end, entirely on the
# Linux cross builder (makensis runs natively; no Windows host needed).
#
# Two steps:
#   1. `staging` assembles the on-disk install tree — bin/ (the exe renamed to
#      leksah.exe so the relocatable datadir logic in IDE.Core.State fires, plus
#      the mingw runtime DLLs and WebView2Loader.dll) and leksah/ (the datadir:
#      the cabal data-files leksah reads at runtime plus the bundled web assets
#      cm6/ and xterm/ and the Hasklig fonts, which are not cabal data-files).
#   2. makensis packages that tree into $out/LeksahSetup.exe via
#      nix/leksah-installer.nsi, with nix/leksah.ico as the installer icon.
#      (The icon lives here, beside the .nsi, rather than in the old win32/
#      tree — that is the classic GTK installer and now lives in
#      leksah-classic/win32.)
#
# Consumed from flake.nix (x86_64-linux only, where crossPlatforms yields ucrt64).
{ pkgs
, lib ? pkgs.lib
, leksah                # cross exe derivation (…leksah:exe:leksah)
, leksah-server ? null  # optional cross exe (…leksah-server:exe:leksah-server)
, src                   # leksah source tree (for the datadir)
, version
}:

let
  staging = pkgs.runCommand "leksah-windows-staging" { } ''
    mkdir -p $out/bin $out/leksah

    # Executable, renamed to leksah.exe (leksahSubDir keys on this name), plus
    # the runtime DLLs (dereferenced so the tree is self-contained).  The .dll.a
    # import libraries in bin/ are build-time only — the *.dll glob skips them.
    cp -L ${leksah}/bin/leksah.exe $out/bin/leksah.exe
    for f in ${leksah}/bin/*.dll; do cp -L "$f" $out/bin/; done
    ${lib.optionalString (leksah-server != null) ''
      cp -L ${leksah-server}/bin/leksah-server.exe $out/bin/
    ''}

    # Datadir.  Keep it to what the app actually reads at runtime.
    cp -r ${src}/pics           $out/leksah/pics
    cp -r ${src}/cm6            $out/leksah/cm6
    cp -r ${src}/xterm          $out/leksah/xterm
    cp -r ${src}/fonts          $out/leksah/fonts
    cp    ${src}/LICENSE        $out/leksah/LICENSE
    cp    ${src}/Readme.md      $out/leksah/Readme.md

    chmod -R u+w $out
  '';
in
pkgs.runCommand "leksah-windows-installer"
  {
    nativeBuildInputs = [ pkgs.nsis ];
    meta = {
      description = "Windows Setup.exe for the leksah-webview2 front end";
      platforms = [ "x86_64-linux" ];
    };
  }
  ''
    mkdir -p $out
    makensis -V3 \
      "-DVERSION=${version}" \
      "-DSTAGING=${staging}" \
      "-DOUTFILE=$out/LeksahSetup.exe" \
      "-DICON=${./leksah.ico}" \
      "-DLICENSE=${staging}/leksah/LICENSE" \
      ${./leksah-installer.nsi}
  ''
