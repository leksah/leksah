{ compiler-nix-name ? "ghc8105"
}:
let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources."haskellNix" {};
  project = haskellNix.hix.project { src = ./.; inherit compiler-nix-name; };
  inherit (project) pkgs;
  launch-leksah-script = pkgs.writeShellScriptBin "launch-leksah" ''
    "$@"
  '';
  launch-leksah = pkgs.stdenv.mkDerivation {
      name = "launch-leksah";
      nativeBuildInputs = with pkgs; [ wrapGAppsHook makeWrapper ];
      buildInputs = with pkgs; [
        gnome3.gtk
        gnome3.dconf
        gnome3.defaultIconTheme
        gnome3.gsettings_desktop_schemas
      ];
      src = ./linux;
      buildPhase = ''
          mkdir -p $out
        '';
      installPhase = ''
        mkdir -p $out/bin
        ln -s ${launch-leksah-script}/bin/launch-leksah $out/bin
        cp launch-leksah/Info.plist $out/bin
        wrapProgram $out/bin/launch-leksah \
          --prefix 'PATH' ':' "${project.tool "cabal" "latest"}/bin" \
          --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${compiler-nix-name}}/bin" \
          --suffix 'PATH' ':' "${project.hsPkgs.doctest.components.exes.doctest}/bin" \
          --suffix 'LD_LIBRARY_PATH' ':' "${pkgs.cairo}/lib" \
          --suffix 'FONTCONFIG_PATH' ':' "${pkgs.fontconfig.out}/etc/fonts" \
          --set 'XDG_DATA_DIRS' ""
        '';
  };
in project.hsPkgs.leksah.components.exes.leksah // {
    inherit launch-leksah pkgs;
    inherit (project) shell;
  }

