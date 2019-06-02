{ haskellCompiler }:
let
  fixMacOsGioIntrospection = self: super: {
    gobject-introspection = if super.pkgs.stdenv.isDarwin
      then super.gobject-introspection.overrideAttrs (oldAttrs: {
        postInstall = (oldAttrs.postInstall or "") + ''
          sed -i '/<function name="content_type_[gs]et_mime_dirs"/,/<\/function>/d' $dev/share/gir-1.0/Gio-2.0.gir
          ./tools/g-ir-compiler --includedir $dev/share/gir-1.0 $dev/share/gir-1.0/Gio-2.0.gir > $out/lib/girepository-1.0/Gio-2.0.typelib
        '';
      })
      else super.gobject-introspection;
  };
  # Gtk actually has three states debug enabled, disabled and unspecified.
  # the unspecified option turns on some very useful debug features.
  dontDisableGtkDebug = self: super: {
    gtk3 = super.gtk3.overrideAttrs (oldAttrs: {
      configureFLags =
        builtins.filter (x: !builtins.elem x [
          "--disable-debug"
          "--disable-dependency-tracking"
          "--disable-glibtest"])
        (super.pkgs.lib.lists.flatten oldAttrs.configureFlags);
    });
  };
  selectGhc = self: super: {
    ghc = super.haskell.compiler.${haskellCompiler};
  };
  # iohk-nix can be overridden for debugging purposes by setting
  # NIX_PATH=iohk_nix=/path/to/iohk-nix
  iohkNix = import (
    let try = builtins.tryEval <iohk_nix>;
    in if try.success
    then builtins.trace "using host <iohk_nix>" try.value
    else
      let
        spec = builtins.fromJSON (builtins.readFile ./pins/iohk-nix-src.json);
      in builtins.fetchTarball {
        url = "${spec.url}/archive/${spec.rev}.tar.gz";
        inherit (spec) sha256;
      }) {
      nixpkgsJsonOverride = ./pins/nixpkgs-src.json;
      haskellNixJsonOverride = ./pins/haskell-nix-src.json;
      nixpkgsOverlays = [ fixMacOsGioIntrospection dontDisableGtkDebug selectGhc ];
    };

  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
in lib // { inherit iohkNix pkgs; inherit (iohkNix) nix-tools; }
