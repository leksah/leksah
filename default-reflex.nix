let
  reflex-platform = import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "e477ed22b57e0c5551f42751ffd5df60198a964b";
      sha256 = "11n5q55dv4an07w9j3f6i0c66bmn2h4i89xm64mnf4yrh1i9j5lm";
    }) {};
  nixpkgs = reflex-platform.nixpkgs;
  gtk2hs = nixpkgs.fetchFromGitHub {
    owner = "gtk2hs";
    repo = "gtk2hs";
    rev = "1a90b30efe4aaca8d78aa05a7d878f799cd4d015";
    sha256 = "1i7saivg7yq0all6z4awphmvrdmngw10sv1d1rc3asx8xf4lbjg7";
  };
  pkgConfigPackages = with nixpkgs; [
    atk.dev gdk_pixbuf.dev cairo cairo.dev glib.dev pango.dev gnome3.gtk.dev gnome3.gtksourceview.dev
    gnome3.webkitgtk.dev cairo.dev gnome3.gsettings_desktop_schemas pixman fontconfig.dev
    freetype.dev zlib.dev bzip2.dev libpng.dev expat.dev xorg.libxcb.dev xorg.libpthreadstubs
    xorg.libXau.dev xorg.xproto xorg.libXdmcp.dev xorg.libXrender.dev xorg.renderproto
    xorg.libX11.dev xorg.kbproto xorg.libXext.dev xorg.xextproto harfbuzz.dev graphite2
    epoxy.dev ]
    ++ (if stdenv.isDarwin then [ gtk-mac-integration-gtk3 ] else []);
  cleanSrc =
        builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
          nixpkgs.lib.all (i: toString i != path) [ ./.DS_Store ./osx/Leksah ./osx/keymap.lkshk ./osx/prefs.lkshp ./win32/SourceDir ./default.nix ./vendor ]
            && nixpkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".vagrant" ".DS_Store" "result" ]
            && nixpkgs.lib.all (i: !(nixpkgs.lib.hasSuffix i path)) [ ".dmg" ".msi" ".exe" ".lkshf" ".wixobj" ".wixpdb" ".wxs" ]
            && nixpkgs.lib.all (i: !(nixpkgs.lib.hasPrefix i path)) [ ".ghc.environment." ]
            # TODO: what else?
          ) ./.;
  filterSubmodule = src:
    builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
      nixpkgs.lib.all (i: toString i != path) [ ./.DS_Store ./default.nix ]
        && nixpkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".vagrant" ".DS_Store" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasSuffix i path)) [ ".lkshf" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasPrefix i path)) [ ".ghc.environment." ]
        # TODO: what else?
      ) src;
  project = reflex-platform.project ({ pkgs, ... }: {
      packages = {
        gtk2hs-buildtools = "${gtk2hs}/tools";
        leksah = cleanSrc;
        leksah-server = filterSubmodule vendor/leksah-server;
        ltk = filterSubmodule vendor/ltk;
        yi = filterSubmodule vendor/yi;
        vcswrapper = filterSubmodule vendor/haskellVCSWrapper/vcswrapper;
        vcsgui = filterSubmodule vendor/haskellVCSGUI/vcsgui;
      };

      overrides = self: super:
      let
        fixCairoGI = p: pkgs.haskell.lib.overrideCabal p (drv: {
          preCompileBuildDriver = (drv.preCompileBuildDriver or "") + ''
            export LD_LIBRARY_PATH="${pkgs.cairo}/lib"
          '';
        });
      in {
        gtk2hs-buildtools = pkgs.haskell.lib.overrideCabal super.gtk2hs-buildtools (drv: {
          libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.hashtables ];
        });
        gi-gtk-hs = pkgs.haskell.lib.doJailbreak super.gi-gtk-hs; 
        gi-cairo = fixCairoGI super.gi-cairo;
        gi-gdk = fixCairoGI super.gi-gdk;
        gi-gtk = fixCairoGI super.gi-gtk;
        gi-pango = fixCairoGI super.gi-pango;
        gi-webkit2 = fixCairoGI super.gi-webkit2;
        gi-gtksource = fixCairoGI super.gi-gtksource;
        gi-gtkosxapplication = fixCairoGI (super.gi-gtkosxapplication.override {
          gtk-mac-integration-gtk3 = pkgs.gtk-mac-integration-gtk3;
        });
        gtk3 = null;
        haskell-gi = pkgs.haskell.lib.dontCheck super.haskell-gi;

        ltk = pkgs.haskell.lib.appendConfigureFlag super.ltk "-f-check-gtk-version";
        leksah-server = pkgs.haskell.lib.dontCheck super.leksah-server; # FIXME: really `dontCheck`?
        leksah = pkgs.haskell.lib.appendConfigureFlag super.leksah "-f-check-gtk-version";
      };

      shells = {
        ghc = ["leksah" "ltk" "leksah-server" "yi" "vcswrapper" "vcsgui"];
        ghcjs = [];
      };
  });

  ghc = project.ghc // {
    leksah = nixpkgs.stdenv.mkDerivation {
      name = "leksah";
      nativeBuildInputs = with nixpkgs; [ wrapGAppsHook makeWrapper ];
      buildInputs = with nixpkgs; [
        gnome3.gtk
        gnome3.dconf
        gnome3.defaultIconTheme
        gnome3.gsettings_desktop_schemas
      ];
      src = ./linux;
      buildPhase =
        if nixpkgs.stdenv.isLinux then ''
          mkdir -p $out/share
          cp -r * $out/share/
        '' else ''
          mkdir -p $out
        '';
      installPhase = ''
        mkdir -p $out/bin
        ln -s ${project.ghc.leksah}/bin/leksah $out/bin
        wrapProgram $out/bin/leksah \
          --prefix 'PATH' ':' "${project.ghc.leksah-server}/bin" \
          --suffix 'PATH' ':' "${project.ghc.ghcWithPackages (self: [])}/bin" \
          --suffix 'PATH' ':' "${project.ghc.cabal-install}/bin"
      '';
    };
  };

  shells = project.shells // {
    ghc = project.shells.ghc.overrideAttrs (oldAttrs: {
      #buildInputs = (oldAttrs.buildInputs or [])
      #  ++ [ nixpkgs.gnome3.gtk
      #   nixpkgs.gnome3.dconf
      #   nixpkgs.gnome3.defaultIconTheme 
      #   nixpkgs.gnome3.gsettings_desktop_schemas
      #   ];
      #shellHook = (oldAttrs.shellHook or "") + ''
      #  export CFLAGS="$NIX_CFLAGS_COMPILE" # TODO: why is this needed?
      #  export XDG_DATA_DIRS="$XDG_ICON_DIRS:$GSETTINGS_SCHEMAS_PATH:$XDG_DATA_DIRS" # TODO: how to do this better?
      #  export PKG_CONFIG_PATH='' + nixpkgs.lib.concatMapStrings (p: "${p}/lib/pkgconfig:") pkgConfigPackages + ''$PKG_CONFIG_PATH
      #'';
    });
  };

in project // {
  inherit ghc shells;
}
