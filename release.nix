let
  localLib = import ./nix/lib.nix { haskellCompiler = "ghc865"; };
  disabled = [
    ["nix-tools" "tests" "x86_64-darwin"]
  ];
in
{ leksah ? { outPath = ./.; rev = "abcdef"; } ,... }@args:
localLib.pkgs.lib.mapAttrsRecursiveCond
(as: !(as ? "type" && as.type == "derivation"))
(path: v: if (builtins.elem path disabled) then null else v)
(localLib.nix-tools.release-nix {
  _this = leksah;
  package-set-path = ./.;

  packages = [ "leksah" ];

  required-name = "leksah-required-checks";
  required-targets = jobs: [

    jobs.nix-tools.libs.leksah.x86_64-darwin
    jobs.nix-tools.libs.leksah.x86_64-linux

    # windows cross compilation targets
    jobs.nix-tools.libs.x86_64-pc-mingw32-leksah.x86_64-linux
  ];

} (builtins.removeAttrs args ["leksah"]))
