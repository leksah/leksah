# Gtk actually has three states debug enabled, disabled and unspecified.
# the unspecified option turns on some very useful debug features.
self: super: {
  gtk3 = super.gtk3.overrideAttrs (oldAttrs: {
    configureFLags =
      builtins.filter (x: !builtins.elem x [
        "--disable-debug"
        "--disable-dependency-tracking"
        "--disable-glibtest"])
      (super.pkgs.lib.lists.flatten (oldAttrs.configureFlags or []));
  });
}
