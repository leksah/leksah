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
  gtk-mac-integration = super.gtk-mac-integration.overrideAttrs (oldAttrs: {
    version = "3.0.1";
    src = self.fetchFromGitLab {
      domain = "gitlab.gnome.org";
      owner = "GNOME";
      repo = "gtk-mac-integration";
      rev = "gtk-mac-integration-3.0.1";
      sha256 = "sha256-6k0pikco+6/XIB4A5Ej0qffopjivnUZiga6XjO6ogGk=";
    };
  });
}
