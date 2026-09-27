# The Microsoft.Web.WebView2 NuGet package (a plain zip), shared by the build
# and the installer: WebView2.h for jsaddle-webview2's C shim at compile time
# (nix/hix.nix), WebView2Loader.dll beside leksah.exe at run time
# (nix/windows-installer.nix).  Nothing from it is linked.
{ fetchzip }:
fetchzip {
  url = "https://www.nuget.org/api/v2/package/Microsoft.Web.WebView2/1.0.4022.49";
  extension = "zip";
  stripRoot = false;
  hash = "sha256-RoVh4A/Pg9/40kHtIIsC916QgPkB8TnDeOvN4ptPNM4=";
}
