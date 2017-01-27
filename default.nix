{ mkDerivation, array, base, base-compat, binary, binary-shared
, blaze-html, bytestring, Cabal, conduit, containers, cpphs
, deepseq, directory, executable-path, filepath, ghc
, ghcjs-codemirror, gi-cairo, gi-gdk, gi-gdkpixbuf, gi-gio, gi-glib
, gi-gobject, gi-gtk, gi-gtk-hs, gi-gtksource, gi-pango, gi-webkit2
, gnome3, haskell-gi-base, haskell-src-exts, hlint, hslogger, HTTP
, leksah-server, ltk, monad-loops, mtl, network, network-uri
, old-time, parsec, pretty, pretty-show, QuickCheck, regex-base
, regex-tdfa, regex-tdfa-text, shakespeare, split, stdenv, stm
, strict, text, time, transformers, unix, utf8-string, vado, vcsgui
, vcswrapper
}:
mkDerivation {
  pname = "leksah";
  version = "0.16.2.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base base-compat binary binary-shared blaze-html bytestring
    Cabal conduit containers cpphs deepseq directory executable-path
    filepath ghc ghcjs-codemirror gi-cairo gi-gdk gi-gdkpixbuf gi-gio
    gi-glib gi-gobject gi-gtk gi-gtk-hs gi-gtksource gi-pango
    gi-webkit2 haskell-gi-base haskell-src-exts hlint hslogger HTTP
    leksah-server ltk mtl network network-uri old-time parsec pretty
    pretty-show QuickCheck regex-base regex-tdfa regex-tdfa-text
    shakespeare split stm strict text time transformers unix
    utf8-string vado vcsgui vcswrapper
  ];
  libraryPkgconfigDepends = [ gnome3.gtk3 ];
  executableHaskellDepends = [ base gi-gtk-hs leksah-server stm ];
  testHaskellDepends = [
    base Cabal containers gi-glib gi-gtk-hs gi-gtksource hslogger
    leksah-server ltk monad-loops QuickCheck stm text transformers
  ];
  homepage = "http://www.leksah.org";
  description = "Haskell IDE written in Haskell";
  license = "GPL";
}
