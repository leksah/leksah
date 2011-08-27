#!/bin/sh

export PATH='/C/Program Files (x86)/Haskell Platform/2010.2.0.0/lib/extralibs/bin':'/C/Program Files (x86)/Haskell Platform/2010.2.0.0/bin':$PATH
ghc -V
win32/makeinstaller.sh

export PATH='/C/Program Files (x86)/Haskell Platform/2011.2.0.1/lib/extralibs/bin':'/C/Program Files (x86)/Haskell Platform/2011.2.0.1/bin':$PATH
ghc -V
win32/makeinstaller.sh

