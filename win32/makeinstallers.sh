#!/bin/sh

export PATH='/c/Program Files (x86)/Haskell Platform/2010.2.0.0/bin':$PATH
ghc -V || exit
win32/makeinstaller.sh || exit

export PATH='/c/Program Files (x86)/Haskell Platform/2011.2.0.1/bin':$PATH
ghc -V || exit
win32/makeinstaller.sh || exit

export PATH='/c/Program Files (x86)/Haskell Platform/2011.4.0.0/bin':$PATH
ghc -V || exit
win32/makeinstaller.sh || exit

export PATH='/c/Program Files (x86)/Haskell Platform/2012.2.0.0/bin':$PATH
ghc -V || exit
win32/makeinstaller.sh || exit


