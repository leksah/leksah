#!/bin/sh

#export PATH='/c/Program Files (x86)/Haskell Platform/2010.2.0.0/bin':$PATH
#ghc -V || exit
#scripts/clean.sh
#PATH='/c/Program Files (x86)/Haskell Platform/2011.4.0.0/lib/extralibs/bin':$PATH cabal install haddock-2.8.1
#win32/makeinstaller.sh || exit

export PATH='/c/Program Files (x86)/Haskell Platform/2011.2.0.1/bin':$PATH
ghc -V || exit
scripts/clean.sh
PATH='/c/Program Files (x86)/Haskell Platform/2011.2.0.1/lib/extralibs/bin':$PATH cabal install haddock-2.9.2
win32/makeinstaller.sh || exit

export PATH='/c/Program Files (x86)/Haskell Platform/2011.4.0.0/bin':$PATH
ghc -V || exit
scripts/clean.sh
PATH='/c/Program Files (x86)/Haskell Platform/2011.4.0.0/lib/extralibs/bin':$PATH cabal install haddock-2.9.2
win32/makeinstaller.sh || exit

export PATH='/c/Program Files (x86)/Haskell Platform/2012.2.0.0/bin':$PATH
ghc -V || exit
scripts/clean.sh
win32/makeinstaller.sh || exit

export PATH='/c/Program Files (x86)/Haskell Platform/2012.4.0.0/bin':$PATH
ghc -V || exit
scripts/clean.sh
win32/makeinstaller.sh || exit

export PATH='/c/Program Files (x86)/Haskell Platform/2013.2.0.0/bin':$PATH
ghc -V || exit
scripts/clean.sh
win32/makeinstaller.sh || exit

