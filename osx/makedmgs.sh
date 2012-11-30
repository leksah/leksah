#!/bin/sh -ex

scripts/clean.sh
PATH=/Users/hamish/Library/Haskell/bin:/Library/Frameworks/GHC.framework/Versions/7.0.3-i386/usr/bin:$PATH osx/makedmg.sh
scripts/clean.sh
PATH=/Users/hamish/Library/Haskell/bin:/Library/Frameworks/GHC.framework/Versions/7.0.4-i386/usr/bin:$PATH osx/makedmg.sh
scripts/clean.sh
PATH=/Users/hamish/Library/Haskell/bin:/Library/Frameworks/GHC.framework/Versions/7.4.1-i386/usr/bin:$PATH osx/makedmg.sh
scripts/clean.sh
PATH=/Users/hamish/Library/Haskell/bin:/Library/Frameworks/GHC.framework/Versions/7.4.2-i386/usr/bin:$PATH osx/makedmg.sh
scripts/clean.sh
PATH=/Users/hamish/Library/Haskell/bin:$HOME/ghc-7.6.1/bin:$PATH osx/makedmg.sh


