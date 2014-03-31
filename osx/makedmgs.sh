#!/bin/sh -ex

#export PATH=/Users/hamish/Library/Haskell/bin:/Library/Frameworks/GHC.framework/Versions/7.0.3-i386/usr/bin:$PATH
#scripts/clean.sh
#osx/makedmg.sh
#export PATH=/Users/hamish/Library/Haskell/bin:/Library/Frameworks/GHC.framework/Versions/7.0.4-i386/usr/bin:$PATH
#scripts/clean.sh
#osx/makedmg.sh
#export PATH=/Users/hamish/Library/Haskell/bin:/Library/Frameworks/GHC.framework/Versions/7.4.1-i386/usr/bin:$PATH
#scripts/clean.sh
#osx/makedmg.sh
#export PATH=/Users/hamish/Library/Haskell/bin:/Library/Frameworks/GHC.framework/Versions/7.4.2-i386/usr/bin:$PATH
#scripts/clean.sh
#osx/makedmg.sh
#export PATH=/Users/hamish/Library/Haskell/bin:/Library/Frameworks/GHC.framework/Versions/7.6.3-i386/usr/bin:$PATH
#scripts/clean.sh
#osx/makedmg.sh
export GHCVERSION=-7.6.3
scripts/clean.sh
osx/makedmg.sh
export GHCVERSION=
scripts/clean.sh
osx/makedmg.sh

