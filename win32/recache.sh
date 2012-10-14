#!/bin/sh

# This is just a handy script to run after blowing away your ~/AppData/Roaming/ghc directory.
# On Vista and later you will need to be admin.

export PATH='/C/Program Files (x86)/Haskell Platform/2010.2.0.0/bin':$PATH
ghc -V
ghc-pkg recache

export PATH='/C/Program Files (x86)/Haskell Platform/2011.2.0.1/bin':$PATH
ghc -V
ghc-pkg recache

export PATH='/C/Program Files (x86)/Haskell Platform/2011.4.0.0/bin':$PATH
ghc -V
ghc-pkg recache

