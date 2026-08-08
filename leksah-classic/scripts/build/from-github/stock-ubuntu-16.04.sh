#!/bin/bash -xe

BUILD_HOME=~/leksah-github-build

# Some general Haskell prerequisites:
# ===================================

# Install stack
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
echo 'deb http://download.fpcomplete.com/ubuntu wily main' | sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update && sudo apt-get install -y --allow-unauthenticated stack

# Install git and cabal
sudo apt-get install -y git cabal-install

# Add cabal-installed binaries to your path
echo -e '\nPATH=$PATH:$HOME/.cabal/bin' >> ~/.bashrc

# Avoids "./genBindings.sh: line 23: haskell-gi: command not found" error
export PATH=$PATH:$HOME/.cabal/bin


# Leksah-specific prerequsistes
# ===================================

# Install binaries required by Leksah
sudo apt-get install -y libgirepository1.0-dev libgtksourceview-3.0-dev \
    libjavascriptcoregtk-3.0-dev libjavascriptcoregtk-4.0-dev \
    libwebkit2gtk-4.0-dev libwebkitgtk-3.0-dev libpoppler-glib-dev \
    libvte-2.91-dev libnotify-dev libgstreamer1.0-dev libgstreamer-plugins-base1.0-dev


# Clone all git repositories
mkdir --parents $BUILD_HOME
cd $BUILD_HOME

git clone https://github.com/haskell-gi/haskell-gi-base.git
git clone https://github.com/haskell-gi/haskell-gi.git
git clone https://github.com/gtk2hs/gi-gtk-hs.git
git clone https://github.com/ghcjs/jsaddle.git
git clone https://github.com/ghcjs/jsaddle-dom.git
git clone https://github.com/ghcjs/ghcjs-dom.git
git clone https://github.com/leksah/leksah.git
cd leksah
git submodule update --init
cd -


# Install cabal prerequisites
cabal update
cabal install alex happy gtk2hs-buildtools
cabal install ./haskell-gi-base ./haskell-gi


cd haskell-gi/bindings
./genBindings.sh
cd -


cd leksah

# Remove OSX-specific stack dependency
sed -i "/GtkosxApplication/d" stack.yaml

stack build
