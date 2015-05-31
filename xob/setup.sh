#!/bin/sh -ex

sudo dnf -y update
# Install GNOME desktop
sudo dnf groupinstall -y "Fedora Workstation"
# Install other useful things
sudo dnf -y install \
                           webkitgtk3-devel \
                           gtksourceview3-devel \
                           wine.x86_64 \
                           wine.i686 \
                           mingw64-webkitgtk3.noarch \
                           mingw64-gtksourceview3.noarch \
                           mingw32-winpthreads \
                           wget \
                           tar \
                           make \
                           autoconf \
                           automake \
                           p7zip \
                           unzip \
                           git \
                           msitools \
                           libxslt \
                           nodejs

# Install VirtualBox Guest Additions
sudo dnf install -y dkms kernel-devel wget make
if [ ! -e ~/vbox/VBoxGuestAdditions_4.3.28.iso ]
then
    mkdir ~/vbox || true
    cd ~/vbox
    wget --no-verbose http://download.virtualbox.org/virtualbox/4.3.28/VBoxGuestAdditions_4.3.28.iso
fi
sudo mkdir /media/VBoxGuestAdditions || true
sudo mount -o loop,ro ~/vbox/VBoxGuestAdditions_4.3.28.iso /media/VBoxGuestAdditions
sudo sh /media/VBoxGuestAdditions/VBoxLinuxAdditions.run || true
sudo umount /media/VBoxGuestAdditions
sudo rmdir /media/VBoxGuestAdditions
sudo usermod -a -G wheel vagrant

# Disable Wayland GDM and enable auto login
sudo sed -i -e 's|^# *WaylandEnable=false$|WaylandEnable=false\nAutomaticLoginEnable=True\nAutomaticLogin=vagrant|' /etc/gdm/custom.conf
# Switch on GUI by default
sudo systemctl set-default graphical.target
# Disable sceen saver since this is a VM
DISPLAY=:0 gsettings set org.gnome.desktop.session idle-delay 0

# Install Hasklig fonts
if [ ! -d ~/hasklig ]
then
    mkdir ~/hasklig
    cd ~/hasklig
    wget --no-verbose https://github.com/i-tu/Hasklig/releases/download/0.4/Hasklig-0.4.zip
    unzip Hasklig-0.4.zip
    sudo cp *.otf /usr/share/fonts
    fc-cache
fi

# Install GHC
sudo dnf copr -y enable petersen/ghc-7.10.1
sudo dnf install -y ghc cabal-install

# Add ~/.cabal/bin to PATH
if [ ! -e ~/.profile ]
then
    echo 'export PATH=$HOME/.cabal/bin:$HOME/haskell/ghcjs/.cabal-sandbox/bin:$PATH' >> ~/.profile
    sed -i -e 's|^export PATH$|export PATH=$HOME/.cabal/bin:$HOME/haskell/ghcjs/.cabal-sandbox/bin:$PATH\n\0|' ~/.bash_profile
fi
export PATH=$HOME/.cabal/bin:$HOME/haskell/ghcjs/.cabal-sandbox/bin:$PATH

cabal update

if [ ! -e ~/.cabal/bin/leksah ]
then
    # Update Cabal and cabal-install
    cabal install Cabal cabal-install --constraint='Cabal>=1.22.3.0'
    hash -r

    # Update alex and happy
    cabal install alex happy

    # Install Leksah
    cabal install gtk2hs-buildtools
    cabal install regex-tdfa-text --ghc-options=-XFlexibleContexts
    cabal install leksah
fi

# Install GHCJS
if [ ! -d ~/haskell/ghcjs ]
then
    mkdir ~/haskell || true
    cd ~/haskell
    git clone https://github.com/ghcjs/ghcjs.git
fi

if [ ! -e ~/haskell/ghcjs/.cabal-sandbox/bin/ghcjs ]
then
    cd ~/haskell/ghcjs
    git checkout origin/improved-base
    cabal sandbox init
    cabal install --constraint='Cabal>=1.22.3.0'
fi

if [ ! -e ~/.ghcjs/x86_64-linux-0.1.0-7.10.1/ghcjs/ghcjs_boot.completed ]
then
    ghcjs-boot --dev --ghcjs-boot-dev-branch improved-base --shims-dev-branch improved-base
fi

# Install pkg-config for Wine
if [ ! -e /usr/x86_64-w64-mingw32/sys-root/mingw/bin/pkg-config.exe ]
then
    mkdir ~/pkg-config || true
    cd ~/pkg-config
    wget --no-verbose http://pkgconfig.freedesktop.org/releases/pkg-config-0.28.tar.gz
    tar -xzf pkg-config-0.28.tar.gz
    cd pkg-config-0.28
    mingw64-configure
    make
    sudo make install
    make clean
    rm -rf pkg-config-0.28.tar.gz pkg-config-0.28
fi

# Fix the broken pkg-config files (it is important to use ${prefix} or they will not work when used in Wine):
( grep -lZ '^Cflags:.*-I/usr/x86_64-w64-mingw32/sys-root/mingw' /usr/x86_64-w64-mingw32/sys-root/mingw/lib/pkgconfig/*.pc \
    | xargs -0 -l sudo sed -i.bak -e '/^Cflags:/ s|-I/usr/x86_64-w64-mingw32/sys-root/mingw|-I${prefix}|g' ) || true

# Initialize Wine:
if [ ! -e ~/.wine ]
then
    wineboot && wine cmd /C echo "Wine OK" && wineserver -w
fi
if [ ! -e ~/.wine32 ]
then
    ( export WINEPREFIX=~/.wine32 && export WINEARCH=win32 &&
      wineboot && wine cmd /C echo "Wine 32 OK" && wineserver -w
    )
fi

export GHCVER=7.10.1

# Install Windows version of GHC (but not the old mingw that comes with it):
if [ ! -d ~/.wine/drive_c/ghc-$GHCVER ]
then
    cd ~/.wine/drive_c
    wget --no-verbose https://www.haskell.org/ghc/dist/$GHCVER/ghc-$GHCVER-x86_64-unknown-mingw32.tar.xz
    tar -xJf ghc-$GHCVER-x86_64-unknown-mingw32.tar.xz
    rm -rf ghc-*/mingw
    rm ghc-*-x86_64-unknown-mingw32.tar.xz
fi

# Use `wine regedit` to add a `Path` to `HKEY_CURRENT_USER\Environment`:
    ( echo 'REGEDIT4' && \
      echo && \
      echo '[HKEY_CURRENT_USER\Environment]' && \
      echo '"Path"="C:\\users\\root\\Application Data\\cabal\\bin;C:\\ghc-'$GHCVER'\\bin;C:\\bin;Z:\\usr\\x86_64-w64-mingw32\\sys-root\\mingw\\bin;%PATH%"') > Environment.reg && \
    wine regedit Environment.reg && \
    ( echo 'REGEDIT4' && \
      echo && \
      echo '[HKEY_CURRENT_USER\Environment]' && \
      echo '"Path"="C:\\users\\root\\Application Data\\cabal\\bin;C:\\ghc-'$GHCVER'\\bin;C:\\bin;Z:\\usr\\i686-w64-mingw32\\sys-root\\mingw\\bin;%PATH%"') > Environment.reg && \
    ( export WINEPREFIX=~/.wine32 && export WINEARCH=win32 && \
      wine regedit Environment.reg && wineserver -w \
    ) && \
    rm Environment.reg

# Replace the MinGW that comes with GHC:
if [ ! -d ~/.wine/drive_c/ghc-$GHCVER/mingw ]
then
    cd ~/.wine/drive_c/ghc-$GHCVER
    wget --no-verbose http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/4.9.2/threads-posix/seh/x86_64-4.9.2-release-posix-seh-rt_v4-rev2.7z
    7za x x86_64-4.9.2-release-posix-seh-rt_v4-rev2.7z
    mv mingw64 mingw
    rm -rf x86_64-4.9.2-release-posix-seh-rt_v4-rev2.7z
fi

# Install 64bit Windows version of cabal-install:
export WINEDEBUG=-all
if [ ! -e ~/.wine32/drive_c/bin/cabal.exe ]
then
    mkdir ~/.wine/drive_c/bin
    cd ~/.wine/drive_c/bin
    wget --no-verbose http://leksah.org/packages/cabal-mingw64.7z
    7za x cabal-mingw64.7z
    rm cabal-mingw64.7z
fi

# Run `cabal update` and install some packages we will need.
# Some packages need us to run `mingw64-configure` manually (after `wine cabal configure` fails):
    wineserver -p1 && \
    cabal update && \
    wine cabal update && \
    wine cabal install Cabal && \
    wine cabal install old-locale && \
    cabal unpack network && \
    cd network-* && \
    (wine cabal configure || true) && \
    mingw64-configure && \
    wine cabal build && \
    wine cabal copy && \
    wine cabal register && \
    cd .. && \
    cabal unpack old-time && \
    cd old-time-* && \
    (wine cabal configure || true) && \
    mingw64-configure && \
    wine cabal build && \
    wine cabal copy && \
    wine cabal register && \
    cd .. && \
    wine cabal install alex happy && \
    wine cabal install gtk2hs-buildtools && \
    wine cabal install regex-tdfa-text --ghc-options=-XFlexibleContexts && \
    wineserver -w && \
    rm -rf network-* old-time-*


