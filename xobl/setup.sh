#!/bin/sh -ex

export VBOXVER=5.0.16
export GHCVER=7.10.3

# Install GNOME desktop
sudo dnf groupinstall -y "Fedora Workstation"
# Install other useful things
sudo dnf -y install \
                           gobject-introspection-devel \
                           atk-devel \
                           libsoup-devel \
                           webkitgtk3-devel \
                           webkitgtk4-devel \
                           gtksourceview3-devel \
                           poppler-glib-devel \
                           vte291-devel \
                           libnotify-devel \
                           gstreamer1-devel \
                           gstreamer1-plugins-base-devel \
                           wine.x86_64 \
                           wine.i686 \
                           mingw64-gtksourceview3.noarch \
                           mingw32-winpthreads \
                           wget \
                           tar \
                           xz \
                           make \
                           autoconf \
                           automake \
                           p7zip \
                           unzip \
                           cabextract \
                           git \
                           msitools \
                           Xvfb \
                           which \
                           mono-core \
                           mono-locale-extras \
                           libxslt \
                           nodejs \
                           npm

# Disable password authenticated SSH (key should be on there now) and enable sshd.
# Run vagrant ssh to connect or vagrant ssh_config to find out the connection settings.
sudo sed -i -e 's|^\(PasswordAuthentication *\)yes$|\1no|' /etc/ssh/sshd_config
sudo setenforce 0
sudo systemctl enable sshd.service
sudo systemctl restart sshd.service
sudo setenforce 1

# Install VirtualBox Guest Additions
sudo dnf install -y dkms kernel-devel wget make
if [ ! -e ~/vbox/VBoxGuestAdditions_$VBOXVER.iso ]
then
    mkdir ~/vbox || true
    cd ~/vbox
    wget --no-verbose http://download.virtualbox.org/virtualbox/$VBOXVER/VBoxGuestAdditions_$VBOXVER.iso
fi
sudo mkdir /media/VBoxGuestAdditions || true
sudo mount -o loop,ro ~/vbox/VBoxGuestAdditions_$VBOXVER.iso /media/VBoxGuestAdditions
sudo sh /media/VBoxGuestAdditions/VBoxLinuxAdditions.run || true
sudo umount /media/VBoxGuestAdditions
sudo rmdir /media/VBoxGuestAdditions
sudo usermod -a -G wheel vagrant

# Disable Wayland GDM and enable auto login
sudo sed -i -e 's|^# *WaylandEnable=false$|WaylandEnable=false\nAutomaticLoginEnable=True\nAutomaticLogin=vagrant|' /etc/gdm/custom.conf
# Switch on GUI by default
sudo systemctl set-default graphical.target

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
sudo dnf copr -y enable petersen/ghc-$GHCVER
sudo dnf install -y ghc cabal-install

# Add ~/.cabal/bin to PATH
if [ ! -e ~/.profile ]
then
    echo 'export PATH=$HOME/.cabal/bin:$HOME/haskell/ghcjs/.cabal-sandbox/bin:$PATH' >> ~/.profile
    echo 'export WINEDEBUG=-all' >> ~/.profile
    sed -i -e 's|^export PATH$|export PATH=$HOME/.cabal/bin:$HOME/haskell/ghcjs/.cabal-sandbox/bin:$PATH\n\0|' ~/.bash_profile
    echo 'export WINEDEBUG=-all' >> ~/.bash_profile
fi
export PATH=$HOME/.cabal/bin:$HOME/haskell/ghcjs/.cabal-sandbox/bin:$PATH

cabal update

mkdir -p ~/haskell

if [ ! -d ~/haskell/haskell-gi-base ]
then
    cd ~/haskell
    git clone https://github.com/haskell-gi/haskell-gi-base.git
fi

if [ ! -d ~/haskell/haskell-gi ]
then
    cd ~/haskell
    git clone https://github.com/haskell-gi/haskell-gi.git
fi

if [ ! -d ~/haskell/gi-gtk-hs ]
then
    cd ~/haskell
    git clone https://github.com/gtk2hs/gi-gtk-hs.git
fi

if [ ! -d ~/haskell/jsaddle ]
then
    cd ~/haskell
    git clone https://github.com/ghcjs/jsaddle.git
fi

if [ ! -d ~/haskell/jsaddle-dom ]
then
    cd ~/haskell
    git clone https://github.com/ghcjs/jsaddle-dom.git
fi

if [ ! -d ~/haskell/ghcjs-dom ]
then
    cd ~/haskell
    git clone https://github.com/ghcjs/ghcjs-dom.git
fi

if [ ! -d ~/haskell/leksah ]
then
    cd ~/haskell
    git clone https://github.com/leksah/leksah.git
    cd leksah
    git submodule update --init
fi

if [ ! -e ~/.cabal/bin/haskell-gi ]
then
    # Update alex and happy
    cabal install alex happy

    cd ~/haskell
    cabal install ./haskell-gi-base ./haskell-gi --force-reinstalls
fi

if [ ! -e ~/.cabal/bin/leksah ]
then
    cd ~/haskell/haskell-gi/bindings
    ./genBindings.sh
    ./buildAll.sh
    
    cd ~/haskell/leksah

    # Install Leksah
    cabal install gtk2hs-buildtools
    cabal install ../gi-gtk-hs ../jsaddle ../jsaddle-dom ../ghcjs-dom ./ ./vendor/ltk ./vendor/leksah-server ./vendor/haskellVCSGUI/vcsgui --force-reinstalls
fi

# Install socket.io (needed for GHCJSi)
if [ ! -d /lib/node_modules/socket.io ]
then
    sudo npm install -g socket.io
fi

# Install GHCJS
if [ ! -d ~/haskell/ghcjs ]
then
    cd ~/haskell
    git clone https://github.com/ghcjs/ghcjs.git
fi

if [ ! -e ~/haskell/ghcjs/.cabal-sandbox/bin/ghcjs ]
then
    cd ~/haskell/ghcjs
    cabal sandbox init
    cabal install --constraint='Cabal>=1.22.3.0'
fi

if [ ! -e ~/.ghcjs/x86_64-linux-*-$GHCVER/ghcjs/ghcjs_boot.completed ]
then
	cabal install cabal-install-1.22.7.0
	hash -r
    ghcjs-boot --dev
fi

if [ ! -e ~/.ghcjs/x86_64-linux-$GHCVER ]
then
	cd ~/.ghcjs
    ln -sf x86_64-linux-*-$GHCVER x86_64-linux-$GHCVER
fi

# Install Wine Tricks
if [ ! -e /usr/bin/winetricks ]
then
    cd ~
    wget http://winetricks.org/winetricks
    sudo cp winetricks /usr/bin
    sudo chmod +x /usr/bin/winetricks
    rm winetricks
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

# Initialize Wine and 32bit dotnet40:
if [ ! -e ~/.wine ]
then
    wineboot && wine cmd /C echo "Wine OK" && wineserver -w
fi
if [ ! -e ~/.wine32 ]
then
    ( export WINEPREFIX=~/.wine32 && export WINEARCH=win32 &&
      wineboot && wine cmd /C echo "Wine 32 OK" && xvfb-run winetricks --unattended dotnet40 corefonts && wineserver -w && wineserver -w
    )
fi

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
      echo '"Path"="C:\\users\\vagrant\\Application Data\\cabal\\bin;C:\\ghc-'$GHCVER'\\bin;C:\\bin;Z:\\usr\\x86_64-w64-mingw32\\sys-root\\mingw\\bin;%PATH%"') > Environment.reg && \
    wine regedit Environment.reg && \
    ( echo 'REGEDIT4' && \
      echo && \
      echo '[HKEY_CURRENT_USER\Environment]' && \
      echo '"Path"="C:\\users\\vagrant\\Application Data\\cabal\\bin;C:\\ghc-'$GHCVER'\\bin;C:\\bin;Z:\\usr\\i686-w64-mingw32\\sys-root\\mingw\\bin;%PATH%"') > Environment.reg && \
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
if [ ! -e ~/.wine/drive_c/bin/cabal.exe ]
then
    mkdir ~/.wine/drive_c/bin
    cd ~/.wine/drive_c/bin
    wget --no-verbose http://leksah.org/packages/cabal-mingw64.7z
    7za x cabal-mingw64.7z
    rm cabal-mingw64.7z
fi

if [ ! -e  ~/.wine/drive_c/users/vagrant/Application\ Data/ghc/x86_64-mingw32-$GHCVER/package.conf.d/network-* ]
then
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
    wineserver -w && \
    rm -rf network-* old-time-*
fi

# Collect Leksah Metadata
if [ ! -d ~/.leksah-0.15 ]
then
    mkdir ~/.leksah-0.15
    cd ~/.leksah-0.15
    wget https://raw.githubusercontent.com/leksah/leksah/master/data/prefscoll.lkshp
    # Add ~/haskell to the list of directories checked for system package source
    (head -n1 ~/.cabal/share/x86_64-linux-ghc-$GHCVER/leksah-server-*/data/prefscoll.lkshp &&
     echo '               ["~/haskell"]' &&
     tail -n+3 ~/.cabal/share/x86_64-linux-ghc-$GHCVER/leksah-server-*/data/prefscoll.lkshp) > prefs.lkshp
    cp prefs.lkshp prefscoll.lkshp
    echo 'TextView Font: "Hasklig Light 12"' >> prefs.lkshp
    leksah-server -sob
fi    

# Add Leksah application
if [ ! -e /usr/share/applications/leksah.desktop ]
then
    sudo cp ~/haskell/leksah/linux/applications/leksah.desktop /usr/share/applications/leksah.desktop
fi

# Set Leksah to autostart
if [ ! -e ~/.config/autostart/leksah.desktop ]
then
    mkdir -p ~/.config/autostart
    cp ~/haskell/leksah/linux/applications/leksah.desktop ~/.config/autostart/leksah.desktop
fi

# Disable sceen saver since this is a VM
if [ ! -e ~/.config/autostart/DisableScreenLock.desktop ]
then
    (echo '[Desktop Entry]' &&
     echo 'Name=DisableScreenLock' &&
     echo 'Type=Application' &&
     echo 'Exec=sh -c "gsettings set org.gnome.desktop.session idle-delay 0 && rm ~/.config/autostart/DisableScreenLock.desktop"') > ~/.config/autostart/DisableScreenLock.desktop
fi
