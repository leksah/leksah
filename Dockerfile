FROM Fedora-Docker-Base-23-20151030.x86_64
MAINTAINER Hamish.K.Mackenzie@gmail.com

RUN dnf -y install sudo && dnf clean all

ENV WINE wine
ENV WINEDEBUG -all
ENV WINEPATH_U winepath -u

RUN sudo dnf -y --enablerepo updates-testing update && \
    sudo dnf clean all

RUN sudo dnf -y --enablerepo updates-testing install \
                           wine.x86_64 \
                           wine.i686 \
                           mingw64-gtksourceview3.noarch \
                           mingw32-winpthreads \
                           mingw64-enchant \
                           mingw64-libsoup \
                           mingw64-libidn \
                           mingw64-sqlite \
                           mingw64-gstreamer1-plugins-base \
                           mingw64-libwebp \
                           mingw64-libxslt \
                           mingw32-enchant \
                           mingw32-libsoup \
                           mingw32-libidn \
                           mingw32-sqlite \
                           mingw32-gstreamer1-plugins-base \
                           mingw32-libwebp \
                           mingw32-libxslt \
                           mingw32-gtk3 \
                           bison \
                           flex \
                           gettext \
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
                           gperf \
                           ruby \
                           wget \
                           tar \
                           xz \
                           make \
                           p7zip \
                           unzip \
                           cabextract \
                           cabal-install \
                           git \
                           msitools \
                           Xvfb \
                           which \
                           mono-core \
                           mono-locale-extras \
                           libxslt \
                           rpm-build

RUN sudo dnf -y --enablerepo updates-testing install dnf-plugins-core

RUN dnf download --source mingw32-webkitgtk3 && \
    rpm -i mingw-webkitgtk3-2.4.9-1.fc23.src.rpm

RUN ls /usr/src

ADD linux/specfiles/mingw-webkitgtk3.spec /root/rpmbuild/SPECS/

RUN cd /root/rpmbuild/SOURCES && \
    wget https://raw.githubusercontent.com/Alexpux/MINGW-packages/master/mingw-w64-webkitgtk3/0101-webkitgtk-2.4.3-gcc-asm.all.patch

RUN cd /root/rpmbuild/SPECS && \
    rpmbuild -ba mingw-webkitgtk3.spec

RUN sudo rpm -i /root/rpmbuild/RPMS/noarch/mingw64-webkitgtk3-2.4.9-1.fc23.noarch.rpm

RUN rm -rf /root/rpmbuild
RUN sudo dnf clean all

RUN wget http://winetricks.org/winetricks && \
    sudo cp winetricks /usr/bin && \
    sudo chmod +x /usr/bin/winetricks

# Install pkg-config for using in Wine and Windows
RUN wget http://pkgconfig.freedesktop.org/releases/pkg-config-0.28.tar.gz && \
    tar -xzf pkg-config-0.28.tar.gz && \
    cd pkg-config-0.28 && \
    mingw64-configure && \
    make && \
    sudo make install && \
    make clean && \
    rm -rf pkg-config-0.28.tar.gz pkg-config-0.28

# Fix the broken pkg-config files (it is important to use ${prefix} or they will not work
# when used on Windows or in Wine:
RUN grep -lZ '^Cflags:.*-I/usr/x86_64-w64-mingw32/sys-root/mingw' /usr/x86_64-w64-mingw32/sys-root/mingw/lib/pkgconfig/*.pc | xargs -0 -l sudo sed -i.bak -e '/^Cflags:/ s|-I/usr/x86_64-w64-mingw32/sys-root/mingw|-I${prefix}|g'

# Initialize Wine:
RUN wineboot && wine cmd /C echo "Wine OK" && wineserver -w && \
    ( export WINEPREFIX=~/.wine32 && export WINEARCH=win32 && \
      wineboot && wine cmd /C echo "Wine 32 OK" && xvfb-run winetricks --unattended dotnet40 corefonts && wineserver -w \
    )

ENV GHCVER 7.10.3

# Install Windows version of GHC (but not the old mingw that comes with it):
RUN cd ~/.wine/drive_c && \
    wget https://www.haskell.org/ghc/dist/$GHCVER/ghc-$GHCVER-x86_64-unknown-mingw32.tar.xz && \
    tar -xJf ghc-$GHCVER-x86_64-unknown-mingw32.tar.xz && \
    rm -rf ghc-*/mingw && \
    rm ghc-*-x86_64-unknown-mingw32.tar.xz

# Use `wine regedit` to add a `Path` to `HKEY_CURRENT_USER\Environment`:
RUN ( echo 'REGEDIT4' && \
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
RUN cd ~/.wine/drive_c/ghc-$GHCVER && \
    wget http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/4.9.2/threads-posix/seh/x86_64-4.9.2-release-posix-seh-rt_v4-rev2.7z && \
    7za x x86_64-4.9.2-release-posix-seh-rt_v4-rev2.7z && \
    mv mingw64 mingw && \
    rm -rf x86_64-4.9.2-release-posix-seh-rt_v4-rev2.7z

# Install WiX Toolset:
ADD wix39-binaries.zip /root/
RUN mkdir ~/.wine32/drive_c/bin && \
    cd ~/.wine32/drive_c/bin && \
    unzip ~/wix39-binaries.zip && \
    rm ~/wix39-binaries.zip

# Install 64bit Windows version of cabal-install:
RUN mkdir ~/.wine/drive_c/bin && \
    cd ~/.wine/drive_c/bin && \
    wget http://leksah.org/packages/cabal-mingw64.7z && \
    7za x cabal-mingw64.7z && \
    rm cabal-mingw64.7z

# Run `cabal update` and install some packages we will need.
# Some packages need us to run `mingw64-configure` manually (after `wine cabal configure` fails):
RUN wineserver -p1 && \
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

# Install fonts to bundle with Leksah
RUN sudo dnf -y install \
                   dejavu-sans-fonts \
                   dejavu-serif-fonts \
                   dejavu-sans-mono-fonts \
                   dejavu-lgc-sans-fonts \
                   dejavu-lgc-serif-fonts \
                   dejavu-lgc-sans-mono-fonts && \
    sudo dnf clean all && \
    mkdir -p ~/.wine32/drive_c/dejavu-fonts/ttf && \
    ln -s /usr/share/fonts/dejavu/* ~/.wine32/drive_c/dejavu-fonts/ttf

# Install grep to bundle with Leksah:
RUN mkdir grep && \
    cd grep && \
    wget http://sourceforge.net/projects/mingw/files/MSYS/Base/grep/grep-2.5.4-2/grep-2.5.4-2-msys-1.0.13-bin.tar.lzma && \
    tar --lzma -xvf grep-2.5.4-2-msys-1.0.13-bin.tar.lzma && \
    sudo cp bin/*grep.exe /usr/x86_64-w64-mingw32/sys-root/mingw/bin && \
    rm grep-2.5.4-2-msys-1.0.13-bin.tar.lzma

RUN wineserver -p1 && \
    wine cabal update && \
    wineserver -w

RUN wineserver -p1 && \
    wine cabal install shakespeare lens hlint hscolour && \
    wineserver -w

RUN git clone https://github.com/haskell-gi/haskell-gi-base.git && \
    git clone https://github.com/haskell-gi/haskell-gi.git && \
    git clone https://github.com/ghcjs/jsaddle.git && \
    git clone https://github.com/ghcjs/jsaddle-dom.git && \
    git clone https://github.com/ghcjs/ghcjs-dom.git

RUN sudo dnf copr -y enable petersen/ghc-$GHCVER
RUN sudo dnf install -y ghc cabal-install
RUN cabal install happy alex

RUN ghc-pkg list

RUN cabal install ./haskell-gi-base ./haskell-gi --force-reinstalls

RUN cd haskell-gi/bindings && \
    LANG=en_GB.UTF-8 PATH=/root/.cabal/bin:$PATH ./genBindings.sh

# Add Leksah files to Docker:
ADD leksah.cabal LICENSE LICENSE.rtf Readme.md Setup.lhs SetupLocale.lhs sources.txt leksah/

# Docker has no way to COPY or ADD two directories at once!?!?
ADD bew leksah/bew
ADD data leksah/data
ADD doc leksah/doc
ADD language-specs leksah/language-specs
ADD linux leksah/linux
ADD main leksah/main
ADD osx leksah/osx
ADD pics leksah/pics
ADD po leksah/po
ADD scripts leksah/scripts
ADD src leksah/src
ADD tests leksah/tests
ADD vendor leksah/vendor
ADD win32 leksah/win32

RUN wineserver -p1 && \
    rm -rf haskell-gi-base/dist && \
    wine cabal install \
                 ./haskell-gi-base \
                 ./haskell-gi/bindings/GLib \
                 ./haskell-gi/bindings/Gdk \
                 ./haskell-gi/bindings/Gtk \
                 ./haskell-gi/bindings/Atk \
                 ./haskell-gi/bindings/GdkPixbuf \
                 ./haskell-gi/bindings/Pango \
                 ./haskell-gi/bindings/Cairo \
                 ./haskell-gi/bindings/PangoCairo \
                 ./haskell-gi/bindings/WebKit \
                 ./haskell-gi/bindings/Gio \
                 ./haskell-gi/bindings/JavaScriptCore-3.0 \
                 ./haskell-gi/bindings/GtkSource \
                 ./haskell-gi/bindings/Notify \
                 ./haskell-gi/bindings/GObject \
                 ./haskell-gi/bindings/Soup && \
    wineserver -w

RUN git clone https://github.com/gtk2hs/gi-gtk-hs.git

RUN wineserver -p1 && \
    rm -rf haskell-gi-base/dist && \
    wine cabal install \
                 ./gi-gtk-hs ./jsaddle ./jsaddle-dom ./ghcjs-dom \
                 ./leksah/vendor/haskellVCSGUI/vcsgui && \
    wineserver -w

# Build leksah and make the MSI file:
RUN wineserver -p1 && \
    cd leksah && \
    ./win32/makeinstaller.sh && \
    wineserver -w

