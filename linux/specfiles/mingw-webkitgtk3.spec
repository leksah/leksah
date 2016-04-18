%{?mingw_package_header}

# JavaScriptCore LLInt fails to build for 64 bit; disable it for now.
%global mingw_build_win64 1

## NOTE: Lots of files in various subdirectories have the same name (such as
## "LICENSE") so this short macro allows us to distinguish them by using their
## directory names (from the source tree) as prefixes for the files.
%global add_to_doc_files32() \
    mkdir -p %{buildroot}%{_docdir}/mingw32-webkitgtk3 ||: ; \
    cp -p %1  %{buildroot}%{_docdir}/mingw32-webkitgtk3/$(echo '%1' | sed -e 's!/!.!g')
%global add_to_doc_files64() \
    mkdir -p %{buildroot}%{_docdir}/mingw64-webkitgtk3 ||: ; \
    cp -p %1  %{buildroot}%{_docdir}/mingw64-webkitgtk3/$(echo '%1' | sed -e 's!/!.!g')

Name:           mingw-webkitgtk3
Version:        2.4.9
Release:        1%{?dist}
Summary:        MinGW Windows GTK+ Web content engine library

Group:          Development/Libraries
License:        LGPLv2+ and BSD
URL:            http://webkit.org/

Source0:        http://webkitgtk.org/releases/webkitgtk-%{version}.tar.xz
Patch0:         https://raw.githubusercontent.com/Alexpux/MINGW-packages/master/mingw-w64-webkitgtk3/0101-webkitgtk-2.4.3-gcc-asm.all.patch
BuildArch:      noarch

BuildRequires:  bison
BuildRequires:  flex
BuildRequires:  gettext
BuildRequires:  gperf
BuildRequires:  perl
BuildRequires:  perl(Getopt::Long)
BuildRequires:  ruby

# Required for glib-mkenums
BuildRequires:  glib2-devel

BuildRequires:  mingw32-binutils
BuildRequires:  mingw32-enchant
BuildRequires:  mingw32-filesystem >= 95
BuildRequires:  mingw32-fontconfig
BuildRequires:  mingw32-freetype
BuildRequires:  mingw32-gcc
BuildRequires:  mingw32-gcc-c++
BuildRequires:  mingw32-gstreamer1
BuildRequires:  mingw32-gstreamer1-plugins-base
BuildRequires:  mingw32-gtk3
BuildRequires:  mingw32-icu
BuildRequires:  mingw32-libidn
BuildRequires:  mingw32-libsoup
BuildRequires:  mingw32-libwebp
BuildRequires:  mingw32-libxml2
BuildRequires:  mingw32-libxslt
BuildRequires:  mingw32-pthreads
BuildRequires:  mingw32-sqlite

%if 0%{?mingw_build_win64} == 1
BuildRequires:  mingw64-binutils
BuildRequires:  mingw64-enchant
BuildRequires:  mingw64-filesystem >= 95
BuildRequires:  mingw64-fontconfig
BuildRequires:  mingw64-freetype
BuildRequires:  mingw64-gcc
BuildRequires:  mingw64-gcc-c++
BuildRequires:  mingw64-gstreamer1
BuildRequires:  mingw64-gstreamer1-plugins-base
BuildRequires:  mingw64-gtk3
BuildRequires:  mingw64-icu
BuildRequires:  mingw64-libidn
BuildRequires:  mingw64-libsoup
BuildRequires:  mingw64-libwebp
BuildRequires:  mingw64-libxml2
BuildRequires:  mingw64-libxslt
BuildRequires:  mingw64-pthreads
BuildRequires:  mingw64-sqlite
%endif

%description
WebKitGTK+ is an open-source Web content engine library.
This package contains the shared libraries for the WebKit GTK+ port
as well as the sample GtkLauncher tool.

This is the MinGW port of WebKitGTK+ for GTK+ 3.


%package -n mingw32-webkitgtk3
Summary:        MinGW Windows web content engine library

%description -n mingw32-webkitgtk3
WebKitGTK+ is an open-source Web content engine library.
This package contains the shared libraries for the WebKit GTK+ port
as well as the sample GtkLauncher tool.

This is the MinGW port of WebKitGTK+ for GTK+ 3.


%if 0%{?mingw_build_win64} == 1
%package -n mingw64-webkitgtk3
Summary:        MinGW Windows web content engine library

%description -n mingw64-webkitgtk3
WebKitGTK+ is an open-source Web content engine library.
This package contains the shared libraries for the WebKit GTK+ port
as well as the sample GtkLauncher tool.

This is the MinGW port of WebKitGTK+ for GTK+ 3.
%endif


%{?mingw_debug_package}


%prep
%setup -qn "webkitgtk-%{version}"
%patch0 -p1


%build
# lower debug level to prevent memory exhaustion by linker
%global mingw32_cflags %(echo %{mingw32_cflags} | sed 's/-g /-g1 /')
%global mingw64_cflags %(echo %{mingw64_cflags} | sed 's/-g /-g1 /')

%mingw_configure                                                \
                        --enable-win32-target                   \
                        --with-gtk=3.0                          \
                        --disable-accelerated-compositing       \
                        --disable-egl                           \
                        --disable-credential-storage            \
                        --disable-geolocation                   \
                        --disable-webkit2                       \
                        --disable-gtk-doc-html

%mingw_make %{?_smp_mflags} V=1


%install
%mingw_make install DESTDIR=%{buildroot}

install -m 755 build_win32/Programs/.libs/GtkLauncher.exe %{buildroot}%{mingw32_bindir}/GtkLauncher-3.exe
%if 0%{?mingw_build_win64} == 1
install -m 755 build_win64/Programs/.libs/GtkLauncher.exe %{buildroot}%{mingw64_bindir}/GtkLauncher-3.exe
%endif

# Drop all .la files
find $RPM_BUILD_ROOT -name "*.la" -delete

%mingw_find_lang WebKitGTK-3.0

## Copy over and rename the various files for %%doc inclusion.
%add_to_doc_files32 Source/WebKit/LICENSE
%add_to_doc_files32 Source/WebKit/gtk/NEWS
%add_to_doc_files32 Source/WebCore/icu/LICENSE
%add_to_doc_files32 Source/WebCore/LICENSE-APPLE
%add_to_doc_files32 Source/WebCore/LICENSE-LGPL-2
%add_to_doc_files32 Source/WebCore/LICENSE-LGPL-2.1
%add_to_doc_files32 Source/JavaScriptCore/COPYING.LIB
%add_to_doc_files32 Source/JavaScriptCore/THANKS
%add_to_doc_files32 Source/JavaScriptCore/AUTHORS
%add_to_doc_files32 Source/JavaScriptCore/icu/README
%add_to_doc_files32 Source/JavaScriptCore/icu/LICENSE

%if 0%{?mingw_build_win64} == 1
%add_to_doc_files64 Source/WebKit/LICENSE
%add_to_doc_files64 Source/WebKit/gtk/NEWS
%add_to_doc_files64 Source/WebCore/icu/LICENSE
%add_to_doc_files64 Source/WebCore/LICENSE-APPLE
%add_to_doc_files64 Source/WebCore/LICENSE-LGPL-2
%add_to_doc_files64 Source/WebCore/LICENSE-LGPL-2.1
%add_to_doc_files64 Source/JavaScriptCore/COPYING.LIB
%add_to_doc_files64 Source/JavaScriptCore/THANKS
%add_to_doc_files64 Source/JavaScriptCore/AUTHORS
%add_to_doc_files64 Source/JavaScriptCore/icu/README
%add_to_doc_files64 Source/JavaScriptCore/icu/LICENSE
%endif


%files -n mingw32-webkitgtk3 -f mingw32-WebKitGTK-3.0.lang
%{_docdir}/mingw32-webkitgtk3/
%{mingw32_bindir}/jsc-3.exe
%{mingw32_bindir}/GtkLauncher-3.exe
%{mingw32_bindir}/libjavascriptcoregtk-3.0-0.dll
%{mingw32_bindir}/libwebkitgtk-3.0-0.dll
%{mingw32_includedir}/webkitgtk-3.0/
%{mingw32_libdir}/libjavascriptcoregtk-3.0.dll.a
%{mingw32_libdir}/libwebkitgtk-3.0.dll.a
%{mingw32_libdir}/pkgconfig/javascriptcoregtk-3.0.pc
%{mingw32_libdir}/pkgconfig/webkitgtk-3.0.pc
%{mingw32_datadir}/webkitgtk-3.0/

%if 0%{?mingw_build_win64} == 1
%files -n mingw64-webkitgtk3 -f mingw64-WebKitGTK-3.0.lang
%{_docdir}/mingw64-webkitgtk3/
%{mingw64_bindir}/jsc-3.exe
%{mingw64_bindir}/GtkLauncher-3.exe
%{mingw64_bindir}/libjavascriptcoregtk-3.0-0.dll
%{mingw64_bindir}/libwebkitgtk-3.0-0.dll
%{mingw64_includedir}/webkitgtk-3.0/
%{mingw64_libdir}/libjavascriptcoregtk-3.0.dll.a
%{mingw64_libdir}/libwebkitgtk-3.0.dll.a
%{mingw64_libdir}/pkgconfig/javascriptcoregtk-3.0.pc
%{mingw64_libdir}/pkgconfig/webkitgtk-3.0.pc
%{mingw64_datadir}/webkitgtk-3.0/
%endif


%changelog
* Mon Aug 31 2015 Kalev Lember <klember@redhat.com> - 2.4.9-1
- Update to 2.4.9
- Temporarily disable the mingw64 build

* Wed Jun 17 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.2.8-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Tue Oct 14 2014 Kalev Lember <kalevlember@gmail.com> - 2.2.8-1
- Update to 2.2.8

* Sat Jun 07 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.2.7-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Thu May 22 2014 Kalev Lember <kalevlember@gmail.com> - 2.2.7-1
- Update to 2.2.7

* Wed Mar 26 2014 Erik van Pienbroek <epienbro@fedoraproject.org> - 2.2.6-1
- Update to 2.2.6
- Fix use-after-free in WTF threading code (WebKit bug #130122)

* Wed Dec  4 2013 Erik van Pienbroek <epienbro@fedoraproject.org> - 2.2.3-1
- Update to 2.2.3

* Fri Sep 20 2013 Erik van Pienbroek <epienbro@fedoraproject.org> - 2.0.4-2
- Rebuild against winpthreads

* Sun Aug 11 2013 Kalev Lember <kalevlember@gmail.com> - 2.0.4-1
- Update to 2.0.4
- Switch to unversioned docdirs (#993918)

* Sat Aug 03 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.0.3-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Sat Jul 13 2013 Erik van Pienbroek <epienbro@fedoraproject.org> - 2.0.3-4
- Rebuild against libpng 1.6

* Mon Jun 17 2013 Kalev Lember <kalevlember@gmail.com> - 2.0.3-3
- Rebuilt for mingw-icu 50

* Sun Jun 16 2013 Erik van Pienbroek <epienbro@fedoraproject.org> - 2.0.3-2
- Rebuild to resolve InterlockedCompareExchange regression in mingw32 libraries

* Tue Jun 11 2013 Kalev Lember <kalevlember@gmail.com> - 2.0.3-1
- Update to 2.0.3
- Drop upstreamed patches

* Sun Jun 09 2013 Kalev Lember <kalevlember@gmail.com> - 2.0.2-3
- Link with harfbuzz-icu (split into separate library in harfbuzz 0.9.18)

* Sun Jun  2 2013 Erik van Pienbroek <epienbro@fedoraproject.org> - 2.0.2-2
- Fix FTBFS caused by the changed pkg-config behavior in mingw-filesystem

* Mon May 27 2013 Paweł Forysiuk <tuxator@o2.pl> - 2.0.2-1
- Update to 2.0.2
- Drop upstreamed patches
- Backport patches for JIT on 64 bit machines
- Re-enable 64 bit build

* Sun Apr 07 2013 Kalev Lember <kalevlember@gmail.com> - 2.0.0-1
- Update to 2.0.0
- Switch to gstreamer1
- Add libwebp as a build dep
- Backport a number of patches to fix the Windows build
- Re-enable JIT

* Sun Jan 27 2013 Erik van Pienbroek <epienbro@fedoraproject.org> - 1.10.2-3
- Rebuild against mingw-gcc 4.8 (win64 uses SEH exceptions now)

* Wed Jan 02 2013 Erik van Pienbroek <erik-fedora@vanpienbroek.nl> - 1.10.2-2
- Rebuilt against mingw-icu 49

* Tue Dec 11 2012 Kalev Lember <kalevlember@gmail.com> - 1.10.2-1
- Update to 1.10.2

* Mon Nov 19 2012 Kalev Lember <kalevlember@gmail.com> - 1.10.1-2
- Disable the JIT to fix crashes in 32-bit builds

* Sat Oct 20 2012 Kalev Lember <kalevlember@gmail.com> - 1.10.1-1
- Update to 1.10.1
- Drop upstreamed patches
- Enable parallel make
- Adjust for webkit -> webkitgtk tarball rename

* Thu Aug 30 2012 Kalev Lember <kalevlember@gmail.com> - 1.8.3-1
- Update to 1.8.3

* Mon Aug 06 2012 Kalev Lember <kalevlember@gmail.com> - 1.8.2-1
- Update to 1.8.2
- Backport NPAPI plugin support from trunk
- Fix the build with spellchecking enabled (Paweł Forysiuk)
- Backport a build fix with bison 2.6

* Fri Jul 20 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 1.8.1-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Wed Jun 13 2012 Kalev Lember <kalevlember@gmail.com> - 1.8.1-1
- Initial mingw-webkitgtk3 packaging, based on Fedora mingw-webkitgtk
