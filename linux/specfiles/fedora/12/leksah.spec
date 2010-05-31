%global pkg_name leksah

%bcond_without doc
%bcond_without prof

# ghc does not emit debug information
%global debug_package %{nil}

Name:           %{pkg_name}
Version:        0.8.0.6
Release:        1%{?dist}
Summary:        Haskell IDE

Group:          System Environment/Languages
License:        GPL
URL:            http://code.haskell.org/leksah/
Source0:        %{pkg_name}-%{version}.tar.gz
# fedora ghc archs:
ExclusiveArch:  %{ix86} x86_64 ppc alpha
BuildRequires:  ghc >= 6.10.1, ghc-rpm-macros >= 0.2.5, ghc-gtk2hs-compat >= 0.10.1, ghc-utf8-string-devel >= 0.3.1.1, ghc-haskell-platform-devel,ghc-gtksourceview2-devel,ghc-gtk-devel,ghc-glib-devel, ghc-binary-shared-devel, haddock-leksah, leksah-server == 0.8.0.6, leksah-server-devel == 0.8.0.6, ghc-regex-tdfa-devel >= 1.1.0, ghc-regex-base-devel >= 0.93
%if %{with doc}
BuildRequires:  ghc-doc
%endif
%if %{with prof}
BuildRequires:  ghc-prof
%endif
Requires: atk, glibc, cairo, fontconfig, freetype, gtk2, glib2, gmp, gtksourceview2, pango, leksah-server == 0.8.0.6

%description
Haskell IDE

%if %{with doc}
%package doc
Summary:        Documentation for %{name}
Group:          Development/Libraries
Requires:       ghc-doc = %{ghc_version}
Requires(post): ghc-doc = %{ghc_version}
Requires(postun): ghc-doc = %{ghc_version}

%description doc
This package contains development documentation files
for the %{name} library.
%endif


%if %{with prof}
%package prof
Summary:        Profiling libraries for %{name}
Group:          Development/Libraries
Requires:       %{name}-devel = %{version}-%{release}
Requires:       ghc-prof = %{ghc_version}

%description prof
This package contains profiling libraries for %{name}
built for ghc-%{ghc_version}.
%endif

%prep
%setup -q
%cabal_configure


%build
%cabal_configure --ghc %{?with_prof:-p}
%cabal build
%if %{with doc}
%cabal haddock
%endif

%install
%cabal_install
mkdir $RPM_BUILD_ROOT/usr/share/applications
cp linux/applications/leksah.desktop $RPM_BUILD_ROOT/usr/share/applications
mkdir -p $RPM_BUILD_ROOT/usr/share/icons/hicolor/48x48/apps
cp linux/icons/hicolor/48x48/apps/leksah_48x48.png $RPM_BUILD_ROOT/usr/share/icons/hicolor/48x48/apps

%clean
rm -rf $RPM_BUILD_ROOT

%post
gtk-update-icon-cache

%postun
gtk-update-icon-cache

%if %{with doc}
%post doc
%ghc_reindex_haddock
%endif


%if %{with doc}
%postun doc
if [ "$1" -eq 0 ] ; then
  %ghc_reindex_haddock
fi
%endif

%files
%defattr(-,root,root,-)
%doc LICENSE
%attr(755,root,root) %{_bindir}/%{name}
%attr(755,root,root) %{_datadir}/%{pkg_name}-%{version}
%attr(644,root,root) /usr/share/applications/%{pkg_name}.desktop
%attr(644,root,root) /usr/share/icons/hicolor/48x48/apps/leksah_48x48.png

%changelog
* Fri May 28 2010 <lakshminaras2002@gmail.com>
- Upgrade to leksah 0.8.0.6

* Fri Apr 09 2010 <lakshminaras2002@gmail.com>
- Updated to 0.8.0.4
- Modified dependencies to accommodate split of leksah-server
- Added macros for prof and doc rpms
- Currently no prof or doc files are generated 

* Mon Feb 08 2010 <lakshminaras2002@gmail.com>
- Initial Version for Fedora 12
