%global pkg_name leksah

%bcond_without doc
%bcond_without prof
%bcond_without shared

# ghc does not emit debug information
%global debug_package %{nil}

Name:           %{pkg_name}
Version:        0.8.0.1
Release:        1%{?dist}
Summary:        Haskell IDE

Group:          System Environment/Languages
License:        BSD
URL:            http://code.haskell.org/leksah/
Source0:        %{pkg_name}-%{version}.tar.gz
# fedora ghc archs:
ExclusiveArch:  %{ix86} x86_64 ppc alpha
BuildRequires:  ghc >= 6.10.1, ghc-rpm-macros >= 0.2.5, ghc-gtk2hs-compat >= 0.10.1, ghc-utf8-string-devel >= 0.3.1.1, ghc-haskell-platform-devel,ghc-gtksourceview2-devel,ghc-gtk-devel,ghc-glib-devel, ghc-binary-shared-devel, haddock-leksah, leksah-server == 0.8.0.1
%if %{with doc}
BuildRequires:  ghc-doc
%endif
%if %{with prof}
BuildRequires:  ghc-prof
%endif
Requires: atk, glibc, cairo, fontconfig, freetype, gtk2, glib2, gmp, gtksourceview2, pango, leksah-server == 0.8.0.1

%description
Haskell IDE

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
cp applications/leksah.desktop $RPM_BUILD_ROOT/usr/share/applications
mkdir -p $RPM_BUILD_ROOT/usr/share/icons/hicolor/48x48/apps
cp icons/hicolor/48x48/apps/leksah_48x48.png $RPM_BUILD_ROOT/usr/share/icons/hicolor/48x48/apps

%clean
rm -rf $RPM_BUILD_ROOT

%post
gtk-update-icon-cache

%postun
gtk-update-icon-cache

%files
%defattr(-,root,root,-)
%doc LICENSE
%attr(755,root,root) %{_bindir}/%{name}
%attr(755,root,root) %{_datadir}/%{pkg_name}-%{version}
%attr(644,root,root) /usr/share/applications/%{pkg_name}.desktop
%attr(644,root,root) /usr/share/icons/hicolor/48x48/apps/leksah_48x48.png

%changelog
* Mon Feb 08 2010 <lakshminaras2002@gmail.com>
- Initial Version for Fedora 12
