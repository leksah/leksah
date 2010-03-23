%global pkg_name leksah-server

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
URL:            http://code.haskell.org/leksah/leksah-server
Source0:        %{pkg_name}-%{version}.tar.gz
# fedora ghc archs:
ExclusiveArch:  %{ix86} x86_64 ppc alpha
BuildRequires:  ghc >= 6.10.1, ghc-rpm-macros >= 0.2.5, ghc-gtk2hs-compat >= 0.10.1, ghc-utf8-string-devel >= 0.3.1.1, ghc-haskell-platform-devel,ghc-gtksourceview2-devel,ghc-gtk-devel,ghc-glib-devel, ghc-binary-shared-devel, ghc-deepseq-devel, ghc-deepseq-prof, ghc-hslogger-devel >= 1.0.10, ghc-ltk-devel >= 0.8, ghc-network-devel >= 2.2.1.4, ghc-binary-devel >= 0.5.0.2, ghc-binary-shared-devel >= 0.8, haddock-leksah >= 2.5.0 
%if %{with doc}
BuildRequires:  ghc-doc
%endif
%if %{with prof}
BuildRequires:  ghc-prof
%endif
Requires: atk, glibc, cairo, fontconfig, freetype, gtk2, glib2, gmp, gtksourceview2, pango, bash, ghc-hslogger-devel >= 1.0.10, ghc-ltk-devel >= 0.8, ghc-network-devel >= 2.2.1.4, ghc-binary-devel >= 0.5.0.2, ghc-binary-shared-devel >= 0.8, ghc-deepseq-devel >= 1.1.0.0, haddock-leksah >= 2.5.0, ghc, cabal-install

%description
Haskell IDE

%prep
%setup -q
%cabal_configure --ghc %{?with_prof:-p}


%build
%cabal build
%ghc_gen_scripts
%if %{with doc}
%cabal haddock
%endif

%install
%cabal_install
%ghc_install_scripts
%ghc_gen_filelists %{name}

%clean
rm -rf $RPM_BUILD_ROOT

%post
%ghc_register_pkg

%preun
if [ "$1" -eq 0 ] ; then
  %ghc_unregister_pkg
fi

%files
%defattr(-,root,root,-)
%doc LICENSE
%attr(755,root,root) %{_bindir}/%{name}
%attr(755,root,root) %{_bindir}/leksahecho
%attr(755,root,root) %{_datadir}/%{pkg_name}-%{version}
%attr(755,root,root) %{_libdir}/ghc-%{ghc_version}/leksah-server-%{version}
%attr(755,root,root) %{_docdir}/ghc/libraries/leksah-server

%changelog
* Mon Feb 08 2010 <lakshminaras2002@gmail.com>
- Initial Version for Fedora 12
