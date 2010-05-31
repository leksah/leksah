%global pkg_name leksah-server

%bcond_without doc
%bcond_without prof
%bcond_without shared

# ghc does not emit debug information
%global debug_package %{nil}

Name:           %{pkg_name}
Version:        0.8.0.6
Release:        1%{?dist}
Summary:        Haskell IDE

Group:          System Development/IDE
License:        GPL
URL:            http://code.haskell.org/leksah/leksah-server
Source0:        %{pkg_name}-%{version}.tar.gz
# fedora ghc archs:
ExclusiveArch:  %{ix86} x86_64 ppc alpha
BuildRequires:  ghc = %{ghc_version}, ghc-rpm-macros >= 0.2.5, ghc-gtk2hs-compat >= 0.10.1
%if %{with doc}
BuildRequires:  ghc-doc
%endif
%if %{with prof}
BuildRequires:  ghc-prof
%endif
Requires: atk, glibc, cairo, fontconfig, freetype, gtk2, glib2, gmp, gtksourceview2, pango, bash, ghc, cabal-install

%description
Haskell IDE

%package devel
Summary:        Haskell IDE leksah-server development files
Group:          Development/Libraries
BuildRequires:  ghc = %{ghc_version}, ghc-rpm-macros >= 0.2.5, ghc-utf8-string-devel >= 0.3.1.1, ghc-haskell-platform-devel,ghc-gtksourceview2-devel,ghc-gtk-devel,ghc-glib-devel, ghc-deepseq-devel, ghc-deepseq-prof, ghc-hslogger-devel >= 1.0.10, ghc-ltk-devel >= 0.8, ghc-network-devel >= 2.2.1.4, ghc-binary-devel >= 0.5.0.2, ghc-binary-shared-devel >= 0.8, haddock-leksah >= 2.5.0, ghc-process-leksah-devel >= 1.0.1.3,  ghc-hslogger-prof >= 1.0.10, ghc-process-leksah-prof >= 1.0.1.3, ghc-ltk-prof >= 0.8, ghc-binary-shared-prof >= 0.8, ghc-network-prof >= 2.2.1.4, ghc-binary-prof >= 0.5.0.2
Requires:       ghc = %{ghc_version}, ghc-hslogger-devel >= 1.0.10, ghc-ltk-devel >= 0.8, ghc-network-devel >= 2.2.1.4, ghc-binary-devel >= 0.5.0.2, ghc-binary-shared-devel >= 0.8, ghc-deepseq-devel >= 1.1.0.0, haddock-leksah >= 2.5.0, cabal-install, ghc-process-leksah-devel >= 1.0.1.3
Requires(post): ghc = %{ghc_version}
Requires(postun): ghc = %{ghc_version}

%description devel
This package contains development files for leksah-server

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

%post devel
%ghc_register_pkg 

%preun devel
if [ "$1" -eq 0 ] ; then
  %ghc_unregister_pkg
fi

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
%attr(755,root,root) %{_bindir}/leksahecho
%attr(755,root,root) %{_datadir}/%{pkg_name}-%{version}

%files devel -f %{name}-devel.files
%attr(755,root,root) %{_libdir}/ghc-%{ghc_version}/leksah-server-%{version}
%attr(755,root,root) %{_docdir}/ghc/libraries/leksah-server

%if %{with doc}
%files doc -f %{name}-doc.files
%defattr(-,root,root,-)
%endif

%if %{with prof}
%files prof -f %{name}-prof.files
%defattr(-,root,root,-)
%endif

%changelog
* Fri Apr 09 2010 <lakshminaras2002@gmail.com>
- Split leksah server into two rpms. The base rpm contains only
- the leskah-server, leksahecho binaries. The devel rpm contains
- the pa files. The installation of this rpm will require the 
- other dependencies used in %ghc_register_pkg. This leads to 
- smaller rpm size for users who only need leskah for their 
- development work.
- Added ghc-process-leksah dependency.Fixed profiling requirements
- for building the rpm
- Added if macros for doc and prof rpms

* Mon Feb 08 2010 <lakshminaras2002@gmail.com>
- Initial Version for Fedora 12
