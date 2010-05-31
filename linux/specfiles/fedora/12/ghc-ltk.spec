%global pkg_name ltk

%bcond_without doc
%bcond_without prof
%bcond_without shared

# ghc does not emit debug information
%global debug_package %{nil}

Name:           ghc-%{pkg_name}
Version:        0.8
Release:        1%{?dist}
Summary:         UI Framework used by leksah

Group:          System Environment/Libraries
License:        GPL
URL:            http://hackage.haskell.org/package/%{pkg_name}-%{version}
Source0:        %{pkg_name}-%{version}.tar.gz
# fedora ghc archs:
ExclusiveArch:  %{ix86} x86_64 ppc alpha
BuildRequires:  ghc, ghc-rpm-macros
%if %{with doc}
BuildRequires:  ghc-doc
%endif
%if %{with prof}
BuildRequires:  ghc-prof
%endif 

%description
UI Framework used by leksah

%package devel
Summary:        Haskell ltk implementation
Group:          Development/Libraries
Requires:       ghc = %{ghc_version}, ghc-glib-devel >= 0.10.1, ghc-gtk-devel >= 0.10.1
Requires(post): ghc = %{ghc_version}
Requires(postun): ghc = %{ghc_version}

%description devel
ghc-ltk library

This package contains the development files for %{name}
built for ghc-%{ghc_version}.


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
%setup -q -n %{pkg_name}-%{version}


%build
%cabal_configure --ghc %{?with_prof:-p}
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


%preun devel
if [ "$1" -eq 0 ] ; then
  %ghc_unregister_pkg
fi

%files devel -f %{name}-devel.files
%defattr(-,root,root,-)
%{_docdir}/%{name}-%{version}

%if %{with doc}
%files doc -f %{name}-doc.files
%defattr(-,root,root,-)
%endif

%if %{with prof}
%files prof -f %{name}-prof.files
%defattr(-,root,root,-)
%endif


%changelog
* Wed Dec 23 2009 Lakshmi Narasimhan <lakshminaras202@gmail.com> - 1.0.7
- Initial version for Fedora 12

