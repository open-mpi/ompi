Name: libfabric
Version: 1.0.0rc5
Release: 1%{?dist}
Summary: User-space RDMA Fabric Interfaces
Group: System Environment/Libraries
License: GPLv2 or BSD
Url: http://www.github.com/ofiwg/libfabric
Source: http://www.openfabrics.org/downloads/fabrics/%{name}-%{version}.tar.bz2
Prefix: ${_prefix}

BuildRequires: libnl-devel
BuildRequires: librdmacm-devel
BuildRequires: libibverbs-devel
BuildRequires: infinipath-psm-devel

%description
libfabric provides a user-space API to access high-performance fabric
services, such as RDMA.

%package devel
Summary: Development files for the libfabric library
Group: System Environment/Libraries
Requires: libfabric = %{version}

%description devel
Development files for the libfabric library.

%prep
%setup -q -n %{name}-%{version}

%build
# defaults: with-dlopen and without-valgrind can be over-rode:
%configure %{?_without_dlopen} %{?_with_valgrind} \
	--enable-sockets --enable-verbs --enable-usnic --enable-psm
make %{?_smp_mflags}

%install
rm -rf %{buildroot}
%makeinstall installdirs
# remove unpackaged files from the buildroot
rm -f %{buildroot}%{_libdir}/*.la

%clean
rm -rf %{buildroot}

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%{_libdir}/lib*.so.*
%dir %{_libdir}/libfabric/
%doc AUTHORS COPYING README

%files devel
%defattr(-,root,root)
%{_libdir}/libfabric*.so
%{_libdir}/*.a
%{_includedir}/*
%{_mandir}/man3/*
%{_mandir}/man7/*

%changelog
* Tue Apr 1 2015 Open Fabrics Interfaces Working Group <ofiwg@lists.openfabrics.org> 1.0.0
- Release 1.0.0rc5
