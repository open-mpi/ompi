%define ver 0.0.2

Name: libfabric
Version: 0.0.2
Release: 1%{?dist}
Summary: Userspace RDMA Fabric Interfaces

Group: System Environment/Libraries
License: GPLv2 or BSD
Url: http://www.github.com/ofiwg/libfabric
Source: http://www.openfabrics.org/downloads/fabrics/%{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

%description
libfabric provides a userspace API to access high-performance fabric
services, such as RDMA.

%package devel
Summary: Development files for the libfabric library
Group: System Environment/Libraries

%description devel
Development files for the libfabric library.

%package utils
Summary: Examples for the libfabric library
Group: System Environment/Libraries
Requires: %{name} = %{version}-%{release}

%description utils
Example test programs for the libfabric library.

%prep
%setup -q -n %{name}-%{ver}

%build
%configure
make %{?_smp_mflags}

%install
rm -rf $RPM_BUILD_ROOT
%makeinstall
# remove unpackaged files from the buildroot
rm -f $RPM_BUILD_ROOT%{_libdir}/*.la

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%{_libdir}/lib*.so.*
%doc AUTHORS COPYING README

%files devel
%defattr(-,root,root)
%{_libdir}/libfabric*.so
%{_libdir}/*.a
%{_includedir}/*
%{_mandir}/man3/*
%{_mandir}/man7/*

%files utils
%defattr(-,root,root,-)
%{_bindir}/*
%{_mandir}/man1/*

%changelog

