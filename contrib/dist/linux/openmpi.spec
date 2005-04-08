#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
############################################################################
#
# Copyright (c) 2003, The Regents of the University of California, through
# Lawrence Berkeley National Laboratory (subject to receipt of any
# required approvals from the U.S. Dept. of Energy).  All rights reserved.
#
# Written and maintained by:
#       Greg Kurtzer, <gmkurtzer@lbl.gov>
#
############################################################################


#############################################################################
#
# Configuration Options
#
#############################################################################

# Define this if you want to make this SRPM build in /opt/NAME/VERSION-RELEASE
# instead of the default /usr/
# type: bool (0/1)
%define install_in_opt	1

# Should this drop a modulefile (if being used with enviornment modules)? 
# Specify the modulefile PATH you wish to use, or '0' for null (which will
# cause /etc/profile.d/ scripts to be created.
# note: This will only work if %{install_in_opt} is true.
# type: bool (0/1)
%define use_modulefile 1
# type: string (path to install modulefiles)
%define modulepath /etc/modulefiles/


#############################################################################
#
# Configuration Logic
#
#############################################################################

%if %{install_in_opt}
%define _prefix /opt/%{name}/%{version}-%{release}
%define _sysconfdir /opt/%{name}/%{version}-%{release}/etc
%define _libdir /opt/%{name}/%{version}-%{release}/lib
%define _includedir /opt/%{name}/%{version}-%{release}/include
%endif

#############################################################################
#
# Preamble Section
#
#############################################################################

Summary: A powerful implementaion of MPI
Name: openmpi
Version: 10.0a1r5001
Release: 1
License: BSD
Group: FIXME/SetThis
Source: openmpi-10.0a1r5001.tar.bz2
Packager: %{?_packager:%{_packager}}%{!?_packager:%{_vendor}}
Vendor: %{?_vendorinfo:%{_vendorinfo}}%{!?_vendorinfo:%{_vendor}}
Distribution: %{?_distribution:%{_distribution}}%{!?_distribution:%{_vendor}}
Prefix: %{_prefix}
BuildRoot: /var/tmp/%{name}-%{version}-%{release}-root
%if %{use_modulefile}
Requires: modules
%endif

%description
Open MPI is a project combining technologies and resources from several other
projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in order to build the best
MPI library available.

#############################################################################
#
# Preamble Section (devel)
#
#############################################################################

%package devel
Summary: Development components for OpenMPI
Group: Development/Libraries
Provides: mpi

%description devel
Open MPI is a project combining technologies and resources from several other
projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in order to build the best
MPI library available.

This subpackage provides the development components for OpenMPI.

#############################################################################
#
# Preamble Section (docs)
#
#############################################################################

%package docs
Summary: Documentation for OpenMPI
Group: Development/Documentation

%description docs
Open MPI is a project combining technologies and resources from several other
projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in order to build the best
MPI library available.

This subpackage provides the documentation for OpenMPI.

#############################################################################
#
# Preamble Section (mca-general)
#
#############################################################################

# First conversations with Jeff and we were going to do this, but later we
# decided that since these are pretty much always needed, they should be
# included in the main pacakge. I am leaving this here just incase there
# is a reason to re-include.

%package mca-general
Summary: General communication modules for OpenMPI
Group: Development/Libraries

%description mca-general
Open MPI is a project combining technologies and resources from several other
projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in order to build the best
MPI library available.

This subpackage provides the general "MCA"s -- Modular Component Architecture 
modules

#############################################################################
#
# Prepatory Section
#
#############################################################################
%prep
%setup -q 

#############################################################################
#
# Build Section
#
#############################################################################

%build

CFLAGS="%{?cflags:%{cflags}}%{!?cflags:$RPM_OPT_FLAGS}"
CXXFLAGS="%{?cxxflags:%{cxxflags}}%{!?cflags:$RPM_OPT_FLAGS}"
export CFLAGS CXXFLAGS

%configure %{?acflags}
%{__make} %{?mflags}


#############################################################################
#
# Install Section
#
#############################################################################
%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

%if %{install_in_opt}
# An attempt to make enviornment happier when installed into non /usr path
%if %{use_modulefile}
%{__mkdir_p} $RPM_BUILD_ROOT/%{modulepath}/%{name}/
cat <<EOF >$RPM_BUILD_ROOT/%{modulepath}/%{name}/%{version}-%{release}
#%Module
proc ModulesHelp { } {
   puts stderr "This module adds Open MPI (%{version}-%{release}) to various paths"
}
module-whatis   "Sets up Open MPI in your enviornment"
append-path PATH "%{_prefix}/bin/"
append-path LD_LIBRARY_PATH %{_libdir}
append-path MANPATH %{_mandir}
EOF
%else
%{__mkdir_p} $RPM_BUILD_ROOT/etc/profile.d/
cat <<EOF > $RPM_BUILD_ROOT/etc/profile.d/%{name}-%{version}-%{release}.sh
PATH=\${PATH}:%{_prefix}/bin/
LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:%{_libdir}
MANPATH=\${MANPATH}:%{_mandir}
export PATH LD_LIBRARY_PATH MANPATH
EOF
cat <<EOF > $RPM_BUILD_ROOT/etc/profile.d/%{name}-%{version}-%{release}.csh
setenv PATH \${PATH}:%{_prefix}/bin/
setenv LD_LIBRARY_PATH \${LD_LIBRARY_PATH}:%{_libdir}
setenv MANPATH \${MANPATH}:%{_mandir}
EOF
%endif
%endif

# Build the files lists. Since the files are still not completly known to me,
# it is easier to do some all-emcompasing find's.
find $RPM_BUILD_ROOT -type f | \
   sed -e "s@$RPM_BUILD_ROOT@@" |\
   grep -v "man" |\
   grep -v ".la" |\
   grep -v "include" > main.files

find $RPM_BUILD_ROOT -type f | \
   sed -e "s@$RPM_BUILD_ROOT@@" |\
   egrep ".la|include" > devel.files


#############################################################################
#
# Clean Section
#
#############################################################################
%clean
test "x$RPM_BUILD_ROOT" != "x" && rm -rf $RPM_BUILD_ROOT


#############################################################################
#
# Post (Un)Install Section
#
#############################################################################
%post
# Stub

%postun
# Stub

#############################################################################
#
# Files Section
#
#############################################################################
%files -f main.files
%defattr(-, root, root)
%doc README INSTALL LICENSE 

%files devel -f devel.files
%defattr(-, root, root)


#############################################################################
#
# Changelog
#
#############################################################################
%changelog
* Thu Apr 07 2005 Greg Kurtzer <GMKurtzer@lbl.gov>
- Added opt building
- Added profile.d/modulefile logic and creation
- Minor cleanups

* Fri Apr 01 2005 Greg Kurtzer <GMKurtzer@lbl.gov>
- Added comments
- Split package into subpackages
- Cleaned things up a bit
- Sold the code to Microsoft, and now I am retiring. Thanks guys!

* Wed Mar 23 2005 Mezzanine <mezzanine@kainx.org>
- Specfile auto-generated by Mezzanine

