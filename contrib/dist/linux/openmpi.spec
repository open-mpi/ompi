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

# Part of the purpose of this specfile is to help Los Alamos National
# Labs (LANL), so we're going to put in a bunch of defaults for them.

%define lanl 0

# Define this if you want to make this SRPM build in /opt/NAME/VERSION-RELEASE
# instead of the default /usr/
# type: bool (0/1)
%define install_in_opt	0

# Define this if you want this RPM to install environment setup
# scripts, currently either a modulefile or profile.d script.
# type: bool (0/1)
%define install_env_scripts 0

# Should this drop a modulefile (if being used with enviornment modules)? 
# Specify the modulefile PATH you wish to use, or '0' for null (which will
# cause /etc/profile.d/ scripts to be created.
# note: This will only work if %{install_in_opt} is true.
# type: bool (0/1)
%define install_modulefile 0
# type: string (root path to install modulefiles)
%define modulefile_path /etc/modulefiles
# type: string (subdir to install modulefile)
%define modulefile_subdir %{name}
# type: string (name of modulefile)
%define modulefile_name %{version}-%{release}

# The name of the modules RPM.  Can vary from system to system.
# type: string (name of modules RPM)
%define modules_rpm_name modules


#############################################################################
#
# LANL-specific defaults
#
#############################################################################

%if %{lanl}
%define install_in_opt 1
%define install_env_scripts 1
%define install_modulefile 1
%define modulefile_path /usr/share/modules/modulefiles/mpi/openmpi-%{version}
%define modulefile_path_subdir mpi
%define modulefile_name %{name}-%{version}
%define modules_rpm_name environment-modules
%endif


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
Version: $VERSION
Release: 1
License: BSD
Group: Development/Libraries
Source: openmpi-%{version}.tar.bz2
Packager: %{?_packager:%{_packager}}%{!?_packager:%{_vendor}}
Vendor: %{?_vendorinfo:%{_vendorinfo}}%{!?_vendorinfo:%{_vendor}}
Distribution: %{?_distribution:%{_distribution}}%{!?_distribution:%{_vendor}}
Prefix: %{_prefix}
BuildRoot: /var/tmp/%{name}-%{version}-%{release}-root
%if %{module_modulefile}
Requires: %{modules_rpm_name}
%endif

%description
Open MPI is a project combining technologies and resources from
several other projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in
order to build the best MPI library available.

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
Open MPI is a project combining technologies and resources from
several other projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in
order to build the best MPI library available.

This subpackage provides the development files for Open MPI, such as
header files for MPI development.

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

This subpackage provides the documentation for Open MPI.

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

This subpackage provides the general Module Component Architecture
(MCA) components.

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

# An attempt to make enviornment happier when installed into non /usr path

%if %{install_env_scripts}

%if %{install_modulefile}
%{__mkdir_p} $RPM_BUILD_ROOT/%{modulefile_path}/%{modulefile_subdir}/
cat <<EOF >$RPM_BUILD_ROOT/%{modulefile_path}/%{modulefile_subdir}/%{modulefile_name}
#%Module

# NOTE: This is an automatically-generated file!  (generated by the
# Open MPI RPM).  Any changes made here will be lost a) if the RPM is
# uninstalled, or b) if the RPM is upgraded or uninstalled.

proc ModulesHelp { } {
   puts stderr "This module adds Open MPI (%{version}-%{release}) to various paths"
}

module-whatis   "Sets up Open MPI in your enviornment"

append-path PATH "%{_prefix}/bin/"
append-path LD_LIBRARY_PATH %{_libdir}
append-path MANPATH %{_mandir}
%if %{lanl}
setenv MPI_ROOT %{_prefix}
setenv MPIHOME %{_prefix}
# These flags are now obsolete -- use mpicc (etc.)
setenv MPI_LD_FLAGS ""
setenv MPI_COMPILE_FLAGS ""
%endif
EOF
%else
%{__mkdir_p} $RPM_BUILD_ROOT/etc/profile.d/
cat <<EOF > $RPM_BUILD_ROOT/etc/profile.d/%{name}-%{version}-%{release}.sh
# NOTE: This is an automatically-generated file!  (generated by the
# Open MPI RPM).  Any changes made here will be lost a) if the RPM is
# uninstalled, or b) if the RPM is upgraded or uninstalled.

CHANGED=0
if test -z "`echo $PATH | grep %{_prefix}/bin`"; then
    PATH=\${PATH}:%{_prefix}/bin/
    CHANGED=1
fi
if test -z "`echo $LD_LIBRARY_PATH | grep %{_libdir}`"; then
    LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:%{_libdir}
    CHANGED=1
fi
if test -z "`echo $MANPATH | grep %{_mandir}`"; then
    MANPATH=\${MANPATH}:%{_mandir}
    CHANGED=1
fi
if test "$CHANGED" = "1"; then
    export PATH LD_LIBRARY_PATH MANPATH
fi
EOF
cat <<EOF > $RPM_BUILD_ROOT/etc/profile.d/%{name}-%{version}-%{release}.csh
# NOTE: This is an automatically-generated file!  (generated by the
# Open MPI RPM).  Any changes made here will be lost a) if the RPM is
# uninstalled, or b) if the RPM is upgraded or uninstalled.

if ("`echo $PATH | grep %{_prefix}/bin`") then
    setenv PATH \${PATH}:%{_prefix}/bin/
endif
if ("$?LD_LIBRARY_PATH") then
    if ("`echo $LD_LIBRARY_PATH | grep %{_libdir}`") then
        setenv LD_LIBRARY_PATH \${LD_LIBRARY_PATH}:%{_libdir}
    endif
endif
if ("$?MANPATH") then
    if ("`echo $MANPATH | grep %{_mandir}`") then
        setenv MANPATH \${MANPATH}:%{_mandir}
    endif
endif
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
   egrep "lib\*.a|lib\*.so|.la|include" > devel.files

find $RPM_BUILD_ROOT -type f | \
   sed -e "s@$RPM_BUILD_ROOT@@" |\
   egrep "/man/" > man.files

find $RPM_BUILD_ROOT -type f | \
   sed -e "s@$RPM_BUILD_ROOT@@" |\
   egrep "mca\*so" > mca.files


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

%files man -f man.files
%defattr(-, root, root)

%files mca-general -f mca.files
%defattr(-, root, root)


#############################################################################
#
# Changelog
#
#############################################################################
%changelog
* Wed May 03 2005 Jeff Squyres <jsquyres@open-mpi.org>
- Added some defines for LANL defaults
- Added more defines for granulatirty of installation location for
  modulefile
- Differentiate between installing in /opt and whether we want to
  install environment script files
- Filled in files for man and mca-general subpackages

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

