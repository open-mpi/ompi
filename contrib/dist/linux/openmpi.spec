#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
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

%{!?lanl: %define lanl 0}

# Define this if you want to make this SRPM build in /opt/NAME/VERSION-RELEASE
# instead of the default /usr/
# type: bool (0/1)
%{!?install_in_opt: %define install_in_opt 0}

# Define this if you want this RPM to install environment setup
# scripts, currently either a modulefile or profile.d script.
# type: bool (0/1)
%{!?install_env_scripts: %define install_env_scripts 0}

# Should this drop a modulefile (if being used with enviornment modules)? 
# Specify the modulefile PATH you wish to use, or '0' for null (which will
# cause /etc/profile.d/ scripts to be created.
# note: This will only work if %{install_in_opt} is true.
# type: bool (0/1)
%{!?install_modulefile: %define install_modulefile 0}
# type: string (root path to install modulefiles)
%{!?modulefile_path: %define modulefile_path /etc/modulefiles}
# type: string (subdir to install modulefile)
%{!?modulefile_subdir: %define modulefile_subdir %{name}}
# type: string (name of modulefile)
%{!?modulefile_name: %define modulefile_name %{version}-%{release}}

# The name of the modules RPM.  Can vary from system to system.
# type: string (name of modules RPM)
%{!?modules_rpm_name: %define modules_rpm_name modules}

# Should we build a debuginfo RPM or not?
# type: bool (0/1)
%{!?build_debuginfo_rpm: %define build_debuginfo_rpm 0}

# Should we build an all-in-one RPM, or several sub-package RPMs?
# type: bool (0/1)
%{!?build_all_in_one_rpm: %define build_all_in_one_rpm 1}

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

%if !%{build_debuginfo_rpm}
%define debug_package %{nil}
%endif

%if %(test "%{_prefix}" = "/usr" && echo 1 || echo 0)
%global _sysconfdir /etc
%else
%global _sysconfdir %{_prefix}/etc
%endif

%{!?configure_options: %define configure_options %{nil}}

#############################################################################
#
# Preamble Section
#
#############################################################################

Summary: A powerful implementaion of MPI
Name: %{?_name:%{_name}}%{!?_name:openmpi}
Version: $VERSION
Release: 1
License: BSD
Group: Development/Libraries
Source: openmpi-%{version}.tar.$EXTENSION
Packager: %{?_packager:%{_packager}}%{!?_packager:%{_vendor}}
Vendor: %{?_vendorinfo:%{_vendorinfo}}%{!?_vendorinfo:%{_vendor}}
Distribution: %{?_distribution:%{_distribution}}%{!?_distribution:%{_vendor}}
Prefix: %{_prefix}
Provides: mpi
BuildRoot: /var/tmp/%{name}-%{version}-%{release}-root
%if %{install_modulefile}
Requires: %{modules_rpm_name}
%endif

%description
Open MPI is a project combining technologies and resources from
several other projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in
order to build the best MPI library available.

This RPM contains all the tools necessary to compile, link, and run
Open MPI jobs.

%if !%{build_all_in_one_rpm}

#############################################################################
#
# Preamble Section (runtime)
#
#############################################################################

%package runtime
Summary: Tools and plugin modules for running Open MPI jobs
Group: Development/Libraries
Provides: mpi

%description runtime
Open MPI is a project combining technologies and resources from several other
projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in order to build the best
MPI library available.

This subpackage provides general tools (mpirun, mpiexec, etc.) and the
Module Component Architecture (MCA) base and plugins necessary for
running Open MPI jobs.

%endif

#############################################################################
#
# Preamble Section (devel)
#
#############################################################################

%package devel
Summary: Development tools and header files for Open MPI
Group: Development/Libraries
Requires: openmpi-runtime

%description devel
Open MPI is a project combining technologies and resources from
several other projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in
order to build the best MPI library available.

This subpackage provides the development files for Open MPI, such as
wrapper compilers and header files for MPI development.

#############################################################################
#
# Preamble Section (docs)
#
#############################################################################

%package docs
Summary: Documentation for Open MPI
Group: Development/Documentation

%description docs
Open MPI is a project combining technologies and resources from several other
projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in order to build the best
MPI library available.

This subpackage provides the documentation for Open MPI.

#############################################################################
#
# Prepatory Section
#
#############################################################################
%prep
%setup -q -n openmpi-%{version}

#############################################################################
#
# Build Section
#
#############################################################################

%build

CFLAGS="%{?cflags:%{cflags}}%{!?cflags:$RPM_OPT_FLAGS}"
CXXFLAGS="%{?cxxflags:%{cxxflags}}%{!?cxxflags:$RPM_OPT_FLAGS}"
F77FLAGS="%{?f77flags:%{f77flags}}%{!?f7flags:$RPM_OPT_FLAGS}"
FCFLAGS="%{?fcflags:%{fcflags}}%{!?fcflags:$RPM_OPT_FLAGS}"
export CFLAGS CXXFLAGS F77FLAGS FCFLAGS

%configure %{configure_options}
%{__make} %{?mflags}


#############################################################################
#
# Install Section
#
#############################################################################
%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

# Currently remove a few executables that are not yet ready for prime
# time

rm -f "$RPM_BUILD_ROOT/%{_bindir}/openmpi"
rm -f "$RPM_BUILD_ROOT/%{_bindir}/orteconsole"
rm -f "$RPM_BUILD_ROOT/%{_bindir}/orteprobe"

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

# Build the files lists. Since the files are still not completly known,
# it is easier to do some all-emcompasing find's.
find $RPM_BUILD_ROOT -type f -o -type l | \
   sed -e "s@$RPM_BUILD_ROOT@@" |\
   egrep -v "README|INSTALL|LICENSE" > main.files

# Runtime files
find $RPM_BUILD_ROOT -type f -o -type l | \
   sed -e "s@$RPM_BUILD_ROOT@@" |\
   egrep "lib.*.so|mca.*so|etc/openmpi|share/openmpi/.*txt" > runtime.files

# Devel files
find $RPM_BUILD_ROOT -type f -o -type l | \
   sed -e "s@$RPM_BUILD_ROOT@@" |\
   egrep "lib.*\.a|lib.*\.la|/include/" > devel.files

# Docs files
# For when we have man pages / documentation
#find $RPM_BUILD_ROOT -type f -o -type l | \
#   sed -e "s@$RPM_BUILD_ROOT@@" |\
#   egrep "/man/" > docs.files


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

%if %{build_all_in_one_rpm}

#
# All in one RPM
#

%files -f main.files
%defattr(-, root, root)
%doc README INSTALL LICENSE

%else

#
# Sub-package RPMs
#

%files runtime -f runtime.files
%defattr(-, root, root)
%doc README INSTALL LICENSE
%{_bindir}/mpirun
%{_bindir}/mpiexec
%{_bindir}/ompi_info
%{_bindir}/orterun
%{_bindir}/orted

%files devel -f devel.files
%defattr(-, root, root)
%{_bindir}/mpicc
%{_bindir}/mpiCC
%{_bindir}/mpic++
%{_bindir}/mpicxx
%{_bindir}/mpif77
%{_bindir}/mpif90

# For when we have documentation
#% files docs -f docs.files
#% defattr(-, root, root)

%endif


#############################################################################
#
# Changelog
#
#############################################################################
%changelog
* Wed Mar 30 2006 Jeff Squyres <jsquyres@cisco.com>
- Lots of bit rot updates
- Reorganize and rename the subpackages
- Add / formalize a variety of rpmbuild --define options
- Comment out the docs subpackage for the moment (until we have some
  documentation -- coming in v1.1!)

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

