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
# Initially written by:
#       Greg Kurtzer, <gmkurtzer@lbl.gov>
#
############################################################################


#############################################################################
#
# Configuration Options
#
#############################################################################

# Help for OSCAR RPMs

%{!?oscar: %define oscar 0}

# Help for OFED RPMs

%{!?ofed: %define ofed 0}

# Define this if you want to make this SRPM build in /opt/NAME/VERSION-RELEASE
# instead of the default /usr/
# type: bool (0/1)
%{!?install_in_opt: %define install_in_opt 0}

# Define this if you want this RPM to install environment setup
# scripts in /etc/profile.d.  Only used if install_in_opt is true.
# type: bool (0/1)
%{!?install_profile_d_scripts: %define install_profile_d_scripts 0}

# Define this to 1 if you want this RPM to install a modulefile.  Only
# used if install_in_opt is true.
# type: bool (0/1)
%{!?install_modulefile: %define install_modulefile 0}
# type: string (root path to install modulefiles)
%{!?modulefile_path: %define modulefile_path /etc/modulefiles}
# type: string (subdir to install modulefile)
%{!?modulefile_subdir: %define modulefile_subdir %{name}}
# type: string (name of modulefile)
%{!?modulefile_name: %define modulefile_name %{version}}

# The name of the modules RPM.  Can vary from system to system.
# type: string (name of modules RPM)
%{!?modules_rpm_name: %define modules_rpm_name modules}

# Should we build a debuginfo RPM or not?
# type: bool (0/1)
%{!?build_debuginfo_rpm: %define build_debuginfo_rpm 0}

# Should we build an all-in-one RPM, or several sub-package RPMs?
# type: bool (0/1)
%{!?build_all_in_one_rpm: %define build_all_in_one_rpm 1}

# Should we leave the BUILD_ROOT around?  Default: no
# type: bool (0/1)
%{!?leave_build_root: %define leave_build_root 0}

# Should we use the default "check_files" RPM step (i.e., check for
# unpackaged files)?  It is discouraged to disable this, but some
# installers need it (e.g., OFED, because it installs lots of other
# stuff in the BUILD_ROOT before Open MPI).
%{!?use_check_files: %define use_check_files 1}


#############################################################################
#
# OSCAR-specific defaults
#
#############################################################################

%if %{oscar}
%define install_in_opt 1
%define install_modulefile 1
%define modulefile_path /opt/modules/modulefiles
%define modulefile_subdir openmpi
%define modulefile_name %{name}-%{version}
%define modules_rpm_name modules-oscar
%endif


#############################################################################
#
# OFED-specific defaults
#
#############################################################################

%if %{ofed}
%define leave_build_root 1
%define use_check_files 0
%endif


#############################################################################
#
# Configuration Logic
#
#############################################################################

%if %{install_in_opt}
%define _prefix /opt/%{name}/%{version}
%define _sysconfdir /opt/%{name}/%{version}/etc
%define _libdir /opt/%{name}/%{version}/lib
%define _includedir /opt/%{name}/%{version}/include
%define _mandir /opt/%{name}/%{version}/man
%define _pkgdatadir /opt/%{name}/%{version}/share/%{name}
%endif

%if !%{build_debuginfo_rpm}
%define debug_package %{nil}
%endif

%if %(test "%{_prefix}" = "/usr" && echo 1 || echo 0)
%global _sysconfdir /etc
%else
%global _sysconfdir %{_prefix}/etc
%endif

%if !%{use_check_files}
%define __check_files %{nil}
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
Release: 2
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
%if %{install_modulefile}
Requires: %{modules_rpm_name}
%endif

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
Requires: openmpi-runtime

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
# Unbelievably, some versions of RPM do not first delete the previous
# installation root (e.g., it may have been left over from a prior
# failed build).  This can lead to Badness later if there's files in
# there that are not meant to be packaged.  HOWEVER: in some cases, we
# do not want to delete the prior RPM_BUILD_ROOT because there may be
# other stuff in there that we need (e.g., the OFED installer installs
# everything into RPM_BUILD_ROOT that OMPI needs to compile, like the
# OpenIB drivers).
%if !%{leave_build_root}
rm -rf $RPM_BUILD_ROOT
%endif

%setup -q -n openmpi-%{version}

#############################################################################
#
# Build Section
#
#############################################################################

%build

# Non-gcc compilers cannot use FORTIFY_SOURCE (at least, not as of 6
# Oct 2006).  So if we're not GCC, strip out any -DFORTIFY_SOURCE
# arguments in the RPM_OPT_FLAGS before potentially propagating them
# everywhere.  We can really only examine the basename of the
# compiler, so search for it in a few places.

fortify_source=1
if test "$CC" != "" -a "`basename $CC`" != "gcc"; then
    fortify_source=0
else
    compiler="`echo %{configure_options} | sed -e 's@.* CC=\([^ ]*\).*@\1@'`"
    # If that didn't find it, try for CC at the beginning of the line
    if test "$compiler" = "%{configure_options}"; then
        compiler="`echo %{configure_options} | sed -e 's@^CC=\([^ ]*\).*@\1@'`"
    fi

    # Now that we *might* have the compiler name, do a best-faith
    # effort to see if it's gcc.  Blah!
    if test "$compiler" != "" -a "`basename $compiler`" != "gcc"; then
        fortify_source=0
    fi
fi

if test "$fortify_source" = 0; then
    RPM_OPT_FLAGS="`echo $RPM_OPT_FLAGS | sed -e 's@-D_FORTIFY_SOURCE[=0-9]*@@'`"
fi

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

# An attempt to make enviornment happier when installed into non /usr path

%if %{install_in_opt}

# First, the [optional] modulefile

%if %{install_modulefile}
%{__mkdir_p} $RPM_BUILD_ROOT/%{modulefile_path}/%{modulefile_subdir}/
cat <<EOF >$RPM_BUILD_ROOT/%{modulefile_path}/%{modulefile_subdir}/%{modulefile_name}
#%Module

# NOTE: This is an automatically-generated file!  (generated by the
# Open MPI RPM).  Any changes made here will be lost a) if the RPM is
# uninstalled, or b) if the RPM is upgraded or uninstalled.

proc ModulesHelp { } {
   puts stderr "This module adds Open MPI v%{version} to various paths"
}

module-whatis   "Sets up Open MPI v%{version}  in your enviornment"

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
%endif
# End of modulefile if

# Next, the [optional] profile.d scripts

%if %{install_profile_d_scripts}
%{__mkdir_p} $RPM_BUILD_ROOT/etc/profile.d/
cat <<EOF > $RPM_BUILD_ROOT/etc/profile.d/%{name}-%{version}.sh
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
cat <<EOF > $RPM_BUILD_ROOT/etc/profile.d/%{name}-%{version}.csh
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
# End of profile.d if
%endif
# End of install_in_opt if

%if !%{build_all_in_one_rpm}

# Build lists of files that are specific to each package that are not
# easily identifiable by a single directory (e.g., the different
# libraries).  In a somewhat lame move, we can't just pipe everything
# together because if the user, for example, did --disable-shared
# --enable-static, the "grep" for .so files will not find anything and
# therefore return a non-zero exit status.  This will cause RPM to
# barf.  So be super lame and dump the egrep through /bin/true -- this
# always gives a 0 exit status.

# Runtime files
find $RPM_BUILD_ROOT -type f -o -type l | \
   sed -e "s@$RPM_BUILD_ROOT@@" | \
   egrep "lib.*.so|mca.*so" > runtime.files | /bin/true

# Devel files
find $RPM_BUILD_ROOT -type f -o -type l | \
   sed -e "s@$RPM_BUILD_ROOT@@" | \
   egrep "lib.*\.a|lib.*\.la" > devel.files | /bin/true

%endif
# End of build_all_in_one_rpm

#############################################################################
#
# Clean Section
#
#############################################################################
%clean
# Remove installed driver after rpm build finished
rm -rf $RPM_BUILD_DIR/%{_name}-%{_version} 

# Leave $RPM_BUILD_ROOT in order to build dependent packages, if desired
%if !%{leave_build_root}
test "x$RPM_BUILD_ROOT" != "x" && rm -rf $RPM_BUILD_ROOT
%endif

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
# Easy; just list the prefix and then specifically call out the doc
# files.
#

%files
%defattr(-, root, root, -)
%{_prefix}
# If we're not installing in /opt, then the prefix is /usr, but the
# sysconfdir is /etc -- so list them both.  Otherwise, we install in
# /opt/openmpi/<version>, so be sure to list /opt/openmpi as well (so
# that it can be removed).
%if !%{install_in_opt}
%{_sysconfdir}
%else
%dir /opt/%{name}
%endif
# If we're installing the modulefile, get that, too
%if %{install_modulefile}
%{modulefile_path}
%endif
# If we're installing the profile.d scripts, get those, too
%if %{install_profile_d_scripts}
/etc/profile.d/%{name}-%{version}.sh
/etc/profile.d/%{name}-%{version}.csh
%endif
%doc README INSTALL LICENSE

%else

#
# Sub-package RPMs
#
# Harder than all-in-one.  We list the directories specifically so
# that if the RPM creates directories when it is installed, we will
# remove them when the RPM is uninstalled.  We also have to use
# specific file lists.
#

%files runtime -f runtime.files
%defattr(-, root, root, -)
%dir %{_prefix}
# If we're not installing in /opt, then the prefix is /usr, but the
# sysconfdir is /etc -- so list them both.  Otherwise, we install in
# /opt/openmpi/<version>, so be sure to list /opt/openmpi as well (so
# that it can be removed).
%if %{install_in_opt}
%dir /opt/%{name}
%dir /opt/%{name}/%{version}/share
%else
%{_sysconfdir}
%endif
# If we're installing the modulefile, get that, too
%if %{install_modulefile}
%{modulefile_path}
%endif
# If we're installing the profile.d scripts, get those, too
%if %{install_profile_d_scripts}
/etc/profile.d/%{name}-%{version}.sh
/etc/profile.d/%{name}-%{version}.csh
%endif
%dir %{_bindir}
%dir %{_libdir}
%dir %{_libdir}/openmpi
%doc README INSTALL LICENSE
%{_sysconfdir}
%{_pkgdatadir}
%{_bindir}/mpirun
%{_bindir}/mpiexec
%{_bindir}/ompi_info
%{_bindir}/orterun
%{_bindir}/orted

%files devel -f devel.files
%defattr(-, root, root, -)
%{_includedir}
%{_bindir}/mpicc
%{_bindir}/mpiCC
%{_bindir}/mpic++
%{_bindir}/mpicxx
%{_bindir}/mpif77
%{_bindir}/mpif90
%{_bindir}/opal_wrapper

# Note that we list the mandir specifically here, because we want all
# files found in that tree, because rpmbuild may have compressed them
# (e.g., foo.1.gz or foo.1.bz2) -- and we therefore don't know the
# exact filenames.
%files docs
%defattr(-, root, root, -)
%{_mandir}

%endif


#############################################################################
#
# Changelog
#
#############################################################################
%changelog
* Fri Oct  6 2006 Jeff Squyes <jsquyres@cisco.com>
- Remove LANL section; they don't want it
- Add some help for OFED building
- Remove some outdated "rm -f" lines for executables that we no longer ship

* Wed Apr 26 2006 Jeff Squyres <jsquyres@cisco.com>
- Revamp files listings to ensure that rpm -e will remove directories
  if rpm -i created them.
- Simplify options for making modulefiles and profile.d scripts.
- Add oscar define.
- Ensure to remove the previous installation root during prep.
- Cleanup the modulefile specification and installation; also ensure
  that the profile.d scripts get installed if selected.
- Ensure to list sysconfdir in the files list if it's outside of the
  prefix.

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

