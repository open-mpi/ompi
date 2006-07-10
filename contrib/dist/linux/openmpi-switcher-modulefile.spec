# 
# Copyright (c) 2006 The Trustees of Indiana University and Indiana
#                    University Research and Technology
#                    Corporation.  All rights reserved.
# Copyright (c) 2006 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$

#############################################################################
#
# Helpful defines
#
#############################################################################

# This is an OSCAR-specific RPM specfile.  It exists for two reasons:
# 1. Installing an Open MPI-installation-agnostic modulefile that will
#    automatically pick which of multiple Open MPI installations to use
#    if a specific one is not asked for by the user.
# 2. Make the main Open MPI spec file cleaner (don't need any of this
#    switcher gorp).

# Values specified here are what are used if the user does not pass in
# any defaults.

%{!?oscar_prefix: %define oscar_prefix /opt/%{name}}

%define _prefix %{oscar_prefix}-%{version}
%define _pkgdatadir %{oscar_prefix}-%{version}/share/openmpi

# Defining these to nothing overrides the stupid automatic RH9
# functionality of making "debuginfo" RPMs.

%define debug_package %{nil}
%define __check_files %{nil}


#############################################################################
#
# Preamble Section
#
#############################################################################

Summary: OSCAR-specific modulefile for run-time picking of an Open MPI installation to use
Name: openmpi-switcher-modulefile
Version: OPENMPIVERSION
Release: 1
Vendor: Open MPI Team
License: BSD
Group: Development/Libraries
Source: openmpi-switcher-modulefile.tcl
URL: http://www.open-mpi.org/
BuildRoot: %{_tmppath}/%{name}-%{version}-root
Requires: %__rm %__mkdir %__sed %__mv %__chmod %__chown
Requires: modules-oscar
Requires: env-switcher

%description 

This RPM is used to install a Open MPI-installation-agnostic
modulefile that is used to automatically select which Open MPI
installation to use on a per-shell basis.  That is, if there are
multiple Open MPI installations on a single node (due to limitations
in the OSCAR design, aimed at difficulties in knowing exactly which
version of various support libraries to compile Open MPI against), it
will attempt to pick the "right" one and load the appropriate
modulefile for that installation.

Current possibilities include:
- plain vanilla Open MPI
- Open MPI with gm
- Open MPI with blcr
- Open MPI with blcr and gm


#############################################################################
#
# Prep Section
#
#############################################################################
%prep

# Nothing to do...


#############################################################################
#
# Build Section
#
#############################################################################
%build

cd "$RPM_BUILD_DIR"
%__mkdir -p %{name}-%{version}
%__chmod -R a+rX,g-w,o-w %{name}-%{version}
cd %{name}-%{version}

%__cp "$RPM_SOURCE_DIR/openmpi-switcher-modulefile.tcl" \
	"$RPM_BUILD_DIR/%{name}-%{version}/openmpi-%{version}"
%__chmod 644 "$RPM_BUILD_DIR/%{name}-%{version}/openmpi-%{version}"


#############################################################################
#
# Install Section
#
#############################################################################
%install
rm -rf "$RPM_BUILD_ROOT"

# Copy the shell script to pkgdatadir

pkgdatadir="$RPM_BUILD_ROOT/%{_pkgdatadir}"
%__mkdir -p "$pkgdatadir"
%__cp "$RPM_BUILD_DIR/%{name}-%{version}/openmpi-%{version}" \
	"$pkgdatadir/openmpi-%{version}"
%__chmod 644 "$pkgdatadir/openmpi-%{version}"


#############################################################################
#
# Clean Section
#
#############################################################################
%clean
%__rm -rf "$RPM_BUILD_ROOT"


#############################################################################
#
# Post (module) section
#
#############################################################################
%post

# Add ourselves into the switcher repository.  Run the modules and
# switcher setup scripts because this RPM install may be part of a
# massive "rpm -ivh ..." that includes the modules and switcher RPMs
# themselves -- in which case, the environment for those packages will
# not yet have been setup.

# In an ungrade situation, there will already be a openmpi-%{version}
# modulefile resigtered, so we have to unregister it first, and then
# do the registration.

if test "$1" \> 1; then
    switcher mpi --rm-name openmpi-%{version} --force --silent
fi

. /etc/profile.d/00-modules.sh
switcher mpi --add-name openmpi-%{version} %{_pkgdatadir} --force --silent


#############################################################################
#
# Preun (module) Section
#
#############################################################################
%preun

# Remove ourselves from the switcher repository.  Run the modules and
# switcher setup scripts because this RPM install may be part of the
# same shell that "rpm -ivh ..." the switcher/modules RPM's, in which
# case, the environment for those packages will not yet have been
# setup.  This is important to do *before* we are uninstalled because
# of the case where "rpm -Uvh openmpi-module..." is used; the current
# RPM will be uninstalled and then the new one will be installed.  If
# we are %postun here, then the new RPM will be installed, and then
# this will run, which will remove the [new] tag from switcher, which
# is obviously not what we want.

# Grrr...  It seems that "rpm -ivh a.rpm b.rpm c.rpm" is smart enough
# to re-order the order of installation to ensure that dependencies
# are met.  However, "rpm -e a b c" does *not* order the
# uninstallations to ensure that dependencies are still met.  So if
# someone does "rpm -e switcher modules openmpi-module", it is quite
# possible that rpm will uninstall switcher and/or modules *before*
# this RPM is uninstalled.  As such, the following lines will fail,
# which will cause all kinds of Badness.  Arrggh!!  So we have to test
# to ensure that these files are still here before we try to use them.

# If this is an upgrade, then we only want to do this when the last
# RPM is removed (i.e., when this is *not* an upgrade).

if test "$1" == 0; then
    a=/etc/profile.d/00-modules.sh
    which switcher > /dev/null 2> /dev/null
    b=$?
    if test -f $a -a "$b" = "0"; then
	. $a
	default="`switcher mpi --show --system | grep default | cut -d= -f2`"

	# If this RPM was the system default switcher module, then
	# both print a warning and remove the system default.

	if test "$default" = "openmpi-%{version}"; then
		echo "WARNING: This RPM was the default MPI implementation!"
		echo "WARNING: There is no NO DEFAULT MPI implementation!"
		switcher mpi = none --system --force --silent
	fi

	# Remove this module's name from the switcher repository

	switcher mpi --rm-name openmpi-%{version} --force --silent
    fi
fi


#############################################################################
#
# Files Section
#
#############################################################################
%files
%defattr(-,root,root)
# Just list the prefix so that a) it grabs the whole tree, and b) it
# removes directories when the RPM is removed.
%{_prefix}


#############################################################################
#
# Changelog
#
#############################################################################
%changelog
* Sun Jul 02 2006 Jeff Squyres <jsquyres@cisco.com>
- Just use the prefix in the % files list so that we a) grab the whole
  tree and b) it removes all the directories when the RPM is removed.
  Thanks to Bernard Li for reporting the problem.

* Wed Apr 26 2006 Jeff Squyres <jsquyres@cisco.com>
- Adapted for Open MPI.  Still the way to go, despite claims to the
  contrary in the original log statement. :-)

* Wed Dec 31 2003 Jeff Squyres <jsquyres@lam-mpi.org>
- First version.  Hopefully, this RPM won't need to live very long
  because LAM/MPI 7.1 will obviate the need for it.
