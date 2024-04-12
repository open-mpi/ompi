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
# Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017-2022 Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
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
# Options that can be passed in via rpmbuild's --define option.  Note
# that --define takes *1* argument: a multi-token string where the first
# token is the name of the variable to define, and all remaining tokens
# are the value.  For example:
#
# shell$ rpmbuild ... --define 'install_in_opt 1' ...
#
# Or (a multi-token example):
#
# shell$ rpmbuild ... \
#    --define 'configure_options CFLAGS=-g --with-pmix=/usr/local/pmix' ...
#
#############################################################################

# Define this if you want to make this SRPM build in
# /opt/NAME/VERSION-RELEASE instead of the default /usr/.
# type: bool (0/1)
%{!?install_in_opt: %define install_in_opt 0}

# Define this if you want this RPM to install environment setup
# shell scripts.
# type: bool (0/1)
%{!?install_shell_scripts: %define install_shell_scripts 0}
# type: string (root path to install shell scripts)
%{!?shell_scripts_path: %define shell_scripts_path %{_bindir}}
# type: string (base name of the shell scripts)
%{!?shell_scripts_basename: %define shell_scripts_basename mpivars}

# Define this to 1 if you want this RPM to install a modulefile.
# type: bool (0/1)
%{!?install_modulefile: %define install_modulefile 0}
# type: string (root path to install modulefiles)
%{!?modulefile_path: %define modulefile_path /usr/share/Modules/modulefiles}
# type: string (subdir to install modulefile)
%{!?modulefile_subdir: %define modulefile_subdir %{name}}
# type: string (name of modulefile)
%{!?modulefile_name: %define modulefile_name %{version}}

# The name of the modules RPM.  Can vary from system to system.
# RHEL6 calls it "environment-modules".
# type: string (name of modules RPM)
%{!?modules_rpm_name: %define modules_rpm_name environment-modules}

# Should we use the mpi-selector functionality?
# type: bool (0/1)
%{!?use_mpi_selector: %define use_mpi_selector 0}
# The name of the mpi-selector RPM.  Can vary from system to system.
# type: string (name of mpi-selector RPM)
%{!?mpi_selector_rpm_name: %define mpi_selector_rpm_name mpi-selector}
# The location of the mpi-selector executable (can be a relative path
# name if "mpi-selector" can be found in the path)
# type: string (path to mpi-selector exectuable)
%{!?mpi_selector: %define mpi_selector mpi-selector}

# Should we build a debuginfo RPM or not?
# type: bool (0/1)
%{!?build_debuginfo_rpm: %define build_debuginfo_rpm 0}

# Should we build an all-in-one RPM, or several sub-package RPMs?
# type: bool (0/1)
%{!?build_all_in_one_rpm: %define build_all_in_one_rpm 1}

# Should we use the default "check_files" RPM step (i.e., check for
# unpackaged files)?  It is discouraged to disable this, but some
# installers need it (e.g., older versions of OFED, because they
# installed lots of other stuff in the BUILD_ROOT before Open MPI/SHMEM).
# type: bool (0/1)
%{!?use_check_files: %define use_check_files 1}

# By default, RPM supplies a bunch of optimization flags, some of
# which may not work with non-gcc compilers.  We attempt to weed some
# of these out (below), but sometimes it's better to just ignore them
# altogether (e.g., PGI 6.2 will warn about unknown compiler flags,
# but PGI 7.0 will error -- and RPM_OPT_FLAGS contains a lot of flags
# that PGI 7.0 does not understand).  The default is to use the flags,
# but you can set this variable to 0, indicating that RPM_OPT_FLAGS
# should be erased (in which case you probabl want to supply your own
# optimization flags!).
# type: bool (0/1)
%{!?use_default_rpm_opt_flags: %define use_default_rpm_opt_flags 1}

# Some compilers can be installed via tarball or RPM (e.g., Intel,
# PGI).  If they're installed via RPM, then rpmbuild's auto-dependency
# generation stuff will work fine.  But if they're installed via
# tarball, then rpmbuild's auto-dependency generation stuff will
# break; complaining that it can't find a bunch of compiler .so files.
# So provide an option to turn this stuff off.
# type: bool (0/1)
%{!?disable_auto_requires: %define disable_auto_requires 0}

# On some platforms, Open MPI/SHMEM just flat-out doesn't work with
# -D_FORTIFY_SOURCE (e.g., some users have reported that there are
# problems on ioa64 platforms).  In this case, just turn it off
# (meaning: this specfile will strip out that flag from the
# OS-provided compiler flags).  We already strip out _FORTIFY_SOURCE
# for non-GCC compilers; setting this option to 0 will *always* strip
# it out, even if you're using GCC.
# type: bool (0/1)
%{!?allow_fortify_source: %define allow_fortify_source 1}

# Select md5 packing algorithm, that src.rpm created on one distro can be read on another.
%global _binary_filedigest_algorithm 1
%global _source_filedigest_algorithm 1

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
%endif

%if !%{build_debuginfo_rpm}
%define debug_package %{nil}
%endif

%if %(test "%{_prefix}" = "/usr" && echo 1 || echo 0)
%global _sysconfdir /etc
%else
%global _sysconfdir %{_prefix}/etc
%endif

# Is the sysconfdir under the prefix directory?  This affects
# whether we list the sysconfdir separately in the files sections,
# below.
%define sysconfdir_in_prefix %(test "`echo %{_sysconfdir} | grep %{_prefix}`" = "" && echo 0 || echo 1)

%{!?_pkgdatadir: %define _pkgdatadir %{_datadir}/prrte}

%if !%{use_check_files}
%define __check_files %{nil}
%endif

%{!?configure_options: %define configure_options %{nil}}

%if !%{use_default_rpm_opt_flags}
%define optflags ""
%endif

#############################################################################
#
# Preamble Section
#
#############################################################################

Summary: PMIx Reference RunTime Environment (PRRTE)
Name: %{?_name:%{_name}}%{!?_name:prrte}
Version: $VERSION
Release: 1%{?dist}
License: BSD
Group: Development/Libraries
Source: %{name}-%{version}.tar.$EXTENSION
Packager: %{?_packager:%{_packager}}%{!?_packager:%{_vendor}}
Vendor: %{?_vendorinfo:%{_vendorinfo}}%{!?_vendorinfo:%{_vendor}}
Distribution: %{?_distribution:%{_distribution}}%{!?_distribution:%{_vendor}}
Prefix: %{_prefix}
Provides: prrte = %{version}
BuildRoot: /var/tmp/%{name}-%{version}-%{release}-root
BuildRequires: gcc
BuildRequires: flex
BuildRequires: libevent-devel
BuildRequires: pmix >= 4.2.0
BuildRequires: hwloc-devel
%if %{disable_auto_requires}
AutoReq: no
%endif
%if %{install_modulefile}
Requires: %{modules_rpm_name}
%endif

%description
PRRTE is the PMIx Reference Run Time Environment.

The project is formally referred to in documentation by "PRRTE", and
the GitHub repository is "prrte".

However, we have found that most users do not like typing the two
consecutive "r"s in the name. Hence, all of the internal API symbols,
environment variables, MCA frameworks, and CLI executables all use the
abbreviated "prte" (one "r", not two) for convenience.

This RPM contains all the tools necessary to compile, link, and run
the PRRTE system.

%if !%{build_all_in_one_rpm}

#############################################################################
#
# Preamble Section (runtime)
#
#############################################################################

%package runtime
Summary: Tools and plugin modules for running PRRTE
Group: Development/Libraries
Provides: prrte = %{version}
Provides: prrte-runtime = %{version}
%if %{disable_auto_requires}
AutoReq: no
%endif
%if %{install_modulefile}
Requires: %{modules_rpm_name}
%endif

%description runtime
PRRTE is the PMIx Reference Run Time Environment.

The project is formally referred to in documentation by "PRRTE", and
the GitHub repository is "prrte".

However, we have found that most users do not like typing the two
consecutive "r"s in the name. Hence, all of the internal API symbols,
environment variables, MCA frameworks, and CLI executables all use the
abbreviated "prte" (one "r", not two) for convenience.

This subpackage provides general tools (prte, prun, prterun, etc.) and the
Module Component Architecture (MCA) base and plugins necessary for
running the PRRTE system.

%endif

#############################################################################
#
# Preamble Section (devel)
#
#############################################################################

%package devel
Summary: Development tools and header files for PRRTE
Group: Development/Libraries
%if %{disable_auto_requires}
AutoReq: no
%endif
Provides: prrte-devel = %{version}
Requires: %{name}-runtime

%description devel
PRRTE is the PMIx Reference Run Time Environment.

The project is formally referred to in documentation by "PRRTE", and
the GitHub repository is "prrte".

However, we have found that most users do not like typing the two
consecutive "r"s in the name. Hence, all of the internal API symbols,
environment variables, MCA frameworks, and CLI executables all use the
abbreviated "prte" (one "r", not two) for convenience.

This subpackage provides the development files for PRRTE,
such as wrapper compilers and header files for development
of PRRTE plugins.

#############################################################################
#
# Prepatory Section
#
#############################################################################
%prep
# Unbelievably, some versions of RPM do not first delete the previous
# installation root (e.g., it may have been left over from a prior
# failed build).  This can lead to Badness later if there's files in
# there that are not meant to be packaged.
rm -rf $RPM_BUILD_ROOT

%setup -q -n %{name}-%{version}

#############################################################################
#
# Build Section
#
#############################################################################

%build

# rpmbuild processes seem to be geared towards the GNU compilers --
# they pass in some flags that will only work with gcc.  So if we're
# trying to build with some other compiler, the process will choke.
# This is *not* something the user can override with a well-placed
# --define on the rpmbuild command line, unless they find and override
# all "global" CFLAGS kinds of RPM macros (every distro names them
# differently).  For example, non-gcc compilers cannot use
# FORTIFY_SOURCE (at least, not as of 6 Oct 2006).  We can really only
# examine the basename of the compiler, so search for it in a few
# places.

%if %{allow_fortify_source}
using_gcc=1
if test "$CC" != ""; then
    # Do horrible things to get the basename of just the compiler,
    # particularly in the case of multword values for $CC
    eval "set $CC"
    if test "`basename $1`" != "gcc"; then
        using_gcc=0
    fi
fi

if test "$using_gcc" = "1"; then
    # Do wretched things to find a CC=* token
    eval "set -- %{configure_options}"
    compiler=
    while test "$1" != "" -a "$compiler" = ""; do
         case "$1" in
         CC=*)
                 compiler=`echo $1 | cut -d= -f2-`
                 ;;
         esac
         shift
    done

    # Now that we *might* have the compiler name, do a best-faith
    # effort to see if it's gcc.  Blah!
    if test "$compiler" != ""; then
        if test "`basename $compiler`" != "gcc"; then
            using_gcc=0
        fi
    fi
fi
%else
# If we're not allowing _FORTIFY_SOURCE, then just set using_gcc to 0 and
# the logic below will strip _FORTIFY_SOURCE out if it's present.
using_gcc=0
%endif

# If we're not using the default RPM_OPT_FLAGS, then wipe them clean
# (the "optflags" macro has already been wiped clean, above).

%if !%{use_default_rpm_opt_flags}
RPM_OPT_FLAGS=
export RPM_OPT_FLAGS
%endif

# If we're not GCC, strip out any GCC-specific arguments in the
# RPM_OPT_FLAGS before potentially propagating them everywhere.

if test "$using_gcc" = 0; then

    # Non-gcc compilers cannot handle FORTIFY_SOURCE (at least, not as
    # of Oct 2006)
    RPM_OPT_FLAGS="`echo $RPM_OPT_FLAGS | sed -e 's@-D_FORTIFY_SOURCE[=0-9]*@@'`"

    # Non-gcc compilers will generate warnings for several flags
    # placed in RPM_OPT_FLAGS by RHEL5, but -mtune=generic will cause
    # an error for icc 9.1.
    RPM_OPT_FLAGS="`echo $RPM_OPT_FLAGS | sed -e 's@-mtune=generic@@'`"
fi

CFLAGS="%{?cflags:%{cflags}}%{!?cflags:$RPM_OPT_FLAGS}"
export CFLAGS

%configure %{configure_options}
%{__make} %{?mflags}


#############################################################################
#
# Install Section
#
#############################################################################

%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

# We've had cases of config.log being left in the installation tree.
# We don't need that in an RPM.
find $RPM_BUILD_ROOT -name config.log -exec rm -f {} \;

# First, the [optional] modulefile

%if %{install_modulefile}
%{__mkdir_p} $RPM_BUILD_ROOT/%{modulefile_path}/%{modulefile_subdir}/
cat <<EOF >$RPM_BUILD_ROOT/%{modulefile_path}/%{modulefile_subdir}/%{modulefile_name}
#%Module

# NOTE: This is an automatically-generated file!  (generated by the
# PRRTE RPM).  Any changes made here will be lost a) if the RPM is
# uninstalled, or b) if the RPM is upgraded or uninstalled.

proc ModulesHelp { } {
   puts stderr "This module adds PRRTE v%{version} to various paths"
}

module-whatis   "Sets up PRRTE v%{version} in your enviornment"

prepend-path PATH "%{_prefix}/bin/"
prepend-path LD_LIBRARY_PATH %{_libdir}
EOF
%endif
# End of modulefile if

# Next, the [optional] shell scripts

%if %{install_shell_scripts}
%{__mkdir_p} $RPM_BUILD_ROOT/%{shell_scripts_path}
cat <<EOF > $RPM_BUILD_ROOT/%{shell_scripts_path}/%{shell_scripts_basename}.sh
# NOTE: This is an automatically-generated file!  (generated by the
# PRRTE RPM).  Any changes made here will be lost if the RPM is
# uninstalled or upgraded.

# PATH
if test -z "\`echo \$PATH | grep %{_bindir}\`"; then
    PATH=%{_bindir}:\${PATH}
    export PATH
fi

# LD_LIBRARY_PATH
if test -z "\`echo \$LD_LIBRARY_PATH | grep %{_libdir}\`"; then
    LD_LIBRARY_PATH=%{_libdir}\${LD_LIBRARY_PATH:+:}\${LD_LIBRARY_PATH}
    export LD_LIBRARY_PATH
fi

cat <<EOF > $RPM_BUILD_ROOT/%{shell_scripts_path}/%{shell_scripts_basename}.csh
# NOTE: This is an automatically-generated file!  (generated by the
# PRRTE RPM).  Any changes made here will be lost if the RPM is
# uninstalled or upgraded.

# path
if ("" == "\`echo \$path | grep %{_bindir}\`") then
    set path=(%{_bindir} \$path)
endif

# LD_LIBRARY_PATH
if ("1" == "\$?LD_LIBRARY_PATH") then
    if ("\$LD_LIBRARY_PATH" !~ *%{_libdir}*) then
        setenv LD_LIBRARY_PATH %{_libdir}:\${LD_LIBRARY_PATH}
    endif
else
    setenv LD_LIBRARY_PATH %{_libdir}
endif
%endif

# End of shell_scripts if

%if !%{build_all_in_one_rpm}

# Build lists of files that are specific to each package that are not
# easily identifiable by a single directory (e.g., the different
# libraries).  In a somewhat lame move, we can't just pipe everything
# together because if the user, for example, did --disable-shared
# --enable-static, the "grep" for .so files will not find anything and
# therefore return a non-zero exit status.  This will cause RPM to
# barf.  So be super lame and dump the egrep through /bin/true -- this
# always gives a 0 exit status.

# First, find all the files
rm -f all.files runtime.files remaining.files devel.files docs.files
find $RPM_BUILD_ROOT -type f -o -type l | \
   sed -e "s@$RPM_BUILD_ROOT@@" \
   > all.files | /bin/true

# Runtime files.  This should generally be library files and some
# executables (no man pages, no doc files, no header files).  Do *not*
# include wrapper compilers.
cat all.files | egrep '/lib/|/lib64/|/lib32/|/bin/|/etc/|/help-' > tmp.files | /bin/true
# Snip out a bunch of executables (e.g., wrapper compilers, pkgconfig
# files, .la and .a files)
egrep -vi 'pcc|pkgconfig|\.la$|\.a$' tmp.files > runtime.files | /bin/true
rm -f tmp.files

# Now take the runtime files out of all.files so that we don't get
# duplicates.
grep -v -f runtime.files all.files > remaining.files

# Devel files, potentially including VT files.  Basically -- just
# exclude the man pages and doc files.
cat remaining.files | \
   egrep -v '/man/|/doc/' \
   > devel.files | /bin/true

# Now take those files out of reaming.files so that we don't get
# duplicates.
grep -v -f devel.files remaining.files > docs.files

#################################################

# Now that we have a final list of files for each of the runtime and
# devel RPMs, snip even a few more files out of those lists
# because for directories that are wholly in only one RPM, we just
# list that directory in the file lists below, and RPM will pick up
# all files in that tree.  We therefore don't want to list any files
# in those trees in our *.files file lists.  Additionally, the man
# pages may get compressed by rpmbuild after this "install" step, so we
# might not even have their final filenames, anyway.

# runtime sub package
%if !%{sysconfdir_in_prefix}
grep -v %{_sysconfdir} runtime.files > tmp.files
mv tmp.files runtime.files
%endif
grep -v %{_pkgdatadir} runtime.files > tmp.files
mv tmp.files runtime.files

# devel sub package
grep -v %{_includedir} devel.files > tmp.files
mv tmp.files devel.files

%endif
# End of build_all_in_one_rpm

#############################################################################
#
# Clean Section
#
#############################################################################
%clean
# We may be in the directory that we're about to remove, so cd out of
# there before we remove it
cd /tmp

# Remove installed driver after rpm build finished
rm -rf $RPM_BUILD_DIR/%{name}-%{version}

test "x$RPM_BUILD_ROOT" != "x" && rm -rf $RPM_BUILD_ROOT

#############################################################################
#
# Post Install Section
#
#############################################################################

#############################################################################
#
# Pre Uninstall Section
#
#############################################################################

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
%if %(test "%{_prefix}" = "/usr" && echo 1 || echo 0)
%{_bindir}/*
%{_libdir}/*
%{_datadir}
%{_includedir}
%else
%{_prefix}
%endif
# If the sysconfdir is not under the prefix, then list it explicitly.
%if !%{sysconfdir_in_prefix}
%{_sysconfdir}
%endif
# If %%{install_in_opt}, then we're instaling PRRTE to
# /opt/prrte/<version>.  But be sure to also explicitly mention
# /opt/prrte so that it can be removed by RPM when everything under
# there is also removed.
%if %{install_in_opt}
%dir /opt/%{name}
%endif
# If we're installing the modulefile, get that, too
%if %{install_modulefile}
%{modulefile_path}
%endif
# If we're installing the shell scripts, get those, too
%if %{install_shell_scripts}
%{shell_scripts_path}/%{shell_scripts_basename}.sh
%{shell_scripts_path}/%{shell_scripts_basename}.csh
%endif
%doc README.md LICENSE

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
%if %(test "%{_prefix}" = "/usr" && echo 1 || echo 0)
%{_bindir}/*
%{_libdir}/*
%{_datadir}
%else
%{_prefix}
%endif
# If the sysconfdir is not under the prefix, then list it explicitly.
%if !%{sysconfdir_in_prefix}
%{_sysconfdir}
%endif
# If %%{install_in_opt}, then we're instaling PRRTE to
# /opt/prte/<version>.  But be sure to also explicitly mention
# /opt/prte so that it can be removed by RPM when everything under
# there is also removed.  Also list /opt/prrte/<version>/share so
# that it can be removed as well.
%if %{install_in_opt}
%dir /opt/%{name}
%dir /opt/%{name}/%{version}/share
%endif
# If we're installing the modulefile, get that, too
%if %{install_modulefile}
%{modulefile_path}
%endif
# If we're installing the shell scripts, get those, too
%if %{install_shell_scripts}
%{shell_scripts_path}/%{shell_scripts_basename}.sh
%{shell_scripts_path}/%{shell_scripts_basename}.csh
%endif
%doc README.md LICENSE
%{_pkgdatadir}

%files devel -f devel.files
%defattr(-, root, root, -)
%{_includedir}

%endif


#############################################################################
#
# Changelog
#
#############################################################################
%changelog
* Wed Aug 10 2022 Ralph Castain <rhc@pmix.org>
- Major cleanup of cruft from prior history that does not
  pertain to PRRTE. Cleanup from bad global search/replace
  of "prrte" with "prte"

* Thu Apr 7 2022 Adam Goldman <adam.goldman@intel.com>
- Several minor fixes: added _includedir to build_all_in_one_rpm, 
  escape macro in comment, and use %{name} instead of hard-coded value
  
* Tue Mar 28 2017 Jeff Squyres <jsquyres@cisco.com>
- Reverting a decision from a prior changelog entry: if
  install_in_opt==1, then even put the modulefile under /opt.

* Thu Nov 12 2015 Gilles Gouaillardet <gilles@rist.or.jp>
- Revamp packaging when prefix is /usr

* Tue Jan 20 2015 Bert Wesarg <bert.wesarg@tu-dresden.de>
- Remove VampirTrace wrapper from package.

* Mon Jul 07 2014 Jeff Squyres <jsquyres@cisco.com>
- Several minor fixes from Oliver Lahaye: fix dates in changelog,
  added %{?dist} tag to the Release field, and added some Provides
  fields in case %{name} is overridden.

* Mon Jun 24 2013 Igor Ivanov <Igor.Ivanov@itseez.com>
- Add Open SHMEM parallel programming library as part of Open MPI

* Tue Dec 11 2012 Jeff Squyres <jsquyres@cisco.com>
- Re-release 1.6.0-1.6.3 SRPMs (with new SRPM Release numbers) with
  patch for VampirTrace's configure script to make it install the
  private "libtool" script in the right location (the script is used
  to build user VT applications).
- Update the regexps/methodology used to generate the lists of files
  in the multi-RPM sub-packages; it's been broken for a little while.
- No longer explicitly list the bin dir executables in the multi-RPM
  sub-packages
- Per https://svn.open-mpi.org/trac/ompi/ticket/3382, remove all files
  named "config.log" from the install tree so that we can use this
  spec file to re-release all OMPI v1.6.x SRPMs.

* Wed Jun 27 2012 Jeff Squyres <jsquyres@cisco.com>
- Remove the "ofed" and "munge_build_into_install" options, because
  OFED no longer distributes MPI implementations.  Yay!

* Mon Jun 04 2012 Jeff Squyres <jsquyres@cisco.com>
- Didn't change the specfile, but changed the script that generates
  the SRPM to force the use of MD5 checksums (vs. SHA1 checksums) so
  that the SRPM is friendly to older versions of RPM, such as that on
  RHEL 5.x.

* Fri Feb 17 2012 Jeff Squyres <jsquyres@cisco.com>
- Removed OSCAR defines.
- If use_mpi_selector==1, then also set install_shell_scripts to 1.
- Change modules default RPM name and modulefiles path to the defaults
  on RHEL6.

* Mon Dec 14 2009 Jeff Squyres <jsquyres@cisco.com>
- Add missing executables to specfile (ompi-server, etc.)
- Fix: pull in VT files when building multiple RPMs (reported by Jim
  Kusznir).
- Add allow_fortify_source option to let users selectively disable
  _FORTIFY_SOURCE processing on platforms where it just doesn't work
  (even with gcc; also reported by Jim Kusznir).

* Tue Sep  8 2009 Jeff Squyres <jsquyres@cisco.com>
- Change shell_scripts_basename to not include version number to
  accomodate what mpi-selector expects.

* Mon Feb  4 2008 Jeff Squyres <jsquyres@cisco.com>
- OFED 1.3 has a much better installer; remove all the
  leave_build_root kludge nastyness.  W00t!

* Fri Jan 18 2008 Jeff Squyres <jsquyres@cisco.com>
- Remove the hard-coded "prte" name from two Requires statements
  and use %{name} instead (FWIW, %{_name} caused rpmbuild to barf).

* Wed Jan  2 2008 Jeff Squyres <jsquyres@cisco.com>
- Remove duplicate %{_sysconfdir} in the % files sections when
  building the sub-packages.
- When building the sub-packages, ensure that devel.files also picks
  up the F90 module.
- Hard-code the directory name "prte" into _pkglibdir (vs. using
  %{name}) because the OMPI code base has it hard-coded as well.
  Thanks to Jim Kusznir for noticing the problem.

* Tue Dec  4 2007 Jeff Squyres <jsquyres@cisco.com>
- Added define option for disabling the use of rpmbuild's
  auto-dependency generation stuff.  This is necessary for some
  compilers that allow themselves to be installed via tarball (not
  RPM), such as the Portland Group compiler.

* Thu Jul 12 2007 Jeff Squyres <jsquyres@cisco.com>
- Change default doc location when using install_in_opt.  Thanks to
  Alex Tumanov for pointing this out and to Doug Ledford for
  suggestions where to put docdir in this case.

* Thu May  3 2007 Jeff Squyres <jsquyres@cisco.com>
- Ensure to move out of $RPM_BUILD_ROOT before deleting it in % clean.
- Remove a debugging "echo" that somehow got left in there

* Thu Apr 12 2007 Jeff Squyres <jsquyres@cisco.com>
- Ensure that _pkglibdir is always defined, suggested by Greg Kurtzer.

* Wed Apr  4 2007 Jeff Squyres <jsquyres@cisco.com>
- Fix several mistakes in the generated profile.d scripts
- Fix several bugs with identifying non-GNU compilers, stripping of
  FORTIFY_SOURCE, -mtune, etc.

* Fri Feb  9 2007 Jeff Squyres <jsquyres@cisco.com>
- Revamp to make profile.d scripts more general: default to making the
  shell script files be %{_bindir}/mpivars.{sh|csh}
- Add %{munge_build_into_install} option for OFED 1.2 installer on SLES
- Change shell script files and modulefile to *pre*pend all the OMPI paths
- Make shell script and modulefile installation indepdendent of
  %{install_in_opt} (they're really separate issues)
- Add more "ofed" shortcut qualifiers
- Slightly better test for basename CC in the fortify source section
- Fix some problems in the csh shell script

* Fri Oct  6 2006 Jeff Squyres <jsquyres@cisco.com>
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

* Thu Mar 30 2006 Jeff Squyres <jsquyres@cisco.com>
- Lots of bit rot updates
- Reorganize and rename the subpackages
- Add / formalize a variety of rpmbuild --define options
- Comment out the docs subpackage for the moment (until we have some
  documentation -- coming in v1.1!)

* Tue May 03 2005 Jeff Squyres <jsquyres@open-mpi.org>
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
