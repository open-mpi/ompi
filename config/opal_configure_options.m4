dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2022 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2009      Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009-2011 Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
dnl Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
dnl Copyright (c) 2019-2021 Triad National Security, LLC. All rights
dnl                         reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OPAL_CONFIGURE_OPTIONS],[
opal_show_subtitle "General configuration options"


#
# Is this a developer copy?
#

if test -d .git; then
    OPAL_DEVEL=1
else
    OPAL_DEVEL=0
fi


#
# Code coverage options
#

AC_MSG_CHECKING([if want to run code coverage])
AC_ARG_ENABLE([coverage],
              [AS_HELP_STRING([--enable-coverage],
                             [enable code coverage files to be generated])])
if test "$enable_coverage" = "yes"; then
    if test "$enable_shared" = "yes"; then
        AC_MSG_WARN([Code coverage can run only with static libraries. Please
run configure with --enable-static --disable-shared if
you want code coverage. Also ensure that you execute
make clean too ensure removal of all leftover shared
mpi libraries])
        AC_MSG_ERROR([Cannot continue processing])
    fi
    AC_MSG_RESULT([yes])
    WANT_COVERAGE=1
else
    AC_MSG_RESULT([no])
    WANT_COVERAGE=0
fi


#
# Branch Probabilities options
#

AC_MSG_CHECKING([if want to compile with branch probabilities])
AC_ARG_ENABLE([branch-probabilities],
              [AS_HELP_STRING([--enable-branch-probabilities],
                             [enable profile arcs and branch probability optimization])])
if test "$enable_branch_probabilities" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_BRANCH_PROBABILITIES=1
else
    AC_MSG_RESULT([no])
    WANT_BRANCH_PROBABILITIES=0
fi


#
# Memory debugging
#

AC_MSG_CHECKING([if want to debug memory usage])
AC_ARG_ENABLE([mem-debug],
    [AS_HELP_STRING([--enable-mem-debug],
                   [enable memory debugging (not for general MPI users!) (default: disabled)])])
if test "$enable_mem_debug" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_MEM_DEBUG=1
else
    AC_MSG_RESULT([no])
    WANT_MEM_DEBUG=0
fi
AC_DEFINE_UNQUOTED(OPAL_ENABLE_MEM_DEBUG, $WANT_MEM_DEBUG,
    [Whether we want the memory profiling or not])

#
# Memory profiling
#

AC_MSG_CHECKING([if want to profile memory usage])
AC_ARG_ENABLE([mem-profile],
    [AS_HELP_STRING([--enable-mem-profile],
                   [enable memory profiling (not for general MPI users!) (default: disabled)])])
if test "$enable_mem_profile" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_MEM_PROFILE=1
else
    AC_MSG_RESULT([no])
    WANT_MEM_PROFILE=0
fi
AC_DEFINE_UNQUOTED(OPAL_ENABLE_MEM_PROFILE, $WANT_MEM_PROFILE,
    [Whether we want the memory profiling or not])

#
# Developer picky compiler options
#

AC_MSG_CHECKING([if want developer-level compiler pickyness])
AC_ARG_ENABLE([picky],
    [AS_HELP_STRING([--enable-picky],
                   [enable developer-level compiler pickyness when building Open MPI (default: disabled, unless a .git directory is found in the build tree)])])
if test "$enable_picky" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_PICKY_COMPILER=1
else
    AC_MSG_RESULT([no])
    WANT_PICKY_COMPILER=0
fi
#################### Developer default override ####################
if test "$WANT_PICKY_COMPILER" = "0" && test -z "$enable_picky" && test "$OPAL_DEVEL" = 1; then
    WANT_PICKY_COMPILER=1
    echo "--> developer override: enable picky compiler by default"
fi
#################### Developer default override ####################

#
# Developer debugging
#

AC_MSG_CHECKING([if want developer-level debugging code])
AC_ARG_ENABLE([debug],
    [AS_HELP_STRING([--enable-debug],
                   [enable developer-level debugging code (not for general MPI users!) (default: disabled)])])
if test "$enable_debug" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_DEBUG=1
else
    AC_MSG_RESULT([no])
    WANT_DEBUG=0
fi


AC_MSG_CHECKING([if want to developer-level timing framework])
AC_ARG_ENABLE([timing],
    [AS_HELP_STRING([--enable-timing],
                   [enable developer-level timing code (not for general MPI users!) (default: disabled)])])
if test "$enable_timing" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_TIMING=1
else
    AC_MSG_RESULT([no])
    WANT_TIMING=0
fi

AC_DEFINE_UNQUOTED(OPAL_ENABLE_TIMING, $WANT_TIMING,
    [Whether we want developer-level timing framework or not])

AM_CONDITIONAL([OPAL_COMPILE_TIMING], [test "$WANT_TIMING" = "1"])
AM_CONDITIONAL([OPAL_INSTALL_TIMING_BINARIES], [test "$WANT_TIMING" = "1" && test "$enable_binaries" != "no"])

# Later calls to AC_PROG_CC/CXX/FC can
# inject things like -O2 into compile flags if they are
# not defined, which we don't want. Make sure these flags
# are at least set to an empty string now.
#
# Complicating matters is that autogen can re-order
# these calls toward the top of configure. This block should
# be at/near the top, so do it now.
#
if test "$WANT_DEBUG" = "0"; then
    CFLAGS="-DNDEBUG $CFLAGS"
    CXXFLAGS="-DNDEBUG $CXXFLAGS"

    # NDEBUG doesn't exist in fortran, so just make sure it's defined.
    if [ test -z "$FCFLAGS" ]; then
      FCFLAGS=""
    fi
else
    # Do we want debugging symbols?
    if test "$enable_debug_symbols" != "no" ; then
        CFLAGS="$CFLAGS -g"
        CXXFLAGS="$CXXFLAGS -g"
        FCFLAGS="$FCFLAGS -g"
        AC_MSG_WARN([-g has been added to compiler (--enable-debug)])
    else
        # If not set, define compile flags to an empty string
        # to prevent AC_PROG_CC/FC/CXX from modifying compiler flags.
        # See: https://www.gnu.org/software/autoconf/manual/autoconf-2.69/html_node/C-Compiler.html
        # for more info.
        if [ test -z "$CFLAGS" ]; then
            CFLAGS=""
        fi
        if [ test -z "$CXXFLAGS" ]; then
            CXXFLAGS=""
        fi
        if [ test -z "$FCFLAGS" ]; then
            FCFLAGS=""
        fi
    fi
fi

AC_DEFINE_UNQUOTED(OPAL_ENABLE_DEBUG, $WANT_DEBUG,
    [Whether we want developer-level debugging code or not])

AC_ARG_ENABLE([debug-symbols],
    [AS_HELP_STRING([--disable-debug-symbols],
        [Disable adding compiler flags to enable debugging symbols if --enable-debug is specified.  For non-debugging builds, this flag has no effect.])])

#
# Do we want to install all of OPAL/ORTE and OMPI's header files?
#

AC_MSG_CHECKING([if want to install project-internal header files])
AC_ARG_WITH([devel-headers],
    [AS_HELP_STRING([--with-devel-headers],
                   [normal MPI users/applications do not need this (mpi.h and mpif.h are ALWAYS installed).  Developer headers are only necessary for MCA module authors (default: disabled).])])
if test "$with_devel_headers" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_INSTALL_HEADERS=1
else
    AC_MSG_RESULT([no])
    WANT_INSTALL_HEADERS=0
fi
AM_CONDITIONAL(WANT_INSTALL_HEADERS, test "$WANT_INSTALL_HEADERS" = 1)

#
# Do we want the pretty-print stack trace feature?
#

AC_MSG_CHECKING([if want pretty-print stacktrace])
AC_ARG_ENABLE([pretty-print-stacktrace],
    [AS_HELP_STRING([--enable-pretty-print-stacktrace],
                    [Pretty print stacktrace on process signal (default: enabled)])])
if test "$enable_pretty_print_stacktrace" = "no" ; then
    AC_MSG_RESULT([no])
    WANT_PRETTY_PRINT_STACKTRACE=0
else
    AC_MSG_RESULT([yes])
    WANT_PRETTY_PRINT_STACKTRACE=1
fi
AC_DEFINE_UNQUOTED([OPAL_WANT_PRETTY_PRINT_STACKTRACE],
                   [$WANT_PRETTY_PRINT_STACKTRACE],
                   [if want pretty-print stack trace feature])


#
# Do we want PTY support?
#

AC_MSG_CHECKING([if want pty support])
AC_ARG_ENABLE([pty-support],
    [AS_HELP_STRING([--enable-pty-support],
                   [Enable/disable PTY support for STDIO forwarding.  (default: enabled)])])
if test "$enable_pty_support" = "no" ; then
    AC_MSG_RESULT([no])
    OPAL_ENABLE_PTY_SUPPORT=0
else
    AC_MSG_RESULT([yes])
    OPAL_ENABLE_PTY_SUPPORT=1
fi
AC_DEFINE_UNQUOTED([OPAL_ENABLE_PTY_SUPPORT], [$OPAL_ENABLE_PTY_SUPPORT],
                   [Whether user wants PTY support or not])


#
# Do we want to disable weak symbols for some reason?
#

AC_MSG_CHECKING([if want weak symbol support])
AC_ARG_ENABLE([weak-symbols],
    [AS_HELP_STRING([--enable-weak-symbols],
                   [use weak symbols, if available (default: enabled)])])
if test "$enable_weak_symbols" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_WEAK_SYMBOLS=1
else
    AC_MSG_RESULT([no])
    WANT_WEAK_SYMBOLS=0
fi


#
# Do we want to allow DLOPEN?
#

AC_MSG_CHECKING([if want dlopen support])
AC_ARG_ENABLE([dlopen],
    [AS_HELP_STRING([--enable-dlopen],
                    [Whether build should attempt to use dlopen (or
                     similar) to dynamically load components.
                     Disabling dlopen implies --disable-mca-dso.
                     (default: enabled)])])
if test "$enable_dlopen" = "no" ; then
    OPAL_ENABLE_DLOPEN_SUPPORT=0
    AC_MSG_RESULT([no])
else
    OPAL_ENABLE_DLOPEN_SUPPORT=1
    AC_MSG_RESULT([yes])
fi


#
# Do we want to show component load error messages by default?
#

AC_MSG_CHECKING([for default value of mca_base_component_show_load_errors])
AC_ARG_WITH([show-load-errors],
            [AS_HELP_STRING([--with-show-load-errors],
                            [Set the default value for the MCA
                            parameter
                            mca_base_component_show_load_errors (but
                            can be overridden at run time by the usual
                            MCA-variable-setting mechansism).
                            (default: "all")])])

AS_IF([test -z "$with_show_load_errors" -o "$with_show_load_errors" = "yes"],
      [with_show_load_errors=all
       AC_MSG_RESULT([enabled for all])],
      [AS_IF([test "$with_show_load_errors" = "no"],
             [with_show_load_errors=none
              AC_MSG_RESULT([disabled for all])],
             [AC_MSG_RESULT([$with_show_load_errors])])])

AC_DEFINE_UNQUOTED(OPAL_SHOW_LOAD_ERRORS_DEFAULT, ["$with_show_load_errors"],
                   [Default value for mca_base_component_show_load_errors MCA variable])


#
# Heterogeneous support
#

AC_MSG_CHECKING([if want heterogeneous support])
AC_ARG_ENABLE([heterogeneous],
    [AS_HELP_STRING([--enable-heterogeneous],
                    [Enable features required for heterogeneous
                     platform support (default: disabled)])])
if test "$enable_heterogeneous" = "yes" ; then
     AC_MSG_RESULT([yes])
     opal_want_heterogeneous=1
else
     AC_MSG_RESULT([no])
     opal_want_heterogeneous=0
fi
AC_DEFINE_UNQUOTED([OPAL_ENABLE_HETEROGENEOUS_SUPPORT],
                   [$opal_want_heterogeneous],
                   [Enable features required for heterogeneous support])


#
# Cross-compile data
#
AC_ARG_WITH([cross],
    [AS_HELP_STRING([--with-cross=FILE],
        [Specify configure values that can not be determined in a cross-compilation environment.  See the Open MPI FAQ.])])
if test "$with_cross" = "yes" ; then
    AC_MSG_ERROR([--with-cross argument must include FILE option])
elif test "$with_cross" = "no" ; then
    AC_MSG_ERROR([--without-cross is not a valid argument])
elif test "$with_cross" != "" ; then
    if test ! -r $with_cross ; then
        AC_MSG_ERROR([could not find cross-compile data file $with_cross])
    fi

    # eval into environment
    OPAL_LOG_MSG([Loading cross-compile file $with_cross, with contents below])
    OPAL_LOG_FILE([$with_cross])
    . "$with_cross"
fi

#
# Do we want to install binaries?
#
AC_ARG_ENABLE([binaries],
    [AS_HELP_STRING([--enable-binaries],
        [Build and install binaries required for Open MPI, such as the wrapper compilers.   Useful for multi-lib installations.  (default: enabled)])])
AM_CONDITIONAL([OPAL_INSTALL_BINARIES], [test "$enable_binaries" != "no"])

AC_ARG_ENABLE([script-wrapper-compilers],
  [AS_HELP_STRING([--enable-script-wrapper-compilers],
     [Use less featured script-based wrapper compilers instead of the standard C-based wrapper compilers.  This option ignores the --disable-binaries option and is mainly useful in cross-compile environments])])
  if test "$enable_script_wrapper_compilers" = "yes"; then
      WANT_SCRIPT_WRAPPER_COMPILERS=1
  else
      WANT_SCRIPT_WRAPPER_COMPILERS=0
  fi
AM_CONDITIONAL([OPAL_WANT_SCRIPT_WRAPPER_COMPILERS], [test "$enable_script_wrapper_compilers" = "yes"])

#
# Support per-user config files?
#
OPAL_VAR_SCOPE_PUSH([result])
AC_ARG_ENABLE([per-user-config-files],
   [AS_HELP_STRING([--enable-per-user-config-files],
      [Disable per-user configuration files, to save disk accesses during job start-up.  This is likely desirable for large jobs.  Note that this can also be achieved by environment variables at run-time.  (default: enabled)])])
if test "$enable_per_user_config_files" = "no" ; then
  result=0
else
  result=1
fi
AC_DEFINE_UNQUOTED([OPAL_WANT_HOME_CONFIG_FILES], [$result],
     [Enable per-user config files])
OPAL_VAR_SCOPE_POP

#
# Do we want to enable IPv6 support?
#
AC_MSG_CHECKING([if want IPv6 support])
AC_ARG_ENABLE([ipv6],
    [AS_HELP_STRING([--enable-ipv6],
        [Enable IPv6 support, but only if the underlying system supports it (default: disabled)])])
if test "$enable_ipv6" = "yes"; then
    AC_MSG_RESULT([yes])
    opal_want_ipv6=1
else
    AC_MSG_RESULT([no])
    opal_want_ipv6=0
fi
AC_DEFINE_UNQUOTED([OPAL_ENABLE_IPV6], [$opal_want_ipv6],
                   [Enable IPv6 support, but only if the underlying system supports it])


#
# Package/brand string
#
AC_MSG_CHECKING([if want package/brand string])
AC_ARG_WITH([package-string],
     [AS_HELP_STRING([--with-package-string=STRING],
                     [Use a branding string throughout Open MPI])])
if test "$with_package_string" = "" || test "$with_package_string" = "no"; then
    with_package_string="Open MPI $OPAL_CONFIGURE_USER@$OPAL_CONFIGURE_HOST Distribution"
fi
AC_DEFINE_UNQUOTED([OPAL_PACKAGE_STRING], ["$with_package_string"],
     [package/branding string for Open MPI])
AC_MSG_RESULT([$with_package_string])

#
# Ident string
#
AC_MSG_CHECKING([if want ident string])
AC_ARG_WITH([ident-string],
     [AS_HELP_STRING([--with-ident-string=STRING],
                     [Embed an ident string into Open MPI object files])])
if test "$with_ident_string" = "" || test "$with_ident_string" = "no"; then
    with_ident_string="%VERSION%"
fi
# This is complicated, because $OPAL_VERSION may have spaces in it.
# So put the whole sed expr in single quotes -- i.e., directly
# substitute %VERSION% for (not expanded) $OPAL_VERSION.
with_ident_string="`echo $with_ident_string | sed -e 's/%VERSION%/$OPAL_VERSION/'`"

# Now eval an echo of that so that the "$OPAL_VERSION" token is
# replaced with its value.  Enclose the whole thing in "" so that it
# ends up as 1 token.
with_ident_string="`eval echo $with_ident_string`"

AC_DEFINE_UNQUOTED([OPAL_IDENT_STRING], ["$with_ident_string"],
     [ident string for Open MPI])
AC_MSG_RESULT([$with_ident_string])


#
# Use alternative checksum algorithm
#
AC_MSG_CHECKING([if want to use an alternative checksum algo for messages])
AC_ARG_WITH([dst-checksum],
     [AS_HELP_STRING([--with-dst-checksum],
                     [Use an alternative checksum algorithm for messages])])
if test "$with_dst_checksum" = "yes"; then
    AC_MSG_RESULT([yes])
    CFLAGS="-DOPAL_CSUM_DST $CFLAGS"
else
    AC_MSG_RESULT([no])
fi


#
# User level (mpi.h.in) visible maximum lengths of strings.
# These may be required in lower-level libraries to set up matching
# data-structures (e.g. OPAL_MAX_OBJECT_NAME).
#
# Default values (as of OMPI-1.3), and some sane minimum and maximum values
#

# No lower and upper bound required or enforced
OPAL_WITH_OPTION_MIN_MAX_VALUE(processor_name,  256,  16, 1024)

# Min length according to information passed in ompi/errhandler/errcode.c
OPAL_WITH_OPTION_MIN_MAX_VALUE(error_string,    256,  64, 1024)

# Min length according to MPI-2.1, p. 236 (information passed in ompi/communicator/comm.c: min only 48)
OPAL_WITH_OPTION_MIN_MAX_VALUE(object_name,      64,  64,  256)

# Min and Max length according to MPI-2.1, p. 287 is 32; longest key in ROMIO however 33
OPAL_WITH_OPTION_MIN_MAX_VALUE(info_key,         36,  34,  255)

# No lower and upper bound required or enforced!
OPAL_WITH_OPTION_MIN_MAX_VALUE(info_val,        256,  32, 1024)

# Min length according to _POSIX_HOST_NAME_MAX=255 (4*HOST_NAME_MAX)
OPAL_WITH_OPTION_MIN_MAX_VALUE(port_name,      1024, 255, 2048)

# Min length according to MPI-2.1, p. 418
OPAL_WITH_OPTION_MIN_MAX_VALUE(datarep_string,  128,  64,  256)

OPAL_WITH_OPTION_MIN_MAX_VALUE(pset_name_len,  512,  512, 4096)

OPAL_WITH_OPTION_MIN_MAX_VALUE(stringtag_len,     1024, 256, 2048)

# some systems don't want/like getpwuid
AC_MSG_CHECKING([if want getpwuid support])
AC_ARG_ENABLE([getpwuid],
    [AS_HELP_STRING([--disable-getpwuid],
        [Disable getpwuid support (default: enabled)])])
if test "$enable_getpwuid" = "no"; then
    AC_MSG_RESULT([no])
    opal_want_getpwuid=0
else
    AC_MSG_RESULT([yes])
    opal_want_getpwuid=1
fi
AC_DEFINE_UNQUOTED([OPAL_ENABLE_GETPWUID], [$opal_want_getpwuid],
                   [Disable getpwuid support (default: enabled)])

dnl We no longer support the old OPAL_ENABLE_PROGRESS_THREADS.  At
dnl some point, this should die.
AC_DEFINE([OPAL_ENABLE_PROGRESS_THREADS],
          [0],
          [Whether we want BTL progress threads enabled])
])dnl
