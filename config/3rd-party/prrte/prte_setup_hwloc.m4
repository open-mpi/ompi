# -*- shell-script -*-
#
# Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2021      Nanook Consulting.  All rights reserved.
# Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# PRRTE 2.0 branch calls this PRTE_HWLOC_CONFIG.  2.1 and later calls
# it PRTE_SETUP_HWLOC.
#
AC_DEFUN([PRTE_HWLOC_CONFIG], [PRTE_SETUP_HWLOC])

#
# We have two modes for building hwloc.
#
# First is as a co-built hwloc.  In this case, Prte's CPPFLAGS
# will be set before configure to include the right -Is to pick up
# hwloc headers and LIBS will point to where the .la file for
# hwloc will exist.  When co-building, hwloc's configure will be
# run already, but the library will not yet be built.  It is ok to run
# any compile-time (not link-time) tests in this mode.  This mode is
# used when the --with-hwloc=cobuild option is specified.
#
# Second is an external package.  In this case, all compile and link
# time tests can be run.  This macro must do any CPPFLAGS/LDFLAGS/LIBS
# modifications it desires in order to compile and link against
# hwloc.  This mode is used whenever the other modes are not used.
#
# PRTE_SETUP_HWLOC()
# --------------------------------------------------------------------
AC_DEFUN([PRTE_SETUP_HWLOC], [
    PRTE_VAR_SCOPE_PUSH()

    AC_ARG_WITH([hwloc],
                [AS_HELP_STRING([--with-hwloc=DIR],
                                [Search for hwloc headers and libraries in DIR ])])

    AC_ARG_WITH([hwloc-libdir],
                [AS_HELP_STRING([--with-hwloc-libdir=DIR],
                                [Search for hwloc libraries in DIR ])])

    prte_hwloc_support=1
    prte_check_hwloc_save_CPPFLAGS="$CPPFLAGS"
    prte_check_hwloc_save_LDFLAGS="$LDFLAGS"
    prte_check_hwloc_save_LIBS="$LIBS"
    prte_have_topology_dup=0

    if test "$with_hwloc" == "no"; then
        AC_MSG_WARN([PRRTE requires HWLOC topology library support.])
        AC_MSG_WARN([Please reconfigure so we can find the library.])
        AC_MSG_ERROR([Cannot continue.])
    fi

    # if we're cobuild, don't have to do a package search. Otherwise,
    # search or the package and get the right {CPP,C,LD}FLAGS and
    # LIBS.
    AS_IF([test "$with_hwloc" = "cobuild"],
          [_PRTE_HWLOC_EMBEDDED_MODE()],
          [_PRTE_HWLOC_EXTERNAL([prte_hwloc_support=1], [prte_hwloc_support=0])])

    if test $prte_hwloc_support -eq 0; then
        AC_MSG_WARN([Prte requires HWLOC topology library support, but])
        AC_MSG_WARN([an adequate version of that library was not found.])
        AC_MSG_WARN([Please reconfigure and point to a location where])
        AC_MSG_WARN([the HWLOC library can be found.])
        AC_MSG_ERROR([Cannot continue.])
    fi

    # update global flags to test for HWLOC version
    if test ! -z "$prte_hwloc_CPPFLAGS"; then
        PRTE_FLAGS_PREPEND_UNIQ(CPPFLAGS, $prte_hwloc_CPPFLAGS)
    fi
    if test ! -z "$prte_hwloc_LDFLAGS"; then
        PRTE_FLAGS_PREPEND_UNIQ(LDFLAGS, $prte_hwloc_LDFLAGS)
    fi
    if test ! -z "$prte_hwloc_LIBS"; then
        PRTE_FLAGS_PREPEND_UNIQ(LIBS, $prte_hwloc_LIBS)
    fi

    AC_MSG_CHECKING([if hwloc version is 1.5 or greater])
    AC_COMPILE_IFELSE(
          [AC_LANG_PROGRAM([[#include <hwloc.h>]],
          [[
    #if HWLOC_API_VERSION < 0x00010500
    #error "hwloc API version is less than 0x00010500"
    #endif
          ]])],
          [AC_MSG_RESULT([yes])],
          [AC_MSG_RESULT([no])
           AC_MSG_ERROR([Cannot continue])])

    AC_MSG_CHECKING([if hwloc version is 1.8 or greater])
    AC_COMPILE_IFELSE(
          [AC_LANG_PROGRAM([[#include <hwloc.h>]],
          [[
    #if HWLOC_API_VERSION < 0x00010800
    #error "hwloc API version is less than 0x00010800"
    #endif
          ]])],
          [AC_MSG_RESULT([yes])
           prte_have_topology_dup=1],
          [AC_MSG_RESULT([no])])

    CPPFLAGS=$prte_check_hwloc_save_CPPFLAGS
    LDFLAGS=$prte_check_hwloc_save_LDFLAGS
    LIBS=$prte_check_hwloc_save_LIBS

    if test ! -z "$prte_hwloc_CPPFLAGS"; then
        PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_CPPFLAGS, $prte_hwloc_CPPFLAGS)
    fi
    if test ! -z "$prte_hwloc_LDFLAGS"; then
        PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_LDFLAGS, $prte_hwloc_LDFLAGS)
    fi
    if test ! -z "$prte_hwloc_LIBS"; then
        PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_LIBS, $prte_hwloc_LIBS)
    fi

    AC_DEFINE_UNQUOTED([PRTE_HAVE_HWLOC_TOPOLOGY_DUP], [$prte_have_topology_dup],
                       [Whether or not hwloc_topology_dup is available])

    dnl This is needed for backwards comptability with the 2.1 and
    dnl earlier branches.
    AC_DEFINE([PRTE_HWLOC_HEADER], [<hwloc.h>], [Location of hwloc.h])

    PRTE_SUMMARY_ADD([[Required Packages]],[[HWLOC]], [prte_hwloc], [yes ($prte_hwloc_source)])

    PRTE_VAR_SCOPE_POP()
])dnl


AC_DEFUN([_PRTE_HWLOC_EMBEDDED_MODE],[
    AC_MSG_CHECKING([for hwloc])
    AC_MSG_RESULT(["cobuild"])

    AC_ARG_WITH([hwloc-cobuild-libs],
                [AS_HELP_STRING([--with-hwloc-cobuild-libs=LIBS],
                                [Add the LIBS string to LIBS])])

    prte_hwloc_source="cobuild"
    PRTE_FLAGS_APPEND_UNIQ([PRTE_FINAL_LIBS], [$with_hwloc_cobuild_libs])
])dnl


dnl To support cobuilds, any tests that require linking or running (as
dnl opposed to preprocessing or compiling) should only exist in the
dnl HWLOC_EXTERNAL section, since cobuild does not guarantee that a
dnl library will be available at configure time.
AC_DEFUN([_PRTE_HWLOC_EXTERNAL],[
    PRTE_VAR_SCOPE_PUSH([hwloc_prefix hwlocdir_prefix])

    # get rid of any trailing slash(es)
    hwloc_prefix=$(echo $with_hwloc | sed -e 'sX/*$XXg')
    hwlocdir_prefix=$(echo $with_hwloc_libdir | sed -e 'sX/*$XXg')

    AS_IF([test ! -z "$hwloc_prefix" && test "$hwloc_prefix" != "yes"],
                 [prte_hwloc_dir="$hwloc_prefix"],
                 [prte_hwloc_dir=""])

    AS_IF([test ! -z "$hwlocdir_prefix" && test "$hwlocdir_prefix" != "yes"],
                 [prte_hwloc_libdir="$hwlocdir_prefix"],
                 [AS_IF([test ! -z "$hwloc_prefix" && test "$hwloc_prefix" != "yes"],
                        [if test -d $hwloc_prefix/lib64; then
                            prte_hwloc_libdir=$hwloc_prefix/lib64
                         elif test -d $hwloc_prefix/lib; then
                            prte_hwloc_libdir=$hwloc_prefix/lib
                         else
                            AC_MSG_WARN([Could not find $hwloc_prefix/lib or $hwloc_prefix/lib64])
                            AC_MSG_ERROR([Can not continue])
                         fi
                        ],
                        [prte_hwloc_libdir=""])])

    PRTE_CHECK_PACKAGE([prte_hwloc],
                       [hwloc.h],
                       [hwloc],
                       [hwloc_topology_init],
                       [],
                       [$prte_hwloc_dir],
                       [$prte_hwloc_libdir],
                       [$1],
                       [$2],
                       [])

    if test -z "$prte_hwloc_dir"; then
        prte_hwloc_source="Standard locations"
    else
        prte_hwloc_source=$prte_hwloc_dir
    fi

    PRTE_VAR_SCOPE_POP
])dnl
