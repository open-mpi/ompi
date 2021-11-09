# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2020-2021 Amazon.com, Inc. or its affiliates.  All Rights
#                         reserved.
# Copyright (c) 2021      Nanook Consulting.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# We have two modes for building hwloc.
#
# First is as a co-built hwloc.  In this case, PMIx's CPPFLAGS
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
# PMIX_SETUP_HWLOC()
# --------------------------------------------------------------------
AC_DEFUN([PMIX_SETUP_HWLOC],[
    PMIX_VAR_SCOPE_PUSH([pmix_hwloc_source])

    AC_ARG_WITH([hwloc],
                [AS_HELP_STRING([--with-hwloc=DIR],
                                [Search for hwloc headers and libraries in DIR ])])

    AC_ARG_WITH([hwloc-libdir],
                [AS_HELP_STRING([--with-hwloc-libdir=DIR],
                                [Search for hwloc libraries in DIR ])])

    pmix_hwloc_support=1
    pmix_check_hwloc_save_CPPFLAGS="$CPPFLAGS"
    pmix_check_hwloc_save_LDFLAGS="$LDFLAGS"
    pmix_check_hwloc_save_LIBS="$LIBS"
    pmix_have_topology_dup=0

    if test "$with_hwloc" == "no"; then
        AC_MSG_WARN([PRRTE requires HWLOC topology library support.])
        AC_MSG_WARN([Please reconfigure so we can find the library.])
        AC_MSG_ERROR([Cannot continue.])
    fi

    # if we're cobuild, don't have to do a package search. Otherwise,
    # search or the package and get the right {CPP,C,LD}FLAGS and
    # LIBS.
    AS_IF([test "$with_hwloc" = "cobuild"],
          [_PMIX_HWLOC_EMBEDDED_MODE()],
          [_PMIX_HWLOC_EXTERNAL([pmix_hwloc_support=1], [pmix_hwloc_support=0])])

    if test $pmix_hwloc_support -eq 0; then
        AC_MSG_WARN([PMIx requires HWLOC topology library support, but])
        AC_MSG_WARN([an adequate version of that library was not found.])
        AC_MSG_WARN([Please reconfigure and point to a location where])
        AC_MSG_WARN([the HWLOC library can be found.])
        AC_MSG_ERROR([Cannot continue.])
    fi

    # update global flags to test for HWLOC version
    PMIX_FLAGS_PREPEND_UNIQ([CPPFLAGS], [$pmix_hwloc_CPPFLAGS])
    PMIX_FLAGS_PREPEND_UNIQ([LDFLAGS], [$pmix_hwloc_LDFLAGS])
    PMIX_FLAGS_PREPEND_UNIQ([LIBS], [$pmix_hwloc_LIBS])

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
           pmix_have_topology_dup=1],
          [AC_MSG_RESULT([no])])

    AC_MSG_CHECKING([if hwloc version is 2.0 or greater])
    AC_COMPILE_IFELSE(
          [AC_LANG_PROGRAM([[#include <hwloc.h>]],
          [[
    #if HWLOC_API_VERSION < 0x00020000
    #error "hwloc API version is less than 0x00020000"
    #endif
          ]])],
          [AC_MSG_RESULT([yes])
           pmix_version_high=1],
          [AC_MSG_RESULT([no])
           pmix_version_high=0])

    CPPFLAGS=$pmix_check_hwloc_save_CPPFLAGS
    LDFLAGS=$pmix_check_hwloc_save_LDFLAGS
    LIBS=$pmix_check_hwloc_save_LIBS

    PMIX_FLAGS_APPEND_UNIQ([PMIX_FINAL_CPPFLAGS], [$pmix_hwloc_CPPFLAGS])
    PMIX_WRAPPER_FLAGS_ADD([CPPFLAGS], [$pmix_hwloc_CPPFLAGS])

    PMIX_FLAGS_APPEND_UNIQ([PMIX_FINAL_LDFLAGS], [$pmix_hwloc_LDFLAGS])
    PMIX_WRAPPER_FLAGS_ADD([LDFLAGS], [$pmix_hwloc_LDFLAGS])

    PMIX_FLAGS_APPEND_UNIQ([PMIX_FINAL_LIBS], [$pmix_hwloc_LIBS])
    PMIX_WRAPPER_FLAGS_ADD([LIBS], [$pmix_hwloc_LIBS])

    AC_DEFINE_UNQUOTED([PMIX_HAVE_HWLOC_TOPOLOGY_DUP], [$pmix_have_topology_dup],
                       [Whether or not hwloc_topology_dup is available])

    AM_CONDITIONAL([PMIX_HWLOC_VERSION_HIGH], [test $pmix_version_high -eq 1])

    pmix_hwloc_support_will_build=yes
    if test -z "$pmix_hwloc_dir"; then
        pmix_hwloc_source="Standard locations"
    else
        pmix_hwloc_source=$pmix_hwloc_dir
    fi

    PMIX_SUMMARY_ADD([[Required Packages]],[[HWLOC]], [pmix_hwloc], [$pmix_hwloc_support_will_build ($pmix_hwloc_source)])

    PMIX_VAR_SCOPE_POP

    dnl This is needed for backwards comptability with the 4.2 and
    dnl earlier branches.
    AC_DEFINE([PMIX_HWLOC_HEADER], [<hwloc.h>], [Location of hwloc.h])
])dnl


AC_DEFUN([_PMIX_HWLOC_EMBEDDED_MODE],[
    AC_MSG_CHECKING([for hwloc])
    AC_MSG_RESULT(["cobuild"])

    AC_ARG_WITH([hwloc-cobuild-libs],
                [AS_HELP_STRING([--with-hwloc-cobuild-libs=LIBS],
                                [Add the LIBS string to LIBS])])
    AC_ARG_WITH([hwloc-cobuild-wrapper-libs],
                [AS_HELP_STRING([--with-hwloc-cobuild-wrapper-libs=LIBS],
                                [Add the LIBS string to the wrapper compiler LIBS])])

    pmix_hwloc_dir="cobuild"
    PMIX_FLAGS_APPEND_UNIQ([PMIX_FINAL_LIBS], [$with_hwloc_cobuild_libs])
    PMIX_WRAPPER_FLAGS_ADD([LIBS], [$with_hwloc_cobuild_wrapper_libs])
])dnl


dnl To support cobuilds, any tests that require linking or running (as
dnl opposed to preprocessing or compiling) should only exist in the
dnl HWLOC_EXTERNAL section, since cobuild does not guarantee that a
dnl library will be available at configure time.
AC_DEFUN([_PMIX_HWLOC_EXTERNAL],[
    PMIX_VAR_SCOPE_PUSH([hwloc_prefix hwlocdir_prefix])

    # get rid of any trailing slash(es)
    hwloc_prefix=$(echo $with_hwloc | sed -e 'sX/*$XXg')
    hwlocdir_prefix=$(echo $with_hwloc_libdir | sed -e 'sX/*$XXg')

    AS_IF([test ! -z "$hwloc_prefix" && test "$hwloc_prefix" != "yes"],
                 [pmix_hwloc_dir="$hwloc_prefix"],
                 [pmix_hwloc_dir=""])

    AS_IF([test ! -z "$hwlocdir_prefix" && test "$hwlocdir_prefix" != "yes"],
                 [pmix_hwloc_libdir="$hwlocdir_prefix"],
                 [AS_IF([test ! -z "$hwloc_prefix" && test "$hwloc_prefix" != "yes"],
                        [if test -d $hwloc_prefix/lib64; then
                            pmix_hwloc_libdir=$hwloc_prefix/lib64
                         elif test -d $hwloc_prefix/lib; then
                            pmix_hwloc_libdir=$hwloc_prefix/lib
                         else
                            AC_MSG_WARN([Could not find $hwloc_prefix/lib or $hwloc_prefix/lib64])
                            AC_MSG_ERROR([Can not continue])
                         fi
                        ],
                        [pmix_hwloc_libdir=""])])

    PMIX_CHECK_PACKAGE([pmix_hwloc],
                       [hwloc.h],
                       [hwloc],
                       [hwloc_topology_init],
                       [],
                       [$pmix_hwloc_dir],
                       [$pmix_hwloc_libdir],
                       [pmix_hwloc_support=1],
                       [pmix_hwloc_support=0],
                       [])

    PMIX_VAR_SCOPE_POP
])dnl
