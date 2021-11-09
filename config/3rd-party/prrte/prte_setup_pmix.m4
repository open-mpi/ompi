# -*- shell-script ; indent-tabs-mode:nil -*-
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
# Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2014-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2021      Nanook Consulting  All rights reserved.
# Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# We have two modes for building pmix.
#
# First is as a co-built pmix.  In this case, Prte's CPPFLAGS
# will be set before configure to include the right -Is to pick up
# pmix headers and LIBS will point to where the .la file for
# pmix will exist.  When co-building, pmix's configure will be
# run already, but the library will not yet be built.  It is ok to run
# any compile-time (not link-time) tests in this mode.  This mode is
# used when the --with-pmix=cobuild option is specified.
#
# Second is an external package.  In this case, all compile and link
# time tests can be run.  This macro must do any CPPFLAGS/LDFLAGS/LIBS
# modifications it desires in order to compile and link against
# pmix.  This mode is used whenever the other modes are not used.
#
# PRTE_SETUP_PMIX()
# --------------------------------------------------------------------
AC_DEFUN([PRTE_CHECK_PMIX],[
    PRTE_VAR_SCOPE_PUSH([prte_external_pmix_save_CPPFLAGS prte_external_pmix_save_LDFLAGS prte_external_pmix_save_LIBS prte_external_pmix_version_found prte_external_pmix_version])

    AC_ARG_WITH([pmix],
                [AS_HELP_STRING([--with-pmix(=DIR)],
                                [Where to find PMIx support, optionally adding DIR to the search path])])

    AC_ARG_WITH([pmix-libdir],
                [AS_HELP_STRING([--with-pmix-libdir=DIR],
                                [Look for libpmix in the given directory DIR, DIR/lib or DIR/lib64])])

    AC_ARG_ENABLE([pmix-devel-support],
                  [AS_HELP_STRING([--enable-pmix-devel-support],
                                  [Add necessary wrapper flags to enable access to PMIx devel headers])])

    prte_pmix_support=0

    if test "$with_pmix" = "no"; then
        AC_MSG_WARN([PRTE requires PMIx support using])
        AC_MSG_WARN([an external copy that you supply.])
        AC_MSG_ERROR([Cannot continue])
    fi

    prte_external_pmix_save_CPPFLAGS=$CPPFLAGS
    prte_external_pmix_save_LDFLAGS=$LDFLAGS
    prte_external_pmix_save_LIBS=$LIBS

    # figure out our mode...
    AS_IF([test "$with_pmix" = "cobuild"],
          [_PRTE_PMIX_EMBEDDED_MODE()],
          [_PRTE_PMIX_EXTERNAL()])

    # need to add resulting flags to global ones so we can
    # test the version
    if test ! -z "$prte_pmix_CPPFLAGS"; then
        PRTE_FLAGS_PREPEND_UNIQ(CPPFLAGS, $prte_pmix_CPPFLAGS)
    fi
    if test ! -z "$prte_pmix_LDFLAGS"; then
        PRTE_FLAGS_PREPEND_UNIQ(LDFLAGS, $prte_pmix_LDFLAGS)
    fi
    if test ! -z "$prte_pmix_LIBS"; then
        PRTE_FLAGS_PREPEND_UNIQ(LIBS, $prte_pmix_LIBS)
    fi

    # if the version file exists, then we need to parse it to find
    # the actual release series
    AC_MSG_CHECKING([version 4x])
    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                        #include <pmix_version.h>
                                        #if (PMIX_VERSION_MAJOR < 4L)
                                        #error "not version 4 or above"
                                        #endif
                                       ], [])],
                      [AC_MSG_RESULT([found])
                       prte_external_pmix_version=4x
                       prte_external_pmix_version_found=4],
                      [AC_MSG_RESULT([not found])
                       prte_external_pmix_version_found=0])

    AS_IF([test "$prte_external_pmix_version_found" = "4"],
          [AC_MSG_CHECKING([version 4.1 or greater])
            AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                                #include <pmix_version.h>
                                                #if (PMIX_VERSION_MAJOR == 4L && PMIX_VERSION_MINOR < 1L)
                                                #error "not version 4.1 or above"
                                                #endif
                                               ], [])],
                              [AC_MSG_RESULT([found])],
                              [AC_MSG_RESULT([not found])
                               prte_external_pmix_version_found=0])])

    # restore the global flags
    CPPFLAGS=$prte_external_pmix_save_CPPFLAGS
    LDFLAGS=$prte_external_pmix_save_LDFLAGS
    LIBS=$prte_external_pmix_save_LIBS

    AS_IF([test "$prte_external_pmix_version_found" = "0"],
          [AC_MSG_WARN([PRTE does not support PMIx versions])
           AC_MSG_WARN([less than v4.1 as only PMIx-based tools can])
           AC_MSG_WARN([can connect to the server.])
           AC_MSG_ERROR([Please select a newer version and configure again])])

    if test ! -z "$prte_pmix_CPPFLAGS"; then
        PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_CPPFLAGS, $prte_pmix_CPPFLAGS)
    fi
    if test ! -z "$prte_pmix_LDFLAGS"; then
        PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_LDFLAGS, $prte_pmix_LDFLAGS)
    fi
    if test ! -z "$prte_pmix_LIBS"; then
        PRTE_FLAGS_APPEND_UNIQ(PRTE_FINAL_LIBS, $prte_pmix_LIBS)
    fi

    PMIXCC_PATH=""
    if test -z "$pmix_ext_install_dir"; then
        PRTE_WHICH([pmixcc], [PMIXCC_PATH])
        AS_IF([test -z "$PMIXCC_PATH"],
                [AC_MSG_WARN([Could not find pmixcc in PATH])
                 prte_pmixcc_happy=no],
                [prte_pmixcc_happy=yes])
    else
        PMIXCC_PATH=$pmix_ext_install_dir/bin
        if test -d $PMIXCC_PATH; then
            PMIXCC_PATH=$PMIXCC_PATH/pmixcc
            if test -e $PMIXCC_PATH; then
                prte_pmixcc_happy=yes

            else
                AC_MSG_WARN([Could not find usable $PMIXCC_PATH])
                prte_pmixcc_happy=no
            fi
        else
            AC_MSG_WARN([Could not find $PMIXCC_PATH])
            prte_pmixcc_happy=no
        fi
    fi
    AM_CONDITIONAL(PRTE_HAVE_PMIXCC, test "$prte_pmixcc_happy" = "yes")
    AC_SUBST(PMIXCC_PATH)

    PRTE_SUMMARY_ADD([[Required Packages]],[[PMIx]],[pmix],[yes ($prte_pmix_source)])

    PRTE_VAR_SCOPE_POP

    dnl This is needed for backwards comptability with the 2.1 and
    dnl earlier branches.
    AC_DEFINE([PRTE_PMIX_HEADER], [<pmix.h>], [PMIx header to use])
])dnl


AC_DEFUN([_PRTE_PMIX_EMBEDDED_MODE],[
    AC_MSG_CHECKING([for PMIX])
    AC_MSG_RESULT(["cobuild"])

    AC_ARG_WITH([pmix-cobuild-libs],
                [AS_HELP_STRING([--with-pmix-cobuild-libs=LIBS],
                                [Add the LIBS string to LIBS])])

    prte_pmix_source="cobuild"
    PRTE_FLAGS_APPEND_UNIQ([PRTE_FINAL_LIBS], [$with_pmix_cobuild_libs])
])dnl


dnl To support cobuilds, any tests that require linking or running (as
dnl opposed to preprocessing or compiling) should only exist in the
dnl PMIX_EXTERNAL section, since cobuild does not guarantee that a
dnl library will be available at configure time.
AC_DEFUN([_PRTE_PMIX_EXTERNAL],[
    PRTE_VAR_SCOPE_PUSH([pmix_prefix pmixdir_prefix pmix_ext_install_libdir])

    # get rid of any trailing slash(es)
    pmix_prefix=$(echo $with_pmix | sed -e 'sX/*$XXg')
    pmixdir_prefix=$(echo $with_pmix_libdir | sed -e 'sX/*$XXg')

    # check for external pmix header */
    AS_IF([test ! -z "$pmix_prefix" && test "$pmix_prefix" != "yes"],
                 [pmix_ext_install_dir="$pmix_prefix"],
                 [pmix_ext_install_dir=""])

    AS_IF([test ! -z "$pmixdir_prefix" && test "$pmixdir_prefix" != "yes"],
                 [pmix_ext_install_libdir="$pmixdir_prefix"],
                 [AS_IF([test ! -z "$pmix_prefix" && test "$pmix_prefix" != "yes"],
                        [if test -d $pmix_prefix/lib64; then
                            pmix_ext_install_libdir=$pmix_prefix/lib64
                         elif test -d $pmix_prefix/lib; then
                            pmix_ext_install_libdir=$pmix_prefix/lib
                         else
                            AC_MSG_WARN([Could not find $pmix_prefix/lib or $pmix_prefix/lib64])
                            AC_MSG_ERROR([Can not continue])
                         fi
                        ],
                        [pmix_ext_install_libdir=""])])

    PRTE_CHECK_PACKAGE([prte_pmix],
                       [pmix.h],
                       [pmix],
                       [PMIx_Init],
                       [],
                       [$pmix_ext_install_dir],
                       [$pmix_ext_install_libdir],
                       [],
                       [],
                       [AC_MSG_WARN([PRTE requires PMIx support using])
                        AC_MSG_WARN([an external copy that you supply.])
                        if test -z "$pmix_ext_install_libdir"; then
                            AC_MSG_WARN([The library was not found in standard locations.])
                        else
                            AC_MSG_WARN([The library was not found in $pmix_ext_install_libdir.])
                        fi
                        AC_MSG_ERROR([Cannot continue])])

    if test -z "$pmix_ext_install_dir"; then
        prte_pmix_source="Standard locations"
    else
        prte_pmix_source=$pmix_ext_install_dir
    fi

    PRTE_VAR_SCOPE_POP
])dnl
