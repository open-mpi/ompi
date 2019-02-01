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
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2014-2017 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OPAL_CHECK_PMIX],[

    OPAL_VAR_SCOPE_PUSH([opal_external_pmix_save_CPPFLAGS opal_external_pmix_save_LDFLAGS opal_external_pmix_save_LIBS])

    AC_ARG_WITH([pmix],
                [AC_HELP_STRING([--with-pmix(=DIR)],
                                [Where to find PMIx support, optionally adding DIR to the search path])])

    AC_ARG_WITH([pmix-libdir],
                [AC_HELP_STRING([--with-pmix-libdir=DIR],
                                [Look for libpmix in the given directory DIR, DIR/lib or DIR/lib64])])

    AS_IF([test "$with_pmix" = "no"],
          [AC_MSG_WARN([PRTE requires PMIx support using])
           AC_MSG_WARN([an external copy that you supply.])
           AC_MSG_ERROR([Cannot continue])])

    opal_external_have_pmix1=0

    # check for external pmix lib */
    AS_IF([test -z "$with_pmix"],
          [pmix_ext_install_dir=/usr],
          [pmix_ext_install_dir=$with_pmix])

    # Make sure we have the headers and libs in the correct location
    OPAL_CHECK_WITHDIR([pmix], [$pmix_ext_install_dir/include], [pmix.h])

    AS_IF([test -n "$with_pmix_libdir"],
          [AC_MSG_CHECKING([libpmix.* in $with_pmix_libdir])
           files=`ls $with_pmix_libdir/libpmix.* 2> /dev/null | wc -l`
           AS_IF([test "$files" -gt 0],
                 [AC_MSG_RESULT([found])
                  pmix_ext_install_libdir=$with_pmix_libdir],
                 [AC_MSG_RESULT([not found])
                  AC_MSG_CHECKING([libpmix.* in $with_pmix_libdir/lib64])
                  files=`ls $with_pmix_libdir/lib64/libpmix.* 2> /dev/null | wc -l`
                  AS_IF([test "$files" -gt 0],
                        [AC_MSG_RESULT([found])
                         pmix_ext_install_libdir=$with_pmix_libdir/lib64],
                        [AC_MSG_RESULT([not found])
                         AC_MSG_CHECKING([libpmix.* in $with_pmix_libdir/lib])
                         files=`ls $with_pmix_libdir/lib/libpmix.* 2> /dev/null | wc -l`
                         AS_IF([test "$files" -gt 0],
                               [AC_MSG_RESULT([found])
                                pmix_ext_install_libdir=$with_pmix_libdir/lib],
                                [AC_MSG_RESULT([not found])
                                 AC_MSG_ERROR([Cannot continue])])])])],
          [# check for presence of lib64 directory - if found, see if the
           # desired library is present and matches our build requirements
           AC_MSG_CHECKING([libpmix.* in $pmix_ext_install_dir/lib64])
           files=`ls $pmix_ext_install_dir/lib64/libpmix.* 2> /dev/null | wc -l`
           AS_IF([test "$files" -gt 0],
           [AC_MSG_RESULT([found])
            pmix_ext_install_libdir=$pmix_ext_install_dir/lib64],
           [AC_MSG_RESULT([not found])
            AC_MSG_CHECKING([libpmix.* in $pmix_ext_install_dir/lib])
            files=`ls $pmix_ext_install_dir/lib/libpmix.* 2> /dev/null | wc -l`
            AS_IF([test "$files" -gt 0],
                  [AC_MSG_RESULT([found])
                   pmix_ext_install_libdir=$pmix_ext_install_dir/lib],
                  [AC_MSG_RESULT([not found])
                   AC_MSG_ERROR([Cannot continue])])])])

    # check the version
    opal_external_pmix_save_CPPFLAGS=$CPPFLAGS
    opal_external_pmix_save_LDFLAGS=$LDFLAGS
    opal_external_pmix_save_LIBS=$LIBS

    # if the pmix_version.h file does not exist, then
    # this must be from a pre-1.1.5 version
    AC_MSG_CHECKING([for PMIx version file])
    CPPFLAGS="-I$pmix_ext_install_dir/include $CPPFLAGS"
    AS_IF([test "x`ls $pmix_ext_install_dir/include/pmix_version.h 2> /dev/null`" = "x"],
           [AC_MSG_RESULT([not found - assuming pre-v2.0])
            AC_MSG_WARN([PRTE does not support PMIx versions])
            AC_MSG_WARN([less than v2.0 as only PMIx-based tools can])
            AC_MSG_WARN([can connect to the server.])
            AC_MSG_ERROR([Please select a newer version and configure again])],
           [AC_MSG_RESULT([found])
            opal_external_pmix_version_found=0])

    # if it does exist, then we need to parse it to find
    # the actual release series
    AS_IF([test "$opal_external_pmix_version_found" = "0"],
          [AC_MSG_CHECKING([version 4x])
            AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                                #include <pmix_version.h>
                                                #if (PMIX_VERSION_MAJOR < 4L)
                                                #error "not version 4 or above"
                                                #endif
                                               ], [])],
                              [AC_MSG_RESULT([found])
                               opal_external_pmix_version=4x
                               opal_external_pmix_version_found=4
                               opal_prun_happy=yes],
                              [AC_MSG_RESULT([not found])])])

    AS_IF([test "$opal_external_pmix_version_found" = "0"],
          [AC_MSG_CHECKING([version 3x])
            AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                                #include <pmix_version.h>
                                                #if (PMIX_VERSION_MAJOR != 3L)
                                                #error "not version 3"
                                                #endif
                                               ], [])],
                              [AC_MSG_RESULT([found])
                               opal_external_pmix_version=3x
                               opal_external_pmix_version_found=3
                               opal_prun_happy=yes],
                              [AC_MSG_RESULT([not found])])])

    AS_IF([test "$opal_external_pmix_version_found" = "0"],
          [AC_MSG_CHECKING([version 2x])
           AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                               #include <pmix_version.h>
                                               #if (PMIX_VERSION_MAJOR != 2L)
                                               #error "not version 2"
                                               #endif
                                              ], [])],
                             [AC_MSG_RESULT([found])
                              opal_external_pmix_version=2x
                              opal_external_pmix_version_found=2
                              opal_prun_happy=yes],
                             [AC_MSG_RESULT([not found])])])

    AS_IF([test "$opal_external_pmix_version_found" = "0"],
          [AC_MSG_CHECKING([version 1x])
           AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                               #include <pmix_version.h>
                                               #if (PMIX_VERSION_MAJOR != 1L)
                                               #error "not version 1"
                                               #endif
                                              ], [])],
                             [AC_MSG_RESULT([found])
                              AC_MSG_WARN([PRTE does not support PMIx versions])
                              AC_MSG_WARN([less than v2.0 as only PMIx-based tools can])
                              AC_MSG_WARN([can connect to the server.])
                              AC_MSG_ERROR([Please select a newer version and configure again])],
                             [AC_MSG_RESULT([not found])])])

    AS_IF([test "x$opal_external_pmix_version" = "x"],
          [AC_MSG_WARN([PMIx version information could not])
           AC_MSG_WARN([be detected])
           AC_MSG_ERROR([cannot continue])])

    CPPFLAGS=$opal_external_pmix_save_CPPFLAGS
    LDFLAGS=$opal_external_pmix_save_LDFLAGS
    LIBS=$opal_external_pmix_save_LIBS

    AS_IF([test "$pmix_ext_install_dir" != "/usr"],
          [CPPFLAGS="-I$pmix_ext_install_dir/include $CPPFLAGS"
           LDFLAGS="-L$pmix_ext_install_libdir $LDFLAGS"])

    LIBS="$LIBS -lpmix"

    CPPFLAGS="$CPPFLAGS -I$pmix_ext_install_dir/include"
    OPAL_WRAPPER_FLAGS_ADD([CPPFLAGS], [-I$pmix_ext_install_dir/include])

    LDFLAGS="$LDFLAGS -L$pmix_ext_install_libdir"
    OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [-L$pmix_ext_install_libdir])

    opal_external_pmix_happy=yes

    OPAL_SUMMARY_ADD([[Required Packages]],[[PMIx]],[pmix],[$pmix_ext_install_dir])

    OPAL_VAR_SCOPE_POP
])
