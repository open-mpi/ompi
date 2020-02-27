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
# Copyright (c) 2014-2018 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (c) 2020      Triad National Security, LLC. All rights
#                         reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# define an internal function for checking the existence
# and validity of an external PMIx library
#
# OPAL_CHECK_PMIX_LIB(installdir, libdir, [action-if-valid], [action-if-not-valid])
AC_DEFUN([OPAL_CHECK_PMIX_LIB],[

    OPAL_VAR_SCOPE_PUSH([opal_external_pmix_save_CPPFLAGS opal_external_pmix_save_LDFLAGS opal_external_pmix_save_LIBS])
    opal_external_pmix_happy=no

    # Make sure we have the headers and libs in the correct location
    AC_MSG_CHECKING([for pmix.h in $1])
    files=`ls $1/pmix.h 2> /dev/null | wc -l`
    AS_IF([test "$files" -gt 0],
         [AC_MSG_RESULT([found])
          pmix_ext_install_incdir=$1
          opal_external_pmix_header_happy=yes],
         [AC_MSG_RESULT([not found])
          AC_MSG_CHECKING([for pmix.h in $1/include])
          files=`ls $1/include/pmix.h 2> /dev/null | wc -l`
          AS_IF([test "$files" -gt 0],
                [AC_MSG_RESULT([found])
                 pmix_ext_install_incdir=$1/include
                 opal_external_pmix_header_happy=yes],
                [AC_MSG_RESULT([not found])
                 opal_external_pmix_header_happy=no])])

    AS_IF([test "$opal_external_pmix_header_happy" = "yes"],
         [AS_IF([test -n "$2"],
                [AC_MSG_CHECKING([libpmix.* in $2])
                 files=`ls $2/libpmix.* 2> /dev/null | wc -l`
                 AS_IF([test "$files" -gt 0],
                       [AC_MSG_RESULT([found])
                        pmix_ext_install_libdir=$2],
                       [AC_MSG_RESULT([not found])
                        AC_MSG_CHECKING([libpmix.* in $2/lib64])
                        files=`ls $2/lib64/libpmix.* 2> /dev/null | wc -l`
                        AS_IF([test "$files" -gt 0],
                              [AC_MSG_RESULT([found])
                               pmix_ext_install_libdir=$2/lib64],
                              [AC_MSG_RESULT([not found])
                               AC_MSG_CHECKING([libpmix.* in $2/lib])
                               files=`ls $2/lib/libpmix.* 2> /dev/null | wc -l`
                               AS_IF([test "$files" -gt 0],
                                     [AC_MSG_RESULT([found])
                                      pmix_ext_install_libdir=$2/lib],
                                     [AC_MSG_RESULT([not found])
                                      AC_MSG_ERROR([Cannot continue])])])])],
                [# check for presence of lib64 directory - if found, see if the
                 # desired library is present and matches our build requirements
                 AC_MSG_CHECKING([libpmix.* in $1/lib64])
                 files=`ls $1/lib64/libpmix.* 2> /dev/null | wc -l`
                 AS_IF([test "$files" -gt 0],
                       [AC_MSG_RESULT([found])
                        pmix_ext_install_libdir=$1/lib64],
                       [AC_MSG_RESULT([not found])
                        AC_MSG_CHECKING([libpmix.* in $1/lib])
                        files=`ls $1/lib/libpmix.* 2> /dev/null | wc -l`
                        AS_IF([test "$files" -gt 0],
                              [AC_MSG_RESULT([found])
                               pmix_ext_install_libdir=$1/lib],
                              [AC_MSG_RESULT([not found])
                               AC_MSG_ERROR([Cannot continue])])])])

          # check the version
          opal_external_pmix_save_CPPFLAGS=$CPPFLAGS
          opal_external_pmix_save_LDFLAGS=$LDFLAGS
          opal_external_pmix_save_LIBS=$LIBS

          # if the pmix_version.h file does not exist, then
          # this must be from a pre-1.1.5 version OMPI does
          # NOT support anything older than v1.2.5
          AC_MSG_CHECKING([PMIx version])
          AS_IF([test "$pmix_ext_install_incdir" != "/usr" && test "$pmix_ext_install_incdir" != "/usr/include"],
                [CPPFLAGS="-I$pmix_ext_install_incdir $CPPFLAGS"])
          AS_IF([test "$pmix_ext_install_libdir" != "/usr" && test "$pmix_ext_install_libdir" != "/usr/include"],
                [LDFLAGS="-L$pmix_ext_install_libdir $LDFLAGS"])
          LIBS="$LIBS -lpmix"

          AS_IF([test "x`ls $1/include/pmix_version.h 2> /dev/null`" = "x"],
                [AC_MSG_RESULT([version file not found - assuming v1.1.4])
                 opal_external_pmix_version_found=1
                 opal_external_pmix_happy=no
                 opal_external_pmix_version=internal],
                [AC_MSG_RESULT([version file found])
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
                                     opal_external_pmix_version_found=1
                                     opal_external_pmix_happy=yes],
                                    [AC_MSG_RESULT([not found])])])

          AS_IF([test "$opal_external_pmix_version_found" = "0"],
                [AC_MSG_CHECKING([version 3x or above])
                 AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                              #include <pmix_version.h>
                                              #if (PMIX_VERSION_MAJOR != 3L)
                                              #error "not version 3"
                                              #endif
                                              ], [])],
                                   [AC_MSG_RESULT([found])
                                    opal_external_pmix_version=3x
                                    opal_external_pmix_version_found=1
                                    opal_external_pmix_happy=yes],
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
                                    opal_external_pmix_version_found=1
                                    opal_external_pmix_happy=yes],
                                   [AC_MSG_RESULT([not found])])])

          AS_IF([test "$opal_external_pmix_version_found" = "0"],
                [AC_MSG_CHECKING([version 1x])
                 AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                              #include <pmix_version.h>
                                              #if (PMIX_VERSION_MAJOR != 1L && PMIX_VERSION_MINOR != 2L)
                                              #error "not version 1.2.x"
                                              #endif
                                              ], [])],
                                   [AC_MSG_RESULT([found])
                                    opal_external_pmix_version=1x
                                    opal_external_pmix_version_found=1
                                    opal_external_have_pmix1=1
                                    opal_external_pmix_happy=yes],
                                   [AC_MSG_RESULT([not found])])])

          AS_IF([test "x$opal_external_pmix_version" = "x"],
                [AC_MSG_WARN([External PMIx support detected, but version])
                 AC_MSG_WARN([information of the external lib could not])
                 AC_MSG_WARN([be detected])
                 opal_external_pmix_happy=no])

    ])
    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [$3
           # add the new flags to our wrapper compilers
           AS_IF([test "$pmix_ext_install_incdir" != "/usr" && test "$pmix_ext_install_incdir" != "/usr/include"],
                 [pmix_external_WRAPPER_EXTRA_CPPFLAGS="-I$pmix_ext_install_incdir"])
           AS_IF([test "$pmix_ext_install_libdir" != "/usr" && test "$pmix_ext_install_libdir" != "/usr/include"],
                 [pmix_external_WRAPPER_EXTRA_LDFLAGS="-L$pmix_ext_install_libdir"
                  pmix_external_WRAPPER_EXTRA_LIBS="-lpmix"])],
          [$4])

dnl swap back in original LDFLAGS, LIBS to avoid messing up subsequent configury checks
dnl don't swap back in orig CFLAGS as there are lots of places where the external pmix
dnl header file location needs to be known
    LDFLAGS=$opal_external_pmix_save_LDFLAGS
    LIBS=$opal_external_pmix_save_LIBS

    OPAL_VAR_SCOPE_POP
])


AC_DEFUN([OPAL_CHECK_PMIX],[

    AC_ARG_WITH([pmix],
                [AC_HELP_STRING([--with-pmix(=DIR)],
                                [Build PMIx support.  DIR can take one of three values: "internal", "external", or a valid directory name.  "internal" (or no DIR value) forces Open MPI to use its internal copy of PMIx.  "external" forces Open MPI to use an external installation of PMIx.  Supplying a valid directory name also forces Open MPI to use an external installation of PMIx, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries. Note that Open MPI does not support --without-pmix.])])

    AC_ARG_WITH([pmix-libdir],
                [AC_HELP_STRING([--with-pmix-libdir=DIR],
                                [Look for libpmix in the given directory DIR, DIR/lib or DIR/lib64])])

    AS_IF([test "$with_pmix" = "no"],
          [AC_MSG_WARN([Open MPI requires PMIx support. It can be built])
           AC_MSG_WARN([with either its own internal copy of PMIx, or with])
           AC_MSG_WARN([an external copy that you supply.])
           AC_MSG_ERROR([Cannot continue])])

    opal_external_have_pmix1=0
    AC_MSG_CHECKING([if user requested internal PMIx support($with_pmix)])
    opal_external_pmix_happy=no
    pmix_ext_install_libdir=
    pmix_ext_install_dir=

    AS_IF([test "$with_pmix" = "internal"],
          [AC_MSG_RESULT([yes])
           opal_external_pmix_happy=no
           opal_external_pmix_version=internal
           opal_enable_pmix=yes],

          [AC_MSG_RESULT([no])
           # check for external pmix lib */
           AS_IF([test -z "$with_pmix" || test "$with_pmix" = "yes" || test "$with_pmix" = "external"],
                 [pmix_ext_install_dir=/usr],
                 [pmix_ext_install_dir=$with_pmix])
           AS_IF([test -n "$with_pmix_libdir"],
                 [pmix_ext_install_libdir=$with_pmix_libdir])
           OPAL_CHECK_PMIX_LIB([$pmix_ext_install_dir],
                               [$pmix_ext_install_libdir],
                               [opal_external_pmix_happy=yes
                                opal_enable_pmix=yes],
                               [opal_external_pmix_happy=no])])

    # Final check - if they explicitly pointed us at an external
    # installation that wasn't acceptable, then error out
    AS_IF([test -n "$with_pmix" && test "$with_pmix" != "yes" && test "$with_pmix" != "external" && test "$with_pmix" != "internal" && test "$opal_external_pmix_happy" = "no"],
          [AC_MSG_WARN([External PMIx support requested, but either the version])
           AC_MSG_WARN([of the external lib was not supported or the required])
           AC_MSG_WARN([header/library files were not found])
           AC_MSG_ERROR([Cannot continue])])

    # Final check - if they didn't point us explicitly at an external version
    # but we found one anyway, use the internal version if it is higher
    AS_IF([test "$opal_external_pmix_version" != "internal" && (test -z "$with_pmix" || test "$with_pmix" = "yes")],
          [AS_IF([test "$opal_external_pmix_version" != "4x"],
                 [AC_MSG_WARN([discovered external PMIx version is less than internal version 4.x])
                  AC_MSG_WARN([using internal PMIx])
                  opal_external_pmix_version=internal
                  opal_external_pmix_happy=no])])

    AC_MSG_CHECKING([PMIx version to be used])
    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [AC_MSG_RESULT([external($opal_external_pmix_version)])
           AS_IF([test "$pmix_ext_install_dir" != "/usr"],
                 [opal_external_pmix_CPPFLAGS="-I$pmix_ext_install_dir/include"
                  opal_external_pmix_LDFLAGS=-L$pmix_ext_install_libdir])
           opal_external_pmix_LIBS="-lpmix"],
          [AC_MSG_RESULT([internal])])

    AC_DEFINE_UNQUOTED([OPAL_PMIX_V1],[$opal_external_have_pmix1],
                       [Whether the external PMIx library is v1])

    AC_SUBST(opal_external_pmix_LDFLAGS)
    AC_SUBST(opal_external_pmix_LIBS)

    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [AS_IF([test "$opal_external_pmix_version" = "1x"],
                 [OPAL_SUMMARY_ADD([[Miscellaneous]],[[PMIx support]], [opal_pmix], [External (1.2.5) WARNING - DYNAMIC OPS NOT SUPPORTED])],
                 [OPAL_SUMMARY_ADD([[Miscellaneous]],[[PMIx support]], [opal_pmix], [External ($opal_external_pmix_version)])])],
          [OPAL_SUMMARY_ADD([[Miscellaneous]], [[PMIx support]], [opal_pmix], [Internal])])
])
