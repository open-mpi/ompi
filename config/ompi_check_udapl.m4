# -*- shell-script -*-
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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_CHECK_UDAPL(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if uDAPL support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_UDAPL],[
    AC_ARG_WITH([udapl],
        [AC_HELP_STRING([--with-udapl(=DIR)],
             [Build uDAPL support, searching for libraries in DIR])])
    AC_ARG_WITH([udapl-libdir],
       [AC_HELP_STRING([--with-udapl-libdir=DIR],
             [Search for uDAPL libraries in DIR])])

    AS_IF([test ! -z "$with_udapl" -a "$with_udapl" != "yes"],
          [ompi_check_udapl_dir="$with_udapl"])
    AS_IF([test ! -z "$with_udapl_libdir" -a "$with_udapl_libdir" != "yes"],
          [ompi_check_udapl_libdir="$with_udapl_libdir"])
    AS_IF([test "$with_udapl" = "no"],
          [ompi_check_udapl_happy="no"],
          [ompi_check_udapl_happy="yes"])

dnl Do not use ompi_check_package directly, because then we have
dnl to test for the header file twice, and caching is disabled
dnl for all ompi_check_package checks.  Instead, do what
dnl ompi_check_package does, but only do the header check once.
dnl Still do the lib check twice, the second time if it turns
dnl out we need -ldapl to link (looks like udapl over GM).

    ompi_check_package_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_package_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_package_$1_save_LIBS="$LIBS"

    ompi_check_package_$1_orig_CPPFLAGS="$$1_CPPFLAGS"
    ompi_check_package_$1_orig_LDFLAGS="$$1_LDFLAGS"
    ompi_check_package_$1_orig_LIBS="$$1_LIBS"

    AS_IF([test "$ompi_check_udapl_happy" = "yes"],
          [_OMPI_CHECK_PACKAGE_HEADER([$1], 
                [dat/udat.h],
                [$ompi_check_udapl_dir],
                [ompi_check_udapl_happy="yes"],
                [ompi_check_udapl_happy="no"])])

    AS_IF([test "$ompi_check_udapl_happy" = "yes"],
          [_OMPI_CHECK_PACKAGE_LIB([$1],
                [dat],
                [dat_registry_list_providers],
                [],
                [$ompi_check_udapl_dir],
                [$ompi_check_udapl_libdir],
                [ompi_check_udapl_happy="yes"],
                [_OMPI_CHECK_PACKAGE_LIB([$1],
                      [dat],
                      [dat_registry_list_providers],
                      [-ldapl],
                      [$ompi_check_udapl_dir],
                      [$ompi_check_udapl_libdir],
                      [ompi_check_udapl_happy="yes"],
                      [ompi_check_udapl_happy="no"])])])

    CPPFLAGS="$ompi_check_package_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_package_$1_save_LDFLAGS"
    LIBS="$ompi_check_package_$1_save_LIBS"

    AS_IF([test "$ompi_check_udapl_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_udapl" -a "$with_udapl" != "no"],
                 [AC_MSG_ERROR([uDAPL support requested but not found.  Aborting])])
           $3])
])

