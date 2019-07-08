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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
# Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pnet_opa_CONFIG([action-if-can-compile],
#                     [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_pnet_opa_CONFIG],[
    AC_CONFIG_FILES([src/mca/pnet/opa/Makefile])

    PMIX_CHECK_PSM2([pnet_opa],
                    [pnet_opa_happy="yes"],
                    [pnet_opa_happy="no"])

    AC_ARG_WITH([opamgt],
        [AC_HELP_STRING([--with-opamgt(=DIR)],
        [Build OmniPath Fabric Management support (optionally adding DIR/include, DIR/include/opamgt, DIR/lib, and DIR/lib64 to the search path for headers and libraries])], [], [with_opamgt=no])

    AC_ARG_WITH([opamgt-libdir],
                [AC_HELP_STRING([--with-opamgt-libdir=DIR],
                                [Search for OmniPath Fabric Management libraries in DIR])])

    pmix_check_opamgt_save_CPPFLAGS="$CPPFLAGS"
    pmix_check_opamgt_save_LDFLAGS="$LDFLAGS"
    pmix_check_opamgt_save_LIBS="$LIBS"

    pmix_check_opamgt_libdir=
    pmix_check_opamgt_dir=

    AC_MSG_CHECKING([if opamgt requested])
    AS_IF([test "$with_opamgt" = "no"],
          [AC_MSG_RESULT([no])
           pmix_check_opamgt_happy=no],
          [AC_MSG_RESULT([yes])
           PMIX_CHECK_WITHDIR([opamgt-libdir], [$with_opamgt_libdir], [libopamgt.*])
           AS_IF([test ! -z "$with_opamgt" && test "$with_opamgt" != "yes"],
                 [pmix_check_opamgt_dir="$with_opamgt"
                  AS_IF([test ! -d "$pmix_check_opamgt_dir" || test ! -f "$pmix_check_opamgt_dir/opamgt.h"],
                         [$pmix_check_opamgt_dir=$pmix_check_opamgt_dir/include
                          AS_IF([test ! -d "$pmix_check_opamgt_dir" || test ! -f "$pmix_check_opamgt_dir/opamgt.h"],
                                [$pmix_check_opamgt_dir=$pmix_check_opamgt_dir/opamgt
                                 AS_IF([test ! -d "$pmix_check_opamgt_dir" || test ! -f "$pmix_check_opamgt_dir/opamgt.h"],
                                       [AC_MSG_WARN([OmniPath Fabric Management support requested, but])
                                        AC_MSG_WARN([required header file opamgt.h not found. Locations tested:])
                                        AC_MSG_WARN([    $with_opamgt])
                                        AC_MSG_WARN([    $with_opamgt/include])
                                        AC_MSG_WARN([    $with_opamgt/include/opamgt])
                                        AC_MSG_ERROR([Cannot continue])])])])],
                 [pmix_check_opamgt_dir="/usr/include/opamgt"])

           AS_IF([test ! -z "$with_opamgt_libdir" && test "$with_opamgt_libdir" != "yes"],
                 [pmix_check_opamgt_libdir="$with_opamgt_libdir"])

           # no easy way to check this, so let's ensure that the
           # full opamgt install was done, including the iba support
           AS_IF([test ! -d "$pmix_check_opamgt_dir/iba" || test ! -f "$pmix_check_opamgt_dir/iba/vpi.h"],
                 [pmix_check_opamgt_happy="no"],
                 [PMIX_CHECK_PACKAGE([pnet_opamgt],
                                     [opamgt.h],
                                     [opamgt],
                                     [omgt_query_sa],
                                     [],
                                     [$pmix_check_opamgt_dir],
                                     [$pmix_check_opamgt_libdir],
                                     [pmix_check_opamgt_happy="yes"
                                      pnet_opa_CFLAGS="$pnet_opa_CFLAGS $pnet_opamgt_CFLAGS"
                                      pnet_opa_CPPFLAGS="$pnet_opa_CPPFLAGS $pnet_opamgt_CPPFLAGS"
                                      pnet_opa_LDFLAGS="$pnet_opa_LDFLAGS $pnet_opamgt_LDFLAGS"
                                      pnet_opa_LIBS="$pnet_opa_LIBS $pnet_opamgt_LIBS"],
                                     [pmix_check_opamgt_happy="no"])])
           ])

    AS_IF([test "$pmix_check_opamgt_happy" = "yes"],
          [pmix_want_opamgt=1],
          [pmix_want_opamgt=0])
    AC_DEFINE_UNQUOTED([PMIX_WANT_OPAMGT], [$pmix_want_opamgt],
                       [Whether or not to include OmniPath Fabric Manager support])

    CPPFLAGS="$pmix_check_opamgt_save_CPPFLAGS"
    LDFLAGS="$pmix_check_opamgt_save_LDFLAGS"
    LIBS="$pmix_check_opamgt_save_LIBS"

    AS_IF([test "$pnet_opa_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build psm2
    AC_SUBST([pnet_opa_CFLAGS])
    AC_SUBST([pnet_opa_CPPFLAGS])
    AC_SUBST([pnet_opa_LDFLAGS])
    AC_SUBST([pnet_opa_LIBS])
])dnl
