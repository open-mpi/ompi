# -*- shell-script -*-
#
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_pnet_sshot_CONFIG([action-if-can-compile],
#                            [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_pnet_sshot_CONFIG], [
    AC_CONFIG_FILES([src/mca/pnet/sshot/Makefile])

    AC_REQUIRE([PMIX_CHECK_CURL])
    AC_REQUIRE([PMIX_CHECK_JANSSON])

    AC_ARG_WITH([slingshot], [AS_HELP_STRING([--with-slingshot], [Include Slingshot fabric support])],
                [pmix_want_sshot=yes], [pmix_want_sshot=no])

    AC_ARG_WITH([cxi], [AS_HELP_STRING([--with-cxi(=DIR)],
                                       [Include CXI service library support, optionally adding DIR/include, DIR/include/cxi, DIR/lib, and DIR/lib64 to the search path for headers and libraries])],
                [], [with_cxi=no])

    AC_ARG_WITH([cxi-libdir],
                [AS_HELP_STRING([--with-cxi-libdir=DIR],
                                [Search for CXI libraries in DIR])])

    AC_MSG_CHECKING([include CXI support])
    AS_IF([test "$with_cxi" = "no"],
          [AC_MSG_RESULT([no])
           pmix_check_cxi_happy=no],
          [AC_MSG_RESULT([yes])
           AS_IF([test ! -z "$with_cxi" && test "$with_cxi" != "yes"],
                 [with_cxi_incdir="$with_cxi"
                  AS_IF([test ! -d "$with_cxi_incdir" || test ! -f "$with_cxi_incdir/cxi.h"],
                         [$with_cxi_incdir=$with_cxi_incdir/include
                          AS_IF([test ! -d "$with_cxi_incdir" || test ! -f "$with_cxi_incdir/cxi.h"],
                                [$with_cxi_incdir=$with_cxi_incdir/cxi
                                 AS_IF([test ! -d "$with_cxi_incdir" || test ! -f "$with_cxi_incdir/cxi.h"],
                                       [AC_MSG_WARN([CXI library support requested, but])
                                        AC_MSG_WARN([required header file cxi.h not found. Locations tested:])
                                        AC_MSG_WARN([    $with_cxi])
                                        AC_MSG_WARN([    $with_cxi/include])
                                        AC_MSG_WARN([    $with_cxi/include/cxi])
                                        AC_MSG_ERROR([Cannot continue])])])])],
                 [with_cxi_incdir="/usr/include/cxi"])

           OAC_CHECK_PACKAGE([cxi],
                             [pnet_cxi],
                             [cxi.h],
                             [cxi],
                             [CXI_FUNCTION],
                             [pmix_check_cxi_happy="yes"],
                             [pmix_check_cxi_happy="no"])
           ])

    # for NOW, hardwire cxi support to be happy
    pmix_check_cxi_happy=yes

    AS_IF([test "$pmix_want_sshot" = "yes" && test "$pmix_check_cxi_happy" = "yes" && test "$pmix_check_jansson_happy" = "yes" && test "$pmix_check_curl_happy" = "yes"],
          [$1
           pnet_sshot_happy=yes],
          [$2
           pnet_sshot_happy=no])

    PMIX_SUMMARY_ADD([Transports], [HPE Slingshot], [], [$pnet_sshot_happy])

])dnl
