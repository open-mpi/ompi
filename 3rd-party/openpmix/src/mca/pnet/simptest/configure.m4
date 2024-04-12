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

# MCA_pmix_pnet_simptest_CONFIG([action-if-can-compile],
#                               [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_pnet_simptest_CONFIG], [
    AC_CONFIG_FILES([src/mca/pnet/simptest/Makefile])

    AC_ARG_WITH([simptest], [AS_HELP_STRING([--with-simptest], [Include simptest fabric support])],
                [pmix_want_simptest=yes], [pmix_want_simptest=no])

    AS_IF([test "$pmix_want_simptest" = "yes"],
          [$1],
          [$2])

    PMIX_SUMMARY_ADD([Transports], [Simptest], [], [$pmix_want_simptest])
])dnl
