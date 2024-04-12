# -*- shell-script -*-
#
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_pmix_gds_hash_CONFIG], [
    AC_CONFIG_FILES([src/mca/gds/hash/Makefile])
    dnl This component is always available.
    AS_IF([test "yes" = "yes"],
          [$1
           pmix_gds_hash=yes],
          [$2
           pmix_gds_hash=no])

    PMIX_SUMMARY_ADD([GDS], [Hash], [], [$pmix_gds_hash])
])dnl
