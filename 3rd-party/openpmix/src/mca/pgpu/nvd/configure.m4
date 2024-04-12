# -*- shell-script -*-
#
# Copyright (C) 2015-2017 Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_pgpu_nvd_CONFIG(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([MCA_pmix_pgpu_nvd_CONFIG],[
    AC_CONFIG_FILES([src/mca/pgpu/nvd/Makefile])

    AS_IF([test "yes" = "no"],
          [$1
           pmix_pgpu_nvd_happy=yes],
          [$2
           pmix_pgpu_nvd_happy=no])

    PMIX_SUMMARY_ADD([GPUs], [NVIDIA], [], [$pmix_pgpu_nvd_happy])])])
])

