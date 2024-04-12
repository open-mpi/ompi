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

AC_DEFUN([MCA_pmix_gds_shmem2_CONFIG], [
    AC_CONFIG_FILES([src/mca/gds/shmem2/Makefile])
    dnl We rely on large virtual address spaces in gds/shmem2, so make sure that
    dnl we are dealing with a 64-bit architecture. For example, a 32-bit virtual
    dnl address space is probably too small for the 'virtual memory hole'
    dnl finding that we do here. Below assumes support for only 32- and 64-bit
    dnl architectures.
    AS_IF([test $ac_cv_sizeof_void_p -ne 4 && test $oac_have_apple == 0],
          [$1
           pmix_gds_shmem2=yes],
          [$2
           pmix_gds_shmem2=no])

    PMIX_SUMMARY_ADD([GDS], [Shared-Memory], [], [$pmix_gds_shmem2])
])dnl
