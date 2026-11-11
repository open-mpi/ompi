# -*- shell-script -*-
#
#
# Copyright (c) 2021      Mellanox Technologies. All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#


# MCA_scoll_ucc_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_oshmem_scoll_ucc_CONFIG],[
    AC_CONFIG_FILES([oshmem/mca/scoll/ucc/Makefile])

    OMPI_CHECK_UCC([scoll_ucc],
                     [scoll_ucc_happy="yes"],
                     [scoll_ucc_happy="no"])

    # Share the memory hooks set up of common/ucx, when it is built: loading
    # the UCC library loads UCX, and its memory hooks, in the process.
    scoll_ucc_COMMON_UCX_LIBS=
    scoll_ucc_have_common_ucx=0
    AS_IF([test "$scoll_ucc_happy" = "yes" && test "$common_ucx_happy" = "yes"],
          [OPAL_MCA_CHECK_DEPENDENCY([oshmem], [scoll], [ucc], [opal], [common], [ucx])
           scoll_ucc_have_common_ucx=1
           scoll_ucc_CPPFLAGS="$scoll_ucc_CPPFLAGS $common_ucx_CPPFLAGS"
           scoll_ucc_COMMON_UCX_LIBS="\$(OPAL_TOP_BUILDDIR)/opal/mca/common/ucx/lib${OPAL_LIB_NAME}mca_common_ucx.la"])
    AC_DEFINE_UNQUOTED([OSHMEM_SCOLL_UCC_HAVE_COMMON_UCX], [$scoll_ucc_have_common_ucx],
                       [Whether the scoll/ucc component sets up the memory hooks through common/ucx])

    AS_IF([test "$scoll_ucc_happy" = "yes"],
          [scoll_ucc_WRAPPER_EXTRA_LDFLAGS="$scoll_ucc_LDFLAGS"
           scoll_ucc_CPPFLAGS="$scoll_ucc_CPPFLAGS"
           scoll_ucc_WRAPPER_EXTRA_LIBS="$scoll_ucc_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build ucc
    AC_SUBST([scoll_ucc_CPPFLAGS])
    AC_SUBST([scoll_ucc_LDFLAGS])
    AC_SUBST([scoll_ucc_LIBS])
    AC_SUBST([scoll_ucc_COMMON_UCX_LIBS])
])dnl

