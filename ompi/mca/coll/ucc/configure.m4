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


# MCA_coll_ucc_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_coll_ucc_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/ucc/Makefile])

    OMPI_CHECK_UCC([coll_ucc],
                     [coll_ucc_happy="yes"],
                     [coll_ucc_happy="no"])

    # Share the memory hooks set up of common/ucx, when it is built: loading
    # the UCC library loads UCX, and its memory hooks, in the process.
    coll_ucc_COMMON_UCX_LIBS=
    coll_ucc_have_common_ucx=0
    AS_IF([test "$coll_ucc_happy" = "yes" && test "$common_ucx_happy" = "yes"],
          [OPAL_MCA_CHECK_DEPENDENCY([ompi], [coll], [ucc], [opal], [common], [ucx])
           coll_ucc_have_common_ucx=1
           coll_ucc_CPPFLAGS="$coll_ucc_CPPFLAGS $common_ucx_CPPFLAGS"
           coll_ucc_COMMON_UCX_LIBS="\$(OPAL_TOP_BUILDDIR)/opal/mca/common/ucx/lib${OPAL_LIB_NAME}mca_common_ucx.la"])
    AC_DEFINE_UNQUOTED([OMPI_COLL_UCC_HAVE_COMMON_UCX], [$coll_ucc_have_common_ucx],
                       [Whether the coll/ucc component sets up the memory hooks through common/ucx])

    AS_IF([test "$coll_ucc_happy" = "yes"],
          [coll_ucc_WRAPPER_EXTRA_LDFLAGS="$coll_ucc_LDFLAGS"
           coll_ucc_CPPFLAGS="$coll_ucc_CPPFLAGS"
           coll_ucc_WRAPPER_EXTRA_LIBS="$coll_ucc_LIBS"
           $1],
          [$2])
    
    OPAL_SUMMARY_ADD([Miscellaneous], [Open UCC], [], [$coll_ucc_happy])

    # substitute in the things needed to build ucc
    AC_SUBST([coll_ucc_CPPFLAGS])
    AC_SUBST([coll_ucc_LDFLAGS])
    AC_SUBST([coll_ucc_LIBS])
    AC_SUBST([coll_ucc_COMMON_UCX_LIBS])
])dnl
