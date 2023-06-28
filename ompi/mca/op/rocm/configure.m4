# -*- shell-script -*-
#
# Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2023      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# If ROCm support was requested, then build the ROCm support library.
# This code checks makes sure the check was done earlier by the
# opal_check_rocm.m4 code. It also copies the flags and libs under
# opal_rocm_CPPFLAGS, opal_rocm_LDFLAGS, and opal_rocm_LIBS

AC_DEFUN([MCA_ompi_op_rocm_CONFIG],[

    AC_CONFIG_FILES([ompi/mca/op/rocm/Makefile])

    OPAL_CHECK_CUDA([op_rocm])

    AS_IF([test "x$ROCM_SUPPORT" = "x1"],
          [$1],
          [$2])

    AC_SUBST([op_rocm_CPPFLAGS])
    AC_SUBST([op_rocm_LDFLAGS])
    AC_SUBST([op_rocm_LIBS])

])dnl
