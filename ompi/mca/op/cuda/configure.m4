# -*- shell-script -*-
#
# Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2013      The University of Tennessee and The University
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
# If CUDA support was requested, then build the CUDA support library.
# This code checks makes sure the check was done earlier by the
# opal_check_cuda.m4 code. It also copies the flags and libs under
# opal_cuda_CPPFLAGS, opal_cuda_LDFLAGS, and opal_cuda_LIBS

AC_DEFUN([MCA_ompi_op_cuda_CONFIG],[

    AC_CONFIG_FILES([ompi/mca/op/cuda/Makefile])

    OPAL_CHECK_CUDA([op_cuda])

    AS_IF([test "x$CUDA_SUPPORT" = "x1"],
          [$1],
          [$2])

    AC_SUBST([op_cuda_CPPFLAGS])
    AC_SUBST([op_cuda_LDFLAGS])
    AC_SUBST([op_cuda_LIBS])

])dnl
