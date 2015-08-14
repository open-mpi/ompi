# -*- shell-script -*-
#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# Copyright (c) 2015      NVIDIA, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_MPIEXT_cuda_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([OMPI_MPIEXT_cuda_CONFIG],[
    AC_CONFIG_FILES([ompi/mpiext/cuda/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/cuda/c/Makefile])
    AC_CONFIG_HEADER([ompi/mpiext/cuda/c/mpiext_cuda_c.h])

    AC_DEFINE_UNQUOTED([MPIX_CUDA_AWARE_SUPPORT],[$CUDA_SUPPORT],
                       [Macro that is set to 1 when CUDA-aware support is configured in and 0 when it is not])

    # We compile this whether CUDA support was requested or not. It allows
    # us to to detect if we have CUDA support.
    AS_IF([test "$ENABLE_cuda" = "1" || \
           test "$ENABLE_EXT_ALL" = "1"],
          [$1],
          [$2])
])
