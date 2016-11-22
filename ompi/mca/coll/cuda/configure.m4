# -*- shell-script -*-
#
# Copyright (c) 2014      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_coll_cuda_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_coll_cuda_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/cuda/Makefile])

    # make sure that CUDA-aware checks have been done
    AC_REQUIRE([OPAL_CHECK_CUDA])

    # Only build if CUDA support is available
    AS_IF([test "x$CUDA_SUPPORT" = "x1"],
          [$1],
          [$2])

])dnl

