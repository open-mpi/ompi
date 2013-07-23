# -*- shell-script -*-
#
# Copyright (c) 2009      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_btl_smcuda_CONFIG([action-if-can-compile],
#                   [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_btl_smcuda_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/btl/smcuda/Makefile])

    # make sure that CUDA-aware checks have been done
    AC_REQUIRE([OPAL_CHECK_CUDA])

    # Only build if CUDA 4.1 support is available
    AS_IF([test "x$CUDA_SUPPORT_41" = "x1"],
          [$1],
          [$2])

])dnl
