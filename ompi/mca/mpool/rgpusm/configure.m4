# -*- shell-script -*-
#
# Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# If CUDA support was requested, then build the CUDA memory pools.
# This code checks the variable CUDA_SUPPORT which was set earlier in
# the configure sequence by the opal_configure_options.m4 code.
#

AC_DEFUN([MCA_ompi_mpool_rgpusm_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/mpool/rgpusm/Makefile])

    # Use CUDA_SUPPORT which was filled in by the opal configure code.
    AS_IF([test "x$CUDA_SUPPORT_41" = "x1"],
          [$1],
          [$2])

])dnl
