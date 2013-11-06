# -*- shell-script -*-
#
# Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# If CUDA support was requested, then build the CUDA support library.
# This code checks just makes sure the check was done earlier by the
# opal_check_cuda.m4 code.
#

AC_DEFUN([MCA_ompi_common_cuda_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/common/cuda/Makefile])

    # make sure that CUDA-aware checks have been done
    AC_REQUIRE([OPAL_CHECK_CUDA])

    AS_IF([test "x$CUDA_SUPPORT" = "x1"],
          [$1],
          [$2])

    # Copy over the includes needed to build CUDA
    common_cuda_CPPFLAGS=$opal_datatype_cuda_CPPFLAGS
    AC_SUBST([common_cuda_CPPFLAGS])

])dnl
