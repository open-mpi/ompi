# -*- shell-script -*-
#
# Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# If CUDA support was requested, then build the CUDA support library.
# This code checks the variable CUDA_SUPPORT which was set earlier in
# the configure sequence by the opal_configure_options.m4 code.
#

AC_DEFUN([MCA_ompi_common_cuda_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/common/cuda/Makefile])

    # Use CUDA_SUPPORT which was filled in by the opal configure code.
    AM_CONDITIONAL([MCA_ompi_cuda_support], [test "x$CUDA_SUPPORT" = "x1"])
    AC_DEFINE_UNQUOTED([OMPI_CUDA_SUPPORT],$CUDA_SUPPORT,
                       [Whether we want cuda memory registration support in OMPI code])
    AS_IF([test "x$CUDA_SUPPORT" = "x1"],
          [$1],
          [$2])

    # Copy over the includes and libs needed to build CUDA
    common_cuda_CPPFLAGS=$opal_datatype_CPPFLAGS
    common_cuda_LIBS=$opal_datatype_LIBS
    AC_SUBST([common_cuda_CPPFLAGS])
    AC_SUBST([common_cuda_LIBS])

])dnl
