# Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# If CUDA support was requested, then build the CUDA smBTL, otherwise
# there will be no need.
# This code checks makes sure the check was done earlier by the
# opal_check_cuda.m4 code. It also copies the flags and libs under
# opal_cuda_CPPFLAGS, opal_cuda_LDFLAGS, and opal_cuda_LIBS

AC_DEFUN([MCA_opal_btl_smcuda_CONFIG],[

    AC_CONFIG_FILES([opal/mca/btl/smcuda/Makefile])

    OPAL_CHECK_CUDA([btl_smcuda])

    AS_IF([test "x$CUDA_SUPPORT" = "x1"],
          [$1],
          [$2])

    AC_SUBST([btl_smcuda_CPPFLAGS], [opal_cuda_CPPFLAGS])
    AC_SUBST([btl_smcuda_LDFLAGS], [opal_cuda_LDFLAGS])
    AC_SUBST([btl_smcuda_LIBS], [opal_cuda_LIBS])

])dnl
