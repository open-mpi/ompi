# Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# If any accelerators have been discovered, then build support for the
# accelerator BTL. This assumes the discovery has already been done.
#
# Beware: Un like the name seems to indicate this BTl is generic and used by
# all accelerators.

AC_DEFUN([MCA_opal_btl_smcuda_CONFIG],[

    AC_CONFIG_FILES([opal/mca/btl/smcuda/Makefile])

    OPAL_CHECK_CUDA([btl_smcuda])

    AS_IF([test "x$OMPI_HAVE_ACCELERATOR_SUPPORT" = "x1"],
          [$1],
          [$2])

])dnl
