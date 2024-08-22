# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# If any accelerators have been discovered, then build support for the
# accelerator SMSC component.
#
AC_DEFUN([MCA_opal_smsc_accelerator_CONFIG],[

    AC_CONFIG_FILES([opal/mca/smsc/accelerator/Makefile])

    OPAL_CHECK_CUDA([smsc_accelerator])

    AS_IF([test "x$OMPI_HAVE_ACCELERATOR_SUPPORT" = "x1"],
          [$1],
          [$2])

])dnl
