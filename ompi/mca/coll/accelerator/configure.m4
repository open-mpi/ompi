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
# accelerator collective component.
#
AC_DEFUN([MCA_ompi_coll_accelerator_CONFIG],[

    AC_CONFIG_FILES([ompi/mca/coll/accelerator/Makefile])

    OPAL_CHECK_CUDA([coll_accelerator])

    AS_IF([test "x$OMPI_HAVE_ACCELERATOR_SUPPORT" = "x1"],
          [$1],
          [$2])

])dnl
