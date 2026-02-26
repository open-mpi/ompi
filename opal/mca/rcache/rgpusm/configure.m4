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
# accelerator rcache component.
#
AC_DEFUN([MCA_opal_rcache_rgpusm_CONFIG],[

    AC_CONFIG_FILES([opal/mca/rcache/rgpusm/Makefile])

    OPAL_CHECK_CUDA([rcache_rgpusm])

    AS_IF([test "x$OMPI_HAVE_ACCELERATOR_SUPPORT" = "x1"],
          [$1],
          [$2])

])dnl
