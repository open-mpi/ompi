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
AC_DEFUN([MCA_opal_rcache_gpusm_CONFIG],[

    AC_CONFIG_FILES([opal/mca/rcache/gpusm/Makefile])

    # This component shall be configured only after the accelerator discovery
    # has been completed. This discovery is part of the OPAL accelerator framework.
    AC_MSG_CHECKING([if any accelerator components were found (cuda, rocm, ze)])
    AS_IF([test "x$OMPI_HAVE_ACCELERATOR_SUPPORT" = "x1"],
              [AC_MSG_RESULT([yes])
              $1],
              [AC_MSG_RESULT([no])
              $2])

])dnl
