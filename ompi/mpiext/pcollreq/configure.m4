# -*- shell-script -*-
#
# Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_MPIEXT_pcollreq_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([OMPI_MPIEXT_pcollreq_CONFIG],[
    AC_CONFIG_FILES([ompi/mpiext/pcollreq/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/pcollreq/c/Makefile])

    AS_IF([test "$ENABLE_pcollreq" = "1" || \
           test "$ENABLE_EXT_ALL" = "1"],
          [$1],
          [$2])
])
