# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_MPIEXT_affinity_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([OMPI_MPIEXT_affinity_CONFIG], [
    AC_CONFIG_FILES([ompi/mpiext/affinity/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/affinity/c/Makefile])

    # This example can always build, so we just execute $1 if it was
    # requested.
    AS_IF([test "$ENABLE_affinity" = "1" || \
           test "$ENABLE_EXT_ALL" = "1"],
          [$1],
          [$2])
])
