# -*- shell-script -*-
#
# Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2016      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2026      Triad National Security, LLC.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
# SPDX-License-Identifier: BSD-3-Clause-Open-MPI
#

# OMPI_MPIEXT_xcallbacks_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([OMPI_MPIEXT_xcallbacks_CONFIG],[
    AC_CONFIG_FILES([ompi/mpiext/xcallbacks/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/xcallbacks/c/Makefile])

    AS_IF([test "$ENABLE_xcallbacks" = "1" || \
           test "$ENABLE_EXT_ALL" = "1"],
          [$1],
          [$2])
])dnl
