# -*- shell-script -*-
#
# Copyright (c) 2021      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_MPIEXT_continue_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([OMPI_MPIEXT_continue_CONFIG],[
    AC_CONFIG_FILES([ompi/mpiext/continue/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/continue/c/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/continue/c/profile/Makefile])

    # This module is not stable yet so it should only be built
    # if explicitly requested
    AS_IF([test "$ENABLE_continue" = "1"],
          [$1],
          [$2])

    AS_IF([test "$ENABLE_continue" = "1"],
          [AC_DEFINE_UNQUOTED([OMPI_HAVE_MPI_EXT_CONTINUE], [1],
                              [Whether MPI Continuations are enabled])],
          [])
])dnl

# we need init/finalize
AC_DEFUN([OMPI_MPIEXT_continue_NEED_INIT], [1])
