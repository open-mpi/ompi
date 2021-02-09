# -*- shell-script -*-
#
# Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2016      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_MPIEXT_ftmpi_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([OMPI_MPIEXT_ftmpi_CONFIG],[
    AC_CONFIG_FILES([ompi/mpiext/ftmpi/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/ftmpi/c/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/ftmpi/c/profile/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/ftmpi/mpif-h/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/ftmpi/use-mpi/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/ftmpi/use-mpi-f08/Makefile])

    # If we don't want FT, don't compile this component
    AS_IF([test "$opal_want_ft_mpi" = "1"],
        [$1],
        [$2])
])dnl
