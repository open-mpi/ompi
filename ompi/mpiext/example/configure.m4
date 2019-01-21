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

# OMPI_MPIEXT_example_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([OMPI_MPIEXT_example_CONFIG],[
    AC_CONFIG_FILES([ompi/mpiext/example/Makefile])

    AC_CONFIG_FILES([ompi/mpiext/example/c/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/example/mpif-h/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/example/use-mpi/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/example/use-mpi-f08/Makefile])

    # If your extension can build, run $1.  Otherwise, run $2.  For
    # the purposes of this example, we don't want it to build in most
    # cases.  So only build if someone specifies an --enable-mpi-ext
    # value that contains the token "example".
    AS_IF([test "$ENABLE_example" = "1" || \
           test "$ENABLE_EXT_ALL" = "1"],
          [$1],
          [$2])
])

# only need to set this if the component needs init/finalize hooks
AC_DEFUN([OMPI_MPIEXT_example_NEED_INIT], [1])

# By default, mpiext_example_mpifh.h is included in the source file
# of the mpi_ext module. To disable it, define this macro as 0.
#AC_DEFUN([OMPI_MPIEXT_example_INCLUDE_MPIFH_IN_USEMPI], [0])

# By default, mpiext_example_mpifh.h is included in the source file
# of the mpi_f08_ext module. To disable it, define this macro as 0.
#AC_DEFUN([OMPI_MPIEXT_example_INCLUDE_MPIFH_IN_USEMPIF08], [0])
