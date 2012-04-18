# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
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

    # This example can always build, so we just execute $1.
    $1
])

# only need to set this if the component needs init/finalize hooks
AC_DEFUN([OMPI_MPIEXT_example_NEED_INIT], [1])
