# -*- shell-script -*-
#
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_MPIEXT_shortfloat_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([OMPI_MPIEXT_shortfloat_CONFIG],[
    AC_CONFIG_FILES([
        ompi/mpiext/shortfloat/Makefile
        ompi/mpiext/shortfloat/c/Makefile
        ompi/mpiext/shortfloat/mpif-h/Makefile
        ompi/mpiext/shortfloat/use-mpi/Makefile
        ompi/mpiext/shortfloat/use-mpi-f08/Makefile
    ])

    AS_IF([{ test "$ENABLE_shortfloat" = "1" || \
             test "$ENABLE_EXT_ALL" = "1"; } && \
           { test "$ac_cv_type_short_float" = "yes" || \
             test "$ac_cv_type_opal_short_float_t" = "yes"; }],
          [$1],
          [$2])
])

# The mpi_f08_ext module should not include mpiext_shortfloat_mpifh.h
# because types of a datatype are different between the mpi_ext module
# and the mpi_f08_ext module. The former is integer and the latter is
# type(mpi_datatype).
AC_DEFUN([OMPI_MPIEXT_shortfloat_INCLUDE_MPIFH_IN_USEMPIF08], [0])
