# -*- shell-script -*-
#
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2012      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_io_CONFIG(project_name, framework_name)
# -------------------------------------------
AC_DEFUN([MCA_ompi_io_CONFIG],
[
    OPAL_VAR_SCOPE_PUSH([define_mpi_io])

    AS_IF([test "$enable_mpi_io" != "no"],
          [OMPI_MPIF_IO_CONSTANTS_INCLUDE="include \"mpif-io-constants.h\""
           OMPI_MPIF_IO_HANDLES_INCLUDE="include \"mpif-io-handles.h\""
           define_mpi_io=1],
          [OMPI_MPIF_IO_CONSTANTS_INCLUDE=
           OMPI_MPIF_IO_HANDLES_INCLUDE=
           define_mpi_io=0])
    AC_SUBST(OMPI_MPIF_IO_CONSTANTS_INCLUDE)
    AC_SUBST(OMPI_MPIF_IO_HANDLES_INCLUDE)

    MCA_CONFIGURE_FRAMEWORK([$1], [$2], [$define_mpi_io])

    OMPI_PROVIDE_MPI_FILE_INTERFACE=$define_mpi_io
    AC_SUBST(OMPI_PROVIDE_MPI_FILE_INTERFACE)
    AC_DEFINE_UNQUOTED([OMPI_PROVIDE_MPI_FILE_INTERFACE], [$define_mpi_io],
                       [Whether OMPI should provide MPI File interface])
    AM_CONDITIONAL([OMPI_PROVIDE_MPI_FILE_INTERFACE], [test "$enable_mpi_io" != "no"])

    OPAL_VAR_SCOPE_POP
])
