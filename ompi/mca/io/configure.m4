# -*- shell-script -*-
#
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  
# All rights reserved. 
# Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
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
    AC_ARG_ENABLE([mpi-io],
        [AC_HELP_STRING([--disable-mpi-io],
            [Disable built-in support for MPI-2 I/O, likely because
             an externally-provided MPI I/O package will be used.
             Default is to use the internal component system and
             its specially modified version of ROMIO])])

    OMPI_MPIF_IO_CONSTANTS_INCLUDE=
    OMPI_MPIF_IO_HANDLES_INCLUDE=
    AS_IF([test "$enable_mpi_io" != "no"],
          [define_mpi_io=1
           OMPI_MPIF_IO_CONSTANTS_INCLUDE="include \"mpif-io-constants.h\""
           OMPI_MPIF_IO_HANDLES_INCLUDE="include \"mpif-io-handles.h\""],
          [define_mpi_io=0])
    AC_SUBST(OMPI_MPIF_IO_CONSTANTS_INCLUDE)
    AC_SUBST(OMPI_MPIF_IO_HANDLES_INCLUDE)

    MCA_CONFIGURE_FRAMEWORK([$1], [$2], [$define_mpi_io])

    OMPI_PROVIDE_MPI_FILE_INTERFACE=$define_mpi_io
    AC_SUBST(OMPI_PROVIDE_MPI_FILE_INTERFACE)
    AC_DEFINE_UNQUOTED([OMPI_PROVIDE_MPI_FILE_INTERFACE], [$define_mpi_io],
                       [Whether OMPI should provide MPI File interface])
    AM_CONDITIONAL([OMPI_PROVIDE_MPI_FILE_INTERFACE], [test "$define_mpi_io" = "1"])

])
