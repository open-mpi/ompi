# -*- shell-script -*-
#
# Copyright (c) 2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This file is m4_included in the top-level configure.ac only if we
# are building the ompi project.  You cannot put an AC_CONFIG_FILES in
# an AC_DEFUN that is conditionally called (because Autoconf will
# still process the AC_CONFIG_FILES unconditionally); you can only m4
# include the file or not.

AC_CONFIG_FILES([
    ompi/Makefile
    ompi/etc/Makefile
    ompi/include/Makefile
    ompi/include/mpif.h
    ompi/include/mpif-config.h

    ompi/datatype/Makefile
    ompi/debuggers/Makefile

    ompi/mpi/c/Makefile
    ompi/mpi/c/profile/Makefile
    ompi/mpi/cxx/Makefile
    ompi/mpi/f77/Makefile
    ompi/mpi/f77/profile/Makefile
    ompi/mpi/f90/Makefile
    ompi/mpi/f90/fortran_kinds.sh
    ompi/mpi/f90/fortran_sizes.h
    ompi/mpi/f90/scripts/Makefile

    ompi/tools/ompi_info/Makefile
    ompi/tools/wrappers/Makefile
    ompi/tools/wrappers/mpicc-wrapper-data.txt
    ompi/tools/wrappers/mpic++-wrapper-data.txt
    ompi/tools/wrappers/mpif77-wrapper-data.txt
    ompi/tools/wrappers/mpif90-wrapper-data.txt
    ompi/tools/ortetools/Makefile
    ompi/tools/ompi-server/Makefile
    ompi/tools/ompi-probe/Makefile
    ompi/tools/ompi-profiler/Makefile

    test/peruse/Makefile
])
