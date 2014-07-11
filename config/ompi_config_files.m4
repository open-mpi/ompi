# -*- shell-script -*-
#
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([OMPI_CONFIG_FILES],[
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
        ompi/mpi/fortran/base/Makefile
        ompi/mpi/fortran/mpif-h/Makefile
        ompi/mpi/fortran/mpif-h/profile/Makefile
        ompi/mpi/fortran/use-mpi-tkr/Makefile
        ompi/mpi/fortran/use-mpi-tkr/fortran_sizes.h
        ompi/mpi/fortran/use-mpi-tkr/fortran_kinds.sh
        ompi/mpi/fortran/use-mpi-ignore-tkr/Makefile
        ompi/mpi/fortran/use-mpi-ignore-tkr/mpi-ignore-tkr-interfaces.h
        ompi/mpi/fortran/use-mpi-ignore-tkr/mpi-ignore-tkr-file-interfaces.h
        ompi/mpi/fortran/use-mpi-f08/Makefile
        ompi/mpi/fortran/use-mpi-f08-desc/Makefile
        ompi/mpi/fortran/mpiext/Makefile
        ompi/mpi/tool/Makefile
        ompi/mpi/tool/profile/Makefile
    
        ompi/tools/ompi_info/Makefile
        ompi/tools/wrappers/Makefile
        ompi/tools/wrappers/mpicc-wrapper-data.txt
        ompi/tools/wrappers/mpic++-wrapper-data.txt
        ompi/tools/wrappers/mpifort-wrapper-data.txt
        ompi/tools/wrappers/ompi.pc
        ompi/tools/wrappers/ompi-c.pc
        ompi/tools/wrappers/ompi-cxx.pc
        ompi/tools/wrappers/ompi-fort.pc
        ompi/tools/wrappers/mpijavac.pl
    ])
])
