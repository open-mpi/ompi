# -*- shell-script -*-
#
# Copyright (c) 2009-2022 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017-2020 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2018      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
# Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
#                         reserved.
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
        ompi/mpi/fortran/base/Makefile
        ompi/mpi/fortran/mpif-h/Makefile
        ompi/mpi/fortran/mpif-h/profile/Makefile
        ompi/mpi/fortran/use-mpi/Makefile
        ompi/mpi/fortran/use-mpi/mpi-types.F90
        ompi/mpi/fortran/use-mpi-tkr/Makefile
        ompi/mpi/fortran/use-mpi-tkr/fortran_sizes.h
        ompi/mpi/fortran/use-mpi-tkr/fortran_kinds.sh
        ompi/mpi/fortran/use-mpi-ignore-tkr/Makefile
        ompi/mpi/fortran/use-mpi-ignore-tkr/mpi-ignore-tkr-interfaces.h
        ompi/mpi/fortran/use-mpi-ignore-tkr/mpi-ignore-tkr-file-interfaces.h
        ompi/mpi/fortran/use-mpi-ignore-tkr/mpi-ignore-tkr-removed-interfaces.h
        ompi/mpi/fortran/use-mpi-f08/Makefile
        ompi/mpi/fortran/use-mpi-f08/base/Makefile
        ompi/mpi/fortran/use-mpi-f08/profile/Makefile
        ompi/mpi/fortran/use-mpi-f08/bindings/Makefile
        ompi/mpi/fortran/use-mpi-f08/mod/Makefile
        ompi/mpi/fortran/use-mpi-f08/mod/mpi-f08-interfaces.h
        ompi/mpi/fortran/mpiext-use-mpi/Makefile
        ompi/mpi/fortran/mpiext-use-mpi-f08/Makefile
        ompi/mpi/tool/Makefile

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
        ompi/tools/mpisync/Makefile
        ompi/tools/mpirun/Makefile
    ])
])
