/*
 * Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/attribute/attribute.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_ABI_SET_FOTRAN_INFO = ompi_abi_set_fortran_info_f
#pragma weak pmpi_abi_set_fortran_info = ompi_abi_set_fortran_info_f
#pragma weak pmpi_abi_set_fortran_info_ = ompi_abi_set_fortran_info_f
#pragma weak pmpi_abi_set_fortran_info__ = ompi_abi_set_fortran_info_f

#pragma weak PMPI_Abi_set_fortran_info_f = ompi_abi_set_fortran_info_f
#pragma weak PMPI_Abi_set_fortran_info_f08 = ompi_abi_set_fortran_info_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ABI_SET_FOTRAN_INFO,
                           pmpi_abi_set_fortran_info,
                           pmpi_abi_set_fortran_info_,
                           pmpi_abi_set_fortran_info__,
                           pompi_abi_set_fortran_info_f,
                           (MPI_Fint *info, MPI_Fint *ierr),
                           (info, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ABI_SET_FOTRAN_INFO = ompi_abi_set_fortran_info_f
#pragma weak mpi_abi_set_fortran_info = ompi_abi_set_fortran_info_f
#pragma weak mpi_abi_set_fortran_info_ = ompi_abi_set_fortran_info_f
#pragma weak mpi_abi_set_fortran_info__ = ompi_abi_set_fortran_info_f

#pragma weak MPI_Abi_set_fortran_info_f = ompi_abi_set_fortran_info_f
#pragma weak MPI_Abi_set_fortran_info_f08 = ompi_abi_set_fortran_info_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ABI_SET_FOTRAN_INFO,
                           mpi_abi_set_fortran_info,
                           mpi_abi_set_fortran_info_,
                           mpi_abi_set_fortran_info__,
                           ompi_abi_set_fortran_info_f,
                           (MPI_Fint *info, MPI_Fint *ierr),
                           (info, ierr) )
#else
#define ompi_abi_set_fortran_info_f pompi_abi_set_fortran_info_f
#endif
#endif


void ompi_abi_set_fortran_info_f(MPI_Fint *info, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Info c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPI_Abi_set_fortran_info(c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
