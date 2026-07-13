/*
 * Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
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
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak PMPI_ABI_SET_FORTRAN_INFO = ompi_abi_set_fortran_info_f
#pragma weak pmpi_abi_set_fortran_info = ompi_abi_set_fortran_info_f
#pragma weak pmpi_abi_set_fortran_info_ = ompi_abi_set_fortran_info_f
#pragma weak pmpi_abi_set_fortran_info__ = ompi_abi_set_fortran_info_f

#pragma weak PMPI_Abi_set_fortran_info_f = ompi_abi_set_fortran_info_f
#pragma weak PMPI_Abi_set_fortran_info_f08 = ompi_abi_set_fortran_info_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ABI_SET_FORTRAN_INFO,
                           pmpi_abi_set_fortran_info,
                           pmpi_abi_set_fortran_info_,
                           pmpi_abi_set_fortran_info__,
                           ompi_abi_set_fortran_info_f,
                           (MPI_Fint *info, MPI_Fint *ierr),
                           (info, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_ABI_SET_FORTRAN_INFO = ompi_abi_set_fortran_info_f
#pragma weak mpi_abi_set_fortran_info = ompi_abi_set_fortran_info_f
#pragma weak mpi_abi_set_fortran_info_ = ompi_abi_set_fortran_info_f
#pragma weak mpi_abi_set_fortran_info__ = ompi_abi_set_fortran_info_f

#pragma weak MPI_Abi_set_fortran_info_f = ompi_abi_set_fortran_info_f
#pragma weak MPI_Abi_set_fortran_info_f08 = ompi_abi_set_fortran_info_f
#else
OMPI_GENERATE_WEAK_F77_BINDINGS (MPI_ABI_SET_FORTRAN_INFO,
                           mpi_abi_set_fortran_info,
                           mpi_abi_set_fortran_info_,
                           mpi_abi_set_fortran_info__,
                           ompi_abi_set_fortran_info_f,
                           (MPI_Fint *info, MPI_Fint *ierr),
                           (info, ierr) )
#endif


void ompi_abi_set_fortran_info_f(MPI_Fint *info, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Info c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPI_Abi_set_fortran_info(c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
