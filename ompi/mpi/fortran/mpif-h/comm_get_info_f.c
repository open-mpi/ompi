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
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_COMM_GET_INFO = ompi_comm_get_info_f
#pragma weak pmpi_comm_get_info = ompi_comm_get_info_f
#pragma weak pmpi_comm_get_info_ = ompi_comm_get_info_f
#pragma weak pmpi_comm_get_info__ = ompi_comm_get_info_f

#pragma weak PMPI_Comm_get_info_f = ompi_comm_get_info_f
#pragma weak PMPI_Comm_get_info_f08 = ompi_comm_get_info_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_GET_INFO,
                           pmpi_comm_get_info,
                           pmpi_comm_get_info_,
                           pmpi_comm_get_info__,
                           pompi_comm_get_info_f,
                           (MPI_Fint *comm, MPI_Fint *info_used, MPI_Fint *ierr),
                           (comm, info_used, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_GET_INFO = ompi_comm_get_info_f
#pragma weak mpi_comm_get_info = ompi_comm_get_info_f
#pragma weak mpi_comm_get_info_ = ompi_comm_get_info_f
#pragma weak mpi_comm_get_info__ = ompi_comm_get_info_f

#pragma weak MPI_Comm_get_info_f = ompi_comm_get_info_f
#pragma weak MPI_Comm_get_info_f08 = ompi_comm_get_info_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_GET_INFO,
                           mpi_comm_get_info,
                           mpi_comm_get_info_,
                           mpi_comm_get_info__,
                           ompi_comm_get_info_f,
                           (MPI_Fint *comm, MPI_Fint *info_used, MPI_Fint *ierr),
                           (comm, info_used, ierr) )
#else
#define ompi_comm_get_info_f pompi_comm_get_info_f
#endif
#endif


void ompi_comm_get_info_f(MPI_Fint *comm, MPI_Fint *info_used, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Info c_info;

    c_ierr = PMPI_Comm_get_info(c_comm, &c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *info_used = PMPI_Info_c2f(c_info);
    }
}
