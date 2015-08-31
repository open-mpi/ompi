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
#pragma weak PMPI_COMM_SET_INFO = ompi_comm_set_info_f
#pragma weak pmpi_comm_set_info = ompi_comm_set_info_f
#pragma weak pmpi_comm_set_info_ = ompi_comm_set_info_f
#pragma weak pmpi_comm_set_info__ = ompi_comm_set_info_f

#pragma weak PMPI_Comm_set_info_f = ompi_comm_set_info_f
#pragma weak PMPI_Comm_set_info_f08 = ompi_comm_set_info_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SET_INFO,
                           pmpi_comm_set_info,
                           pmpi_comm_set_info_,
                           pmpi_comm_set_info__,
                           pompi_comm_set_info_f,
                           (MPI_Fint *comm, MPI_Fint *info, MPI_Fint *ierr),
                           (comm, info, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SET_INFO = ompi_comm_set_info_f
#pragma weak mpi_comm_set_info = ompi_comm_set_info_f
#pragma weak mpi_comm_set_info_ = ompi_comm_set_info_f
#pragma weak mpi_comm_set_info__ = ompi_comm_set_info_f

#pragma weak MPI_Comm_set_info_f = ompi_comm_set_info_f
#pragma weak MPI_Comm_set_info_f08 = ompi_comm_set_info_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SET_INFO,
                           mpi_comm_set_info,
                           mpi_comm_set_info_,
                           mpi_comm_set_info__,
                           ompi_comm_set_info_f,
                           (MPI_Fint *comm, MPI_Fint *info, MPI_Fint *ierr),
                           (comm, info, ierr) )
#else
#define ompi_comm_set_info_f pompi_comm_set_info_f
#endif
#endif


void ompi_comm_set_info_f(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Info c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPI_Comm_set_info(c_comm, c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
