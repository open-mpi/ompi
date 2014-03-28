/*
 * Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_GET_INFO = ompi_comm_get_info_f
#pragma weak pmpi_comm_get_info = ompi_comm_get_info_f
#pragma weak pmpi_comm_get_info_ = ompi_comm_get_info_f
#pragma weak pmpi_comm_get_info__ = ompi_comm_get_info_f

#pragma weak PMPI_Comm_get_info_f = ompi_comm_get_info_f
#pragma weak PMPI_Comm_get_info_f08 = ompi_comm_get_info_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_GET_INFO,
                           pmpi_comm_get_info,
                           pmpi_comm_get_info_,
                           pmpi_comm_get_info__,
                           pompi_comm_get_info_f,
                           (MPI_Fint *comm, MPI_Fint *info_used, MPI_Fint *ierr),
                           (comm, info_used, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_GET_INFO = ompi_comm_get_info_f
#pragma weak mpi_comm_get_info = ompi_comm_get_info_f
#pragma weak mpi_comm_get_info_ = ompi_comm_get_info_f
#pragma weak mpi_comm_get_info__ = ompi_comm_get_info_f

#pragma weak MPI_Comm_get_info_f = ompi_comm_get_info_f
#pragma weak MPI_Comm_get_info_f08 = ompi_comm_get_info_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_GET_INFO,
                           mpi_comm_get_info,
                           mpi_comm_get_info_,
                           mpi_comm_get_info__,
                           ompi_comm_get_info_f,
                           (MPI_Fint *comm, MPI_Fint *info_used, MPI_Fint *ierr),
                           (comm, info_used, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_comm_get_info_f(MPI_Fint *comm, MPI_Fint *info_used, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = MPI_Comm_f2c(*comm);
    MPI_Info c_info;

    c_ierr = MPI_Comm_get_info(c_comm, &c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *info_used = MPI_Info_c2f(c_info);
    }
}
