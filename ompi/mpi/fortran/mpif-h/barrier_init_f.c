/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2021 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_BARRIER_INIT = ompi_barrier_init_f
#pragma weak pmpi_barrier_init = ompi_barrier_init_f
#pragma weak pmpi_barrier_init_ = ompi_barrier_init_f
#pragma weak pmpi_barrier_init__ = ompi_barrier_init_f

#pragma weak PMPI_Barrier_init_f = ompi_barrier_init_f
#pragma weak PMPI_Barrier_init_f08 = ompi_barrier_init_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_BARRIER_INIT,
                            pmpi_barrier_init,
                            pmpi_barrier_init_,
                            pmpi_barrier_init__,
                            pompi_barrier_init_f,
                            (MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (comm, info, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BARRIER_INIT = ompi_barrier_init_f
#pragma weak mpi_barrier_init = ompi_barrier_init_f
#pragma weak mpi_barrier_init_ = ompi_barrier_init_f
#pragma weak mpi_barrier_init__ = ompi_barrier_init_f

#pragma weak MPI_Barrier_init_f = ompi_barrier_init_f
#pragma weak MPI_Barrier_init_f08 = ompi_barrier_init_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_BARRIER_INIT,
                            mpi_barrier_init,
                            mpi_barrier_init_,
                            mpi_barrier_init__,
                            ompi_barrier_init_f,
                            (MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (comm, info, request, ierr) )
#else
#define ompi_barrier_init_f pompi_barrier_init_f
#endif
#endif


void ompi_barrier_init_f(MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Comm c_comm;
    MPI_Info c_info;
    MPI_Request c_req;

    c_comm = PMPI_Comm_f2c(*comm);
    c_info = PMPI_Info_f2c(*info);

    ierr_c = PMPI_Barrier_init(c_comm, c_info, &c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);

    if (MPI_SUCCESS == ierr_c) *request = PMPI_Request_c2f(c_req);
}
