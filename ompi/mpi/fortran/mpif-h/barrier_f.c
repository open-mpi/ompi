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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_BARRIER = ompi_barrier_f
#pragma weak pmpi_barrier = ompi_barrier_f
#pragma weak pmpi_barrier_ = ompi_barrier_f
#pragma weak pmpi_barrier__ = ompi_barrier_f

#pragma weak PMPI_Barrier_f = ompi_barrier_f
#pragma weak PMPI_Barrier_f08 = ompi_barrier_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_BARRIER,
                           pmpi_barrier,
                           pmpi_barrier_,
                           pmpi_barrier__,
                           pompi_barrier_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BARRIER = ompi_barrier_f
#pragma weak mpi_barrier = ompi_barrier_f
#pragma weak mpi_barrier_ = ompi_barrier_f
#pragma weak mpi_barrier__ = ompi_barrier_f

#pragma weak MPI_Barrier_f = ompi_barrier_f
#pragma weak MPI_Barrier_f08 = ompi_barrier_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_BARRIER,
                           mpi_barrier,
                           mpi_barrier_,
                           mpi_barrier__,
                           ompi_barrier_f,
                           (MPI_Fint *comm, MPI_Fint *ierr),
                           (comm, ierr) )
#else
#define ompi_barrier_f pompi_barrier_f
#endif
#endif


void ompi_barrier_f(MPI_Fint *comm, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Comm c_comm;

    c_comm = PMPI_Comm_f2c(*comm);

    ierr_c = PMPI_Barrier(c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);
}
