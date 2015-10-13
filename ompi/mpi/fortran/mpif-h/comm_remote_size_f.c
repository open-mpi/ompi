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
#pragma weak PMPI_COMM_REMOTE_SIZE = ompi_comm_remote_size_f
#pragma weak pmpi_comm_remote_size = ompi_comm_remote_size_f
#pragma weak pmpi_comm_remote_size_ = ompi_comm_remote_size_f
#pragma weak pmpi_comm_remote_size__ = ompi_comm_remote_size_f

#pragma weak PMPI_Comm_remote_size_f = ompi_comm_remote_size_f
#pragma weak PMPI_Comm_remote_size_f08 = ompi_comm_remote_size_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_REMOTE_SIZE,
                           pmpi_comm_remote_size,
                           pmpi_comm_remote_size_,
                           pmpi_comm_remote_size__,
                           pompi_comm_remote_size_f,
                           (MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr),
                           (comm, size, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_REMOTE_SIZE = ompi_comm_remote_size_f
#pragma weak mpi_comm_remote_size = ompi_comm_remote_size_f
#pragma weak mpi_comm_remote_size_ = ompi_comm_remote_size_f
#pragma weak mpi_comm_remote_size__ = ompi_comm_remote_size_f

#pragma weak MPI_Comm_remote_size_f = ompi_comm_remote_size_f
#pragma weak MPI_Comm_remote_size_f08 = ompi_comm_remote_size_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_REMOTE_SIZE,
                           mpi_comm_remote_size,
                           mpi_comm_remote_size_,
                           mpi_comm_remote_size__,
                           ompi_comm_remote_size_f,
                           (MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr),
                           (comm, size, ierr) )
#else
#define ompi_comm_remote_size_f pompi_comm_remote_size_f
#endif
#endif

void ompi_comm_remote_size_f(MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = PMPI_Comm_f2c ( *comm );
    OMPI_SINGLE_NAME_DECL(size);

    c_ierr = PMPI_Comm_remote_size(c_comm, OMPI_SINGLE_NAME_CONVERT(size ));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(size);
    }
}
