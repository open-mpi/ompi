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
 * Copyright (c) 2025      Triad National Security, LLC.  All rights reserved.
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
#pragma weak PMPI_COMM_IFLUSH_BUFFER = ompi_comm_iflush_buffer_f
#pragma weak pmpi_comm_iflush_buffer = ompi_comm_iflush_buffer_f
#pragma weak pmpi_comm_iflush_buffer_ = ompi_comm_iflush_buffer_f
#pragma weak pmpi_comm_iflush_buffer__ = ompi_comm_iflush_buffer_f

#pragma weak PMPI_Comm_iflush_buffer_f = ompi_comm_iflush_buffer_f
#pragma weak PMPI_Comm_iflush_buffer_f08 = ompi_comm_iflush_buffer_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_IFLUSH_BUFFER,
                            pmpi_comm_iflush_buffer,
                            pmpi_comm_iflush_buffer_,
                            pmpi_comm_iflush_buffer__,
                            pompi_comm_iflush_buffer_f,
                            (MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (comm, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_IFLUSH_BUFFER = ompi_comm_iflush_buffer_f
#pragma weak mpi_comm_iflush_buffer = ompi_comm_iflush_buffer_f
#pragma weak mpi_comm_iflush_buffer_ = ompi_comm_iflush_buffer_f
#pragma weak mpi_comm_iflush_buffer__ = ompi_comm_iflush_buffer_f

#pragma weak MPI_Comm_iflush_buffer_f = ompi_comm_iflush_buffer_f
#pragma weak MPI_Comm_iflush_buffer_f08 = ompi_comm_iflush_buffer_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_IFLUSH_BUFFER,
                            mpi_comm_iflush_buffer,
                            mpi_comm_iflush_buffer_,
                            mpi_comm_iflush_buffer__,
                            ompi_comm_iflush_buffer_f,
                            (MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (comm, request, ierr) )
#else
#define ompi_comm_iflush_buffer_f pompi_comm_iflush_buffer_f
#endif
#endif


void ompi_comm_iflush_buffer_f(MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Comm c_comm;
    MPI_Request c_req;

    c_comm = PMPI_Comm_f2c(*comm);

    ierr_c = PMPI_Comm_iflush_buffer(c_comm, &c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);

    if (MPI_SUCCESS == ierr_c) *request = PMPI_Request_c2f(c_req);
}
