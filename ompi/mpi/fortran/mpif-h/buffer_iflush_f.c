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
#pragma weak PMPI_BUFFER_IFLUSH = ompi_buffer_iflush_f
#pragma weak pmpi_buffer_iflush = ompi_buffer_iflush_f
#pragma weak pmpi_buffer_iflush_ = ompi_buffer_iflush_f
#pragma weak pmpi_buffer_iflush__ = ompi_buffer_iflush_f

#pragma weak PMPI_buffer_iflush_f = ompi_buffer_iflush_f
#pragma weak PMPI_buffer_iflush_f08 = ompi_buffer_iflush_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_BUFFER_IFLUSH,
                            pmpi_buffer_iflush,
                            pmpi_buffer_iflush_,
                            pmpi_buffer_iflush__,
                            pompi_buffer_iflush_f,
                            (MPI_Fint *request, MPI_Fint *ierr),
                            (request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BUFFER_IFLUSH = ompi_buffer_iflush_f
#pragma weak mpi_buffer_iflush = ompi_buffer_iflush_f
#pragma weak mpi_buffer_iflush_ = ompi_buffer_iflush_f
#pragma weak mpi_buffer_iflush__ = ompi_buffer_iflush_f

#pragma weak MPI_buffer_iflush_f = ompi_buffer_iflush_f
#pragma weak MPI_buffer_iflush_f08 = ompi_buffer_iflush_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_BUFFER_IFLUSH,
                            mpi_buffer_iflush,
                            mpi_buffer_iflush_,
                            mpi_buffer_iflush__,
                            ompi_buffer_iflush_f,
                            (MPI_Fint *request, MPI_Fint *ierr),
                            (request, ierr) )
#else
#define ompi_buffer_iflush_f pompi_buffer_iflush_f
#endif
#endif


void ompi_buffer_iflush_f(MPI_Fint *request, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Request c_req;

    ierr_c = PMPI_buffer_iflush(&c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);

    if (MPI_SUCCESS == ierr_c) *request = PMPI_Request_c2f(c_req);
}
