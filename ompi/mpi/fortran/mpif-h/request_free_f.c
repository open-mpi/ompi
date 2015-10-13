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
#pragma weak PMPI_REQUEST_FREE = ompi_request_free_f
#pragma weak pmpi_request_free = ompi_request_free_f
#pragma weak pmpi_request_free_ = ompi_request_free_f
#pragma weak pmpi_request_free__ = ompi_request_free_f

#pragma weak PMPI_Request_free_f = ompi_request_free_f
#pragma weak PMPI_Request_free_f08 = ompi_request_free_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_REQUEST_FREE,
                           pmpi_request_free,
                           pmpi_request_free_,
                           pmpi_request_free__,
                           pompi_request_free_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REQUEST_FREE = ompi_request_free_f
#pragma weak mpi_request_free = ompi_request_free_f
#pragma weak mpi_request_free_ = ompi_request_free_f
#pragma weak mpi_request_free__ = ompi_request_free_f

#pragma weak MPI_Request_free_f = ompi_request_free_f
#pragma weak MPI_Request_free_f08 = ompi_request_free_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_REQUEST_FREE,
                           mpi_request_free,
                           mpi_request_free_,
                           mpi_request_free__,
                           ompi_request_free_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#else
#define ompi_request_free_f pompi_request_free_f
#endif
#endif


void ompi_request_free_f(MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;

    MPI_Request c_req = PMPI_Request_f2c( *request );
    c_ierr = PMPI_Request_free(&c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = OMPI_INT_2_FINT(MPI_REQUEST_NULL->req_f_to_c_index);
    }
}
