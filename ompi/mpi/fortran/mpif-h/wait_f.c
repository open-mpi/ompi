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
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_WAIT = ompi_wait_f
#pragma weak pmpi_wait = ompi_wait_f
#pragma weak pmpi_wait_ = ompi_wait_f
#pragma weak pmpi_wait__ = ompi_wait_f

#pragma weak PMPI_Wait_f = ompi_wait_f
#pragma weak PMPI_Wait_f08 = ompi_wait_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_WAIT,
                           pmpi_wait,
                           pmpi_wait_,
                           pmpi_wait__,
                           pompi_wait_f,
                           (MPI_Fint *request, MPI_Fint *status, MPI_Fint *ierr),
                           (request, status, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WAIT = ompi_wait_f
#pragma weak mpi_wait = ompi_wait_f
#pragma weak mpi_wait_ = ompi_wait_f
#pragma weak mpi_wait__ = ompi_wait_f

#pragma weak MPI_Wait_f = ompi_wait_f
#pragma weak MPI_Wait_f08 = ompi_wait_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_WAIT,
                           mpi_wait,
                           mpi_wait_,
                           mpi_wait__,
                           ompi_wait_f,
                           (MPI_Fint *request, MPI_Fint *status, MPI_Fint *ierr),
                           (request, status, ierr) )
#else
#define ompi_wait_f pompi_wait_f
#endif
#endif


void ompi_wait_f(MPI_Fint *request, MPI_Fint *status, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Request c_req = PMPI_Request_f2c(*request);
    MPI_Status  c_status;

    c_ierr = PMPI_Wait(&c_req, &c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = OMPI_INT_2_FINT(c_req->req_f_to_c_index);
        if (!OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
            PMPI_Status_c2f(&c_status, status);
        }
    }
}
