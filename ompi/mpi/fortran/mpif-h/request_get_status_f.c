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
#pragma weak PMPI_REQUEST_GET_STATUS = ompi_request_get_status_f
#pragma weak pmpi_request_get_status = ompi_request_get_status_f
#pragma weak pmpi_request_get_status_ = ompi_request_get_status_f
#pragma weak pmpi_request_get_status__ = ompi_request_get_status_f

#pragma weak PMPI_Request_get_status_f = ompi_request_get_status_f
#pragma weak PMPI_Request_get_status_f08 = ompi_request_get_status_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_REQUEST_GET_STATUS,
                           pmpi_request_get_status,
                           pmpi_request_get_status_,
                           pmpi_request_get_status__,
                           pompi_request_get_status_f,
                           (MPI_Fint *request, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REQUEST_GET_STATUS = ompi_request_get_status_f
#pragma weak mpi_request_get_status = ompi_request_get_status_f
#pragma weak mpi_request_get_status_ = ompi_request_get_status_f
#pragma weak mpi_request_get_status__ = ompi_request_get_status_f

#pragma weak MPI_Request_get_status_f = ompi_request_get_status_f
#pragma weak MPI_Request_get_status_f08 = ompi_request_get_status_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_REQUEST_GET_STATUS,
                           mpi_request_get_status,
                           mpi_request_get_status_,
                           mpi_request_get_status__,
                           ompi_request_get_status_f,
                           (MPI_Fint *request, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#else
#define ompi_request_get_status_f pompi_request_get_status_f
#endif
#endif


void ompi_request_get_status_f(MPI_Fint *request, ompi_fortran_logical_t *flag,
                              MPI_Fint *status, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Status c_status;
    MPI_Request c_req = PMPI_Request_f2c( *request );
    OMPI_LOGICAL_NAME_DECL(flag);

    /* This seems silly, but someone will do it */

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        *flag = OMPI_INT_2_LOGICAL(0);
        c_ierr = MPI_SUCCESS;
    } else {
        c_ierr = PMPI_Request_get_status(c_req,
                                        OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag),
                                        &c_status);
        OMPI_SINGLE_INT_2_LOGICAL(flag);
        PMPI_Status_c2f( &c_status, status );
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
