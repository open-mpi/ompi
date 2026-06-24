/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
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
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_REQUEST_STATUS_GET_ANY = ompi_request_get_status_any_f
#pragma weak pmpi_request_get_status_any = ompi_request_get_status_any_f
#pragma weak pmpi_request_get_status_any_ = ompi_request_get_status_any_f
#pragma weak pmpi_request_get_status_any__ = ompi_request_get_status_any_f

#pragma weak PMPI_Request_get_status_any_f = ompi_request_get_status_any_f
#pragma weak PMPI_Request_get_status_any_f08 = ompi_request_get_status_any_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_REQUEST_STATUS_GET_ANY,
                           pmpi_request_get_status_any,
                           pmpi_request_get_status_any_,
                           pmpi_request_get_status_any__,
                           pompi_request_get_status_any_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *indx, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, indx, flag, status, ierr))
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REQUEST_STATUS_GET_ANY = ompi_request_get_status_any_f
#pragma weak mpi_request_get_status_any = ompi_request_get_status_any_f
#pragma weak mpi_request_get_status_any_ = ompi_request_get_status_any_f
#pragma weak mpi_request_get_status_any__ = ompi_request_get_status_any_f

#pragma weak MPI_Request_get_status_any_f = ompi_request_get_status_any_f
#pragma weak MPI_Request_get_status_any_f08 = ompi_request_get_status_any_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_REQUEST_STATUS_GET_ANY,
                           mpi_request_get_status_any,
                           mpi_request_get_status_any_,
                           mpi_request_get_status_any__,
                           ompi_request_get_status_any_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *indx, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, indx, flag, status, ierr))
#else
#define ompi_request_get_status_any_f pompi_request_get_status_any_f
#endif
#endif


static const char FUNC_NAME[] = "MPI_REQUEST_STATUS_GET_ANY";


void ompi_request_get_status_any_f(MPI_Fint *count, MPI_Fint *array_of_requests,
		   MPI_Fint *indx, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status c_status;
    int i, c_ierr;
    OMPI_LOGICAL_NAME_DECL(flag);
    OMPI_SINGLE_NAME_DECL(indx);

    /* Shortcut to avoid malloc(0) if *count==0.  We're intentionally
       skipping other parameter error checks. */
    if (OPAL_UNLIKELY(0 == OMPI_FINT_2_INT(*count))) {
        *indx = OMPI_INT_2_FINT(MPI_UNDEFINED);
        PMPI_Status_c2f(&ompi_status_empty, status);
        *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
        return;
    }

    c_req = (MPI_Request *) malloc(OMPI_FINT_2_INT(*count) * sizeof(MPI_Request));
    if (NULL == c_req) {
        c_ierr = OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_NO_MEM,
                                        FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) {
        c_req[i] = PMPI_Request_f2c(array_of_requests[i]);
    }

    c_ierr = PMPI_Request_get_status_any(OMPI_FINT_2_INT(*count), c_req,
                         OMPI_SINGLE_NAME_CONVERT(indx), OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag),
                         &c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {

        OMPI_SINGLE_INT_2_LOGICAL(flag);

        if (1 == OMPI_LOGICAL_2_INT(*flag)){
            OMPI_SINGLE_INT_2_FINT(indx);
            ++(*indx); /* Increment indexes by one for fortran conventions */
            if (!OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
                PMPI_Status_c2f(&c_status, status);
            }
        }
    }
    free(c_req);
}
