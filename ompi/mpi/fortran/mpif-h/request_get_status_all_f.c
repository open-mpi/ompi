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
#pragma weak PMPI_REQUEST_GET_STATUS_ALL = ompi_request_get_status_all_f
#pragma weak pmpi_request_get_status_all = ompi_request_get_status_all_f
#pragma weak pmpi_request_get_status_all_ = ompi_request_get_status_all_f
#pragma weak pmpi_request_get_status_all__ = ompi_request_get_status_all_f

#pragma weak PMPI_Request_get_status_all_f = ompi_request_get_status_all_f
#pragma weak PMPI_Request_get_status_all_f08 = ompi_request_get_status_all_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_REQUEST_GET_STATUS_ALL,
                           pmpi_request_get_status_all,
                           pmpi_request_get_status_all_,
                           pmpi_request_get_status_all__,
                           pompi_request_get_status_all_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, ompi_fortran_logical_t *flag, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, flag, array_of_statuses, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REQUEST_GET_STATUS_ALL = ompi_request_get_status_all_f
#pragma weak mpi_request_get_status_all = ompi_request_get_status_all_f
#pragma weak mpi_request_get_status_all_ = ompi_request_get_status_all_f
#pragma weak mpi_request_get_status_all__ = ompi_request_get_status_all_f

#pragma weak MPI_Request_get_status_all_f = ompi_request_get_status_all_f
#pragma weak MPI_Request_get_status_all_f08 = ompi_request_get_status_all_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_REQUEST_GET_STATUS_ALL,
                           mpi_request_get_status_all,
                           mpi_request_get_status_all_,
                           mpi_request_get_status_all__,
                           ompi_request_get_status_all_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, ompi_fortran_logical_t *flag, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, flag, array_of_statuses, ierr) )
#else
#define ompi_request_get_status_all_f pompi_request_get_status_all_f
#endif
#endif


static const char FUNC_NAME[] = "MPI_REQUEST_GET_STATUS_ALL";


void ompi_request_get_status_all_f(MPI_Fint *count, MPI_Fint *array_of_requests,
 		                   ompi_fortran_logical_t *flag, MPI_Fint *array_of_statuses, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status *c_status;
    int i, c_ierr;
    OMPI_LOGICAL_NAME_DECL(flag);

    /* Shortcut to avoid malloc(0) if *count==0.  We're intentionally
       skipping other parameter error checks. */
    if (OPAL_UNLIKELY(0 == OMPI_FINT_2_INT(*count))) {
        *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
        *flag = OMPI_FORTRAN_VALUE_TRUE;
        return;
    }

    c_req = (MPI_Request *) malloc(OMPI_FINT_2_INT(*count) *
                   (sizeof(MPI_Request) + sizeof(MPI_Status)));
    if (NULL == c_req) {
        c_ierr = OMPI_ERRHANDLER_NOHANDLE_INVOKE(
                                        MPI_ERR_NO_MEM,
                                        FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }
    c_status = (MPI_Status*) (c_req + OMPI_FINT_2_INT(*count));

    for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) {
        c_req[i] = PMPI_Request_f2c(array_of_requests[i]);
    }

    c_ierr = PMPI_Request_get_status_all(OMPI_FINT_2_INT(*count), c_req, OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag), c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_LOGICAL(flag);
        if (1 == OMPI_LOGICAL_2_INT(*flag)){
            for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) {
                if (!OMPI_IS_FORTRAN_STATUSES_IGNORE(array_of_statuses) &&
                    !OMPI_IS_FORTRAN_STATUS_IGNORE(&array_of_statuses[i])) {
                    PMPI_Status_c2f( &c_status[i], &array_of_statuses[i * (sizeof(MPI_Status) / sizeof(int))]);
                }
            }
        }
    }
    free(c_req);
}
