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
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/errhandler/errhandler.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_WAITSOME = ompi_waitsome_f
#pragma weak pmpi_waitsome = ompi_waitsome_f
#pragma weak pmpi_waitsome_ = ompi_waitsome_f
#pragma weak pmpi_waitsome__ = ompi_waitsome_f

#pragma weak PMPI_Waitsome_f = ompi_waitsome_f
#pragma weak PMPI_Waitsome_f08 = ompi_waitsome_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_WAITSOME,
                           pmpi_waitsome,
                           pmpi_waitsome_,
                           pmpi_waitsome__,
                           pompi_waitsome_f,
                           (MPI_Fint *incount, MPI_Fint *array_of_requests, MPI_Fint *outcount, MPI_Fint *array_of_indices, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WAITSOME = ompi_waitsome_f
#pragma weak mpi_waitsome = ompi_waitsome_f
#pragma weak mpi_waitsome_ = ompi_waitsome_f
#pragma weak mpi_waitsome__ = ompi_waitsome_f

#pragma weak MPI_Waitsome_f = ompi_waitsome_f
#pragma weak MPI_Waitsome_f08 = ompi_waitsome_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_WAITSOME,
                           mpi_waitsome,
                           mpi_waitsome_,
                           mpi_waitsome__,
                           ompi_waitsome_f,
                           (MPI_Fint *incount, MPI_Fint *array_of_requests, MPI_Fint *outcount, MPI_Fint *array_of_indices, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierr) )
#else
#define ompi_waitsome_f pompi_waitsome_f
#endif
#endif


static const char FUNC_NAME[] = "MPI_WAITSOME";


void ompi_waitsome_f(MPI_Fint *incount, MPI_Fint *array_of_requests,
                    MPI_Fint *outcount, MPI_Fint *array_of_indices,
                    MPI_Fint *array_of_statuses, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Request *c_req;
    MPI_Status  *c_status;
    int i;
    OMPI_SINGLE_NAME_DECL(outcount);
    OMPI_ARRAY_NAME_DECL(array_of_indices);

    /* Shortcut to avoid malloc(0) if *count==0.  We're intentionally
       skipping other parameter error checks. */
    if (OPAL_UNLIKELY(0 == OMPI_FINT_2_INT(*incount))) {
        *outcount = OMPI_INT_2_FINT(MPI_UNDEFINED);
        *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
        return;
    }

    c_req = (MPI_Request *) malloc(OMPI_FINT_2_INT(*incount) *
                   (sizeof(MPI_Request) + sizeof(MPI_Status)));
    if (NULL == c_req) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD,
                                        MPI_ERR_NO_MEM,
                                        FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }
    c_status = (MPI_Status*) (c_req + OMPI_FINT_2_INT(*incount));

    for (i = 0; i < OMPI_FINT_2_INT(*incount); ++i) {
        c_req[i] = PMPI_Request_f2c(array_of_requests[i]);
    }

    OMPI_ARRAY_FINT_2_INT_ALLOC(array_of_indices, *incount);
    c_ierr = PMPI_Waitsome(OMPI_FINT_2_INT(*incount), c_req,
                          OMPI_SINGLE_NAME_CONVERT(outcount),
                          OMPI_ARRAY_NAME_CONVERT(array_of_indices),
                          c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(outcount);
        OMPI_ARRAY_INT_2_FINT(array_of_indices, *incount);

        /* Increment indexes by one for fortran conventions */

        if (MPI_UNDEFINED != OMPI_FINT_2_INT(*outcount)) {
            for (i = 0; i < OMPI_FINT_2_INT(*outcount); ++i) {
                array_of_requests[OMPI_INT_2_FINT(array_of_indices[i])] =
                    c_req[OMPI_INT_2_FINT(array_of_indices[i])]->req_f_to_c_index;
                ++array_of_indices[i];
            }
        }
        if (!OMPI_IS_FORTRAN_STATUSES_IGNORE(array_of_statuses)) {
            for (i = 0; i < OMPI_FINT_2_INT(*incount); ++i) {
                if (!OMPI_IS_FORTRAN_STATUS_IGNORE(&array_of_statuses[i])) {
                    PMPI_Status_c2f(&c_status[i], &array_of_statuses[i * (sizeof(MPI_Status) / sizeof(int))]);
                }
            }
        }
    }
    free(c_req);
}
