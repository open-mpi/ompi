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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TESTSOME = ompi_testsome_f
#pragma weak pmpi_testsome = ompi_testsome_f
#pragma weak pmpi_testsome_ = ompi_testsome_f
#pragma weak pmpi_testsome__ = ompi_testsome_f

#pragma weak PMPI_Testsome_f = ompi_testsome_f
#pragma weak PMPI_Testsome_f08 = ompi_testsome_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TESTSOME,
                           pmpi_testsome,
                           pmpi_testsome_,
                           pmpi_testsome__,
                           pompi_testsome_f,
                           (MPI_Fint *incount, MPI_Fint *array_of_requests, MPI_Fint *outcount, MPI_Fint *array_of_indices, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TESTSOME = ompi_testsome_f
#pragma weak mpi_testsome = ompi_testsome_f
#pragma weak mpi_testsome_ = ompi_testsome_f
#pragma weak mpi_testsome__ = ompi_testsome_f

#pragma weak MPI_Testsome_f = ompi_testsome_f
#pragma weak MPI_Testsome_f08 = ompi_testsome_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TESTSOME,
                           mpi_testsome,
                           mpi_testsome_,
                           mpi_testsome__,
                           ompi_testsome_f,
                           (MPI_Fint *incount, MPI_Fint *array_of_requests, MPI_Fint *outcount, MPI_Fint *array_of_indices, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_TESTSOME";


void ompi_testsome_f(MPI_Fint *incount, MPI_Fint *array_of_requests, 
		    MPI_Fint *outcount, MPI_Fint *array_of_indices,
		    MPI_Fint *array_of_statuses, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status *c_status;
    int i, c_ierr;
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
        c_req[i] = MPI_Request_f2c(array_of_requests[i]);
    }

    OMPI_ARRAY_FINT_2_INT_ALLOC(array_of_indices, OMPI_FINT_2_INT(*incount));
    c_ierr = MPI_Testsome(OMPI_FINT_2_INT(*incount), c_req, 
                          OMPI_SINGLE_NAME_CONVERT(outcount), 
                          OMPI_ARRAY_NAME_CONVERT(array_of_indices), 
                          c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_SINGLE_INT_2_FINT(outcount);
    OMPI_ARRAY_INT_2_FINT(array_of_indices, *incount);

    if (MPI_SUCCESS == c_ierr) {
        if (MPI_UNDEFINED != OMPI_FINT_2_INT(*outcount)) {
            for (i = 0; i < OMPI_FINT_2_INT(*outcount); ++i) {
                array_of_requests[OMPI_INT_2_FINT(array_of_indices[i])] =
                    c_req[OMPI_INT_2_FINT(array_of_indices[i])]->req_f_to_c_index;
                ++array_of_indices[i];
            }
        }
        if (!OMPI_IS_FORTRAN_STATUSES_IGNORE(array_of_statuses)) {
            for (i = 0; i < OMPI_FINT_2_INT(*outcount); ++i) {
                if (!OMPI_IS_FORTRAN_STATUS_IGNORE(&array_of_statuses[i])) {
                    MPI_Status_c2f(&c_status[i], &array_of_statuses[i * (sizeof(MPI_Status) / sizeof(int))]);
                }
            }
        }
    }
    free(c_req);
}
