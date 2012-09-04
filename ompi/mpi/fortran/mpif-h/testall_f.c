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
#pragma weak PMPI_TESTALL = ompi_testall_f
#pragma weak pmpi_testall = ompi_testall_f
#pragma weak pmpi_testall_ = ompi_testall_f
#pragma weak pmpi_testall__ = ompi_testall_f

#pragma weak PMPI_Testall_f = ompi_testall_f
#pragma weak PMPI_Testall_f08 = ompi_testall_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TESTALL,
                           pmpi_testall,
                           pmpi_testall_,
                           pmpi_testall__,
                           pompi_testall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, ompi_fortran_logical_t *flag, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, flag, array_of_statuses, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TESTALL = ompi_testall_f
#pragma weak mpi_testall = ompi_testall_f
#pragma weak mpi_testall_ = ompi_testall_f
#pragma weak mpi_testall__ = ompi_testall_f

#pragma weak MPI_Testall_f = ompi_testall_f
#pragma weak MPI_Testall_f08 = ompi_testall_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TESTALL,
                           mpi_testall,
                           mpi_testall_,
                           mpi_testall__,
                           ompi_testall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, ompi_fortran_logical_t *flag, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, flag, array_of_statuses, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_TESTALL";

void ompi_testall_f(MPI_Fint *count, MPI_Fint *array_of_requests, ompi_fortran_logical_t *flag, MPI_Fint *array_of_statuses, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status *c_status;
    int i, c_ierr;
    OMPI_LOGICAL_NAME_DECL(flag);

    /* Shortcut to avoid malloc(0) if *count==0.  We're intentionally
       skipping other parameter error checks. */
    if (OPAL_UNLIKELY(0 == OMPI_FINT_2_INT(*count))) {
        *flag = OMPI_FORTRAN_VALUE_TRUE;
        *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
        return;
    }

    c_req = (MPI_Request *) malloc(OMPI_FINT_2_INT(*count) *
                   (sizeof(MPI_Request) + sizeof(MPI_Status)));
    if (NULL == c_req){
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD,
                                        MPI_ERR_NO_MEM,
                                        FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }
    c_status = (MPI_Status*) (c_req + OMPI_FINT_2_INT(*count));
    for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) {
        c_req[i] = MPI_Request_f2c(array_of_requests[i]);
    }

    c_ierr = MPI_Testall(OMPI_FINT_2_INT(*count), c_req,
                         OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag),
                         c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_SINGLE_INT_2_LOGICAL(flag);

    /* All Fortran Compilers have FALSE == 0, so just check for any
       nonzero value (because TRUE is not always == 1) */
    if (MPI_SUCCESS == c_ierr && *flag) {
        for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) {
            array_of_requests[i] = c_req[i]->req_f_to_c_index;
            if (!OMPI_IS_FORTRAN_STATUSES_IGNORE(array_of_statuses) &&
                !OMPI_IS_FORTRAN_STATUS_IGNORE(&array_of_statuses[i])) {
                MPI_Status_c2f(&c_status[i], &array_of_statuses[i * (sizeof(MPI_Status) / sizeof(int))]);
            }
        }
    }

    free(c_req);
}
