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
#pragma weak PMPI_TESTANY = ompi_testany_f
#pragma weak pmpi_testany = ompi_testany_f
#pragma weak pmpi_testany_ = ompi_testany_f
#pragma weak pmpi_testany__ = ompi_testany_f

#pragma weak PMPI_Testany_f = ompi_testany_f
#pragma weak PMPI_Testany_f08 = ompi_testany_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TESTANY,
                           pmpi_testany,
                           pmpi_testany_,
                           pmpi_testany__,
                           pompi_testany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *indx, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, indx, flag, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TESTANY = ompi_testany_f
#pragma weak mpi_testany = ompi_testany_f
#pragma weak mpi_testany_ = ompi_testany_f
#pragma weak mpi_testany__ = ompi_testany_f

#pragma weak MPI_Testany_f = ompi_testany_f
#pragma weak MPI_Testany_f08 = ompi_testany_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TESTANY,
                           mpi_testany,
                           mpi_testany_,
                           mpi_testany__,
                           ompi_testany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *indx, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, indx, flag, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_TESTANY";


void ompi_testany_f(MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *indx, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status c_status;
    int i, c_ierr;
    OMPI_LOGICAL_NAME_DECL(flag);
    OMPI_SINGLE_NAME_DECL(indx);

    /* Shortcut to avoid malloc(0) if *count==0.  We're intentionally
       skipping other parameter error checks. */
    if (OPAL_UNLIKELY(0 == OMPI_FINT_2_INT(*count))) {
        *flag = OMPI_FORTRAN_VALUE_TRUE;
        *indx = OMPI_INT_2_FINT(MPI_UNDEFINED);
        MPI_Status_c2f(&ompi_status_empty, status); 
        *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
        return;
    }

    c_req = (MPI_Request *) malloc(OMPI_FINT_2_INT(*count) * sizeof(MPI_Request));
    if (c_req == NULL) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, 
                                        MPI_ERR_NO_MEM,
                                        FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) {
        c_req[i] = MPI_Request_f2c(array_of_requests[i]);
    }

    c_ierr = MPI_Testany(OMPI_FINT_2_INT(*count), c_req,
                         OMPI_SINGLE_NAME_CONVERT(indx),
                         OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag),
                         &c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_SINGLE_INT_2_LOGICAL(flag);
    if (MPI_SUCCESS == c_ierr) {

        /* Increment index by one for fortran conventions.  Note that
           all Fortran compilers have FALSE==0; we just need to check
           for any nonzero value (because TRUE is not always 1) */

        OMPI_SINGLE_INT_2_FINT(indx);
        if (*flag &&
            MPI_UNDEFINED != *(OMPI_SINGLE_NAME_CONVERT(indx))) {
            array_of_requests[OMPI_INT_2_FINT(*indx)] =
                c_req[OMPI_INT_2_FINT(*indx)]->req_f_to_c_index;
            ++(*indx);
        }
        if (!OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
            MPI_Status_c2f(&c_status, status); 
        }
    }
    free(c_req);
}
