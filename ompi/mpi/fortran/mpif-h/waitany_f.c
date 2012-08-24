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
#pragma weak PMPI_WAITANY = ompi_waitany_f
#pragma weak pmpi_waitany = ompi_waitany_f
#pragma weak pmpi_waitany_ = ompi_waitany_f
#pragma weak pmpi_waitany__ = ompi_waitany_f

#pragma weak PMPI_Waitany_f = ompi_waitany_f
#pragma weak PMPI_Waitany_f08 = ompi_waitany_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WAITANY,
                           pmpi_waitany,
                           pmpi_waitany_,
                           pmpi_waitany__,
                           pompi_waitany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *indx, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, indx, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WAITANY = ompi_waitany_f
#pragma weak mpi_waitany = ompi_waitany_f
#pragma weak mpi_waitany_ = ompi_waitany_f
#pragma weak mpi_waitany__ = ompi_waitany_f

#pragma weak MPI_Waitany_f = ompi_waitany_f
#pragma weak MPI_Waitany_f08 = ompi_waitany_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WAITANY,
                           mpi_waitany,
                           mpi_waitany_,
                           mpi_waitany__,
                           ompi_waitany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *indx, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, indx, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_WAITANY";


void ompi_waitany_f(MPI_Fint *count, MPI_Fint *array_of_requests,
		   MPI_Fint *indx, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status c_status;
    int i, c_ierr;
    OMPI_SINGLE_NAME_DECL(indx);

    /* Shortcut to avoid malloc(0) if *count==0.  We're intentionally
       skipping other parameter error checks. */
    if (OPAL_UNLIKELY(0 == OMPI_FINT_2_INT(*count))) {
        *indx = OMPI_INT_2_FINT(MPI_UNDEFINED);
        MPI_Status_c2f(&ompi_status_empty, status); 
        *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
        return;
    }

    c_req = (MPI_Request *) malloc(OMPI_FINT_2_INT(*count) * sizeof(MPI_Request));
    if (NULL == c_req) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                        FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) {
        c_req[i] = MPI_Request_f2c(array_of_requests[i]);
    }

    c_ierr = MPI_Waitany(OMPI_FINT_2_INT(*count), c_req, 
                         OMPI_SINGLE_NAME_CONVERT(indx),
                         &c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {

        /* Increment index by one for fortran conventions */

        OMPI_SINGLE_INT_2_FINT(indx);
        if (MPI_UNDEFINED != *(OMPI_SINGLE_NAME_CONVERT(indx))) {
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
