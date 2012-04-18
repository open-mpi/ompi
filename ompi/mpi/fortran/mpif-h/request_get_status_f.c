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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_REQUEST_GET_STATUS = ompi_request_get_status_f
#pragma weak pmpi_request_get_status = ompi_request_get_status_f
#pragma weak pmpi_request_get_status_ = ompi_request_get_status_f
#pragma weak pmpi_request_get_status__ = ompi_request_get_status_f

#pragma weak PMPI_Request_get_status_f = ompi_request_get_status_f
#pragma weak PMPI_Request_get_status_f08 = ompi_request_get_status_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_REQUEST_GET_STATUS,
                           pmpi_request_get_status,
                           pmpi_request_get_status_,
                           pmpi_request_get_status__,
                           pompi_request_get_status_f,
                           (MPI_Fint *request, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REQUEST_GET_STATUS = ompi_request_get_status_f
#pragma weak mpi_request_get_status = ompi_request_get_status_f
#pragma weak mpi_request_get_status_ = ompi_request_get_status_f
#pragma weak mpi_request_get_status__ = ompi_request_get_status_f

#pragma weak MPI_Request_get_status_f = ompi_request_get_status_f
#pragma weak MPI_Request_get_status_f08 = ompi_request_get_status_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_REQUEST_GET_STATUS,
                           mpi_request_get_status,
                           mpi_request_get_status_,
                           mpi_request_get_status__,
                           ompi_request_get_status_f,
                           (MPI_Fint *request, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_request_get_status_f(MPI_Fint *request, ompi_fortran_logical_t *flag,
                              MPI_Fint *status, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Status c_status;
    MPI_Request c_req = MPI_Request_f2c( *request ); 
    OMPI_LOGICAL_NAME_DECL(flag);

    /* This seems silly, but someone will do it */

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        *flag = OMPI_INT_2_LOGICAL(0);
        c_ierr = MPI_SUCCESS;
    } else {
        c_ierr = MPI_Request_get_status(c_req, 
                                        OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag),
                                        &c_status);
        OMPI_SINGLE_INT_2_LOGICAL(flag);
        MPI_Status_c2f( &c_status, status );
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
