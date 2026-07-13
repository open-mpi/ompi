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
 * Copyright (c) 2026      Triad National Security, LLC.  All rights reserved.
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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak PMPI_STATUS_SET_ERROR = ompi_status_set_error_f
#pragma weak pmpi_status_set_error = ompi_status_set_error_f
#pragma weak pmpi_status_set_error_ = ompi_status_set_error_f
#pragma weak pmpi_status_set_error__ = ompi_status_set_error_f

#pragma weak PMPI_Status_set_error_f = ompi_status_set_error_f
#pragma weak PMPI_Status_set_error_f08 = ompi_status_set_error_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_STATUS_SET_ERROR,
                           pmpi_status_set_error,
                           pmpi_status_set_error_,
                           pmpi_status_set_error__,
                           ompi_status_set_error_f,
                           (MPI_Fint *status, MPI_Fint *err, MPI_Fint *ierr),
                           (status, err, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_STATUS_SET_ERROR = ompi_status_set_error_f
#pragma weak mpi_status_set_error = ompi_status_set_error_f
#pragma weak mpi_status_set_error_ = ompi_status_set_error_f
#pragma weak mpi_status_set_error__ = ompi_status_set_error_f

#pragma weak MPI_Status_set_error_f = ompi_status_set_error_f
#pragma weak MPI_Status_set_error_f08 = ompi_status_set_error_f
#else
OMPI_GENERATE_WEAK_F77_BINDINGS (MPI_STATUS_SET_ERROR,
                           mpi_status_set_error,
                           mpi_status_set_error_,
                           mpi_status_set_error__,
                           ompi_status_set_error_f,
                           (MPI_Fint *status, MPI_Fint *err, MPI_Fint *ierr),
                           (status, err, ierr) )
#endif


void ompi_status_set_error_f(MPI_Fint *status, MPI_Fint *err, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Status c_status;

    /* This seems silly, but someone will do it */

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        c_ierr = MPI_SUCCESS;
    } else {
        PMPI_Status_f2c( status, &c_status );

        c_ierr = PMPI_Status_set_error(&c_status, OMPI_FINT_2_INT(*err));

        if (MPI_SUCCESS == c_ierr) {
            PMPI_Status_c2f(&c_status, status);
        }
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
