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
#pragma weak PMPI_STATUS_SET_CANCELLED = ompi_status_set_cancelled_f
#pragma weak pmpi_status_set_cancelled = ompi_status_set_cancelled_f
#pragma weak pmpi_status_set_cancelled_ = ompi_status_set_cancelled_f
#pragma weak pmpi_status_set_cancelled__ = ompi_status_set_cancelled_f

#pragma weak PMPI_Status_set_cancelled_f = ompi_status_set_cancelled_f
#pragma weak PMPI_Status_set_cancelled_f08 = ompi_status_set_cancelled_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_STATUS_SET_CANCELLED,
                           pmpi_status_set_cancelled,
                           pmpi_status_set_cancelled_,
                           pmpi_status_set_cancelled__,
                           pompi_status_set_cancelled_f,
                           (MPI_Fint *status, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_STATUS_SET_CANCELLED = ompi_status_set_cancelled_f
#pragma weak mpi_status_set_cancelled = ompi_status_set_cancelled_f
#pragma weak mpi_status_set_cancelled_ = ompi_status_set_cancelled_f
#pragma weak mpi_status_set_cancelled__ = ompi_status_set_cancelled_f

#pragma weak MPI_Status_set_cancelled_f = ompi_status_set_cancelled_f
#pragma weak MPI_Status_set_cancelled_f08 = ompi_status_set_cancelled_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_STATUS_SET_CANCELLED,
                           mpi_status_set_cancelled,
                           mpi_status_set_cancelled_,
                           mpi_status_set_cancelled__,
                           ompi_status_set_cancelled_f,
                           (MPI_Fint *status, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#else
#define ompi_status_set_cancelled_f pompi_status_set_cancelled_f
#endif
#endif


void ompi_status_set_cancelled_f(MPI_Fint *status, ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Status c_status;

    /* This seems silly, but someone will do it */

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        c_ierr = MPI_SUCCESS;
    } else {
        PMPI_Status_f2c( status, &c_status );

        c_ierr = PMPI_Status_set_cancelled(&c_status,
                                          OMPI_LOGICAL_2_INT(*flag));
        if (MPI_SUCCESS == c_ierr) {
            PMPI_Status_c2f(&c_status, status);
        }
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
