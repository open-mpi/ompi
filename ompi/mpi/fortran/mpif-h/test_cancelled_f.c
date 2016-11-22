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
#pragma weak PMPI_TEST_CANCELLED = ompi_test_cancelled_f
#pragma weak pmpi_test_cancelled = ompi_test_cancelled_f
#pragma weak pmpi_test_cancelled_ = ompi_test_cancelled_f
#pragma weak pmpi_test_cancelled__ = ompi_test_cancelled_f

#pragma weak PMPI_Test_cancelled_f = ompi_test_cancelled_f
#pragma weak PMPI_Test_cancelled_f08 = ompi_test_cancelled_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TEST_CANCELLED,
                           pmpi_test_cancelled,
                           pmpi_test_cancelled_,
                           pmpi_test_cancelled__,
                           pompi_test_cancelled_f,
                           (MPI_Fint *status, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TEST_CANCELLED = ompi_test_cancelled_f
#pragma weak mpi_test_cancelled = ompi_test_cancelled_f
#pragma weak mpi_test_cancelled_ = ompi_test_cancelled_f
#pragma weak mpi_test_cancelled__ = ompi_test_cancelled_f

#pragma weak MPI_Test_cancelled_f = ompi_test_cancelled_f
#pragma weak MPI_Test_cancelled_f08 = ompi_test_cancelled_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TEST_CANCELLED,
                           mpi_test_cancelled,
                           mpi_test_cancelled_,
                           mpi_test_cancelled__,
                           ompi_test_cancelled_f,
                           (MPI_Fint *status, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#else
#define ompi_test_cancelled_f pompi_test_cancelled_f
#endif
#endif


void ompi_test_cancelled_f(MPI_Fint *status, ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Status c_status;
    OMPI_LOGICAL_NAME_DECL(flag);

    /* This seems silly, but someone will do it */

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        *flag = OMPI_INT_2_LOGICAL(0);
        c_ierr = MPI_SUCCESS;
    } else {
        c_ierr = PMPI_Status_f2c( status, &c_status );

        if (MPI_SUCCESS == c_ierr) {
            c_ierr = PMPI_Test_cancelled(&c_status,
                                        OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));

            OMPI_SINGLE_INT_2_LOGICAL(flag);
        }
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
