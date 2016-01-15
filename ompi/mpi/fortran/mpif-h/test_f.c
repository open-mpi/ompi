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
#pragma weak PMPI_TEST = ompi_test_f
#pragma weak pmpi_test = ompi_test_f
#pragma weak pmpi_test_ = ompi_test_f
#pragma weak pmpi_test__ = ompi_test_f

#pragma weak PMPI_Test_f = ompi_test_f
#pragma weak PMPI_Test_f08 = ompi_test_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TEST,
                           pmpi_test,
                           pmpi_test_,
                           pmpi_test__,
                           pompi_test_f,
                           (MPI_Fint *request, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TEST = ompi_test_f
#pragma weak mpi_test = ompi_test_f
#pragma weak mpi_test_ = ompi_test_f
#pragma weak mpi_test__ = ompi_test_f

#pragma weak MPI_Test_f = ompi_test_f
#pragma weak MPI_Test_f08 = ompi_test_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TEST,
                           mpi_test,
                           mpi_test_,
                           mpi_test__,
                           ompi_test_f,
                           (MPI_Fint *request, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#else
#define ompi_test_f pompi_test_f
#endif
#endif


void ompi_test_f(MPI_Fint *request, ompi_fortran_logical_t *flag,
                MPI_Fint *status, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Request c_req = PMPI_Request_f2c(*request);
    MPI_Status c_status;
    OMPI_LOGICAL_NAME_DECL(flag);

    c_ierr = PMPI_Test(&c_req,
                      OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag),
                      &c_status);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_SINGLE_INT_2_LOGICAL(flag);

    /* Note that all Fortran compilers have logical FALSE == 0; we
       just need to check for any nonzero value (because TRUE is not
       always 1). */
    if (MPI_SUCCESS == c_ierr && *flag) {
        *request = OMPI_INT_2_FINT(c_req->req_f_to_c_index);
        if (!OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
            PMPI_Status_c2f(&c_status, status);
        }
    }
}
