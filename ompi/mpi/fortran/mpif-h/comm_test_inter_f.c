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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_COMM_TEST_INTER = ompi_comm_test_inter_f
#pragma weak pmpi_comm_test_inter = ompi_comm_test_inter_f
#pragma weak pmpi_comm_test_inter_ = ompi_comm_test_inter_f
#pragma weak pmpi_comm_test_inter__ = ompi_comm_test_inter_f

#pragma weak PMPI_Comm_test_inter_f = ompi_comm_test_inter_f
#pragma weak PMPI_Comm_test_inter_f08 = ompi_comm_test_inter_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_TEST_INTER,
                           pmpi_comm_test_inter,
                           pmpi_comm_test_inter_,
                           pmpi_comm_test_inter__,
                           pompi_comm_test_inter_f,
                           (MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (comm, flag, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_TEST_INTER = ompi_comm_test_inter_f
#pragma weak mpi_comm_test_inter = ompi_comm_test_inter_f
#pragma weak mpi_comm_test_inter_ = ompi_comm_test_inter_f
#pragma weak mpi_comm_test_inter__ = ompi_comm_test_inter_f

#pragma weak MPI_Comm_test_inter_f = ompi_comm_test_inter_f
#pragma weak MPI_Comm_test_inter_f08 = ompi_comm_test_inter_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_TEST_INTER,
                           mpi_comm_test_inter,
                           mpi_comm_test_inter_,
                           mpi_comm_test_inter__,
                           ompi_comm_test_inter_f,
                           (MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (comm, flag, ierr) )
#else
#define ompi_comm_test_inter_f pompi_comm_test_inter_f
#endif
#endif


void ompi_comm_test_inter_f(MPI_Fint *comm, ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = PMPI_Comm_f2c (*comm);
    OMPI_LOGICAL_NAME_DECL(flag);

    c_ierr = PMPI_Comm_test_inter(c_comm, OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_LOGICAL(flag);
    }
}
