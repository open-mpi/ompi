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
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
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
#pragma weak PMPI_TOPO_TEST = ompi_topo_test_f
#pragma weak pmpi_topo_test = ompi_topo_test_f
#pragma weak pmpi_topo_test_ = ompi_topo_test_f
#pragma weak pmpi_topo_test__ = ompi_topo_test_f

#pragma weak PMPI_Topo_test_f = ompi_topo_test_f
#pragma weak PMPI_Topo_test_f08 = ompi_topo_test_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TOPO_TEST,
                           pmpi_topo_test,
                           pmpi_topo_test_,
                           pmpi_topo_test__,
                           pompi_topo_test_f,
                           (MPI_Fint *comm, MPI_Fint *topo_type, MPI_Fint *ierr),
                           (comm, topo_type, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TOPO_TEST = ompi_topo_test_f
#pragma weak mpi_topo_test = ompi_topo_test_f
#pragma weak mpi_topo_test_ = ompi_topo_test_f
#pragma weak mpi_topo_test__ = ompi_topo_test_f

#pragma weak MPI_Topo_test_f = ompi_topo_test_f
#pragma weak MPI_Topo_test_f08 = ompi_topo_test_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TOPO_TEST,
                           mpi_topo_test,
                           mpi_topo_test_,
                           mpi_topo_test__,
                           ompi_topo_test_f,
                           (MPI_Fint *comm, MPI_Fint *topo_type, MPI_Fint *ierr),
                           (comm, topo_type, ierr) )
#else
#define ompi_topo_test_f pompi_topo_test_f
#endif
#endif


void ompi_topo_test_f(MPI_Fint *comm, MPI_Fint *topo_type, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    OMPI_SINGLE_NAME_DECL(topo_type);

    c_comm = PMPI_Comm_f2c(*comm);

    c_ierr = PMPI_Topo_test(c_comm, OMPI_SINGLE_NAME_CONVERT(topo_type));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(topo_type);
    }
}
