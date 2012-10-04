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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TOPO_TEST = ompi_topo_test_f
#pragma weak pmpi_topo_test = ompi_topo_test_f
#pragma weak pmpi_topo_test_ = ompi_topo_test_f
#pragma weak pmpi_topo_test__ = ompi_topo_test_f

#pragma weak PMPI_Topo_test_f = ompi_topo_test_f
#pragma weak PMPI_Topo_test_f08 = ompi_topo_test_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TOPO_TEST,
                           pmpi_topo_test,
                           pmpi_topo_test_,
                           pmpi_topo_test__,
                           pompi_topo_test_f,
                           (MPI_Fint *comm, MPI_Fint *topo_type, MPI_Fint *ierr),
                           (comm, topo_type, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TOPO_TEST = ompi_topo_test_f
#pragma weak mpi_topo_test = ompi_topo_test_f
#pragma weak mpi_topo_test_ = ompi_topo_test_f
#pragma weak mpi_topo_test__ = ompi_topo_test_f

#pragma weak MPI_Topo_test_f = ompi_topo_test_f
#pragma weak MPI_Topo_test_f08 = ompi_topo_test_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TOPO_TEST,
                           mpi_topo_test,
                           mpi_topo_test_,
                           mpi_topo_test__,
                           ompi_topo_test_f,
                           (MPI_Fint *comm, MPI_Fint *topo_type, MPI_Fint *ierr),
                           (comm, topo_type, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_topo_test_f(MPI_Fint *comm, MPI_Fint *topo_type, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    OMPI_SINGLE_NAME_DECL(topo_type);

    c_comm = MPI_Comm_f2c(*comm);
    
    c_ierr = MPI_Topo_test(c_comm, OMPI_SINGLE_NAME_CONVERT(topo_type));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(topo_type);
    }
}
