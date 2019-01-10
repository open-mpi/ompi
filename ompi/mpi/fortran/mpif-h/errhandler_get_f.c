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
 * Copyright (c) 2018      IBM Corporation.  All rights reserved.
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
#pragma weak PMPI_ERRHANDLER_GET = ompi_errhandler_get_f
#pragma weak pmpi_errhandler_get = ompi_errhandler_get_f
#pragma weak pmpi_errhandler_get_ = ompi_errhandler_get_f
#pragma weak pmpi_errhandler_get__ = ompi_errhandler_get_f

#pragma weak PMPI_Errhandler_get_f = ompi_errhandler_get_f
#pragma weak PMPI_Errhandler_get_f08 = ompi_errhandler_get_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_GET,
                           pmpi_errhandler_get,
                           pmpi_errhandler_get_,
                           pmpi_errhandler_get__,
                           pompi_errhandler_get_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_GET = ompi_errhandler_get_f
#pragma weak mpi_errhandler_get = ompi_errhandler_get_f
#pragma weak mpi_errhandler_get_ = ompi_errhandler_get_f
#pragma weak mpi_errhandler_get__ = ompi_errhandler_get_f

#pragma weak MPI_Errhandler_get_f = ompi_errhandler_get_f
#pragma weak MPI_Errhandler_get_f08 = ompi_errhandler_get_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_GET,
                           mpi_errhandler_get,
                           mpi_errhandler_get_,
                           mpi_errhandler_get__,
                           ompi_errhandler_get_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#else
#define ompi_errhandler_get_f pompi_errhandler_get_f
#endif
#endif


void ompi_errhandler_get_f(MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Errhandler c_errhandler;

    c_comm = PMPI_Comm_f2c(*comm);

    c_ierr = PMPI_Errhandler_get(c_comm, &c_errhandler);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *errhandler = PMPI_Errhandler_c2f(c_errhandler);
    }
}

