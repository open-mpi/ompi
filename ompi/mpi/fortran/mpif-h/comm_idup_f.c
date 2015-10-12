/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2011-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
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
#pragma weak PMPI_COMM_IDUP = ompi_comm_idup_f
#pragma weak pmpi_comm_idup = ompi_comm_idup_f
#pragma weak pmpi_comm_idup_ = ompi_comm_idup_f
#pragma weak pmpi_comm_idup__ = ompi_comm_idup_f

#pragma weak PMPI_Comm_idup_f = ompi_comm_idup_f
#pragma weak PMPI_Comm_idup_f08 = ompi_comm_idup_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_IDUP,
                           pmpi_comm_idup,
                           pmpi_comm_idup_,
                           pmpi_comm_idup__,
                           pompi_comm_idup_f,
                            (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr),
                            (comm, newcomm, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_IDUP = ompi_comm_idup_f
#pragma weak mpi_comm_idup = ompi_comm_idup_f
#pragma weak mpi_comm_idup_ = ompi_comm_idup_f
#pragma weak mpi_comm_idup__ = ompi_comm_idup_f

#pragma weak MPI_Comm_idup_f = ompi_comm_idup_f
#pragma weak MPI_Comm_idup_f08 = ompi_comm_idup_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_IDUP,
                            mpi_comm_idup,
                            mpi_comm_idup_,
                            mpi_comm_idup__,
                            ompi_comm_idup_f,
                            (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr),
                            (comm, newcomm, request, ierr) )
#else
#define ompi_comm_idup_f pompi_comm_idup_f
#endif
#endif


void ompi_comm_idup_f(MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_newcomm;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Request c_req;

    c_ierr = PMPI_Comm_idup(c_comm, &c_newcomm, &c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *newcomm = PMPI_Comm_c2f(c_newcomm);
        *request = PMPI_Request_c2f(c_req);
    }
}
