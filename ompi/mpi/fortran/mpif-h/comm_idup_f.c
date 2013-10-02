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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_IDUP = ompi_comm_idup_f
#pragma weak pmpi_comm_idup = ompi_comm_idup_f
#pragma weak pmpi_comm_idup_ = ompi_comm_idup_f
#pragma weak pmpi_comm_idup__ = ompi_comm_idup_f

#pragma weak PMPI_Comm_idup_f = ompi_comm_idup_f
#pragma weak PMPI_Comm_idup_f08 = ompi_comm_idup_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_IDUP,
                           pmpi_comm_idup,
                           pmpi_comm_idup_,
                           pmpi_comm_idup__,
                           pompi_comm_idup_f,
                            (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr),
                            (comm, newcomm, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_IDUP = ompi_comm_idup_f
#pragma weak mpi_comm_idup = ompi_comm_idup_f
#pragma weak mpi_comm_idup_ = ompi_comm_idup_f
#pragma weak mpi_comm_idup__ = ompi_comm_idup_f

#pragma weak MPI_Comm_idup_f = ompi_comm_idup_f
#pragma weak MPI_Comm_idup_f08 = ompi_comm_idup_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_IDUP,
                            mpi_comm_idup,
                            mpi_comm_idup_,
                            mpi_comm_idup__,
                            ompi_comm_idup_f,
                            (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr),
                            (comm, newcomm, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_comm_idup_f(MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_newcomm;
    MPI_Comm c_comm = MPI_Comm_f2c(*comm);
    MPI_Request c_req;

    c_ierr = MPI_Comm_idup(c_comm, &c_newcomm, &c_req);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *newcomm = MPI_Comm_c2f(c_newcomm);
        *request = MPI_Request_c2f(c_req);
    }
}
