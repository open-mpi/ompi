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
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_NEIGHBOR_ALLTOALLW = ompi_neighbor_alltoallw_f
#pragma weak pmpi_neighbor_alltoallw = ompi_neighbor_alltoallw_f
#pragma weak pmpi_neighbor_alltoallw_ = ompi_neighbor_alltoallw_f
#pragma weak pmpi_neighbor_alltoallw__ = ompi_neighbor_alltoallw_f

#pragma weak PMPI_Neighbor_alltoallw_f = ompi_neighbor_alltoallw_f
#pragma weak PMPI_Neighbor_alltoallw_f08 = ompi_neighbor_alltoallw_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_NEIGHBOR_ALLTOALLW,
                           pmpi_neighbor_alltoallw,
                           pmpi_neighbor_alltoallw_,
                           pmpi_neighbor_alltoallw__,
                           pompi_neighbor_alltoallw_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_NEIGHBOR_ALLTOALLW = ompi_neighbor_alltoallw_f
#pragma weak mpi_neighbor_alltoallw = ompi_neighbor_alltoallw_f
#pragma weak mpi_neighbor_alltoallw_ = ompi_neighbor_alltoallw_f
#pragma weak mpi_neighbor_alltoallw__ = ompi_neighbor_alltoallw_f

#pragma weak MPI_Neighbor_alltoallw_f = ompi_neighbor_alltoallw_f
#pragma weak MPI_Neighbor_alltoallw_f08 = ompi_neighbor_alltoallw_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_NEIGHBOR_ALLTOALLW,
                           mpi_neighbor_alltoallw,
                           mpi_neighbor_alltoallw_,
                           mpi_neighbor_alltoallw__,
                           ompi_neighbor_alltoallw_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Aint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Aint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, ierr) )
#else
#define ompi_neighbor_alltoallw_f pompi_neighbor_alltoallw_f
#endif
#endif


void ompi_neighbor_alltoallw_f(char *sendbuf, MPI_Fint *sendcounts,
                               MPI_Aint *sdispls, MPI_Fint *sendtypes,
                               char *recvbuf, MPI_Fint *recvcounts,
                               MPI_Aint *rdispls, MPI_Fint *recvtypes,
                               MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype *c_sendtypes, *c_recvtypes;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(recvcounts);

    c_comm = PMPI_Comm_f2c(*comm);
    PMPI_Comm_size(c_comm, &size);

    c_sendtypes = (MPI_Datatype *) malloc(size * sizeof(MPI_Datatype));
    c_recvtypes = (MPI_Datatype *) malloc(size * sizeof(MPI_Datatype));

    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);

    while (size > 0) {
        c_sendtypes[size - 1] = PMPI_Type_f2c(sendtypes[size - 1]);
        c_recvtypes[size - 1] = PMPI_Type_f2c(recvtypes[size - 1]);
        --size;
    }

    /* Alltoallw does not support MPI_IN_PLACE */
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Neighbor_alltoallw(sendbuf,
                                    OMPI_ARRAY_NAME_CONVERT(sendcounts),
                                    sdispls,
                                    c_sendtypes,
                                    recvbuf,
                                    OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                    rdispls,
                                    c_recvtypes, c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    free(c_sendtypes);
    free(c_recvtypes);
}
