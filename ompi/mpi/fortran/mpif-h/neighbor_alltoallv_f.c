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
#pragma weak PMPI_NEIGHBOR_ALLTOALLV = ompi_neighbor_alltoallv_f
#pragma weak pmpi_neighbor_alltoallv = ompi_neighbor_alltoallv_f
#pragma weak pmpi_neighbor_alltoallv_ = ompi_neighbor_alltoallv_f
#pragma weak pmpi_neighbor_alltoallv__ = ompi_neighbor_alltoallv_f

#pragma weak PMPI_Neighbor_alltoallv_f = ompi_neighbor_alltoallv_f
#pragma weak PMPI_Neighbor_alltoallv_f08 = ompi_neighbor_alltoallv_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_NEIGHBOR_ALLTOALLV,
                           pmpi_neighbor_alltoallv,
                           pmpi_neighbor_alltoallv_,
                           pmpi_neighbor_alltoallv__,
                           pompi_neighbor_alltoallv_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_NEIGHBOR_ALLTOALLV = ompi_neighbor_alltoallv_f
#pragma weak mpi_neighbor_alltoallv = ompi_neighbor_alltoallv_f
#pragma weak mpi_neighbor_alltoallv_ = ompi_neighbor_alltoallv_f
#pragma weak mpi_neighbor_alltoallv__ = ompi_neighbor_alltoallv_f

#pragma weak MPI_Neighbor_alltoallv_f = ompi_neighbor_alltoallv_f
#pragma weak MPI_Neighbor_alltoallv_f08 = ompi_neighbor_alltoallv_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_NEIGHBOR_ALLTOALLV,
                           mpi_neighbor_alltoallv,
                           mpi_neighbor_alltoallv_,
                           mpi_neighbor_alltoallv__,
                           ompi_neighbor_alltoallv_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, ierr) )
#else
#define ompi_neighbor_alltoallv_f pompi_neighbor_alltoallv_f
#endif
#endif


void ompi_neighbor_alltoallv_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls,
                               MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts,
                               MPI_Fint *rdispls, MPI_Fint *recvtype,
                               MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(sdispls);
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(rdispls);

    c_comm = PMPI_Comm_f2c(*comm);
    c_sendtype = PMPI_Type_f2c(*sendtype);
    c_recvtype = PMPI_Type_f2c(*recvtype);

    PMPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(sdispls, size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(rdispls, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Neighbor_alltoallv(sendbuf,
                                    OMPI_ARRAY_NAME_CONVERT(sendcounts),
                                    OMPI_ARRAY_NAME_CONVERT(sdispls),
                                    c_sendtype,
                                    recvbuf,
                                    OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                    OMPI_ARRAY_NAME_CONVERT(rdispls),
                                    c_recvtype, c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(sdispls);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(rdispls);
}
