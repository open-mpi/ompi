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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_NEIGHBOR_ALLGATHERV = ompi_neighbor_allgatherv_f
#pragma weak pmpi_neighbor_allgatherv = ompi_neighbor_allgatherv_f
#pragma weak pmpi_neighbor_allgatherv_ = ompi_neighbor_allgatherv_f
#pragma weak pmpi_neighbor_allgatherv__ = ompi_neighbor_allgatherv_f

#pragma weak PMPI_Neighbor_allgatherv_f = ompi_neighbor_allgatherv_f
#pragma weak PMPI_Neighbor_allgatherv_f08 = ompi_neighbor_allgatherv_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_NEIGHBOR_ALLGATHERV,
                           pmpi_neighbor_allgatherv,
                           pmpi_neighbor_allgatherv_,
                           pmpi_neighbor_allgatherv__,
                           pompi_neighbor_allgatherv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_NEIGHBOR_ALLGATHERV = ompi_neighbor_allgatherv_f
#pragma weak mpi_neighbor_allgatherv = ompi_neighbor_allgatherv_f
#pragma weak mpi_neighbor_allgatherv_ = ompi_neighbor_allgatherv_f
#pragma weak mpi_neighbor_allgatherv__ = ompi_neighbor_allgatherv_f

#pragma weak MPI_Neighbor_allgatherv_f = ompi_neighbor_allgatherv_f
#pragma weak MPI_Neighbor_allgatherv_f08 = ompi_neighbor_allgatherv_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_NEIGHBOR_ALLGATHERV,
                           mpi_neighbor_allgatherv,
                           mpi_neighbor_allgatherv_,
                           mpi_neighbor_allgatherv__,
                           ompi_neighbor_allgatherv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_neighbor_allgatherv_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                                char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs,
                                MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    int size, ierr_c;
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    ierr_c = MPI_Neighbor_allgatherv(sendbuf,
                                     OMPI_FINT_2_INT(*sendcount),
                                     c_sendtype,
                                     recvbuf,
                                     OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                     OMPI_ARRAY_NAME_CONVERT(displs),
                                     c_recvtype, c_comm);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}
