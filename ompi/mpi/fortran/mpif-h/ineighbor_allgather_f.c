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
#pragma weak PMPI_INEIGHBOR_ALLGATHER = ompi_ineighbor_allgather_f
#pragma weak pmpi_ineighbor_allgather = ompi_ineighbor_allgather_f
#pragma weak pmpi_ineighbor_allgather_ = ompi_ineighbor_allgather_f
#pragma weak pmpi_ineighbor_allgather__ = ompi_ineighbor_allgather_f

#pragma weak PMPI_Ineighbor_allgather_f = ompi_ineighbor_allgather_f
#pragma weak PMPI_Ineighbor_allgather_f08 = ompi_ineighbor_allgather_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INEIGHBOR_ALLGATHER,
                            pmpi_ineighbor_allgather,
                            pmpi_ineighbor_allgather_,
                            pmpi_ineighbor_allgather__,
                            pompi_ineighbor_allgather_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INEIGHBOR_ALLGATHER = ompi_ineighbor_allgather_f
#pragma weak mpi_ineighbor_allgather = ompi_ineighbor_allgather_f
#pragma weak mpi_ineighbor_allgather_ = ompi_ineighbor_allgather_f
#pragma weak mpi_ineighbor_allgather__ = ompi_ineighbor_allgather_f

#pragma weak MPI_Ineighbor_allgather_f = ompi_ineighbor_allgather_f
#pragma weak MPI_Ineighbor_allgather_f08 = ompi_ineighbor_allgather_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INEIGHBOR_ALLGATHER,
                            mpi_ineighbor_allgather,
                            mpi_ineighbor_allgather_,
                            mpi_ineighbor_allgather__,
                            ompi_ineighbor_allgather_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_ineighbor_allgather_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                                char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype,
                                MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Comm c_comm;
    MPI_Request c_req;
    MPI_Datatype c_sendtype, c_recvtype;

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    ierr_c = MPI_Ineighbor_allgather(sendbuf,
                                     OMPI_FINT_2_INT(*sendcount),
                                     c_sendtype,
                                     recvbuf,
                                     OMPI_FINT_2_INT(*recvcount),
                                     c_recvtype, c_comm, &c_req);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);

    if (MPI_SUCCESS == ierr_c) *request = MPI_Request_c2f(c_req);
}
