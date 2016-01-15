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
#include "ompi/mpi/fortran/base/constants.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_GATHER = ompi_gather_f
#pragma weak pmpi_gather = ompi_gather_f
#pragma weak pmpi_gather_ = ompi_gather_f
#pragma weak pmpi_gather__ = ompi_gather_f

#pragma weak PMPI_Gather_f = ompi_gather_f
#pragma weak PMPI_Gather_f08 = ompi_gather_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_GATHER,
                           pmpi_gather,
                           pmpi_gather_,
                           pmpi_gather__,
                           pompi_gather_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GATHER = ompi_gather_f
#pragma weak mpi_gather = ompi_gather_f
#pragma weak mpi_gather_ = ompi_gather_f
#pragma weak mpi_gather__ = ompi_gather_f

#pragma weak MPI_Gather_f = ompi_gather_f
#pragma weak MPI_Gather_f08 = ompi_gather_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_GATHER,
                           mpi_gather,
                           mpi_gather_,
                           mpi_gather__,
                           ompi_gather_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#else
#define ompi_gather_f pompi_gather_f
#endif
#endif


void ompi_gather_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
		  char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype,
		  MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;

    c_comm = PMPI_Comm_f2c(*comm);
    c_sendtype = PMPI_Type_f2c(*sendtype);
    c_recvtype = PMPI_Type_f2c(*recvtype);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Gather(sendbuf, OMPI_FINT_2_INT(*sendcount),
                        c_sendtype, recvbuf,
                        OMPI_FINT_2_INT(*recvcount),
                        c_recvtype,
                        OMPI_FINT_2_INT(*root),
                        c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
