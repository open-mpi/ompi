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
#pragma weak PMPI_ALLGATHER = ompi_allgather_f
#pragma weak pmpi_allgather = ompi_allgather_f
#pragma weak pmpi_allgather_ = ompi_allgather_f
#pragma weak pmpi_allgather__ = ompi_allgather_f

#pragma weak PMPI_Allgather_f = ompi_allgather_f
#pragma weak PMPI_Allgather_f08 = ompi_allgather_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ALLGATHER,
                           pmpi_allgather,
                           pmpi_allgather_,
                           pmpi_allgather__,
                           pompi_allgather_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLGATHER = ompi_allgather_f
#pragma weak mpi_allgather = ompi_allgather_f
#pragma weak mpi_allgather_ = ompi_allgather_f
#pragma weak mpi_allgather__ = ompi_allgather_f

#pragma weak MPI_Allgather_f = ompi_allgather_f
#pragma weak MPI_Allgather_f08 = ompi_allgather_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ALLGATHER,
                           mpi_allgather,
                           mpi_allgather_,
                           mpi_allgather__,
                           ompi_allgather_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierr) )
#else
#define ompi_allgather_f pompi_allgather_f
#endif
#endif


void ompi_allgather_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
		     char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype,
		     MPI_Fint *comm, MPI_Fint *ierr)
{
    int ierr_c;
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;

    c_comm = PMPI_Comm_f2c(*comm);
    c_sendtype = PMPI_Type_f2c(*sendtype);
    c_recvtype = PMPI_Type_f2c(*recvtype);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    ierr_c = PMPI_Allgather(sendbuf,
                            OMPI_FINT_2_INT(*sendcount),
                            c_sendtype,
                            recvbuf,
                            OMPI_FINT_2_INT(*recvcount),
                            c_recvtype, c_comm);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);
}
