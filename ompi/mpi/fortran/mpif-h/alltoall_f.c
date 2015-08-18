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
#pragma weak PMPI_ALLTOALL = ompi_alltoall_f
#pragma weak pmpi_alltoall = ompi_alltoall_f
#pragma weak pmpi_alltoall_ = ompi_alltoall_f
#pragma weak pmpi_alltoall__ = ompi_alltoall_f

#pragma weak PMPI_Alltoall_f = ompi_alltoall_f
#pragma weak PMPI_Alltoall_f08 = ompi_alltoall_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ALLTOALL,
                           pmpi_alltoall,
                           pmpi_alltoall_,
                           pmpi_alltoall__,
                           pompi_alltoall_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLTOALL = ompi_alltoall_f
#pragma weak mpi_alltoall = ompi_alltoall_f
#pragma weak mpi_alltoall_ = ompi_alltoall_f
#pragma weak mpi_alltoall__ = ompi_alltoall_f

#pragma weak MPI_Alltoall_f = ompi_alltoall_f
#pragma weak MPI_Alltoall_f08 = ompi_alltoall_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ALLTOALL,
                           mpi_alltoall,
                           mpi_alltoall_,
                           mpi_alltoall__,
                           ompi_alltoall_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_alltoall_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
		    char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype,
		    MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = MPI_Alltoall(sendbuf,
                          OMPI_FINT_2_INT(*sendcount),
                          c_sendtype,
                          recvbuf,
                          OMPI_FINT_2_INT(*recvcount),
                          c_recvtype, c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
