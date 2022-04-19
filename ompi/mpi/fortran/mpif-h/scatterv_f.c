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
#pragma weak PMPI_SCATTERV = ompi_scatterv_f
#pragma weak pmpi_scatterv = ompi_scatterv_f
#pragma weak pmpi_scatterv_ = ompi_scatterv_f
#pragma weak pmpi_scatterv__ = ompi_scatterv_f

#pragma weak PMPI_Scatterv_f = ompi_scatterv_f
#pragma weak PMPI_Scatterv_f08 = ompi_scatterv_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_SCATTERV,
                           pmpi_scatterv,
                           pmpi_scatterv_,
                           pmpi_scatterv__,
                           pompi_scatterv_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_SCATTERV = ompi_scatterv_f
#pragma weak mpi_scatterv = ompi_scatterv_f
#pragma weak mpi_scatterv_ = ompi_scatterv_f
#pragma weak mpi_scatterv__ = ompi_scatterv_f

#pragma weak MPI_Scatterv_f = ompi_scatterv_f
#pragma weak MPI_Scatterv_f08 = ompi_scatterv_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_SCATTERV,
                           mpi_scatterv,
                           mpi_scatterv_,
                           mpi_scatterv__,
                           ompi_scatterv_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr) )
#else
#define ompi_scatterv_f pompi_scatterv_f
#endif
#endif


void ompi_scatterv_f(char *sendbuf, MPI_Fint *sendcounts,
		    MPI_Fint *displs, MPI_Fint *sendtype,
		    char *recvbuf, MPI_Fint *recvcount,
		    MPI_Fint *recvtype, MPI_Fint *root,
		    MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = PMPI_Comm_f2c(*comm);
    c_sendtype = PMPI_Type_f2c(*sendtype);
    c_recvtype = PMPI_Type_f2c(*recvtype);

    PMPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_IN_PLACE(recvbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Scatterv(sendbuf,
                          OMPI_ARRAY_NAME_CONVERT(sendcounts),
                          OMPI_ARRAY_NAME_CONVERT(displs),
                          c_sendtype, recvbuf,
                          OMPI_FINT_2_INT(*recvcount),
                          c_recvtype,
                          OMPI_FINT_2_INT(*root), c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}
