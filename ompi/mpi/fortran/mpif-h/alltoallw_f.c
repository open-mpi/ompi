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
 * Copyright (c) 2015-2019 Research Organization for Information Science
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
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_ALLTOALLW = ompi_alltoallw_f
#pragma weak pmpi_alltoallw = ompi_alltoallw_f
#pragma weak pmpi_alltoallw_ = ompi_alltoallw_f
#pragma weak pmpi_alltoallw__ = ompi_alltoallw_f

#pragma weak PMPI_Alltoallw_f = ompi_alltoallw_f
#pragma weak PMPI_Alltoallw_f08 = ompi_alltoallw_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ALLTOALLW,
                           pmpi_alltoallw,
                           pmpi_alltoallw_,
                           pmpi_alltoallw__,
                           pompi_alltoallw_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLTOALLW = ompi_alltoallw_f
#pragma weak mpi_alltoallw = ompi_alltoallw_f
#pragma weak mpi_alltoallw_ = ompi_alltoallw_f
#pragma weak mpi_alltoallw__ = ompi_alltoallw_f

#pragma weak MPI_Alltoallw_f = ompi_alltoallw_f
#pragma weak MPI_Alltoallw_f08 = ompi_alltoallw_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ALLTOALLW,
                           mpi_alltoallw,
                           mpi_alltoallw_,
                           mpi_alltoallw__,
                           ompi_alltoallw_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, ierr) )
#else
#define ompi_alltoallw_f pompi_alltoallw_f
#endif
#endif


void ompi_alltoallw_f(char *sendbuf, MPI_Fint *sendcounts,
		     MPI_Fint *sdispls, MPI_Fint *sendtypes,
		     char *recvbuf, MPI_Fint *recvcounts,
		     MPI_Fint *rdispls, MPI_Fint *recvtypes,
		     MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype *c_sendtypes = NULL, *c_recvtypes;
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(sdispls);
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(rdispls);

    c_comm = PMPI_Comm_f2c(*comm);
    size = OMPI_COMM_IS_INTER(c_comm)?ompi_comm_remote_size(c_comm):ompi_comm_size(c_comm);

    if (!OMPI_IS_FORTRAN_IN_PLACE(sendbuf)) {
        c_sendtypes = (MPI_Datatype *) malloc(size * sizeof(MPI_Datatype));
        OMPI_ARRAY_FINT_2_INT(sendcounts, size);
        OMPI_ARRAY_FINT_2_INT(sdispls, size);
        for (int i=0; i<size; i++) {
            c_sendtypes[i] = PMPI_Type_f2c(sendtypes[i]);
        }
    }

    c_recvtypes = (MPI_Datatype *) malloc(size * sizeof(MPI_Datatype));
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(rdispls, size);
    for (int i=0; i<size; i++) {
        c_recvtypes[i] = PMPI_Type_f2c(recvtypes[i]);
    }

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Alltoallw(sendbuf,
                           OMPI_ARRAY_NAME_CONVERT(sendcounts),
                           OMPI_ARRAY_NAME_CONVERT(sdispls),
                           c_sendtypes,
                           recvbuf,
                           OMPI_ARRAY_NAME_CONVERT(recvcounts),
                           OMPI_ARRAY_NAME_CONVERT(rdispls),
                           c_recvtypes, c_comm);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(sdispls);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(rdispls);
    if (NULL != c_sendtypes) {
        free(c_sendtypes);
    }
    free(c_recvtypes);
}
