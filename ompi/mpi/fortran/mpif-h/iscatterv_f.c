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
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
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
#pragma weak PMPI_ISCATTERV = ompi_iscatterv_f
#pragma weak pmpi_iscatterv = ompi_iscatterv_f
#pragma weak pmpi_iscatterv_ = ompi_iscatterv_f
#pragma weak pmpi_iscatterv__ = ompi_iscatterv_f

#pragma weak PMPI_Iscatterv_f = ompi_iscatterv_f
#pragma weak PMPI_Iscatterv_f08 = ompi_iscatterv_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ISCATTERV,
                            pmpi_iscatterv,
                            pmpi_iscatterv_,
                            pmpi_iscatterv__,
                            pompi_iscatterv_f,
                            (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ISCATTERV = ompi_iscatterv_f
#pragma weak mpi_iscatterv = ompi_iscatterv_f
#pragma weak mpi_iscatterv_ = ompi_iscatterv_f
#pragma weak mpi_iscatterv__ = ompi_iscatterv_f

#pragma weak MPI_Iscatterv_f = ompi_iscatterv_f
#pragma weak MPI_Iscatterv_f08 = ompi_iscatterv_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ISCATTERV,
                            mpi_iscatterv,
                            mpi_iscatterv_,
                            mpi_iscatterv__,
                            ompi_iscatterv_f,
                            (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, request, ierr) )
#else
#define ompi_iscatterv_f pompi_iscatterv_f
#endif
#endif


void ompi_iscatterv_f(char *sendbuf, MPI_Fint *sendcounts,
                      MPI_Fint *displs, MPI_Fint *sendtype,
                      char *recvbuf, MPI_Fint *recvcount,
                      MPI_Fint *recvtype, MPI_Fint *root,
                      MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Request c_request;
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

    c_ierr = PMPI_Iscatterv(sendbuf,
                           OMPI_ARRAY_NAME_CONVERT(sendcounts),
                           OMPI_ARRAY_NAME_CONVERT(displs),
                           c_sendtype, recvbuf,
                           OMPI_FINT_2_INT(*recvcount),
                           c_recvtype,
                           OMPI_FINT_2_INT(*root), c_comm, &c_request);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_request);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}
