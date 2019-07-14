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
 *                         and Technology (RIST).  All rights reserved.
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
#pragma weak PMPI_IGATHERV = ompi_igatherv_f
#pragma weak pmpi_igatherv = ompi_igatherv_f
#pragma weak pmpi_igatherv_ = ompi_igatherv_f
#pragma weak pmpi_igatherv__ = ompi_igatherv_f

#pragma weak PMPI_Igatherv_f = ompi_igatherv_f
#pragma weak PMPI_Igatherv_f08 = ompi_igatherv_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_IGATHERV,
                            pmpi_igatherv,
                            pmpi_igatherv_,
                            pmpi_igatherv__,
                            pompi_igatherv_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IGATHERV = ompi_igatherv_f
#pragma weak mpi_igatherv = ompi_igatherv_f
#pragma weak mpi_igatherv_ = ompi_igatherv_f
#pragma weak mpi_igatherv__ = ompi_igatherv_f

#pragma weak MPI_Igatherv_f = ompi_igatherv_f
#pragma weak MPI_Igatherv_f08 = ompi_igatherv_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_IGATHERV,
                            mpi_igatherv,
                            mpi_igatherv_,
                            mpi_igatherv__,
                            ompi_igatherv_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request,MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, request, ierr) )
#else
#define ompi_igatherv_f pompi_igatherv_f
#endif
#endif


void ompi_igatherv_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                     char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs,
                     MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm,
                     MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    int c_root = OMPI_FINT_2_INT(*root);
    MPI_Datatype c_sendtype = NULL, c_recvtype = NULL;
    int c_sendcount = 0, c_recvcount = 0;
    MPI_Request c_request;
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    if (OMPI_COMM_IS_INTER(c_comm)) {
        if (MPI_ROOT == c_root) {
            OMPI_COND_STATEMENT(int size = ompi_comm_remote_size(c_comm));
            c_recvtype = PMPI_Type_f2c(*recvtype);
            OMPI_ARRAY_FINT_2_INT(recvcounts, size);
            OMPI_ARRAY_FINT_2_INT(displs, size);
        } else if (MPI_PROC_NULL != c_root) {
            c_sendtype = PMPI_Type_f2c(*sendtype);
            c_sendcount = OMPI_FINT_2_INT(*sendcount);
        }
    } else {
        if (OMPI_IS_FORTRAN_IN_PLACE(sendbuf)) {
            sendbuf = MPI_IN_PLACE;
        } else {
            c_sendtype = PMPI_Type_f2c(*sendtype);
            c_sendcount = OMPI_FINT_2_INT(*sendcount);
        }
        if (ompi_comm_rank(c_comm) == c_root) {
            OMPI_COND_STATEMENT(int size = ompi_comm_size(c_comm));
            c_recvtype = PMPI_Type_f2c(*recvtype);
            OMPI_ARRAY_FINT_2_INT(recvcounts, size);
            OMPI_ARRAY_FINT_2_INT(displs, size);
        }
    }

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Igatherv(sendbuf, c_sendcount,
                           c_sendtype, recvbuf,
                           OMPI_ARRAY_NAME_CONVERT(recvcounts),
                           OMPI_ARRAY_NAME_CONVERT(displs),
                           c_recvtype,
                           c_root,
                           c_comm, &c_request);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_request);
}
