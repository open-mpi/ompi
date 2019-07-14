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
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_ISCATTER = ompi_iscatter_f
#pragma weak pmpi_iscatter = ompi_iscatter_f
#pragma weak pmpi_iscatter_ = ompi_iscatter_f
#pragma weak pmpi_iscatter__ = ompi_iscatter_f

#pragma weak PMPI_Iscatter_f = ompi_iscatter_f
#pragma weak PMPI_Iscatter_f08 = ompi_iscatter_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ISCATTER,
                            pmpi_iscatter,
                            pmpi_iscatter_,
                            pmpi_iscatter__,
                            pompi_iscatter_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ISCATTER = ompi_iscatter_f
#pragma weak mpi_iscatter = ompi_iscatter_f
#pragma weak mpi_iscatter_ = ompi_iscatter_f
#pragma weak mpi_iscatter__ = ompi_iscatter_f

#pragma weak MPI_Iscatter_f = ompi_iscatter_f
#pragma weak MPI_Iscatter_f08 = ompi_iscatter_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ISCATTER,
                            mpi_iscatter,
                            mpi_iscatter_,
                            mpi_iscatter__,
                            ompi_iscatter_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, request, ierr) )
#else
#define ompi_iscatter_f pompi_iscatter_f
#endif
#endif


void ompi_iscatter_f(char *sendbuf, MPI_Fint *sendcount,
                     MPI_Fint *sendtype, char *recvbuf,
                     MPI_Fint *recvcount, MPI_Fint *recvtype,
                     MPI_Fint *root, MPI_Fint *comm, MPI_Fint *request,
                     MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    int c_root = OMPI_FINT_2_INT(*root);
    MPI_Datatype c_sendtype = NULL, c_recvtype = NULL;
    int c_sendcount = 0, c_recvcount = 0;
    MPI_Request c_request;

    if (OMPI_COMM_IS_INTER(c_comm)) {
        if (MPI_ROOT == c_root) {
            c_sendtype = PMPI_Type_f2c(*sendtype);
            c_sendcount = OMPI_FINT_2_INT(*sendcount);
        } else if (MPI_PROC_NULL != c_root) {
            c_recvtype = PMPI_Type_f2c(*recvtype);
            c_recvcount = OMPI_FINT_2_INT(*recvcount);
        }
    } else {
        if (OMPI_IS_FORTRAN_IN_PLACE(recvbuf)) {
            recvbuf = MPI_IN_PLACE;
        } else {
            c_recvtype = PMPI_Type_f2c(*recvtype);
            c_recvcount = OMPI_FINT_2_INT(*recvcount);
        }
        if (ompi_comm_rank(c_comm) == c_root) {
            c_sendtype = PMPI_Type_f2c(*sendtype);
            c_sendcount = OMPI_FINT_2_INT(*sendcount);
        }
    }

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Iscatter(sendbuf,c_sendcount, c_sendtype,
                           recvbuf, c_recvcount, c_recvtype,
                           c_root, c_comm, &c_request);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_request);
}
