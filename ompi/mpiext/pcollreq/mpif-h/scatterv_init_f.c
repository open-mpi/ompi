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
#include "ompi/mpiext/pcollreq/mpif-h/mpiext_pcollreq_prototypes.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPIX_SCATTERV_INIT = ompix_scatterv_init_f
#pragma weak pmpix_scatterv_init = ompix_scatterv_init_f
#pragma weak pmpix_scatterv_init_ = ompix_scatterv_init_f
#pragma weak pmpix_scatterv_init__ = ompix_scatterv_init_f

#pragma weak PMPIX_Scatterv_init_f = ompix_scatterv_init_f
#pragma weak PMPIX_Scatterv_init_f08 = ompix_scatterv_init_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPIX_SCATTERV_INIT,
                            pmpix_scatterv_init,
                            pmpix_scatterv_init_,
                            pmpix_scatterv_init__,
                            pompix_scatterv_init_f,
                            (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPIX_SCATTERV_INIT = ompix_scatterv_init_f
#pragma weak mpix_scatterv_init = ompix_scatterv_init_f
#pragma weak mpix_scatterv_init_ = ompix_scatterv_init_f
#pragma weak mpix_scatterv_init__ = ompix_scatterv_init_f

#pragma weak MPIX_Scatterv_init_f = ompix_scatterv_init_f
#pragma weak MPIX_Scatterv_init_f08 = ompix_scatterv_init_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPIX_SCATTERV_INIT,
                            mpix_scatterv_init,
                            mpix_scatterv_init_,
                            mpix_scatterv_init__,
                            ompix_scatterv_init_f,
                            (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *displs, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, info, request, ierr) )
#else
#define ompix_scatterv_init_f pompix_scatterv_init_f
#endif
#endif


void ompix_scatterv_init_f(char *sendbuf, MPI_Fint *sendcounts,
                           MPI_Fint *displs, MPI_Fint *sendtype,
                           char *recvbuf, MPI_Fint *recvcount,
                           MPI_Fint *recvtype, MPI_Fint *root,
                           MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype = NULL, c_recvtype = NULL;
    int c_root, c_ierr;
    MPI_Info c_info;
    MPI_Request c_request;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_root = OMPI_FINT_2_INT(*root);
    c_comm = PMPI_Comm_f2c(*comm);
    c_info = PMPI_Info_f2c(*info);
    if (OMPI_COMM_IS_INTER(c_comm)) {
        if (MPI_ROOT == c_root) {
            OMPI_COND_STATEMENT(int size = ompi_comm_remote_size(c_comm));
            c_sendtype = PMPI_Type_f2c(*sendtype);
            OMPI_ARRAY_FINT_2_INT(sendcounts, size);
            OMPI_ARRAY_FINT_2_INT(displs, size);
        } else if (MPI_PROC_NULL != c_root) {
            c_sendtype = PMPI_Type_f2c(*sendtype);
        }
    } else {
        if (OMPI_IS_FORTRAN_IN_PLACE(recvbuf)) {
            recvbuf = MPI_IN_PLACE;
        } else {
            c_recvtype = PMPI_Type_f2c(*recvtype);
        }
        if (ompi_comm_rank(c_comm) == c_root) {
            OMPI_COND_STATEMENT(int size = ompi_comm_size(c_comm));
            c_sendtype = PMPI_Type_f2c(*sendtype);
            OMPI_ARRAY_FINT_2_INT(sendcounts, size);
            OMPI_ARRAY_FINT_2_INT(displs, size);
        }
    }

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPIX_Scatterv_init(sendbuf,
                                OMPI_ARRAY_NAME_CONVERT(sendcounts),
                                OMPI_ARRAY_NAME_CONVERT(displs),
                                c_sendtype, recvbuf,
                                OMPI_FINT_2_INT(*recvcount),
                                c_recvtype,
                                OMPI_FINT_2_INT(*root), c_comm, c_info, &c_request);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_request);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}
