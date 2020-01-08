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
#pragma weak PMPIX_ALLGATHERV_INIT = ompix_allgatherv_init_f
#pragma weak pmpix_allgatherv_init = ompix_allgatherv_init_f
#pragma weak pmpix_allgatherv_init_ = ompix_allgatherv_init_f
#pragma weak pmpix_allgatherv_init__ = ompix_allgatherv_init_f

#pragma weak PMPIX_Allgatherv_init_f = ompix_allgatherv_init_f
#pragma weak PMPIX_Allgatherv_init_f08 = ompix_allgatherv_init_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPIX_ALLGATHERV_INIT,
                            pmpix_allgatherv_init,
                            pmpix_allgatherv_init_,
                            pmpix_allgatherv_init__,
                            pompix_allgatherv_init_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPIX_ALLGATHERV_INIT = ompix_allgatherv_init_f
#pragma weak mpix_allgatherv_init = ompix_allgatherv_init_f
#pragma weak mpix_allgatherv_init_ = ompix_allgatherv_init_f
#pragma weak mpix_allgatherv_init__ = ompix_allgatherv_init_f

#pragma weak MPIX_Allgatherv_init_f = ompix_allgatherv_init_f
#pragma weak MPIX_Allgatherv_init_f08 = ompix_allgatherv_init_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPIX_ALLGATHERV_INIT,
                            mpix_allgatherv_init,
                            mpix_allgatherv_init_,
                            mpix_allgatherv_init__,
                            ompix_allgatherv_init_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr) )
#else
#define ompix_allgatherv_init_f pompix_allgatherv_init_f
#endif
#endif


void ompix_allgatherv_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                             char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs,
                             MPI_Fint *recvtype, MPI_Fint *comm,
                             MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype = NULL, c_recvtype;
    MPI_Request c_request;
    MPI_Info c_info;
    int ierr_c;
    OMPI_COND_STATEMENT(int size);
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = PMPI_Comm_f2c(*comm);
    c_info = PMPI_Info_f2c(*info);
    c_comm = PMPI_Comm_f2c(*comm);
    if (OMPI_COMM_IS_INTER(c_comm)) {
        OMPI_COND_STATEMENT(size = ompi_comm_remote_size(c_comm));
    } else {
        OMPI_COND_STATEMENT(size = ompi_comm_size(c_comm));
        if (OMPI_IS_FORTRAN_IN_PLACE(sendbuf)) {
            sendbuf = MPI_IN_PLACE;
        } else {
            c_sendtype = PMPI_Type_f2c(*sendtype);
        }
    }
    c_recvtype = PMPI_Type_f2c(*recvtype);

    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    ierr_c = PMPIX_Allgatherv_init(sendbuf,
                                   OMPI_FINT_2_INT(*sendcount),
                                   c_sendtype,
                                   recvbuf,
                                   OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                   OMPI_ARRAY_NAME_CONVERT(displs),
                                   c_recvtype, c_comm, c_info, &c_request);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);
    if (MPI_SUCCESS == ierr_c) *request = PMPI_Request_c2f(c_request);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}
