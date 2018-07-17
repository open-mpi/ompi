/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
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
#include "ompi/mpiext/pcollreq/mpif-h/mpiext_pcollreq_prototypes.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPIX_NEIGHBOR_ALLGATHERV_INIT = ompix_neighbor_allgatherv_init_f
#pragma weak pmpix_neighbor_allgatherv_init = ompix_neighbor_allgatherv_init_f
#pragma weak pmpix_neighbor_allgatherv_init_ = ompix_neighbor_allgatherv_init_f
#pragma weak pmpix_neighbor_allgatherv_init__ = ompix_neighbor_allgatherv_init_f

#pragma weak PMPIX_Neighbor_allgatherv_init_f = ompix_neighbor_allgatherv_init_f
#pragma weak PMPIX_Neighbor_allgatherv_init_f08 = ompix_neighbor_allgatherv_init_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPIX_NEIGHBOR_ALLGATHERV_INIT,
                            pmpix_neighbor_allgatherv_init,
                            pmpix_neighbor_allgatherv_init_,
                            pmpix_neighbor_allgatherv_init__,
                            pompix_neighbor_allgatherv_init_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPIX_NEIGHBOR_ALLGATHERV_INIT = ompix_neighbor_allgatherv_init_f
#pragma weak mpix_neighbor_allgatherv_init = ompix_neighbor_allgatherv_init_f
#pragma weak mpix_neighbor_allgatherv_init_ = ompix_neighbor_allgatherv_init_f
#pragma weak mpix_neighbor_allgatherv_init__ = ompix_neighbor_allgatherv_init_f

#pragma weak MPIX_Neighbor_allgatherv_init_f = ompix_neighbor_allgatherv_init_f
#pragma weak MPIX_Neighbor_allgatherv_init_f08 = ompix_neighbor_allgatherv_init_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPIX_NEIGHBOR_ALLGATHERV_INIT,
                            mpix_neighbor_allgatherv_init,
                            mpix_neighbor_allgatherv_init_,
                            mpix_neighbor_allgatherv_init__,
                            ompix_neighbor_allgatherv_init_f,
                            (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr),
                            (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, info, request, ierr) )
#else
#define ompix_neighbor_allgatherv_init_f pompix_neighbor_allgatherv_init_f
#endif
#endif


void ompix_neighbor_allgatherv_init_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
                                      char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs,
                                      MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *info, MPI_Fint *request,
                                      MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    MPI_Info c_info;
    MPI_Request c_request;
    int size, ierr_c;
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = PMPI_Comm_f2c(*comm);
    c_sendtype = PMPI_Type_f2c(*sendtype);
    c_recvtype = PMPI_Type_f2c(*recvtype);
    c_info = PMPI_Info_f2c(*info);

    PMPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    ierr_c = PMPIX_Neighbor_allgatherv_init(sendbuf,
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
