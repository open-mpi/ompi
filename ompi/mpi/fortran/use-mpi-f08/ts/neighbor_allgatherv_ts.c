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
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mpi/fortran/use-mpi-f08/ts/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

static const char FUNC_NAME[] = "MPI_Neighbor_allgatherv";

void ompi_neighbor_allgatherv_ts(CFI_cdesc_t *x1, MPI_Fint *sendcount, MPI_Fint *sendtype,
                                 CFI_cdesc_t *x2, MPI_Fint *recvcounts, MPI_Fint *displs,
                                 MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Datatype c_senddatatype, c_sendtype = PMPI_Type_f2c(*sendtype);
    char *sendbuf = x1->base_addr, *recvbuf = x2->base_addr;
    int c_sendcount =  OMPI_FINT_2_INT(*sendcount);
    MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    OMPI_CFI_CHECK_CONTIGUOUS(x2, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME)
        return;
    }
    OMPI_CFI_2_C(x1, c_sendcount, c_sendtype, c_senddatatype, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME)
        return;
    }

    c_sendtype = PMPI_Type_f2c(*sendtype);
    c_recvtype = PMPI_Type_f2c(*recvtype);

    PMPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Neighbor_allgatherv(sendbuf,
                                      c_sendcount,
                                      c_sendtype,
                                      recvbuf,
                                      OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                      OMPI_ARRAY_NAME_CONVERT(displs),
                                      c_recvtype, c_comm);

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}
