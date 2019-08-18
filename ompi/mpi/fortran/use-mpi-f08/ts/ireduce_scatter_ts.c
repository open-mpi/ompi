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

#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mpi/fortran/use-mpi-f08/ts/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

static const char FUNC_NAME[] = "MPI_Ireduce_scatter";

void ompi_ireduce_scatter_ts(CFI_cdesc_t *x1, CFI_cdesc_t *x2,
                             MPI_Fint *recvcounts, MPI_Fint *datatype,
                             MPI_Fint *op, MPI_Fint *comm, MPI_Fint *request,
                             MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Datatype c_type;
    MPI_Request c_request;
    MPI_Op c_op;
    int size;
    char *sendbuf = x1->base_addr, *recvbuf = x2->base_addr;
    OMPI_ARRAY_NAME_DECL(recvcounts);

    OMPI_CFI_CHECK_CONTIGUOUS(x1, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME);
        return;
    }
    OMPI_CFI_CHECK_CONTIGUOUS(x2, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME);
        return;
    }
    c_type = PMPI_Type_f2c(*datatype);
    c_op = PMPI_Op_f2c(*op);

    PMPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Ireduce_scatter(sendbuf, recvbuf,
                                 OMPI_ARRAY_NAME_CONVERT(recvcounts),
                                 c_type, c_op, c_comm, &c_request);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_request);
}
