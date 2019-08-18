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

#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mpi/fortran/use-mpi-f08/ts/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/communicator/communicator.h"

static const char FUNC_NAME[] = "MPI_Alltoallw";

void ompi_alltoallw_ts(CFI_cdesc_t *x1, MPI_Fint *sendcounts,
                       MPI_Fint *sdispls, MPI_Fint *sendtypes,
                       CFI_cdesc_t *x2, MPI_Fint *recvcounts,
                       MPI_Fint *rdispls, MPI_Fint *recvtypes,
                       MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Datatype *c_sendtypes = NULL, *c_recvtypes;
    int size, c_ierr;
    char *sendbuf = x1->base_addr, *recvbuf = x2->base_addr;

    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(sdispls);
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(rdispls);

    OMPI_CFI_CHECK_CONTIGUOUS(x1, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME)
        return;
    }
    OMPI_CFI_CHECK_CONTIGUOUS(x2, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME)
        return;
    }
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
