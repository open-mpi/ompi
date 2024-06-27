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

static const char FUNC_NAME[] = "MPI_Iallgather";

void ompi_iallgather_ts(CFI_cdesc_t* x1, MPI_Fint *sendcount, MPI_Fint *sendtype,
                        CFI_cdesc_t* x2, MPI_Fint *recvcount, MPI_Fint *recvtype,
                        MPI_Fint *comm, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Request c_req;
    MPI_Datatype c_sendtype = NULL, c_senddatatype = NULL;
    void *sendbuf = x1->base_addr;
    int c_sendcount =  0;
    MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
    void *recvbuf = x2->base_addr;
    int c_recvcount =  OMPI_FINT_2_INT(*recvcount);

    OMPI_CFI_CHECK_CONTIGUOUS(x2, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME)
        return;
    }
    if (OMPI_COMM_IS_INTER(c_comm) || !OMPI_IS_FORTRAN_IN_PLACE(sendbuf)) {
        c_sendtype = PMPI_Type_f2c(*sendtype);
        c_sendcount = OMPI_FINT_2_INT(*sendcount);
        OMPI_CFI_2_C(x1, c_sendcount, c_sendtype, c_senddatatype, c_ierr);
        if (MPI_SUCCESS != c_ierr) {
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME)
            return;
        }
    } else {
        sendbuf = MPI_IN_PLACE;
    }

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_ierr = PMPI_Iallgather(sendbuf, c_sendcount, c_sendtype,
                             recvbuf, c_recvcount, c_recvtype,
                             c_comm, &c_req);

    if (c_senddatatype != c_sendtype && c_senddatatype != NULL) {
        ompi_datatype_destroy(&c_senddatatype);
    }

    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_req);
}
