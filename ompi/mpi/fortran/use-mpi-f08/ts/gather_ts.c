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

static const char FUNC_NAME[] = "MPI_Gather";

void ompi_gather_ts(CFI_cdesc_t *x1, MPI_Fint *sendcount, MPI_Fint *sendtype,
                    CFI_cdesc_t *x2, MPI_Fint *recvcount, MPI_Fint *recvtype,
                    MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_root, c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_senddatatype = NULL, c_sendtype = NULL;
    void *sendbuf = x1->base_addr;
    int c_sendcount =  0, c_recvcount = 0;
    MPI_Datatype c_recvtype = NULL, c_recvdatatype = NULL;
    char *recvbuf = NULL;

    c_comm = PMPI_Comm_f2c(*comm);
    c_root = OMPI_FINT_2_INT(*root);

    if (OMPI_COMM_IS_INTER(c_comm)) {
        if (MPI_ROOT == c_root) {
            c_recvtype = PMPI_Type_f2c(*recvtype);
            c_recvcount = OMPI_FINT_2_INT(*recvcount);
            OMPI_CFI_2_C_ALLOC(x2, recvbuf, c_recvcount, c_recvtype, c_recvdatatype, c_ierr);
            if (MPI_SUCCESS != c_ierr) {
                if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
                OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME)
                return;
            }
        } else if (MPI_PROC_NULL != c_root) {
            c_sendtype = PMPI_Type_f2c(*sendtype);
            c_sendcount = OMPI_FINT_2_INT(*sendcount);
            OMPI_CFI_2_C(x1, c_sendcount, c_sendtype, c_senddatatype, c_ierr);
            if (MPI_SUCCESS != c_ierr) {
                if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
                OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME)
                return;
            }
        }
    } else {
        if (ompi_comm_rank(c_comm) == c_root) {
            c_recvtype = PMPI_Type_f2c(*recvtype);
            c_recvcount = OMPI_FINT_2_INT(*recvcount);
            if (OMPI_IS_FORTRAN_IN_PLACE(sendbuf)) {
                sendbuf = MPI_IN_PLACE;
                OMPI_CFI_2_C_COPY(x2, recvbuf, c_recvcount, c_recvtype, c_recvdatatype, c_ierr);
            } else {
                OMPI_CFI_2_C_ALLOC(x2, recvbuf, c_recvcount, c_recvtype, c_recvdatatype, c_ierr);
            }
            if (MPI_SUCCESS != c_ierr) {
                if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
                OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME)
                return;
            }
        }
        if (!OMPI_IS_FORTRAN_IN_PLACE(sendbuf)) {
            c_sendtype = PMPI_Type_f2c(*sendtype);
            c_sendcount = OMPI_FINT_2_INT(*sendcount);
            OMPI_CFI_2_C(x1, c_sendcount, c_sendtype, c_senddatatype, c_ierr);
            if (MPI_SUCCESS != c_ierr) {
                if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
                OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME)
                return;
            }
        }
    }

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);


    c_ierr = PMPI_Gather(sendbuf, c_sendcount,
                        c_senddatatype, recvbuf,
                        c_recvcount,
                        c_recvdatatype,
                        c_root,
                        c_comm);
    if (c_senddatatype != c_sendtype) {
        ompi_datatype_destroy(&c_senddatatype);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    if ((OMPI_COMM_IS_INTER(c_comm) && (MPI_ROOT == c_root)) ||
        (!OMPI_COMM_IS_INTER(c_comm) && (ompi_comm_rank(c_comm) == c_root))) {
        OMPI_C_2_CFI_COPY(x2, recvbuf, c_recvcount, c_recvtype, c_recvdatatype, c_ierr);
    }   
}
