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

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/use-mpi-f08/cdesc/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

void ompi_sendrecv_cdesc(CFI_cdesc_t* x1, MPI_Fint *sendcount, MPI_Fint *sendtype,
		         MPI_Fint *dest, MPI_Fint *sendtag, CFI_cdesc_t* x2,
		         MPI_Fint *recvcount, MPI_Fint *recvtype,
		         MPI_Fint *source, MPI_Fint *recvtag, MPI_Fint *comm,
		         MPI_Fint *status, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_senddatatype, c_sendtype = PMPI_Type_f2c(*sendtype);
    MPI_Datatype c_recvdatatype, c_recvtype = PMPI_Type_f2c(*recvtype);
    MPI_Status c_status;
    void *sendbuf = x1->base_addr;
    int c_sendcount =  OMPI_FINT_2_INT(*sendcount);
    void *recvbuf = x2->base_addr;
    int c_recvcount =  OMPI_FINT_2_INT(*recvcount);

    c_comm = PMPI_Comm_f2c (*comm);

    c_senddatatype = c_sendtype;
    if (x1->rank != 0 && !CFI_is_contiguous(x1)) {
        c_ierr = ompi_cdesc_create_datatype(x1, c_sendcount, c_sendtype, &c_senddatatype);
        if (MPI_SUCCESS != c_ierr) {
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            return;
        }
        c_sendcount = 1;
    }
    c_recvdatatype = c_recvtype;
    if (x2->rank != 0 && !CFI_is_contiguous(x2)) {
        c_ierr = ompi_cdesc_create_datatype(x2, c_recvcount, c_recvtype, &c_recvdatatype);
        if (MPI_SUCCESS != c_ierr) {
            if (c_senddatatype != c_sendtype) {
                ompi_datatype_destroy(&c_senddatatype);
            }
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            return;
        }
        c_recvcount = 1;
    }
    c_ierr = PMPI_Sendrecv(OMPI_F2C_BOTTOM(sendbuf), c_sendcount,
                           c_senddatatype,
                           OMPI_FINT_2_INT(*dest),
                           OMPI_FINT_2_INT(*sendtag),
                           OMPI_F2C_BOTTOM(recvbuf), c_recvcount,
                           c_recvdatatype, OMPI_FINT_2_INT(*source),
                           OMPI_FINT_2_INT(*recvtag),
                           c_comm, &c_status);
    if (c_senddatatype != c_sendtype) {
        ompi_datatype_destroy(&c_senddatatype);
    }
    if (c_recvdatatype != c_recvtype) {
        ompi_datatype_destroy(&c_recvdatatype);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr &&
        !OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        PMPI_Status_c2f(&c_status, status);
    }
}
