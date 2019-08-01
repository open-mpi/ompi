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
 * Copyright (c) 2011-2013 Cisco Systems, Inc.  All rights reserved.
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


void ompi_scatterv_cdesc(char *sendbuf, MPI_Fint *sendcounts,
		         MPI_Fint *displs, MPI_Fint *sendtype,
		         CFI_cdesc_t *x, MPI_Fint *recvcount,
		         MPI_Fint *recvtype, MPI_Fint *root,
		         MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype;
    MPI_Datatype c_recvdatatype, c_recvtype = PMPI_Type_f2c(*recvtype);
    void *recvbuf = x->base_addr;
    int c_recvcount =  OMPI_FINT_2_INT(*recvcount);
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = PMPI_Comm_f2c(*comm);
    c_sendtype = PMPI_Type_f2c(*sendtype);

    PMPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_IN_PLACE(recvbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_recvdatatype = c_recvtype;
    if (x->rank != 0 && !CFI_is_contiguous(x)) {
        c_ierr = ompi_cdesc_create_datatype(x, c_recvcount, c_recvtype, &c_recvdatatype);
        if (MPI_SUCCESS != c_ierr) {
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            return;
        }
        c_recvcount = 1;
    }
    c_ierr = PMPI_Scatterv(sendbuf,
                          OMPI_ARRAY_NAME_CONVERT(sendcounts),
                          OMPI_ARRAY_NAME_CONVERT(displs),
                          c_sendtype, recvbuf,
                          c_recvcount,
                          c_recvdatatype,
                          OMPI_FINT_2_INT(*root), c_comm);
    if (c_recvdatatype != c_recvtype) {
        ompi_datatype_destroy(&c_recvdatatype);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(displs);
}
