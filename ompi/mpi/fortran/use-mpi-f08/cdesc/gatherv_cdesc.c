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


void ompi_gatherv_cdesc(CFI_cdesc_t* x1, MPI_Fint *sendcount, MPI_Fint *sendtype,
		        char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs,
		        MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm,
		        MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_senddatatype, c_sendtype = PMPI_Type_f2c(*sendtype);
    void *sendbuf = x1->base_addr;
    int c_sendcount =  OMPI_FINT_2_INT(*sendcount);
    MPI_Datatype c_recvtype = PMPI_Type_f2c(*recvtype);
    int size, c_ierr;
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = PMPI_Comm_f2c(*comm);

    PMPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    c_senddatatype = c_sendtype;
    if (x1->rank != 0 && !CFI_is_contiguous(x1)) {
        c_ierr = ompi_cdesc_create_datatype(x1, c_sendcount, c_sendtype, &c_senddatatype);
        if (MPI_SUCCESS != c_ierr) {
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            return;
        }
        c_sendcount = 1;
    }
    c_ierr = PMPI_Gatherv(sendbuf, OMPI_FINT_2_INT(*sendcount),
                         c_sendtype, recvbuf,
                         OMPI_ARRAY_NAME_CONVERT(recvcounts),
                         OMPI_ARRAY_NAME_CONVERT(displs),
                         c_recvtype,
                         OMPI_FINT_2_INT(*root),
                         c_comm);
    if (c_senddatatype != c_sendtype) {
        ompi_datatype_destroy(&c_senddatatype);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
