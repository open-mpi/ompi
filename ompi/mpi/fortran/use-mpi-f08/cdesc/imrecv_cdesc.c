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
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      FUJITSU LIMITED.  All rights reserved.
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

void ompi_imrecv_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
                       MPI_Fint *message, MPI_Fint *request, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_datatype, c_type = PMPI_Type_f2c(*datatype);
    MPI_Request c_req;
    MPI_Message c_message;
    void *buf = x->base_addr;
    int c_count =  OMPI_FINT_2_INT(*count);

    c_message = PMPI_Message_f2c(*message);

    if (x->rank != 0 && !CFI_is_contiguous(x)) {
        c_ierr = ompi_cdesc_create_datatype(x, c_count, c_type, &c_datatype);
        if (MPI_SUCCESS != c_ierr) {
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            return;
        }
        c_count = 1;
    }
    c_ierr = OMPI_INT_2_FINT(PMPI_Imrecv(OMPI_F2C_BOTTOM(buf), c_count,
                                         c_datatype, &c_message, &c_req));
    if (c_datatype != c_type) {
        ompi_datatype_destroy(&c_datatype);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
       *request = PMPI_Request_c2f(c_req);
       /* message is an INOUT, and may be updated by the recv */
       *message = PMPI_Message_c2f(c_message);
    }
}
