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


void ompi_bcast_cdesc(CFI_cdesc_t* x, MPI_Fint *count, MPI_Fint *datatype,
		      MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_datatype, c_type = PMPI_Type_f2c(*datatype);
    void *buffer = x->base_addr;
    int c_count =  OMPI_FINT_2_INT(*count);

    c_comm = PMPI_Comm_f2c(*comm);

    c_datatype = c_type;
    if (x->rank != 0 && !CFI_is_contiguous(x)) {
        c_ierr = ompi_cdesc_create_datatype(x, c_count, c_type, &c_datatype);
        if (MPI_SUCCESS != c_ierr) {
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            return;
        }
        c_count = 1;
    }
    c_ierr = PMPI_Bcast(OMPI_F2C_BOTTOM(buffer),
                       c_count,
                       c_datatype,
                       OMPI_FINT_2_INT(*root),
                       c_comm);
    if (c_datatype != c_type) {
        ompi_datatype_destroy(&c_datatype);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
