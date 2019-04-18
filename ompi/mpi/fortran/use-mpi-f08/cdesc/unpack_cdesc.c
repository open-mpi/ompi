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

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/mpi/fortran/use-mpi-f08/cdesc/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

void ompi_unpack_cdesc(char *inbuf, MPI_Fint *insize, MPI_Fint *position,
		       CFI_cdesc_t* x, MPI_Fint *outcount, MPI_Fint *datatype,
		       MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm;
    MPI_Datatype c_datatype, c_type = PMPI_Type_f2c(*datatype);
    OMPI_SINGLE_NAME_DECL(position);
    void *outbuf = x->base_addr;
    int c_outcount = OMPI_FINT_2_INT(*outcount);

    c_datatype = c_type;
    if (x->rank != 0 && !CFI_is_contiguous(x)) {
        c_ierr = ompi_cdesc_create_datatype(x, c_outcount, c_type, &c_datatype);
        if (MPI_SUCCESS != c_ierr) {
            if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
            return;
        }
        c_outcount = 1;
    }
    c_comm = PMPI_Comm_f2c(*comm);
    OMPI_SINGLE_FINT_2_INT(position);

    c_ierr = PMPI_Unpack(inbuf, OMPI_FINT_2_INT(*insize),
                         OMPI_SINGLE_NAME_CONVERT(position),
                         OMPI_F2C_BOTTOM(outbuf), c_outcount,
                         c_datatype, c_comm);
    if (c_datatype != c_type) {
        ompi_datatype_destroy(&c_datatype);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
         OMPI_SINGLE_INT_2_FINT(position);
     }
}
