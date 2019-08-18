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
#include "ompi/mpi/fortran/use-mpi-f08/ts/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

static const char FUNC_NAME[] = "MPI_Unpack";

void ompi_unpack_ts(CFI_cdesc_t* x1, MPI_Fint *insize, MPI_Fint *position,
                    CFI_cdesc_t* x2, MPI_Fint *outcount, MPI_Fint *datatype,
                    MPI_Fint *comm, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Comm c_comm = PMPI_Comm_f2c(*comm);
    MPI_Datatype c_datatype, c_type = PMPI_Type_f2c(*datatype);
    OMPI_SINGLE_NAME_DECL(position);
    char *inbuf = x1->base_addr;
    void *outbuf = x2->base_addr;
    int c_outcount = OMPI_FINT_2_INT(*outcount);

    OMPI_CFI_CHECK_CONTIGUOUS(x1, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME);
        return;
    }

    OMPI_CFI_2_C(x2, c_outcount, c_type, c_datatype, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME);
        return;
    }

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
