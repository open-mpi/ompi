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
#include "ompi/errhandler/errhandler.h"
#include "ompi/mpi/fortran/use-mpi-f08/ts/bindings.h"
#include "ompi/constants.h"
#include "ompi/mpi/fortran/base/constants.h"
#include "ompi/mpi/fortran/base/fortran_base_strings.h"

static const char FUNC_NAME[] = "MPI_Pack_external";

void ompi_pack_external_ts(char *datarep, CFI_cdesc_t* x1, MPI_Fint *incount,
                           MPI_Fint *datatype, CFI_cdesc_t* x2,
                           MPI_Aint *outsize, MPI_Aint *position,
                           MPI_Fint *ierr, int datarep_len)
{
    int ret, c_ierr;
    char *c_datarep;
    MPI_Datatype c_datatype, c_type = PMPI_Type_f2c(*datatype);
    void *inbuf = x1->base_addr;
    char *outbuf = x2->base_addr;
    int c_incount = OMPI_FINT_2_INT(*incount);

    /* Convert the fortran string */

    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(datarep, datarep_len,
                                                       &c_datarep))) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_SELF, ret, FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    OMPI_CFI_2_C(x1, c_incount, c_type, c_datatype, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(MPI_COMM_SELF, c_ierr, FUNC_NAME);
        return;
    }

    OMPI_CFI_CHECK_CONTIGUOUS(x2, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (c_datatype != c_type) {
            ompi_datatype_destroy(&c_datatype);
        }
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(MPI_COMM_SELF, c_ierr, FUNC_NAME);
        return;
    }

    c_ierr = PMPI_Pack_external(c_datarep, OMPI_F2C_BOTTOM(inbuf),
                               c_incount,
                               c_datatype, outbuf,
                               *outsize,
                               position);
    if (c_datatype != c_type) {
        ompi_datatype_destroy(&c_datatype);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    free(c_datarep);
}
