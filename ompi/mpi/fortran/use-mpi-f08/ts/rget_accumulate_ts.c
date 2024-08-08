/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2014-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2015      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/win/win.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mpi/fortran/use-mpi-f08/ts/bindings.h"
#include "ompi/mpi/fortran/base/constants.h"

static const char FUNC_NAME[] = "MPI_Rget_accumulate";

void ompi_rget_accumulate_ts(CFI_cdesc_t *x1, MPI_Fint *origin_count,
                             MPI_Fint *origin_datatype, CFI_cdesc_t *x2,
                             MPI_Fint *result_count, MPI_Fint *result_datatype,
                             MPI_Fint *target_rank, MPI_Aint *target_disp,
                             MPI_Fint *target_count, MPI_Fint *target_datatype,
                             MPI_Fint *op, MPI_Fint *win, MPI_Fint *request,
                             MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_origin_datatype, c_origin_type = PMPI_Type_f2c(*origin_datatype);
    MPI_Datatype c_result_datatype, c_result_type = PMPI_Type_f2c(*result_datatype);
    MPI_Datatype c_target_datatype = PMPI_Type_f2c(*target_datatype);
    MPI_Win c_win = PMPI_Win_f2c(*win);
    MPI_Op c_op = PMPI_Op_f2c(*op);
    char *origin_addr = x1->base_addr;
    int c_origin_count = OMPI_INT_2_FINT(*origin_count);
    char *result_addr = x2->base_addr;
    int c_result_count = OMPI_INT_2_FINT(*result_count);
    MPI_Request c_req;

    OMPI_CFI_2_C(x1, c_origin_count, c_origin_type, c_origin_datatype, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_win, c_ierr, FUNC_NAME)
        return;
    }
    OMPI_CFI_2_C(x2, c_result_count, c_result_type, c_result_datatype, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        if (c_origin_datatype != c_origin_type) {
            ompi_datatype_destroy(&c_origin_datatype);
        }
        OMPI_ERRHANDLER_INVOKE(c_win, c_ierr, FUNC_NAME);
        return;
    }
    c_ierr = PMPI_Rget_accumulate(OMPI_F2C_BOTTOM(origin_addr),
                                  c_origin_count,
                                  c_origin_datatype,
                                  OMPI_F2C_BOTTOM(result_addr),
                                  c_result_count,
                                  c_result_datatype,
                                  OMPI_FINT_2_INT(*target_rank),
                                  *target_disp,
                                  OMPI_FINT_2_INT(*target_count),
                                  c_target_datatype, c_op, c_win, &c_req);
    if (c_origin_datatype != c_origin_type) {
        ompi_datatype_destroy(&c_origin_datatype);
    }
    if (c_result_datatype != c_result_type) {
        ompi_datatype_destroy(&c_result_datatype);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *request = PMPI_Request_c2f(c_req);
    }
}
