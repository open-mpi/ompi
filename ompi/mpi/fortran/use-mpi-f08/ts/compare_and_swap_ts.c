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
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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

static const char FUNC_NAME[] = "MPI_Compare_and_swap";

void ompi_compare_and_swap_ts(CFI_cdesc_t *x1, CFI_cdesc_t *x2, CFI_cdesc_t *x3,
                              MPI_Fint *datatype, MPI_Fint *target_rank, MPI_Aint *target_disp,
                              MPI_Fint *win, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_datatype = PMPI_Type_f2c(*datatype);
    MPI_Win c_win = PMPI_Win_f2c(*win);
    char *origin_addr = x1->base_addr, *compare_addr = x2->base_addr, *result_addr = x3->base_addr;

    OMPI_CFI_CHECK_CONTIGUOUS(x1, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_win, c_ierr, FUNC_NAME)
        return;
    }
    OMPI_CFI_CHECK_CONTIGUOUS(x2, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        OMPI_ERRHANDLER_INVOKE(c_win, c_ierr, FUNC_NAME)
        return;
    }
    OMPI_CFI_CHECK_CONTIGUOUS(x3, c_ierr);
    if (MPI_SUCCESS != c_ierr) {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }
    c_ierr = PMPI_Compare_and_swap(OMPI_F2C_BOTTOM(origin_addr),
                                   OMPI_F2C_BOTTOM(compare_addr),
                                   OMPI_F2C_BOTTOM(result_addr),
                                   c_datatype,
                                   OMPI_FINT_2_INT(*target_rank),
                                   *target_disp, c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
