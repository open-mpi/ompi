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
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
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

static const char FUNC_NAME[] = "MPI_Win_create";

void ompi_win_create_ts(CFI_cdesc_t *x, MPI_Aint *size, MPI_Fint *disp_unit,
                        MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win,
                        MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Win c_win;
    MPI_Info c_info;
    MPI_Comm c_comm;

    c_comm = PMPI_Comm_f2c(*comm);
    c_info = PMPI_Info_f2c(*info);

    if (OMPI_CFI_IS_CONTIGUOUS(x)) {
        c_ierr = PMPI_Win_create(x->base_addr, *size,
                                 OMPI_FINT_2_INT(*disp_unit),
                                 c_info, c_comm, &c_win);
    } else {
        c_ierr = MPI_ERR_BUFFER;
        OMPI_ERRHANDLER_INVOKE(c_comm, c_ierr, FUNC_NAME);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
       *win = PMPI_Win_c2f(c_win);
    }
}
