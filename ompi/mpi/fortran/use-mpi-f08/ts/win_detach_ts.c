/*
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

static const char FUNC_NAME[] = "MPI_Win_detach";

void ompi_win_detach_ts(MPI_Fint *win, CFI_cdesc_t *x,
                        MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Win c_win;

    c_win = PMPI_Win_f2c(*win);
    if (OMPI_CFI_IS_CONTIGUOUS(x)) {
         c_ierr = PMPI_Win_detach(c_win, x->base_addr);
    } else {
        c_ierr = MPI_ERR_BUFFER;
        OMPI_ERRHANDLER_INVOKE(c_win, c_ierr, FUNC_NAME);
    }
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
