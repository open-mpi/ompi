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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"

#include <stdio.h>

#include "mpi/c/bindings.h"
#include "info/info.h"
#include "win/win.h"
#include "attribute/attribute.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Win_create = PMPI_Win_create
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Win_create";


int MPI_Win_create(void *base, MPI_Aint size, int disp_unit,
                   MPI_Info info, MPI_Comm comm, MPI_Win *win) 
{
    ompi_communicator_t *ompi_comm;
    ompi_win_t *ompi_win;
    int ret = OMPI_SUCCESS;

    /* argument checking */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (MPI_COMM_NULL == comm || ompi_comm_invalid (comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);

        } else if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_INFO,
                                          FUNC_NAME);

        } else if (NULL == win) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_WIN, FUNC_NAME);
        }
    }

    ompi_comm = (ompi_communicator_t*) comm;
    if (OMPI_COMM_IS_INTER(ompi_comm)) {
        /* must be an intracommunicator */
        return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COMM, FUNC_NAME);
    }

    /*
     * Create a shell of a window 
     */
    ret = ompi_win_create(base, size, disp_unit, ompi_comm,
                          info, &ompi_win);
    if (OMPI_SUCCESS != ret) return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_WIN, FUNC_NAME);

    *win = (MPI_Win) ompi_win;

    return OMPI_SUCCESS;
}
