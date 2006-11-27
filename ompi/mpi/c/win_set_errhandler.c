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

#include "ompi/mpi/c/bindings.h"
#include "ompi/win/win.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Win_set_errhandler = PMPI_Win_set_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Win_set_errhandler";


int MPI_Win_set_errhandler(MPI_Win win, MPI_Errhandler errhandler) 
{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (ompi_win_invalid(win) && win != MPI_WIN_NULL) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        } else if (NULL == errhandler ||
                   MPI_ERRHANDLER_NULL == errhandler ||
                   (OMPI_ERRHANDLER_TYPE_WIN != errhandler->eh_mpi_object_type && 
                    OMPI_ERRHANDLER_TYPE_PREDEFINED != errhandler->eh_mpi_object_type) ) {
            return OMPI_ERRHANDLER_INVOKE(win, MPI_ERR_ARG, FUNC_NAME);
        }
    }

    /* Ditch the old errhandler, and decrement its refcount */
    OBJ_RELEASE(win->error_handler);

    /* We have a valid comm and errhandler, so increment its refcount */
    win->error_handler = errhandler;
    OBJ_RETAIN(win->error_handler);

    /* All done */
    return MPI_SUCCESS;
}
