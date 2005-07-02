/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Finalized = PMPI_Finalized
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Finalized";


int MPI_Finalized(int *flag) 
{
    MPI_Comm null = NULL;

    if (MPI_PARAM_CHECK) {
        if (NULL == flag) {

            /* If we have an error, the action that we take depends on
               whether we're currently (after MPI_Init and before
               MPI_Finalize) or not */

            if (ompi_mpi_initialized && !ompi_mpi_finalized) {
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                              FUNC_NAME);
            } else {
                return OMPI_ERRHANDLER_INVOKE(null, MPI_ERR_ARG,
                                              FUNC_NAME);
            }
        }
    }

    /* Pretty simple */

    *flag = ompi_mpi_finalized;
    return MPI_SUCCESS;
}
