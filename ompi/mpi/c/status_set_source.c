/*
 * Copyright (c) 2025      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Status_set_source = PMPI_Status_set_source
#endif
#define MPI_Status_set_source PMPI_Status_set_source
#endif

static const char FUNC_NAME[] = "MPI_Status_set_source";


int MPI_Status_set_source(MPI_Status *status, int source)
{
    MEMCHECKER(
        if(status != MPI_STATUSES_IGNORE) {
            /*
             * Before checking the complete status, we need to reset the definedness
             * of the MPI_ERROR-field (single-completion calls wait/test).
             */
            opal_memchecker_base_mem_defined(&status->MPI_ERROR, sizeof(int));
            memchecker_status(status);
        }
    );

    if (MPI_PARAM_CHECK) {
        int rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == status ||
            MPI_STATUS_IGNORE == status ||
            MPI_STATUSES_IGNORE == status) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_NOHANDLE_CHECK(rc, rc, FUNC_NAME);
    }

    status->MPI_SOURCE = source;
    return MPI_SUCCESS;
}
